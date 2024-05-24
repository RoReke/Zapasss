box::use(
  shiny[...],
  shinyjs[...],
  dplyr[...],
  lubridate[...],
  shinyWidgets[...],
  reactable[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  facturacion <- div(
    titlePanel("Facturación"),
    div(
      actionButton(ns("facturar"), "Facturar"),
      downloadButton(ns("download"), "Descargar")
    ),
    reactableOutput(ns("table"))
  )
}

#' @export
server <- function(id, db_manager) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$facturar, {
      productos <- db_manager$get_productos()
      factura_nro <- db_manager$get_last_number_factura() %>% pull(factura_nro)
      factura_nro <- if (!isTruthy(factura_nro)) 1 else factura_nro + 1  
      showModal(
        modalDialog(
          size = "l",
          fluidPage(
            useShinyjs(),
            fluidRow(
              column(4, dateInput(session$ns("date"), value = today(), label = "Fecha: ", format = "dd/mm/yyyy")),
              column(4, textInput(session$ns("cliente"), label = "Cliente: ")),
              column(4, disabled(numericInput(session$ns("factura_nro"), label = "Factura: ", value = factura_nro)))
            ), 
            hr(style="margin-top: 5px; margin-bottom: 5px;"),
            fluidRow(
              column(3, 
                selectInput(session$ns("articulo"), label = "Articulo", choices = c("", productos$articulo), selected = NULL)
              ),
              column(9,
                disabled(selectInput(session$ns("descripcion"), label = "Descripcion", choices = c("", productos$descripcion))) %>% 
                  shiny::tagAppendAttributes(style = 'width: 100%;')
              )
            ),
            fluidRow(
              column(4,
                numericInput(session$ns("cantidades"), label = "Cantidad: ", value = NULL)
              ),
              column(4,
                numericInput(session$ns("talle"), label = "Talle: ", value = NULL)
              ),
              column(4,
                textInput(session$ns("color"), label = "Color: ")
              )
            ),
            fluidRow(
              column(5,
                disabled(currencyInput(session$ns("precio_u"), label = "Precio Unitario:", value = NULL, format = "dollar"))
              ),
              column(5,
                disabled(currencyInput(session$ns("precio_total"), label = "Subtotal: ", value = NULL, format = "dollar"))
              ),
              column(2,actionButton(session$ns("agregar"), "Agregar", icon = icon("plus"), style = "position: relative; top: 36px;"))
            ),
            fluidRow(
              reactable::reactableOutput(session$ns("table_add"),width = "100%") 
            ),
            hr(style="margin-top: 5px; margin-bottom: 5px;"),
            fluidRow(
              column(5,
                disabled(numericInput(session$ns("total_q"), label = "Total Cantidades:", value = NULL,))
              ),
              column(5,
                disabled(currencyInput(session$ns("total"), label = "Total: ", value = NULL, format = "dollar"))
              )
            ),
            div("Regla de negocio: >=6 Precio Mayorista")
          ),
          footer = div(modalButton("Cancelar"), actionButton(session$ns("confirmar_facturar"), "Facturar"))
        )
      )
    })
    
    data_table_add <- reactiveVal(data.frame(
      articulo = character(),
      cantidad = numeric(),
      talle = integer(),
      color = character(),
      precio_u = numeric(),
      precio_total = numeric(),
      stringsAsFactors = FALSE
    ))
    
    observeEvent(input$agregar, {
      req(input$precio_total != 0)
      req(input$cantidades)
      req(input$articulo)
      data <- data.frame(
        articulo = input$articulo,
        cantidad = input$cantidades,
        talle = input$talle,
        color = input$color,
        precio_u = input$precio_u,
        precio_total = input$precio_total
      )
      data_table_add(data_table_add() %>% bind_rows(data))
      if (sum(data_table_add()$cantidad) >= 6) {
        new_data <- data_table_add() %>% 
          select(-c(precio_u, precio_total)) %>% 
          left_join(db_manager$get_productos() %>% select(articulo, p_mayorista), by = "articulo") %>% 
          mutate(
            precio_u = p_mayorista,
            precio_total = p_mayorista * cantidad
          ) %>% 
          select(-p_mayorista)
        data_table_add(new_data)
      }
      shinyjs::reset("articulo")
      shinyjs::reset("cantidades")
      shinyjs::reset("talle")
      shinyjs::reset("color")
      shinyWidgets::updateCurrencyInput(inputId = "precio_u", value = 0)
      shinyWidgets::updateCurrencyInput(inputId = "precio_total", value = 0)
      
      shinyWidgets::updateAutonumericInput(inputId = "total_q", value = sum(data_table_add()$cantidad))
      shinyWidgets::updateCurrencyInput(inputId = "total", value = sum(data_table_add()$precio_total))
    }) 
    
    output$table_add <- reactable:::renderReactable({
      req(data_table_add())
      reactable::reactable(
        data_table_add()
      )
    })
    observeEvent(input$articulo, {
      productos <- db_manager$get_productos()
      updateSelectInput(inputId = "descripcion", selected = productos %>% filter(articulo == input$articulo) %>% pull(descripcion))
    })
    
    observeEvent(list(input$articulo, input$cantidades), {
      req(input$cantidades)
      req(input$articulo)
      productos <- db_manager$get_productos()
      precio_u <- if (input$cantidades < 6) {
        productos %>% filter(articulo == input$articulo) %>% pull(p_minorista)
      } else {
        productos %>% filter(articulo == input$articulo) %>% pull(p_mayorista)
      }
      updateCurrencyInput(inputId = "precio_u", value = precio_u)
    })
    
    observeEvent(input$precio_u, {
      updateCurrencyInput(inputId = "precio_total", value = input$precio_u * input$cantidades)
    })
    
    update_table <- reactiveVal(0)
    observeEvent(input$confirmar_facturar, {
      req(nrow(data_table_add()) > 0)
      data <- data.frame(
        id = uuid::UUIDgenerate(),
        factura_nro = input$factura_nro,
        fecha = input$date,
        cliente = input$cliente,
        articulo = data_table_add()$articulo,
        cantidad = data_table_add()$cantidad,
        talle = data_table_add()$talle,
        color = data_table_add()$color,
        precio_venta_unitario = data_table_add()$precio_u,
        precio_total = data_table_add()$precio_total
      )
      
      db_manager$append_to_table(
        table_name = "facturacion",
        value = data
      )
      
      removeModal()
      update_table(update_table() + 1)
      
      data_table_add(data.frame(
        articulo = character(),
        cantidad = numeric(),
        talle = integer(),
        color = character(),
        precio_u = numeric(),
        precio_total = numeric(),
        stringsAsFactors = FALSE
      ))
    })
    
    output$table <- renderReactable({
      update_table()
      data <- db_manager$get_facturacion() %>% mutate(action_delete = "")
      reactable(
        data = data,
        columns = list(
          "id" = colDef(
            name = "",
            show = FALSE
          ),
          "created_at" = colDef(
            name = "",
            show = FALSE
          ),
          "fecha" = colDef(
            name = "Fecha",
            show = TRUE
          ),
          "precio_venta_unitario" = colDef(
            name = "Precio Unitario",
            show = TRUE
          ),
          "precio_total" = colDef(
            name = "Precio Total",
            show = TRUE
          ),
          action_delete = colDef(
            name = "",
            maxWidth = 70,
            sortable = FALSE,
            html = TRUE,
            align = "right",
            cell = function(value, row_index, col_name) {
              id <- data[row_index, "id"]
              buttons_column(
                session = session,
                row_id = id,
                btt_name = "btt_delete",
                class = "btt_delete"
              )
            }
          )
        )
      )
    })
    
    observeEvent(input$btt_delete, {
      db_manager$delete_facturacion_by_id(input$btt_delete)
      update_table(update_table() + 1)
    })
    
    output$download <- downloadHandler(
      filename = function() {
        paste0("facturacion.csv")
      },
      content = function(file) {
        utils::write.csv2(db_manager$get_facturacion(), file)
      }
    )
    
  })
}


buttons_column <- function(
    session, row_id, btt_name, class = "table-button"
) {
  
  btn_id <- session$ns(btt_name)
  button <- as.character(
    tags$div(
      tags$button(
        class = class,
        onclick = set_input_value(btn_id, row_id)
      )
    )
  )
  return(button)
}

set_input_value <- function(btn_id, row_id) {
  paste0(
    "Shiny.setInputValue('", btn_id, "','", row_id, "',{priority: 'event'})"
  )
}
