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
      showModal(
        modalDialog(
          size = "l",
          fluidPage(
            useShinyjs(),
            fluidRow(
              dateInput(session$ns("date"), value = today(), label = "Fecha: ", format = "dd/mm/yyyy"),
            ), 
            fluidRow(
              column(6, 
                selectInput(session$ns("articulo"), label = "Articulo", choices = productos$articulo, selected = NULL)
              ),
              column(3,
                numericInput(session$ns("cantidades"), label = "Cantidad: ", value = NULL)
              )
            ),
            fluidRow(
              column(6,
                numericInput(session$ns("talle"), label = "Talle: ", value = NULL)
              ),
              column(6,
                textInput(session$ns("color"), label = "Color: ")
              )
            ),
            fluidRow(
              disabled(selectInput(session$ns("descripcion"), label = "Descripcion", choices = productos$descripcion)) %>% 
                shiny::tagAppendAttributes(style = 'width: 100%;')
            ),
            fluidRow(
              disabled(currencyInput(session$ns("precio_u"), label = "Precio Unitario:", value = NULL, format = "dollar")),
              disabled(currencyInput(session$ns("precio_total"), label = "Total: ", value = NULL, format = "dollar"))
            ),
            div("Regla de negocio: >=6 Precio Mayorista")
          ),
          footer = div(modalButton("Cancelar"), actionButton(session$ns("confirmar_facturar"), "Facturar"))
        )
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
      req(input$precio_total != 0)
      req(input$cantidades)
      req(input$articulo)
      
      data <- data.frame(
        id = uuid::UUIDgenerate(),
        fecha = input$date,
        articulo = input$articulo,
        cantidad = input$cantidades,
        talle = input$talle,
        color = input$color,
        precio_venta_unitario = input$precio_u,
        precio_total = input$precio_total
      )
      
      db_manager$append_to_table(
        table_name = "facturacion",
        value = data
      )
      
      removeModal()
      update_table(update_table() + 1)
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
