box::use(
  shiny[...],
  reactable[...],
  dplyr[...],
  shinyWidgets[...],
  shinyjs[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    useShinyjs(),
    titlePanel("Liquidar a fábrica"),
    actionButton(ns("liquidar"), "Liquidar"),
    reactableOutput(ns("table")),
    hidden(downloadButton(outputId = ns("download_liquidacion"), ""))
  )
}

#' @export
server <- function(id, db_manager, state_manager) {
  moduleServer(id, function(input, output, session) {
    state_manager$set("update_liquidacion_table", NULL)
    update_table <- reactiveVal(0)
    output$table <- renderReactable({
      update_table()
      state_manager$listen("update_liquidacion_table")
      data <- db_manager$get_facturacion() %>% select(id, factura_nro, created_at, fecha, articulo, cantidad, costo, esta_liquidado)
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
          )
        )
      )
    })
    
    observeEvent(input$liquidar,{
      facturacion <- db_manager$get_facturacion()
      min_date <- min(facturacion$fecha)
      max_date <- max(facturacion$fecha)
      showModal(
        modalDialog(
          size = "l",
          fluidPage(
            fluidRow(
              shiny::dateRangeInput(session$ns("date"), label = "Fechas liquidación:", start = min_date, end = max_date),
              currencyInput(session$ns("total_liquidar"), label = "Costo Total a Liquidar", value = NULL, format = "dollar")
            ),
            reactable::reactableOutput(session$ns("table_modal"))
          ),
          footer = div(modalButton("Cerrar"), actionButton(session$ns("confirmar_liquidar"), "Liquidar"))
        )
      )
    })
    table_modal <- reactiveVal()
    
    observeEvent(input$date, {
      facturacion <- db_manager$get_facturacion() %>% 
        filter(esta_liquidado == FALSE) %>% 
        filter(fecha >= input$date[[1]], fecha <= input$date[[2]])
      table_modal(facturacion)
      costo <- facturacion %>% pull(costo)
      shinyWidgets::updateCurrencyInput(inputId = "total_liquidar", value = sum(costo))
    })
    
    output$table_modal <- reactable::renderReactable({
      reactable::reactable(
        table_modal() %>% select(c(factura_nro, fecha, articulo, cantidad, costo, esta_liquidado))
      )
    })
    
    observeEvent(input$confirmar_liquidar, {
      db_manager$update_esta_liquidado(table_modal()$id)
      click("download_liquidacion")
      removeModal()
      update_table(update_table() + 1)
    })
    
    output$download_liquidacion <- downloadHandler(
      filename = function() {
        paste0("liquidaction.csv")
      },
      content = function(file) {
        utils::write.csv2(db_manager$get_facturacion(), file)
      }
    )
  })
}
