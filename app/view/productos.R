box::use(
  shiny[...],
  reactable[...],
  DT[...],
  dplyr[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    titlePanel("Lista Precios"),
    actionButton(ns("btt_upload"), "Subir Nueva Lista de Precios"),
    downloadButton(ns("download"), "Descargar"),
    reactableOutput(ns("table"))
  )
  
}

#' @export
server <- function(id, db_manager, state_manager) {
  moduleServer(id, function(input, output, session) {
    upload <- reactiveValues(data=NULL)
    update_table <- reactiveVal(0)
    
    observeEvent(input$btt_upload, {
      showModal(
        modalDialog(
          size = "xl",
          div(
            fileInput(session$ns("upload"), NULL, accept = c(".csv")),
            DT::DTOutput(session$ns("upload_table"))
          ),
          footer = div(
            modalButton("Cancel"),
            actionButton(session$ns("save"), "Guardar")
          )
        )
      )
    })
    
    observeEvent(input$upload, {
      req(input$upload)
      
      ext <- tools::file_ext(input$upload$name)
      upload$data <- switch(ext,
             csv = vroom::vroom(input$upload$datapath, delim = ";"),
             validate("Invalid file; Please upload a .csv or .tsv file")
      )
      
    })
    
    output$upload_table <- DT::renderDT({
      DT::datatable(
        upload$data,
        editable = TRUE
      )
    })
    
    observeEvent(input$upload_table_cell_edit, {
      info = input$upload_table_cell_edit
      i = info$row
      j = info$col  
      v = info$value
      
      upload$data[i, j] <<- DT::coerceValue(v, upload$data[i, j])
    })
    
    observeEvent(input$save, {
      db_manager$update_productos(upload$data)
      removeModal()
      update_table(update_table() + 1)
    })
    
    output$download <- downloadHandler(
      filename = function() {
        paste0("products.csv")
      },
      content = function(file) {
        utils::write.csv2(db_manager$get_productos(), file, row.names = FALSE)
      }
    )
    
    output$table <- renderReactable({
      update_table()
      reactable(
        db_manager$get_productos(),
        columns = list(
          "descripcion" = colDef(
            name = "Descripcion",
            show = TRUE,
            minWidth = 250
          )
        )
      )
    })
  })
}
