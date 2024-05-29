box::use(
  shiny[...],
  shiny.router[...],
  dplyr[...],
  bslib[...],
  googledrive[...]
)

box::use(
  view = app/view,
  app/logic/db_manager[db_manager],
  app/logic/state_manager[state_manager]
)

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "app/.secrets"
)

# Read constants file
constants <- config::get(file = "./app/constants/constants.yml")

# Read environment variables
DB_SOURCE <- "local"

db_credentials <- config::get(
  value = DB_SOURCE,
  file = "app/database/credentials.yml"
)

db_manager <- db_manager(db_credentials)

menu <- tags$ul(
  tags$li(a(class = "item", href = route_link("/"), "Facturación")),
  tags$li(a(class = "item", href = route_link("liquidar_fabrica"), "Liquidar")),
  tags$li(a(class = "item", href = route_link("lista_precios"), "Lista Precios"))
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bslib::page(
    bslib::page_sidebar(
      window_title = "Zapassss",
      theme = bslib::bs_theme(
        version = 5
      ),
      
      sidebar = bslib::sidebar(
        open = TRUE,
        width = 200,
        div(
          menu, 
          actionButton(ns("send_data"), "Enviar data", icon = icon("sync")),
          hr(),
          actionButton(ns("obtain_data"), "Obtener data", icon = icon("sync"))
        )
      ),
      router_ui(
        route("/", view$facturacion$ui(ns("facturacion"))),
        route("liquidar_fabrica", view$liquidar_fabrica$ui(ns("liquidar_fabrica"))),
        route("lista_precios", view$productos$ui(ns("productos"))),
      )
    )
  )
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server()
    state_manager <- state_manager()
    view$facturacion$server("facturacion", db_manager, state_manager, constants)
    view$productos$server("productos", db_manager, state_manager, constants)
    view$liquidar_fabrica$server("liquidar_fabrica", db_manager, state_manager)
    
    observeEvent(input$send_data, {
      # enviar data de facturacion
      withProgress(message  = 'Enviando...', value = 1/10, { 
        facturacion_in_db <- db_manager$get_facturacion() %>% filter(compu == constants$compu)
        incProgress(5/15)
        utils::write.csv2(facturacion_in_db, paste0(constants$compu, "_facturas.csv"), row.names = FALSE)
        incProgress(10/15)
        drive_update(as_id(constants[[paste0("facturas_", constants$compu, "_google_id")]]), paste0(constants$compu, "_facturas.csv"))
      })
    })
    
    observeEvent(input$obtain_data, {
      # get new data from drive 
      withProgress(message  = 'Recibiendo...', value = 1/10, { 
        facturacion_in_drive <- drive_get(as_id(constants[[paste0("facturas_", constants$compu2, "_google_id")]])) %>% 
          googledrive::drive_read_string(., encoding = "UTF-8") %>% 
          utils::read.csv2(text = .)
        incProgress(5/15)
        db_manager$delete_facturacion_by_compu(constants$compu2)
        incProgress(10/15)
        db_manager$append_to_table("facturacion", facturacion_in_drive)
        state_manager$update_value("update_facturacion", NULL, TRUE)
        
        lista_precios <- drive_get(as_id(constants$productos_google_id)) %>% 
          googledrive::drive_read_string(., encoding = "UTF-8") %>% 
          utils::read.csv2(text = .)
        db_manager$update_productos(lista_precios)
        state_manager$update_value("update_products", NULL, TRUE)
      })
    })
    
  })
}

onStop(function() {
  db_manager$disconnect()
})
