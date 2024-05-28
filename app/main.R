box::use(
  shiny[...],
  shiny.router[...],
  dplyr[...]
)

box::use(
  view = app/view,
  app/logic/db_manager[db_manager],
  app/logic/state_manager[state_manager]
)

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
  bslib::page_sidebar(
    window_title = "Zapassss",
    theme = bslib::bs_theme(
      version = 5
    ),
    
    sidebar = bslib::sidebar(
      open = TRUE,
      width = 200,
      menu
    ),
    router_ui(
      route("/", view$facturacion$ui(ns("facturacion"))),
      route("liquidar_fabrica", view$liquidar_fabrica$ui(ns("liquidar_fabrica"))),
      route("lista_precios", view$productos$ui(ns("productos"))),
    )
  )
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server()
    state_manager <- state_manager()
    view$facturacion$server("facturacion", db_manager, state_manager)
    view$productos$server("productos", db_manager, state_manager)
    view$liquidar_fabrica$server("liquidar_fabrica", db_manager, state_manager)
  })
}

onStop(function() {
  db_manager$disconnect()
})
