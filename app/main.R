box::use(
  shiny[...],
  shiny.router[...],
  dplyr[...]
)

box::use(
  view = app/view,
  app/logic/db_manager[db_manager]
)

# Read environment variables
DB_SOURCE <- Sys.getenv("DB_SOURCE", unset = "local")

db_credentials <- config::get(
  value = DB_SOURCE,
  file = "app/database/credentials.yml"
)

db_manager <- db_manager(db_credentials)

lista_precios <- db_manager$get_productos()
  
menu <- tags$ul(
  tags$li(a(class = "item", href = route_link("/"), "Facturación")),
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
      route("lista_precios", view$productos$ui(ns("productos"))),
    )
  )
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server()
    view$facturacion$server("facturacion", db_manager)
    view$productos$server("productos", db_manager)
  })
}

onStop(function() {
  db_manager$disconnect()
})
