box::use(
  R6[R6Class],
  pool[...],
  glue[...],
  DBI[...]
)

#' R6 Class to manage DBs
DatabaseManager <- R6Class( #nolint
  "DatabaseManager",
  private = list(
    connection = "connection",
    get_schema_from_parameters = function(...) {
      parameters <- (...)
      if ("schema" %in% names(parameters)) {
        message(
          sprintf("Setting schema: %s", parameters$schema)
        )
        parameters$schema
      } else {
        warning(
          sprintf("Schema was not provided, using public as default")
        )
        "public"
      }
    },
    connect = function(...) {
      args <- (...)
      # We don't need schema in the config on ci. we have to remove it.
      # Since bdname only exist for ci config, we use it as condition.
      if ("dbname" %in% names(args)) {
        args$schema <- NULL
      }
      private$connection <- do.call(dbPool, args)
      self$set_schema()
      message("Opening DB connection")
    }
  ),
  public = list(
    schema = NULL,
    
    #' @description Create a new DB connection
    #' @param ... Arguments needed for both DBI::dbConnect()
    #' @return
    initialize = function(...) {
      self$schema <- private$get_schema_from_parameters(...)
      private$connect(...)
    },
    
    #' @description
    #' @param
    #' @return
    set_schema = function() {
      set_schema <- glue("SET search_path TO \"{self$schema}\";")
      dbExecute(
        conn = private$connection,
        statement = set_schema
      )
    },
    
    #' @description
    #' @param
    #' @return
    get_schema = function() {
      self$schema
    },
    
    #' @description
    #' @param
    #' @return
    disconnect = function() {
      message("Closing DB connection")
      poolClose(private$connection)
    },
    
    #' @description Get connection
    #' @param
    #' @return
    get_connection = function() {
      private$connection
    },
    
    #' @description
    #' @param
    #' @return
    append_to_table = function(table_name, value) {
      dbAppendTable(
        conn = private$connection,
        name = Id(
          schema = self$schema,
          table = table_name
        ),
        value = value
      )
    },
    
    get_facturacion = function() {
      dbGetQuery(private$connection, "SELECT * FROM facturacion")
    },
    
    get_productos = function() {
      dbGetQuery(private$connection, "SELECT * FROM productos")
    },
    
    get_last_number_factura = function() {
      dbGetQuery(private$connection, "SELECT factura_nro FROM facturacion ORDER BY factura_nro DESC LIMIT 1")
    },
    
    get_facturacion_by_factura = function(factura_nro) {
      dbGetQuery(
        private$connection, 
        glue_sql(.con = private$connection, "SELECT * FROM facturacion WHERE factura_nro = {factura_nro}")
      )
    },
    
    update_esta_liquidado = function(ids) {
      purrr::walk(
        ids, 
        ~ dbExecute(
          private$connection, 
          glue::glue_sql(
            .con = private$connection, "UPDATE facturacion SET esta_liquidado = TRUE WHERE id = {.x}"
          )
        )
      )
      
    },
    
    delete_facturacion_by_id = function(id) {
      dbExecute(
        private$connection,
        glue_sql(
          .con = private$connection,
          "DELETE
            FROM facturacion
            WHERE id = {id}"
        )
      )
    },
    
    update_productos = function(data) {
      conn <- private$connection
      pool <- poolCheckout(conn)
      self$set_schema()
      dbWithTransaction(
        pool,
        {
          dbExecute(conn, "TRUNCATE productos")
          self$append_to_table("productos", data)
          
        }
      )
      poolReturn(pool)
    }
    
  )
)


#' @export
db_manager <- DatabaseManager$new
