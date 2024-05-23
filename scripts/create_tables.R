box::use(
  pool[dbExecute, dbListTables, dbGetQuery],
  glue[glue, glue_sql],
  DBI[SQL, dbExecute],
  dplyr[...]
)

box::use(
  app/logic/db_manager[db_manager],
)

# Read environment variables
DB_SOURCE <- Sys.getenv("DB_SOURCE", unset = "local") #nolint
# Initialize schema
SCHEMA <- NULL


db_credentials <- config::get(
  value = DB_SOURCE,
  file = "app/database/credentials.yml"
)

SCHEMA <- db_credentials$schema
db_manager <- db_manager(db_credentials)
con <- db_manager$get_connection()


if (FALSE) {
  drop_sql <- c(
    glue("DROP TABLE IF EXISTS {SCHEMA}.facturacion;"),
    glue("DROP TABLE IF EXISTS {SCHEMA}.productos;")
  )
  
  # WARNING - Dropping tables
  for (drop_statement in drop_sql) {
    dbExecute(con, drop_statement)
  }
}

dbGetQuery(
  conn = con,
  glue(
    "CREATE TABLE {SCHEMA}.facturacion (
    id uuid NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    fecha date,
    articulo varchar(50) NOT NULL,
    cantidad int NOT NULL,
    talle int,
    color varchar(50),
    precio_venta_unitario numeric NOT NULL,
    precio_total numeric NOT NULL
    );",
    .con = con
  )
)

dbGetQuery(
  conn = con,
  glue(
    "CREATE TABLE {SCHEMA}.productos (
      articulo VARCHAR(50) NOT NULL,
      descripcion VARCHAR(200) NOT NULL,
      talle VARCHAR(50) NOT NULL,
      costo INT NOT NULL,
      p_minorista INT NOT NULL,
      p_mayorista INT NOT NULL
    );",
    .con = con
  )
)

lista_precios <- utils::read.csv2("data/Lista precios.csv", sep = ";", encoding = "UTF-8")

db_manager$append_to_table("productos", lista_precios)

db_manager$disconnect()
