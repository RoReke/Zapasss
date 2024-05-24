box::use(
  purrr[iwalk],
  R6[R6Class],
  dplyr[`%>%`],
  rlang[hash],
  rhino[log],
  shiny[reactiveVal, reactiveValues, getDefaultReactiveDomain],
)
#' R6 Class to manage the Application State
#'
StateManager <- R6Class( #nolint
  "StateManager",
  private = list(
    keys = NA_character_,
    values = reactiveValues(),
    previous_value = NULL,
    update_event = NULL,
    is_key_in_list = function(key) {
      isTRUE(key %in% names(private$values))
    },
    emit_update_event = function() {
      session <- getDefaultReactiveDomain()
      if (!is.null(session)) {
        if (is.null(private$update_event())) {
          private$update_event(0)
        } else {
          private$update_event(private$update_event() + 1)
        }
      } else {
        warning("You can emit events only from within a reactive context.")
      }
    }
  ),
  public = list(

    #' @description
    #' Instantiates a State Manager
    #' @return A State Manager object.
    initialize = function() {
      private$update_event <- reactiveVal()
      private$values <- reactiveValues()
      private$previous_value <- NULL
    },


    #' @description
    #' Gets a reactive update event that is triggered every time the State Manager
    #' is updated.
    #' @return A reactiveVal.
    get_update_event = function() {
      private$update_event
    },


    #' @description
    #' Sets a key:value pair in the State Manager.
    #' @param key A character. The key that will be later used to retrieve this value.
    #' @param value Any. The value that will be saved in the State Manager.
    #' @param raise_update_event A logical. If the user wants to trigger the update
    #' event or not.
    #' @return A State Manager object.
    set = function(key, value, db_field = NULL, raise_update_event = FALSE) {
      if (key %in% private$keys) {
        log$error("The statemanager key '{key}' is already in use")
        stop()
      }
      private$keys <- private$keys %>%  append(key)
      private$values[[key]] <- list(value = value)
      if (raise_update_event) {
        private$emit_update_event()
      }
      self
    },


    #' @description
    #' Updates the value of an existing key or creates the key if this didn't exist
    #' @param key A character. The key that will be later used to retrieve this value.
    #' @param value Any. The value that will be saved in the State Manager.
    #' @param raise_update_event A logical. If the user wants to trigger the update
    #' event or not.
    #' @return A State Manager object.
    update_value = function(key, value, raise_update_event = FALSE) {
      if (private$is_key_in_list(key)) {
        private$values[[key]][["value"]] <- value
        if (raise_update_event) {
          private$emit_update_event()
          private$values[[key]][["value"]] <- private$update_event
        }
      } else {
        warning(
          sprintf(
            "The value for %s cannot be updated because is not present in the State Manager. Use set before updating.", key #nolint
          )
        )
      }
    },


    #' @description
    #' Gets a value from the State Manager
    #' @param key A character. The key that will be later used to retrieve this value.
    #' @return Any value stored in the Manager.
    get = function(key) {
      if (private$is_key_in_list(key)) {
        private$values[[key]]
      } else {
        warning(
          sprintf(
            "The value for %s is not present in the State Manager, returning NULL", key
          )
        )
        NULL
      }
    },


    #' @description
    #' Gets a value from the State Manager
    #' @param key A character. The key that will be later used to retrieve this value.
    #' @return Any value stored in the Manager.
    save_previous_state = function(key) {
      if (private$is_key_in_list(key)) {
        private$previous_value[[key]][["value"]] <- self$get(key)
      } else {
        warning(
          sprintf(
            "The value for %s is not present in the State Manager, returning NULL", key
          )
        )
        NULL
      }
    },


    #' @description
    #' Check if a value change
    #' @param key A character. The key that will be later used to retrieve this value.
    #' @return boolean
    is_value_change = function(key) {
      !is.null(self$get(key)$value) &&
        (is.null(private$previous_value[[key]][["value"]]) && !is.null(self$get(key)$value) ||
           private$previous_value[[key]][["value"]] != self$get(key)$value)
    },


    #' @description
    #' listener stored values
    #' @param key A character. The key that will be later used to retrieve this value.
    #' @return Any value stored in the Manager.
    listen = function(key) {
      self$get(key)
    }
  )
)

#' @export
state_manager <- StateManager$new
