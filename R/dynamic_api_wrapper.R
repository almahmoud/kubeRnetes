#' Generate API Wrapper Functions
#'
#' This function dynamically generates wrapper functions for all operations in the API.
#'
#' @param api The API object returned by rapiclient::get_api()
#' @param operations A list of operations returned by rapiclient::get_operations()
#' @return A list of generated wrapper functions
#' @export
generate_api_wrappers <- function(api, operations) {
  wrappers <- list()
  
  for (op_name in names(operations)) {
    wrappers[[op_name]] <- create_wrapper_function(api, operations, op_name)
  }
  
  return(wrappers)
}

#' Create Wrapper Function
#'
#' This function creates a wrapper function for a specific API operation.
#'
#' @param api The API object returned by rapiclient::get_api()
#' @param operations A list of operations returned by rapiclient::get_operations()
#' @param op_name The name of the operation
#' @return A wrapper function for the specified operation
create_wrapper_function <- function(api, operations, op_name) {
  op <- operations[[op_name]]
  
  wrapper <- function(...) {
    tryCatch({
      result <- op(...)
      return(result)
    }, error = function(e) {
      stop(paste("Error in", op_name, ":", conditionMessage(e)))
    })
  }
  
  # Set function name and add basic documentation
  formals(wrapper) <- alist(...=)
  attr(wrapper, "name") <- op_name
  attr(wrapper, "description") <- paste("Wrapper for the", op_name, "API operation")
  
  return(wrapper)
}

#' Initialize API and Generate Wrappers
#'
#' This function initializes the API and generates wrapper functions for all operations.
#'
#' @param url The URL of the API
#' @param config Optional configuration for the API (default is NULL)
#' @param type Optional type specification for the API (default is NULL)
#' @return A list containing the API object and generated wrapper functions
#' @export
initialize_api_with_wrappers <- function(url, config = NULL, type = NULL) {
  api <- rapiclient::get_api(url, config, type)
  operations <- rapiclient::get_operations(api, config)
  wrappers <- generate_api_wrappers(api, operations)
  
  return(list(api = api, wrappers = wrappers))
}


#' List Available API Operations
#'
#' This function lists all available API operations and their basic information.
#'
#' @param api_wrappers The list of API wrappers returned by initialize_api_with_wrappers()
#' @return A data frame of operation names and descriptions
#' @export
list_api_operations <- function(api_wrappers) {
  wrappers <- api_wrappers$wrappers
  
  operations <- data.frame(
    operation = names(wrappers),
    description = sapply(wrappers, attr, "description"),
    stringsAsFactors = FALSE
  )
  
  return(operations)
}



