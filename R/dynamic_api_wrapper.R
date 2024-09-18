#' Initialize API and Generate Operations Wrapper
#'
#' This function initializes the API and generates wrapper functions for all operations.
#'
#' @param url The URL of the API
#' @param config Optional configuration for the API (default is NULL)
#' @param type Optional type specification for the API (default is NULL)
#' @return A list containing the API object and generated wrapper functions
#' @export
initialize_api <- function(url, config = NULL, type = NULL) {
  api <- rapiclient::get_api(url, config, type)
  operations <- rapiclient::get_operations(api, .headers = config$headers)
  
  return(list(api = api, operations = operations))
}


#' List Available API Operations
#'
#' This function lists all available API operations and their basic information.
#'
#' @param api The API objected returned by initialize_api()
#' @return A data frame of operation names
#' @export
list_api_operations <- function(api) {
  wrappers <- api_wrappers$wrappers
  
  operations <- data.frame(
    operation = names(operations)
  )
  
  return(operations)
}



