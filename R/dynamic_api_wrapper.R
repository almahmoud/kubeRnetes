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
  op_formals <- formals(op)

  wrapper <- function(...) {
    args <- list(...)
    named_args <- as.list(match.call())[-1]
    all_args <- modifyList(named_args, args)

    tryCatch({
      result <- do.call(op, all_args)
      return(httr::content(result))
    }, error = function(e) {
      stop(paste("Error in", op_name, ":", conditionMessage(e)))
    })
  }

  # Set function parameters explicitly and include ...
  formals(wrapper) <- c(op_formals, alist(... = ))
  
  # Add attributes
  attr(wrapper, "name") <- op_name
  attr(wrapper, "description") <- paste("Wrapper for the", op_name, "API operation")
  
  return(wrapper)
}

# Add Namespace Functions
create_namespace <- function(wrappers, dryRun = NULL, fieldManager = NULL, apiVersion = NULL, kind = NULL, metadata = NULL, spec = NULL, status = NULL) {
  # Call the create namespace function from the k8s object
  wrappers$createCoreV1Namespace(dryRun = dryRun, fieldManager = fieldManager, apiVersion = apiVersion, kind = kind, metadata = metadata, spec = spec, status = status)
}

update_namespace <- function(wrappers, name, dryRun = NULL, fieldManager = NULL, apiVersion = NULL, kind = NULL, metadata = NULL, spec = NULL, status = NULL) {
  # Call the update namespace function from the k8s object
  wrappers$replaceCoreV1Namespace(name = name, dryRun = dryRun, fieldManager = fieldManager, apiVersion = apiVersion, kind = kind, metadata = metadata, spec = spec, status = status)
}

delete_namespace <- function(wrappers, name, dryRun = NULL, gracePeriodSeconds = NULL, orphanDependents = NULL, propagationPolicy = NULL, apiVersion = NULL, preconditions = NULL) {
  # Call the delete namespace function from the k8s object
  wrappers$deleteCoreV1Namespace(name = name, dryRun = dryRun, gracePeriodSeconds = gracePeriodSeconds, orphanDependents = orphanDependents, propagationPolicy = propagationPolicy, apiVersion = apiVersion, preconditions = preconditions)
}

patch_namespace <- function(wrappers, name, dryRun = NULL, fieldManager = NULL, force = NULL, patchData) {
  # Call the patch namespace function from the k8s object
  wrappers$patchCoreV1Namespace(name = name, dryRun = dryRun, fieldManager = fieldManager, force = force, patchData = patchData)
}

get_namespace <- function(wrappers, name, pretty = NULL) {
  # Call the get namespace function from the k8s object
  wrappers$readCoreV1Namespace(name = name, pretty = pretty)
}

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
  wrappers <- generate_api_wrappers(api, operations)

  # Add namespace functions to the api object
  namespaces <- list(
    create_namespace = function(dryRun = NULL, fieldManager = NULL, apiVersion = NULL, kind = NULL, metadata = NULL, spec = NULL, status = NULL) create_namespace(wrappers, dryRun, fieldManager, apiVersion, kind, metadata, spec, status),
    update_namespace = function(name, dryRun = NULL, fieldManager = NULL, apiVersion = NULL, kind = NULL, metadata = NULL, spec = NULL, status = NULL) update_namespace(wrappers, name, dryRun, fieldManager, apiVersion, kind, metadata, spec, status),
    delete_namespace = function(name, dryRun = NULL, gracePeriodSeconds = NULL, orphanDependents = NULL, propagationPolicy = NULL, apiVersion = NULL, preconditions = NULL) delete_namespace(wrappers, name, dryRun, gracePeriodSeconds, orphanDependents, propagationPolicy, apiVersion, preconditions),
    patch_namespace = function(name, dryRun = NULL, fieldManager = NULL, force = NULL, patchData) patch_namespace(wrappers, name, dryRun, fieldManager, force, patchData),
    get_namespace = function(name, pretty = NULL) get_namespace(wrappers, name, pretty)
  )

  return(list(api = api, operations = operations, wrappers = wrappers, namespaces = namespaces))
}

#' List Available API Operations
#'
#' This function lists all available API operations and their basic information.
#'
#' @param api The API list object returned by initialize_api()
#' @return A data frame of operation names and descriptions
#' @export
list_api_operations <- function(api) {
  wrappers <- api$wrappers
  
  operations <- data.frame(
    operation = names(wrappers),
    description = sapply(wrappers, attr, "description"),
    stringsAsFactors = FALSE
  )
  
  return(operations)
}
