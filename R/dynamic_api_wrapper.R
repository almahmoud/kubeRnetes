#' Generate API Wrapper Functions
#'
#' This function dynamically generates wrapper functions for all operations 
#' in the API.
#'
#' @param api The API object returned by rapiclient::get_api()
#' @param operations A list of operations returned by 
#' rapiclient::get_operations()
#' @return A list of generated wrapper functions
#' @export
generate_api_wrappers <- function(api, operations) {
  wrappers <- lapply(names(operations), function(op_name) {
    create_wrapper_function(api, operations, op_name)
  })
  names(wrappers) <- names(operations)
  return(wrappers)
}

#' Create Wrapper Function
#'
#' This function creates a wrapper function for a specific API operation.
#'
#' @param api The API object returned by rapiclient::get_api()
#' @param operations A list of operations returned by 
#' rapiclient::get_operations()
#' @param op_name The name of the operation
#' @return A wrapper function for the specified operation
create_wrapper_function <- function(api, operations, op_name) {
  op <- operations[[op_name]]
  op_formals <- formals(op)

  wrapper <- function(...) {
    args <- list(...)
    named_args <- as.list(match.call())[-1]
    all_args <- modifyList(named_args, args)

    cat("Debug: Operation name:", op_name, "\n")
    cat("Debug: Arguments passed:", paste(names(all_args), collapse=", "), "\n")
    cat("Debug: Argument values:", paste(sapply(all_args, function(x) paste(deparse(x), collapse=" ")), collapse=", "), "\n")

    tryCatch({
      result <- do.call(op, all_args)
      if (httr::status_code(result) >= 400) {
        stop(sprintf("API request failed with status code %d: %s", 
                     httr::status_code(result), 
                     httr::content(result, "text")))
      }
      return(httr::content(result))
    }, error = function(e) {
      stop(sprintf("Error in %s: %s", op_name, conditionMessage(e)))
    })
  }

  formals(wrapper) <- c(op_formals, alist(... = ))
  
  attr(wrapper, "name") <- op_name
  attr(wrapper, "description") <- sprintf("Wrapper for the %s API operation", op_name)
  
  return(wrapper)
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

default_api_version <- "v1"
default_ns_api_version <- default_api_version
default_kind <- "Namespace"

default_ns_spec_yaml <- "
spec:
  finalizers:
    - kubernetes
"

default_ns_spec <- yaml::yaml.load(default_ns_spec_yaml)

get_wrapper_name <- function(api_version, kind, action) {
  # Remove non-alphanumeric characters from api_version and kind
  api_version <- gsub("[^[:alnum:]]", "", api_version)
  kind <- gsub("[^[:alnum:]]", "", kind)
  sprintf("%s%s%s", action, api_version, kind)
}

#' Initialize API and Generate Operations Wrapper
#'
#' This function initializes the API and generates wrapper functions for 
#' all operations.
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

  create_namespace_wrapper <- function(action) {
    function(...) {
      args <- list(...)
      api_version <- args$api_version %||% default_ns_api_version
      kind <- args$kind %||% default_kind
      wrapper_name <- get_wrapper_name(api_version, kind, action)
      wrapper_index <- which(tolower(names(wrappers)) == tolower(wrapper_name))
      if (length(wrapper_index) > 0) {
        wrapper_func <- wrappers[[wrapper_index]]
        
        # Check for required 'name' argument
        if (is.null(args$name)) {
          stop("Missing required argument: name")
        }
        
        # Get all possible arguments for this wrapper
        possible_args <- names(formals(wrapper_func))
        
        # Filter args to only include those accepted by the wrapper function
        filtered_args <- args[names(args) %in% possible_args]
        
        cat("Debug: Wrapper name:", wrapper_name, "\n")
        cat("Debug: Arguments being passed:", 
            paste(names(filtered_args), collapse=", "), "\n")
        cat("Debug: Argument values:", 
            paste(sapply(filtered_args, function(x) paste(deparse(x), collapse=" ")), collapse=", "), "\n")
        
        do.call(wrapper_func, filtered_args)
      } else {
        stop(sprintf("Wrapper function %s not found", wrapper_name))
      }
    }
  }

  namespaces <- list(
    create_namespace = create_namespace_wrapper("createCore"),
    update_namespace = create_namespace_wrapper("replaceCore"),
    delete_namespace = create_namespace_wrapper("deleteCore"),
    patch_namespace = create_namespace_wrapper("patchCore"),
    get_namespace = create_namespace_wrapper("readCore")
  )

  return(list(
    api = api, operations = operations, wrappers = wrappers, namespaces = namespaces
  ))
}

#' List Available API Operations
#'
#' This function lists all available API operations and their basic 
#' information.
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