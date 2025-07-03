#' Initialize FaaSr configuration
#'
#' @description Creates a FaaSr configuration object with proper settings for
#' data storage and S3 connections based on the provided configuration.
#'
#' @param config A list containing configuration settings, including S3 bucket information.
#'               Should have elements `s3` (optional) and `run_config$use_s3`.
#'
#' @details This function initializes the global `.faasr` object with default data store
#' settings and configures S3 connections based on the provided configuration. It sets up
#' access keys, buckets, regions, and endpoints for each data store specified in the configuration.
#' The function also provides warnings if AWS credentials are missing when S3 usage is enabled.
#'
#' @return Returns (invisibly) the created `.faasr` configuration object.
#'
#' @examples
#' \dontrun{
#' config <- list(
#'   s3 = list(
#'     forecast_output = list(
#'       endpoint = "s3.us-east-1.amazonaws.com",
#'       bucket = "my-forecast-bucket"
#'     )
#'   ),
#'   run_config = list(
#'     use_s3 = TRUE
#'   )
#' )
#' initialize_faasr(config)
#' }
#' @keywords internal
#' @export
initialize_faasr <- function(config) {

  cat("=== DEBUG initialize_faasr START ===\n")
  cat("Received config structure:\n")
  cat("- config$s3 is NULL:", is.null(config$s3), "\n")
  if (!is.null(config$s3)) {
    cat("- config$s3 names:", paste(names(config$s3), collapse=", "), "\n")
    for(name in names(config$s3)) {
      cat("  - ", name, ":", 
          "endpoint=", config$s3[[name]]$endpoint %||% "NULL", 
          "bucket=", config$s3[[name]]$bucket %||% "NULL", "\n")
    }
  }
  cat("- config$run_config$use_s3:", config$run_config$use_s3 %||% "NULL", "\n")

  .faasr <<- list(
    DefaultDataStore = "restart",
    DataStores = list()
  )
  if (!is.null(config$s3)) {
    for (datastore_name in names(config$s3)) {

      endpoint_str <- config$s3[[datastore_name]]$endpoint
      datastore_config <- list(
        AccessKey = Sys.getenv("AWS_ACCESS_KEY_ID"),
        SecretKey = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
        Bucket = stringr::str_split_fixed(config$s3[[datastore_name]]$bucket, "/", n = 2)[1],
        Region = stringr::str_split_fixed(endpoint_str, pattern = "\\.", n = 3)[2],
        Anonymous = "FALSE"
      )


      if (!grepl("amazonaws", endpoint_str, ignore.case = TRUE)) {
        endpoint_url <- paste0("https://", sub("^[^\\.]+\\.", "", endpoint_str))
        datastore_config$Endpoint <- endpoint_url
      }
      else
      {
        endpoint_url <- paste0("")
        datastore_config$Endpoint <- endpoint_url

      }
      .faasr$DataStores[[datastore_name]] <- datastore_config
    }
  }

  # Warnings for AWS credentials
  if (Sys.getenv(x = "AWS_ACCESS_KEY_ID") == "" && config$run_config$use_s3 == TRUE) {
    warning(paste0("Use s3 is set to TRUE in the configuration file. ",
                   "AWS_ACCESS_KEY_ID environment variable is not set. S3 can still be used for downloading weather forecasts."))
  }

  if (Sys.getenv(x = "AWS_SECRET_ACCESS_KEY") == "" && config$run_config$use_s3 == TRUE) {
    warning(paste0("Use s3 is set to TRUE in the configuration file. ",
                   "AWS_SECRET_ACCESS_KEY environment variable is not set. S3 can still be used for downloading."))
  }

  cat("=== Created faasr_config ===\n")
  cat("- DefaultDataStore:", faasr_config$DefaultDataStore %||% "NULL", "\n")
  cat("- LoggingDataStore:", faasr_config$LoggingDataStore %||% "NULL", "\n")
  cat("- DataStores created:", paste(names(faasr_config$DataStores), collapse=", "), "\n")
  
  for(ds_name in names(faasr_config$DataStores)) {
    ds <- faasr_config$DataStores[[ds_name]]
    cat("  DataStore '", ds_name, "':\n")
    cat("    Endpoint:", ds$Endpoint %||% "NULL", "\n")
    cat("    Bucket:", ds$Bucket %||% "NULL", "\n")
    cat("    Region:", ds$Region %||% "NULL", "\n")
  }
  cat("=== DEBUG initialize_faasr END ===\n")
  

  return(invisible(.faasr))
}
