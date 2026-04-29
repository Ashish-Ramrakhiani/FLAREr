#' Validate FaaSr / S3 / local-mode configuration at setup time.
#'
#' Replaces the old `.faasr` global construction. Under the
#' `flare_io.R` dispatch design no global is required: the wrappers
#' read directly from `config$s3` and `config$run_config`. This
#' function exists to fail fast if the configuration is internally
#' inconsistent, and to surface a warning early when AWS credentials
#' are missing under `mode="s3"`.
#'
#' @param config FLAREr config list with `run_config$use_s3` and
#'   optionally `run_config$use_faasr`.
#' @return `NULL`, invisibly. Errors if `use_faasr=TRUE` while
#'   `use_s3=FALSE`. Warns if `use_s3=TRUE` and AWS credentials are
#'   not set in the environment.
#' @export
initialize_faasr <- function(config) {
  invisible(flare_io_mode(config))

  if (isTRUE(config$run_config$use_s3) &&
      Sys.getenv("AWS_ACCESS_KEY_ID") == "") {
    warning("use_s3=TRUE but AWS_ACCESS_KEY_ID is unset; ",
            "non-anonymous DataStores will fail.")
  }

  if (isTRUE(config$run_config$use_s3) &&
      Sys.getenv("AWS_SECRET_ACCESS_KEY") == "") {
    warning("use_s3=TRUE but AWS_SECRET_ACCESS_KEY is unset; ",
            "non-anonymous DataStores will fail.")
  }

  invisible(NULL)
}
