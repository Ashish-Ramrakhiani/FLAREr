#' @title Generating a list of flow files in the flare_tempdir

#' @param flow_forecast_dir location of the forecast files
#' @param flow_historical_dir location of the historical files
#' @param flow_type inflow or outflow
#' @param variables what variables are included in the flow file
#' @param out_dir the directory in which to put the flow files (e.g. flare_tempdir)
#' @param start_datetime start of simulation
#' @param end_datetime end of simulation
#' @param forecast_start_datetime start of the forecast period (break between historical + future periods)
#' @param forecast_horizon horizon
#' @param site_id site code
#' @param use_s3 logical
#' @param bucket s3 storage location
#' @param endpoint s3 storage location
#' @param local_directory local storage location
#' @param use_ler_vars T/F
#'
#' @return matrix of flow_file_names
#' @noRd
#'

create_flow_files <- function(flow_forecast_dir = NULL,
                              flow_historical_dir = NULL,
                              flow_type = "inflow",
                              variables = c("time", "FLOW", "TEMP", "SALT"),
                              out_dir,
                              start_datetime,
                              end_datetime = NA,
                              forecast_start_datetime = NA,
                              forecast_horizon = 0,
                              site_id,
                              use_s3 = FALSE,
                              bucket = NULL,
                              endpoint = NULL,
                              local_directory = NULL,
                              use_ler_vars = FALSE,
                              config = config) {

  lake_name_code <- site_id

  server_name <- if (flow_type == "inflow") {
    "inflow_drivers"
  } else if (flow_type == "outflow") {
    "outflow_drivers"
  } else {
    stop("Invalid flow_type. Please use 'inflow' or 'outflow'.")
  }



  round_level <- 10

  if (use_s3 && (is.null(bucket) || is.null(endpoint))) {
    stop("create_flow_files needs bucket and endpoint if use_s3=TRUE")
  }
  if (!use_s3 && is.null(local_directory) &&
      (!is.null(flow_forecast_dir) || !is.null(flow_historical_dir))) {
    stop("create_flow_files needs local_directory if use_s3=FALSE")
  }

  vars <- arrow_env_vars()
  on.exit(unset_arrow_vars(vars), add = TRUE)

  # bucket_tail only contributes to faasr_prefix on S3-bound dispatch;
  # it is safe to leave empty when bucket is NULL because mode=local
  # uses local_path and ignores the prefix.
  bucket_tail <- if (!is.null(bucket)) stringr::str_split_fixed(bucket, "/", n = 2)[2] else ""

  # `use_s3` here is the per-driver flow toggle (e.g.
  # config$flows$use_flows_s3), distinct from the global
  # config$run_config$use_s3. Forwarding it as mode_override lets a
  # single driver be local while global outputs still go to S3 / FaaSr.
  driver_mode <- if (use_s3) NULL else "local"

  future_s3 <- if (!is.null(flow_forecast_dir)) {
    flare_arrow_s3_bucket(
      server_name   = server_name,
      faasr_prefix  = file.path(bucket_tail, flow_forecast_dir),
      local_path    = if (!is.null(local_directory)) file.path(local_directory, flow_forecast_dir) else NULL,
      mode_override = driver_mode,
      config        = config
    )
  } else NULL

  hist_s3 <- if (!is.null(flow_historical_dir)) {
    flare_arrow_s3_bucket(
      server_name   = server_name,
      faasr_prefix  = file.path(bucket_tail, flow_historical_dir),
      local_path    = if (!is.null(local_directory)) file.path(local_directory, flow_historical_dir) else NULL,
      mode_override = driver_mode,
      config        = config
    )
  } else NULL

  # when does the simulation start and end?
  start_datetime <- lubridate::as_datetime(start_datetime)

  if (is.na(forecast_start_datetime)) {
    end_datetime <- lubridate::as_datetime(end_datetime)
    forecast_start_datetime <- end_datetime
  }  else {
    forecast_start_datetime <- lubridate::as_datetime(forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(forecast_horizon)
  }


  # Access the data
  if (!is.null(future_s3)) {

    future_df <- dplyr::collect(arrow::open_dataset(future_s3)) |>
      filter(datetime >= forecast_start_datetime,
             datetime <= end_datetime) |>
      dplyr::distinct()
  } else {
    future_df <- NULL # No future data
  }

  if (!is.null(hist_s3)) {
    hist_df <- dplyr::collect(arrow::open_dataset(hist_s3)) |>
      dplyr::filter(datetime < forecast_start_datetime,
                    datetime >= start_datetime) |>
      dplyr::distinct()


    if(!("parameter" %in% colnames(hist_df))){
      hist_df <- hist_df |> mutate(parameter = 1)
    }

    if("observation" %in% colnames(hist_df)){
      hist_df <- hist_df |> rename(prediction = observation)
    }

  } else {
    hist_df <- NULL # No historical data
  }



  if (!is.null(future_df) & !is.null(hist_df)) { # when there is historical and future data
    if (!setequal(unique(future_df$flow_number), unique(hist_df$flow_number))) { # Checks the data are consistent across the periods (same number of flows)
      print(tail(future_df))
      print(tail(hist_df))
      stop('need the same number of flows in historical and future periods')
    } else {
      num_flows <- max(future_df$flow_number)
    }


    future_ensemble_members <- unique(future_df$parameter)
    hist_ensemble_members <- unique(hist_df$parameter)


    # If there are a different number of ensemble members in the historical and future periods
    # this will resample the period with fewer ensemble members to match
    if (length(hist_ensemble_members) < length(future_ensemble_members)) {
      hist_ensemble_members <- sample(hist_ensemble_members, size = length(future_ensemble_members), replace = T)
    } else if (length(future_ensemble_members) < length(hist_ensemble_members)) {
      future_ensemble_members <- sample(future_ensemble_members, size = length(hist_ensemble_members), replace = T)
    }

    # Create an empty array to put the results in
    flow_file_names <- array(NA, dim = c(max(c(1, length(future_ensemble_members))),
                                         num_flows))

    for (j in 1:num_flows) {
      for (i in 1:length(future_ensemble_members)) {
        # generate the future period
        future_ens <- future_df |>
          dplyr::filter(flow_number == j,
                        parameter == future_ensemble_members[i],
                        datetime >= lubridate::as_date(forecast_start_datetime)) |>
          tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
          dplyr::rename(time = datetime) |>
          dplyr::select(dplyr::all_of(variables)) |>
          dplyr::mutate_if(where(is.numeric), list(~round(., round_level)))

        # generate the historical period
        hist_ens <- hist_df |>
          dplyr::filter(flow_number == j,
                        parameter == hist_ensemble_members[i],
                        datetime >= start_datetime,
                        datetime < lubridate::as_date(forecast_start_datetime)) |>
          tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
          dplyr::rename(time = datetime) |>
          dplyr::mutate(PHY_cyano_IN = 0,
                        PHY_green_IN = 0,
                        PHY_diatom_IN = 0,
                        PHY_cyano_IP = 0,
                        PHY_green_IP = 0,
                        PHY_diatom_IP = 0) |>
          dplyr::select(dplyr::all_of(variables)) |>
          dplyr::mutate_if(where(is.numeric), list(~round(., round_level)))

        # combine to single df
        flow <- dplyr::bind_rows(hist_ens,
                                 future_ens) |>
          arrange(time)

        if (use_ler_vars) {
          flow <- as.data.frame(flow)

          ler_vars_lookup <- c(Flow_metersCubedPerSecond = "FLOW",
                               Water_Temperature_celsius = "TEMP",
                               Salinity_practicalSalinityUnits = "SALT")

          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow[, 1] <- lubridate::with_tz(flow[, 1]) + lubridate::hours(hour_step)
          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow <- flow |>
            dplyr::select(any_of(c("time", "FLOW", "TEMP", "SALT"))) |>
            dplyr::rename(any_of(ler_vars_lookup))
        }        else {
          flow <- mutate(flow, time = lubridate::as_date(time))
        }

        flow_file_name <- file.path(out_dir, paste0(flow_type,
                                                    j, "_ens", i, ".csv"))
        flow_file_names[i, j] <- flow_file_name
        readr::write_csv(x = flow, file = flow_file_name,
                         quote = "none")
      }
    }
  } else if (!is.null(hist_df) & is.null(future_df)) { # do the same thing but when there is only historical data

    num_flows <- max(hist_df$flow_number)
    hist_ensemble_members <- unique(hist_df$parameter)


    flow_file_names <- array(NA, dim = c(max(c(1, length(hist_ensemble_members))),
                                         num_flows))


    for (j in 1:num_flows) {
      for (i in 1:length(hist_ensemble_members)) {
        hist_ens <- hist_df |>
          dplyr::filter(flow_number == j,
                        parameter == hist_ensemble_members[i],
                        datetime >= start_datetime,
                        datetime < lubridate::as_date(forecast_start_datetime)) |>
          tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
          dplyr::rename(time = datetime) |>
          dplyr::mutate(PHY_cyano_IN = 0,
                        PHY_green_IN = 0,
                        PHY_diatom_IN = 0,
                        PHY_cyano_IP = 0,
                        PHY_green_IP = 0,
                        PHY_diatom_IP = 0) |>
          dplyr::select(dplyr::all_of(variables)) |>
          dplyr::mutate_if(where(is.numeric), list(~round(., round_level)))


        flow <- hist_ens |>
          arrange(time)

        if (use_ler_vars) {
          flow <- as.data.frame(flow)

          ler_vars_lookup <- c(Flow_metersCubedPerSecond = "FLOW",
                               Water_Temperature_celsius = "TEMP",
                               Salinity_practicalSalinityUnits = "SALT")

          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow[, 1] <- lubridate::with_tz(flow[, 1]) + lubridate::hours(hour_step)
          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow <- flow |>
            dplyr::select(any_of(c("time", "FLOW", "TEMP", "SALT"))) |>
            dplyr::rename(any_of(ler_vars_lookup))
        }        else {
          flow <- mutate(flow, time = lubridate::as_date(time))
        }

        flow_file_name <- file.path(out_dir, paste0(flow_type,
                                                    j, "_ens", i, ".csv"))
        flow_file_names[i, j] <- flow_file_name
        readr::write_csv(x = flow, file = flow_file_name,
                         quote = "none")
      }
    }
  } else if (is.null(hist_df) & !is.null(future_df)) { # do the same thing but when there is only future data

    num_flows <- max(future_df$flow_number)
    future_ensemble_members <- unique(future_df$parameter)

    flow_file_names <- array(NA, dim = c(max(c(1, length(future_ensemble_members))),
                                         num_flows))

    for (j in 1:num_flows) {
      for (i in 1:length(future_ensemble_members)) {
        future_ens <- future_df |>
          dplyr::filter(flow_number == j,
                        parameter == future_ensemble_members[i],
                        datetime >= lubridate::as_date(forecast_start_datetime)) |>
          tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
          dplyr::rename(time = datetime) |>
          dplyr::mutate(PHY_cyano_IN = 0,
                        PHY_green_IN = 0,
                        PHY_diatom_IN = 0,
                        PHY_cyano_IP = 0,
                        PHY_green_IP = 0,
                        PHY_diatom_IP = 0) |>
          dplyr::select(dplyr::all_of(variables)) |>
          dplyr::mutate_if(where(is.numeric), list(~round(., round_level)))


        flow <- future_ens |>
          arrange(time)

        if (use_ler_vars) {
          flow <- as.data.frame(flow)

          ler_vars_lookup <- c(Flow_metersCubedPerSecond = "FLOW",
                               Water_Temperature_celsius = "TEMP",
                               Salinity_practicalSalinityUnits = "SALT")

          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow[, 1] <- lubridate::with_tz(flow[, 1]) + lubridate::hours(hour_step)
          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow <- flow |>
            dplyr::select(any_of(c("time", "FLOW", "TEMP", "SALT"))) |>
            dplyr::rename(any_of(ler_vars_lookup))
        }        else {
          flow <- mutate(flow, time = lubridate::as_date(time))
        }

        flow_file_name <- file.path(out_dir, paste0(flow_type,
                                                    j, "_ens", i, ".csv"))
        flow_file_names[i, j] <- flow_file_name
        readr::write_csv(x = flow, file = flow_file_name,
                         quote = "none")
      }
    }
  }

  if (!is.null(flow_historical_dir) | !is.null(flow_forecast_dir)) {
    return(flow_file_names)
  } else {
    return(NULL)
  }
}
