###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2018}  {Ryan Murphy, Daniel Schlaepfer,
#    William Lauenroth, John Bradford}
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################


#' Estimate coefficients for use by \var{SOILWAT2} weather generator
#'
#' Estimates coefficients for the two site-specific files
#' \var{mkv_covar.in} and \var{mkv_prob.in} required by the first-order
#' Markov weather generator in \var{SOILWAT2} \var{> v4.2.5}.
#'
#' @section Notes: This code is a complete overhaul compared to the version
#'   from \var{rSFSTEP2} on \code{2019-Feb-10}
#'   commit \var{cd9e161971136e1e56d427a4f76062bbb0f3d03a}
#'   \url{https://github.com/DrylandEcology/rSFSTEP2}.
#'
#' @section Notes: This function will produce \code{NA}s in the output if there
#'   are insufficient weather observation in the input data \code{weatherData}
#'   for a specific day or week of the year. Such \code{NA}s will cause a
#'   \var{SOILWAT2} run to fail (potentially non-graciously and
#'   with non-obvious error messages). To avoid that, this function offers
#'   imputation approaches in order to fill in those failed coefficient
#'   estimates; see \code{\link[rSW2utils]{impute_df}}, but please note that
#'   any such imputation likely introduces biases in the generated weather.
#'
#' @section Details: Most users will likely want to set \code{propagate_NAs} to
#'   \code{FALSE}. Note: \code{propagate_NAs} corresponds to \code{!na.rm}
#'   from previous versions of this function with a different default value.
#'   Consider an example: a the 30-year long input \code{weatherData} is
#'   complete except for missing values on Jan 1, 2018.
#'   \itemize{
#'     \item If \code{propagate_NAs} is set to \code{TRUE}, then the
#'     coefficients for day 1 and week 1 of year will be \code{NA} --
#'     despite all the available data. In this case, the missing coefficients
#'     for day 1 and week 1 of year will be imputed.
#'   \item If \code{propagate_NAs} is set to \code{FALSE}, then the coefficients
#'     for day 1 and week 1 of year will be calculated based on the non-missing
#'     values for that day respectively that week of year. No imputation occurs.
#'   }
#'
#' @param weatherData A list of elements of class
#'   \code{\linkS4class{swWeatherData}} or a \code{data.frame} as returned by
#'   \code{\link{dbW_weatherData_to_dataframe}}.
#' @param WET_limit_cm A numeric value. A day with more precipitation than
#'   this value is considered \var{wet} instead of \var{dry}. Default is 0.
#'   This values should be equal to the corresponding value used in
#'   \var{SOILWAT2}'s function \code{SW_MKV_today}.
#' @param propagate_NAs A logical value. If \code{TRUE}, then missing weather
#'   values in the input \code{weatherData} are excluded; if \code{FALSE}, then
#'   missing values are propagated to the estimation. See Details.
#' @inheritParams set_missing_weather
#' @inheritParams rSW2utils::impute_df
#'
#' @return A list with two named elements:
#'   \describe{
#'     \item{\code{mkv_doy}}{A data.frame with 366 rows (day of year) and
#'        5 columns: \describe{
#'          \item{DOY}{Day of year.}
#'          \item{p_W_W}{Probability that doy is wet if the previous day
#'            (doy - 1) was wet.}
#'          \item{p_W_D}{Probability that doy is wet if the previous day
#'            (doy - 1) was dry.}
#'          \item{PPT_avg}{Average amount of precipitation (centimeters)
#'            on doy if it is wet.}
#'          \item{PPT_sd}{Standard deviation of amount of precipitation
#'            (centimeters) on doy if it is wet.}
#'        }}
#'     \item{\code{mkv_woy}}{A data.frame with 53 rows (\var{SOILWAT2}
#'       weeks of year, i.e., counted as consecutive \var{heptads} of days) and
#'       11 columns: \describe{
#'         \item{WEEK}{Week of year.}
#'         \item{wTmax_C}{Average daily maximum temperature (C) for week.}
#'         \item{wTmin_C}{Average daily minimum temperature (C) for week.}
#'         \item{var_MAX}{Variance of daily maximum temperature for week.}
#'         \item{cov_MAXMIN}{Covariance of daily maximum and minimum
#'           temperatures for week.}
#'         \item{cov_MINMAX}{Identical to \code{cov_MAXMIN}.}
#'         \item{var_MIN}{Variance of daily minimum temperature for week.}
#'         \item{CF_Tmax_wet}{Difference between average daily maximum
#'           temperature (C) for wet days of week and \code{wTmax_C}.}
#'         \item{CF_Tmax_dry}{Difference between average daily maximum
#'           temperature (C) for dry days of week and \code{wTmax_C}.}
#'         \item{CF_Tmin_wet}{Same as \code{CF_Tmax_wet} but for daily
#'           minimum temperature.}
#'         \item{CF_Tmin_dry}{Same as \code{CF_Tmax_dry} but for daily
#'           minimum temperature.}
#'       }}
#'   }
#'
#' @seealso \code{\link{print_mkv_files}} to print values to \var{SOILWAT2}
#'   compatible files. \code{\link{swMarkov_Prob}} and
#'   \code{\link{swMarkov_Conv}} to extract/replace values in a \pkg{rSOILWAT2}
#'   input object of class \code{\linkS4class{swInputData}}.
#'
#' @examples
#' res1 <- dbW_estimate_WGen_coefs(rSOILWAT2::weatherData)
#' wdata <- data.frame(dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))
#' res2 <- dbW_estimate_WGen_coefs(wdata)
#'
#' sw_in <- rSOILWAT2::sw_exampleData
#' swMarkov_Prob(sw_in) <- res2[["mkv_doy"]]
#' swMarkov_Conv(sw_in) <- res2[["mkv_woy"]]
#'
#' @export
dbW_estimate_WGen_coefs <- function(weatherData, WET_limit_cm = 0,
  propagate_NAs = FALSE, valNA = NULL,
  imputation_type = c("none", "mean", "locf"),
  imputation_span = 5L) {

  # daily weather data
  if (inherits(weatherData, "list") &&
      all(sapply(weatherData, inherits, what = "swWeatherData"))) {
    wdata <- data.frame(dbW_weatherData_to_dataframe(weatherData,
      valNA = valNA))
  } else {
    wdata <- data.frame(set_missing_weather(weatherData, valNA = valNA))
  }

  n_days <- nrow(wdata)

  imputation_type <- match.arg(imputation_type)

  na.rm <- !propagate_NAs

  #-----------------------------------------------------------------------------
  #------ calculate mkv_prob.in
  icol_day <- grep("DOY|Day", colnames(wdata), ignore.case = TRUE,
    value = TRUE)

  #--- calculate WET days
  wdata[["WET"]] <- wdata[["PPT_cm"]] > WET_limit_cm
  wdata[["WET_yesterday"]] <- c(wdata[["WET"]][n_days], wdata[["WET"]][-n_days])


  #--- calculate WET days that follow a WET day (WW) and
  #              WET days that follow a DRY day (WD)
  wdata[["WW"]] <- wdata[["WET"]] & wdata[["WET_yesterday"]]
  wdata[["WD"]] <- wdata[["WET"]] & !wdata[["WET_yesterday"]]


  #--- output container: dataframe for storing mkv_prob.in data
  doys <- 366  # see SOILWAT2 constant `MAX_DAYS`
  outs <- c("DOY", "p_W_W", "p_W_D", "PPT_avg", "PPT_sd")
  mkv_prob <- data.frame(matrix(NA, nrow = doys, ncol = length(outs),
    dimnames = list(NULL, outs)))
  mkv_prob[, "DOY"] <- seq_len(doys)

  #--- mean/sd of precipitation across years for doy i if it is a wet day
  temp <- by(wdata[, c("WET", "PPT_cm")], INDICES = wdata[, icol_day],
    function(x) {
      # if `na.rm` is TRUE, then remove NAs in `WET`; if only NAs -> PPT_avg = 0
      # if `na.rm` is FALSE, then any NA propagates to PPT_avg = NA
      iswet <- if (na.rm) which(x[, "WET"]) else x[, "WET"]
      ppt <- x[iswet, "PPT_cm"]
      if (length(ppt) > 0) {
        c(PPT_avg = mean(ppt, na.rm = na.rm),
          PPT_sd = sd(ppt, na.rm = na.rm))
      } else {
        # there are no wet days for this DOY; thus PPT = 0
        c(PPT_avg = 0, PPT_sd = 0)
      }
    })
  mkv_prob[, c("PPT_avg", "PPT_sd")] <- do.call(rbind, temp)


  #--- wetprob = p(wet|wet) = "p_W_W" #nolint
  #    = probability that it precipitates today if it was wet
  #      (precipitated) yesterday
  #    dryprob = p(wet|dry) = "p_W_D" #nolint
  #    = probability that it precipitates today if it was dry
  #      (did not precipitate) yesterday
  temp <- by(wdata[, c("WET", "WET_yesterday", "WW", "WD")],
    INDICES = wdata[, icol_day], function(x) {
      # p(wet): probability that today is wet
      p_W <- mean(x[, "WET"], na.rm = na.rm)
      # number of DOY = i that follow a wet day
      n_Wy <- sum(x[, "WET_yesterday"], na.rm = na.rm)
      # number of DOY = i that follow a dry day
      n_Dy <- sum(!x[, "WET_yesterday"], na.rm = na.rm)

      c(
        p_W_W = if (isTRUE(n_Wy > 0)) {
            # `p(wet|wet)` estimated as the number of years with doy being wet
            # given previous day is wet divided by the number of years with
            # the previous day being wet
            sum(x[, "WW"], na.rm = na.rm) / n_Wy
          } else {
            # `p(wet|wet)` approximated with frequency that today is wet for
            # data where yesterday is never wet (avoid division by zero);
            # this value is likely near 0 because p(wet yesterday) = 0
            # and p(wet today) ~ p(wet yesterday)
            p_W
          },
        p_W_D = if (isTRUE(n_Dy > 0)) {
            # `p(wet|dry)` estimated as the number of years with doy being wet
            # given previous day is dry divided by the number of years with
            # the previous day being dry
            sum(x[, "WD"], na.rm = na.rm) / n_Dy
          } else {
            # `p(wet|dry)` approximated with frequency that today is wet for
            # data where yesterday is never dry (avoid division by zero);
            # this value is likely near 1 because p(wet yesterday) = 1
            # and p(wet today) ~ p(wet yesterday)
            p_W
          })
    })
  mkv_prob[, c("p_W_W", "p_W_D")] <- do.call(rbind, temp)

  #--- Make sure probability values are well formed: 0 <= p <= 1
  ids_bad0 <- which(mkv_prob[, "p_W_W"] < 0)
  mkv_prob[ids_bad0, "p_W_W"] <- 0

  ids_bad1 <- which(mkv_prob[, "p_W_W"] > 1)
  mkv_prob[ids_bad1, "p_W_W"] <- 1


  #--- Check that no missing coefficients
  if (anyNA(mkv_prob)) {
    ids_baddoy <- mkv_prob[apply(mkv_prob, 1, anyNA), "DOY"]

    msg <- paste0("values for n = ", length(ids_baddoy), " DOYs: ",
      paste(ids_baddoy, collapse = ", "))

    if (imputation_type == "none") {
      warning("Insufficient weather data to estimate ", msg)
    } else {
      message("Impute missing `mkv_prob` ", msg)
      mkv_prob <- rSW2utils::impute_df(mkv_prob,
        imputation_type = imputation_type,
        imputation_span = imputation_span,
        cyclic = TRUE
      )
    }
  }



  #-----------------------------------------------------------------------------
  #------ mkv_covar.in

  #--- week as interpreted by SOILWAT2 function `Doy2Week`
  wdata[["WEEK"]] <- 1 + floor((wdata[[icol_day]] - 1) / 7)

  #--- output container: dataframe for storing mkv_cov.in data
  weeks <- 53 # see SOILWAT2 constant `MAX_WEEKS`
  outs <- c("WEEK", "wTmax_C", "wTmin_C",
    "var_MAX", "cov_MAXMIN", "cov_MINMAX", "var_MIN",
    "CF_Tmax_wet", "CF_Tmax_dry", "CF_Tmin_wet", "CF_Tmin_dry")
  mkv_cov <- data.frame(matrix(NA, nrow = weeks, ncol = length(outs),
    dimnames = list(NULL, outs)))


  #--- Aggregate for each week
  mkv_cov[, "WEEK"] <- seq_len(weeks)

  # Average weekly temperature values
  mkv_cov[, "wTmax_C"] <- tapply(wdata[["Tmax_C"]], wdata[["WEEK"]], mean,
    na.rm = na.rm)
  mkv_cov[, "wTmin_C"] <- tapply(wdata[["Tmin_C"]], wdata[["WEEK"]], mean,
    na.rm = na.rm)

  # Variance-covariance values among maximum and minimum temperature
  temp <- by(wdata[, c("Tmax_C", "Tmin_C")], wdata[["WEEK"]], cov,
    use = if (na.rm) "na.or.complete" else "everything")
  temp <- sapply(temp, function(x) c(x[1, 1], x[1, 2], x[2, 1], x[2, 2]))
  mkv_cov[, "var_MAX"] <- temp[1, ]
  mkv_cov[, "cov_MAXMIN"] <- temp[2, ]
  mkv_cov[, "cov_MINMAX"] <- temp[3, ]
  mkv_cov[, "var_MIN"] <- temp[4, ]


  #--- Temperature correction factors (delta values)
  # Used to correct random temperature values based on average conditions
  # if that target day is wet or dry (e.g., overcast weather tends to
  # increase minimum daily temperature and decrease maximum daily tempature)
  temp <- by(wdata[, c("WET", "Tmax_C", "Tmin_C")], INDICES = wdata[, "WEEK"],
    function(x) {
      # if `na.rm` is TRUE, then consider `WET` = NA as FALSE
      # if `na.rm` is FALSE, then propagate NAs in `WET` -> neither wet nor dry
      iswet <- if (na.rm) {
          which_wet <- which(x[, "WET"]) # numeric vector
          out <- rep(FALSE, length(x[, "WET"]))
          # only days where 'WET' is TRUE are considered wet
          out[which_wet] <- TRUE
          out # logical vector same length as x[, "WET"]
        } else {
          x[, "WET"] # logical vector
        }
      isanywet <- isTRUE(any(iswet, na.rm = na.rm))
      # previously isdry became all FALSE if na.rm = TRUE (because then iswet
      # was numeric  vector with all positive digits)
      isdry <- !iswet
      isanydry <- isTRUE(any(isdry, na.rm = na.rm))

      # if no wet/dry days in week of year, then use overall mean instead
      # of conditional mean (i.e., given wet/dry)
      c(Tmax_mean_wet = if (isanywet) {
            mean(x[iswet, "Tmax_C"], na.rm = na.rm)
          } else {
            mean(x[, "Tmax_C"], na.rm = na.rm)
          },
        Tmax_mean_dry = if (isanydry) {
            mean(x[isdry, "Tmax_C"], na.rm = na.rm)
          } else {
            mean(x[, "Tmax_C"], na.rm = na.rm)
          },
        Tmin_mean_wet = if (isanywet) {
            mean(x[iswet, "Tmin_C"], na.rm = na.rm)
          } else {
            mean(x[, "Tmin_C"], na.rm = na.rm)
          },
        Tmin_mean_dry = if (isanydry) {
            mean(x[isdry, "Tmin_C"], na.rm = na.rm)
          } else {
            mean(x[, "Tmin_C"], na.rm = na.rm)
          }
      )
    }
  )
  temp <- do.call(rbind, temp)

  mkv_cov[, "CF_Tmax_wet"] <- temp[, "Tmax_mean_wet"] - mkv_cov[, "wTmax_C"]
  mkv_cov[, "CF_Tmax_dry"] <- temp[, "Tmax_mean_dry"] - mkv_cov[, "wTmax_C"]
  mkv_cov[, "CF_Tmin_wet"] <- temp[, "Tmin_mean_wet"] - mkv_cov[, "wTmin_C"]
  mkv_cov[, "CF_Tmin_dry"] <- temp[, "Tmin_mean_dry"] - mkv_cov[, "wTmin_C"]


  #--- Check that no missing coefficients
  if (anyNA(mkv_cov)) {
    ids_badweek <- mkv_cov[apply(mkv_cov, 1, anyNA), "WEEK"]

    msg <- paste0("values for n = ", length(ids_badweek), " weeks: ",
      paste(ids_badweek, collapse = ", "))

    if (imputation_type == "none") {
      warning("Insufficient weather data to estimate ", msg)
    } else {
      message("Impute missing `mkv_cov` ", msg)
      mkv_cov <- rSW2utils::impute_df(mkv_cov,
        imputation_type = imputation_type,
        imputation_span = imputation_span,
        cyclic = TRUE
      )
    }
  }


  list(mkv_woy = mkv_cov, mkv_doy = mkv_prob)
}


#' Print Markov weather generator files as required by \var{SOILWAT2}
#'
#' @param mkv_doy A data.frame. The same named output element of
#'   \code{\link{dbW_estimate_WGen_coefs}}.
#' @param mkv_woy A data.frame. The same named output element of
#'   \code{\link{dbW_estimate_WGen_coefs}}.
#'
#' @seealso \code{\link{dbW_estimate_WGen_coefs}} to
#'   calculate the necessary values based on daily weather data.
#'
#' @return This function is called for its side effect, i.e., writing values
#'   to files \var{\dQuote{mkv_prob.in}} and \var{\dQuote{mkv_covar.in}}.
#'
#' @examples
#' path <- tempdir()
#'
#' res <- dbW_estimate_WGen_coefs(rSOILWAT2::weatherData)
#' print_mkv_files(
#'   mkv_doy = res[["mkv_doy"]],
#'   mkv_woy = res[["mkv_woy"]],
#'   path = path)
#'
#' ## Cleanup
#' unlink(list.files(path), force = TRUE)
#'
#' @export
print_mkv_files <- function(mkv_doy, mkv_woy, path, digits = 5) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  colnames(mkv_doy)[1] <- paste0("#", colnames(mkv_doy)[1])
  utils::write.table(
    format(mkv_doy, digits = digits),
    file = file.path(path, "mkv_prob.in"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  colnames(mkv_woy)[1] <- paste0("#", colnames(mkv_woy)[1])
  utils::write.table(
    format(mkv_woy, digits = digits),
    file = file.path(path, "mkv_covar.in"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  invisible(TRUE)
}



# Check that \code{weather} meets expectations
check_weather <- function(weather, required_variables) {
  stopifnot(
    length(dim(weather)) == 2,
    sapply(required_variables, function(p)
      length(grep(p, x = colnames(weather))) == 1)
  )
}

# Aggregate daily weather for each time step
prepare_weather <- function(data_daily,
  time_steps = c("Year", "Month", "Week", "Day"), na.rm = FALSE) {

  weather_list <- list()
  id_daily <- "Day" == time_steps

  for (it in time_steps[!id_daily]) {
    weather_list[[it]] <- dbW_dataframe_aggregate(data_daily, it, na.rm = na.rm)
  }

  weather_list[["Day"]] <- data_daily
  weather_list
}

# Prepare weather data object for \code{\link{compare_dailyweather}}
prepare_weather_for_comparison <- function(weather,
  time_steps = c("Year", "Month", "Week", "Day"), na.rm = FALSE) {
  req_vars <- c("Year", "Tmax_C", "Tmin_C", "PPT_cm")

  if (length(weather) == length(time_steps) &&
      inherits(weather, "list")) {
    # if a list, then four suitable elements one for each time step
    stopifnot(time_steps %in% names(weather))
    for (it in time_steps) {
      required_variables <- c(req_vars, switch(it,
        Year = NULL, Month = "Month", Week = "Week", Day = "DOY|Day"))
      check_weather(weather[[it]], required_variables)
      if (it == "Day") {
        colnames(weather[[it]])[2] <- "Day"
      }
    }

    res <- weather

  } else if (inherits(weather, c("matrix", "data.frame"))) {
    # if a two-dim object, then it has to be daily data and we need to
    # aggregate for each time step
    check_weather(weather, c("DOY|Day", req_vars))
    colnames(weather)[2] <- "Day"
    res <- prepare_weather(weather, na.rm = na.rm)

  } else {
    stop("Structure of `weather` not suitable")
  }

  res
}

#' Compare two weather datasets: produces comparison plots for means, quantiles,
#' and Markov weather generator input parameters for all time steps
#'
#' @section Notes: The number of days represented by \code{ref_weather} and by
#'   \code{weather} does not need to be the same.
#' @section Notes: See also the Weather generator integration tests (in file
#'   \var{"tests/testthat/test_WeatherGenerator_functionality.R}).
#'
#' @param ref_weather A \code{list} of or a two-dimensional numerical object,
#'   e.g., \code{data.frame} or \code{matrix}. If a \code{list} then four
#'   suitable elements one for each \code{time_step}. Otherwise, daily weather
#'   data with columns \var{Year}, \var{DOY}, \var{Tmax_C}, \var{Tmin_C},
#'   and \var{PPT_cm}; for instance, the result of
#'   function \code{\link{dbW_weatherData_to_dataframe}}. This represents the
#'   reference weather against which \code{weather} is compared.
#' @param weather A \code{list} of elements such as described for
#'   \code{ref_weather} or an object such as \code{ref_weather}. This represents
#'   the weather data (potentially from many sites and/or runs) that are
#'   compared against \code{ref_weather}.
#' @param N An integer number representing the number of runs or sites contained
#'   in \code{weather}.
#' @inheritParams dbW_estimate_WGen_coefs
#' @param path A character string. The directory path in which output figures
#'   will be saved.
#' @param tag A character string to uniquely identify a set of output figures.
#'
#' @return This function is called for its side effects of producing figures
#'   that are saved as \var{png} files on the disk.
#'
#' @examples
#' path <- tempdir()
#'
#' ## Example with default rSOILWAT2 weather data
#' w0 <- dbW_weatherData_to_dataframe(rSOILWAT2::weatherData)
#' w1 <- w0
#' w1[, "Tmax_C"] <- w1[, "Tmax_C"] + 2
#' w1[, "Tmin_C"] <- w1[, "Tmin_C"] - 2
#'
#' compare_weather(
#'   ref_weather = w0,
#'   weather = w1,
#'   N = 1,
#'   path = path,
#'   tag = "Example1-Silly"
#' )
#'
#' ## Example with STEPWAT2 output data averaged across iterations (`-o` option)
#' fname_main <- "file_STEPWAT2_main.csv"
#' fname_dev <- "file_STEPWAT2_dev.csv"
#' if (all(file.exists(fname_main, fname_dev))) {
#'   cols_STEPWAT2 <- c(
#'     "Year", "Day", "PRECIP_ppt_Mean", "TEMP_max_C_Mean", "TEMP_min_C_Mean"
#'   )
#'   cols_rSOILWAT2 <- c("Year", "Day", "PPT_cm", "Tmax_C", "Tmin_C")
#'   w0 <- utils::read.csv(fname_main)[, cols_STEPWAT2]
#'   colnames(w0) <- cols_rSOILWAT2
#'   w1 <- utils::read.csv(fname_dev)[, cols_STEPWAT2]
#'   colnames(w1) <- cols_rSOILWAT2
#'
#'   # Note: Since values are averages across many iterations, most days
#'   # have average precipitation values > 0; thus, we need to adjust
#'   # `WET_limit_cm` accordingly (here, with a guess)
#'   compare_weather(
#'     ref_weather = w0,
#'     weather = w1,
#'     N = 1,
#'     WET_limit_cm = 0.1,
#'     path = path,
#'     tag = "Example2-STEPWAT2"
#'   )
#' }
#'
#' ## Cleanup
#' unlink(list.files(path), force = TRUE)
#'
#' @export
compare_weather <- function(ref_weather, weather, N, WET_limit_cm = 0,
  path, tag) {

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  time_steps <- c("Year", "Month", "Week", "Day")
  weather_vars <- c("Tmax_C", "Tmin_C", "PPT_cm")

  #--- Prepare reference weather
  ref_df <- list(prepare_weather_for_comparison(ref_weather, na.rm = TRUE))

  #--- Prepare other weather
  if (N > 1 && length(weather) == N && inherits(weather, "list") &&
      !all(time_steps %in% names(weather))) {
    # many runs/sites: each element consists of an element as `ref_weather`
    comp_df <- lapply(weather, prepare_weather_for_comparison)

  } else if (N == 1) {
    # one run/site
    comp_df <- list(prepare_weather_for_comparison(weather, na.rm = TRUE))

  } else {
    stop("Structure of `weather` not suitable")
  }


  #------- OUTPUTS
  #--- Compare means and SDs: boxplots
  calculate_MeansSDs <- function(data) {
    temp <- lapply(weather_vars, function(var)
      sapply(time_steps, function(ts)
        sapply(data, function(x) {
          temp <- x[[ts]][, var]
          c(mean(temp, na.rm = TRUE), sd(temp, na.rm = TRUE))
        })
      ))

    array(unlist(temp),
      dim = c(2, length(data), length(time_steps), length(weather_vars)),
      dimnames = list(c("mean", "sd"), names(data), time_steps, weather_vars))
  }

  foo_bxp <- function(data, ref_data, ylab, legend = FALSE) {
    if (is.null(dim(data))) {
      data <- matrix(data, nrow = 1, dimnames = list(NULL, names(data)))
    }
    stopifnot(ncol(data) == length(ref_data))
    ylim <- range(data, ref_data, na.rm = TRUE)

    if (all(is.finite(ylim))) {
      graphics::boxplot(data, ylim = ylim, ylab = ylab)
      graphics::points(seq_along(ref_data), ref_data, col = "red", pch = 4,
        lwd = 2)

      if (legend) {
        graphics::legend("topright", legend = c("Reference", "Weather"),
          col = c("red", "black"), pch = c(4, 16), pt.lwd = 2)
      }

    } else {
      graphics::plot.new()
    }
  }


  # Calculate means and sds
  ref_MeanSD <- calculate_MeansSDs(ref_df)
  comp_MeanSD <- calculate_MeansSDs(comp_df)

  # Make figure
  panels <- c(3, 2)
  grDevices::png(units = "in", res = 150,
    height = 3 * panels[1], width = 6 * panels[2],
    file = file.path(path, paste0(tag, "_CompareWeather_Boxplots_MeanSD.png")))
  par_prev <- graphics::par(mfrow = panels, mar = c(2, 2.5, 0.5, 0.5),
    mgp = c(1, 0, 0), tcl = 0.3, cex = 1)

  foo_bxp(data = comp_MeanSD["mean", , , "PPT_cm"],
    ref_data = ref_MeanSD["mean", , , "PPT_cm"],
    ylab = "Mean Precipitation (cm)", legend = TRUE)
  foo_bxp(data = comp_MeanSD["sd", , , "PPT_cm"],
    ref_data = ref_MeanSD["sd", , , "PPT_cm"],
    ylab = "SD Precipitation (cm)")

  foo_bxp(data = comp_MeanSD["mean", , , "Tmax_C"],
    ref_data = ref_MeanSD["mean", , , "Tmax_C"],
    ylab = "Mean Daily Max Temperature (C)")
  foo_bxp(data = comp_MeanSD["sd", , , "Tmax_C"],
    ref_data = ref_MeanSD["sd", , , "Tmax_C"],
    ylab = "SD Daily Max Temperature (C)")

  foo_bxp(data = comp_MeanSD["mean", , , "Tmin_C"],
    ref_data = ref_MeanSD["mean", , , "Tmin_C"],
    ylab = "Mean Daily Min Temperature (C)")
  foo_bxp(data = comp_MeanSD["sd", , , "Tmin_C"],
    ref_data = ref_MeanSD["sd", , , "Tmin_C"],
    ylab = "SD Daily Min Temperature (C)")

  graphics::par(par_prev)
  grDevices::dev.off()


  #--- Quantile-quantile comparisons: scatterplots
  foo_qq <- function(data, ref_data, var, time, lab, legend = FALSE) {

    vlim <- range(sapply(c(ref_data, data),
      function(x) range(x[[time]][, var], na.rm = TRUE)))

    if (all(is.finite(vlim))) {
      probs <- seq(0, 1, length.out = 1000)

      x <- quantile(ref_data[[1]][[time]][, var], probs = probs,
        na.rm = TRUE)
      graphics::plot(x, x, type = "n", xlim = vlim, ylim = vlim, asp = 1,
        xlab = paste0(time, "ly : reference ", lab),
        ylab = paste0(time, "ly : weather ", lab))
      for (k in seq_along(data)) {
        qy <- quantile(data[[k]][[time]][, var], probs = probs,
          na.rm = TRUE)
        graphics::points(x, qy, pch = 46)
      }

      graphics::abline(h = 0, lty = 2)
      graphics::abline(v = 0, lty = 2)
      graphics::segments(x0 = vlim[1], y0 = vlim[1],
        x1 = vlim[2], y1 = vlim[2], col = "red", lwd = 2)


      if (legend) {
        graphics::legend("topleft", legend = c("Reference", "Weather"),
          col = c("red", "black"), pch = c(NA, 16), pt.lwd = 2,
          lty = c(1, NA), lwd = 2, merge = TRUE)
      }

    } else {
      graphics::plot.new()
    }
  }

  # Make figure
  panels <- c(length(time_steps), 3)
  grDevices::png(units = "in", res = 150,
    height = 3 * panels[1], width = 3 * panels[2],
    file = file.path(path, paste0(tag, "_CompareWeather_QQplots.png")))
  par_prev <- graphics::par(mfrow = panels, mar = c(2, 2.5, 0.5, 0.5),
    mgp = c(1, 0, 0), tcl = 0.3, cex = 1)

  for (ts in time_steps) {
    foo_qq(comp_df, ref_df, var = "PPT_cm", time = ts,
      lab = "precipitation (cm)", legend = ts == time_steps[1])
    foo_qq(comp_df, ref_df, var = "Tmax_C", time = ts,
      lab = "max temp (C)")
    foo_qq(comp_df, ref_df, var = "Tmin_C", time = ts,
      lab = "min temp (C)")
  }

  graphics::par(par_prev)
  grDevices::dev.off()


  #--- Does output weather recreate weather generator inputs?
  ref_wgin <- dbW_estimate_WGen_coefs(ref_df[[1]][["Day"]],
    WET_limit_cm = WET_limit_cm, imputation_type = "mean")
  comp_wgin <- lapply(comp_df, function(x)
      dbW_estimate_WGen_coefs(x[["Day"]],
        WET_limit_cm = WET_limit_cm, imputation_type = "mean")
    )


  foo_scatter_wgin <- function(data, ref_data, obj, fname) {
    vars <- colnames(ref_data[[obj]])[-1]
    panels <- if (length(vars) == 4) {
      c(2, 2)
    } else if (length(vars) == 10) {
      c(4, 3)
    } else {
      rep(ceiling(sqrt(length(vars))), 2)
    }

    grDevices::png(units = "in", res = 150,
      height = 3 * panels[1], width = 3 * panels[2],
      file = fname)
    par_prev <- graphics::par(mfrow = panels, mar = c(2, 2.5, 0.5, 0.5),
      mgp = c(1, 0, 0), tcl = 0.3, cex = 1)

    for (v in vars) {
      x <- ref_data[[obj]][, v]

      vlim_obs <- range(x, na.rm = TRUE)
      vlim <- range(sapply(data, function(x)
        range(x[[obj]][, v], na.rm = TRUE)))

      if (all(is.finite(vlim_obs)) && all(is.finite(vlim))) {
        graphics::plot(x, x, type = "n", xlim = vlim, ylim = vlim, asp = 1,
          xlab = paste0("Reference ", v), ylab = paste0("Weather ", v))
        for (k in seq_along(data)) {
          isgood <- complete.cases(cbind(x, data[[k]][[obj]][, v]))
          graphics::lines(stats::lowess(x[isgood], data[[k]][[obj]][isgood, v]),
            col = "gray")
        }

        graphics::abline(h = 0, lty = 2)
        graphics::abline(v = 0, lty = 2)
        graphics::segments(x0 = vlim_obs[1], y0 = vlim_obs[1],
          x1 = vlim_obs[2], y1 = vlim_obs[2], col = "red", lwd = 2)

        if (v == vars[1]) {
          graphics::legend("topleft", legend = c("Reference", "Weather"),
            col = c("red", "black"), lwd = 2)
        }

      } else {
        graphics::plot.new()
      }
    }

    graphics::par(par_prev)
    grDevices::dev.off()
  }


  foo_scatter_wgin(data = comp_wgin, ref_data = ref_wgin, obj = "mkv_doy",
    fname = file.path(path,
      paste0(tag, "_CompareWeather_WGenInputs_DayOfYear.png")))
  foo_scatter_wgin(data = comp_wgin, ref_data = ref_wgin, obj = "mkv_woy",
    fname = file.path(path,
      paste0(tag, "_CompareWeather_WGenInputs_WeekOfYear.png")))

}


#' Generate daily weather data using SOILWAT2's weather generator
#'
#' This function is a convenience wrapper for
#' \code{\link{dbW_estimate_WGen_coefs}}.
#'
#' @inheritParams dbW_estimate_WGen_coefs
#' @param years An integer vector. The calendar years for which to generate
#'   daily weather. If \code{NULL}, then extracted from \code{weatherData}.
#' @param wgen_coeffs A list with two named elements \var{mkv_doy} and
#'   \var{mkv_woy}, i.e., the return value of
#'   \code{\link{dbW_estimate_WGen_coefs}}. If \code{NULL}, then determined
#'   based on \code{weatherData}.
#' @inheritParams rSW2utils::impute_df
#' @param seed An integer value or \code{NULL}. See \code{\link{set.seed}}.
#'
#' @return A list of elements of class \code{\linkS4class{swWeatherData}}.
#'
#' @examples
#' # Load data for 1949-2010
#' wdata <- data.frame(dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))
#'
#' # Treat data for 2005-2010 as our 'dataset'
#' ids <- wdata[, "Year"] >= 2005
#' x <- wdata[ids, ]
#'
#' # Set June-August of 2008 as missing
#' ids <- x[, "Year"] == 2008 & x[, "DOY"] >= 153 & x[, "DOY"] <= 244
#' x[ids, -(1:2)] <- NA
#'
#' ## Example 1: generate weather for any missing values in our 'dataset'
#' wout1 <- dbW_generateWeather(x)
#'
#' ## Example 2: generate weather based on our 'dataset' but for
#' ## years 2005-2015 and use estimated weather generator coefficients from
#' ## a different dataset
#' wgen_coeffs <- dbW_estimate_WGen_coefs(
#'   wdata,
#'   imputation_type = "mean",
#'   imputation_span = 5
#' )
#'
#' # Set seed to make output reproducible
#' wout2 <- dbW_generateWeather(
#'   x,
#'   years = 2005:2015,
#'   wgen_coeffs = wgen_coeffs,
#'   seed = 123
#' )
#'
#' ## Example 3: generate weather based only on estimated weather generator
#' ## coefficients from a different dataset
#' x_empty <- list(new("swWeatherData"))
#' wout3 <- dbW_generateWeather(
#'   x_empty,
#'   years = 2050:2055,
#'   wgen_coeffs = wgen_coeffs,
#'   seed = 123
#' )
#'
#' ## Compare input weather with generated weather
#' path <- tempdir()
#' compare_weather(
#'   ref_weather = x,
#'   weather = dbW_weatherData_to_dataframe(wout1),
#'   N = 1,
#'   path = path,
#'   tag = "Example1-WeatherGenerator"
#' )
#' unlink(list.files(path), force = TRUE)
#'
#' @export
dbW_generateWeather <- function(weatherData, years = NULL, wgen_coeffs = NULL,
  imputation_type = "mean", imputation_span = 5L, seed = NULL) {

  #--- Obtain missing/null arguments
  if (is.null(wgen_coeffs)) {
    wgen_coeffs <- dbW_estimate_WGen_coefs(weatherData,
      propagate_NAs = FALSE,
      imputation_type = imputation_type,
      imputation_span = imputation_span
    )
  }

  if (is.data.frame(weatherData)) {
    weatherData <- dbW_dataframe_to_weatherData(weatherData)
  }

  if (is.null(years)) {
    years <- get_years_from_weatherData(weatherData)
  }

  #--- Put rSOILWAT2 run together to produce imputed daily weather
  sw_in <- rSOILWAT2::sw_exampleData

  # Set years
  swWeather_FirstYearHistorical(sw_in) <- min(years)
  swYears_EndYear(sw_in) <- max(years)
  swYears_StartYear(sw_in) <- min(years)

  # Set weather data
  set_WeatherHistory(sw_in) <- weatherData

  # Turn on weather generator
  swWeather_UseMarkov(sw_in) <- TRUE

  # Set weather generator coefficients
  swMarkov_Prob(sw_in) <- wgen_coeffs[["mkv_doy"]]
  swMarkov_Conv(sw_in) <- wgen_coeffs[["mkv_woy"]]

  # Turn off CO2-effects to avoid any issues
  swCarbon_Use_Bio(sw_in) <- 0
  swCarbon_Use_WUE(sw_in) <- 0

  #--- Execute SOILWAT2 to generate weather
  set.seed(seed)
  sw_out <- sw_exec(inputData = sw_in)


  #--- Extract weather generator imputed daily weather
  xdf <- slot(slot(sw_out, "TEMP"), "Day")[, c("Year", "Day", "max_C", "min_C")]
  colnames(xdf) <- c("Year", "DOY", "Tmax_C", "Tmin_C")
  xdf <- data.frame(
    xdf,
    PPT_cm = slot(slot(sw_out, "PRECIP"), "Day")[, "ppt"]
  )

  # Convert to rSOILWAT2 weather data format
  dbW_dataframe_to_weatherData(xdf)
}
