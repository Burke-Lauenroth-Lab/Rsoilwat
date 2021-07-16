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


## ------SQLite weather database functions
# Daily weather data is stored in database as SQL-blob of a list of R objects
# of class \code{\linkS4class{swWeatherData}}

#' Check whether registered weather database connection is valid
#' @return A logical value.
#' @export
dbW_IsValid <- function() {
  !is.null(rSW2_glovars$con) && DBI::dbIsValid(rSW2_glovars$con)
}

#' Query version number of registered weather database
#' @return A numeric version number.
#' @export
dbW_version <- function() {
  stopifnot(dbW_IsValid())

  sql <- "SELECT Value FROM Meta WHERE Desc=\'Version\'"
  numeric_version(as.character(DBI::dbGetQuery(rSW2_glovars$con, sql)[1, 1]))
}

#' Check that version of registered weather database is up-to-date
#' @return A logical value.
#' @export
dbW_check_version <- function(dbW_min_version = NULL) {
  stopifnot(dbW_IsValid())

  v_dbW <- dbW_version()

  if (is.null(dbW_min_version)) {
    dbW_min_version <- rSW2_glovars[["dbW_version"]]
  }
  success <- v_dbW >= dbW_min_version

  if (!success) {
    message(paste("The version", shQuote(v_dbW), "of the weather database",
      shQuote(basename(slot(rSW2_glovars$con, "dbname"))), "is outdated;",
      "minimal suggested version is", shQuote(dbW_min_version),
      "-- please update."))
  }

  success
}


#' Query compression type of registered weather database
#' @return A character string.
#' @export
dbW_compression <- function() {
  stopifnot(dbW_IsValid())

  sql <- "SELECT Value FROM Meta WHERE Desc=\'Compression_type\'"
  as.character(DBI::dbGetQuery(rSW2_glovars$con, sql)[1, 1])
}


#' Check availability of content in registered weather database
#'
#' @param Site_ids An integer vector. The IDs/database keys of the queried site.
#' @param Labels A vector of character strings. The names/labels of the queried
#'   sites.
#' @param Scenario_ids An integer vector. The IDs/database keys of the queried
#'   scenario.
#' @param Scenarios A vector of character strings. The names/labels of the
#'   queried scenarios.
#' @param ignore.case A logical value.
#' @name check_content
NULL

#' @rdname check_content
#' @section Details: \code{dbW_has_siteIDs} checks whether sites are available.
#' @return \code{dbW_has_siteIDs} returns a logical vector of the length of
#'   queried sites.
#' @export
dbW_has_sites <- function(Labels, ignore.case = FALSE) {
  stopifnot(dbW_IsValid())

  sql <- paste(
    "SELECT COUNT(*) FROM Sites WHERE Label=:x",
    if (ignore.case) "COLLATE NOCASE"
  )

  DBI::dbGetQuery(rSW2_glovars$con, sql, params = list(x = Labels))[, 1] > 0
}

#' @rdname check_content
#' @section Details: \code{dbW_has_siteIDs} checks whether sites are available.
#' @return \code{dbW_has_siteIDs} returns a logical vector of the length of
#'   queried sites.
#' @export
dbW_has_siteIDs <- function(Site_ids) {
  stopifnot(dbW_IsValid())

  sql <- "SELECT COUNT(*) FROM Sites WHERE Site_id=:x"

  DBI::dbGetQuery(rSW2_glovars$con, sql, params = list(x = Site_ids))[, 1] > 0
}

#' @rdname check_content
#' @section Details: \code{dbW_has_scenarioIDs} checks whether scenarios are
#'   available.
#' @return \code{dbW_has_scenarios} returns a logical vector of the length of
#'   queried Scenarios.
#' @export
dbW_has_scenarioIDs <- function(Scenario_ids) {
  stopifnot(dbW_IsValid())

  sql <- "SELECT COUNT(*) FROM Scenarios WHERE id=:x"

  DBI::dbGetQuery(
    rSW2_glovars$con,
    sql,
    params = list(x = Scenario_ids)
  )[, 1] > 0
}

#' @rdname check_content
#' @section Details: \code{dbW_has_scenarios} checks whether scenarios are
#'   available.
#' @return \code{dbW_has_scenarios} returns a logical vector of the length of
#'   queried Scenarios.
#' @export
dbW_has_scenarios <- function(Scenarios, ignore.case = FALSE) {
  stopifnot(dbW_IsValid())

  sql <- paste(
    "SELECT COUNT(*) FROM Scenarios WHERE Scenario=:x",
    if (ignore.case) "COLLATE NOCASE"
  )

  DBI::dbGetQuery(rSW2_glovars$con, sql, params = list(x = Scenarios))[, 1] > 0
}

#' @rdname check_content
#' @section Details: \code{dbW_has_weatherData} checks whether weather data are
#'   available.
#' @return \code{dbW_has_weatherData} returns a logical matrix with rows
#'   corresponding to queried sites and columns to queried scenarios.
#' @export
dbW_has_weatherData <- function(Site_ids, Scenario_ids) {
  stopifnot(dbW_IsValid())

  sites_N <- length(Site_ids)
  scen_N <- length(Scenario_ids)

  if (sites_N > scen_N) {
    sql <- paste(
      "SELECT COUNT(*) FROM WeatherData",
      "WHERE Site_id IN (:x1) AND Scenario = :x2"
    )

    res <- lapply(Scenario_ids, function(x) {
      res <- DBI::dbGetQuery(
        rSW2_glovars$con,
        sql,
        params = list(x1 = Site_ids, x2 = rep(x, sites_N))
      )
      res[, 1] == 1L
    })

    res <- do.call(cbind, res)

  } else {
    sql <- paste(
      "SELECT COUNT(*) FROM WeatherData",
      "WHERE Site_id = :x1 AND Scenario IN (:x2)"
    )

    res <- lapply(Site_ids, function(x) {
      res <- DBI::dbGetQuery(
        rSW2_glovars$con,
        sql,
        params = list(x1 = rep(x, scen_N), x2 = Scenario_ids)
      )
      res[, 1] == 1L
    })

    res <- do.call(rbind, res)
  }

  dimnames(res) <- list(
    paste("Site", Site_ids, sep = "_"),
    paste("Scenario", Scenario_ids, sep = "_")
  )

  res
}


#' Extract table keys to connect sites with weather data in the registered
#' weather database
#'
#' @details The key(s) (\var{Site_id}) can be located by either providing a
#'   \code{Labels} or by providing \code{lat} and \code{long} of the requested
#'   site(s).
#'
#' @param lat A numeric vector or \code{NULL}. The latitude in decimal degrees
#'   of \var{WGS84}. Northern latitude are positive, sites on the southern
#'   hemisphere have negative values.
#' @param long A numeric vector or \code{NULL}. The longitude in decimal degrees
#'   of \var{WGS84}. Eastern longitudes are positive, sites on the western
#'   hemisphere have negative values.
#' @param tol_xy A numeric value. The tolerance used to match requested
#'   longitude and latitude values.
#' @inheritParams check_content
#'
#' @return An integer vector with the values of the keys or \code{NA} if not
#'   located.
#' @export
dbW_getSiteId <- function(
  lat = NULL, long = NULL, tol_xy = 1e-4,
  Labels = NULL,
  ignore.case = FALSE,
  verbose = FALSE
) {

  stopifnot(dbW_IsValid(), identical(length(lat), length(long)))

  x <- if (is.character(Labels)) {
      sql <- paste0(
        "SELECT Site_id FROM Sites WHERE Label=:x",
        if (ignore.case) " COLLATE NOCASE"
      )

      tmp <- DBI::dbGetQuery(
        rSW2_glovars$con,
        statement = sql,
        params = list(x = Labels)
      )[, 1]

      ntmp <- length(tmp)

      if (ntmp == 0) {
        rep(NA, length(Labels))

      } else if (ntmp == length(Labels)) {
        tmp

      } else {
        # Some Labels are missing, but we don't know which ones
        # Slowly go through Labels one by one
        rs <- DBI::dbSendQuery(rSW2_glovars$con, sql)

        tmp <- sapply(
          Labels,
          function(x) {
            DBI::dbBind(rs, list(x = x))
            tmp <- DBI::dbFetch(rs)[, 1]
            if (is.null(tmp)) NA else tmp
          }
        )

        DBI::dbClearResult(rs)
        tmp
      }


    } else if (is.numeric(lat) && is.numeric(long)) {
      # Find the latitude and longitude with the minimum difference if less
      # than deviating by less than tolerance
      sql <- paste(
        "SELECT dxy2.Site_id",
        "FROM (",
          "SELECT",
            "Site_id,",
            "dxy.adlat AS adlat,",
            "dxy.adlon AS adlon,",
            "MIN(dxy.adlat) AS min_adlat,",
            "MIN(dxy.adlon) AS min_adlon",
          "FROM (",
            "SELECT",
              "Site_id,",
              "ABS(Latitude - :lat) AS adlat,",
              "ABS(Longitude - :lon) AS adlon",
            "FROM Sites",
            "WHERE",
              "Latitude BETWEEN :lat - :tol AND :lat + :tol AND",
              "Longitude BETWEEN :lon - :tol AND :lon + :tol",
          ") AS dxy",
        ") AS dxy2",
        "WHERE",
          "dxy2.adlat = dxy2.min_adlat AND",
          "dxy2.adlon = dxy2.min_adlon"
      )

      rs <- DBI::dbSendQuery(rSW2_glovars$con, sql)

      tmp <- sapply(
        seq_along(lat),
        function(k) {
          DBI::dbBind(rs, list(lat = lat[k], lon = long[k], tol = tol_xy))
          tmp <- DBI::dbFetch(rs)[, 1]
          if (is.null(tmp)) NA else tmp
        }
      )

      DBI::dbClearResult(rs)
      tmp


    } else {
      if (verbose) {
        message("'dbW_getSiteId': not enough information to obtain site IDs")
      }

      rep(NA, max(length(Labels), length(long)))
    }

  as.integer(x)
}


#' Extract table keys to connect scenario(s) with weather data in the registered
#' weather database
#'
#' @inheritParams check_content
#'
#' @return An integer vector with the values of the keys or \code{NA} if not
#'   located.
#' @export
dbW_getScenarioId <- function(Scenario, ignore.case = FALSE, verbose = FALSE) {
  stopifnot(dbW_IsValid())

  sql <- paste0("SELECT id FROM Scenarios WHERE Scenario = :x",
    if (ignore.case) " COLLATE NOCASE")
  x <- sapply(Scenario, function(x) {
    temp <- DBI::dbGetQuery(rSW2_glovars$con, sql, params = list(x = x))[, 1]
    if (is.null(temp)) NA else temp
  })

  as.integer(x)
}

#' Locate keys for weather database tables in the registered weather database
#'
#' Obtain database table keys 'Site_id' and 'Scenario_id' using alternative
#' information and optionally add missing sites and scenarios.
#' A site will be identified alternatively
#' by \itemize{
#'    \item its identification number \code{site_id},
#'    \item its name \code{site_label}, or
#'    \item its geographic location \code{long} and \code{lat}.
#' }
#' A scenario will be identified alternatively
#' by \itemize{
#'    \item its identification number \code{scenario_id}, or
#'    \item its name \code{scenario}.
#' }
#'
#'
#' @inheritParams dbW_getSiteId
#'
#' @return A list with two elements \code{site_id} and \code{scenario_id}.
#'
#' @export
dbW_getIDs <- function(
  site_id = NULL, site_label = NULL,
  long = NULL, lat = NULL, tol_xy = 1e-4,
  scenario = NULL, scenario_id = NULL,
  add_if_missing = FALSE,
  ignore.case = FALSE,
  verbose = FALSE
) {

  res <- list(site_id = site_id, scenario_id = scenario_id)

  has_siteID <- is.numeric(res[["site_id"]]) &&
    all(dbW_has_siteIDs(res[["site_id"]]))

  if (!has_siteID) {
    res[["site_id"]] <- dbW_getSiteId(
      Labels = site_label,
      lat = lat, long = long, tol_xy = tol_xy,
      ignore.case = ignore.case,
      verbose = verbose
    )

    if (anyNA(res[["site_id"]]) && add_if_missing) {
      iadd <- is.na(res[["site_id"]])
      do_temp <- (is.character(site_label[iadd]) &&
          all(nchar(site_label[iadd]) > 0)) ||
        (as.numeric(lat[iadd]) && as.numeric(long[iadd]))
      temp <- if (do_temp) {
          df <- data.frame(Latitude = lat[iadd], Longitude = long[iadd],
            Label = site_label[iadd], stringsAsFactors = FALSE)
          try(dbW_addSites(site_data = df, ignore.case = ignore.case),
            silent = TRUE)

        } else FALSE

      if (!inherits(temp, "try-error") && isTRUE(temp)) {
        res[["site_id"]] <- dbW_getSiteId(Labels = site_label,
          lat = lat, long = long, ignore.case = ignore.case, verbose = verbose)
      }
    }
  }

  has_scenID <- is.numeric(res[["scenario_id"]]) &&
    all(dbW_has_scenarioIDs(res[["scenario_id"]]))

  if (!has_scenID) {
    res[["scenario_id"]] <- dbW_getScenarioId(Scenario = scenario,
      ignore.case = ignore.case, verbose = verbose)

    if (anyNA(res[["scenario_id"]]) && add_if_missing) {
      iadd <- is.na(res[["scenario_id"]])
      do_temp <- as.character(scenario[iadd]) && all(nchar(scenario[iadd]) > 0)
      temp <- if (do_temp) {
          try(dbW_addScenarios(Scenarios = scenario[iadd],
            ignore.case = ignore.case), silent = TRUE)
        } else FALSE

      if (!inherits(temp, "try-error") && isTRUE(temp)) {
        res[["scenario_id"]] <- dbW_getScenarioId(Scenario = scenario,
          ignore.case = ignore.case, verbose = verbose)
      }
    }
  }

  res
}

#' Read entire table of sites from the registered weather database
#'
#' @return A data.frame.
#' @export
dbW_getSiteTable <- function() {
  stopifnot(dbW_IsValid())

  DBI::dbReadTable(rSW2_glovars$con, "Sites")
}

#' Read entire table of Scenarios from the registered weather database
#'
#' @return A data.frame.
#' @export
dbW_getScenariosTable <- function() {
  stopifnot(dbW_IsValid())

  DBI::dbReadTable(rSW2_glovars$con, "Scenarios")
}


# Index along years to narrow the start and/or end year if not NULL
select_years <- function(years, start_year = NULL, end_year = NULL) {

  if (!is.null(start_year) || !is.null(end_year)) {
    start_year <- as.integer(start_year)
    use_start <- !is.na(start_year)
    end_year <- as.integer(end_year)
    use_end <- !is.na(end_year)

    if (use_start && use_end &&
        (start_year >= end_year || start_year < 0 || end_year < 0)) {
      warning("'select_years': wrong value for argument 'start_year' ",
        "and/or 'end_year'")
    }

  } else {
    use_start <- use_end <- FALSE
  }

  idx_start_year <- 1L
  if (use_start) {
    temp <- match(start_year, years)
    if (!is.na(temp)) {
      idx_start_year <- temp
    }
  }

  idx_end_year <- length(years)
  if (use_end) {
    temp <- match(end_year, years)
    if (!is.na(temp)) {
      idx_end_year <- temp
    }
  }

  idx_start_year:idx_end_year
}

#' Extract years from a \var{weatherData} object
#' @param wd A list of elements of class \code{\linkS4class{swWeatherData}}
#' @export
get_years_from_weatherData <- function(wd) {
  as.integer(unlist(lapply(wd, FUN = slot, "year")))
}


#' Extracts daily weather data from a registered weather database
#'
#' Weather data for the soil water simulation run can be stored in the input
#' data or it can be separate to keep the input data size down for multiple
#' variations of the same site. This function is used to return the weather
#' data from a predefined weather database. Using the database was faster then
#' reading in multiple weather files from disk.
#'
#' \pkg{SOILWAT2} does not handle missing weather data. If you have missing
#' data, then you have to impute yourself or use the built-in Markov
#' weather generator (see examples for \code{\link{sw_exec}}).
#'
#' @param Site_id Numeric. Used to identify site and extract weather data.
#' @param lat Numeric. Latitude used with longitude to identify site id if
#'   \code{Site_id} is missing.
#' @param long Numeric. Longitude and Latitude are used to identify site if
#'   \code{Site_id} is missing.
#' @param Label A character string. A site label.
#' @param startYear Numeric. Extracted weather data will start with this year.
#' @param endYear Numeric. Extracted weather data will end with this year.
#' @param Scenario A character string.
#' @inheritParams dbW_getSiteId
#'
#' @return Returns weather data as list. Each element is an object of class
#'   \code{\linkS4class{swWeatherData}} and contains data for one year.
#'
#' @seealso \code{\link{getWeatherData_folders}}
#'
#' @export
dbW_getWeatherData <- function(
  Site_id = NULL,
  lat = NULL, long = NULL, tol_xy = 1e-4,
  Label = NULL,
  startYear = NULL, endYear = NULL,
  Scenario = "Current", Scenario_id = NULL,
  ignore.case = FALSE,
  verbose = FALSE
) {

  stopifnot(dbW_IsValid())
  IDs <- dbW_getIDs(
    site_id = Site_id, site_label = Label,
    long = long, lat = lat, tol_xy = tol_xy,
    scenario = Scenario, scenario_id = Scenario_id,
    add_if_missing = FALSE,
    ignore.case = ignore.case,
    verbose = verbose
  )


  if (any(!sapply(IDs, function(x) length(x) > 0 && is.finite(x)))) {
    stop("'dbW_getWeatherData': insufficient information to locate weather ",
      "data.")
  }

  sql <- "SELECT data FROM WeatherData WHERE Site_id = :x1 AND Scenario = :x2"
  res <- DBI::dbGetQuery(rSW2_glovars$con, sql,
    params = list(x1 = IDs[["site_id"]], x2 = IDs[["scenario_id"]]))[1, 1]
  if (is.na(res) || all(lengths(res) == 0)) {
    stop(paste("Weather data for site", shQuote(IDs[["site_id"]]),
      "and scenario", shQuote(IDs[["scenario_id"]]),
      "does not exist in weather database."))
  }

  wd <- try(dbW_blob_to_weatherData(res, rSW2_glovars$blob_compression_type))
  if (inherits(wd, "try-error")) {
    stop(paste("Weather data for site", shQuote(IDs[["site_id"]]),
      "and scenario", shQuote(IDs[["scenario_id"]]), "is corrupted."))
  }

  temp <- class(wd[[1]])
  if (!(attr(temp, "package") == "rSOILWAT2")) {
    message(paste("WARNING: The class of the extracted weather data object is",
      shQuote(temp), "from package", shQuote(attr(temp, "package")), "which is",
      "outdated. Please, upgrade weather database with function",
      "'dbW_upgrade_to_rSOILWAT2'."))
  }

  years <- get_years_from_weatherData(wd)
  ids <- select_years(years, startYear, endYear)

  wd[ids]
}

#' Registers/connects a SQLite weather database with the package
#'
#' @param dbFilePath A character string. The weather database file path.
#' @param create_if_missing A logical value. If \code{TRUE} and now file
#'   \code{dbFilePath} exists then create a new file.
#' @param check_version A logical value. If \code{TRUE} then check database
#'   version against currently implemented version by the package.
#' @param verbose A logical value.
#'
#' @return An invisible logical value indicating success/failure.
#'
#' @export
dbW_setConnection <- function(dbFilePath, create_if_missing = FALSE,
  check_version = FALSE, verbose = FALSE) {

  rSW2_glovars$con <- NULL

  dbFilePath <- try(normalizePath(dbFilePath, mustWork = FALSE), silent = TRUE)
  if (inherits(dbFilePath, "try-error") || !file.exists(dbFilePath)) {
    if (verbose) {
      message(paste("'dbW_setConnection':", shQuote(basename(dbFilePath)),
        "does not exist."))
    }
    if (create_if_missing) {
      if (verbose) {
        message(paste("'dbW_setConnection': creating a new database."))
      }
    } else {
      return(invisible(FALSE))
    }
  }

  # Check if 'dbFilePath' can be created
  temp1 <- try(suppressWarnings(DBI::dbConnect(RSQLite::SQLite(),
    dbname = dbFilePath)), silent = TRUE)
  if (inherits(temp1, "try-error")) {
    if (verbose) {
      message(paste("'dbW_setConnection':", shQuote(dbFilePath),
        "cannot be created likely because the path does not exist."))
    }
    return(invisible(FALSE))
  }

  # Check if 'dbFilePath' is likely a good SQLite-database
  temp2 <- try(DBI::dbExecute(temp1, "PRAGMA synchronous = off"), silent = TRUE)
  if (inherits(temp2, "try-error")) {
    if (verbose) {
      message(paste("'dbW_setConnection':", shQuote(basename(dbFilePath)),
        "exists but is likely not a SQLite-database."))
    }
    return(invisible(FALSE))
  }

  # Check that foreign key constraints are supported or at least accepted
  temp3 <- try(DBI::dbExecute(temp1, "PRAGMA foreign_keys = ON"), silent = TRUE)
  if (inherits(temp3, "try-error")) {
    if (verbose) {
      message(paste("'dbW_setConnection': foreign keys are not supported."))
    }
    return(invisible(FALSE))
  }

  rSW2_glovars$con <- temp1
  has_meta <- DBI::dbExistsTable(rSW2_glovars$con, "Meta")
  rSW2_glovars$blob_compression_type <- if (has_meta) {
      dbW_compression()
    } else {
      rSW2_glovars$default_blob_compression_type
    }

  if (check_version) {
    dbW_check_version()
  }

  invisible(dbW_IsValid())
}

#' Disconnects a SQLite weather database from the package
#' @return An invisible logical value indicating success with \code{TRUE} and
#'  failure with \code{FALSE}.
#' @export
dbW_disconnectConnection <- function() {
  res <- if (dbW_IsValid()) {
      DBI::dbDisconnect(rSW2_glovars$con)
    } else TRUE

  rSW2_glovars$con <- NULL
  rSW2_glovars$blob_compression_type <- NULL

  invisible(res)
}

#' Adds new sites to a registered weather database
#'
#' @inheritParams check_content
#' @inheritParams dbW_createDatabase
#' @return An invisible logical value indicating success with \code{TRUE} and
#'  failure with \code{FALSE}.
#' @export
dbW_addSites <- function(site_data, ignore.case = FALSE, verbose = FALSE) {
  stopifnot(dbW_IsValid())

  req_cols <- c("Latitude", "Longitude", "Label")
  if (!all(req_cols %in% colnames(site_data))) {
    stop("'dbW_addSites': argument misses required columns.")
  }

  has_sites <- dbW_has_sites(site_data[, "Label"], ignore.case = ignore.case)
  dos_add <- !has_sites

  if (any(dos_add)) {
    sql <- "INSERT INTO Sites VALUES(NULL, :Latitude, :Longitude, :Label)"
    DBI::dbExecute(rSW2_glovars$con, sql,
      params = as.list(site_data[dos_add, req_cols]))
  }

  if (any(has_sites) && verbose) {
    message(paste("'dbW_addSites': sites are already in database, labels =",
      paste(shQuote(site_data[has_sites, "Label"]), collapse = ", ")))
  }

  invisible(TRUE)
}

#' Updates existing sites or adds new sites to a registered weather database
#'
#' @inheritParams check_content
#' @inheritParams dbW_createDatabase
#' @return An invisible logical value indicating success with \code{TRUE} and
#'  failure with \code{FALSE}.
#' @export
dbW_updateSites <- function(Site_ids, site_data, ignore.case = FALSE,
  verbose = FALSE) {
  stopifnot(dbW_IsValid())

  dos_update <- dbW_has_siteIDs(Site_ids)
  dos_add <- !dos_update

  if (any(dos_update)) {
    sql <- paste("UPDATE Sites SET Latitude=:Latitude, Longitude=:Longitude, ",
      "Label=:Label WHERE Site_id=:id")
    rs <- DBI::dbSendStatement(rSW2_glovars$con, sql)
    on.exit(DBI::dbClearResult(rs), add = TRUE)

    for (k in which(dos_update)) {
      DBI::dbBind(rs,
        param = c(as.list(site_data[k, c("Latitude", "Longitude", "Label")]),
        list(id = Site_ids[k])))
    }
  }

  if (any(dos_add)) {
    stopifnot(dbW_addSites(site_data[dos_add, ], ignore.case = ignore.case,
      verbose = verbose))
  }

  invisible(TRUE)
}

#' Adds new Scenarios to a registered weather database
#'
#' @inheritParams check_content
#' @inheritParams dbW_createDatabase
#' @return An invisible logical value indicating success with \code{TRUE} and
#'  failure with \code{FALSE}.
#' @export
dbW_addScenarios <- function(Scenarios, ignore.case = FALSE, verbose = FALSE) {
  stopifnot(dbW_IsValid())

  has_scenarios <- dbW_has_scenarios(Scenarios, ignore.case = ignore.case)
  dos_add <- !has_scenarios

  if (any(dos_add)) {
    sql <- "INSERT INTO Scenarios VALUES(NULL, :sc)"
    DBI::dbExecute(rSW2_glovars$con, sql,
      params = list(sc = unlist(Scenarios[dos_add])))
  }

  if (any(has_scenarios) && verbose) {
    message(paste("'dbW_addScenarios': Scenarios are already in database,",
      "Scenarios =", paste(shQuote(Scenarios[has_scenarios]), collapse = ", ")))
  }

  invisible(TRUE)
}

dbW_addWeatherDataNoCheck <- function(Site_id, Scenario_id, StartYear, EndYear,
  weather_blob) {

  sql <- paste("INSERT INTO",
    "WeatherData (Site_id, Scenario, StartYear, EndYear, data)",
    "VALUES (:Site_id, :Scenario_id, :StartYear, :EndYear, :weather_blob)")
  DBI::dbExecute(rSW2_glovars$con, sql, params = list(Site_id = Site_id,
    Scenario_id = Scenario_id, StartYear = StartYear, EndYear = EndYear,
    weather_blob = weather_blob))
}

#' Adds daily weather data to a registered weather database
#' @inheritParams check_content
#' @inheritParams dbW_getWeatherData
#'
#' @return An invisible logical value indicating success with \code{TRUE} and
#'  failure with \code{FALSE}.
#'
#' @export
dbW_addWeatherData <- function(
  Site_id = NULL,
  lat = NULL, long = NULL, tol_xy = 1e-4,
  weatherFolderPath = NULL, weatherData = NULL,
  Label = NULL,
  Scenario_id = NULL, Scenario = "Current",
  weather_tag = "weath",
  ignore.case = FALSE,
  overwrite = FALSE,
  verbose = FALSE
) {

  stopifnot(dbW_IsValid())

  has_weatherFolderPath <- !is.null(weatherFolderPath) &&
    file.exists(weatherFolderPath)
  has_weatherData <- !is.null(weatherData) && is.list(weatherData) &&
    inherits(weatherData[[1]], "swWeatherData")

  if (!has_weatherFolderPath && !has_weatherData) {
    stop("'dbW_addWeatherData' requires either a folder path or weatherData.")
  }

  Label <- if (!is.null(weatherFolderPath) && is.null(Label)) {
      basename(weatherFolderPath)
    } else Label

  IDs <- dbW_getIDs(
    site_id = Site_id, site_label = Label,
    long = long, lat = lat, tol_xy = tol_xy,
    scenario = Scenario, scenario_id = Scenario_id,
    add_if_missing = TRUE,
    ignore.case = ignore.case,
    verbose = verbose
  )

  if (any(!sapply(IDs, function(x) length(x) > 0 && is.finite(x)))) {
    stop("'dbW_addWeatherData': insufficient information to generate ",
      "site/scenario.")
  }

  if (dbW_has_weatherData(IDs[["site_id"]], IDs[["scenario_id"]])) {
    temp <- paste("'dbW_addWeatherData': weather data for site",
      IDs[["site_id"]], "and scenario", IDs[["scenario_id"]], "already exists.")

    if (overwrite) {
      if (verbose) {
        message(paste(temp, "Previous data will be overwritten with new data."))
      }
      stopifnot(dbW_deleteSiteData(IDs[["site_id"]], IDs[["scenario_id"]]))

    } else {
      stop(temp)
    }
  }

  if (is.null(weatherData)) {
    weatherData <- getWeatherData_folders(
      LookupWeatherFolder = weatherFolderPath, filebasename = weather_tag)
  }

  years <- get_years_from_weatherData(weatherData)
  blob <- dbW_weatherData_to_blob(weatherData,
    rSW2_glovars$blob_compression_type)

  dbW_addWeatherDataNoCheck(IDs[["site_id"]], IDs[["scenario_id"]], years[1],
    years[length(years)], weather_blob = blob)

  invisible(TRUE)
}


.create_dbW <- function(site_data, Scenarios, scen_ambient) {
  sql <- "CREATE TABLE 'Meta' ('Desc' TEXT PRIMARY KEY, 'Value' TEXT)"
  DBI::dbExecute(rSW2_glovars$con, sql)

  sql <- "INSERT INTO 'Meta' VALUES(:Desc, :Value)"
  DBI::dbExecute(rSW2_glovars$con, sql, params = list(
    Desc = c("Version", "Compression_type"),
    Value = c(rSW2_glovars$dbW_version, rSW2_glovars$blob_compression_type)))

  # Table of sites
  sql <- paste("CREATE TABLE 'Sites'",
    "('Site_id' INTEGER PRIMARY KEY AUTOINCREMENT,",
    "'Latitude' REAL, 'Longitude' REAL, 'Label' TEXT UNIQUE)")
  DBI::dbExecute(rSW2_glovars$con, sql)
  # Table of scenario names
  sql <- paste("CREATE TABLE 'Scenarios'",
    "('id' INTEGER PRIMARY KEY AUTOINCREMENT,",
    "'Scenario' TEXT UNIQUE NOT NULL)")
  DBI::dbExecute(rSW2_glovars$con, sql)
  # Table for weather data
  DBI::dbExecute(rSW2_glovars$con, "PRAGMA foreign_keys = ON")
  sql <- paste("CREATE TABLE 'WeatherData'",
    "('wdid' INTEGER PRIMARY KEY AUTOINCREMENT,",
    "'Site_id' INTEGER, 'Scenario' INTEGER, 'StartYear' INTEGER NOT NULL,",
    "'EndYear' INTEGER NOT NULL, 'data' BLOB,",
    "FOREIGN KEY(Site_id) REFERENCES Sites(Site_id),",
    "FOREIGN KEY(Scenario) REFERENCES Scenarios(id))")
  DBI::dbExecute(rSW2_glovars$con, sql)
  DBI::dbExecute(rSW2_glovars$con,
    "CREATE INDEX wdindex ON WeatherData(Site_id, Scenario)")

  # View all data
  sql <- paste("CREATE VIEW wd_all AS",
    "SELECT Sites.Site_id, Sites.Latitude, Sites.Longitude,",
      "Sites.Label AS Site_Label, Scenarios.id AS Scenario_id,",
      "Scenarios.Scenario, WeatherData.StartYear, WeatherData.EndYear,",
      "WeatherData.data",
    "FROM Sites, Scenarios, WeatherData",
    "WHERE WeatherData.Site_id=Sites.Site_id AND",
      "WeatherData.Scenario=Scenarios.id")
  DBI::dbExecute(rSW2_glovars$con, sql)


  #---Add sites
  if (NROW(site_data)) {
    stopifnot(dbW_addSites(site_data))
  }

  #---Add Scenarios
  Scenarios <- c(scen_ambient, Scenarios[!(Scenarios == scen_ambient)])
  stopifnot(dbW_addScenarios(Scenarios, ignore.case = FALSE))

  invisible(TRUE)
}


#' Create a weather database
#'
#' @section Details: A \pkg{rSOILWAT2} weather database has the following
#'   format: \describe{
#'   \item{Table \var{Meta}}{contains two fields \var{Desc} and \var{Value}
#'      which contain \itemize{
#'      \item the records \var{Version} and \var{Compression_type}}}
#'   \item{Table \var{Sites}}{contains four fields \var{Site_id},
#'      \var{Latitude}, \var{Longitude}, and \var{Label}}
#'   \item{Table \var{WeatherData}}{contains six fields \var{wdid}
#'      (the ID of the weather data record), \var{Site_id}, \var{Scenario}
#'      (i.e., the ID of the scenario), \var{StartYear}, \var{EndYear}, and
#'      \var{data}}
#'   \item{Table \var{Scenarios}}{contains two fields \var{id} and
#'      \var{Scenario} (i.e., the scenario name)}
#' }
#'
#' @param dbFilePath A character string. The file path of the weather database.
#'  This will be a file of type \code{sqlite3}. In-memory databases are not
#'  supported.
#' @param site_data A data.frame. The site data with column names
#'  \var{Latitude}, \var{Longitude}, and \var{Label}.
#' @param Scenarios A vector of character strings. The climate scenarios of
#'  which the first one is enforced to be \code{scen_ambient}.
#' @param scen_ambient A character string. The first/default climate scenario.
#' @param compression_type A character string. The type of compression for
#'  the weather blob. See \code{\link[base]{memCompress}} for the available
#'  choices.
#' @param verbose A logical value.
#' @param ... Additional/deprecated arguments which are currently ignored.
#'
#' @return \code{TRUE} on success; \code{FALSE} otherwise. If the file
#'   \code{dbFilePath} didn't already exist, but creating it failed, then the
#'   attempt will be disconnected and removed.
#' @export
dbW_createDatabase <- function(dbFilePath = "dbWeatherData.sqlite3", site_data,
  Scenarios, scen_ambient = "Current", compression_type = "gzip",
  verbose = FALSE, ...) {

  dbFilePath <- file.path(normalizePath(dirname(dbFilePath)),
    basename(dbFilePath))

  rm_file <- FALSE
  on.exit({
    if (rm_file && file.exists(dbFilePath)) {
      if (verbose) {
        message("'dbW_createDatabase': deletes db-file due to failure.")
      }

      temp <- dbW_disconnectConnection()
      if (!temp) {
        message("'dbW_createDatabase': attempted to disconnect from db-file ",
          "but failed.")
      }

      # sqlite3 on Windows OS may not be releasing the file until
      # garbage collection
      gc()

      temp <- unlink(dbFilePath)
      if (temp != 0) {
        message("'dbW_createDatabase': attempted to delete db-file but ",
          "'unlink' suggests a failure.")
      }

      if (file.exists(dbFilePath)) {
        message("'dbW_createDatabase': attempted to delete db-file with ",
          "'unlink' but db-file is still present.")
        file.remove(dbFilePath)
      }

      if (file.exists(dbFilePath))
        message("'dbW_createDatabase': attempted to delete db-file with ",
          "'file.remove' but db-file is still present.")
    }
  }, add = TRUE)

  dots <- list(...)
  if (length(dots)) {
    message(paste("'dbW_createDatabase': arguments ignored/deprecated",
      paste(shQuote(names(dots)), collapse = ", ")))
  }

  if (file.exists(dbFilePath)) {
    if (verbose) {
      message(paste("'dbW_createDatabase': cannot create a new database",
        "because the file", shQuote(basename(dbFilePath)),
        "does already exist."))
    }
    return(FALSE)
  }

  temp <- dbW_setConnection(dbFilePath, create_if_missing = TRUE,
    verbose = verbose)

  if (!temp) {
    if (verbose) {
      message(paste("'dbW_createDatabase': was not able to create a new",
        "database and connect to the file", shQuote(basename(dbFilePath)), "."))
    }
    rm_file <- TRUE
    return(FALSE)
  }

  # Meta information
  temp <- eval(formals(memCompress)[[2]])
  if (missing(compression_type) || !(compression_type %in% temp)) {
    compression_type <- rSW2_glovars$default_blob_compression_type
  }
  rSW2_glovars$blob_compression_type <- compression_type

  # Create tables
  temp <- try(.create_dbW(site_data, Scenarios, scen_ambient), silent = TRUE)
  res <- !inherits(temp, "try-error")

  if (!res) {
    if (verbose) {
      message(paste("'dbW_createDatabase': was not able to create a new",
        "database", shQuote(basename(dbFilePath)), "because of errors in the",
        "table data."))
    }
    rm_file <- TRUE
  }

  res
}


#dataframe of columns folder, lat, long, label where label can equal folderName
#' @export
dbW_addFromFolders <- function(MetaData = NULL, FoldersPath,
  ScenarioName = "Current", weather_tag = "weath") {

  if (!is.null(MetaData)) {
    temp <- apply(MetaData, MARGIN = 1, function(x)
      dbW_addWeatherData(Site_id = NULL, lat = x[2], long = x[3],
      weatherFolderPath = file.path(FoldersPath, x[1]),
      weatherData = NULL, Label = x[4], Scenario = ScenarioName,
        weather_tag = weather_tag))

  } else {
    files <- list.files(path = FoldersPath, pattern = weather_tag)
    temp <- lapply(files, function(x)
      dbW_addWeatherData(Site_id = NULL, lat = NULL, long = NULL,
      weatherFolderPath = file.path(FoldersPath, x), weatherData = NULL,
      Scenario = ScenarioName, weather_tag = weather_tag))
  }

  invisible(TRUE)
}

#' Delete a site and all associated weather data from a registered weather
#' database
#' @inheritParams check_content
#' @return An invisible logical value indicating success with \code{TRUE} and
#'   failure with \code{FALSE}.
#' @export
dbW_deleteSite <- function(Site_ids) {
  stopifnot(dbW_IsValid())

  # First delete all weather data (so that foreign key constraint is not
  # violated)
  stopifnot(dbW_deleteSiteData(Site_ids, Scenario_id = NULL))

  # Delete site entry in Sites table
  sql <- "DELETE FROM \"Sites\" WHERE Site_id=:x"
  DBI::dbExecute(rSW2_glovars$con, sql, params = list(x = Site_ids))

  invisible(TRUE)
}

#' Delete a weather data record from a registered weather database
#' @inheritParams check_content
#' @return An invisible logical value indicating success with \code{TRUE} and
#'   failure with \code{FALSE}.
#' @export
dbW_deleteSiteData <- function(Site_id, Scenario_id = NULL) {
  stopifnot(dbW_IsValid())

  if (is.null(Scenario_id)) {
    #Remove all data for this site
    sql <- "DELETE FROM \"WeatherData\" WHERE Site_id=:x"
    DBI::dbExecute(rSW2_glovars$con, sql, params = list(x = Site_id))

  } else {
    # Remove data for specific scenario
    sql <- "DELETE FROM \"WeatherData\" WHERE Site_id=:x1 AND Scenario=:x2"
    DBI::dbExecute(rSW2_glovars$con, sql,
      params = list(x1 = Site_id, x2 = Scenario_id))
  }

  invisible(TRUE)
}


## ------ Conversion of weather data formats

#' Conversion: (Compressed) raw vector (e.g., SQL-retrieved blob) to
#' (uncompressed) object
#'
#' The \pkg{rSOILWAT2} SQLite-DB which manages daily weather data (each as a
#' list of elements of class \code{\linkS4class{swWeatherData}}), uses
#' internally (compressed) blobs. This function is used to convert the blob
#' object to the object used by \pkg{rSOILWAT2}'s simulation functions.
#'
#' @param data_blob A raw vector
#' @param type A character string. One of \code{c("gzip", "bzip2", "xz",
#'   "none")}.
#'
#' @seealso \code{\link{memDecompress}}, \code{\link{unserialize}}
#' @export
dbW_blob_to_weatherData <- function(data_blob, type = "gzip") {
  # RSQLite versions < 2.0 return a list of 'raw'; starting with v >= 2.0,
  #  the class changed to 'blob'

  if ((inherits(data_blob, "list") || inherits(data_blob, "blob")) &&
    inherits(data_blob[[1]], "raw") && length(data_blob) == 1) {
    data_blob <- data_blob[[1]]
  }

  unserialize(memDecompress(data_blob, type = type))
}

#' Conversion: R object to (compressed) \var{SQL-blob-ready} character vector
#'
#' The \pkg{rSOILWAT2} database which manages daily weather data (each as a
#' list of elements of class \code{\linkS4class{swWeatherData}}), uses
#' internally (compressed) blobs. This function is used to a list of daily
#' weather data used by \pkg{rSOILWAT2}'s simulation functions to a blob object
#' which can be inserted into a SQLite DB.
#'
#' @param weatherData A list of elements of class
#'   \code{\linkS4class{swWeatherData}} or any suitable object.
#' @param type A character string. One of
#'   \code{c("gzip", "bzip2", "xz", "none")}.
#'
#' @seealso \code{\link[base]{memCompress}}, \code{\link{serialize}}
#' @export
dbW_weatherData_to_blob <- function(weatherData, type = "gzip") {
  blob::as_blob(memCompress(serialize(weatherData, connection = NULL),
    type = type))
}



#----- Conversion: reading of SOILWAT input text files to object of class
# \code{\linkS4class{swWeatherData}}

#' Reads daily weather data from files
#'
#' \pkg{SOILWAT2} does not handle missing weather data. If you have missing
#' data, then you have to impute yourself or use the built-in Markov weather
#' generator (see examples for \code{\link{sw_exec}}).
#'
#' @param LookupWeatherFolder A character string. The path to the parent folder
#'   of \code{weatherDirName}.
#' @param weatherDirName String. Name of the folder with the daily weather data
#'   files.
#' @param filebasename String. File prefix for weather data. Usually
#'   \var{weath}.
#' @param startYear Numeric. Extracted weather data will start with this year.
#' @param endYear Numeric. Extracted weather data will end with this year.
#'
#' @return A list of elements of class \code{\linkS4class{swWeatherData}}.
#'
#' @seealso \code{\link{dbW_getWeatherData}}
#'
#' @examples
#'
#' path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")
#'
#' ## ------ Simulation with data prepared beforehand and separate weather data
#' ## Read inputs from files on disk
#' sw_in3 <- sw_inputDataFromFiles(dir = path_demo, files.in = "files.in")
#'
#' ## Read forcing weather data from files on disk (there are also functions
#' ##   to set up a SQLite database for the weather data)
#' sw_weath3 <- getWeatherData_folders(
#'    LookupWeatherFolder = file.path(path_demo, "Input"),
#'    weatherDirName = "data_weather", filebasename = "weath",
#'    startYear = 1979, endYear = 2010)
#'
#' ## List of the slots of the input objects of class 'swWeatherData'
#' utils::str(sw_weath3, max.level=1)
#'
#' ## Execute the simulation run
#' sw_out3 <- sw_exec(inputData = sw_in3, weatherList = sw_weath3)
#'
#' @export
getWeatherData_folders <- function(LookupWeatherFolder, weatherDirName = NULL,
  filebasename = "weath", startYear = NULL, endYear = NULL) {

  if (is.null(LookupWeatherFolder) || is.null(filebasename)) {
    stop("Need 'LookupWeatherFolder' and 'filebasename' ",
      "to locate weather data")
  }

  dir_weather <- if (is.null(weatherDirName)) {
      LookupWeatherFolder
    } else {
      file.path(LookupWeatherFolder, weatherDirName)
    }
  fweath <- tryCatch(list.files(dir_weather, pattern = filebasename), warning =
      function(w) stop("Path to weather data bad or filebasename not correct."))

  if (!endsWith(filebasename, ".")) {
    filebasename <- paste0(filebasename, ".")
  }
  years <- as.numeric(sub(pattern = filebasename, replacement = "", fweath))
  stopifnot(!anyNA(years))
  index <- select_years(years, startYear, endYear)

  weathDataList <- list()
  for (k in seq_along(index)) {
    weathDataList[[k]] <- swReadLines(new("swWeatherData",
      year = years[index[k]]), file.path(dir_weather, fweath[index[k]]))
  }
  names(weathDataList) <- as.character(years[index])

  weathDataList
}


#' Deal with missing weather values: convert to NAs
#'
#' Missing weather values may be coded with \code{NA},
#'   with the corresponding \var{SOILWAT2} value (i.e.,
#'   \code{rSOILWAT2:::rSW2_glovars[["kSOILWAT2"]][["kNUM"]][["SW_MISSING"]]}),
#'   or with the value of the argument \code{valNA}.
#'
#' @param data A numerical object.
#' @param valNA The (numerical) value of missing weather data.
#'   If \code{NULL}, then default values are interpreted as missing.
#'
#' @return \code{data} where \pkg{SOILWAT2} missing values are converted to
#'   R-compatible \code{NA}s.
#' @export
set_missing_weather <- function(data, valNA = NULL) {
  if (is.null(valNA)) {
    # missing values coded as NA or in SOILWAT2' format
    data[data == rSW2_glovars[["kSOILWAT2"]][["kNUM"]][["SW_MISSING"]]] <- NA

  } else if (is.finite(valNA)) {
    # missing values coded as 'valNA'
    data[data == valNA] <- NA
  }

  data
}


#' Convert an object of class \code{\linkS4class{swWeatherData}} to a data.frame
#'
#' @inheritParams set_missing_weather
#'
#' @export
dbW_weatherData_to_dataframe <- function(weatherData, valNA = NULL) {
  do.call(rbind, lapply(weatherData, FUN = function(x) {
              temp <- set_missing_weather(x@data, valNA = valNA)
              Year <- rep(x@year, times = nrow(temp))
              cbind(Year, temp)
            }))
}

#' Conversion: object of class \code{\linkS4class{swWeatherData}} to
#' matrix of monthly values (\var{mean Tmax}, \var{mean Tmin}, \var{sum PPT})
#'
#' @inheritParams set_missing_weather
#'
#' @export
dbW_weatherData_to_monthly <- function(dailySW, na.rm = FALSE, valNA = NULL) {
  monthly <- matrix(NA, nrow = length(dailySW) * 12, ncol = 5,
    dimnames = list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm")))

  for (y in seq_along(dailySW)) {
    weath <- dailySW[[y]]
    month <- as.POSIXlt(paste(weath@year, weath@data[, "DOY"], sep = "-"),
                        format = "%Y-%j", tz = "UTC")$mon + 1
    temp <- set_missing_weather(weath@data, valNA = valNA)
    monthly[1:12 + 12 * (y - 1), ] <- data.matrix(cbind(
      Year = weath@year, Month = 1:12,
      aggregate(temp[, c("Tmax_C", "Tmin_C")],
        by = list(month), FUN = mean, na.rm = na.rm)[, 2:3],
      PPT_cm = as.vector(tapply(temp[, "PPT_cm"], month, FUN = sum,
        na.rm = na.rm))
    ))
  }

  monthly
}


#' Aggregate daily weather data.frame to weekly, monthly, or yearly values
#' @export
dbW_dataframe_aggregate <- function(dailySW,
  time_step = c("Year", "Month", "Week", "Day"), na.rm = FALSE) {

  time_step <- match.arg(time_step)

  if (time_step == "Day") {
    return(dailySW)
  }

  icol_day <- grep("DOY|Day", colnames(dailySW), ignore.case = TRUE,
    value = TRUE)

  temp <- apply(dailySW[, c("Year", icol_day)], 1, paste, collapse = "-")
  temp <- as.POSIXlt(temp, format = "%Y-%j", tz = "UTC")
  ytemp <- 1900L + unique(temp$year)

  if (time_step == "Year") {
    idaggs <- list(dailySW[, "Year"])
    hout <- data.frame(Year = ytemp)

  } else if (time_step == "Month") {
    idaggs <- list(1L + temp$mon, dailySW[, "Year"])
    hout <- data.frame(
      Year = rep(ytemp, each = 12),
      Month = rep(seq_len(12), times = length(ytemp))
    )

  } else if (time_step == "Week") {
    idaggs <- list(1L + floor(temp$yday / 7), dailySW[, "Year"])
    hout <- data.frame(
      Year = rep(ytemp, each = 53),
      Week = rep(seq_len(53), times = length(ytemp))
    )
  }

  as.matrix(cbind(hout,
    Tmax_C = as.vector(tapply(dailySW[, "Tmax_C"], INDEX = idaggs, FUN = mean,
      na.rm = na.rm)),
    Tmin_C = as.vector(tapply(dailySW[, "Tmin_C"], INDEX = idaggs, FUN = mean,
      na.rm = na.rm)),
    PPT_cm = as.vector(tapply(dailySW[, "PPT_cm"], INDEX = idaggs, FUN = sum,
      na.rm = na.rm))
  ))

}

#' Conversion: object of daily weather data.frame to matrix of monthly values
#' (\var{mean Tmax}, \var{mean Tmin}, \var{sum PPT})
#' @export
dbW_dataframe_to_monthly <- function(dailySW, na.rm = FALSE) {
  dbW_dataframe_aggregate(dailySW, time_step = "Month", na.rm = na.rm)
}




#' Assign years to weather data.frame
#' @param weatherDF A data.frame. data.frame containing weather information for
#' site.
#' @param years A numeric or integer vector or \code{NULL}. Vector of year data
#' where length is equal to either the number of years in the weather data.frame
#' or the number of rows in the data.frame.
#' @param weatherDF_dataColumns A vector of string values. Column names of the
#' weather data.frame.
#' @return A named list of length 2.
#' \itemize{
#'  \item \code{years} a vector of unique year values.
#'  \item \code{year_ts} a vector of time series values for each row/day of the
#' data.frame.
#' }
#' @export

get_years_from_weatherDF <- function(weatherDF, years, weatherDF_dataColumns) {
  if (!is.null(years)) {
    if (length(years) == nrow(weatherDF)) {
      year_ts <- years
    } else if (length(years) ==
        sum(weatherDF[, weatherDF_dataColumns[1]] == 1)) {
      year_ts <- rep(years, times = diff(c(
        which(weatherDF[, weatherDF_dataColumns[1]] == 1),
        nrow(weatherDF) + 1)))
    } else {
      stop("Not sufficient year information was provided with the ",
        "'weatherDF' object")
    }

  } else if (
    any(temp <- grepl("year", colnames(weatherDF), ignore.case = TRUE))) {

    year_ts <- weatherDF[, which(temp)[1]]

  } else {
    stop("No year information was provided with the 'weatherDF' object")
  }

  years <- sort(unique(year_ts))

  return(list(years = years, year_ts = year_ts))
}


#' Conversion: data.frame to object of class \code{\linkS4class{swWeatherData}}
#' @export
dbW_dataframe_to_weatherData <- function(weatherDF, years = NULL,
  weatherDF_dataColumns = c("DOY", "Tmax_C", "Tmin_C", "PPT_cm"), round = 2) {

  if (!(length(weatherDF_dataColumns) == 4) ||
      !all(weatherDF_dataColumns %in% colnames(weatherDF))) {
    stop("Not every required weatherDF_dataColumns is available in the ",
      "'weatherDF' object")
  }

  ylist <- get_years_from_weatherDF(weatherDF, years, weatherDF_dataColumns)

  if (isTRUE(is.logical(round) && round || is.numeric(round))) {
    weatherDF <- round(weatherDF, digits = if (is.logical(round)) 2 else round)
  }

  weatherData <- list()
  for (i in seq_along(ylist$years)) {
    ydata <- as.matrix(weatherDF[ylist$year_ts == ylist$years[i],
      weatherDF_dataColumns])
    colnames(ydata) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
    weatherData[[i]] <- new("swWeatherData", year = ylist$years[i],
      data = ydata)
  }
  names(weatherData) <- ylist$years

  weatherData
}


#' Conversion: object of class \code{\linkS4class{swWeatherData}} or
#' data.frame to \pkg{SOILWAT} input text files
#' @export
dbW_weather_to_SOILWATfiles <- function(path, site.label,
  weatherData = NULL, weatherDF = NULL, years = NULL,
  weatherDF_dataColumns = c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")) {

  stopifnot(is.null(weatherData) || is.null(weatherDF))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  if (!is.null(weatherData)) {
    years <- sapply(weatherData, FUN = function(x) x@year)

  } else if (!is.null(weatherDF)) {
    if (!(length(weatherDF_dataColumns) == 4) ||
        !all(weatherDF_dataColumns %in% colnames(weatherDF))) {
      stop("Not every required weatherDF_dataColumns is available in the ",
        "'weatherDF' object")
    }

    temp <- get_years_from_weatherDF(weatherDF, years, weatherDF_dataColumns)
    years <- temp$years
    year_ts <- temp$year_ts

  } else {
    stop("Provide daily weather data either as 'weatherData' or ",
      "'weatherDF' object")
  }

  for (y in seq_along(years)) {
    data.sw <- if (!is.null(weatherData)) {
        weatherData[[y]]@data
      } else {
        weatherDF[year_ts == years[y], weatherDF_dataColumns]
      }
    sw.filename <- file.path(path, paste0("weath.", years[y]))
    sw.comments <- c(paste("# weather for site", site.label, "year = ",
      years[y]), "# DOY Tmax(C) Tmin(C) PPT(cm)")

    utils::write.table(
      sw.comments,
      file = sw.filename,
      sep = "\t",
      eol = "\r\n",
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )

    utils::write.table(
      data.frame(data.sw[, 1],
        formatC(data.sw[, 2], digits = 2, format = "f"),
        formatC(data.sw[, 3], digits = 2, format = "f"),
        formatC(data.sw[, 4], digits = 2, format = "f")
      ),
      file = sw.filename,
      append = TRUE,
      sep = "\t",
      eol = "\r\n",
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )
  }

  invisible(years)
}


#' Transfer existing weather data to a different (Gregorian) calendar (period)
#'
#' This function can transfer from existing weather data to, e.g.,
#' different years / a subset of years (partially overlapping or not), or
#' can convert from a non-leap to a Gregorian calendar.
#'
#' @inheritParams dbW_estimate_WGen_coefs
#' @param new_startYear An integer value. The first Calendar year of the new
#'   time period. If \code{NULL}, then the first year of \code{weatherData}.
#' @param new_endYear An integer value. The last Calendar year of the new
#'   time period. If \code{NULL}, then the last year of \code{weatherData}.
#' @param type A string that affects how years of \code{weatherData} are
#'   used for transfer. If \code{"asis"}, then years of are used as is.
#'   If \code{"sequential"}, then years are re-coded to start with
#'   \code{new_startYear}.
#' @param name_year A string. Column name of the weather data that corresponds
#'   to year.
#' @param name_DOY A string. Column name of the weather data that corresponds
#'   to day of year.
#' @param name_data A vector of strings. Column names of the weather data.
#' @inheritParams set_missing_weather
#'
#' @return A data.frame formatted as a return object from function
#'   \code{\link{dbW_weatherData_to_dataframe}} with column names as given by
#'   \code{name_year}, \code{name_DOY}, and \code{name_data}.
#'
#' @section Note: The returned object may contain \code{NA}, e.g., for
#'   leap days that were added. Use function \code{\link{dbW_generateWeather}}
#'   to fill in.
#'
#' @examples
#' wdata <- rSOILWAT2::weatherData
#'
#' ## Transfer to different years (partially overlapping)
#' wnew <- dbW_convert_to_GregorianYears(wdata,
#'   new_startYear = 2000, new_endYear = 2020
#' )
#' all.equal(unique(wnew[, "Year"]), 2000:2020)
#' anyNA(wnew) # --> use `dbW_generateWeather`
#'
#' ## Transfer to a subset of years (i.e., subset)
#' wnew <- dbW_convert_to_GregorianYears(wdata,
#'   new_startYear = 2000, new_endYear = 2005
#' )
#' all.equal(unique(wnew[, "Year"]), 2000:2005)
#' anyNA(wnew)
#'
#' ## Correct/convert from a non-leap to a Gregorian calendar
#' wempty <- data.frame(dbW_weatherData_to_dataframe(
#'   list(new("swWeatherData"))))[1:365, ]
#'
#' wnew <- dbW_convert_to_GregorianYears(wempty,
#'   new_startYear = 2016, new_endYear = 2016
#' )
#' all.equal(unique(wnew[, "Year"]), 2016:2016)
#' all.equal(nrow(wnew), 366) # leap year
#'
#' @export
dbW_convert_to_GregorianYears <- function(weatherData,
  new_startYear = NULL, new_endYear = NULL, type = c("asis", "sequential"),
  name_year = "Year", name_DOY = "DOY",
  name_data = c("Tmax_C", "Tmin_C", "PPT_cm"), valNA = NULL) {

  # daily weather data
  if (inherits(weatherData, "list") &&
      all(sapply(weatherData, inherits, what = "swWeatherData"))) {
    wdata <- data.frame(dbW_weatherData_to_dataframe(weatherData,
      valNA = valNA))
  } else {
    wdata <- data.frame(set_missing_weather(weatherData, valNA = valNA))
  }

  # new Calendar years
  if (is.null(new_startYear)) {
    new_startYear <- min(wdata[, name_year])
  }

  if (is.null(new_endYear)) {
    new_endYear <- max(wdata[, name_year])
  }

  # Relabel input years (if requested)
  type <- match.arg(type)

  if (type == "sequential") {
    old_startYear <- min(wdata[, name_year])

    if (old_startYear != new_startYear) {
      delta <- new_startYear - old_startYear
      wdata[, name_year] <- wdata[, name_year] + delta
    }
  }

  # Create data.frame for new Calendar years
  tdays <- rSW2utils::days_in_years(
    start_year = new_startYear,
    end_year = new_endYear
  )

  tdays1 <- as.POSIXlt(tdays)

  wdata2 <- data.frame(
    Year = 1900 + tdays1$year,
    DOY = 1 + tdays1$yday,
    var1 = NA,
    var2 = NA,
    var3 = NA
  )
  colnames(wdata2) <- c(name_year, name_DOY, name_data)

  # Transfer existing values
  tmp <- apply(wdata[, c(name_year, name_DOY)], 1, paste, collapse = "/")
  id_xdf <- format(as.Date(tmp, format = "%Y/%j"))
  id_xdf2 <- format(as.Date(tdays))
  id_match <- match(id_xdf2, id_xdf, nomatch = 0)

  wdata2[id_match > 0, name_data] <- wdata[id_match, name_data]

  wdata2
}



#' Check that weather data is well-formed
#'
#' Check that weather data is organized in a list
#' where each element is of class \code{\linkS4class{swWeatherData}}, and
#' represents daily data for one Gregorian year
#'
#' @param x An object.
#'
#' @return A logical value.
#'
#' @export
dbW_check_weatherData <- function(x) {
  length(x) > 0 &&
  inherits(x, "list") &&
  all(sapply(x, inherits, what = "swWeatherData")) &&
  isTRUE(all.equal(
    unname(sapply(x, function(xyr) nrow(slot(xyr, "data")))),
    365 + as.integer(rSW2utils::isLeapYear(
      sapply(x, slot, name = "year")
    ))
  ))
}


# nolint start
## ------ Scanning of SOILWAT input text files
readCharacter <- function(text, showWarnings = FALSE) {
  temp <- strsplit(x = text, split = "\t")[[1]][1]
  temp <- unlist(strsplit(x = temp, split = " "))[1]
  temp
}

readInteger <- function(text,showWarnings=FALSE) {
  temp <- suppressWarnings(as.integer(strsplit(x=text,split="\t")[[1]][1]))
  if(is.na(temp)) {
    if(showWarnings) print(paste("Line: ",text,sep=""))
    if(showWarnings) print("Not formatted with \t. Going to try [space].")
    temp <- suppressWarnings(as.integer(strsplit(x=text,split=" ")[[1]][1]))
    if(is.na(temp)) {
      stop("Bad Line. Or Bad line numbers.")
    }
  }
  return(temp)
}

readLogical <- function(text,showWarnings=FALSE) {
  temp <- suppressWarnings(as.logical(as.integer(strsplit(x=text,split="\t")[[1]][1])))
  if(is.na(temp)) {
    if(showWarnings) print(paste("Line: ",text,sep=""))
    if(showWarnings) print("Not formatted with \t. Going to try [space].")
    temp <- suppressWarnings(as.logical(as.integer(strsplit(x=text,split=" ")[[1]][1])))
    if(is.na(temp)) {
      stop("Bad Line. Or Bad line numbers.")
    }
  }
  return(temp)
}

readNumeric <- function(text,showWarnings=FALSE) {
  temp <- suppressWarnings(as.numeric(strsplit(x=text,split="\t")[[1]][1]))
  if(is.na(temp)) {
    if(showWarnings) print(paste("Line: ",text,sep=""))
    if(showWarnings) print("Not formatted with \t. Going to try [space].")
    temp <- suppressWarnings(as.numeric(strsplit(x=text,split=" ")[[1]][1]))
    if(is.na(temp)) {
      stop("Bad Line. Or Bad line numbers.")
    }
  }
  return(temp)
}

readNumerics <- function(text,expectedArgs,showWarnings=FALSE) {
  temp <- strsplit(x=text,split="\t")[[1]]
  temp <- temp[temp != ""] #get rid of extra spaces
  if(length(temp) > expectedArgs) temp <- temp[1:expectedArgs] #get rid of comment?
  temp <- suppressWarnings(as.numeric(temp))
  if(any(is.na(temp))) {
    if(showWarnings & any(is.na(temp))) print(paste("Line: ",text,sep=""))
    if(showWarnings & any(is.na(temp))) print("Not formatted with \t. Going to try [space].")
    temp <- strsplit(x=text,split="\t")[[1]][1] #remove comment
    temp <- strsplit(x=temp,split=" ")[[1]]
    temp <- temp[temp!=""] #remove extra spaces
    temp <- suppressWarnings(as.numeric(temp[1:expectedArgs]))
    if(any(is.na(temp))) {
      #last try. tried set by \t then by space. Now try both
      temp <- strsplit(x=text,split=" ",fixed=T)[[1]]
      temp <- unlist(strsplit(x=temp,split="\t",fixed=T))
      temp <- temp[temp!=""] #remove extra spaces
      temp <- suppressWarnings(as.numeric(temp[1:expectedArgs]))
      if(any(is.na(temp))) stop("Bad Line. Or Bad line numbers.")
    }
  }
  if(length(temp) != expectedArgs) {
    if(showWarnings) print(paste("Line: ",text,sep=""))
      stop(paste("Expected ",expectedArgs," Got ",length(temp),sep=""))
  }
  return(temp)
}
# nolint end
