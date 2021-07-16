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

## ------SQLite weather database function: upgrade

#' Weather database upgrade functions
#'
#' @param dbWeatherDataFile A character string. The path to the weather database
#'   file.
#' @param fbackup A character string. The path to where the weather database
#'   should be backed up. If \code{NULL}, then '_copy' is appended to
#'   \code{dbWeatherDataFile}.
#'
#' @name dbW_upgrade
NULL

#' @rdname dbW_upgrade
#'
#' @section Details: \code{dbW_upgrade_to_rSOILWAT2} upgrades a weather database
#'   that was created under \pkg{Rsoilwat31} to the current package version
#'   \pkg{rSOILWAT2}
#'
#' @inheritParams dbW_upgrade
#' @param check_all A logical value. If \code{TRUE}, then every record is
#'   checked; otherwise, only the first record is checked for the package
#'   version.
#' @param with_resume A logical value. If \code{TRUE}, then digest of call and
#'   progress status are stored in a temporary file on disk on the same path as
#'   \code{dbWeatherDataFile}. If such a temporary file is present, then upgrade
#'   is resumed from where progress status indicates. Otherwise, ignore
#'   temporary disk files and attempt to upgrade database records from scratch.
#' @param clean_cache A logical value. If \code{TRUE}, then temporary file
#'   generated with \code{with_resume} set to \code{TRUE}, if present, is
#'   removed once upgrade completed successfully.
#'
#' @export
dbW_upgrade_to_rSOILWAT2 <- function(dbWeatherDataFile, fbackup = NULL,
  check_all = FALSE, with_resume = TRUE, clean_cache = TRUE) {

  print(paste(Sys.time(), ": upgrading database", basename(dbWeatherDataFile),
    "to package 'rSOILWAT2'"))

  dbWeatherDataFile <- normalizePath(dbWeatherDataFile)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWeatherDataFile)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Backup copy
  fbackup <- backup_copy(dbWeatherDataFile, fbackup)

  # Prepare call objects
  temp <- strsplit(basename(dbWeatherDataFile), split = ".", fixed = TRUE)
  temp <- paste0(temp[[1]][-length(temp[[1]])], collapse = ".")

  call_id <- list(f = "rSOILWAT2::dbW_upgrade_to_rSOILWAT2",
    dbWeatherDataFile = basename(dbWeatherDataFile),
    fbackup = basename(fbackup),
    f_cache = paste0(".cache_dbW_upgrade_to_rSOILWAT2__", temp))

  call_cache <- list(v_dbW = NULL, type = NULL, ids = NULL, n_ids = NULL,
    seq_ids = NULL, k = NULL)

  if (with_resume) {
    stopifnot(requireNamespace("digest"))

    # Create hash of current call
    call_hash <- digest::digest(call_id, algo = "sha512", errormode = "silent")

    # Check hash with previous call
    ftemp_cache <- file.path(dirname(dbWeatherDataFile), call_id[["f_cache"]])
    if (file.exists(ftemp_cache)) {
      temp <- try(readRDS(ftemp_cache), silent = TRUE)

      if (inherits(temp, "try-error")) {
        # remove corrupt temporary file
        unlink(ftemp_cache)

      } else if (identical(call_hash, temp[["call_hash"]])) {
        # if identical hash, load cache from previous call
        call_cache <- temp[["call_cache"]]
        print(paste0(shQuote(call_id[["f"]]),
          ": located cache from a previous call; ",
          "upgrade will continue at record #", call_cache[["k"]]))
      }
    }

    # Write cache and hash to file on exit
    on.exit(saveRDS(list(call_hash = call_hash, call_cache = call_cache),
      file = ftemp_cache), add = TRUE)
  }

  # Check database version
  if (is.null(call_cache[["v_dbW"]])) {
    sql <- "SELECT Value FROM Meta WHERE Desc=\'Version\'"
    temp <- DBI::dbGetQuery(con, sql)[1, 1]
    call_cache[["v_dbW"]] <- numeric_version(as.character(temp))
  }

  if (!identical(call_cache[["v_dbW"]],
    numeric_version(rSW2_glovars$dbW_version))) {

    stop(shQuote(call_id[["f"]]), ": requires database version ",
      rSW2_glovars$dbW_version, "; this database is version ",
      call_cache[["v_dbW"]])
  }

  # Extract compression type
  if (is.null(call_cache[["type"]])) {
    sql <- "SELECT Value FROM Meta WHERE Desc=\'Compression_type\'"
    call_cache[["type"]] <- DBI::dbGetQuery(con, sql)[1, 1]
  }

  # Check what data are there
  print(paste(Sys.time(), ": examine database", basename(dbWeatherDataFile)))

  if (is.null(call_cache[["ids"]])) {
    call_cache[["ids"]] <- DBI::dbGetQuery(con,
      "SELECT Site_id, Scenario FROM WeatherData")
  }

  if (is.null(call_cache[["n_ids"]])) {
    call_cache[["n_ids"]] <- NROW(call_cache[["ids"]])
  }

  call_cache[["seq_ids"]] <- if (is.null(call_cache[["k"]])) {
      seq_len(call_cache[["n_ids"]])
    } else {
      # subtract 1 in case the last for-loop didn't finish properly
      max(1L, call_cache[["k"]] - 1L):call_cache[["n_ids"]]
    }

  has_old_notloaded <- FALSE
  wd_pkg <- NULL

  if (length(call_cache[["seq_ids"]]) > 0) {
    for (k in call_cache[["seq_ids"]]) {
      # (drs): it appears that for-loops cannot use a list-element as
      #   iterator variable, but we need to store k in our cache
      call_cache[["k"]] <- k

      print(paste(Sys.time(), ": processing", k, "out of",
        call_cache[["n_ids"]]))

      # Upgrade weather data within a DBI-transaction in case something goes
      # awry
      res <- DBI::dbWithTransaction(con, {

        # extract old blob
        sql <- paste("SELECT data FROM WeatherData WHERE Site_id =",
          call_cache[["ids"]][k, 1], "AND Scenario =",
          call_cache[["ids"]][k, 2])

        res <- DBI::dbGetQuery(con, sql)[1, 1]
        wd <- rSOILWAT2::dbW_blob_to_weatherData(res, call_cache[["type"]])

        # Check that the old package is available and load it
        if (check_all || is.null(wd_pkg) || k == call_cache[["seq_ids"]][1]) {

          wd_class <- if (inherits(wd, "list")) class(wd[[1]]) else class(wd)
          if (!(wd_class == "swWeatherData")) {
            stop(shQuote(call_id[["f"]]),
              ": cannot update a weather database with ",
              "data of class ", shQuote(wd_class),
              "; instead class 'swWeatherData' is required.")
          }

          wd_pkg <- attr(wd_class, "package")
          if (!has_old_notloaded) {
            if (wd_pkg == "rSOILWAT2") {
              if (!check_all) {
                print(paste("Class of weather database data is already from",
                  "package 'rSOILWAT2'; nothing to upgrade."))
                return(invisible(NULL))
              }

            } else {
              has_old_notloaded <-
                !suppressPackageStartupMessages(requireNamespace(wd_pkg))
              if (!has_old_notloaded) {
                warning("The package ", shQuote(wd_pkg), " which created the",
                  " weather data, is not available on this system.")
              }
            }
          }
        }

        if (!(wd_pkg == "rSOILWAT2")) {
          # convert weather data to class of new package
          wd_new <- lapply(wd, function(x) {
            x_data <- slot(x, "data")
            colnames(x_data) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
            new("swWeatherData", year = slot(x, "year"), data = x_data)})
          names(wd_new) <- sapply(wd, slot, "year")

          # update data in weather database
          blob_new <- rSOILWAT2::dbW_weatherData_to_blob(wd_new,
            call_cache[["type"]])
          sql <- paste("UPDATE WeatherData SET data =", blob_new,
            "WHERE Site_id =", call_cache[["ids"]][k, 1],
            " AND Scenario =", call_cache[["ids"]][k, 2])
          DBI::dbExecute(con, sql)
        }
      })

      if (k %% 1000 == 0) {
        # save cache to disk in case function is aborted and on.exit fails
        saveRDS(list(call_hash = call_hash, call_cache = call_cache),
          file = ftemp_cache)
      }
    }
  }

  # Checks and clean-up
  check_updatedDB(con)

  if (clean_cache) {
    print(paste0(shQuote(call_id[["f"]]), ": upgrade appears successful and ",
      "will remove temporary cache file from disk"))
    on.exit(unlink(ftemp_cache), add = TRUE)
  }

  invisible(0)
}


#' @rdname dbW_upgrade
#'
#' @section Details: \code{dbW_upgrade_v31to32} upgrades a weather database
#'   from version 3.1.z' to '3.2.0'
#'
#' @inheritParams dbW_upgrade
#'
#' @export
dbW_upgrade_v31to32 <- function(dbWeatherDataFile, fbackup = NULL) {

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWeatherDataFile)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  v_dbW <- try(numeric_version(as.character(DBI::dbGetQuery(con,
    "SELECT Value FROM Meta WHERE Desc=\'Version\'")[1, 1])), silent = TRUE)

  if (inherits(v_dbW, "try-error") || v_dbW < "3.1.0" || v_dbW >= "3.2.0") {
    warning("The function 'dbW_upgrade_v3to31' upgrades weather databases ",
      "from version 3.1.z to 3.2.0; this database is version ", v_dbW)
    return(FALSE)
  }

  req_tables <- c("Meta", "Sites", "Scenarios", "WeatherData")
  temp <- req_tables %in% DBI::dbListTables(con)
  if (any(!temp)) {
    warning("Missing tables:", paste(shQuote(req_tables[!temp]),
      collapse = "-"))
    return(FALSE)
  }

  print(paste(Sys.time(), ": upgrading database",
    shQuote(basename(dbWeatherDataFile)),
    "to version 3.1.0; be patient, this may take considerable time depending",
    "on the size of the database"))

  # Backup copy
  fbackup <- backup_copy(dbWeatherDataFile, fbackup)
  fbackup <- file.path(dirname(dbWeatherDataFile), basename(fbackup))

  # Retrieve meta information, sites and scenarios
  Meta <- DBI::dbReadTable(con, "Meta")
  Sites <- DBI::dbReadTable(con, "Sites")
  Scenarios <- DBI::dbReadTable(con, "Scenarios")
  wd_index <- DBI::dbGetQuery(con, "SELECT Site_id, Scenario FROM WeatherData")

  # Create new database structure
  DBI::dbDisconnect(con)
  unlink(dbWeatherDataFile)
  stopifnot(dbW_createDatabase(dbWeatherDataFile, site_data = Sites,
    Scenarios = Scenarios[["Scenario"]],
    compression_type = Meta[Meta[["Desc"]] == "Compression_type", "Value"]))

  # Copy weather data from old/backup to new database
  print(paste(Sys.time(), ": moving weather data from old to new database"))

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWeatherDataFile)
  con_old <- DBI::dbConnect(RSQLite::SQLite(), dbname = fbackup)
  on.exit(DBI::dbDisconnect(con_old), add = TRUE)

  Sites2 <- DBI::dbReadTable(con, "Sites")
  Scenarios2 <- DBI::dbReadTable(con, "Scenarios")

  wd_index2 <- data.frame(
    Site_id = {
      temp <- match(wd_index[, "Site_id"], Sites[, "Site_id"])
      temp <- match(Sites[temp, "Label"], Sites2[, "Label"])
      Sites2[temp, "Site_id"]},
    Scenario = {
      temp <- match(wd_index[, "Scenario"], Scenarios[, "id"])
      temp <- match(Scenarios[temp, "Scenario"], Scenarios2[, "Scenario"])
      Scenarios2[temp, "id"]})

  for (k in seq_len(dim(wd_index)[1])) {
    sql <- paste("SELECT StartYear, EndYear, data",
      "FROM WeatherData WHERE Site_id =", wd_index[k, "Site_id"],
      "AND Scenario =", wd_index[k, "Scenario"])
    dat <- DBI::dbGetQuery(con_old, sql)

    dbW_addWeatherDataNoCheck(Site_id = wd_index2[k, "Site_id"],
      Scenario_id = wd_index2[k, "Scenario"], StartYear = dat[1, "StartYear"],
      EndYear = dat[1, "EndYear"], weather_blob = dat[1, "data"])
  }

  # Checks and clean-up
  check_updatedDB(con)

  invisible(TRUE)
}



#' @rdname dbW_upgrade
#'
#' @section Details: \code{dbW_upgrade_v3to31} upgrades a weather database
#'  from version '3.0.x' to '3.1.0'
#'
#' @inheritParams dbW_upgrade
#' @param type_new The type of compression used to compress the weather blobs.
#'  See \code{\link[base]{memCompress}}.
#'
#' @export
dbW_upgrade_v3to31 <- function(dbWeatherDataFile, fbackup = NULL,
  type_new = "gzip") {

  print(paste(Sys.time(), ": upgrading database", basename(dbWeatherDataFile),
    "to version 3.1.0"))

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWeatherDataFile)
  v_dbW <- try(numeric_version(as.character(DBI::dbGetQuery(con,
    "SELECT Value FROM Meta WHERE Desc=\'Version\'")[1, 1])), silent = TRUE)

  if (inherits(v_dbW, "try-error") || v_dbW >= "3.1.0" || v_dbW < "3.0.0") {
    warning("The function 'dbW_upgrade_v3to31' upgrades weather databases ",
      "from version 3.0.z to 3.1.0; this database is version ", v_dbW)

    return(invisible(0))
  }

  type_old <- as.character(DBI::dbGetQuery(con,
    "SELECT Value FROM Meta WHERE Desc=\'Compression_type\'")[1, 1])

  if (!(type_new %in% eval(formals(memCompress)[[2]]))) {
    warning("The upgraded weather database cannot store BLOBs with ",
      "compression type: ", type_new)
    warning("Instead, the previous compression type: ", type_old,
      " will be used")
  }

  # Backup copy
  backup_copy(dbWeatherDataFile, fbackup)

  # Update weather blobs
  ids <- DBI::dbGetQuery(con, "SELECT Site_id, Scenario FROM WeatherData;")
  n_ids <- NROW(ids)

  blob_to_weatherData_old <- function(StartYear, EndYear, data_blob,
    type = "gzip") {

    if (inherits(data_blob, "list") || inherits(data_blob, "blob")) {
      data_blob <- data_blob[[1]]
    }

    temp <- memDecompress(data_blob, type = type, asChar = TRUE)
    data <- strsplit(temp, ";")[[1]]
    years <- StartYear:EndYear

    weatherData <- list()
    for (i in seq_along(years)) {
      zz <- textConnection(data[i])
      ydata <- utils::read.table(zz, header = FALSE, sep = ",",
        stringsAsFactors = FALSE)
      close(zz)
      ydata <- as.matrix(cbind(seq_len(nrow(ydata)), ydata))
      colnames(ydata) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      weatherData[[i]] <- new("swWeatherData", year = years[i], data = ydata)
    }
    names(weatherData) <- years

    weatherData
  }


  if (n_ids > 0) {
    for (i in seq_len(n_ids)) {
      print(paste(Sys.time(), ":", i, "out of", n_ids))

      result <- DBI::dbGetQuery(con,
        paste0("SELECT StartYear,EndYear,data FROM WeatherData WHERE Site_id =",
          ids[i, 1], " AND Scenario =", ids[i, 2], ";"))
      weatherData <- blob_to_weatherData_old(result$StartYear, result$EndYear,
        result$data, type_old)

      if (inherits(weatherData, "try-error") || length(weatherData) == 0) {
        warning("Weather data for Site_id = ", ids[i, 1], " and Scenario = ",
          ids[i, 2], " is missing or is corrupted")

      } else {
        blob_new <- paste0("x'", paste0(memCompress(serialize(weatherData,
          connection = NULL), type = type_new), collapse = ""), "'")
        DBI::dbExecute(con, paste0("UPDATE WeatherData SET data =", blob_new,
          "WHERE Site_id =", ids[i, 1], " AND Scenario =", ids[i, 2], ";"))
        rm(blob_new)

      }
    }
  }


  # Update version number
  DBI::dbExecute(con,
    "UPDATE Meta SET Value = \'3.1.0\' WHERE Desc = \'Version\';")
  if (type_new != type_old) {
    DBI::dbExecute(con, paste0("UPDATE Meta SET Value = \'", type_new,
      "\' WHERE Desc = \'Compression_type\';"))
  }

  # Checks and clean-up
  check_updatedDB(con)
  DBI::dbDisconnect(con)

  invisible(0)
}


#' @rdname dbW_upgrade
#'
#' @section Details: \code{dbW_upgrade_v2to3} upgrades a weather database
#'  from version '2.x.y' to '3.0.0'
#'
#' @inheritParams dbW_upgrade
#'
#' @export
dbW_upgrade_v2to3 <- function(dbWeatherDataFile, fbackup = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWeatherDataFile)
  v_dbW <- try(numeric_version(as.character(DBI::dbGetQuery(con,
    "SELECT Version FROM Version;"))), silent = TRUE)

  if (inherits(v_dbW, "try-error") || v_dbW >= "3" && v_dbW < "2") {
    warning("The function 'dbW_upgrade_v2to3' upgrades weather databases ",
      "from version 2.y.z to 3.0.0; this database is version ", v_dbW)
    return(invisible(0))
  }

  print(paste(Sys.time(), ": upgrading database", basename(dbWeatherDataFile),
    "to version 3.0.0"))

  # Backup copy
  backup_copy(dbWeatherDataFile, fbackup)

  # Add new table 'Meta'
  DBI::dbExecute(con,
    "CREATE TABLE \"Meta\" (\"Desc\" TEXT PRIMARY KEY, \"Value\" TEXT);")

  rs <- DBI::dbSendStatement(con, "INSERT INTO Meta VALUES(:Desc, :Value)")
  DBI::dbBind(rs, param = list(
    Desc = c("Version", "Compression_type"),
    Value = c("3.0.0", "gzip")))
  DBI::dbClearResult(rs)

  # Delete old table 'Version'
  DBI::dbExecute(con, "DROP TABLE Version;")

  # Checks and clean-up
  check_updatedDB(con)
  DBI::dbDisconnect(con)

  invisible(0)
}


#' @rdname dbW_upgrade
#'
#' @section Details: \code{dbW_upgrade_v1to2} upgrades a weather database
#'  from version '1.x.y' to '2.0.0'
#'
#' @inheritParams dbW_upgrade
#'
#' @export
dbW_upgrade_v1to2 <- function(dbWeatherDataFile, fbackup = NULL,
  SWRunInformation) {

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbWeatherDataFile)
  v_dbW <- try(numeric_version(as.character(DBI::dbGetQuery(con,
    "SELECT Version FROM Version;"))), silent = TRUE)

  if (inherits(v_dbW, "try-error") || v_dbW >= "2" && v_dbW < "1") {
    warning("The function 'dbW_upgrade_v1to2' upgrades weather databases ",
      "from version 1.y.z to 2.0.0; this database is version ", v_dbW)
    return(invisible(0))
  }

  print(paste(Sys.time(), ": upgrading database", basename(dbWeatherDataFile),
    "to version 2.0.0; be patient, this may take considerable time depending",
    "on the size of the database"))

  # Backup copy
  backup_copy(dbWeatherDataFile, fbackup)

  runIDs_sites <- which(SWRunInformation[, "Include_YN"] > 0)

  # Rename table 'Sites' as 'Sites_old'
  DBI::dbExecute(con, "ALTER TABLE Sites RENAME TO Sites_old;")


  # Add new table 'Sites'
  DBI::dbExecute(con, paste("CREATE TABLE",
    "\"Sites\" (\"Site_id\" integer PRIMARY KEY, \"Latitude\" REAL,",
    "\"Longitude\" REAL, \"Label\" TEXT);"))

  site_data <- data.frame(Site_id = SWRunInformation$site_id,
          Latitude = SWRunInformation$Y_WGS84,
          Longitude = SWRunInformation$X_WGS84,
          Label = SWRunInformation$WeatherFolder,
          stringsAsFactors = FALSE)

  temp <- c("Site_id", "Latitude", "Longitude", "Label")
  ids <- sapply(temp, function(x) x %in% colnames(site_data))
  if (NROW(site_data) > 0 && all(ids)) {
    # Default values
    MetaData <- data.frame(Site_id = seq_len(max(site_data[, "Site_id"])),
                Latitude = -999, Longitude = -999,
                Label = NA, stringsAsFactors = FALSE)

    # Fill in data
    im <- match(site_data[runIDs_sites, "Site_id"], MetaData[, "Site_id"])
    MetaData[im, c("Latitude", "Longitude", "Label")] <-
      site_data[runIDs_sites, c("Latitude", "Longitude", "Label")]

    rs <- DBI::dbSendStatement(con, paste("INSERT INTO Sites",
      "VALUES(NULL, :Latitude, :Longitude, :Label)"))
    DBI::dbBind(rs,
      param = as.list(MetaData[, c("Latitude", "Longitude", "Label")]))
    DBI::dbClearResult(rs)
  }


  # Create step by step a new WeatherData table: Update Site_id based on
  # matching labels of tables 'Sites' and 'Sites_old'
  old_ids <- DBI::dbGetQuery(con, "SELECT Site_id, Scenario FROM WeatherData;")
  # this assumes that all new Site_id are larger than the old ones:
  index_old <- sort.int(unique(old_ids[, "Site_id"]), decreasing = TRUE)

  if (length(old_ids) > 0) {
    for (iold in index_old) {
      print(paste(Sys.time(), ":", iold, "out of", length(index_old)))

      site_old <- DBI::dbGetQuery(con,
        paste("SELECT Latitude, Longitude, Label",
          "FROM Sites_old WHERE Site_id =", iold, ";"))

      if (nrow(site_old) == 1) {
        site_new <- DBI::dbGetQuery(con,
          paste0("SELECT * FROM Sites WHERE Label = \'", site_old$Label,
            "\' AND Latitude = ", site_old$Latitude, " AND Longitude = ",
            site_old$Longitude, ";"))

        if (nrow(site_new) == 1) {
          DBI::dbExecute(con, paste("UPDATE WeatherData SET Site_id =",
            site_new$Site_id, "WHERE Site_id =", iold, ";"))
        } else {
          utils::str(site_new)
          warning("No updated record for old site_id = ", iold)
        }
      } else {
        utils::str(site_old)
        warning("No record for old site_id = ", iold)
      }
    }

    cur_ids <- DBI::dbGetQuery(con,
      "SELECT Site_id, Scenario FROM WeatherData;")
  }
  DBI::dbExecute(con, "DROP TABLE Sites_old;")

  # Update version number
  DBI::dbExecute(con, "DROP TABLE Version;")
  DBI::dbExecute(con, "CREATE TABLE \"Version\" (\"Version\" TEXT);")
  DBI::dbExecute(con, "INSERT INTO Version (Version) VALUES (\"2.0.0\");")

  # Checks and clean-up
  check_updatedDB(con)
  DBI::dbDisconnect(con)

  invisible(0)
}


#' @export
check_updatedDB <- function(con) {
  print(paste0(Sys.time(),
    ": 'check_updatedDB' started with database integrity"))

  print(paste0(Sys.time(), ": 'check_updatedDB' started 'quick check'"))
  res <- DBI::dbExecute(con, "PRAGMA quick_check;")
  print(res)
  print(paste0(Sys.time(), ": 'check_updatedDB' started 'integrity check'"))
  print(DBI::dbExecute(con, "PRAGMA integrity_check;"))
  print(paste0(Sys.time(), ": 'check_updatedDB' started 'foreign key check'"))
  print(DBI::dbExecute(con, "PRAGMA foreign_key_check;"))

  print(paste0(Sys.time(), ": 'check_updatedDB' checks indices"))
  print(DBI::dbExecute(con, "PRAGMA index_list(WeatherData);"))
  print(DBI::dbExecute(con,
    "PRAGMA index_info(sqlite_autoindex_WeatherData_1);"))
}


#' @rdname dbW_upgrade
#'
#' @section Details: \code{backup_copy} creates a backup copy of a weather
#'   database file if not already present
#'
#' @inheritParams dbW_upgrade
#'
#' @export
backup_copy <- function(dbWeatherDataFile, fbackup = NULL) {
  print(paste(Sys.time(), ": backup database", basename(dbWeatherDataFile)))

  dbWeatherDataFile <- normalizePath(dbWeatherDataFile)

  dir_old <- getwd()
  on.exit(setwd(dir_old))
  setwd(dirname(dbWeatherDataFile))

  if (is.null(fbackup)) {
    fbackup <- strsplit(basename(dbWeatherDataFile), split = ".",
      fixed = TRUE)[[1]]
    fbackup <- paste0(paste(fbackup[1], collapse = ""), "_copy.",
      fbackup[length(fbackup)])

  } else {
    fbackup <- basename(fbackup)
  }

  res <- if (file.exists(dbWeatherDataFile) && !file.exists(fbackup)) {
    system2("cp", args = c(basename(dbWeatherDataFile), fbackup))
  } else if (!file.exists(dbWeatherDataFile) && file.exists(fbackup)) {
    system2("cp", args = c(fbackup, basename(dbWeatherDataFile)))
  }

  invisible(fbackup)
}
