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


dbW_getWeatherData_old <- function(Site_id=NULL,lat=NULL,long=NULL,Label=NULL,startYear=NULL,endYear=NULL, Scenario="Current") {
	.Deprecated("dbW_getWeatherData")
	stopifnot(dbW_IsValid())

	if(is.null(Site_id) && is.null(Label) && is.null(lat) && is.null(long)) {
		stop("No way to locate weather data from input")
	}

	useYears<-FALSE
	useStart<-FALSE
	useEnd  <-FALSE
	if(!is.null(startYear) | !is.null(endYear)) {#See if we should narrow the start end year range
		startYear <- as.integer(startYear)
		if(!is.na(startYear)) useStart<-TRUE
		endYear <- as.integer(endYear)
		if(!is.na(endYear)) useEnd<-TRUE
		if(useStart | useEnd) useYears<-TRUE
		if(useStart & useEnd) {
			if(startYear >= endYear | startYear<0 | endYear<0) {
				stop("Wrong start or end year")
			}
		}
	}
	Site_id<-as.integer(Site_id)
	if(length(Site_id) == 0) {
		Site_id <- dbW_getSiteId(lat,long,Label)
	} else {
		if(!DBI::dbGetQuery(rSW2_glovars$con, paste("SELECT COUNT(*) FROM WeatherData WHERE Site_id=",Site_id,";",sep=""))[1,1]) {
			stop("Site_id does not exist.")
		}
	}
	if(!is.null(Site_id) && is.integer(Site_id) && Site_id >= 0) {
		Scenario <- DBI::dbGetQuery(rSW2_glovars$con, paste("SELECT id FROM Scenarios WHERE Scenario='",Scenario,"';",sep=""))[1,1]
		result <- DBI::dbGetQuery(rSW2_glovars$con, paste("SELECT StartYear,EndYear,data FROM WeatherData WHERE Site_id=",Site_id, " AND Scenario=",Scenario,";",sep=""));
		data <- dbW_blob_to_weatherData_old(result$StartYear, result$EndYear, result$data, rSW2_glovars$blob_compression_type)
		if(inherits(data, "try-error")) stop(paste("Weather data for Site_id", Site_id, "is corrupted"))
	} else {
		stop(paste("Site_id for", Label, "not obtained."))
	}

	if(useYears) {
		if(useStart && useEnd) {
		        # adjusting so we actually explore the values of the "year" slots of our soilwatDB object list
			# startYear_idx <- match(startYear,as.integer(names(data)))
			startYear_idx <- match(startYear,
			                       as.integer(unlist(lapply(data, FUN=slot, "year"))))
			# endYear_idx <- match(endYear,as.integer(names(data)))
			endYear_idx <- match(endYear,
					     as.integer(unlist(lapply(data, FUN=slot, "year"))))
			data <- data[startYear_idx:endYear_idx]
		} else if(useStart) {
			#startYear_idx <- match(startYear,as.integer(names(data)))
			startYear_idx <- match(startYear,
					       as.integer(unlist(lapply(data, FUN=slot, "year"))))
			# data <- data[startYear_idx:length(as.integer(names(data)))]
			data <- data[startYear_idx:length(as.integer(unlist(lapply(data, FUN=slot, "year"))))]
		} else if(useEnd) {
			# endYear_idx <- match(endYear,as.integer(names(data)))
			endYear_idx <- match(endYear,
			                     as.integer(unlist(lapply(data, FUN=slot, "year"))))
			data <- data[1:endYear_idx]
		}
	}
	return(data)
}

dbW_addWeatherData_old <- function(Site_id=NULL, lat=NULL, long=NULL, weatherFolderPath=NULL, weatherData=NULL, label=NULL, ScenarioName="Current") {
	.Deprecated("dbW_addWeatherData")
	stopifnot(dbW_IsValid())

	if( (is.null(weatherFolderPath) | ifelse(!is.null(weatherFolderPath), (weatherFolderPath == "" | !file.exists(weatherFolderPath)), FALSE)) & (is.null(weatherData) | !is.list(weatherData) | class(weatherData[[1]]) != "swWeatherData") ) stop("addWeatherDataToDataBase does not have folder path or weatherData to insert")
	if( (is.null(Site_id) & is.null(lat) & is.null(long) & is.null(weatherFolderPath) & (is.null(label))) | ((!is.null(Site_id) & !is.numeric(Site_id)) & (!is.null(lat) & !is.numeric(lat)) & (!is.null(long) & !is.numeric(long))) ) stop("addWeatherDataToDataBase not enough info to create Site in Sites table.")

	Site_id <- dbW_addSite(
		Site_id = Site_id,
		lat = lat,
		long = long,
		Label = if (!is.null(weatherFolderPath) && is.null(label)) basename(weatherFolderPath) else label)

	Scenarios <- DBI::dbReadTable(rSW2_glovars$con,"Scenarios")$Scenario
	if(ScenarioName %in% Scenarios) {
		scenarioID <- which(ScenarioName %in% Scenarios)
	} else {
		temp <- DBI::dbGetQuery(rSW2_glovars$con, "SELECT MAX(id) FROM \"Scenarios\";")[1,1]
		scenarioID <- ifelse(is.na(temp),1,temp+1)
		SQL <- paste("INSERT INTO \"Scenarios\" VALUES(",scenarioID,",'",ScenarioName,"');",sep="")
		DBI::dbExecute(rSW2_glovars$con, SQL)
	}

	if(!is.null(weatherData)) {
		data_blob <- dbW_weatherData_to_blob_old(weatherData, rSW2_glovars$blob_compression_type)
		temp <- as.integer(names(weatherData))
		StartYear <- temp[1]
		EndYear <- temp[length(temp)]
		DBI::dbExecute(rSW2_glovars$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,", ",scenarioID,", ",StartYear,", ",EndYear,", ",data_blob,");",sep=""))
		#dbCommit(rSW2_glovars$con)
	} else {
		weath <- list.files(weatherFolderPath)
		years <- as.numeric(sub(pattern="weath.",replacement="",weath))
		weatherData <- list()
		for(j in 1:length(weath)) {
			year <- as.numeric(sub(pattern="weath.",replacement="",weath[j]))
			temp <- utils::read.csv(file.path(weatherFolderPath,weath[j]),header=FALSE,skip=2,sep="\t")
			weatherData[[j]] <- swReadLines(new("swWeatherData",year),file.path(weatherFolderPath,weath[j]))
		}
		StartYear <- years[1]
		EndYear <- years[length(years)]
		data_blob <- dbW_weatherData_to_blob_old(weatherData, rSW2_glovars$blob_compression_type)
		DBI::dbExecute(rSW2_glovars$con, paste("INSERT INTO WeatherData (Site_id, Scenario, StartYear, EndYear, data) VALUES (",Site_id,", ",scenarioID,", ",StartYear,", ",EndYear,", ",data_blob,");",sep=""))
		#dbCommit(rSW2_glovars$con)
	}
}


# Conversion: SQL-blob to object of class 'swWeatherData'
dbW_blob_to_weatherData_old <- function(StartYear, EndYear, data_blob, type = "gzip") {
	.Deprecated("dbW_blob_to_weatherData")
	if ((inherits(data_blob, "list") || inherits(data_blob, "blob")) &&
		inherits(data_blob[[1]], "raw") && length(data_blob) == 1) {
		data_blob <- data_blob[[1]]
	}
	data <- strsplit(memDecompress(data_blob, type = type, asChar = TRUE), ";")[[1]]
	years <- StartYear:EndYear

	weatherData <- list()
	for (i in seq_along(years)) {
		zz <- textConnection(data[i])
		ydata <- utils::read.table(zz, header = FALSE, sep = ",", stringsAsFactors = FALSE)
		close(zz)
		ydata <- as.matrix(cbind(seq_len(nrow(ydata)), ydata))
		colnames(ydata) <- c("DOY", "Tmax_C", "Tmin_C", "PPT_cm")
		weatherData[[i]] <- new("swWeatherData", year = years[i], data = ydata)
	}
	names(weatherData) <- years

	weatherData
}

# Conversion: object of class 'swWeatherData' to SQL-blob
dbW_weatherData_to_blob_old <- function(weatherData, type = "gzip") {
	.Deprecated("dbW_weatherData_to_blob")
	dataString <- NULL
	string <- character(length = length(weatherData))
	for(i in seq_along(weatherData)) {
		rm(dataString)
		zz <- textConnection("dataString", "w")
		utils::write.table(x = weatherData[[i]]@data[, -1], file = zz, col.names = FALSE, sep = "," , row.names = FALSE)
		close(zz)
		string[i] <- paste(dataString, collapse = "\n")
	}
	string <- paste(string, collapse=";")

	paste0("x'", paste0(memCompress(string, type = type), collapse = ""), "'")
}



#' @export
dbW_addSite <- function(Site_id = NULL, lat = NULL, long = NULL, Label = NULL) {
	.Deprecated("dbW_addSites")
	stopifnot(dbW_IsValid())

	#Does Site_id exist
	Site_id <- as.integer(Site_id)
	if (length(Site_id) == 0) { #Site_id is NULL or integer(0)
		Site_id <- dbW_getSiteId(lat, long, Label)
	}
	stopifnot(length(Site_id) == 1L)

	if (!dbW_has_siteIDs(Site_id)) {
		# Site_id does not exist in database: create it
		if (is.null(lat)) lat <- "NULL"
		if (is.null(long)) long <- "NULL"
		Label <- if (is.null(Label)) "NULL" else shQuote(Label)
		sql <- "SELECT MAX(Site_id) FROM Sites"
		temp <- DBI::dbGetQuery(rSW2_glovars$con, sql)[1,1]
		Site_id <- if (is.na(temp)) 1L else {temp + 1}
		sql <- paste0("INSERT INTO Sites VALUES(", Site_id, ", ", lat, ", ", long, ", ",
			Label, ")")
		DBI::dbExecute(rSW2_glovars$con, sql)

	} else {
		# Site_id exists already
		sql <- paste("SELECT * FROM Sites WHERE Site_id=", Site_id)
		SiteData <- DBI::dbGetQuery(rSW2_glovars$con, sql)

		bad_lat <- !is.null(lat) &&
			!(is.null(SiteData[1, "Latitude"]) || identical(SiteData[1, "Latitude"], "NULL")) &&
			SiteData[1, "Latitude"] != lat
		bad_long <- !is.null(long) &&
			!(is.null(SiteData[1, "Longitude"]) || identical(SiteData[1, "Longitude"], "NULL")) &&
			SiteData[1, "Longitude"] != long
		bad_label <- !is.null(Label) && nchar(Label) > 0 &&
			!(is.null(SiteData[1, "Label"]) || identical(SiteData[1, "Label"], "NULL")) &&
			SiteData[1, "Label"] != Label
		if (bad_lat || bad_long || bad_label) {
				stop("Site_id: ", Site_id, " already existed in database, but data mismatch ",
				"with NULL being ignored. Compare data (database:input) ",
				"lat(", SiteData[1, "Latitude"], ":", lat, ") ",
				"long(", SiteData[1, "Longitude"], ":", long, ") ",
				"label(", SiteData[1, "Label"], ":", Label, ").")
		}
	}

	Site_id
}
