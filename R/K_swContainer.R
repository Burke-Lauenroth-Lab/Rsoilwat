###############################################################################
#rSOILWAT2
#    Copyright (C) {2009-2018}  {Ryan Murphy, Daniel Schlaepfer, William Lauenroth, John Bradford}
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


# Author: Ryan J. Murphy (2013); Daniel R Schlaepfer (2013-2018)
###############################################################################

#' Class \code{"swInputData"}
#'
#' This class is a container class to the input file S4 objects. The generic functions
#' listed work on this and the proper sw classes in the container's slots.
#'
#' @param object An object of class \code{\linkS4class{swInputData}}.
#' @param .Object An object of class \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Further arguments to methods.
#' @param year An integer value. The calendar year of the weather or SWC \code{data} object.
#' @param vegtype The name or index of the vegetation type.
#'
#' @seealso \code{\linkS4class{swFiles}} \code{\linkS4class{swYears}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}} \code{\linkS4class{swCloud_daily}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#' \code{\linkS4class{swCarbon}}
#'
#' @examples
#' showClass("swInputData")
#' x <- new("swInputData")
#'
#' @name swInputData-class
#' @export
setClass("swInputData", slots = c(files = "swFiles", years = "swYears",
  weather = "swWeather", cloud = "swCloud", cloud_daily = "swCloud_daily", weatherHistory = "list", markov = "swMarkov",
  prod = "swProd", site = "swSite", soils = "swSoils", estab = "swEstab",
  carbon = "swCarbon", output = "swOUT", swc = "swSWC", log = "swLog"))


#' @rdname swInputData-class
#' @export
setMethod("initialize", signature = "swInputData", function(.Object) {
  sns <- slotNames("swInputData")
  scl <- getSlots("swInputData")

  for (i in seq_along(sns)) {
    slot(.Object, sns[i]) <- new(scl[i])
  }
  validObject(.Object)

  .Object
})

setValidity("swInputData", function(object) {
	TRUE
})


# Methods for slot 'files'
#' @rdname swInputData-class
#' @export
setMethod("get_swFiles", "swInputData", function(object) object@files)
#' @rdname swInputData-class
#' @export
setMethod("swFiles_ProjDir", "swInputData", function(object) swFiles_ProjDir(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_filesIn", "swInputData", function(object) swFiles_filesIn(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_Years", "swInputData", function(object) swFiles_Years(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_LogFile", "swInputData", function(object) swFiles_LogFile(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_SiteParams", "swInputData", function(object) swFiles_SiteParams(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_Soils", "swInputData", function(object) swFiles_Soils(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_WeatherSetup", "swInputData", function(object) swFiles_WeatherSetup(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_MarkovProbs", "swInputData", function(object) swFiles_MarkovProbs(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_MarkovCov", "swInputData", function(object) swFiles_MarkovCov(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_Cloud", "swInputData", function(object) swFiles_Cloud(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_Prod", "swInputData", function(object) swFiles_Prod(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_Estab", "swInputData", function(object) swFiles_Estab(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_Carbon", "swInputData", function(object) swFiles_Carbon(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_SWCsetup", "swInputData", function(object) swFiles_SWCsetup(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_Output", "swInputData", function(object) swFiles_Output(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_WeatherPrefix", "swInputData", function(object) swFiles_WeatherPrefix(object@files))
#' @rdname swInputData-class
#' @export
setMethod("swFiles_OutputPrefix", "swInputData", function(object) swFiles_OutputPrefix(object@files))

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swFiles", signature = "swInputData", function(object, value) {
  object@files <- value
  validObject(object@files)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_ProjDir", signature = "swInputData", function(object, value) {
  swFiles_ProjDir(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_filesIn", signature = "swInputData", function(object, value) {
  swFiles_filesIn(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_Years", signature = "swInputData", function(object, value) {
  swFiles_Years(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_LogFile", signature = "swInputData", function(object, value) {
  swFiles_LogFile(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_SiteParams", signature = "swInputData", function(object, value) {
  swFiles_SiteParams(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_Soils", signature = "swInputData", function(object, value) {
  swFiles_Soils(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_WeatherSetup", signature = "swInputData", function(object, value) {
  swFiles_WeatherSetup(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_MarkovProbs", signature = "swInputData", function(object, value) {
  swFiles_MarkovProbs(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_MarkovCov", signature = "swInputData", function(object, value) {
  swFiles_MarkovCov(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_Cloud", signature = "swInputData", function(object, value) {
  swFiles_Cloud(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_Prod", signature = "swInputData", function(object, value) {
  swFiles_Prod(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_Estab", signature = "swInputData", function(object, value) {
  swFiles_Estab(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_Carbon", signature = "swInputData", function(object, value) {
  swFiles_Carbon(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_SWCsetup", signature = "swInputData", function(object, value) {
  swFiles_SWCsetup(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_Output", signature = "swInputData", function(object, value) {
  swFiles_Output(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_WeatherPrefix", signature = "swInputData", function(object, value) {
  swFiles_WeatherPrefix(object@files) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swFiles_OutputPrefix", signature = "swInputData", function(object, value) {
  swFiles_OutputPrefix(object@files) <- value
  object
})


# Methods for slot 'years'
#' @rdname swInputData-class
#' @export
setMethod("get_swYears", "swInputData", function(object) object@years)
#' @rdname swInputData-class
#' @export
setMethod("swYears_StartYear", "swInputData", function(object) swYears_StartYear(object@years))
#' @rdname swInputData-class
#' @export
setMethod("swYears_EndYear", "swInputData", function(object) swYears_EndYear(object@years))
#' @rdname swInputData-class
#' @export
setMethod("swYears_FDOFY", "swInputData", function(object) swYears_FDOFY(object@years))
#' @rdname swInputData-class
#' @export
setMethod("swYears_EDOEY", "swInputData", function(object) swYears_EDOEY(object@years))
#' @rdname swInputData-class
#' @export
setMethod("swYears_isNorth", "swInputData", function(object) swYears_isNorth(object@years))

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swYears", signature = "swInputData", function(object, value) {
  object@years <- value
  validObject(object@years)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swYears_StartYear", signature = "swInputData", function(object, value) {
  swYears_StartYear(object@years) <- as.integer(value)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swYears_EndYear", signature = "swInputData", function(object, value) {
  swYears_EndYear(object@years) <- as.integer(value)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swYears_FDOFY", signature = "swInputData", function(object, value) {
  swYears_FDOFY(object@years) <- as.integer(value)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swYears_EDOEY", signature = "swInputData", function(object, value) {
  swYears_EDOEY(object@years) <- as.integer(value)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swYears_isNorth", signature = "swInputData", function(object, value) {
  swYears_isNorth(object@years) <- as.logical(value)
  object
})


# Methods for slot 'cloud'
#' @rdname swInputData-class
#' @export
setMethod("get_swCloud", "swInputData", function(object) object@cloud)

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swCloud", signature = "swInputData", function(object, value) {
  set_swCloud(object@cloud) <- value
  object
})

# Methods for slot 'cloud_daily'
#' @rdname swInputData-class
#' @export
setMethod("get_swCloud_daily", "swInputData", function(object) object@cloud_daily)

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swCloud_daily", signature = "swInputData", function(object, value) {
  set_swCloud(object@cloud_daily) <- value
  object
})


# Methods for slot 'weather'
#' @rdname swInputData-class
#' @export
setMethod("get_swWeather", "swInputData", function(object) object@weather)
#' @rdname swInputData-class
#' @export
setMethod("swWeather_DaysRunningAverage", "swInputData", function(object) swWeather_DaysRunningAverage(object@weather))
#' @rdname swInputData-class
#' @export
setMethod("swWeather_FirstYearHistorical", "swInputData", function(object) swWeather_FirstYearHistorical(object@weather))
#' @rdname swInputData-class
#' @export
setMethod("swWeather_pct_SnowDrift", "swInputData", function(object) swWeather_DaysRunningAverage(object@weather))
#' @rdname swInputData-class
#' @export
setMethod("swWeather_pct_SnowRunoff", "swInputData", function(object) swWeather_pct_SnowDrift(object@weather))
#' @rdname swInputData-class
#' @export
setMethod("swWeather_UseMarkov", "swInputData", function(object) swWeather_UseMarkov(object@weather))
#' @rdname swInputData-class
#' @export
setMethod("swWeather_UseSnow", "swInputData", function(object) swWeather_UseSnow(object@weather))
#' @rdname swInputData-class
#' @export
setMethod("swWeather_MonScalingParams", "swInputData", function(object) swWeather_MonScalingParams(object@weather))

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swWeather", signature = "swInputData", function(object, value) {
  slot(object, "weather") <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swWeather_DaysRunningAverage", signature = "swInputData", function(object, value) {
  swWeather_DaysRunningAverage(object@weather) <- as.integer(value)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swWeather_FirstYearHistorical", signature = "swInputData", function(object, value) {
  swWeather_FirstYearHistorical(object@weather) <- as.integer(value)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swWeather_pct_SnowDrift", signature = "swInputData", function(object, value) {
  swWeather_pct_SnowDrift(object@weather) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swWeather_pct_SnowRunoff", signature = "swInputData", function(object, value) {
  swWeather_pct_SnowRunoff(object@weather) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swWeather_UseMarkov", signature = "swInputData", function(object, value) {
  swWeather_UseMarkov(object@weather) <- as.logical(value)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swWeather_UseSnow", signature = "swInputData", function(object, value) {
  swWeather_UseSnow(object@weather) <- as.logical(value)
  object
})

#' @rdname swInputData-class
#' @export
setReplaceMethod("swWeather_MonScalingParams", signature = "swInputData", function(object, value) {
  swWeather_MonScalingParams(object@weather) <- value
  object
})



# Methods for slot 'cloud'
#' @rdname swInputData-class
#' @export
setMethod("get_swCloud", "swInputData", function(object) object@cloud)
#' @rdname swInputData-class
#' @export
setMethod("swCloud_SkyCover", "swInputData", function(object) swCloud_SkyCover(object@cloud))
#' @rdname swInputData-class
#' @export
setMethod("swCloud_WindSpeed", "swInputData", function(object) swCloud_WindSpeed(object@cloud))
#' @rdname swInputData-class
#' @export
setMethod("swCloud_Humidity", "swInputData", function(object) swCloud_Humidity(object@cloud))
#' @rdname swInputData-class
#' @export
setMethod("swCloud_Transmissivity", "swInputData", function(object) swCloud_Transmissivity(object@cloud))
#' @rdname swInputData-class
#' @export
setMethod("swCloud_SnowDensity", "swInputData", function(object) swCloud_SnowDensity(object@cloud))

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swCloud", signature = "swInputData", function(object, value) {
  set_swCloud(object@cloud) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swCloud_SkyCover", signature = "swInputData", function(object, value) {
  swCloud_SkyCover(object@cloud) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swCloud_WindSpeed", signature = "swInputData", function(object, value) {
  swCloud_WindSpeed(object@cloud) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swCloud_Humidity", signature = "swInputData", function(object, value) {
  swCloud_Humidity(object@cloud) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swCloud_Transmissivity", signature = "swInputData", function(object, value) {
  swCloud_Transmissivity(object@cloud) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swCloud_SnowDensity", signature = "swInputData", function(object, value) {
  swCloud_SnowDensity(object@cloud) <- value
  object
})

# Methods for slot 'cloud_daily'
#' @rdname swInputData-class
#' @export
setMethod("get_swCloud_daily", "swInputData", function(object) object@cloud_daily)
#' @rdname swInputData-class
#' @export
setMethod("swCloud_daily_Humidity", "swInputData", function(object) swCloud_daily_Humidity(object@cloud_daily))
#' @rdname swInputData-class
#' @export
setReplaceMethod("swCloud_daily_Humidity", signature = "swInputData", function(object, value) {
  swCloud_daily_Humidity(object@cloud_daily) <- value
  object
})


# Methods for slot 'markov'
#' @rdname swInputData-class
#' @export
setMethod("get_Markov", "swInputData", function(object) object@markov)
#' @rdname swInputData-class
#' @export
setMethod("swMarkov_Prob", "swInputData", function(object) swMarkov_Prob(object@markov))
#' @rdname swInputData-class
#' @export
setMethod("swMarkov_Conv", "swInputData", function(object) swMarkov_Conv(object@markov))

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_Markov", signature = "swInputData", function(object, value) {
  set_Markov(object@markov) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swMarkov_Prob", signature = "swInputData", function(object, value) {
  swMarkov_Prob(object@markov) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swMarkov_Conv", signature = "swInputData", function(object, value) {
  swMarkov_Conv(object@markov) <- value
  object
})


# Methods for slot 'weatherHistory'
#' @rdname swInputData-class
#' @export
setMethod("get_WeatherHistory", "swInputData", function(object) object@weatherHistory)
#' @rdname swInputData-class
#' @export
setReplaceMethod("set_WeatherHistory", signature = c(object = "swInputData", value = "list"),
  function(object, value) {
    object@weatherHistory <- value
    object
})

#' @rdname swInputData-class
#' @export
setMethod("get_swWeatherData", "swInputData", function(object, year) {
			index<-which(names(object@weatherHistory) == as.character(year))
			if(length(index) != 1) {
				stop("Index has wrong length.")
			}
			if(object@weatherHistory[[index]]@year != as.integer(year))
				print("Somethings wrong with the weather data.")
			return(object@weatherHistory[[index]])
		})
#' @rdname swInputData-class
#' @export
setMethod("get_swWeatherData", "list", function(object,year) {
			index<-which(names(object) == as.character(year))
			if(length(index) != 1) {
				stop("Index has wrong length.")
			}
			if(object[[index]]@year != as.integer(year))
				print("Somethings wrong with the weather data.")
			return(object[[index]])
		})

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swWeatherData", signature = c(object="swInputData",value="swWeatherData"), function(object, value) {
			index<-which(names(object@weatherHistory) == as.character(value@year))
			if(length(index) == 0) {
				object@weatherHistory[[length(object@weatherHistory)+1]] <- value
				years <- unlist(lapply(object@weatherHistory, function(x) {return(x@year)}))
				object@weatherHistory <- object@weatherHistory[order(years)]
				names(object@weatherHistory)<- as.character(years[order(years)])
				if(!all(years[order(years)] == cummax(years[order(years)])))
					print("Weather data is Missing")
			} else if(length(index) == 1){
				object@weatherHistory[[index]] <- value
			} else {
				print("To many index. Not set")
			}
			return(object)
		})
#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swWeatherData", signature = c(object="list",value="swWeatherData"), function(object, value) {
			index<-which(names(object) == as.character(value@year))
			if(length(index) == 0) {
				object[[length(object)+1]] <- value
				years <- unlist(lapply(object, function(x) {return(x@year)}))
				object <- object[order(years)]
				names(object)<- as.character(years[order(years)])
				if(!all(years[order(years)] == cummax(years[order(years)])))
					print("Weather data is Missing")
			} else if(length(index) == 1){
				object[[index]] <- value
			} else {
				print("To many index. Not set")
			}
			return(object)
		})


# Methods for slot 'prod'
#' @rdname swInputData-class
#' @export
setMethod("get_swProd", "swInputData", function(object) object@prod)
#' @rdname swInputData-class
#' @export
setMethod("swProd_Composition", "swInputData", function(object) swProd_Composition(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_Albedo", "swInputData", function(object) swProd_Albedo(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_Cover_stcr", "swInputData", function(object) swProd_Cover_stcr(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_CanopyHeight", "swInputData", function(object) swProd_CanopyHeight(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_VegInterParam", "swInputData", function(object) swProd_VegInterParam(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_LitterInterParam", "swInputData", function(object) swProd_LitterInterParam(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_EsTpartitioning_param", "swInputData", function(object) swProd_EsTpartitioning_param(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_Es_param_limit", "swInputData", function(object) swProd_Es_param_limit(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_Shade", "swInputData", function(object) swProd_Shade(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_HydrRedstro_use", "swInputData", function(object) swProd_HydrRedstro_use(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_HydrRedstro", "swInputData", function(object) swProd_HydrRedstro(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_CritSoilWaterPotential", "swInputData", function(object) swProd_CritSoilWaterPotential(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_CO2Coefficients", "swInputData", function(object) swProd_CO2Coefficients(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_MonProd_veg", "swInputData", function(object, vegtype) swProd_MonProd_veg(object@prod, vegtype))
#' @rdname swInputData-class
#' @export
setMethod("swProd_MonProd_grass", "swInputData", function(object) swProd_MonProd_grass(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_MonProd_shrub", "swInputData", function(object) swProd_MonProd_shrub(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_MonProd_tree", "swInputData", function(object) swProd_MonProd_tree(object@prod))
#' @rdname swInputData-class
#' @export
setMethod("swProd_MonProd_forb", "swInputData", function(object) swProd_MonProd_forb(object@prod))

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swProd", signature = "swInputData", function(object, value) {
  set_swProd(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_Composition", signature = "swInputData", function(object, value) {
  swProd_Composition(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_Albedo", signature = "swInputData", function(object, value) {
  swProd_Albedo(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_Cover_stcr", signature = "swInputData", function(object, value) {
  swProd_Cover_stcr(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_CanopyHeight", signature = "swInputData", function(object, value) {
  swProd_CanopyHeight(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_VegInterParam", signature = "swInputData", function(object, value) {
  swProd_VegInterParam(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_LitterInterParam", signature = "swInputData", function(object, value) {
  swProd_LitterInterParam(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_EsTpartitioning_param", signature = "swInputData", function(object, value) {
  swProd_EsTpartitioning_param(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_Es_param_limit", signature = "swInputData", function(object, value) {
  swProd_Es_param_limit(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_Shade", signature = "swInputData", function(object, value) {
  swProd_Shade(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_HydrRedstro_use", signature = "swInputData", function(object, value) {
  swProd_HydrRedstro_use(object@prod) <- as.logical(value)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_HydrRedstro", signature = "swInputData", function(object, value) {
  swProd_HydrRedstro(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_CritSoilWaterPotential", signature = "swInputData", function(object, value) {
  swProd_CritSoilWaterPotential(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_CO2Coefficients", signature = "swInputData", function(object, value) {
  swProd_CO2Coefficients(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_MonProd_veg", signature = "swInputData",
  function(object, vegtype, value) {
    swProd_MonProd_veg(object@prod, vegtype) <- value
    object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_MonProd_grass", signature = "swInputData", function(object, value) {
  swProd_MonProd_grass(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_MonProd_shrub", signature = "swInputData", function(object, value) {
  swProd_MonProd_shrub(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_MonProd_tree", signature = "swInputData", function(object, value) {
  swProd_MonProd_tree(object@prod) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swProd_MonProd_forb", signature = "swInputData", function(object, value) {
  swProd_MonProd_forb(object@prod) <- value
  object
})


# Methods for slot 'site'
#' @rdname swInputData-class
#' @export
setMethod("get_swSite", "swInputData", function(object) slot(object, "site"))
#' @rdname swInputData-class
#' @export
setMethod("swSite_SWClimits", "swInputData", function(object) swSite_SWClimits(object@site))
#' @rdname swInputData-class
#' @export
setMethod("swSite_ModelFlags", "swInputData", function(object) swSite_ModelFlags(object@site))
#' @rdname swInputData-class
#' @export
setMethod("swSite_ModelCoefficients", "swInputData", function(object) swSite_ModelCoefficients(object@site))
#' @rdname swInputData-class
#' @export
setMethod("swSite_SnowSimulationParams", "swInputData", function(object) swSite_SnowSimulationParams(object@site))
#' @rdname swInputData-class
#' @export
setMethod("swSite_DrainageCoefficient", "swInputData", function(object) swSite_DrainageCoefficient(object@site))
#' @rdname swInputData-class
#' @export
setMethod("swSite_EvapCoefficients", "swInputData", function(object) swSite_EvapCoefficients(object@site))
#' @rdname swInputData-class
#' @export
setMethod("swSite_TranspCoefficients", "swInputData", function(object) swSite_TranspCoefficients(object@site))
#' @rdname swInputData-class
#' @export
setMethod("swSite_IntrinsicSiteParams", "swInputData", function(object) swSite_IntrinsicSiteParams(object@site))
#' @rdname swInputData-class
#' @export
setMethod("swSite_SoilTemperatureFlag", "swInputData", function(object) swSite_SoilTemperatureFlag(object@site))
#' @rdname swInputData-class
#' @export
setMethod("swSite_SoilTemperatureConsts", "swInputData", function(object) swSite_SoilTemperatureConsts(object@site))
#' @rdname swInputData-class
#' @export
setMethod("swSite_TranspirationRegions", "swInputData", function(object) swSite_TranspirationRegions(object@site))

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swSite", signature = "swInputData", function(object, value) {
  set_swSite(object@site) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSite_SWClimits", signature = "swInputData", function(object, value) {
  swSite_SWClimits(object@site) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSite_ModelFlags", signature = "swInputData", function(object, value) {
  swSite_ModelFlags(object@site) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSite_ModelCoefficients", signature = "swInputData", function(object, value) {
  swSite_ModelCoefficients(object@site) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSite_SnowSimulationParams", signature = "swInputData", function(object, value) {
  swSite_SnowSimulationParams(object@site) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSite_DrainageCoefficient", signature = "swInputData", function(object, value) {
  swSite_DrainageCoefficient(object@site) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSite_EvapCoefficients", signature = "swInputData", function(object, value) {
  swSite_EvapCoefficients(object@site) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSite_TranspCoefficients", signature = "swInputData", function(object, value) {
  swSite_TranspCoefficients(object@site) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSite_IntrinsicSiteParams", signature = "swInputData", function(object, value) {
  swSite_IntrinsicSiteParams(object@site) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSite_SoilTemperatureFlag", signature = "swInputData", function(object, value) {
  swSite_SoilTemperatureFlag(object@site) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSite_SoilTemperatureConsts", signature = "swInputData", function(object, value) {
  swSite_SoilTemperatureConsts(slot(object, "site")) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSite_TranspirationRegions", signature = "swInputData", function(object, value) {
  swSite_TranspirationRegions(object@site) <- value*1
  object
})


# Methods for slot 'soils'
#' @rdname swInputData-class
#' @export
setMethod("get_swSoils", "swInputData", function(object) object@soils)
#' @rdname swInputData-class
#' @export
setMethod("swSoils_Layers", "swInputData", function(object) swSoils_Layers(object@soils))

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swSoils", signature = c(object = "swInputData", value = "swSoils"),
  function(object, value) {
    set_swSoils(object@soils) <- value
    object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSoils_Layers", signature = c(object = "swInputData", value = "matrix"),
  function(object, value) {
    swSoils_Layers(object@soils) <- value
    object
})


# Methods for slot 'swc'
#' @rdname swInputData-class
#' @export
setMethod("get_swSWC", "swInputData", function(object) object@swc)
#' @rdname swInputData-class
#' @export
setMethod("swSWC_use", "swInputData", function(object) swSWC_use(object@swc))
#' @rdname swInputData-class
#' @export
setMethod("swSWC_prefix", "swInputData", function(object) swSWC_prefix(object@swc))
#' @rdname swInputData-class
#' @export
setMethod("swSWC_FirstYear", "swInputData", function(object) swSWC_FirstYear(object@swc))
#' @rdname swInputData-class
#' @export
setMethod("swSWC_Method", "swInputData", function(object) swSWC_Method(object@swc))
#' @rdname swInputData-class
#' @export
setMethod("swSWC_HistoricList", "swInputData", function(object) swSWC_HistoricList(object@swc))
#' @rdname swInputData-class
#' @export
setMethod("swSWC_HistoricData", "swInputData", function(object, year) swSWC_HistoricData(object@swc, year))

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swSWC", signature = c(object = "swInputData", value = "swSWC"),
  function(object, value) {
    object@swc <- value
    object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSWC_use", signature = c(object = "swInputData", value = "logical"),
  function(object, value) {
    swSWC_use(object@swc) <- value
    object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSWC_prefix", signature = c(object = "swInputData", value = "character"),
  function(object, value) {
    swSWC_prefix(object@swc) <- value
    object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSWC_FirstYear", signature = c(object = "swInputData", value = "integer"),
  function(object, value) {
    swSWC_FirstYear(object@swc) <- value
    object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSWC_Method", signature = c(object = "swInputData", value = "integer"),
  function(object, value) {
    swSWC_Method(object@swc) <- value
    object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSWC_HistoricList", signature = c(object = "swInputData", value = "list"),
  function(object, value) {
    swSWC_HistoricList(object@swc) <- value
    object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swSWC_HistoricData", signature = c(object = "swInputData", value = "swSWC_hist"),
  function(object, value) {
			index<-which(names(object@swc@History) == as.character(value@year))
			if(length(index) == 0) {
				object@swc@History[[length(object@swc@History)+1]] <- value
				years <- unlist(lapply(object@swc@History, function(x) {return(x@year)}))
				object@swc@History <- object@swc@History[order(years)]
				names(object@swc@History)<- as.character(years[order(years)])
				if(!all(years[order(years)] == cummax(years[order(years)])))
					print("SWC data is Missing")
			} else if(length(index) == 1) {
				object@swc@History[[index]] <- value
			} else {
				print("To many index. Not set")
			}
			return(object)
		})


# Methods for slot 'carbon'
#' @rdname swInputData-class
#' @export
setMethod("get_swCarbon", "swInputData", function(object) object@carbon)
#' @rdname swInputData-class
#' @export
setMethod("swCarbon_Use_Bio", "swInputData", function(object) swCarbon_Use_Bio(object@carbon))
#' @rdname swInputData-class
#' @export
setMethod("swCarbon_Use_WUE", "swInputData", function(object) swCarbon_Use_WUE(object@carbon))
#' @rdname swInputData-class
#' @export
setMethod("swCarbon_Scenario", "swInputData", function(object) swCarbon_Scenario(object@carbon))
#' @rdname swInputData-class
#' @export
setMethod("swCarbon_DeltaYear", "swInputData", function(object) swCarbon_DeltaYear(object@carbon))
#' @rdname swInputData-class
#' @export
setMethod("swCarbon_CO2ppm", "swInputData", function(object) swCarbon_CO2ppm(object@carbon))

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swCarbon", signature = "swInputData", function(object, value) {
  set_swCarbon(object@carbon) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swCarbon_Use_Bio", signature = "swInputData", function(object, value) {
  swCarbon_Use_Bio(object@carbon) <- as.integer(value)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swCarbon_Use_WUE", signature = "swInputData", function(object, value) {
  swCarbon_Use_WUE(object@carbon) <- as.integer(value)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swCarbon_Scenario", signature = "swInputData", function(object, value) {
  swCarbon_Scenario(object@carbon) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swCarbon_DeltaYear", signature = "swInputData", function(object, value) {
  swCarbon_DeltaYear(object@carbon) <- as.integer(value)
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swCarbon_CO2ppm", signature = "swInputData", function(object, value) {
  swCarbon_CO2ppm(object@carbon) <- value
  object
})
##


# Methods for slot 'output'
#' @rdname swInputData-class
#' @export
setMethod("get_swOUT", "swInputData", function(object) object@output)
#' @rdname swInputData-class
#' @export
setMethod("swOUT_TimeStep", "swInputData", function(object) swOUT_TimeStep(object@output))
#' @rdname swInputData-class
#' @export
setMethod("swOUT_OutputSeparator", "swInputData", function(object) swOUT_OutputSeparator(object@output))

#' @rdname swInputData-class
#' @export
setReplaceMethod("set_swOUT", signature = "swInputData", function(object, value) {
  set_swOUT(object@output) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swOUT_TimeStep", signature = "swInputData", function(object, value) {
  swOUT_TimeStep(object@output) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swOUT_TimeStepsForEveryKey", signature = "swInputData", function(object, value) {
  swOUT_TimeStepsForEveryKey(object@output) <- value
  object
})
#' @rdname swInputData-class
#' @export
setReplaceMethod("swOUT_OutputSeparator", signature = "swInputData", function(object, value) {
  swOUT_OutputSeparator(object@output) <- value
  object
})


# Methods for slot 'log'
#' @rdname swInputData-class
#' @export
setReplaceMethod("swLog_setLine", "swInputData", function(object, value) {
			if(object@log@UsedLines <= object@log@MaxLines) {
				object@log@LogData[object@log@UsedLines] <- value
				object@log@UsedLines <- object@log@UsedLines + 1
			}
			return(object)
		})
##


#' @rdname swInputData-class
#' @export
setMethod(f="swReadLines", signature = c(object="swInputData",file="character"), function(object,file) {
  print("TODO: method 'swReadLines' for class 'swInputData' is not up-to-date; hard-coded indices are incorrect")

			object@files <- swReadLines(object@files,file)
			object@files@ProjDir <- dirname(file)
			object@years <- swReadLines(object@years,file.path(object@files@ProjDir, object@files@InFiles[2]))
			object@weather <- swReadLines(object@weather,file.path(object@files@ProjDir, object@files@InFiles[6]))
			weatherFiles <- list.files(path=file.path(object@files@ProjDir,dirname(object@files@WeatherPrefix)), pattern=basename(object@files@WeatherPrefix), include.dirs=F, recursive=F, full.names=T)
			object@weatherHistory <- list()
			if(length(weatherFiles) > 0) {
				for(i in 1:length(weatherFiles)) {
					wd <- new("swWeatherData",year=0)
					wd <- swReadLines(wd, weatherFiles[i])
					object@weatherHistory[[i]] <- wd
				}
			}

			object@cloud <- swReadLines(object@cloud,file.path(object@files@ProjDir, object@files@InFiles[9]))
			if(all(file.exists(file.path(object@files@ProjDir, object@files@InFiles[7:8]))))
				object@markov <- swReadLines(object@markov,file.path(object@files@ProjDir, object@files@InFiles[7:8]))
			object@prod <- swReadLines(object@prod,file.path(object@files@ProjDir, object@files@InFiles[10]))
			object@site <- swReadLines(object@site,file.path(object@files@ProjDir, object@files@InFiles[4]))
			object@soils <- swReadLines(object@soils,file.path(object@files@ProjDir, object@files@InFiles[5]))
			if(file.exists(file.path(object@files@ProjDir, object@files@InFiles[11]))) {#Optional File
				object@estab <- swReadLines(object@estab,c(file.path(object@files@ProjDir, object@files@InFiles[11]),object@files@ProjDir))
			}
			object@output <- swReadLines(object@output,file.path(object@files@ProjDir, object@files@InFiles[14]))
			object@swc <- swReadLines(object@swc,file.path(object@files@ProjDir, object@files@InFiles[13]))
			object@carbon <- swReadLines(object@carbon, file.path(object@files@ProjDir, object@files@InFiles[12]))
			return(object)
		})
