###############################################################################
#rSOILWAT2
#    Copyright (C) 2009-2018  Ryan Murphy, Daniel Schlaepfer, William Lauenroth, John Bradford
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


# TODO: Add comment
#
# Author: Ryan J. Murphy (2013)
###############################################################################

##########################GENERIC FUNCTIONS############################################
#' swReadLines
#' @param object An object of a class such \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swInputData}}
setGeneric("swReadLines", function(object, file) standardGeneric("swReadLines"))

#########FILES##########
#' get_swFiles
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("get_swFiles", function(object) standardGeneric("get_swFiles"))
#' swFiles_ProjDir
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_ProjDir", function(object) standardGeneric("swFiles_ProjDir"))
#' swFiles_filesIn
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_filesIn", function(object) standardGeneric("swFiles_filesIn"))
#' swFiles_Years
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Years", function(object) standardGeneric("swFiles_Years"))
#' swFiles_LogFile
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_LogFile", function(object) standardGeneric("swFiles_LogFile"))
#' swFiles_SiteParams
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_SiteParams", function(object) standardGeneric("swFiles_SiteParams"))
#' swFiles_Soils
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Soils", function(object) standardGeneric("swFiles_Soils"))
#' swFiles_WeatherSetup
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_WeatherSetup", function(object) standardGeneric("swFiles_WeatherSetup"))
#' swFiles_MarkovProbs
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_MarkovProbs", function(object) standardGeneric("swFiles_MarkovProbs"))
#' swFiles_MarkovCov
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_MarkovCov", function(object) standardGeneric("swFiles_MarkovCov"))
#' swFiles_Cloud
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Cloud", function(object) standardGeneric("swFiles_Cloud"))
#' swFiles_Prod
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Prod", function(object) standardGeneric("swFiles_Prod"))
#' swFiles_Estab
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Estab", function(object) standardGeneric("swFiles_Estab"))
#' swFiles_Carbon
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Carbon", function(object) standardGeneric("swFiles_Carbon"))
#' swFiles_SWCsetup
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_SWCsetup", function(object) standardGeneric("swFiles_SWCsetup"))
#' swFiles_Output
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Output", function(object) standardGeneric("swFiles_Output"))
#' swFiles_WeatherPrefix
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_WeatherPrefix", function(object) standardGeneric("swFiles_WeatherPrefix"))
#' swFiles_OutputPrefix
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_OutputPrefix", function(object) standardGeneric("swFiles_OutputPrefix"))

#' set_swFiles
#' Need to define and export this generic method -- otherwise, \code{\link{set_swFiles<-}} doesn't work.
#' @export
setGeneric("set_swFiles", function(object, value) standardGeneric("set_swFiles"))
#' set_swFiles<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("set_swFiles<-", function(object, value) standardGeneric("set_swFiles<-"))
#' swFiles_ProjDir<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_ProjDir<-", function(object, value) standardGeneric("swFiles_ProjDir<-"))
#' swFiles_filesIn<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_filesIn<-", function(object, value) standardGeneric("swFiles_filesIn<-"))
#' swFiles_Years<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Years<-", function(object, value) standardGeneric("swFiles_Years<-"))
#' swFiles_LogFile<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_LogFile<-", function(object, value) standardGeneric("swFiles_LogFile<-"))
#' swFiles_SiteParams<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_SiteParams<-", function(object, value) standardGeneric("swFiles_SiteParams<-"))
#' swFiles_Soils<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Soils<-", function(object, value) standardGeneric("swFiles_Soils<-"))
#' swFiles_WeatherSetup<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_WeatherSetup<-", function(object, value) standardGeneric("swFiles_WeatherSetup<-"))
#' swFiles_MarkovProbs<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_MarkovProbs<-", function(object, value) standardGeneric("swFiles_MarkovProbs<-"))
#' swFiles_MarkovCov<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_MarkovCov<-", function(object, value) standardGeneric("swFiles_MarkovCov<-"))
#' swFiles_Cloud<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Cloud<-", function(object, value) standardGeneric("swFiles_Cloud<-"))
#' swFiles_Prod<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Prod<-", function(object, value) standardGeneric("swFiles_Prod<-"))
#' swFiles_Estab<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Estab<-", function(object, value) standardGeneric("swFiles_Estab<-"))
#' swFiles_Carbon<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Carbon<-", function(object, value) standardGeneric("swFiles_Carbon<-"))
#' swFiles_SWCsetup<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_SWCsetup<-", function(object, value) standardGeneric("swFiles_SWCsetup<-"))
#' swFiles_Output<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_Output<-", function(object, value) standardGeneric("swFiles_Output<-"))
#' swFiles_WeatherPrefix<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_WeatherPrefix<-", function(object, value) standardGeneric("swFiles_WeatherPrefix<-"))
#' swFiles_OutputPrefix<-
#' @param object An object of class \code{\linkS4class{swFiles}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swFiles}} and \code{\linkS4class{swInputData}}
setGeneric("swFiles_OutputPrefix<-", function(object, value) standardGeneric("swFiles_OutputPrefix<-"))
########################

########YEARS###########
#' get_swYears
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("get_swYears", function(object) standardGeneric("get_swYears"))
#' swYears_StartYear
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("swYears_StartYear", function(object) standardGeneric("swYears_StartYear"))
#' swYears_EndYear
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("swYears_EndYear", function(object) standardGeneric("swYears_EndYear"))
#' swYears_FDOFY
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("swYears_FDOFY", function(object) standardGeneric("swYears_FDOFY"))
#' swYears_EDOEY
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("swYears_EDOEY", function(object) standardGeneric("swYears_EDOEY"))
#' swYears_isNorth
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("swYears_isNorth", function(object) standardGeneric("swYears_isNorth"))

#' set_swYears
#' Need to define and export this generic method -- otherwise, \code{\link{set_swYears<-}} doesn't work.
#' @export
setGeneric("set_swYears", function(object, value) standardGeneric("set_swYears"))
#' set_swYears<-
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("set_swYears<-", function(object, value) standardGeneric("set_swYears<-"))
#' swYears_StartYear<-
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("swYears_StartYear<-", function(object, value) standardGeneric("swYears_StartYear<-"))
#' swYears_EndYear<-
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("swYears_EndYear<-", function(object, value) standardGeneric("swYears_EndYear<-"))
#' swYears_FDOFY<-
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("swYears_FDOFY<-", function(object, value) standardGeneric("swYears_FDOFY<-"))
#' swYears_EDOEY<-
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("swYears_EDOEY<-", function(object, value) standardGeneric("swYears_EDOEY<-"))
#' swYears_isNorth<-
#' @param object An object of class \code{\linkS4class{swYears}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swYears}} and \code{\linkS4class{swInputData}}
setGeneric("swYears_isNorth<-", function(object, value) standardGeneric("swYears_isNorth<-"))
########################

########WEATHER#########
#' get_swWeather
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("get_swWeather", function(object) standardGeneric("get_swWeather"))
#' swWeather_DaysRunningAverage
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_DaysRunningAverage", function(object) standardGeneric("swWeather_DaysRunningAverage"))
#' swWeather_FirstYearHistorical
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_FirstYearHistorical", function(object) standardGeneric("swWeather_FirstYearHistorical"))
#' swWeather_pct_SnowDrift
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_pct_SnowDrift", function(object) standardGeneric("swWeather_pct_SnowDrift"))
#' swWeather_pct_SnowRunoff
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_pct_SnowRunoff", function(object) standardGeneric("swWeather_pct_SnowRunoff"))
#' swWeather_UseMarkov
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_UseMarkov", function(object) standardGeneric("swWeather_UseMarkov"))
#' swWeather_UseSnow
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_UseSnow", function(object) standardGeneric("swWeather_UseSnow"))
#' swWeather_MonScalingParams
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_MonScalingParams", function(object) standardGeneric("swWeather_MonScalingParams"))

#' set_swWeather
#' Need to define and export this generic method -- otherwise, \code{\link{set_swWeather<-}} doesn't work.
#' @export
setGeneric("set_swWeather", function(object, value) standardGeneric("set_swWeather"))
#' set_swWeather<-
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("set_swWeather<-", signature = "object", function(object, value)
  standardGeneric("set_swWeather<-"))
#' swWeather_DaysRunningAverage<-
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_DaysRunningAverage<-", function(object, value) standardGeneric("swWeather_DaysRunningAverage<-"))
#' swWeather_FirstYearHistorical<-
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_FirstYearHistorical<-", function(object, value) standardGeneric("swWeather_FirstYearHistorical<-"))
#' swWeather_pct_SnowDrift<-
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_pct_SnowDrift<-", function(object, value) standardGeneric("swWeather_pct_SnowDrift<-"))
#' swWeather_pct_SnowRunoff<-
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_pct_SnowRunoff<-", function(object, value) standardGeneric("swWeather_pct_SnowRunoff<-"))
#' swWeather_UseMarkov<-
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_UseMarkov<-", function(object, value) standardGeneric("swWeather_UseMarkov<-"))
#' swWeather_UseSnow<-
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_UseSnow<-", function(object, value) standardGeneric("swWeather_UseSnow<-"))
#' swWeather_MonScalingParams<-
#' @param object An object of class \code{\linkS4class{swWeather}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeather}} and \code{\linkS4class{swInputData}}
setGeneric("swWeather_MonScalingParams<-", function(object, value) standardGeneric("swWeather_MonScalingParams<-"))
########################

########MARKOV##########
#' get_Markov
#' @param object An object of class \code{\linkS4class{swMarkov}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric("get_Markov", function(object) standardGeneric("get_Markov"))
#' swMarkov_Prob
#' @param object An object of class \code{\linkS4class{swMarkov}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric("swMarkov_Prob", function(object) standardGeneric("swMarkov_Prob"))
#' swMarkov_Conv
#' @param object An object of class \code{\linkS4class{swMarkov}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric("swMarkov_Conv", function(object) standardGeneric("swMarkov_Conv"))


#' set_Markov
#' Need to define and export this generic method -- otherwise, \code{\link{set_Markov<-}} doesn't work.
#' @export
setGeneric("set_Markov", function(object, value) standardGeneric("set_Markov"))
#' set_Markov<-
#' @param object An object of class \code{\linkS4class{swMarkov}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric("set_Markov<-", function(object, value) standardGeneric("set_Markov<-"))
#' swMarkov_Prob<-
#' @param object An object of class \code{\linkS4class{swMarkov}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric("swMarkov_Prob<-", function(object, value) standardGeneric("swMarkov_Prob<-"))
#' swMarkov_Conv<-
#' @param object An object of class \code{\linkS4class{swMarkov}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swMarkov}} and \code{\linkS4class{swInputData}}
setGeneric("swMarkov_Conv<-", function(object, value) standardGeneric("swMarkov_Conv<-"))
########################


#####WeatherData########
#' get_WeatherHistory
#' @param object An object of class \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swInputData}}
setGeneric("get_WeatherHistory", function(object) standardGeneric("get_WeatherHistory"))
#' get_swWeatherData
#' @param object An object of class \code{\linkS4class{swWeatherData}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swWeatherData}} and \code{\linkS4class{swInputData}}
setGeneric("get_swWeatherData", function(object,year) standardGeneric("get_swWeatherData"))

#' set_WeatherHistory
#' Need to define and export this generic method -- otherwise, \code{\link{set_WeatherHistory<-}} doesn't work.
#' @export
setGeneric("set_WeatherHistory", function(object, value) standardGeneric("set_WeatherHistory"))
#' set_WeatherHistory<-
#' @param object An object of class \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swInputData}}
setGeneric("set_WeatherHistory<-", function(object, value) standardGeneric("set_WeatherHistory<-"))
#' set_swWeatherData
#' Need to define and export this generic method -- otherwise, \code{\link{set_swWeatherData<-}} doesn't work.
#' @export
setGeneric("set_swWeatherData", function(object, value) standardGeneric("set_swWeatherData"))
#' set_swWeatherData<-
#' @param object An object of class \code{\linkS4class{swWeatherData}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swWeatherData}} and \code{\linkS4class{swInputData}}
setGeneric("set_swWeatherData<-", function(object, value) standardGeneric("set_swWeatherData<-"))
########################

#######CLOUD############
#' get_swCloud
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("get_swCloud", function(object) standardGeneric("get_swCloud"))
#' swCloud_SkyCover
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_SkyCover", function(object) standardGeneric("swCloud_SkyCover"))
#' swCloud_WindSpeed
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_WindSpeed", function(object) standardGeneric("swCloud_WindSpeed"))
#' swCloud_Humidity
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_Humidity", function(object) standardGeneric("swCloud_Humidity"))
#' swCloud_Transmissivity
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_Transmissivity", function(object) standardGeneric("swCloud_Transmissivity"))
#' swCloud_SnowDensity
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_SnowDensity", function(object) standardGeneric("swCloud_SnowDensity"))

#' get_swCloud_daily
#' @param object An object of class \code{\linkS4class{swCloud_daily}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud_daily}} and \code{\linkS4class{swInputData}}
setGeneric("get_swCloud_daily", function(object) standardGeneric("get_swCloud_daily"))
#' swCloud_Humidity
#' @param object An object of class \code{\linkS4class{swCloud_daily}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCloud_daily}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_daily_Humidity", function(object) standardGeneric("swCloud_daily_Humidity"))

#' set_swCloud
#' Need to define and export this generic method -- otherwise, \code{\link{set_swCloud<-}} doesn't work.
#' @export
setGeneric("set_swCloud", function(object, value) standardGeneric("set_swCloud"))
#' set_swCloud<-
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("set_swCloud<-", function(object, value) standardGeneric("set_swCloud<-"))
#' swCloud_SkyCover<-
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_SkyCover<-", function(object, value) standardGeneric("swCloud_SkyCover<-"))
#' swCloud_WindSpeed<-
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_WindSpeed<-", function(object, value) standardGeneric("swCloud_WindSpeed<-"))
#' swCloud_Humidity<-
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_Humidity<-", function(object, value) standardGeneric("swCloud_Humidity<-"))
#' swCloud_Transmissivity<-
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_Transmissivity<-", function(object, value) standardGeneric("swCloud_Transmissivity<-"))
#' swCloud_SnowDensity<-
#' @param object An object of class \code{\linkS4class{swCloud}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_SnowDensity<-", function(object, value) standardGeneric("swCloud_SnowDensity<-"))

#' set_swCloud
#' Need to define and export this generic method -- otherwise, \code{\link{set_swCloud_daily<-}} doesn't work.
#' @export
setGeneric("set_swCloud_daily", function(object, value) standardGeneric("set_swCloud_daily"))
#' set_swCloud_daily<-
#' @param object An object of class \code{\linkS4class{swCloud_daily}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud_daily}} and \code{\linkS4class{swInputData}}
setGeneric("set_swCloud_daily<-", function(object, value) standardGeneric("set_swCloud_daily<-"))
#' swCloud_Humidity<-
#' @param object An object of class \code{\linkS4class{swCloud_daily}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCloud_daily}} and \code{\linkS4class{swInputData}}
setGeneric("swCloud_daily_Humidity<-", function(object, value) standardGeneric("swCloud_daily_Humidity<-"))
########################

########PROD############
#' get_swProd
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("get_swProd", function(object) standardGeneric("get_swProd"))
#' swProd_Composition
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_Composition", function(object) standardGeneric("swProd_Composition"))
#' swProd_Albedo
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_Albedo", function(object) standardGeneric("swProd_Albedo"))
#' swProd_Cover_stcr
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_Cover_stcr", function(object) standardGeneric("swProd_Cover_stcr"))
#' swProd_CanopyHeight
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_CanopyHeight", function(object) standardGeneric("swProd_CanopyHeight"))
#' swProd_VegInterParam
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_VegInterParam", function(object) standardGeneric("swProd_VegInterParam"))
#' swProd_LitterInterParam
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_LitterInterParam", function(object) standardGeneric("swProd_LitterInterParam"))
#' swProd_EsTpartitioning_param
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_EsTpartitioning_param", function(object) standardGeneric("swProd_EsTpartitioning_param"))
#' swProd_Es_param_limit
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_Es_param_limit", function(object) standardGeneric("swProd_Es_param_limit"))
#' swProd_Shade
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_Shade", function(object) standardGeneric("swProd_Shade"))
#' swProd_HydrRedstro_use
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_HydrRedstro_use", function(object) standardGeneric("swProd_HydrRedstro_use"))
#' swProd_HydrRedstro
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_HydrRedstro", function(object) standardGeneric("swProd_HydrRedstro"))
#' swProd_CritSoilWaterPotential
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_CritSoilWaterPotential", function(object) standardGeneric("swProd_CritSoilWaterPotential"))
#' swProd_CO2Coefficients
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_CO2Coefficients", function(object) standardGeneric("swProd_CO2Coefficients"))
#' swProd_MonProd_veg
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param vegtype The name or index of the vegetation type.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_MonProd_veg", function(object, vegtype) standardGeneric("swProd_MonProd_veg"))
#' swProd_MonProd_grass
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_MonProd_grass", function(object) standardGeneric("swProd_MonProd_grass"))
#' swProd_MonProd_shrub
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_MonProd_shrub", function(object) standardGeneric("swProd_MonProd_shrub"))
#' swProd_MonProd_tree
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_MonProd_tree", function(object) standardGeneric("swProd_MonProd_tree"))
#' swProd_MonProd_forb
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_MonProd_forb", function(object) standardGeneric("swProd_MonProd_forb"))

#' set_swProd
#' Need to define and export this generic method -- otherwise, \code{\link{set_swProd<-}} doesn't work.
#' @export
setGeneric("set_swProd", function(object, value) standardGeneric("set_swProd"))
#' set_swProd<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("set_swProd<-", function(object, value) standardGeneric("set_swProd<-"))
#' swProd_Composition<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_Composition<-", function(object, value) standardGeneric("swProd_Composition<-"))
#' swProd_Albedo<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_Albedo<-", function(object, value) standardGeneric("swProd_Albedo<-"))
#' swProd_Cover_stcr<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_Cover_stcr<-", function(object, value) standardGeneric("swProd_Cover_stcr<-"))
#' swProd_CanopyHeight<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_CanopyHeight<-", function(object, value) standardGeneric("swProd_CanopyHeight<-"))
#' swProd_VegInterParam<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_VegInterParam<-", function(object, value) standardGeneric("swProd_VegInterParam<-"))
#' swProd_LitterInterParam<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_LitterInterParam<-", function(object, value) standardGeneric("swProd_LitterInterParam<-"))
#' swProd_EsTpartitioning_param<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_EsTpartitioning_param<-", function(object, value) standardGeneric("swProd_EsTpartitioning_param<-"))
#' swProd_Es_param_limit<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_Es_param_limit<-", function(object, value) standardGeneric("swProd_Es_param_limit<-"))
#' swProd_Shade<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_Shade<-", function(object, value) standardGeneric("swProd_Shade<-"))
#' swProd_HydrRedstro_use<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_HydrRedstro_use<-", function(object, value) standardGeneric("swProd_HydrRedstro_use<-"))
#' swProd_HydrRedstro<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_HydrRedstro<-", function(object, value) standardGeneric("swProd_HydrRedstro<-"))
#' swProd_CritSoilWaterPotential<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_CritSoilWaterPotential<-", function(object, value) standardGeneric("swProd_CritSoilWaterPotential<-"))
#' swProd_CO2Coefficients<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_CO2Coefficients<-", function(object, value) standardGeneric("swProd_CO2Coefficients<-"))
#' swProd_MonProd_veg<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param vegtype The name or index of the vegetation type.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_MonProd_veg<-", function(object, vegtype, value) standardGeneric("swProd_MonProd_veg<-"))
#' swProd_MonProd_grass<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_MonProd_grass<-", function(object, value) standardGeneric("swProd_MonProd_grass<-"))
#' swProd_MonProd_shrub<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_MonProd_shrub<-", function(object, value) standardGeneric("swProd_MonProd_shrub<-"))
#' swProd_MonProd_tree<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_MonProd_tree<-", function(object, value) standardGeneric("swProd_MonProd_tree<-"))
#' swProd_MonProd_forb<-
#' @param object An object of class \code{\linkS4class{swProd}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swProd}} and \code{\linkS4class{swInputData}}
setGeneric("swProd_MonProd_forb<-", function(object, value) standardGeneric("swProd_MonProd_forb<-"))
########################

#######SITE#############
#' get_swSite
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("get_swSite", function(object) standardGeneric("get_swSite"))
#' swSite_SWClimits
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_SWClimits", function(object) standardGeneric("swSite_SWClimits"))
#' swSite_ModelFlags
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_ModelFlags", function(object) standardGeneric("swSite_ModelFlags"))
#' swSite_ModelCoefficients
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_ModelCoefficients", function(object) standardGeneric("swSite_ModelCoefficients"))
#' swSite_SnowSimulationParams
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_SnowSimulationParams", function(object) standardGeneric("swSite_SnowSimulationParams"))
#' swSite_DrainageCoefficient
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_DrainageCoefficient", function(object) swSite_DrainageCoefficient("swSite_DrainageCoefficient"))
#' swSite_EvapCoefficients
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_EvapCoefficients", function(object) standardGeneric("swSite_EvapCoefficients"))
#' swSite_TranspCoefficients
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_TranspCoefficients", function(object) standardGeneric("swSite_TranspCoefficients"))
#' swSite_IntrinsicSiteParams
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_IntrinsicSiteParams", function(object) standardGeneric("swSite_IntrinsicSiteParams"))
#' swSite_SoilTemperatureFlag
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_SoilTemperatureFlag", function(object) standardGeneric("swSite_SoilTemperatureFlag"))
#' swSite_SoilTemperatureConsts
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_SoilTemperatureConsts", function(object) standardGeneric("swSite_SoilTemperatureConsts"))
#' swSite_TranspirationRegions
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_TranspirationRegions", function(object) standardGeneric("swSite_TranspirationRegions"))

#' set_swSite
#' Need to define and export this generic method -- otherwise, \code{\link{set_swSite<-}} doesn't work.
#' @export
setGeneric("set_swSite", function(object, value) standardGeneric("set_swSite"))
#' set_swSite<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("set_swSite<-", function(object, value) standardGeneric("set_swSite<-"))
#' swSite_SWClimits<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_SWClimits<-", function(object, value) standardGeneric("swSite_SWClimits<-"))
#' swSite_ModelFlags<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_ModelFlags<-", function(object, value) standardGeneric("swSite_ModelFlags<-"))
#' swSite_ModelCoefficients<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_ModelCoefficients<-", function(object, value) standardGeneric("swSite_ModelCoefficients<-"))
#' swSite_SnowSimulationParams<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_SnowSimulationParams<-", function(object, value) standardGeneric("swSite_SnowSimulationParams<-"))
#' swSite_DrainageCoefficient<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_DrainageCoefficient<-", function(object, value) standardGeneric("swSite_DrainageCoefficient<-"))
#' swSite_EvapCoefficients<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_EvapCoefficients<-", function(object, value) standardGeneric("swSite_EvapCoefficients<-"))
#' swSite_TranspCoefficients<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_TranspCoefficients<-", function(object, value) standardGeneric("swSite_TranspCoefficients<-"))
#' swSite_IntrinsicSiteParams<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_IntrinsicSiteParams<-", function(object, value) standardGeneric("swSite_IntrinsicSiteParams<-"))
#' swSite_SoilTemperatureFlag<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_SoilTemperatureFlag<-", function(object, value) standardGeneric("swSite_SoilTemperatureFlag<-"))
#' swSite_SoilTemperatureConsts<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_SoilTemperatureConsts<-", function(object, value) standardGeneric("swSite_SoilTemperatureConsts<-"))
#' swSite_TranspirationRegions<-
#' @param object An object of class \code{\linkS4class{swSite}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSite}} and \code{\linkS4class{swInputData}}
setGeneric("swSite_TranspirationRegions<-", function(object, value) standardGeneric("swSite_TranspirationRegions<-"))
########################

#########SOILS##########
#' get_swSoils
#' @param object An object of class \code{\linkS4class{swSoils}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSoils}} and \code{\linkS4class{swInputData}}
setGeneric("get_swSoils", function(object) standardGeneric("get_swSoils"))
#' swSoils_Layers
#' @param object An object of class \code{\linkS4class{swSoils}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSoils}} and \code{\linkS4class{swInputData}}
setGeneric("swSoils_Layers", function(object) standardGeneric("swSoils_Layers"))

#' set_swSoils
#' Need to define and export this generic method -- otherwise, \code{\link{set_swSoils<-}} doesn't work.
#' @export
setGeneric("set_swSoils", function(object, value) standardGeneric("set_swSoils"))
#' set_swSoils<-
#' @param object An object of class \code{\linkS4class{swSoils}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSoils}} and \code{\linkS4class{swInputData}}
setGeneric("set_swSoils<-", function(object, value) standardGeneric("set_swSoils<-"))
#' swSoils_Layers<-
#' @param object An object of class \code{\linkS4class{swSoils}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSoils}} and \code{\linkS4class{swInputData}}
setGeneric("swSoils_Layers<-", function(object, value) standardGeneric("swSoils_Layers<-"))
########################

#########ESTAB##########
#' get_swEstab
#' @param object An object of class \code{\linkS4class{swEstab}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swEstab}} and \code{\linkS4class{swInputData}}
setGeneric("get_swEstab", function(object) standardGeneric("get_swEstab"))
#' swEstab_useEstab
#' @param object An object of class \code{\linkS4class{swEstab}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swEstab}} and \code{\linkS4class{swInputData}}
setGeneric("swEstab_useEstab", function(object) standardGeneric("swEstab_useEstab"))
#species here#

#' set_swEstab<-
#' @param object An object of class \code{\linkS4class{swEstab}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swEstab}} and \code{\linkS4class{swInputData}}
setGeneric("set_swEstab<-", function(object, value) standardGeneric("set_swEstab<-"))
#' swEstab_useEstab<-
#' @param object An object of class \code{\linkS4class{swEstab}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swEstab}} and \code{\linkS4class{swInputData}}
setGeneric("swEstab_useEstab<-", function(object, value) standardGeneric("swEstab_useEstab<-"))
#species here#
########################

#########CARBON##########
#' get_swCarbon
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("get_swCarbon", function(object) standardGeneric("get_swCarbon"))
#' swCarbon_Use_Bio
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("swCarbon_Use_Bio", function(object) standardGeneric("swCarbon_Use_Bio"))
#' swCarbon_Use_WUE
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("swCarbon_Use_WUE", function(object) standardGeneric("swCarbon_Use_WUE"))
#' swCarbon_Scenario
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("swCarbon_Scenario", function(object) standardGeneric("swCarbon_Scenario"))
#' swCarbon_DeltaYear
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("swCarbon_DeltaYear", function(object) standardGeneric("swCarbon_DeltaYear"))
#' swCarbon_CO2ppm
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("swCarbon_CO2ppm", function(object) standardGeneric("swCarbon_CO2ppm"))

#' set_swCarbon
#' Need to define and export this generic method -- otherwise, \code{\link{set_swCarbon<-}} doesn't work.
#' @export
setGeneric("set_swCarbon", function(object, value) standardGeneric("set_swCarbon"))
#' set_swCarbon<-
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("set_swCarbon<-", function(object, value) standardGeneric("set_swCarbon<-"))
#' swCarbon_Use_Bio<-
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("swCarbon_Use_Bio<-", function(object, value) standardGeneric("swCarbon_Use_Bio<-"))
#' swCarbon_Use_WUE<-
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("swCarbon_Use_WUE<-", function(object, value) standardGeneric("swCarbon_Use_WUE<-"))
#' swCarbon_Scenario<-
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("swCarbon_Scenario<-", function(object, value) standardGeneric("swCarbon_Scenario<-"))
#' swCarbon_DeltaYear<-
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("swCarbon_DeltaYear<-", function(object, value) standardGeneric("swCarbon_DeltaYear<-"))
#' swCarbon_CO2ppm<-
#' @param object An object of class \code{\linkS4class{swCarbon}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swCarbon}} and \code{\linkS4class{swInputData}}
setGeneric("swCarbon_CO2ppm<-", function(object, value) standardGeneric("swCarbon_CO2ppm<-"))
########################

#########SWC############
#' get_swSWC
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("get_swSWC", function(object) standardGeneric("get_swSWC"))
#' swSWC_use
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_use", function(object) standardGeneric("swSWC_use"))
#' swSWC_prefix
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_prefix", function(object) standardGeneric("swSWC_prefix"))
#' swSWC_FirstYear
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_FirstYear", function(object) standardGeneric("swSWC_FirstYear"))
#' swSWC_Method
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_Method", function(object) standardGeneric("swSWC_Method"))
#' swSWC_HistoricList
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_HistoricList", function(object) standardGeneric("swSWC_HistoricList"))
#' swSWC_HistoricData
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_HistoricData", function(object, year) standardGeneric("swSWC_HistoricData"))

#' set_swSWC
#' Need to define and export this generic method -- otherwise, \code{\link{set_swSWC<-}} doesn't work.
#' @export
setGeneric("set_swSWC", function(object, value) standardGeneric("set_swSWC"))
#' set_swSWC<-
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("set_swSWC<-", function(object, value) standardGeneric("set_swSWC<-"))
#' swSWC_use<-
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_use<-", function(object, value) standardGeneric("swSWC_use<-"))
#' swSWC_prefix<-
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_prefix<-", function(object, value) standardGeneric("swSWC_prefix<-"))
#' swSWC_FirstYear<-
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_FirstYear<-", function(object, value) standardGeneric("swSWC_FirstYear<-"))
#' swSWC_Method<-
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_Method<-", function(object, value) standardGeneric("swSWC_Method<-"))
#' swSWC_HistoricList<-
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_HistoricList<-", function(object, value) standardGeneric("swSWC_HistoricList<-"))
#' swSWC_HistoricData<-
#' @param object An object of class \code{\linkS4class{swSWC}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swSWC}} and \code{\linkS4class{swInputData}}
setGeneric("swSWC_HistoricData<-", function(object, value) standardGeneric("swSWC_HistoricData<-"))
########################

#######OUT###########
#' get_swOUT
#' @param object An object of class \code{\linkS4class{swOUT}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric("get_swOUT", function(object) standardGeneric("get_swOUT"))
#' swOUT_TimeStep
#' @param object An object of class \code{\linkS4class{swOUT}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric("swOUT_TimeStep", function(object) standardGeneric("swOUT_TimeStep"))
#' swOUT_OutputSeparator
#' @param object An object of class \code{\linkS4class{swOUT}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric("swOUT_OutputSeparator", function(object) standardGeneric("swOUT_OutputSeparator"))

#' set_swOUT
#' Need to define and export this generic method -- otherwise, \code{\link{set_swOUT<-}} doesn't work.
#' @export
setGeneric("set_swOUT", function(object, value) standardGeneric("set_swOUT"))
#' set_swOUT<-
#' @param object An object of class \code{\linkS4class{swOUT}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric("set_swOUT<-", function(object, value) standardGeneric("set_swOUT<-"))
#' swOUT_TimeStep<-
#' @param object An object of class \code{\linkS4class{swOUT}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric("swOUT_TimeStep<-", function(object, value) standardGeneric("swOUT_TimeStep<-"))
#' swOUT_TimeStepsForEveryKey
#' Need to define and export this generic method -- otherwise, \code{\link{swOUT_TimeStepsForEveryKey<-}} doesn't work.
#' @export
setGeneric("swOUT_TimeStepsForEveryKey", function(object, value) standardGeneric("swOUT_TimeStepsForEveryKey"))
#' swOUT_TimeStepsForEveryKey<-
#' @param object An object of class \code{\linkS4class{swOUT}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric("swOUT_TimeStepsForEveryKey<-", function(object, value) standardGeneric("swOUT_TimeStepsForEveryKey<-"))
#' swOUT_OutputSeparator<-
#' @param object An object of class \code{\linkS4class{swOUT}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swOUT}} and \code{\linkS4class{swInputData}}
setGeneric("swOUT_OutputSeparator<-", function(object, value) standardGeneric("swOUT_OutputSeparator<-"))
########################

########LOG#############
#' swLog_setLine<-
#' @param object An object of class \code{\linkS4class{swLog}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swLog}} and \code{\linkS4class{swInputData}}
setGeneric("swLog_setLine<-", function(object, value) standardGeneric("swLog_setLine<-"))
########################

########swOutput########
#' swOutput_getKEY
#' @param object An object of class \code{\linkS4class{swOutput}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric("swOutput_getKEY", function(object,index) standardGeneric("swOutput_getKEY"))
#' swOutput_KEY_Period
#' @param object An object of class \code{\linkS4class{swOutput}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric("swOutput_KEY_Period", function(object,index) standardGeneric("swOutput_KEY_Period"))
#' swOutput_KEY_TimeStep
#' @param object An object of class \code{\linkS4class{swOutput}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric("swOutput_KEY_TimeStep", function(object) standardGeneric("swOutput_KEY_TimeStep"))
#' swOutput_KEY_Columns
#' @param object An object of class \code{\linkS4class{swOutput}} or \code{\linkS4class{swInputData}}.
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric("swOutput_KEY_Columns", function(object) standardGeneric("swOutput_KEY_Columns"))

#' swOutput_setKEY<-
#' @param object An object of class \code{\linkS4class{swOutput}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric("swOutput_setKEY<-", function(object,index,value) standardGeneric("swOutput_setKEY<-"))
#' swOutput_KEY_Period<-
#' @param object An object of class \code{\linkS4class{swOutput}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric("swOutput_KEY_Period<-", function(object,index,value) standardGeneric("swOutput_KEY_Period<-"))
#' swOutput_KEY_TimeStep<-
#' @param object An object of class \code{\linkS4class{swOutput}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric("swOutput_KEY_TimeStep<-", function(object, value) standardGeneric("swOutput_KEY_TimeStep<-"))
#' swOutput_KEY_Columns<-
#' @param object An object of class \code{\linkS4class{swOutput}} or \code{\linkS4class{swInputData}}.
#' @param value A value to assign to a specific slot of the \code{object}.
#' @seealso \code{\linkS4class{swOutput}} and \code{\linkS4class{swInputData}}
setGeneric("swOutput_KEY_Columns<-", function(object, value) standardGeneric("swOutput_KEY_Columns<-"))

