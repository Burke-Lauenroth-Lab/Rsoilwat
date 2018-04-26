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


########################CLOUD DATA################################
# TODO: consider individual slots for each row of the 5 x 12 matrix

#' Class \code{"swCloud_daily"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swCloud_daily}}.
#' @param .Object An object of class \code{\linkS4class{swCloud_daily}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Further arguments to methods.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swInputData}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swCloud_daily")
#' x <- new("swCloud_daily")
#'
#' @name swCloud_daily-class
#' @export
setClass("swCloud_daily", slots = c(Cloud_daily = "matrix"))


swCloud_daily_validity <- function(object) {
  val <- TRUE
  temp <- dim(object@Cloud_daily)
  
  if (temp[1] != 1) {
    msg <- paste("@Cloud_daily must have exactly 1 rows corresponding to",
                 "HumidityPCT")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (temp[2] != 366) {
    msg <- paste("@Cloud_daily must have exactly 366 columns corresponding days")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  
  if (!all(is.na(object@Cloud_daily[1, ])) && (any(object@Cloud_daily[1, ] < 0) || any(object@Cloud_daily[1, ] > 100))) {
    msg <- paste("@Cloud_daily['HumidityPCT', ] must be values between 0 and 100%.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swCloud_daily", swCloud_daily_validity)

#' @rdname swCloud_daily-class
#' @export
setMethod("initialize", signature = "swCloud_daily", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "cloud_daily")
  sns <- slotNames(def)
  dots <- list(...)
  dns <- names(dots)
  
  # We don't set values for slot `Cloud` (except SnowDensity) if not passed via ...; this
  # is to prevent simulation runs with accidentally incorrect values
  if (!("Cloud_daily" %in% dns)) {
    def@Cloud_daily[-1, ] <- NA_real_
  } else {
    # Guarantee dimnames
    dimnames(dots[["Cloud_daily"]]) <- dimnames(def@Cloud_daily)
  }
  
  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
  }
  
  #.Object <- callNextMethod(.Object, ...) # not needed because no relevant inheritance
  validObject(.Object)
  .Object
})

#' @rdname swCloud_daily-class
#' @export
setMethod("get_swCloud_daily", "swCloud_daily", function(object) object)
#' @rdname swCloud_daily-class
#' @export
setMethod("swCloud_daily_Humidity", "swCloud_daily", function(object) object@Cloud_daily[1, ])

#' @rdname swCloud_daily-class
#' @export
setReplaceMethod("set_swCloud_daily", signature = "swCloud_daily", function(object, value) {
  dimnames(value@Cloud_daily) <- dimnames(object@Cloud_daily)
  object <- value
  validObject(object)
  object
})
#' @rdname swCloud_daily-class
#' @export
setReplaceMethod("swCloud_daily_Humidity", signature = "swCloud_daily", function(object, value) {
  object@Cloud_daily[1, ] <- value
  validObject(object)
  object
})


#' @rdname swCloud_daily-class
#' @export
setMethod("swReadLines", signature = c(object="swCloud_daily",file="character"), function(object,file) {
  infiletext <- readLines(con = file)
  #should be no empty lines
  infiletext <- infiletext[infiletext != ""]
  
  object@Cloud_daily=matrix(data=NA,nrow=1,ncol=366,byrow=T)
  colnames(object@Cloud_daily)<-toString(c(1:366))
  rownames(object@Cloud_daily)<-c("HumidityPCT")
  
  for(i in 1:length(infiletext)) {
    object@Cloud_daily[i,] <- readNumerics(infiletext[i],366)
  }
  return(object)
})
