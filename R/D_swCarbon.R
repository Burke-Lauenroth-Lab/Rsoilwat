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

# Author: Zach Kramer (2017); Daniel R Schlaepfer (2017-2018)
##############################################


#' Class \code{"swCarbon"}
#'
#' Class \code{swCarbon} defines variables that allow \code{SOILWAT2} to
#' simulate the effects of atmospheric carbon dioxide.
#'
#' @param object An object of class \code{\linkS4class{swCarbon}}.
#' @param .Object An object of class \code{\linkS4class{swCarbon}}.
#' @param value A value to assign to a specific slot of the object.
#' @param ... Further arguments to methods.
#'
#' @slot CarbonUseBio Object of class \code{"integer"}, where a value of 1
#'   enables the \var{CO2} biomass multiplier.
#' @slot CarbonUseWUE Object of class \code{"integer"}, where a value of 1
#'   enables the \var{CO2} water-use efficiency \var{WUE} multiplier.
#' @slot Scenario Object of class \code{"character"}, that represents the name
#'   of the scenario that is being simulated. This slot is not used in
#'   \pkg{rSOILWAT2}, but it's useful to see what scenario was used in the
#'   \pkg{SOILWAT2} input object.
#' @slot DeltaYear Object of class \code{"integer"}, that represents the number
#'   of years in the future that this simulation is being run.
#' @slot CO2ppm Object of class \code{"matrix"}, that holds years in the first
#'   column and CO2 ppm concentrations in the second column.
#'
#' @seealso \code{\linkS4class{swInputData}}
#'
#' @examples
#' showClass("swCarbon")
#' x <- new("swCarbon")
#'
#' @name swCarbon-class
#' @export
setClass(
  "swCarbon",
  slots = c(
    CarbonUseBio = "integer",
    CarbonUseWUE = "integer",
    Scenario = "character",
    DeltaYear = "integer",
    CO2ppm = "matrix"
  )
)

#' @rdname swCarbon-class
#' @export
setMethod(
  "initialize",
  signature = "swCarbon",
  function(.Object, ...) {
    def <- slot(rSOILWAT2::sw_exampleData, "carbon")
    sns <- slotNames(def)
    dots <- list(...)
    dns <- names(dots)

    if ("CO2ppm" %in% dns) {
      # Guarantee dimnames
      dimnames(dots[["CO2ppm"]]) <- dimnames(def@CO2ppm)
    }

    for (sn in sns) {
      slot(.Object, sn) <- if (sn %in% dns) dots[[sn]] else slot(def, sn)
    }

    if (FALSE) {
      # not needed because no relevant inheritance
      .Object <- callNextMethod(.Object, ...)
    }

    validObject(.Object)
    .Object
  }
)

setValidity("swCarbon", function(object) {
  val <- TRUE

  if (!all(c("Year", "CO2ppm") == colnames(object@CO2ppm)) ||
    length(colnames(object@CO2ppm)) != 2) {
    msg <- "@CO2ppm: column names must be 'Year' and 'CO2ppm'"
    val <- if (isTRUE(val)) msg else c(val, msg)

  } else {
    is_bad <- any(is.na(object@CO2ppm[, "Year"]) |
      round(object@CO2ppm[, "Year"]) != object@CO2ppm[, "Year"])
    if (is_bad) {
      msg <- "@CO2ppm: has missing and/or non-integer-like years"
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    is_bad <- !all(diff(object@CO2ppm[, "Year"]) == 1)
    if (is_bad) {
      msg <- "@CO2ppm: years are not consecutive"
      val <- if (isTRUE(val)) msg else c(val, msg)
    }

    ids_bad <- is.na(object@CO2ppm[, "CO2ppm"]) | object@CO2ppm[, "CO2ppm"] < 0
    if (any(ids_bad)) {
      msg <- "@CO2ppm: has missing and/or negative CO2-concentration values"
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
  }

  val
})

#' @rdname swCarbon-class
#' @export
setMethod("get_swCarbon", "swCarbon", function(object) object)
#' @rdname swCarbon-class
#' @export
setMethod("swCarbon_Use_Bio", "swCarbon", function(object) object@CarbonUseBio)
#' @rdname swCarbon-class
#' @export
setMethod("swCarbon_Use_WUE", "swCarbon", function(object) object@CarbonUseWUE)
#' @rdname swCarbon-class
#' @export
setMethod("swCarbon_Scenario", "swCarbon", function(object) object@Scenario)
#' @rdname swCarbon-class
#' @export
setMethod("swCarbon_DeltaYear", "swCarbon", function(object) object@DeltaYear)
#' @rdname swCarbon-class
#' @export
setMethod("swCarbon_CO2ppm", "swCarbon", function(object) object@CO2ppm)

#' @rdname swCarbon-class
#' @export
setReplaceMethod("set_swCarbon", signature = "swCarbon",
  function(object, value) {
    object <- value
    validObject(object)
    object
})

#' @rdname swCarbon-class
#' @export
setReplaceMethod("swCarbon_Use_Bio", signature = "swCarbon",
  function(object, value) {
    object@CarbonUseBio <- as.integer(as.logical(value))
    validObject(object)
    object
})

#' @rdname swCarbon-class
#' @export
setReplaceMethod("swCarbon_Use_WUE", signature = "swCarbon",
  function(object, value) {
    object@CarbonUseWUE <- as.integer(as.logical(value))
    validObject(object)
    object
})

#' @rdname swCarbon-class
#' @export
setReplaceMethod("swCarbon_Scenario", signature = "swCarbon",
  function(object, value) {
    object@Scenario <- value
    validObject(object)
    object
})

#' @rdname swCarbon-class
#' @export
setReplaceMethod("swCarbon_DeltaYear", signature = "swCarbon",
  function(object, value) {
    object@DeltaYear <- as.integer(value)
    validObject(object)
    object
})

#' @rdname swCarbon-class
#' @export
setReplaceMethod("swCarbon_CO2ppm", signature = "swCarbon",
  function(object, value) {
    colnames(value) <- colnames(object@CO2ppm)
    object@CO2ppm <- value
    validObject(object)
    object
})
