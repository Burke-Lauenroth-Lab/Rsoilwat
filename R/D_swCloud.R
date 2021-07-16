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


# Author: Ryan J. Murphy (2013); Daniel R Schlaepfer (2013-2018)
###############################################################################


########################CLOUD DATA################################
# TODO: consider individual slots for each row of the 5 x 12 matrix

#' Class \code{"swCloud"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swCloud}}.
#' @param .Object An object of class \code{\linkS4class{swCloud}}.
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
#' showClass("swCloud")
#' x <- new("swCloud")
#'
#' @name swCloud-class
#' @export
setClass("swCloud", slots = c(Cloud = "matrix"))


swCloud_validity <- function(object) {
  val <- TRUE
  temp <- dim(object@Cloud)

  if (temp[1] != 5) {
    msg <- paste(
      "@Cloud must have exactly 5 rows corresponding to",
      "SkyCoverPCT, WindSpeed_m/s, HumidityPCT,",
      "SnowDensity_kg/m^3, and RainEvents_per_day"
    )
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (temp[2] != 12) {
    msg <- paste("@Cloud must have exactly 12 columns corresponding months.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (!all(is.na(object@Cloud[1, ])) && (any(object@Cloud[1, ] < 0) ||
      any(object@Cloud[1, ] > 100))) {
    msg <- paste("@Cloud['SkyCoverPCT', ] must be values between 0 and 100%.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (!all(is.na(object@Cloud[2, ])) && (any(object@Cloud[2, ] < 0))) {
    msg <- paste("@Cloud['WindSpeed_m/s', ] must be values >= 0.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (!all(is.na(object@Cloud[3, ])) && (any(object@Cloud[3, ] < 0) ||
      any(object@Cloud[3, ] > 100))) {
    msg <- paste("@Cloud['HumidityPCT', ] must be values between 0 and 100%.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (!all(is.na(object@Cloud[4, ])) && any(object@Cloud[4, ] < 0)) {
    msg <- paste("@Cloud['SnowDensity_kg/m^3', ] must be values >= 0.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  if (!all(is.na(object@Cloud[5, ])) && any(object@Cloud[5, ] < 1)) {
    msg <- paste("@Cloud['RainEvents_per_day', ] must be values >= 1.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
}
setValidity("swCloud", swCloud_validity)

#' @rdname swCloud-class
#' @export
setMethod("initialize", signature = "swCloud", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "cloud")
  sns <- slotNames(def)
  dots <- list(...)
  dns <- names(dots)

  # We don't set values for slot `Cloud` (except SnowDensity and RainEvents)
  # if not passed via ...; this is to prevent simulation runs with accidentally
  # incorrect values
  if (!("Cloud" %in% dns)) {
    ids <- 4:5
    def@Cloud[- ids, ] <- NA_real_
  } else {
    # Guarantee dimnames
    dimnames(dots[["Cloud"]]) <- dimnames(def@Cloud)
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
})



#' @rdname swCloud-class
#' @export
setMethod("get_swCloud", "swCloud", function(object) object)
#' @rdname swCloud-class
#' @export
setMethod("swCloud_SkyCover", "swCloud", function(object) object@Cloud[1, ])
#' @rdname swCloud-class
#' @export
setMethod("swCloud_WindSpeed", "swCloud", function(object) object@Cloud[2, ])
#' @rdname swCloud-class
#' @export
setMethod("swCloud_Humidity", "swCloud", function(object) object@Cloud[3, ])
#' @rdname swCloud-class
#' @export
setMethod("swCloud_SnowDensity", "swCloud", function(object) object@Cloud[4, ])
#' @rdname swCloud-class
#' @export
setMethod("swCloud_RainEvents", "swCloud", function(object) object@Cloud[5, ])

#' @rdname swCloud-class
#' @export
setReplaceMethod("set_swCloud", signature = "swCloud", function(object, value) {
  dimnames(value@Cloud) <- dimnames(object@Cloud)
  object <- value
  validObject(object)
  object
})
#' @rdname swCloud-class
#' @export
setReplaceMethod("swCloud_SkyCover", signature = "swCloud",
  function(object, value) {
    object@Cloud[1, ] <- value
    validObject(object)
    object
})
#' @rdname swCloud-class
#' @export
setReplaceMethod("swCloud_WindSpeed", signature = "swCloud",
  function(object, value) {
    object@Cloud[2, ] <- value
    validObject(object)
    object
})
#' @rdname swCloud-class
#' @export
setReplaceMethod("swCloud_Humidity", signature = "swCloud",
  function(object, value) {
    object@Cloud[3, ] <- value
    validObject(object)
    object
})
#' @rdname swCloud-class
#' @export
setReplaceMethod("swCloud_SnowDensity", signature = "swCloud",
  function(object, value) {
    object@Cloud[4, ] <- value
    validObject(object)
    object
})
#' @rdname swCloud-class
#' @export
setReplaceMethod("swCloud_RainEvents", signature = "swCloud",
  function(object, value) {
    object@Cloud[5, ] <- value
    validObject(object)
    object
})


#' @rdname swCloud-class
#' @export
setMethod("swReadLines", signature = c(object = "swCloud", file = "character"),
  function(object, file) {
    infiletext <- readLines(con = file)
    #should be no empty lines
    infiletext <- infiletext[infiletext != ""]

    object@Cloud <- matrix(data = NA, nrow = 5, ncol = 12, byrow = TRUE)

    colnames(object@Cloud) <- c("January", "February", "March", "April", "May",
      "June", "July", "August", "September", "October", "November", "December")
    rownames(object@Cloud) <- c("SkyCoverPCT", "WindSpeed_m/s", "HumidityPCT",
      "SnowDensity_kg/m^3", "RainEvents_per_day")

    for (i in seq_along(infiletext)) {
      object@Cloud[i, ] <- readNumerics(infiletext[i], 12)
    }

    object
})
