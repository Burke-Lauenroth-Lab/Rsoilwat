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

#######################Monthly Scaling Params#################################

#' Class \code{"swMonthlyScalingParams"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param .Object An object of class
#'   \code{\linkS4class{swMonthlyScalingParams}}.
#' @param ... Further arguments to methods.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swWeather}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swMonthlyScalingParams")
#' x <- new("swMonthlyScalingParams")
#'
#' @name swMonthlyScalingParams-class
#' @export
setClass("swMonthlyScalingParams", slots = c(MonthlyScalingParams = "matrix"))

setValidity("swMonthlyScalingParams", function(object) {
  val <- TRUE
  temp <- dim(object@MonthlyScalingParams)

  if (temp[2] != 6) {
    msg <- paste("@MonthlyScalingParams must have exactly 6 columns ",
      "corresponding to PPT, MaxT, MinT, SkyCover, Wind, rH")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }
  if (temp[1] != 12) {
    msg <- paste("@MonthlyScalingParams must have exactly 12 rows",
      "corresponding months.")
    val <- if (isTRUE(val)) msg else c(val, msg)
  }

  val
})

#' @rdname swMonthlyScalingParams-class
#' @export
setMethod("initialize", signature = "swMonthlyScalingParams",
  function(.Object, ...) {
    def <- slot(rSOILWAT2::sw_exampleData, "weather")
    sns <- slotNames("swMonthlyScalingParams")
    dots <- list(...)
    dns <- names(dots)

    if ("MonthlyScalingParams" %in% dns) {
      # Guarantee dimnames
      dimnames(dots[["MonthlyScalingParams"]]) <-
        dimnames(def@MonthlyScalingParams)
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



#####################WEATHERSETUP.IN###################################

#' Class \code{"swWeather"}
#'
#' The methods listed below work on this class and the proper slot of the class
#'   \code{\linkS4class{swInputData}}.
#'
#' @param object An object of class \code{\linkS4class{swWeather}}.
#' @param .Object An object of class \code{\linkS4class{swWeather}}.
#' @param value A value to assign to a specific slot of the object.
#' @param file A character string. The file name from which to read.
#' @param ... Further arguments to methods.
#'
#' @seealso \code{\linkS4class{swInputData}} \code{\linkS4class{swFiles}}
#' \code{\linkS4class{swInputData}} \code{\linkS4class{swCloud}}
#' \code{\linkS4class{swMarkov}} \code{\linkS4class{swProd}}
#' \code{\linkS4class{swSite}} \code{\linkS4class{swSoils}}
#' \code{\linkS4class{swEstab}} \code{\linkS4class{swOUT}}
#' \code{\linkS4class{swSWC}} \code{\linkS4class{swLog}}
#'
#' @examples
#' showClass("swWeather")
#' x <- new("swWeather")
#'
#' @name swWeather-class
#' @export
setClass(
  "swWeather",
  slots = c(
    UseSnow = "logical",
    pct_SnowDrift = "numeric",
    pct_SnowRunoff = "numeric",
    use_weathergenerator = "logical",
    use_weathergenerator_only = "logical",
    FirstYear_Historical = "integer"
  ),
  contains = "swMonthlyScalingParams"
)

setValidity("swWeather", function(object) {
  val <- TRUE
  sns <- setdiff(slotNames("swWeather"), inheritedSlotNames("swWeather"))

  for (sn in sns) {
    if (length(slot(object, sn)) != 1) {
      msg <- paste0("@", sn, " must have exactly one value.")
      val <- if (isTRUE(val)) msg else c(val, msg)
    }
  }

  val
})


#' @rdname swWeather-class
#' @export
setMethod("initialize", signature = "swWeather", function(.Object, ...) {
  def <- slot(rSOILWAT2::sw_exampleData, "weather")
  sns <- setdiff(slotNames("swWeather"), inheritedSlotNames("swWeather"))
  dots <- list(...)
  dns <- names(dots)

  for (sn in sns) {
    slot(.Object, sn) <- if (sn %in% dns) {
      dots[[sn]]
    } else {
      if (sn == "FirstYear_Historical") {
        -1L
      } else {
        slot(def, sn)
      }
    }
  }

  .Object <- callNextMethod(.Object, ...)
  validObject(.Object)

  .Object
})



#' @rdname swWeather-class
#' @export
setMethod("swWeather_DaysRunningAverage", "swWeather",
  function(object) object@DaysRunningAverage)

#' @rdname swWeather-class
#' @export
setMethod("swWeather_FirstYearHistorical", "swWeather",
  function(object) object@FirstYear_Historical)

#' @rdname swWeather-class
#' @export
setMethod("swWeather_pct_SnowDrift", "swWeather",
  function(object) object@pct_SnowDrift)

#' @rdname swWeather-class
#' @export
setMethod("swWeather_pct_SnowRunoff", "swWeather",
  function(object) object@pct_SnowRunoff)

#' @rdname swWeather-class
#' @export
setMethod("swWeather_UseMarkov", "swWeather",
  function(object) object@use_weathergenerator)

#' @rdname swWeather-class
#' @export
setMethod("swWeather_UseMarkovOnly", "swWeather",
  function(object) object@use_weathergenerator_only)

#' @rdname swWeather-class
#' @export
setMethod("swWeather_UseSnow", "swWeather",
  function(object) object@UseSnow)

#' @rdname swWeather-class
#' @export
setMethod("swWeather_MonScalingParams", "swWeather",
  function(object) object@MonthlyScalingParams)

#' @rdname swWeather-class
#' @export
setReplaceMethod("swWeather_DaysRunningAverage", signature = "swWeather",
  function(object, value) {
    object@DaysRunningAverage <- as.integer(value)
    validObject(object)
    object
})

#' @rdname swWeather-class
#' @export
setReplaceMethod("swWeather_FirstYearHistorical", signature = "swWeather",
  function(object, value) {
    object@FirstYear_Historical <- as.integer(value)
    validObject(object)
    object
})

#' @rdname swWeather-class
#' @export
setReplaceMethod("swWeather_pct_SnowDrift", signature = "swWeather",
  function(object, value) {
    object@pct_SnowDrift <- as.numeric(value)
    validObject(object)
    object
})

#' @rdname swWeather-class
#' @export
setReplaceMethod("swWeather_pct_SnowRunoff", signature = "swWeather",
  function(object, value) {
    object@pct_SnowRunoff <- as.numeric(value)
    validObject(object)
    object
})

#' @rdname swWeather-class
#' @export
setReplaceMethod("swWeather_UseMarkov", signature = "swWeather",
  function(object, value) {
    object@use_weathergenerator <- as.logical(value)
    validObject(object)
    object
})

#' @rdname swWeather-class
#' @export
setReplaceMethod("swWeather_UseMarkovOnly", signature = "swWeather",
  function(object, value) {
    object@use_weathergenerator_only <- as.logical(value)
    if (object@use_weathergenerator_only) {
      object@use_weathergenerator <- TRUE
    }
    validObject(object)
    object
})

#' @rdname swWeather-class
#' @export
setReplaceMethod("swWeather_UseSnow", signature = "swWeather",
  function(object, value) {
    object@UseSnow <- as.logical(value)
    validObject(object)
    object
})

#' @rdname swWeather-class
#' @export
setReplaceMethod("swWeather_MonScalingParams", signature = "swWeather",
  function(object, value) {
    object@MonthlyScalingParams[] <- value
    validObject(object)
    object
})



#' @rdname swWeather-class
#' @export
setMethod("swReadLines",
  signature = c(object = "swWeather", file = "character"),
  function(object, file) {
    print(paste(
      "TODO: method 'swReadLines' for class 'swWeather' is not up-to-date;",
      "hard-coded indices are incorrect"
    ))
    infiletext <- readLines(con = file)

    object@UseSnow <- readLogical(infiletext[4])
    object@pct_SnowDrift <- readNumeric(infiletext[5])
    object@pct_SnowRunoff <- readNumeric(infiletext[6])
    object@use_weathergenerator <- readLogical(infiletext[7])
    object@FirstYear_Historical <- readInteger(infiletext[8])

    data <- matrix(data = c(rep(1, 12), rep(NA, 12 * 5)), nrow = 12, ncol = 6)
    colnames(data) <- c("PPT", "MaxT", "MinT", "SkyCover", "Wind", "rH")
    rownames(data) <- c("January", "February", "March", "April", "May",
      "June", "July", "August", "September", "October", "November", "December")

    for (i in 21:32) {
      data[i - 20, ] <- readNumerics(infiletext[i], 8)[2:8]
    }
    object@MonthlyScalingParams <- data

    object
})
