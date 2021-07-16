#' Package \pkg{rSOILWAT2}: summary information
#'
#' The package \pkg{rSOILWAT2} is a R implementation of \pkg{SOILWAT2}
#' Modeling Software.  The C code was converted into a library with
#' input/output functions to R. \pkg{SOILWAT2} is a simulator of the dynamics
#' of soil water. The input data structure is \var{S4}. Generic functions that
#' apply to the input container or individual object control setting/getting
#' input data. Output data is in the form of lists.
#'
#'
#' @section Version: The version level of the package is given by the command
#'   \code{packageDescription("rSOILWAT2")}. The most recent version of the
#'   package can be obtained from github at
#'   \url{https://github.com/DrylandEcology/rSOILWAT2}
#'
#' @seealso \itemize{
#'   \item \code{\link{sw_exec}} for running a simulation
#'   \item \code{\link{sw_inputData}} and \code{\link{sw_inputDataFromFiles}}
#'         for data input
#'   \item \code{\link{dbW_getWeatherData}} and
#'         \code{\link{getWeatherData_folders}} for weather data input
#' }
#'
#' @references Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth (2014)
#' Ecohydrology of adjacent sagebrush and lodgepole pine ecosystems: The
#' consequences of climate change and disturbance.  \emph{Ecosystems}
#' \bold{17}:590--605.
#'
#' Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford (2012)
#' Ecohydrological niche of sagebrush ecosystems.  \emph{Ecohydrology}
#' \bold{5}:453--466.
#'
#' Parton, W.J. (1978).  \emph{Abiotic section of ELM}. In: Grassland
#' simulation model (ed. Innis, G.S.).  Springer New York, NY, pp. 31--53.
#'
#' Sala, O.E., Lauenroth, W.K. & Parton, W.J. (1992) Long-term soil-water
#' dynamics in the shortgrass steppe.  \emph{Ecology} \bold{73}:1175--1181.
#'
#' @docType package
#' @name rSOILWAT2
#' @aliases rSOILWAT2 rSOILWAT2-package rSOILWAT2 SoilWat SOILWAT SOILWAT2
#   rSOILWAT2-package
"_PACKAGE"


##------ Package level variables
rSW2_glovars <- new.env()


##------ Import from other packages
## Package uses S3/S4 classes - they are defined in package:methods
#' @importFrom methods slot slot<- as as<- initialize new slotNames
#'   inheritedSlotNames getSlots validObject callNextMethod
#' @importFrom stats aggregate coef complete.cases cor cov fitted median
#'   na.exclude na.omit predict quantile sd weighted.mean
NULL

##------ Access SOILWAT2 C code
#' @useDynLib rSOILWAT2, .registration = TRUE, .fixes = "C_"
NULL
