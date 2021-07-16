#' Pedotransfer functions to convert between soil moisture (volumetric water
#' content, \var{VWC}) and soil water potential (\var{SWP})
#'
#' @param sand A numeric value or vector. Sand content of the soil layer(s) as
#'   fractional value in \code{[0,1]}.
#' @param clay A numeric value or vector. Clay content of the soil layer(s) as
#'   fractional value in \code{[0,1]}.
#'
#' @references Cosby, B. J., G. M. Hornberger, R. B. Clapp, and T. R. Ginn.
#' 1984. A statistical exploration of the relationships of soil moisture
#' characteristics to the physical properties of soils. Water Resources Research
#' 20:682-690.
#'
#' @name pedotransfer
NULL

#' @rdname pedotransfer
#' @section Note: either \code{swp} or \code{sand}/\code{clay} needs be a
#'   single value
pdf_to_vwc <- function(swp, sand, clay, thetas, psis, b, MPa_toBar = -10,
  bar_conversion = 1024) {

  thetas * (psis / (swp * MPa_toBar * bar_conversion)) ^ (1 / b) / 100
}

#' @rdname pedotransfer
#' @section Note: either \code{vwc} or \code{sand}/\code{clay} needs be a
#'   single value
pdf_to_swp <- function(vwc, sand, clay, thetas, psis, b, bar_toMPa = -0.1,
  bar_conversion = 1024) {

  psis / ((vwc * 100 / thetas) ^ b * bar_conversion) * bar_toMPa
}

pedotransfer <- function(x, sand, clay, pdf) {
  stopifnot(length(sand) && length(sand) == length(clay))
  sand <- rSW2utils::finite01(sand, NA, NA)
  clay <- rSW2utils::finite01(clay, NA, NA)

  if (any(complete.cases(sand, clay))) {
    thetas <- -14.2 * sand - 3.7 * clay + 50.5
    psis <- 10 ^ (-1.58 * sand - 0.63 * clay + 2.17)
    b <- -0.3 * sand + 15.7 * clay + 3.10
    if (any(b <= 0, na.rm = TRUE))
      stop("Pedotransfer for soil texture with b <= 0 is not possible.")

    np_x <- NROW(x) * NCOL(x)

    if (NROW(x) == 1 || NCOL(x) == 1) {
      # cases 1-4
      if (np_x == 1 || length(sand) == 1) {
        # cases 1-3
        res <- pdf(x, sand, clay, thetas, psis, b)

      } else {
        # case 4; Note: case 3 could also be calculated with the code for
        # case 4, but is much slower, unless x is a data.frame with one column
        temp <- lapply(x, function(v) pdf(v, sand, clay, thetas, psis, b))
        res <- matrix(unlist(temp), nrow = np_x, byrow = TRUE)
      }

    } else {
      # cases 5-6
      dx <- dim(x)

      if (length(sand) == 1) {
        # case 5
        res <- vapply(seq_len(dx[2]), function(d) {
            pdf(x[, d], sand, clay, thetas, psis, b)
          }, rep(1, dx[1]), USE.NAMES = FALSE)

      } else {
        # case 6
        stopifnot(dx[2] == length(sand))
        res <- vapply(seq_len(dx[2]), function(d) {
            pdf(x[, d], sand[d], clay[d], thetas[d], psis[d], b[d])
          }, rep(1, dx[1]), USE.NAMES = FALSE)
      }
    }

  } else {
    res <- x
    res[] <- NA
  }

  # if SWP then in units of MPa [-Inf, 0]; if VWC then in units of m3/m3 [0, 1]
  res
}

#' Calculate volumetric water content from soil water potential and soil texture
#' @rdname pedotransfer
#' @param swp A numeric value, vector, or 2-dimensional object
#'   (matrix or data.frame). The soil water potential (of the soil matrix) in
#'   units of \var{MPa}, i.e., the soil without the volume of rock and gravel.
#'
#' @return Volumetric water content in units of m^3 (of water) / m^3 (of soil)
#'  \code{[0, 1]}. There are six use cases:\enumerate{
#'    \item 1) \itemize{
#'      \item Input: \code{SWP} [single value]; \code{sand} and \code{clay}
#'        [single values]
#'      \item Output: \code{VWC} [single value]}
#'    \item 2) \itemize{
#'      \item Input: \code{SWP} [single value]; \code{sand} and \code{clay}
#'        [vectors of length d]
#'      \item Output: \code{VWC} [vector of length d]}
#'    \item 3) \itemize{
#'      \item Input: \code{SWP} [vector of length l]; \code{sand} and
#'        \code{clay} infraction [single values]
#'      \item Output: \code{VWC} [vector of length l]}
#'    \item 4) \itemize{
#'      \item Input: \code{SWP} [vector of length l]; \code{sand} and
#'        \code{clay} [vectors of length d]
#'      \item Output: \code{VWC} [l x d matrix] where \code{SWP} is
#'        repeated for each column}
#'    \item 5) \itemize{
#'      \item Input: \code{SWP} [l x d matrix]; \code{sand} and \code{clay}
#'        [single values]
#'      \item Output: \code{VWC} [l x d matrix]}
#'    \item 6) \itemize{
#'      \item Input: \code{SWP} [l x d matrix]; \code{sand} and \code{clay}
#'        [vectors of length d]
#'      \item Output: \code{VWC} [l x d matrix], \code{sand} and \code{clay}
#'        vectors are repeated for each row}
#'  }
#' @export
SWPtoVWC <- function(swp, sand, clay) {
  pedotransfer(swp, sand, clay, pdf = pdf_to_vwc)
}


#' Calculate soil water potential from volumetric water content and soil texture
#' @rdname pedotransfer
#' @param vwc A numeric value, vector, or 2-dimensional object
#'   (matrix or data.frame). The matric soil moisture, i.e., reduced by the
#'   volume of rock and gravel.
#'
#' @return Soil water potential in units of \var{MPa} \code{[-Inf, 0]}.
#'   There are six use cases: \enumerate{
#'    \item 1) \itemize{
#'      \item Input: \code{VWC} [single value]; \code{sand} and \code{clay}
#'        [single values]
#'      \item Output: \code{SWP} [single value]}
#'    \item 2) \itemize{
#'      \item Input: \code{VWC} [single value]; \code{sand} and \code{clay}
#'        [vectors of length d]
#'      \item Output: \code{SWP} [vector of length d]}
#'    \item 3) \itemize{
#'      \item Input: \code{VWC} [vector of length l]; \code{sand} and
#'        \code{clay} in fraction [single values]
#'      \item Output: \code{SWP} [vector of length l]}
#'    \item 4) \itemize{
#'      \item Input: \code{VWC} [vector of length l]; \code{sand} and
#'        \code{clay} [vectors of length d]
#'      \item Output: \code{SWP} [l x d matrix] where \code{VWC} is repeated for
#'        each column}
#'    \item 5) \itemize{
#'      \item Input: \code{VWC} [l x d matrix]; \code{sand} and \code{clay}
#'        [single values]
#'      \item Output: \code{SWP} [l x d matrix]}
#'    \item 6) \itemize{
#'      \item Input: \code{VWC} [l x d matrix]; \code{sand} and \code{clay}
#'        [vectors of length d]
#'      \item Output: \code{SWP} [l x d matrix], \code{sand} and \code{clay}
#'        vectors are repeated for each row}
#'  }
#' @export
VWCtoSWP <- function(vwc, sand, clay) {
  pedotransfer(vwc, sand, clay, pdf = pdf_to_swp)
}
