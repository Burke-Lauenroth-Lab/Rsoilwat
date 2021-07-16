
#' Calculate the composition (land cover) representing a
#' potential natural vegetation based on climate relationships
#'
#' The function returns relative abundance/land cover values that completely
#' cover the surface (i.e., they sum to 1) of a site specified by climate and/or
#' fixed input values.
#'
#' @section Details: Some of the land cover/vegetation types, i.e., trees,
#'   annual grasses, and bare-ground are not estimated from climate
#'   relationships; they are either set to 0, or alternatively fixed at the
#'   value of the input argument(s).
#'
#' @section Details: The remaining vegetation types, i.e., shrubs, C3 grasses,
#'   C4 grasses, forbs, and succulents, are estimated from climate relationships
#'   using equations developed by Paruelo & Lauenroth 1996, or alternatively
#'   fixed at the value of the input argument(s). If values for
#'   \code{dailyC4vars} are provided, then equations developed by Teeri & Stowe
#'   1976 are used to limit the occurrence of C4 grasses.
#'
#' @section Details: The relative abundance values of the the vegetation types
#'   that can be estimated and are not fixed by inputs, are estimated in two
#'   steps: (i) as if they cover the entire surface; (ii) scaled to the
#'   proportion of the surface that is not fixed by inputs.
#'
#' @section Notes: The equations developed Paruelo & Lauenroth 1996 are based
#'  on sites with \var{MAT} from 2 C to 21.2 C and \var{MAP} from 117 to
#'  1011 mm. If \code{warn_extrapolation} is set to \code{TRUE}, then
#'  inputs are checked against supported ranges, i.e., if \var{MAT} is below
#'  1 C, then it is reset to 1 C with a warning. If other inputs exceed their
#'  ranges, then a warning is issued and the code proceeds.
#'
#' @param MAP_mm A numeric value. Mean annual precipitation in millimeter.
#' @param MAT_C A numeric value. Mean annual temperature in degree Celsius.
#' @param mean_monthly_ppt_mm A numeric vector of length 12. Mean monthly
#'   precipitation in millimeter.
#' @param mean_monthly_Temp_C A numeric vector of length 12. Mean monthly
#'   temperature in degree Celsius.
#' @param dailyC4vars A named list of length 3 or \code{NULL}.
#'   If not \code{NULL}, then a correction for C4-grasses based on
#'   Teeri & Stowe 1976 is applied based on the variables: \describe{
#'   \item{\code{Month7th_NSadj_MinTemp_C}}{Mean minimum temperature of July on
#'     the northern hemisphere and January on the southern hemisphere}
#'   \item{\code{DegreeDaysAbove65F_NSadj_DaysC}}{Degree days above 65 F = 18.33
#'     C in units of days x degree Celsius}
#'   \item{\code{LengthFreezeFreeGrowingPeriod_NSadj_Days}}{Mean annual number
#'     of days of the longest continuous period where minimum daily temperature
#'     remain above freezing} }
#' @param isNorth A logical value. \code{TRUE} for locations on northern
#'   hemisphere.
#' @param shrub_limit A numeric value. Default value is 0.2 based on page 1213
#'   of Paruelo & Lauenroth 1996.
#' @param fix_succulents A logical value. If \code{TRUE}, then value for the
#'   succulent component is fixed at \code{Succulents_Fraction} instead of
#'   calculated from climatic relationships.
#' @param Succulents_Fraction A numeric value between 0 and 1. \code{NA} is
#'   treated as if \code{fix_succulents} is \code{FALSE}.
#' @param fix_sumgrasses A logical value. If \code{TRUE}, then the summed
#'   values of the three grass types, i.e., annuals, C3, and C4 grasses,
#'   is fixed at \code{SumGrasses_Fraction}. If they are not fixed themselves,
#'   then their values are estimated and scaled to sum to
#'   \code{SumGrasses_Fraction}.
#' @param SumGrasses_Fraction A numeric value between 0 and 1.
#'   \code{NA} is treated as if \code{fix_sumgrasses} is \code{FALSE}.
#' @param fix_annuals A logical value. If \code{TRUE}, then value for the annual
#'   component is fixed at \code{Annuals_Fraction}.
#' @param Annuals_Fraction A numeric value between 0 and 1. Default value is 0.
#' @param fix_C4grasses A logical value. If \code{TRUE}, then value for the
#'   C4-grass component is fixed at \code{C4_Fraction} instead of calculated
#'   from climatic relationships.
#' @param C4_Fraction A numeric value between 0 and 1. \code{NA} is treated as
#'   if \code{fix_C4grasses} is \code{FALSE}.
#' @param fix_C3grasses A logical value. If \code{TRUE}, then value for the
#'   C3-grass component is fixed at \code{C3_Fraction} instead of calculated
#'   from climatic relationships.
#' @param C3_Fraction A numeric value between 0 and 1. \code{NA} is treated as
#'   if \code{fix_C3grasses} is \code{FALSE}.
#' @param fix_shrubs A logical value. If \code{TRUE}, then value for the shrub
#'   component is fixed at \code{Shrubs_Fraction} instead of calculated from
#'   climatic relationships.
#' @param Shrubs_Fraction A numeric value between 0 and 1. \code{NA} is treated
#'   as if \code{fix_shrubs} is \code{FALSE}.
#' @param fix_forbs A logical value. If \code{TRUE}, then value for the forb
#'   component is fixed at \code{Forbs_Fraction}.
#' @param Forbs_Fraction A numeric value between 0 and 1. Default value is 0.
#' @param fix_trees A logical value. If \code{TRUE}, then value for the tree
#'   component is fixed at \code{Trees_Fraction}.
#' @param Trees_Fraction A numeric value between 0 and 1. Default value is 0.
#' @param fix_BareGround A logical value. If \code{TRUE}, then value for the
#'   bare ground component is fixed at \code{BareGround_Fraction}.
#' @param BareGround_Fraction A numeric value between 0 and 1.
#'  Default value is 0.
#' @param fill_empty_with_BareGround A logical value. If \code{TRUE}, then
#'   incomplete land cover is considered (additional) bare-ground. If
#'   \code{FALSE}, then some hacks are used to "fill in" incomplete land cover
#'   with grasses and/or shrubs, and additionally,
#'   if \code{fix_BareGround} is \code{FALSE} with bare-ground.
#' @param warn_extrapolation A logical value. If \code{TRUE}, then
#'   warnings are issued if climate inputs \code{MAP_mm} and/or \code{MAT_C}
#'   fall outside the range of supported values. See notes.
#'
#' @return A list with three named numeric vectors. \describe{
#'   \item{Rel_Abundance_L0}{A numeric vector of length 8 with
#'     relative abundance/cover [0-1] values of land cover types that sum to 1.
#'     The names of the 8 types are: \var{Succulents}, \var{Forbs},
#'     \var{Grasses_C3}, \var{Grasses_C4}, \var{Grasses_Annuals},
#'     \var{Shrubs}, \var{Trees}, \var{BareGround}.
#'   }
#'   \item{Rel_Abundance_L1}{A numeric vector of length 5 with
#'     relative abundance/cover [0-1] values of \pkg{rSOILWAT2} land cover
#'     types that sum to 1.
#'     The names of the 5 types are: \var{SW_TREES}, \var{SW_SHRUBS},
#'     \var{SW_FORBS}, \var{SW_GRASS}, and \var{SW_BAREGROUND}.
#'   }
#'   \item{Grasses}{A numeric vector of length 3 with
#'     relative abundance/cover [0-1] values of the grass types that sum to 1,
#'     if there is any grass cover; otherwise, the values are 0.
#'     The names of the 3 sub-types are: \var{Grasses_C3}, \var{Grasses_C4},
#'     and \var{Grasses_Annuals}.
#'   }
#' }
#'
#' @references Paruelo J.M., Lauenroth W.K. (1996) Relative abundance of plant
#'   functional types in grasslands and shrublands of North America. Ecological
#'   Applications, 6, 1212-1224.
#' @references Teeri J.A., Stowe L.G. (1976) Climatic patterns and the
#'   distribution of C4 grasses in North America. Oecologia, 23, 1-12.
#'
#' @examples
#' ## Load weather dataset from rSOILWAT2
#' utils::data("weatherData", package = "rSOILWAT2")
#' clim1 <- calc_SiteClimate(weatherList = weatherData)
#' clim2 <- calc_SiteClimate(weatherList = weatherData, do_C4vars = TRUE)
#'
#' ## All estimable vegetation types are estimated:
#' estimate_PotNatVeg_composition(
#'   MAP_mm = 10 * clim1[["MAP_cm"]], MAT_C = clim1[["MAT_C"]],
#'   mean_monthly_ppt_mm = 10 * clim1[["meanMonthlyPPTcm"]],
#'   mean_monthly_Temp_C = clim1[["meanMonthlyTempC"]]
#' )
#'
#' ## Climate is outside supported range with MAT < 0 C:
#' estimate_PotNatVeg_composition(
#'   MAP_mm = 10 * clim1[["MAP_cm"]],
#'   MAT_C = clim1[["MAT_C"]] - clim1[["MAT_C"]],
#'   mean_monthly_ppt_mm = 10 * clim1[["meanMonthlyPPTcm"]],
#'   mean_monthly_Temp_C = clim1[["meanMonthlyTempC"]] - clim1[["MAT_C"]]
#' )
#'
#' ## Some land cover types are fixed and others are estimated, and
#' ## the C4-grass adjustment is used:
#' estimate_PotNatVeg_composition(
#'   MAP_mm = 10 * clim2[["MAP_cm"]], MAT_C = clim2[["MAT_C"]],
#'   mean_monthly_ppt_mm = 10 * clim2[["meanMonthlyPPTcm"]],
#'   mean_monthly_Temp_C = clim2[["meanMonthlyTempC"]],
#'   dailyC4vars = clim2[["dailyC4vars"]],
#'   fix_shrubs = TRUE, Shrubs_Fraction = 0.5,
#'   fix_BareGround = TRUE, BareGround_Fraction = 0.25
#' )
#'
#' ## Fix total grass cover and annual grass cover,
#' ## but estimate relative proportions of C3 and C4 grasses:
#' estimate_PotNatVeg_composition(
#'   MAP_mm = 10 * clim2[["MAP_cm"]], MAT_C = clim2[["MAT_C"]],
#'   mean_monthly_ppt_mm = 10 * clim2[["meanMonthlyPPTcm"]],
#'   mean_monthly_Temp_C = clim2[["meanMonthlyTempC"]],
#'   dailyC4vars = clim2[["dailyC4vars"]],
#'   fix_sumgrasses = TRUE, SumGrasses_Fraction = 0.8,
#'   fix_annuals = TRUE, Annuals_Fraction = 0.3
#' )
#'
#' @export
estimate_PotNatVeg_composition <- function(MAP_mm, MAT_C,
  mean_monthly_ppt_mm, mean_monthly_Temp_C, dailyC4vars = NULL,
  isNorth = TRUE, shrub_limit = 0.2,
  fix_succulents = FALSE, Succulents_Fraction = NA,
  fix_sumgrasses = FALSE, SumGrasses_Fraction = NA,
  fix_annuals = TRUE, Annuals_Fraction = 0,
  fix_C4grasses = FALSE, C4_Fraction = NA,
  fix_C3grasses = FALSE, C3_Fraction = NA,
  fix_shrubs = FALSE, Shrubs_Fraction = NA,
  fix_forbs = FALSE, Forbs_Fraction = NA,
  fix_trees = TRUE, Trees_Fraction = 0,
  fix_BareGround = TRUE, BareGround_Fraction = 0,
  fill_empty_with_BareGround = TRUE,
  warn_extrapolation = TRUE) {

  veg_types <- c(
    "Succulents", "Forbs",
    "Grasses_C3", "Grasses_C4", "Grasses_Annuals",
    "Shrubs", "Trees",
    "BareGround"
  )
  Nveg <- length(veg_types)

  isuc <- 1 # succulents
  ifor <- 2 # forbs
  igc3 <- 3 # grasses-C3
  igc4 <- 4 # grasses-C4
  igan <- 5 # grasses-annuals
  ishr <- 6 # shrubs
  itre <- 7 # trees
  ibar <- 8 # bare-ground

  veg_cover <- rep(0, Nveg)

  # groups without climate-equations, i.e., always set to a specific value
  iset <- c(igan, itre, ibar)

  # groups with climate-equations to estimate relative abundance
  iestim <- c(igc4, igc3, ishr, ifor, isuc)
  igrasses <- c(igc3, igc4, igan)


  #--- Get the user specified fractions: input cover fraction values:
  input_cover <- rep(NA, Nveg)

  # Groups that are either fixed or 0, i.e., cannot be NA = not estimated
  input_cover[igan] <- if (fix_annuals) {
    rSW2utils::finite01(Annuals_Fraction)
  } else {
    0
  }
  input_cover[itre] <- if (fix_trees) {
    rSW2utils::finite01(Trees_Fraction)
  } else {
    0
  }
  input_cover[ibar] <- if (fix_BareGround) {
    rSW2utils::finite01(BareGround_Fraction)
  } else {
    0
  }

  # Groups that are either fixed or estimated based on climate-relationships
  input_cover[igc4] <- if (fix_C4grasses) C4_Fraction else NA
  input_cover[igc3] <- if (fix_C3grasses) C3_Fraction else NA
  input_cover[ishr] <- if (fix_shrubs) Shrubs_Fraction else NA
  input_cover[ifor] <- if (fix_forbs) Forbs_Fraction else NA
  input_cover[isuc] <- if (fix_succulents) Succulents_Fraction else NA

  # treat negative input values as if NA
  input_cover <- rSW2utils::cut0Inf(input_cover, val = NA)


  #--- Check individual components if the sum of grasses is fixed
  fix_sumgrasses <- fix_sumgrasses && isTRUE(!is.na(SumGrasses_Fraction))

  if (fix_sumgrasses) {
    SumGrasses_Fraction <- rSW2utils::cut0Inf(SumGrasses_Fraction, val = 0)

    input_sum_grasses <- rSW2utils::replace_NAs_with_val(
      x = sum(input_cover[igrasses], na.rm = TRUE),
      val_replace = 0
    )

    add_sum_grasses <- SumGrasses_Fraction - input_sum_grasses

    if (add_sum_grasses < 0) {
      stop(
        "'estimate_PotNatVeg_composition': ",
        "User defined grass values including C3, C4, and annuals ",
        "sum to more than user defined total grass cover."
      )

    }

    ids_to_estim_grasses <- is.na(input_cover[igrasses])

    if (add_sum_grasses > 0) {
      if (sum(ids_to_estim_grasses) == 1) {
        # One grass component to estimate: difference from rest
        input_cover[igrasses[ids_to_estim_grasses]] <-
          SumGrasses_Fraction - input_sum_grasses

        add_sum_grasses <- 0
      }

    } else {
      # No grass component to add: set all to zero
      input_cover[igrasses[ids_to_estim_grasses]] <- 0
    }
  }


  #--- Decide if all fractions are sufficiently defined or if they need to be
  # estimated based on climate reltionships
  input_sum <- sum(input_cover, na.rm = TRUE)
  ifixed <- unique(c(iset, which(!is.na(input_cover))))

  ids_to_estim <- which(is.na(input_cover))
  n_to_estim <- length(ids_to_estim)

  if (input_sum > 1) {
    stop(
      "'estimate_PotNatVeg_composition': ",
      "User defined relative abundance values sum to more than ",
      "1 = full land cover."
    )
  }


  #--- Incomplete surface cover
  veg_cover <- input_cover

  if (n_to_estim <= 1) {
    #--- Less than one component to estimate: no need for equations

    if (n_to_estim == 0) {
      #--- All fixed, nothing to estimate
      if (fill_empty_with_BareGround) {
        veg_cover[ibar] <- 1 - sum(veg_cover[-ibar], na.rm = TRUE)

      } else if (input_sum < 1) {
        stop(
          "'estimate_PotNatVeg_composition': ",
          "User defined relative abundance values are all fixed, ",
          "but their sum is smaller than 1 = full land cover."
        )
      }

    } else if (n_to_estim == 1) {
      #--- One value to estimate: difference from rest
      veg_cover[ids_to_estim] <- 1 - input_sum
    }

  } else {
    #---Potential natural vegetation
    # i.e., (input_sum < 1 && sum(is.na(input_cover)) > 1) is TRUE;
    # thus, estimate relative abundance fractions based on climate relationships

    if (MAP_mm <= 1) {
      # No precipitation ==> no vegetation, only bare-ground
      # TODO: what about fog?
      veg_cover[] <- 0
      veg_cover[ibar] <- 1

    } else {

      estim_cover <- rep(NA, Nveg)

      # Estimate climate variables
      if (isNorth) {
        Months_WinterTF <- c(12, 1:2)
        Months_SummerTF <- c(6:8)
      } else {
        Months_WinterTF <- c(6:8)
        Months_SummerTF <- c(12, 1:2)
      }

      # Fraction of precipitation falling during summer/winter months
      ppt.SummerToMAP <- sum(mean_monthly_ppt_mm[Months_SummerTF]) / MAP_mm
      ppt.WinterToMAP <- sum(mean_monthly_ppt_mm[Months_WinterTF]) / MAP_mm

      # Temperature in July minus temperature in January
      therm_amp <- mean_monthly_Temp_C[Months_SummerTF[2]] -
        mean_monthly_Temp_C[Months_WinterTF[2]]

      if (warn_extrapolation) {
        # Adjust climate variables to limits underlying the data used to develop
        # equations Paruelo & Lauenroth (1996): "The selected sites cover a
        #   range of MAT from 2 C to 21.2 C and a range of precipitation (MAP)
        #   from 117 to 1011 mm"

        # MAT limits:
        if (MAT_C < 1) {
          # Note: MAT = 1 C as limit instead of 2 C based on empirical testing;
          # also because log(x) is undefined for x < 0 and results in negative
          # values for x < 1. Hence the threshold of 1.
          warning(
            "Equations used outside supported range (2 - 21.2 C): ",
           "MAT = ", round(MAT_C, 2), " C reset to 1 C."
          )
          MAT_C <- 1
        }

        if (MAT_C > 21.2) {
          warning(
            "Equations used outside supported range (2 - 21.2 C): ",
            "MAT = ", round(MAT_C, 2), " C."
          )
        }

        if (MAP_mm < 117 || MAP_mm > 1011) {
          warning(
            "Equations used outside supported range (117-1011 mm): ",
            "MAP = ", round(MAP_mm), " mm."
          )
        }
      }


      # 1. step: estimate relative abundance based on
      # Paruelo & Lauenroth (1996): shrub climate-relationship:
      if (MAP_mm < 1) {
        estim_cover[ishr] <- 0
      } else {
        # if not enough winter precipitation for a given MAP, then equation
        # results in negative values which we set to 0
        estim_cover[ishr] <- rSW2utils::cut0Inf(
          1.7105 - 0.2918 * log(MAP_mm) + 1.5451 * ppt.WinterToMAP,
          val = 0
        )
      }

      # Paruelo & Lauenroth (1996): C4-grass climate-relationship:
      if (MAT_C <= 0) {
        estim_cover[igc4] <- 0
      } else {
        # if either MAT < 0 or not enough summer precipitation or
        # too cold for a given MAP, then equation results in negative values
        # which we set to 0
        estim_cover[igc4] <- rSW2utils::cut0Inf(
          -0.9837 + 0.000594 * MAP_mm +
            1.3528 * ppt.SummerToMAP + 0.2710 * log(MAT_C),
          val = 0
        )

        # 2. step: Teeri JA, Stowe LG (1976)
        # This equations give percent species/vegetation -> use to limit
        # Paruelo's C4 equation, i.e., where no C4 species => C4 abundance == 0
        if (is.list(dailyC4vars)) {
          if (dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"] <= 0) {
            grass_c4_species <- 0
          } else {
            x10 <- dailyC4vars["Month7th_NSadj_MinTemp_C"] * 9 / 5 + 32
            x13 <- dailyC4vars["DegreeDaysAbove65F_NSadj_DaysC"] * 9 / 5
            x18 <- log(dailyC4vars["LengthFreezeFreeGrowingPeriod_NSadj_Days"])
            grass_c4_species <- as.numeric(
              (1.60 * x10 + 0.0086 * x13 - 8.98 * x18 - 22.44) / 100
            )
          }

          if (grass_c4_species <= rSW2_glovars[["tol"]]) {
            estim_cover[igc4] <- 0
          }
        }
      }

      # Paruelo & Lauenroth (1996): C3-grass climate-relationship:
      if (ppt.WinterToMAP <= 0) {
        c3_in_grassland <- c3_in_shrubland <- NA
      } else {
        # if not enough winter precipitation or too warm for a
        # given MAP, then equation results in negative values which we set to 0
        c3_in_grassland <- rSW2utils::cut0Inf(
          1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 1,
          val = 0
        )
        c3_in_shrubland <- rSW2utils::cut0Inf(
          1.1905 - 0.02909 * MAT_C + 0.1781 * log(ppt.WinterToMAP) - 0.2383 * 2,
          val = 0
        )
      }

      temp <- estim_cover[ishr] >= shrub_limit && !is.na(estim_cover[ishr])
      estim_cover[igc3] <- ifelse(temp, c3_in_shrubland, c3_in_grassland)

      # Paruelo & Lauenroth (1996): forb climate-relationship:
      if (MAP_mm < 1 || MAT_C <= 0) {
        estim_cover[ifor] <- NA
      } else {
        estim_cover[ifor] <- rSW2utils::cut0Inf(
          -0.2035 + 0.07975 * log(MAP_mm) - 0.0623 * log(MAT_C),
          val = 0
        )
      }

      # Paruelo & Lauenroth (1996): succulent climate-relationship:
      if (therm_amp <= 0 || ppt.WinterToMAP <= 0) {
        estim_cover[isuc] <- NA
      } else {
        estim_cover[isuc] <- rSW2utils::cut0Inf(
          -1 + 1.20246 * therm_amp ^ -0.0689 * ppt.WinterToMAP ^ -0.0322,
          val = 0
        )
      }

      # 3. step:
      ngood <- sum(!is.na(estim_cover[iestim]))

      # Any remaining NAs are set to 0
      estim_cover[iestim] <- rSW2utils::replace_NAs_with_val(
        x = estim_cover[iestim],
        val_replace = 0
      )

      if (!fill_empty_with_BareGround && ngood <= 1) {
        #--- Hack if some of the equations produced NAs:
        # [these rules are made up arbitrarily by drs, Nov 2012]:
        # If no or only one successful equation, then add
        #   100% C3 if MAT < 10 C,
        #   100% shrubs if MAP < 600 mm, and
        #   100% C4 if MAT >= 10C & MAP >= 600 mm
        if (MAP_mm < 600) {
          estim_cover[ishr] <- 1 + estim_cover[ishr]
        }

        if (MAT_C < 10) {
          estim_cover[igc3] <- 1 + estim_cover[igc3]
        }

        if (MAT_C >= 10  & MAP_mm >= 600) {
          estim_cover[igc4] <- 1 + estim_cover[igc4]
        }
      }


      # 4. step: put all together:
      # 4-i) groups with set values (iset) and groups with estimable but
      #    fixed values (iestim & !is.na)
      veg_cover[ifixed] <- input_cover[ifixed]

      # 4-ii) rescale grass components to fixed total grass cover
      if (fix_sumgrasses && add_sum_grasses > 0) {
        ids_to_estim_grasses <- intersect(ids_to_estim, igrasses)
        n_to_estim_grasses <- sum(ids_to_estim_grasses)

        estim_grasses_cover_sum <- sum(estim_cover[ids_to_estim_grasses])

        if (estim_grasses_cover_sum > 0) {
          estim_cover[ids_to_estim_grasses] <-
            estim_cover[ids_to_estim_grasses] *
            add_sum_grasses / estim_grasses_cover_sum

        } else if (n_to_estim_grasses > 0) {
          # We estimated zero grass cover, but some was required
          # --> divide requested amount evenly
          estim_cover[ids_to_estim_grasses] <-
            add_sum_grasses / n_to_estim_grasses

          warning(
            "'estimate_PotNatVeg_composition': ",
            "Total grass cover set, but no grass cover estimated; ",
            "requested cover evenly divided among grass types."
          )
        }
      }

      # 4-iii) groups with values to estimate (iestim & is.na):
      veg_cover[ids_to_estim] <- estim_cover[ids_to_estim]

      if (fix_sumgrasses) {
        # Fix grasses and remove them from estimable
        ifixed <- unique(c(ifixed, igrasses))
        ids_to_estim <- setdiff(ids_to_estim, igrasses)
      }

      # Scale fractions to 0-1 with a sum equal to 1 (if needed)
      tot_veg_cover_sum <- sum(veg_cover)

      if (abs(tot_veg_cover_sum - 1) > rSW2_glovars[["tol"]]) {

        estim_cover_sum <- sum(estim_cover[ids_to_estim])

        if (estim_cover_sum > 0) {
          # Scale estimable fractions so that total sums to 1, but
          # scaling doesn't affect those that are fixed
          veg_cover[ids_to_estim] <- veg_cover[ids_to_estim] *
            (1 - sum(veg_cover[ifixed])) / estim_cover_sum

        } else {
          # cover to estimate is 0 and fixed_cover_sum < 1
          if (fill_empty_with_BareGround && !fix_BareGround) {
            # ==> fill land cover up with bare-ground
            veg_cover[ibar] <- 1 - sum(veg_cover[-ibar])

          } else {
            stop(
              "'estimate_PotNatVeg_composition': ",
              "The estimated vegetation cover values are 0, ",
              "the user fixed relative abundance values sum to less than 1, ",
              "and bare-ground is fixed. ",
              "Thus, the function cannot compute ",
              "complete land cover composition."
            )
          }
        }
      }

    }
  }

  names(veg_cover) <- veg_types

  # Scale relative grass components to one (or set to 0)
  c3c4ann <- veg_cover[igrasses]
  grass_fraction <- sum(c3c4ann)

  if (grass_fraction > 0) {
    c3c4ann <- c3c4ann / grass_fraction
  }

  # Return values
  temp <- unname(veg_cover)

  list(
    # Full resolution: suitable for STEPWAT2
    Rel_Abundance_L0 = veg_cover,

    # SOILWAT2 land cover types:
    Rel_Abundance_L1 = c(
      SW_TREES = temp[itre],
      SW_SHRUB = temp[ishr],
      SW_FORBS = temp[ifor] + temp[isuc],
      SW_GRASS = grass_fraction,
      SW_BAREGROUND = temp[ibar]
    ),

    # Relative contributions of sub-types to the grass type
    Grasses = c3c4ann
  )
}




#' Adjust a phenological pattern to a different temperature regime
#'
#' Extract the characteristics of seasonal (mean monthly) values of biomass,
#' activity, etc. (i.e., phenology) that were observed for a specific
#' (reference) climate (at a specific site), and project those characteristics
#' to the mean monthly temperature values of a different site and/or different
#' climate conditions.
#'
#' @param x A numeric vector of length 12. The values
#'   reflect the phenological pattern that occur on average under
#'   reference mean monthly temperature values \code{ref_temp}.
#' @param ref_temp A numeric vector of length 12. Reference mean monthly
#'   temperature values in degree Celsius under which \code{x} was
#'   determined / is valid.
#' @param target_temp A numeric vector of length 12. Mean monthly
#'   temperature values in degree Celsius of a target site / condition
#'   for which \code{x} is to be adjusted.
#' @param x_asif A numeric vector of length 12. If not \code{NULL}, then
#'   the adjustments applied to \code{x} are based on values of \code{x_asif}.
#'
#' @return A copy of \code{x} with values that have been adjusted
#'   to represent the phenology of a target climate described by
#'   \code{target_temp}.
#'
#' @section Details: The implementation takes a geometrical interpretation to
#' the seasonal cycle.
#' The time axis is represented by a standardized twelve month circle;
#' temperature and vegetation values are standardized to lie within a band along
#' the time circle;
#' and warm- and cold-seasons are presumed to exist (i.e., temperate
#' climates will work best) and are represented by ellipses with centers at the
#' time of peak of temperature values (warm-season) below the standardized band
#' of values and half a year earlier (cold-season) above that band.
#' From these centers, radial expansion or contraction rates are calculated
#' as vectors from reference towards target temperatures and these vectors
#' are applied as adjustment rates to vegetation values.
#' Adjusted values are destandardized before returning.
#'
#' @section Details: The purpose of \code{x_asif} is to adjust \code{x} based
#'   on the phenological pattern of \code{x_asif} instead of \code{x}. This
#'   may be useful, for instance, if \code{x} represents total biomass and
#'   \code{x_asif} fraction of live biomass and if we want the adjustments to
#'   reflect that the live biomass fraction predominantly influences the
#'   total biomass response to different temperatures.
#' @section Details: Adjustments based on \code{x_asif} reflect the
#'   amount of variation in \code{x} relative to the amount of variation in
#'   \code{x_asif}. Thus, the less variation in \code{x} relative to
#'   \code{x_asif}, the less \code{x} is adjusted; and if \code{x} does not
#'   vary across months, then \code{x} is not adjusted at all regardless of the
#'   values of \code{x_asif}. This may not be desired in all cases.
#'
#' @section Notes: Phenological adjustments applied to \code{x} will not
#'   necessarily maintain any specific characteristic of \code{x}, e.g.,
#'   values in \code{[0, 1]}, \code{sum(x) == 1}, \code{max(x) == peak}.
#'   Thus, post-adjustment scaling of the returned values may be required. See
#'   examples.
#'
#'
#' @examples
#' sw_in <- rSOILWAT2::sw_exampleData
#' tmp <- swProd_MonProd_grass(sw_in)
#' phen_reference <- data.frame(
#'   tmp,
#'   Litter_pct = tmp[, "Litter"] / max(tmp[, "Litter"]),
#'   Biomass_pct = tmp2 <- tmp[, "Biomass"] / max(tmp[, "Biomass"]),
#'   Biomass2_pct = {tmp2[6:8] <- 1; tmp2}
#' )
#'
#' clim <- calc_SiteClimate(weatherList = rSOILWAT2::weatherData)
#' ref_temp <- clim[["meanMonthlyTempC"]]
#' temp_randomwarmer3C <- ref_temp + stats::rnorm(12, 3, 1)
#'
#' phen_adj <- sapply(
#'   phen_reference,
#'   adj_phenology_by_temp,
#'   ref_temp = ref_temp,
#'   target_temp = temp_randomwarmer3C,
#'   x_asif = phen_reference[, "Live_pct"]
#' )
#'
#' ## Note: depending on what `x` represents, post-adjustment scaling may
#' ## be necessary
#'
#' # Maintain previous peak
#' rSW2utils::scale_to_reference_fun(
#'   x = phen_adj[, "Litter_pct"],
#'   x_ref = phen_reference[, "Litter_pct"],
#'   fun = max
#' )
#'
#' # Maintain previous sum
#' rSW2utils::scale_to_reference_fun(
#'   x = phen_adj[, "Biomass"],
#'   x_ref = phen_reference[, "Biomass"],
#'   fun = sum
#' )
#'
#' # Maintain previous frequency of peaks and cap the peaks at that value
#' rSW2utils::scale_to_reference_peak_frequency(
#'   x = phen_adj[, "Biomass2_pct"],
#'   x_ref = phen_reference[, "Biomass2_pct"],
#'   cap_at_peak = TRUE
#' )
#'
#' ## Plot reference and adjusted monthly values
#' par_prev <- par(mfrow = c(2, 1))
#'
#' plot(
#'   1:12,
#'   phen_reference[, 1],
#'   type = "l",
#'   col = "red",
#'   ylim = c(0, 300),
#'   xlab = "Month"
#' )
#' lines(1:12, phen_adj[, 1])
#' legend(
#'   "bottom",
#'   ncol = 2,
#'   legend = c("reference", "adjusted"),
#'   lwd = 2, lty = 1, col = c("red", "black")
#' )
#'
#' plot(
#'   1:12,
#'   phen_reference[, 2],
#'   type = "l",
#'   col = "red",
#'   ylim = c(0, 1),
#'   xlab = "Month"
#' )
#' lines(1:12, phen_adj[, 2])
#'
#' par(par_prev)
#'
#' @export
adj_phenology_by_temp <- function(x, ref_temp, target_temp, x_asif = NULL) {

  mon <- rSW2_glovars[["st_mo"]]
  nmon <- 12 # number of months `mon`
  nwks <- 53

  tol <- rSW2_glovars[["tol"]]
  tol2 <- sqrt(tol)

  if (is.null(x_asif)) {
    x_asif <- x
  }

  stopifnot(
    length(x) == nmon,
    length(x_asif) == nmon,
    length(ref_temp) == nmon,
    length(target_temp) == nmon
  )

  #--- Free parameters
  Nr <- 2 * nwks # number of steps for adjustments (1 full circle)
  Ninfl <- 19 #17 # number of `nwks` units of seasonal influence
  p_voffset <- 1 / 4 # vertical offset of season centers beyond 0-0.5 values


  #------ Step 1) Define cold- and warm-season circles/ellipses
  yr_std_d <- 1 # duration of 1 year = diameter
  yr_std_r <- yr_std_d / 2 # radius of year
  yr_ctrs <- c(0, yr_std_r) # time (x-axis) centers of cold-/warm-seasons
  vals_std <- c(0, yr_std_r) # y-axis band of cold-/warm-season activity
  vals_std_off <- vals_std[2] * p_voffset # y-axis offset of season centers

  infl_warm <- Ninfl / nwks
  infl_cold <- Ninfl / nwks


  # Cold-season ellipse with center (0, 1), radii 0.5/1,
  # anti-clockwise direction, and start at (-0.5, -0.5)
  ctr_cold <- c(yr_ctrs[1], max(vals_std) + vals_std_off)
  fun_cold <- rSW2utils::f_ellipse(
    x0 = ctr_cold[1],
    y0 = ctr_cold[2],
    a = yr_std_r,
    b = vals_std[2] + vals_std_off,
    theta0 = pi
  )

  # Warm-season ellipse with center (0.5, -0.5), radii 0.5/1,
  # anti-clockwise direction, and start at (0, 0)
  ctr_warm <- c(yr_ctrs[2], min(vals_std) - vals_std_off)
  fun_warm <- rSW2utils::f_ellipse(
    x0 = ctr_warm[1],
    y0 = ctr_warm[2],
    a = yr_std_r,
    b = vals_std[2] + vals_std_off,
    theta0 = pi,
    dir = -1
  )


  # Identify which points fall to the influence of cold- vs. warm-ellipses
  # (with some overlap)
  xrange_warm <- ctr_warm[1] + c(-1, 1) * infl_warm
  xrange_warm2 <- ctr_warm[1] + c(-1, 1) * infl_warm / 2
  xrange_cold <- ctr_cold[1] + c(-1, 1) * infl_cold
  xrange_cold2 <- ctr_cold[1] + c(-1, 1) * infl_cold / 2

  # Season sequences
  #   - must be continuous for "continuous adjustment sections"
  #   - make sure season ellipses' vertices are included in sequence
  #   - increase density of points near season ellipses' vertices
  #     (where uniroot tends to fail and vegetation curve often changes fast)
  tmp <- pi / 2
  seq_angle1 <- sort(unique(c(
    c(-1, 1) * tmp,
    seq_len(Nr) / Nr * 2 * pi
  )))
  seq_angle2 <- seq_len(2 * Nr) / Nr * pi

  seq_cold1 <- fun_cold(seq_angle1)
  ids_cold1 <- which(
    seq_cold1[, 2] <= ctr_cold[2] &
      seq_cold1[, 1] >= xrange_cold[1] &
      seq_cold1[, 1] <= xrange_cold[2]
  )
  seq_cold2 <- fun_cold(seq_angle2)
  ids_cold2 <- which(
    seq_cold2[, 2] <= ctr_cold[2] &
      seq_cold2[, 1] >= xrange_cold2[1] &
      seq_cold2[, 1] <= xrange_cold2[2]
  )
  seq_cold <- unique(rbind(seq_cold1[ids_cold1, ], seq_cold2[ids_cold2, ]))
  seq_cold <- seq_cold[order(seq_cold[, 1]), ]

  seq_warm1 <- fun_warm(seq_angle1)
  ids_warm1 <- which(
    seq_warm1[, 2] >= ctr_warm[2] &
      seq_warm1[, 1] >= xrange_warm[1] &
      seq_warm1[, 1] <= xrange_warm[2]
  )
  seq_warm2 <- fun_warm(seq_angle2)
  ids_warm2 <- which(
    seq_warm2[, 2] >= ctr_warm[2] &
      seq_warm2[, 1] >= xrange_warm2[1] &
      seq_warm2[, 1] <= xrange_warm2[2]
  )
  seq_warm <- unique(rbind(seq_warm1[ids_warm1, ], seq_warm2[ids_warm2, ]))
  seq_warm <- seq_warm[order(seq_warm[, 1]), ]

  n_cold <- nrow(seq_cold)
  n_warm <- nrow(seq_warm)
  Nadj <- n_cold + n_warm


  # Information about the points that define the lines which are used
  # to intersect values for adjustments
  # columns:
  #   1:2 = x/y-coords of center of cold-/warm-season ellipses
  #   3:4 = x/y-coords of points along cold-/warm-season ellipses
  #   5:6 = min/max of x-coord under influence of cold-/warm-season ellipses
  #   7 = x-extent of influence around center of cold-/warm-season ellipses
  ptadj <- matrix(NA, nrow = Nadj, ncol = 7)

  tmp <- seq_len(n_cold)
  ptadj[tmp, 1:2] <- rep(ctr_cold, each = n_cold)
  ptadj[tmp, 3:4] <- seq_cold
  ptadj[tmp, 5:6] <- rep(xrange_cold, each = n_cold)
  ptadj[tmp, 7] <- infl_cold

  tmp <- n_cold + seq_len(n_warm)
  ptadj[tmp, 1:2] <- rep(ctr_warm, each = n_warm)
  ptadj[tmp, 3:4] <- seq_warm
  ptadj[tmp, 5:6] <- rep(xrange_warm, each = n_warm)
  ptadj[tmp, 7] <- infl_warm


  #------ Step 2) Normalize (shift and scale) all values
  # so that they fit into the season ellipses

  # Normalize time of year values (on x-axis) into [0, 1]
  mon_norm <- mon / nmon

  # Normalize temperature values (on y-axis) into [0, 0.5]
  temp_zero <- -min(ref_temp, target_temp)
  tmp1 <- temp_zero + ref_temp
  tmp2 <- temp_zero + target_temp
  temp_2max <- max(tmp1, tmp2) / vals_std[2]

  ref_temp_norm <- tmp1 / temp_2max
  ref_temp_norm_mean <- mean(ref_temp_norm)  # nolint
  target_temp_norm <- tmp2 / temp_2max  # nolint

  # Normalize vegetation values (on y-axis) into [0, 0.5] so that mean = 0.25
  veg_min <- c(0, 0)
  veg_2max <- (c(max(x), max(x_asif)) - veg_min) / vals_std[2]
  veg_4mean <- (c(mean(x), mean(x_asif)) - veg_min) / (vals_std[2] / 2)
  veg_scale <- pmax(veg_2max, veg_4mean)
  veg_norm <- (x - veg_min[1]) / veg_scale[1] # nolint
  veg_asif_norm <- (x_asif - veg_min[2]) / veg_scale[2]  # nolint

  # Calculate factor to correct for different spreads between
  # vegetation data and `x_asif`
  tmp1 <- diff(range(x_asif))
  if (abs(tmp1) < rSW2_glovars[["tol"]]) {
    # if diff(range(x_asif)) -> 0,
    # then max(x_asif) / diff(range(x_asif)) -> Inf
    # ==> cap ratio at one to avoid overcorrection
    cvtoasif <- 1

  } else {
    # if diff(range(x)) -> 0, then diff(range(x)) / max(x) -> 0
    tmp2 <- max(x)
    if (abs(tmp2) < rSW2_glovars[["tol"]]) {
      cvtoasif <- 0

    } else {
      # cap at one to avoid overcorrection
      cvtoasif <- rSW2utils::finite01(
        diff(range(x)) / tmp2 * max(x_asif) / tmp1
      )
    }
  }


  # Shift temperature and vegetation values (on x-axis)
  # so that their ellipses are centered on the warm- and cold-season centers
  # i.e., shift time of year so that peaks lie at warm-season x-center of 0.5

  # Original centers = time of peak season as mean month weighted by values
  tmp <- order(ref_temp_norm, decreasing = TRUE)
  tpeak_norm <- rSW2utils::circ_mean_weighted(
    x = tmp,
    w = ref_temp_norm[tmp],
    int = nmon,
    type = "ZeroPlus2Pi"
  ) / nmon

  sx <- yr_std_r - tpeak_norm
  mon_norm_center <- mon_norm + sx  # nolint



  #------ Step 2) Fit periodic cubic splines to normalized values
  psps <- list(
    t_ref = splines::periodicSpline(
      ref_temp_norm ~ mon_norm_center,
      period = yr_std_d
    ),

    t_target = splines::periodicSpline(
      target_temp_norm ~ mon_norm_center,
      period = yr_std_d
    ),

    veg = splines::periodicSpline(
      veg_norm ~ mon_norm_center,
      period = yr_std_d
    ),

    veg_asif = splines::periodicSpline(
      veg_asif_norm ~ mon_norm_center,
      period = yr_std_d
    )
  )

  itcurves <- 1:2
  ivcurves <- 3:4


  #------ Step 3) Correct vegetation
  # based on radial temperature expansion/contraction

  # Prepare container for adjusted (normalized) vegetation points (xy)
  vadj <- vector("list", length = Nadj)
  k_vadj <- 1
  ksc <- 1 # counter of continous adjustment sections
  kce <- c(1, 0, 0) # counter of continuous failure sections

  # Loop through points along season-ellipses (`ptadj`)
  for (k in seq_len(Nadj)) {

    #--- Locate intersection between target temperature function and
    # line connecting points (time of year, reference temperature function) and
    # season center points
    if (abs(ptadj[k, 3] - ptadj[k, 1]) > rSW2_glovars[["tol"]]) {

      # Function describing line that is connecting the circle-center point
      # with the k-th point on the season-circle
      fline_radial <- rSW2utils::f_2pline(
        x0 = ptadj[k, 1], y0 = ptadj[k, 2],
        x1 = ptadj[k, 3], y1 = ptadj[k, 4]
      )

      # Search limits for locating intersections
      if (ptadj[k, 3] > ptadj[k, 1]) {
        # limits: ellipse center to max influence
        olims <- c(ptadj[k, 1], ptadj[k, 6])
      } else {
        # limits: min influence to ellipse center
        olims <- c(ptadj[k, 5], ptadj[k, 1])
      }
      olims <- rSW2utils::extend_range(olims, 1 + ptadj[k, 7] / 2)

      # Locate the x-axis value where line crosses
      #   (i) normalized reference temperature curve
      #   (ii) normalized target temperature curve
      xcrosses_tcurves <- lapply(
        X = itcurves,
        FUN = function(itp) {
          rSW2utils::uniroots(
            f = function(x) {
              predict(psps[[itp]], x)[["y"]] - fline_radial(x)[, 2]
            },
            xlim = olims,
            tol = tol,
            expected_nroots = 1
          )
        }
      )

      # Success of detection of temperature curves intersections
      good_tcurves <- sapply(
        xcrosses_tcurves,
        rSW2utils::has_uniroots,
        tol = tol2
      )

      if (all(good_tcurves)) {
        # Locate (all) x-axis values where line crosses
        #   (iii) normalized vegetation curves
        xcrosses_vcurves <- lapply(
          X = ivcurves,
          FUN = function(ivp) {
            rSW2utils::uniroots(
              f = function(x) {
                predict(psps[[ivp]], x)[["y"]] - fline_radial(x)[, 2]
              },
              xlim = olims,
              tol = tol,
              expected_nroots = 100
            )
          }
        )

        # Success of detection of vegetation curve intersections
        good_vcurves <- lapply(
          xcrosses_vcurves,
          rSW2utils::has_uniroots,
          tol = tol2
        )

        if (!identical(good_vcurves[[1]], good_vcurves[[2]])) {
          good_vcurves <- FALSE
        }

      } else {
        good_vcurves <- FALSE
      }

    } else {
      # Vertical line (`f_2pline` fails because slope would be inf)
      tmp <- list(list(root = ptadj[k, 3]))

      xcrosses_tcurves <- lapply(seq_along(itcurves), function(...) tmp)
      good_tcurves <- TRUE

      xcrosses_vcurves <- lapply(seq_along(ivcurves), function(...) tmp)
      good_vcurves <- lapply(seq_along(ivcurves), function(...) TRUE)
    }



    #--- Calculate and apply radial expansion/contraction
    if (all(good_tcurves) && all(sapply(good_vcurves, any))) {

      # Intersection points: reference temp, target temp
      x_tvals <- sapply(xcrosses_tcurves, function(x) x[[1]][["root"]])
      y_tvals <- sapply(
        seq_along(x_tvals),
        function(i) predict(psps[[i]], x_tvals[i])[["y"]]
      )

      # Calculate radial x- and y-expansion/contraction
      # (from ref temps to target temps)
      # as fraction of reference temps to season center
      rx <- (x_tvals[2] - x_tvals[1]) / (x_tvals[1] - ptadj[k, 1])
      rx <- if (is.finite(rx)) rx else 0
      ry <- (y_tvals[2] - y_tvals[1]) / (y_tvals[1] - ptadj[k, 2])
      ry <- if (is.finite(ry)) ry else 0

      # Intersection points: vegetation(s)
      x_vvals <- lapply(
        seq_along(ivcurves),
        function(kvc) {
          sapply(
            xcrosses_vcurves[[kvc]][good_vcurves[[kvc]]],
            function(x) x[["root"]]
          )
        }
      )
      y_vvals <- lapply(
        seq_along(ivcurves),
        function(kvc) {
          predict(psps[[ivcurves[kvc]]], x_vvals[[kvc]])[["y"]]
        }
      )

      # Calculate adjusted vegetation point(s)
      # based on `x_asif` (index 2) but applied to `x` (index 1)
      for (kv in seq_along(x_vvals[[2]])) {
        vadj[[k_vadj]] <- c(
          # xy-coordinates
          x_vvals[[1]][kv] + rx * cvtoasif * (x_vvals[[2]][kv] - ptadj[k, 1]),
          y_vvals[[1]][kv] + ry * cvtoasif * (y_vvals[[2]][kv] - ptadj[k, 2]),
          # season that adjusts
          ptadj[k, 1],
          # number of continous adjustment section
          ksc
        )

        k_vadj <- k_vadj + 1
      }

    } else {
      # discontinuation in adjustment sections
      ksc <- ksc + 1

      # count longest continuous failure section
      if (kce[1] == k - 1) {
        kce[1:2] <- c(k, kce[2] + 1)
      } else {
        if (kce[2] > kce[3]) {
          kce[3] <- kce[2]
        }
        kce[1:2] <- c(k, 1)
      }
    }
  }



  if (ksc > 0.2 * Nadj) {
    warning("More than 20% of adjustment points failed.")
  }

  if (max(kce[2:3]) > 0.1 * Nadj) {
    warning(
      "The longest contiguous section of failed adjustments ",
      "is longer than 10% of all points.")
  }


  #--- Pre-process adjusted vegetation values separately for seasons
  # (in case certain time points received both cold- and warm-season adj.)

  # Clean, sort, and separate adjusted vegetation values
  vadj <- vadj[seq_len(k_vadj - 1)]

  vadj2 <- matrix(unlist(vadj), nrow = length(vadj), ncol = 4, byrow = TRUE)
  vadj2 <- vadj2[complete.cases(vadj2), ]

  # determine duplicated values: see `smooth.spline`
  tmp <- vadj2[, 1]
  isdup <- duplicated(round((tmp - mean(tmp)) / (1e-6 * stats::IQR(tmp))))
  vadj2 <- vadj2[!isdup, ]

  vadj2 <- vadj2[order(vadj2[, 1]), , drop = FALSE]

  vadj2 <- lapply(
    c(ctr_cold[1], ctr_warm[1]),
    function(ctr) vadj2[abs(vadj2[, 3] - ctr) < tol, , drop = FALSE]
  )

  # Continous adjustment sections
  scs <- lapply(vadj2, function(x) sort(unique(x[, 4])))


  # Fit periodic linear spline to adjusted vegetation values per season
  # and predict along regular sequence (for each continuous section)
  fxpsp_adj_norm2 <- lapply(
    vadj2,
    function(x) {
      if (nrow(x) >= 4) {
        splines::interpSpline(x[, 2] ~ x[, 1])
      }
    }
  )


  itmp <- seq_len(Nadj) / Nadj - 1 / (2 * Nadj)
  vadj3 <- lapply(
    seq_along(vadj2),
    function(k2) {
      tmp <- matrix(NA, nrow = Nadj, ncol = 1 + length(scs[[k2]]))
      tmp[, 1] <- itmp
      tmp
    }
  )

  # Columns: (1) x-axis location, (2) cold-season adj. veg,
  #          (3) warm-season adj. veg, (4) across season averaged adj. veg
  vadj4 <- matrix(NA, nrow = Nadj, ncol = 4)
  vadj4[, 1] <- itmp


  # Predict seasonaly adjusted vegetation for each continous section
  for (k2 in seq_along(vadj2)) {
    if (!is.null(fxpsp_adj_norm2[[k2]])) {
      for (k3 in seq_along(scs[[k2]])) {
        ids <- vadj2[[k2]][, 4] %in% scs[[k2]][k3]
        xl <- round(Nadj * range(vadj2[[k2]][ids, 1]))
        xt2 <- seq(xl[1], xl[2])

        tmp <- predict(
          fxpsp_adj_norm2[[k2]],
          xt2 / Nadj - 1 / (2 * Nadj)
        )

        tmp <- aggregate(
          tmp[["y"]],
          by = list(tmp[["x"]] %% yr_std_d),
          FUN = mean
        )

        xt2p <- sapply(
          tmp[, 1],
          function(x) which.min(abs(x - vadj3[[k2]][, 1] %% yr_std_d))
        )

        vadj3[[k2]][xt2p, 1 + k3] <- tmp[, 2]
      }

      # Average across continous sections (within season)
      vadj4[, 1 + k2] <- apply(
        vadj3[[k2]][, -1, drop = FALSE],
        MARGIN = 1,
        FUN = mean,
        na.rm = TRUE
      )
    }
  }

  # Average values across seasons
  vadj4[, 4] <- apply(vadj4[, -1], 1, mean, na.rm = TRUE)
  isgood <- complete.cases(vadj4[, c(1, 4)])
  vadj4 <- vadj4[isgood, , drop = FALSE]


  #------ Step 4) Fit periodic linear spline to adjusted vegetation values
  # De-normalize vegetation values back to original scale
  veg_new <- veg_min[1] + veg_scale[1] * vadj4[, 4]

  # Revert temporal shifts due to centering of refs, target temps and veg
  toy_new <- (vadj4[, 1] - sx) %% yr_std_d # nolint

  if (nrow(vadj4) >= 2) {
    # Use linear (instead of cubic) spline to smooth across potential steps
    # between cold- and warm-season adjustments
    fxpsp_adj <- splines::periodicSpline(
      veg_new ~ toy_new,
      period = yr_std_d,
      ord = 2 # linear spline
    )

    # Obtain monthly adjusted vegetation values
    pres <- predict(fxpsp_adj, mon_norm)[["y"]]

  } else {
    warning("Less than 3 adjustment points succeeded: return mean value.")
    pres <- rep(mean(veg_new), nmon)
  }

  pmax(0, pres)
}


#' Biomass equations
#'
#' @param MAP_mm A numeric vector. Mean annual precipitation in
#'   millimeters (mm).
#' @references Milchunas & Lauenroth 1993 (Fig. 2):
#'   Y [g/m2/yr] = c1 * MAP [mm/yr] + c2
#' @name biomass
NULL

#' Estimate shrub biomass density from mean annual precipitation
#' @export
#' @rdname biomass
Shrub_ANPP <- function(MAP_mm) 0.393 * MAP_mm - 10.2

#' Estimate grass biomass density from mean annual precipitation
#' @export
#' @rdname biomass
Grass_ANPP <- function(MAP_mm) 0.646 * MAP_mm - 102.5

#' Adjust mean monthly biomass values by precipitation
#' @section Details: Internally used by
#'   \code{\link{estimate_PotNatVeg_biomass}}.
adjBiom_by_ppt <- function(biom_shrubs, biom_C3, biom_C4, biom_annuals,
  biom_maxs, map_mm_shrubs, map_mm_std_shrubs,
  map_mm_grasses, map_mm_std_grasses,
  vegcomp_std_shrubs, vegcomp_std_grass) {

  # Intercepts to match outcomes of M & L 1993 equations under 'default'
  # MAP with our previous default inputs for shrubs and sgs-grasslands
  # Whereas these intercepts were introduced artificially, they could also be
  # interpreted as perennial storage, e.g., Lauenroth & Whitman (1977) found
  # "Accumulation in the standing dead was 63% of inputs, in the litter 8%,
  # and belowground 37%.". Lauenroth, W.K. & Whitman, W.C. (1977)
  # Dynamics of dry matter production in a mixed-grass prairie in western
  # North Dakota. Oecologia, 27, 339-351.
  Shrub_ANPPintercept <- (vegcomp_std_shrubs[1] * biom_maxs["Sh.Amount.Live"] +
    vegcomp_std_shrubs[2] * biom_maxs["C3.Amount.Live"] +
    vegcomp_std_shrubs[3] * biom_maxs["C4.Amount.Live"]) -
    Shrub_ANPP(map_mm_std_shrubs)
  Grasses_ANPPintercept <- (vegcomp_std_grass[1] * biom_maxs["Sh.Amount.Live"] +
    vegcomp_std_grass[2] * biom_maxs["C3.Amount.Live"] +
    vegcomp_std_grass[3] * biom_maxs["C4.Amount.Live"]) -
    Grass_ANPP(map_mm_std_grasses)

  # Get scaling values for scaled biomass; guarantee that > minimum.totalBiomass
  minimum.totalBiomass <- 0 # This is a SOILWAT2 parameter
  Shrub_BiomassScaler <- max(minimum.totalBiomass,
    Shrub_ANPP(map_mm_shrubs) + Shrub_ANPPintercept)
  Grass_BiomassScaler <- max(minimum.totalBiomass,
    Grass_ANPP(map_mm_grasses) + Grasses_ANPPintercept)

  # Scale live biomass amount by productivity; assumption:
  # ANPP = peak standing live biomass
  biom_shrubs$Sh.Amount.Live <- biom_shrubs$Sh.Amount.Live * Shrub_BiomassScaler
  biom_C3$C3.Amount.Live <- biom_C3$C3.Amount.Live * Grass_BiomassScaler
  biom_C4$C4.Amount.Live <- biom_C4$C4.Amount.Live * Grass_BiomassScaler
  biom_annuals$Annual.Amount.Live <-
    biom_annuals$Annual.Amount.Live * Grass_BiomassScaler

  #Scale litter amount by productivity and adjust for ratio of litter/live
  biom_shrubs$Sh.Litter <- biom_shrubs$Sh.Litter * Shrub_BiomassScaler *
    biom_maxs["Sh.Litter"] / biom_maxs["Sh.Amount.Live"]
  biom_C3$C3.Litter <- biom_C3$C3.Litter * Grass_BiomassScaler *
    biom_maxs["C3.Litter"] / biom_maxs["C3.Amount.Live"]
  biom_C4$C4.Litter <- biom_C4$C4.Litter * Grass_BiomassScaler *
    biom_maxs["C4.Litter"] / biom_maxs["C4.Amount.Live"]
  biom_annuals$Annual.Litter <- biom_annuals$Annual.Litter *
    Grass_BiomassScaler *
    biom_maxs["Annual.Litter"] / biom_maxs["Annual.Amount.Live"]

  #Guarantee that live fraction = ]0, 1]
  biom_shrubs$Sh.Perc.Live <- pmin(1, pmax(rSW2_glovars[["tol"]],
    biom_shrubs$Sh.Perc.Live))
  biom_C3$C3.Perc.Live <- pmin(1, pmax(rSW2_glovars[["tol"]],
    biom_C3$C3.Perc.Live))
  biom_C4$C4.Perc.Live <- pmin(1, pmax(rSW2_glovars[["tol"]],
    biom_C4$C4.Perc.Live))
  biom_annuals$Annual.Perc.Live <- pmin(1, pmax(rSW2_glovars[["tol"]],
    biom_annuals$Annual.Perc.Live))

  #Calculate total biomass based on scaled live biomass amount
  biom_shrubs$Sh.Biomass <- biom_shrubs$Sh.Amount.Live /
    biom_shrubs$Sh.Perc.Live
  biom_C3$C3.Biomass <- biom_C3$C3.Amount.Live / biom_C3$C3.Perc.Live
  biom_C4$C4.Biomass <- biom_C4$C4.Amount.Live / biom_C4$C4.Perc.Live
  biom_annuals$Annual.Biomass <- biom_annuals$Annual.Amount.Live /
    biom_annuals$Annual.Perc.Live

  list(biom_shrubs = biom_shrubs,
       biom_C3 = biom_C3,
       biom_C4 = biom_C4,
       biom_annuals = biom_annuals)
}


#' Adjust mean monthly biomass values of grass and shrub functional groups by
#' climate relationships
#'
#' @inheritParams adj_phenology_by_temp
#' @param MAP_mm A numeric value. Mean annual precipitation in millimeter of the
#'   location.
#' @param tr_VegBiom A data.frame with 12 rows (one for each month) and columns
#'   \code{X.Biomass}, \code{X.Amount.Live}, \code{X.Perc.Live}, and
#'   \code{X.Litter} where \code{X} are for the functional groups shrubs,
#'   \code{X = Sh}; C3-grasses, \code{X = C3}; C4-grasses, \code{X = C4}; and
#'   annuals, \code{X = Annual} containing default input values.
#'   Function default values are from Bradford et al. 2014, see
#'   \code{\link{sw2_tr_VegBiom}}.
#' @param do_adjust_phenology A logical value. If \code{TRUE} then monthly
#'   phenology is adjusted by temperature.
#' @param do_adjust_biomass A logical value. If \code{TRUE} then monthly biomass
#'   is adjusted by precipitation.
#' @param fgrass_c3c4ann A numeric vector of length 3. Relative contribution
#'   [0-1] of the C3-grasses, C4-grasses, and annuals functional groups. The sum
#'   of \code{fgrass_c3c4ann} is 1.
#'
#' @section Default inputs: \itemize{
#'   \item Shrubs are based on location \var{\sQuote{IM_USC00107648_Reynolds}}
#'     which resulted in a vegetation composition of 70 \% shrubs and 30 \%
#'     C3-grasses. Default monthly biomass values were estimated for
#'     MAP = 450 mm yr-1.
#'   \item Grasses are based on location \var{\sQuote{GP_SGSLTER}}
#'     (shortgrass steppe) which resulted in 12 \% shrubs, 22 \% C3-grasses,
#'     and 66 \% C4-grasses. Default biomass values were estimated for
#'     MAP = 340 mm yr-1.
#'  \item Mean monthly reference temperature are the median values across
#'    898 big sagebrush sites
#'    (see \url{https://github.com/DrylandEcology/rSFSTEP2/issues/195})
#' }
#'
#' @return A list with two elements \code{grass}, \code{shrub}. Each element is
#'   a matrix with 12 rows (one for each month) and columns \code{Biomass},
#'   \code{Amount.Live}, \code{Perc.Live}, and \code{Litter}.
#'
#' @seealso Function \code{\link{adjBiom_by_ppt}} is called
#'   if \code{do_adjust_biomass};
#'   function \code{\link{adj_phenology_by_temp}} is called
#'   if \code{do_adjust_phenology}.
#'
#' @references Bradford, J.B., Schlaepfer, D.R., Lauenroth, W.K. & Burke, I.C.
#'   (2014). Shifts in plant functional types have time-dependent and regionally
#'   variable impacts on dryland ecosystem water balance. J Ecol, 102,
#'   1408-1418.
#'
#' @examples
#' clim <- calc_SiteClimate(weatherList = rSOILWAT2::weatherData)
#'
#' veg_cover <- rSOILWAT2::estimate_PotNatVeg_composition(
#'   MAP_mm = 10 * clim[["MAP_cm"]],
#'   MAT_C = clim[["MAT_C"]],
#'   mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
#'   mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
#'   dailyC4vars = clim[["dailyC4vars"]]
#' )
#'
#' rSOILWAT2::estimate_PotNatVeg_biomass(
#'   target_temp = clim[["meanMonthlyTempC"]],
#'   target_MAP_mm = 10 * clim[["MAP_cm"]],
#'   do_adjust_phenology = TRUE,
#'   do_adjust_biomass = TRUE,
#'   fgrass_c3c4ann = veg_cover[["Grasses"]]
#' )
#'
#' @export
estimate_PotNatVeg_biomass <- function(
  target_temp,
  target_MAP_mm,
  ref_temp,
  tr_VegBiom = rSOILWAT2::sw2_tr_VegBiom,
  do_adjust_phenology = FALSE,
  do_adjust_biomass = FALSE,
  fgrass_c3c4ann = c(1, 0, 0)
) {

  if (missing(ref_temp) || is.null(ref_temp)) {
    # Mean monthly reference temperature
    # corresponding to default phenology values:
    # median values across 898 big sagebrush sites
    # (see https://github.com/DrylandEcology/rSFSTEP2/issues/195)
    ref_temp <- c(
      -4.6768, -2.7282, 1.8257, 6.0538, 10.696, 15.3878,
      19.7777, 18.8755, 13.7868, 7.2843, 0.4167, -4.6912
    )
  }

  if (missing(target_MAP_mm) || is.null(target_MAP_mm)) {
    target_MAP_mm <- 450
  }


  ns_VegBiom <- names(tr_VegBiom)
  tmp <- strsplit(ns_VegBiom, split = ".", fixed = TRUE)
  vtypes <- unique(sapply(tmp, function(x) x[[1]]))
  stopifnot(identical(vtypes, c("Sh", "C3", "C4", "Annual")))

  btypes <- unique(sapply(tmp, function(x) paste(x[-1], collapse = ".")))
  stopifnot(identical(btypes, c("Litter", "Biomass", "Perc.Live")))
  btypes2 <- c(btypes, "Amount.Live")

  # Default shrub biomass input is at MAP = 450 mm/yr, and default grass
  # biomass input is at MAP = 340 mm/yr

  #Default site for the grass description is SGS LTER
  StandardGrasses_MAP_mm <- 340
  StandardGrasses_VegComposition <- c(0.12, 0.22, 0.66) # shrubs, C3, and C4
  #Default site for the shrub description is Reynolds Creek, ID
  StandardShrub_MAP_mm <- 250
  StandardShrub_VegComposition <- c(0.7, 0.3, 0) # shrubs, C3, and C4

  # Scale monthly values of litter and biomass amount by column-max
  tmp <- apply(
    tr_VegBiom[, grepl("Biomass", ns_VegBiom)] *
      tr_VegBiom[, grepl("Perc.Live", ns_VegBiom)],
    MARGIN = 2,
    FUN = max
  )
  names(tmp) <- paste0(vtypes, ".Amount.Live")

  colmax <- c(
    apply(
      tr_VegBiom,
      MARGIN = 2,
      FUN = max
    ),
    tmp
  )

  # Pull vegetation types
  x0 <- lapply(
    vtypes,
    function(vt) tr_VegBiom[, grepl(vt, ns_VegBiom)]
  )
  names(x0) <- paste0("biom_", vtypes)

  # adjust phenology for mean monthly temperatures
  if (do_adjust_phenology) {
    x <- lapply(
      X = x0,
      FUN = function(vt_data) {
        sapply(
          X = vt_data,
          adj_phenology_by_temp,
          ref_temp = ref_temp,
          target_temp = target_temp,
          x_asif = vt_data[, grepl("Perc.Live", colnames(vt_data))]
        )
      }
    )

    # Scale to previous peak frequency (0-1)
    for (ns in names(x0)) {
      for (k in seq_len(ncol(x[[ns]]))) {
        x[[ns]][, k] <- rSW2utils::scale_to_reference_peak_frequency(
          x = x[[ns]][, k],
          x_ref = x0[[ns]][, k],
          cap_at_peak = FALSE
        )
      }
    }

  } else {
    x <- x0
  }

  for (vt in vtypes) {
    ns <- paste0("biom_", vt)
    ntmp <- colnames(x[[ns]])

    x[[ns]] <- as.data.frame(x[[ns]])

    # Calculate 'live biomass amount'
    x[[ns]][, paste0(vt, ".Amount.Live")] <-
      x[[ns]][, grepl("Biomass", ntmp)] *
      x[[ns]][, grepl("Perc.Live", ntmp)]

    # Scale so that 1 = previous peak (as required by `adjBiom_by_ppt`)
    for (bt in btypes2[-3]) {
      ctmp <- paste0(vt, ".", bt)
      x[[ns]][, ctmp] <- x[[ns]][, ctmp] / colmax[ctmp]
    }
  }



  # if (do_adjBiom_by_ppt) then adjust biomass amounts by productivity
  # relationship with MAP
  # total biomass is back calculated
  x <- adjBiom_by_ppt(
    biom_shrubs = x[["biom_Sh"]],
    biom_C3 = x[["biom_C3"]],
    biom_C4 = x[["biom_C4"]],
    biom_annuals = x[["biom_Annual"]],
    biom_maxs = colmax,
    map_mm_shrubs = if (do_adjust_biomass) {
      target_MAP_mm
    } else {
      StandardShrub_MAP_mm
    },
    map_mm_std_shrubs = StandardShrub_MAP_mm,
    map_mm_grasses = if (do_adjust_biomass) {
      target_MAP_mm
    } else {
      StandardGrasses_MAP_mm
    },
    map_mm_std_grasses = StandardGrasses_MAP_mm,
    vegcomp_std_shrubs = StandardShrub_VegComposition,
    vegcomp_std_grass = StandardGrasses_VegComposition
  )


  biom_grasses <-
    x[["biom_C3"]] * fgrass_c3c4ann[1] +
    x[["biom_C4"]] * fgrass_c3c4ann[2] +
    x[["biom_annuals"]] * fgrass_c3c4ann[3]

  dimnames(biom_grasses)[[2]] <- btypes2

  biom_shrubs <- x[["biom_shrubs"]]
  dimnames(biom_shrubs)[[2]] <- btypes2

  list(
    grass = as.matrix(biom_grasses),
    shrub = as.matrix(biom_shrubs)
  )
}



#' Lookup transpiration coefficients
#'
#' Lookup transpiration coefficients for grasses, shrubs, and trees per soil
#' layer or per soil depth increment of 1 cm per distribution type for
#' each simulation run and copy values to \var{\sQuote{datafile.soils}}
#'
#' \itemize{
#'    \item first row of datafile is label for per soil layer
#'      \var{\dQuote{Layer}} or per soil depth increment of 1 cm
#'      \var{\dQuote{DepthCM}}
#'    \item second row of datafile is source of data
#'    \item the other rows contain the data for each distribution type = columns
#' }
#' @section Note: cannot write data from \var{\sQuote{sw_input_soils}} to
#'   \var{\sQuote{datafile.soils}}
#' @export
TranspCoeffByVegType <- function(tr_input_code, tr_input_coeff,
  soillayer_no, trco_type, layers_depth,
  adjustType = c("positive", "inverse", "allToLast")) {

  #extract data from table by category
  trco.code <- as.character(tr_input_code[,
    which(colnames(tr_input_code) == trco_type)])
  trco <- rep(0, times = soillayer_no)
  trco.raw <- na.omit(tr_input_coeff[,
    which(colnames(tr_input_coeff) == trco_type)])

  if (trco.code == "DepthCM") {
    temp <- sum(trco.raw, na.rm = TRUE)
    trco_sum <- if (temp == 0 || is.na(temp)) 1L else temp
    lup <- 1
    for (l in 1:soillayer_no) {
      llow <- as.numeric(layers_depth[l])
      if (is.na(llow) | lup > length(trco.raw)) {
        l <- l - 1
        break
      }
      trco[l] <- sum(trco.raw[lup:llow], na.rm = TRUE) / trco_sum
      lup <- llow + 1
    }
    usel <- l

  } else if (trco.code == "Layer") {
    usel <- if (length(trco.raw) < soillayer_no) {
        length(trco.raw)
      } else soillayer_no

    ltemp <- seq_len(usel)
    stemp <- sum(trco.raw[ltemp], na.rm = TRUE)
    temp <- if (stemp == 0 && is.na(stemp)) 1 else stemp
    trco[ltemp] <- trco.raw[ltemp] / temp
  }

  if (identical(adjustType, "positive")) {
    # equivalent to: trco + (1 - sum(trco)) * trco / sum(trco)
    trco <- trco / sum(trco)

  } else if (identical(adjustType, "inverse")) {
    irows <- 1:max(which(trco > 0))
    # equivalent to: trco + (1 - sum(trco)) * rev(trco) / sum(trco)
    trco[irows] <- trco[irows] + rev(trco[irows]) * (1 / sum(trco[irows]) - 1)

  } else if (identical(adjustType, "allToLast")) {
    irow <- max(which(trco > 0))
    if (irow > 1) {
      # adding all the missing roots because soil is too shallow to the
      # deepest available layer
      trco[irow] <- 1 - sum(trco[1:(irow - 1)])
    } else {
      trco[1] <- 1
    }
  }

  trco
}



#' Calculate rooting profile for a soil profile from \var{lookup} tables
#'
#' @param layers_depth A numeric vector. Values describe
#'   the lower soil layer depths [cm].
#' @param trco_type_by_veg A named list of character strings.
#'   The rooting profiles, i.e., column names in the \var{lookup} table,
#'   for each vegetation type. A \code{NA} indicates that a rooting profile
#'   is not calculated for that vegetation type.
#'   The values will be passed to argument \var{trco_type} of function
#'   \code{\link{TranspCoeffByVegType}}.
#' @param trco_adj_by_veg A named list of character strings.
#'   The type of adjustment from the full rooting profile
#'   to the \code{layers_depth}.
#'   The values will be passed to argument \var{adjustType} of function
#'   \code{\link{TranspCoeffByVegType}}.
#' @param fgrass_c3c4ann A named, numeric vector of length 3.
#'   Relative contribution [0-1] of the C3-grasses, C4-grasses, and
#'   annuals functional groups. The sum of \code{fgrass_c3c4ann} is 1.
#' @param trco_table A named list with two elements. Default values
#'   are taken from \code{\link{sw2_trco_table}}.
#'
#' @seealso \code{\link{TranspCoeffByVegType}}
#'
#' @examples
#' sw_in <- rSOILWAT2::sw_exampleData
#'
#' clim <- calc_SiteClimate(weatherList = rSOILWAT2::weatherData)
#'
#' veg_cover <- rSOILWAT2::estimate_PotNatVeg_composition(
#'   MAP_mm = 10 * clim[["MAP_cm"]],
#'   MAT_C = clim[["MAT_C"]],
#'   mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
#'   mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
#'   dailyC4vars = clim[["dailyC4vars"]]
#' )
#'
#' estimate_PotNatVeg_roots(
#'   layers_depth = c(5, 10, 20, 30, 40, 50, 100, 200),
#'   fgrass_c3c4ann = veg_cover[["Grasses"]]
#' )
#'
#' @export
estimate_PotNatVeg_roots <- function(
  layers_depth,
  trco_type_by_veg = list(
    grass_C3 = "SchenkJackson2003_PCdry_grasses",
    grass_C4 = "SchenkJackson2003_PCdry_grasses",
    grass_annuals = "Jacksonetal1996_crops",
    shrub = "SchenkJackson2003_PCdry_shrubs",
    forb = "SchenkJackson2003_PCdry_forbs",
    tree = "Bradfordetal2014_LodgepolePine"
  ),
  trco_adj_by_veg = list(
    grass_C3 = "positive",
    grass_C4 = "positive",
    grass_annuals = "positive",
    shrub = "positive",
    forb = "positive",
    tree = "positive"
  ),
  fgrass_c3c4ann = c(grass_C3 = NA, grass_C4 = NA, grass_annuals = NA),
  trco_table = rSOILWAT2::sw2_trco_table
) {
  n_slyrs <- length(layers_depth)
  veg_types <- c("Grass", "Shrub", "Tree", "Forb")

  res_trco <- array(
    data = NA,
    dim = c(n_slyrs, length(veg_types)),
    dimnames = list(NULL, veg_types)
  )

  for (k1 in seq_along(veg_types)) {
    tmp_type <- sort(grep(
      veg_types[k1],
      names(trco_type_by_veg),
      ignore.case = TRUE,
      value = TRUE
    ))

    tmp_adj <- sort(grep(
      veg_types[k1],
      names(trco_adj_by_veg),
      ignore.case = TRUE,
      value = TRUE
    ))

    if (!all(tmp_type == tmp_adj)) {
      stop(
        "Names of `trco_type_by_veg` do not match ",
        "`trco_adj_by_veg` for ", shQuote(veg_types[k1])
      )
    }

    if (
      isTRUE(!anyNA(unlist(trco_type_by_veg[tmp_type]))) &&
      isTRUE(!anyNA(unlist(trco_adj_by_veg[tmp_adj])))
    ) {
      if (length(tmp_type) > 1 && veg_types[k1] == "Grass") {

        tmp1 <- sapply(
          strsplit(names(fgrass_c3c4ann), split = "_", fixed = TRUE),
          FUN = function(x) x[[2]]
        )
        tmp2 <- sapply(
          strsplit(tmp_type, split = "_", fixed = TRUE),
          FUN = function(x) x[[2]]
        )
        tmp_igfcov <- match(tolower(tmp2), tolower(tmp1), nomatch = NA)

        if (anyNA(tmp_igfcov)) {
          stop(
            "Names of `fgrass_c3c4ann` do not match ",
            "`trco_adj_by_veg` for ", shQuote(veg_types[k1])
          )
        }

        tmp_root <- rep(0, n_slyrs)
        for (k2 in seq_along(tmp_type)) {
          if (isTRUE(fgrass_c3c4ann[tmp_igfcov[k2]] > 0)) {
            tmp <- TranspCoeffByVegType(
              tr_input_code = trco_table[["desc"]],
              tr_input_coeff = trco_table[["data"]],
              soillayer_no = n_slyrs,
              trco_type = trco_type_by_veg[[tmp_type[k2]]],
              layers_depth = layers_depth,
              adjustType = trco_adj_by_veg[[tmp_adj[k2]]]
            )

            tmp_root <- tmp_root + fgrass_c3c4ann[tmp_igfcov[k2]] * tmp
          }
        }


      } else if (length(tmp_type) == 1) {
        tmp_root <- TranspCoeffByVegType(
          tr_input_code = trco_table[["desc"]],
          tr_input_coeff = trco_table[["data"]],
          soillayer_no = n_slyrs,
          trco_type = trco_type_by_veg[[tmp_type]],
          layers_depth = layers_depth,
          adjustType = trco_adj_by_veg[[tmp_adj]]
        )

      } else {
        stop(
          "Root information for ",
          shQuote(veg_types[k1]),
          " incomplete."
        )
      }

      # Check values
      is_good <-
        !anyNA(tmp_root) &&
        all(tmp_root >= 0) &&
        sum(tmp_root) - 1 <= sqrt(.Machine$double.eps)

      if (!is_good) {
        warning(
          "Root information for ",
          shQuote(veg_types[k1]),
          " is problematic: ",
          paste0(round(tmp_root, 4), collapse = " / "),
          "; it was re-set to 0s."
        )

        tmp_root <- rep(0, n_slyrs)
      }

      res_trco[, veg_types[k1]] <- tmp_root

    } else {
      warning(
        "No rooting profile selected for ",
        shQuote(veg_types[k1]), "."
      )
    }
  }

  res_trco
}



#' Replace selected biomass values of a
#' \link[rSOILWAT2:swProd-class]{rSOILWAT2::swProd} object
#'
#' @param fg A character string. One of the functional groups represented by
#'  \pkg{rSOILWAT2}
#' @param use A logical vector.
#'
#' @export
update_biomass <- function(fg = c("Grass", "Shrub", "Tree", "Forb"), use,
  prod_input, prod_default) {

  fg <- match.arg(fg)

  comps <- c("_Litter", "_Biomass", "_FractionLive", "_LAIconv")
  veg_ids <- lapply(comps, function(x)
    grep(paste0(fg, x), names(use)))
  veg_incl <- lapply(veg_ids, function(x) use[x])

  temp <- swProd_MonProd_veg(prod_default, fg)
  if (any(unlist(veg_incl))) {
    for (k in seq_along(comps)) if (any(veg_incl[[k]]))
      temp[veg_incl[[k]], k] <-
        as.numeric(prod_input[, veg_ids[[k]][veg_incl[[k]]]])
  }

  temp
}


# Determine minimal number of rooted soil layers with veg > 0
get_min_rooted_soil_layers <- function(swInputData) {
  veg_comp <- swProd_Composition(swInputData)
  soils <- swSoils_Layers(swInputData)
  var_veg1 <- c("Grass", "Shrub", "Tree", "Forb")
  var_trco <- paste0("transp", var_veg1, "_frac")
  var_comp <- sapply(
    var_veg1,
    function(x) grep(x, names(veg_comp), value = TRUE)
  )

  tmp <- apply(
    soils[, var_trco, drop = FALSE],
    MARGIN = 2,
    FUN = function(x) sum(x > 0)
  )

  min(tmp[veg_comp[var_comp] > 0])
}


#' Check transpiration regions
#'
#' The transpiration regions are checked:
#' \enumerate{
#'  \item There is least one transpiration region
#'  \item All transpiration regions include at least one soil layer
#'  \item Transpiration regions are strictly increasing
#'  \item Transpiration regions go no deeper than the most shallow
#'       rooting profile of any active vegetation type
#' }
#'
#' @param swInputData A \pkg{rSOILWAT2} input object of class
#'   \code{\linkS4class{swInputData}}.
#'
#' @return A logical value.
#'
#' @examples
#' sw_in <- rSOILWAT2::sw_exampleData
#' check_TranspirationRegions(sw_in) ## Expected: TRUE
#'
#' # Make a mistake: set a transpiration region deeper than the rooting profile
#' swSite_TranspirationRegions(sw_in)[2, 2] <- 10
#' check_TranspirationRegions(sw_in) ## Expected: FALSE
#'
#' @export
check_TranspirationRegions <- function(swInputData) {
  tr <- swSite_TranspirationRegions(swInputData)

  # Checks
  nrow(tr) > 0 &&
  tr[1, 2] >= 1 &&
  tr[nrow(tr), 2] <= get_min_rooted_soil_layers(swInputData) &&
  !anyNA(
    rSW2utils::check_monotonic_increase(
      x = tr,
      MARGIN = 2,
      strictly = TRUE
    )
  )
}



#' Adjust transpiration regions for roots and soil layers
#'
#' @param swInputData A \pkg{rSOILWAT2} input object of class
#'   \code{\linkS4class{swInputData}}.
#'
#' @return A transpiration region matrix.
#'
#' @examples
#' sw_in <- rSOILWAT2::sw_exampleData
#'
#' adjust_TranspirationRegions(sw_in)
#'
#' @export
adjust_TranspirationRegions <- function(swInputData) {
  tr <- swSite_TranspirationRegions(swInputData)

  n_tr <- min(4, nrow(tr))
  tri_file <- cbind(Used_TF = 1, DeepestLayer = tr[, 2])

  # adjust maximum transpiration region for minimum soil depth and rooting depth
  n_max <- min(
    nrow(swSoils_Layers(swInputData)),
    get_min_rooted_soil_layers(swInputData)
  )

  if (max(tri_file[tri_file[, 1] > 0, 2], na.rm = TRUE) > n_max) {
    for (k in rev(seq_len(n_tr))) {
      if (tri_file[k, 1] > 0) {
        if (tri_file[k, 2] > n_max) {
          tri_file[k, 2] <- tr[k, 2] <- n_max
        }

        if (k > 1 && tri_file[k, 2] <= tri_file[k - 1, 2]) {
          tr <- matrix(tr[-k, ], ncol = 2)
        }
      }
    }
  }

  tr
}



#' Prepare transpiration regions
#'
#' Translate a soil vector of transpiration region values into a
#' transpiration region matrix.
#'
#' @param tr_lyrs An integer vector.
#'   The transpiration region for each soil layer.
#'
#' @return A transpiration region matrix.
#'
#' @seealso
#'   \code{\link{adjust_TranspirationRegions}} and
#'   \code{\link{check_TranspirationRegions}}
#'
#' @examples
#' # Example values correspond to `CONUSSOIL_BSE_EVERY10cm` of \pkg{rSFSW2}:
#' prepare_TranspirationRegions(tr_lyrs = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3))
#'
#' @export
prepare_TranspirationRegions <- function(tr_lyrs) {
  tmp <- cumsum(rle(tr_lyrs)[["lengths"]])
  cbind(ndx = seq_along(tmp), layer = tmp)
}
