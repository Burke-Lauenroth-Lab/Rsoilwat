#!/usr/bin/env Rscript

#--- rSOILWAT2: use development version
library("methods")  # in case this code is run via 'Rscript'
stopifnot(requireNamespace("pkgbuild"))
stopifnot(requireNamespace("pkgload"))
stopifnot(requireNamespace("usethis"))

pkgbuild::clean_dll()
pkgload::load_all()


dir_definf <- file.path("data-raw", "data")


#--- Vegetation biomass
sw2_tr_VegBiom <- utils::read.csv(
  file = file.path(dir_definf, "Vegetation_MeanMonthly_v5.csv"),
  skip = 1,
  row.names = 1,
  stringsAsFactors = FALSE
)

usethis::use_data(sw2_tr_VegBiom, internal = FALSE, overwrite = TRUE)


#--- Vegetation rooting profile
tmp_trco_desc <- utils::read.csv(
  file = file.path(dir_definf, "TranspirationCoefs_v2.csv"),
  nrows = 1,
  stringsAsFactors = FALSE
)

tmp_trco_data <- utils::read.csv(
  file = file.path(dir_definf, "TranspirationCoefs_v2.csv"),
  skip = 2,
  stringsAsFactors = FALSE
)
colnames(tmp_trco_data) <- colnames(tmp_trco_desc)

sw2_trco_table <- list(
  desc = tmp_trco_desc,
  data = tmp_trco_data
)

usethis::use_data(sw2_trco_table, internal = FALSE, overwrite = TRUE)




#--- Yearly atmospheric CO2 concentration values
sw2_tr_CO2a <- utils::read.csv(
  file = file.path(dir_definf, "AtmosCO2.csv"),
  stringsAsFactors = FALSE
)

usethis::use_data(sw2_tr_CO2a, internal = FALSE, overwrite = TRUE)
