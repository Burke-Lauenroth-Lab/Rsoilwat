context("Manipulate soil")


test_that("Manipulate soils", {
  sw_input <- rSOILWAT2::sw_exampleData

  #--- Check that high volume of gravel/coarse fragments
  # does not error out with
  #`invalid value of (theta / theta(saturated)) ^ b = 0.000000 (must be != 0)` # nolint
  # See https://github.com/DrylandEcology/SOILWAT2/issues/282
  # and https://github.com/DrylandEcology/rSOILWAT2/issues/170
  soil_data <- rSOILWAT2::swSoils_Layers(sw_input)
  soil_data[, "gravel_content"] <- 0.99
  rSOILWAT2::swSoils_Layers(sw_input) <- soil_data

  sw_out <- try(
    sw_exec(inputData = sw_input),
    silent = TRUE
  )

  # Check that run did not fail due to SWP to infinity error in SOILWAT2
  expect_s4_class(sw_out, "swOutput")
})
