beamafilm <- dplyr::tibble(
  "Customer" = rep("Auckland Council", 12)
  , "Resource" = rep("Beamafilm", 12)
  , "Year" = rep("2022", 12)
  , "Month" = month.name
  , "Click" = round(runif(12, min = 1000, max = 20000))
  , "Renewal Date" = as.Date("2021-02-28")
)


writexl::write_excel(beamafilm, test_path("testdata", "beamafilm.xlsx"))
