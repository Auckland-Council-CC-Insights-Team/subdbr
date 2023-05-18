test_that("files are readable", {
  file_path <- test_path("testdata")
  file_name = "test_file"

  expect_equal(
    read_file(file_name, file_path, file_format = "excel") |>
      nrow()
    , 37
    )

  expect_equal(
    read_file(file_name, file_path, file_format = "csv") |>
      nrow()
    , 37
  )

})

test_that("able to complete ETL process for beamafilm", {
  file_path <-test_path("R")
  file_name = "DATASET.R"

  expect_equal(
    prepare_beamafilm() |>
      nrow()
    , 12
  )
})
