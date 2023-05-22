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

test_that("able to get file name", {
  file_path <- test_path("testdata")
  folder_name <- "/beamafilm"

  expect_equal(
    get_file_name(file_path, folder_name) ,"beamafilm/beamafilm"
  )
})

test_that("able to complete ETL process for beamafilm", {
  file_path <-test_path("testdata")

  expect_equal(
    prepare_beamafilm(file_path) |>
      nrow()
    , 12
  )
})
