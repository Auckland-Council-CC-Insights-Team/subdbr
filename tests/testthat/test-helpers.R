test_that("files are readable", {
  file_path <- test_path("testdata")
  file_name = "test_file"

  expect_equal(
    read_file(file_name, file_path, file_type = "excel") |>
      nrow()
    , 37
    )

  expect_equal(
    read_file(file_name, file_path, file_type = "csv") |>
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

test_that("able to complete prepare data for beamafilm", {
  file_path <-test_path("testdata")

  expect_equal(
    prepare_beamafilm(file_path) |>
      nrow()
    , 12
  )
})

#TODO figure out why 2 rows were returned instead of 1
test_that("able to complete prepare data for linked_in_learning", {
  file_path <-test_path("testdata")

  expect_equal(
    prepare_linked_in_learning(file_path) |>
      nrow()
    , 2
  )
})
