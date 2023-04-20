test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("files are readable", {
  file_path <- get0("excel_file", envir = asNamespace("subdbr"))
  expect_equal(
    read_file(file_path, file_format = "excel") |>
      nrow()
    , 37
    )
})
