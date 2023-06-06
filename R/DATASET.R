
# create file directory and file

save_file_excel <- function(test_data, file_name)
{
  file_path <- paste0(testthat::test_path("testdata")
                      , "/"
                      , file_name)

  dir.create(file_path)

  writexl::write_xlsx(test_data, path = paste0(file_path, "/", file_name, ".xlsx"))

  data <- readxl::read_excel(paste0(file_path,"/", file_name, ".xlsx"))

  return(data)
}

save_file_csv <- function(test_data, file_name)
{
  file_path <- paste0(testthat::test_path("testdata")
                      , "/"
                      , file_name)

  dir.create(file_path)

  readr::write_csv(test_data, path = paste0(file_path, "/", file_name, ".csv"))

  data <- readr::read_csv(paste0(file_path, "/", file_name, ".csv"))

  return(data)
}

# create test data frames

beamafilm <- dplyr::tibble(
  "Customer" = rep("Auckland Council", 12)
  , "Resource" = rep("Beamafilm", 12)
  , "Year" = rep("2022", 12)
  , "Month" = month.name
  , "Click" = round(runif(12, min = 1000, max = 20000))
  , "Renewal Date" = as.Date("2021-02-28")
)


linked_in_learning <- dplyr::tibble(
  "Start Day (PST/PDT)" = "2022-02-28"
  , "End Day (PST/PDT)" = "2022-03-31"
  , "People Logged In" = 1
  , "Unique Viewers" = 2
  , "Hours Viewed" = 3
  , "Course Views" = 4
  , "Course Completions" = 5
  , "Video Views" = 6
  , "Video Completions" = 7
  , "Learning Path Views" = 8
  , "Learning Path Completions" = 9
  , "Articles Views" = 10
  , "Article Completions" = 11
  , "Document Views" = 12
  , "Document Completions" = 13
)

# assign data frames to a list

test_data_excel <- list(beamafilm)

test_data_csv <- list(linked_in_learning)

# assign files to a vector

file_name_excel <- c("beamafilm")

file_name_csv <- c("linked_in_learning")

#-

purrr::map2(
  .x = test_data_excel,
  .y = file_name_excel,
  .f = save_file_excel
)

purrr::map2(
  .x = test_data_csv,
  .y = file_name_csv,
  .f = save_file_csv
)
