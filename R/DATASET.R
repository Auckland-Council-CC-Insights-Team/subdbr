
save_excel_file <- function(dir_name, test_data, file_name)
{

  file_path <- paste0(testthat::test_path("testdata")
                      , "/"
                      , dir_name)

  dir.create(file_path)

  writexl::write_xlsx(test_data, path = file_path)

  data <- readxl::read_excel(paste0(file_path,"/", file_name))

  return(data)
}

beamafilm <- dplyr::tibble(
  "Customer" = rep("Auckland Council", 12)
  , "Resource" = rep("Beamafilm", 12)
  , "Year" = rep("2022", 12)
  , "Month" = month.name
  , "Click" = round(runif(12, min = 1000, max = 20000))
  , "Renewal Date" = as.Date("2021-02-28")
)

linked_in_learning <- dplyr::tibble(
  "Start Day" = "2022-02-28"
  , "End Day" = "2022-03-31"
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

# TODO figure out where to put this function
# writexl::write_excel(beamafilm, test_path("testdata", "beamafilm.xlsx"))

test_data_excel <- list(beamafilm)

test_data_csv <- list(linked_in_learning)

dir_name_excel <- c("beamafilm")

#TODO csv

file_name_excel <- c("beamafilm")

file_name_csv <- c("linked_in_learning")

# file_path_csv <-

purrr::pmap(
  list(
    dir_name_excel,
    test_data_excel,
    file_name_excel
  )
  , ~save_excel_file(..1 = dir_name_excel, ..2 = test_data_excel, ..3 = file_name_excel)
  )

# purrr::walk2(
#   test_data_csv
#   , file_name_csv
#   , ~readr::write_csv(x = .x, file = .y))
