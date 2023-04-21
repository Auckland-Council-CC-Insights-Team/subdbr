# excel_file <- readxl::read_excel("data-raw/test_file_xlsx.xlsx")
# csv_file <- readr::read_csv("data-raw/test_file_csv.csv")
# txt_file <- readr::read_file("data-raw/test_file_txt.txt")

excel_file <- "data-raw/test_file"
csv_file <- "data-raw/test_file"

usethis::use_data(excel_file
                  , csv_file
                  , internal = TRUE
                  , overwrite = TRUE)
