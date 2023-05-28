#' Connect to a SharePoint Site
#'
#' @param site_name The name of the SharePoint site to which you wish to
#'   connect, which can be found in the site's homepage URL.
#'
#' @return An R6 object of class ms_site.
#'
#' @noRd
connect_to_sharepoint <- function(site_name = "ConnectedCommunitiesInsightsAnalysisTeam") {

  withr::local_options(list(microsoft365r_use_cli_app_id = TRUE
                            , warn = -1
                            )
                       )

  sharepoint_site <- Microsoft365R::get_sharepoint_site(
    site_url = paste0("https://aklcouncil.sharepoint.com/sites/"
                      , site_name)
    , app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46"
  )

  return(sharepoint_site)
}

#' Make a Tidy Data Frame
#'
#' @description Convert a data frame into a tibble and clean the column names.
#'
#' @param data_frame The data frame that needs to be tidied.
#'
#' @return A tibble with the same number of columns and rows as the original data drame.
#'
#' @noRd
get_tidy_table <- function(data_frame) {
  tidy_table <- data_frame |>
    dplyr::as_tibble() |>
    janitor::clean_names()

  return(tidy_table)
}

#' Get SharePoint List Values
#'
#' @param list_name The name of the SharePoint List you want to get
#'
#' @return A data frame
#'
#' @noRd
get_list_items <- function(site_name, list_name) {
  sharepoint_site <- connect_to_sharepoint(site_name)

  sharepoint_list <- sharepoint_site$get_list(list_name)

  list_item <- sharepoint_list$list_items() |>
    get_tidy_table()

  return(list_items)
}

# TODO figure out best way to read in files
# 1 function vs separate functions for each data source


#' Read Files
#'
#' @param file_name The name of the file
#' @param file_path The path of the file
#' @param file_type the format of the file
#'
#' @return data from the file(s)
#'
#' @noRd
read_file <- function(file_name
                      , file_path = tere::get_file_storage_path()
                      , file_type = c("excel", "csv", "txt")
                      , sheet = 1)
{
  if(file_type == "excel")
  {
    data <- purrr::map(
      .x = file_name
      , .f = ~tere::get_excel_file(filename = .x
                                   , path = file_path
                                   , sheet = sheet)
    ) |>
      list_rbind()
  }

  if(file_type == "csv")
  {
    data <- purrr::map(
      .x = paste0(file_path, "/", file_name, ".csv")
      , .f = ~readr::read_delim(file = .x
                                , delim = ",")
      # , .id = "file_name"
    ) |>
      list_rbind()
  }

  return(data)
}

get_file_name <- function(file_path = tere::get_file_storage_path(), folder_name)
{
  file_name <- fs::dir_ls(paste0(file_path, folder_name)) |>
    stringr::word(start = -2, end = -1, sep = stringr::fixed("/")) |>
    stringr::word(1, sep = stringr::fixed("."))

  return(file_name)
}

#' Prepare beamafilm data
#'
#' @param file_path The path of the file
#'
#' @return A dataframe containing all beamafilm data
#'
#' @noRd
prepare_beamafilm <- function(file_path = tere::get_file_storage_path())
{
  file_name_beamafilm <- get_file_name(file_path, "/beamafilm")

  data_beamafilm <- read_file(
    file_name = file_name_beamafilm
    , file_path = file_path
    , file_type = "excel"
    )

  clean_beamafilm <- data_beamafilm |>
    dplyr::mutate(sierra_record_number = "b37164934") |>
    dplyr::mutate(reporting_period = lubridate::ymd(paste0(year, month, "01"))) |>
    dplyr::mutate(month = lubridate::month(reporting_period, label = TRUE, abbr = FALSE)) |>
    dplyr::mutate(year = lubridate::year(reporting_period)) |>
    dplyr::mutate(metric_name = "views") |>
    dplyr::mutate(value = click) |>
    dplyr::select(sierra_record_number
                  , reporting_period
                  , metric_name
                  , value
                  , month
                  , year)

  return(clean_beamafilm)

}

#' Prepare discovery national archives data
#'
#' @param file_path The path of the file
#'
#' @return A dataframe containing all beamafilm data
#'
#' @noRd
prepare_discovery_national_archives <- function(file_path = tere::get_file_storage_path())
{
  file_name_discovery_national_archives <- get_file_name(file_path, "/discovery_national_archives")

  data_discovery_national_archives <- read_file(
    file_name = file_name_discovery_national_archives
    , file_path = file_path
    , file_type = "excel"
  )

  clean_discovery_national_archives <- data_discovery_national_archives |>
    dplyr::mutate(sierra_record_number = "b36127334") |>
    dplyr::mutate(reporting_period = lubridate::ymd(paste0(year, month, "01"))) |>
    dplyr::mutate(month = lubridate::month(reporting_period, label = TRUE, abbr = FALSE)) |>
    dplyr::mutate(year = lubridate::year(reporting_period)) |>
    dplyr::mutate(metric_name = "Views") |>
    dplyr::mutate(value = click) |>
    dplyr::mutate(sierra_record_number
                  , reporting_period
                  , metric_name
                  , value
                  , month
                  , year)

  return(clean_discovery_national_archives)
}
