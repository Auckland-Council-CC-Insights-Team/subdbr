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


#' Read Files
#'
#' @param file_name The name of the file
#' @param file_path The path of the file
#' @param file_format the format of the file
#'
#' @return data from the file(s)
#'
#' @noRd
read_file <- function(file_name, file_path = tere::get_file_storage_path(), file_format = c("excel", "csv", "txt"))
{
  if(file_format == "excel")
  {
    data <- tere::get_excel_file(
      filename = paste0("/", file_name),
      path = file_path
      )
  }

  if(file_format == "csv")
  {
    data <- readr::read_csv(
      file = paste0(file_path, "/", file_name, ".csv"),
      col_types = "?"
      )
  }

  return(data)
}


read_beamafilm <- function()
{
  # TODO figure out how to read from sharepoint
  data_beamafilm <- read_file(file_name, file_path, file_format) |>
    janitor::clean_names

  return(data_beamafilm)
}
