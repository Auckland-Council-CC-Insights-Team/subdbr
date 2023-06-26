#' #' Connect to a SharePoint Site
#' #'
#' #' @param site_name The name of the SharePoint site to which you wish to
#' #'   connect, which can be found in the site's homepage URL.
#' #'
#' #' @return An R6 object of class ms_site.
#' #'
#' #' @noRd
#' connect_to_sharepoint <- function(site_name = "ConnectedCommunitiesInsightsAnalysisTeam") {
#'
#'   withr::local_options(list(microsoft365r_use_cli_app_id = TRUE
#'                             , warn = -1
#'                             )
#'                        )
#'
#'   sharepoint_site <- Microsoft365R::get_sharepoint_site(
#'     site_url = paste0("https://aklcouncil.sharepoint.com/sites/"
#'                       , site_name)
#'     , app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46"
#'   )
#'
#'   return(sharepoint_site)
#' }
#'
#' #' Make a Tidy Data Frame
#' #'
#' #' @description Convert a data frame into a tibble and clean the column names.
#' #'
#' #' @param data_frame The data frame that needs to be tidied.
#' #'
#' #' @return A tibble with the same number of columns and rows as the original data drame.
#' #'
#' #' @noRd
#' get_tidy_table <- function(data_frame) {
#'   tidy_table <- data_frame |>
#'     dplyr::as_tibble() |>
#'     janitor::clean_names()
#'
#'   return(tidy_table)
#' }
#'
#' #' Get SharePoint List Values
#' #'
#' #' @param site_name The name of the SharePoint site to which you wish to
#' #'   connect, which can be found in the site's homepage URL.
#' #' @param list_name The name of the SharePoint List you wish to retrieve.
#' #'
#' #' @return A data frame
#' #'
#' #' @noRd
#' get_list_items <- function(site_name, list_name) {
#'   sharepoint_site <- connect_to_sharepoint(site_name)
#'
#'   sharepoint_list <- sharepoint_site$get_list(list_name)
#'
#'   list_item <- sharepoint_list$list_items() |>
#'     get_tidy_table()
#'
#'   return(list_items)
#' }

#' Get File Name
#'
#' @param file_path The path of the file
#' @param folder_name The name of the folder
#'
#' @return The name of the file(s)
#'
#' @noRd
get_file_name <- function(file_path = tere::get_file_storage_path(), folder_name)
{
  file_name <- fs::dir_ls(paste0(file_path, folder_name)) |>
    stringr::word(start = -2, end = -1, sep = stringr::fixed("/")) |>
    stringr::word(1, sep = stringr::fixed("."))

  return(file_name)
}

#' Read Files
#'
#' @param file_name The name of the file
#' @param file_path The path of the file
#' @param file_type The format of the file
#' @param sheet The sheet number in the excel file. Defaults to the first sheet
#' @param skip The number of rows to skip before reading in the data
#'
#' @return Data from the file(s)
#'
#' @noRd
read_file <- function(file_name
                      , file_path = tere::get_file_storage_path()
                      , file_type = c("excel", "csv", "txt")
                      , file_extension = c(".xlsx", ".xls", ".csv")
                      , sheet = 1
                      , skip = 0)
{
  if(file_type == "excel")
  {
    data <- purrr::map(
      .x = file_name
      , .f = ~tere::get_excel_file(filename = .x
                                   , path = file_path
                                   , file_extension = file_extension
                                   , sheet = sheet
                                   , skip_rows = skip)
    )

    names(data) <- file_name

    data_with_names <- data |>
      purrr::list_rbind(names_to = "id")
  }

  if(file_type == "csv")
  {
    data <- purrr::map(
      .x = paste0(file_path, "/", file_name, file_extension)
      , .f = ~readr::read_delim(file = .x
                                , delim = ","
                                , col_types = "?"
                                , skip = skip)
      , .id = "file_name"
    )

    names(data) <- file_name

    data_with_names <- data |>
      purrr::list_rbind(names_to = "id") |>
      janitor::clean_names()
  }

  if(file_type == "tsv")
  {
    data <- purrr::map(
      .x = paste0(file_path, "/", file_name, file_extension)
      , .f = ~readr::read_tsv(file = .x
                              , col_types = "?"
                              , skip = skip)
      , .id = "file_name"
    )

    names(data) <- file_name

    data_with_names <- data |>
      purrr::list_rbind(names_to = "id") |>
      janitor::clean_names()
  }

  return(data_with_names)
}

#' Get register data from ms lists
#'
#' @return A dataframe containing data from the Libraries Subscription Databases Register
#' @export
#'
#' @noRd
get_data_register <- function()
{
  data_register <- tere::get_list_items(
    "ConnectedCommunitiesInsightsAnalysisTeam"
    , "Libraries Subscription Databases Register") |>
    janitor::clean_names()

  return(data_register)
}

#' Prepare subscription database information table
#'
#' @return A dataframe used for producing a file for reporting
#' @export
#'
#' @noRd
prepare_subscription_database_info <- function()
{
  data_register <- get_data_register()

  subscription_database_info <- data_register |>
    dplyr::select(
      sierra_record_number
      , database_name = title
      , vendor
      , package
      , subscription_status = subscribed
    ) |>
    dplyr::filter(!is.na(sierra_record_number)) |>
    dplyr::distinct(sierra_record_number, .keep_all = TRUE)

  return(subscription_database_info)
}

#' Prepare subscription database price table
#'
#' @return A dataframe used for producing a file for reporting
#' @export
#'
#' @noRd
prepare_subscription_database_price <- function()
{
  data_register <- get_data_register()

  subscription_database_price <- data_register |>
    dplyr::select(
      sierra_record_number
      , price_type
      , price
    ) |>
    dplyr::filter(!is.na(sierra_record_number)) |>
    dplyr::distinct(sierra_record_number, .keep_all = TRUE)

  return(subscription_database_price)
}

#' Prepare beamafilm data
#'
#' @param file_path The path of the file
#'
#' @return A dataframe containing all beamafilm data
#' @export
#'
#' @noRd
prepare_beamafilm <- function(file_path = paste0(tere::get_file_storage_path(), "/subscription_database"))
{
  file_name_beamafilm <- get_file_name(file_path, "/beamafilm")

  data_beamafilm <- read_file(
    file_name = file_name_beamafilm
    , file_path = file_path
    , file_type = "excel"
    , file_extension = ".xlsx"
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
#' @return A dataframe containing all discovery national archives data
#' @export
#'
#' @noRd
prepare_discovery_national_archives <- function(file_path = paste0(tere::get_file_storage_path(), "/subscription_database"))
{
  file_name_discovery_national_archives <- get_file_name(file_path, "/discovery_national_archives")

  data_discovery_national_archives <- read_file(
    file_name = file_name_discovery_national_archives
    , file_path = file_path
    , file_type = "excel"
    , file_extension = ".xlsx"
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

#' Prepare linked in learning data
#'
#' @param file_path The path of the file
#'
#' @return A dataframe containing all linked in learning data
#' @export
#'
#' @noRd
prepare_linked_in_learning <- function(file_path = paste0(tere::get_file_storage_path(), "/subscription_database"))
{
  file_name_linked_in_learning <- get_file_name(file_path, "/linked_in_learning")

  data_linked_in_learning <- read_file(
    file_name = file_name_linked_in_learning
    , file_path = file_path
    , file_type = "csv"
    , file_extension = ".csv"
  )

  #TODO figure out to handle old column name courses_viewed in oldest files
  clean_linked_in_learning <- data_linked_in_learning |>
    dplyr::select(
      end_day_pst_pdt
      , people_logged_in
      , course_views
      # , courses_viewed
    ) |>
    tidyr::pivot_longer(cols = c(
      people_logged_in
      , course_views
      # , courses_viewed
      )
      , names_to = "metric_name"
      , values_to = "value") |>
    dplyr::mutate(
      sierra_record_number = "b31669840"
      , reporting_period = lubridate::mdy(end_day_pst_pdt)
      , month = lubridate::month(reporting_period, label = TRUE, abbr = FALSE)
      , year = lubridate::year(reporting_period)
      , metric_name = dplyr::case_when(
        metric_name == "people_logged_in" ~ "sessions"
        , metric_name == "course_views" ~ "views"
        # , metric_name == "courses_viewed" ~ "views"
      )) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::select(sierra_record_number
                  , reporting_period
                  , metric_name
                  , value
                  , month
                  , year) |>
    dplyr::arrange(reporting_period)

  return(clean_linked_in_learning)
}


#' Prepare form metric data
#'
#' @param file_path The path of the file
#'
#' @return A dataframe containing all form metric data
#' @export
#'
#' @noRd
prepare_form_metric <- function(file_path = paste0(tere::get_file_storage_path(), "/subscription_database"))
{
  file_name_form_metric <- get_file_name(file_path, "/form_metric")

  data_form_metric <- read_file(
    file_name = file_name_form_metric
    , file_path = file_path
    , file_type = "excel"
    , file_extension = ".xlsx"
    , sheet = "Form1"
  )

  data_alias_table <- get_data_alias_table()

  clean_form_metric <- data_form_metric |>
    dplyr::select(
      database = please_choose_a_subscription_database
      , reporting_period = please_select_the_start_of_the_month_youre_providing_data_for_e_g_1_3_2020_for_march_2020
      , searches = how_many_searches_were_recorded
      , views = how_many_views_were_recorded
      , turnaways = how_many_turnaways_were_recorded
      , sessions = how_many_sessions_were_recorded
    ) |>
    dplyr::mutate(
      clean_database_name = janitor::make_clean_names(database
                                                      , allow_dupes = TRUE)
      , reporting_period = as.Date(reporting_period)
      , month = lubridate::month(reporting_period, label = TRUE, abbr = FALSE)
      , year = lubridate::year(reporting_period)
      , searches = as.double(searches)
      , views = as.double(views)
      , turnaways = as.double(turnaways)
      , sessions = as.double(sessions)
    ) |>
    dplyr::left_join(data_alias_table
                     , by = "clean_database_name"
                     , multiple = "warning") |>
    dplyr::select(
      sierra_record_number
      , reporting_period
      , searches
      , views
      , turnaways
      , sessions
      , year
      , month
      # , database
    ) |>
    tidyr::pivot_longer(cols = c(
      searches
      , views
      , turnaways
      , sessions
    )
    , names_to = "metric_name"
    , values_to = "value") |>
    dplyr::select(
      sierra_record_number
      , reporting_period
      , metric_name
      , value
      , month
      , year
    ) |>
    dplyr::filter(
      sierra_record_number != "NA"
      , !is.na(value))

  return(clean_form_metric)
}
#' Prepare integrated dataset
#'
#' @return A dataframe containing datasets from all data sources
#' @export
#'
#' @noRd
prepare_integrated_dataset <- function()
{

  clean_linked_in_learning <- prepare_linked_in_learning()

  clean_beamafilm <- prepare_beamafilm()

  clean_discovery_national_archives <- prepare_discovery_national_archives()

  # clean_form_metric <- prepare_form_metric()

  integrated_dataset <- dplyr::bind_rows(
    clean_linked_in_learning
    , clean_beamafilm
    , clean_discovery_national_archives
    # , clean_form_metric
  )

  return(integrated_dataset)
}

#' Create dataframe all
#'
#' @return A list of dataframes required to prepare other dataframes and files
#' @export
#'
#' @noRd
create_dataframe_all <- function()
{
  data_register <- get_data_register()
  # data_alias_table <- get_data_alias_table()
  subscription_database_info <- prepare_subscription_database_info()
  subscription_database_price <- prepare_subscription_database_price()
  integrated_dataset <- prepare_integrated_dataset()

  dataframe_list <- list(
    data_register
    # , data_alias_table
    , subscription_database_info
    , subscription_database_price
    , integrated_dataset
  )

  return(dataframe_list)

}


#' Write file all
#'
#' @description Writes all files required for subscription database reporting
#' @export
#'
#' @noRd
  write_file_all <- function()
{
  dataframe_list <- create_dataframe_all()

  # readr::write_csv(dataframe_list[[3]], "subscription_database_info.csv")
  # readr::write_csv(dataframe_list[[4]], "subscription_database_price.csv")
  # readr::write_csv(dataframe_list[[5]], "subscription_database_metric.csv")

  # temp without alias dataframe
  readr::write_csv(dataframe_list[[2]], "subscription_database_info.csv")
  readr::write_csv(dataframe_list[[3]], "subscription_database_price.csv")
  readr::write_csv(dataframe_list[[4]], "subscription_database_metric.csv")

}
