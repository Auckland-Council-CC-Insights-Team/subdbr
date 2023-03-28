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
