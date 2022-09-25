# extract_overview_table -------------------------------------------------------
#' Extract Overview Table
#'
#' @param html_tree html_tree
#'
#' @return extracted overview table 
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom kwb.utils setColumns
#' @importFrom utils URLdecode 
#' @importFrom rvest html_nodes html_table
extract_overview_table <- function(html_tree)
{
  nav_tables <- html_tree %>%
    rvest::html_nodes(xpath = '//table[@class="nav_tabelle"]')
  
  stopifnot(length(nav_tables) == 1L)
  
  overview <- nav_tables[[1L]] %>% 
    rvest::html_table() %>%
    clean_overview()
  
  ids <- html_tree %>%
    rvest::html_nodes(xpath = '//table[@class="nav_tabelle"]/tr/td/div/div/div/a[1]') %>%
    rvest::html_attr("href") %>%
    get_mid() %>%
    utils::URLdecode()
  
  stopifnot(length(ids) == nrow(overview))
  
  kwb.utils::setColumns(overview, id = ids, dbg = FALSE)
}
