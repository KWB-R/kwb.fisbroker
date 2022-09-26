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
#' @importFrom rvest html_attr html_nodes html_table
#' @importFrom utils URLdecode 
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
    extract_single_string(pattern = '&mid=(.*)$') %>%
    utils::URLdecode()
  
  stopifnot(length(ids) == nrow(overview))
  
  kwb.utils::setColumns(overview, id = ids, dbg = FALSE)
}

# clean_overview ---------------------------------------------------------------

#' Clean Overview
#'
#' @param overview overview
#'
#' @return cleaned overview
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom kwb.utils allAreEqual allAreIdentical moveColumnsToFront 
#' @importFrom kwb.utils naToLastNonNa
#' @importFrom stats setNames 
clean_overview <- function(overview)
{
  overview[] <- lapply(overview, kwb.utils::naToLastNonNa)
  
  stopifnot(kwb.utils::allAreIdentical(overview[-(1:2)]))
  
  is_header <- apply(as.matrix(overview), 1L, kwb.utils::allAreEqual)
  
  overview <- overview[!is_header, 1:3] %>%
    stats::setNames(c("name", "formats", "topic")) %>%
    columns_to_factor(c("formats", "topic")) %>%
    kwb.utils::moveColumnsToFront("topic")
}
