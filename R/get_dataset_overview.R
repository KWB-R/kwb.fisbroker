#' Get Dataset Overview 
#'
#' @param dbg whether or not to show debug messages
#' @return tibble with 7 columns and rows equal to number of datasets 
#' (one for each dataset type, e.g WMS, WFS, ATOM) 
#' \describe{
#'   \item{category_id}{Category id of dataset (artifically generated)}
#'   \item{category_name}{Category name of dataset)}
#'   \item{dataset_id}{Dataset id, artifically generated!!!}
#'   \item{dataset_name}{Dataset name as listed on table}
#'   \item{dataset_name_href}{Link of dataset name. JS session Id was replaced 
#'   with a placeholder}
#'   \item{dataset_type}{Dataset type as listed on table one of `ATOM`, `WFS` or 
#'   `WMS`}
#'   \item{dataset_type_href}{Link of dataset type. JS session Id was replaced 
#'   with a placeholder} 
#' } 
#' @export
#' @importFrom dplyr arrange bind_rows filter full_join left_join mutate select
#' @importFrom kwb.utils extractSubstring moveColumnsToFront
#' @importFrom rlang .data
#' @importFrom rvest html_attr html_element html_elements html_text read_html
#' @importFrom stats setNames
#' @importFrom tibble tibble
#' @importFrom tidyr fill
#' @examples
#' fb_dataset_overview <- kwb.fisbroker::get_dataset_overview()
#' # View(fb_dataset_overview)
get_dataset_overview <- function(dbg = TRUE)
{
  #kwb.utils::assignPackageObjects("kwb.fisbroker")
  #dbg <- TRUE
  #`%>%` <- magrittr::`%>%`
  
  session_id <- login_to_fis_broker(dbg = dbg)
  
  table_rows <- session_id %>%
    compose_fis_broker_url(cmd = "navigationFrameResult") %>%
    get_html_as_text(dbg = dbg) %>%
    rvest::read_html() %>% 
    rvest::html_element(css = "table.nav_tabelle") %>%
    rvest::html_elements(css = "tr")
  
  is_category <- grepl("class=\"kategorie\"", as.character(table_rows))
  
  to_id_name <- function(is_selected, prefix) {
    do.call(tibble::tibble, args = stats::setNames(
      nm = c("idx", paste0(prefix, c("id", "name"))),
      object = list(
        which(is_selected),
        seq_len(sum(is_selected)), 
        rvest::html_text(table_rows[is_selected])
      )
    ))
  }
  
  categories <- to_id_name(is_category, prefix = "category_")
  datasets <- to_id_name(!is_category, prefix = "dataset_")
  
  extract_from_row <- function(index) {
    #index <- 4L
    elements <- rvest::html_elements(table_rows[index], css = "a.standard")
    
    if (length(elements) == 0L) {
      stop("no element found")
    }
    
    elements_text <- rvest::html_text(elements)
    elements_href <- rvest::html_attr(elements, "href") 
    
    pattern <- "\\?(cmd=([^&]+)&(type=([^&]+)&)?m?id=([^']+))"
    elements <- c(cmd = 2L, type = 4L, identifier = 5L)
    
    kwb.utils::extractSubstring(pattern, elements_href, elements) %>%
      cbind(dataset_name = elements_text[1L]) %>%
      kwb.utils::moveColumnsToFront("dataset_name")
  }
  
  lapply(datasets$idx, extract_from_row) %>%
    stats::setNames(datasets$idx) %>%
    dplyr::bind_rows(.id = "idx") %>%
    dplyr::mutate(idx = as.integer(.data$idx)) %>%  
    dplyr::full_join(categories, by = "idx") %>% 
    dplyr::arrange(.data$idx) %>% 
    tidyr::fill(.data$category_id, .data$category_name) %>% 
    dplyr::filter(!is.na(.data$dataset_name)) %>%  
    dplyr::left_join(datasets[, c("idx", "dataset_id")], by = "idx") %>% 
    dplyr::select(- .data$idx) %>% 
    columns_to_factor(c("category_name", "cmd", "type")) %>%
    kwb.utils::moveColumnsToFront(c("dataset_id", "dataset_name")) %>%
    kwb.utils::moveColumnsToFront(c("category_id", "category_name")) %>%
    structure(session_id = session_id)
}
