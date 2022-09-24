#' Get Dataset Overview 
#'
#' @return tibble with 7 columns and rows equal to number of datasets (
#' one for each dataset type, e.g WMS, WFS, ATOM) 
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
#' @importFrom dplyr arrange bind_rows left_join full_join relocate
#' @importFrom httr content GET 
#' @importFrom rvest html_element read_html
#' @importFrom stats setNames
#' @importFrom tidyr fill
#' @examples
#' fb_dataset_overview <- kwb.fisbroker::get_dataset_overview()
get_dataset_overview <- function() {

response <- login_to_fis_broker() %>%
  compose_fis_broker_url(cmd = "navigationFrameResult") %>%
  httr::GET()

tabelle <- response %>%
  httr::content(as = "text") %>%
  rvest::read_html() %>% 
  rvest::html_element(css = "table.nav_tabelle") 

x <- tabelle %>% 
  rvest::html_elements(css = "tr") 

is_category <- stringr::str_detect(as.character(x), "class=\"kategorie\"")

categories_df <- tibble::tibble(
  idx = which(is_category),
  category_id = seq_len(length(idx)), 
  category_name = rvest::html_text(x[idx])
)

is_dataset <- !is_category

datasets_df <- tibble::tibble(idx = which(is_dataset), 
                              dataset_id = seq_len(length(idx)), 
                              dataset_name_raw = rvest::html_text(x[idx]))


datasets_text_list <- stats::setNames(lapply(datasets_df$idx, function(index) {
  elements <- x[index] %>%  
    rvest::html_elements(css = "a.standard")
  
  elements_text <- rvest::html_text(elements)
  elements_href <- rvest::html_attr(elements, "href") %>%  
    stringr::str_replace(get_session_id(.),
                         "<jsessionid>")
  
  n_elements <- length(elements)
  
  if(n_elements == 1L) {
    tibble::tibble(dataset_name = elements_text[1L], 
                   dataset_mid = get_mid(elements_href[1L]),
                   dataset_name_href = elements_href[1L])
  } else if (n_elements > 1L) {
    dataset_name <- rvest::html_text(elements[1L])
    dataset_types <- rvest::html_text(elements[-1L])
    
    tibble::tibble(dataset_name = elements_text[1L], 
                   dataset_mid = get_mid(elements_href[1L]),
                   dataset_name_href = elements_href[1L],
                   dataset_type = elements_text[-1L],
                   dataset_type_href = elements_href[-1L])
  } else {
    stop("no element found")
  }  
  
  
}),datasets_df$idx)

fb_datasets <- dplyr::bind_rows(datasets_text_list, .id = "idx") %>%
  dplyr::mutate(idx = as.integer(.data$idx))

fb_datasets <- categories_df %>%  
  dplyr::full_join(fb_datasets, by = "idx") %>% 
  dplyr::arrange(.data$idx) %>% 
  tidyr::fill(.data$category_id, 
              .data$category_name) %>% 
  dplyr::filter(!is.na(.data$dataset_name)) %>%  
  dplyr::left_join(datasets_df[,c("idx", "dataset_id")], by = "idx") %>% 
  dplyr::select(- .data$idx) %>% 
  dplyr::relocate(.data$dataset_id, .before = .data$dataset_name)

fb_datasets
}

