## code to prepare `DATASET` dataset goes here
####"inst/extdata/fisbroker.html = "view-source:https://fbinter.stadt-berlin.de/fb/gisbroker.do;jsessionid=C0EDCE1624C9A8877CB10BFE1B3C74EE?cmd=navigationFrameResult"

get_jsessionid <- function(string) {
  jsessionid <- stringr::str_extract(string, "jsessionid=.*\\?.*")
  
  jsessionid[!is.na(jsessionid )] %>%  
    stringr::str_remove("\\?.*") %>% 
    stringr::str_remove(".*jsessionid=")
}

tabelle <- xml2::read_html("inst/extdata/fisbroker.html", encoding = "UTF-8") %>% 
  rvest::html_element(css = "table.nav_tabelle") 

tabelle %>% 
  rvest::html_elements(css = "tr.kategorie") 

x <- tabelle %>% 
  rvest::html_elements(css = "tr") 



is_category <- stringr::str_detect(x, "class=\"kategorie\"")

categories_df <- tibble::tibble(
  idx = which(is_category),
  category_id = seq_len(length(idx)), 
  category_name = rvest::html_text(x[idx])
)

is_dataset <- !is_category

datasets_df <- tibble::tibble(idx = which(is_dataset), 
                              dataset_id = seq_len(length(idx)), 
                              dataset_name_raw = rvest::html_text(x[idx]))


datasets_text_list <- stats::setNames(lapply(datasets_df$idx, function(idx) {
  elements <- x[idx] %>%  
    rvest::html_elements(css = "a.standard")
  
  elements_text <- rvest::html_text(elements)
  elements_href <- rvest::html_attr(elements, "href") %>%  
    stringr::str_replace(get_jsessionid(.),
                         "<jsessionid>")
  
  n_elements <- length(elements)
  
  if(n_elements == 1) {
    tibble::tibble(dataset_name = elements_text[1], 
                   dataset_name_href = elements_href[1])
  } else if (n_elements > 1) {
    dataset_name <- rvest::html_text(elements[1])
    dataset_types <- rvest::html_text(elements[2:n_elements])
    
    tibble::tibble(dataset_name = elements_text[1], 
                   dataset_name_href = elements_href[1],
                   dataset_type = elements_text[2:n_elements],
                   dataset_type_href = elements_href[2:n_elements])
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

usethis::use_data(fb_datasets, overwrite = TRUE)
