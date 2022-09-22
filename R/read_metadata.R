#' Read metadata
#'
#' @param dataset_id id of wfs dataset (default: "s_wfs_alkis_bezirk"). Needs to
#' checked out manually as described below:
#' 1. Go to: \url{https://fbinter.stadt-berlin.de/fb/}
#' 2. Click on a "WFS" or "WMS" Dataset
#' 3. Read the content of \code{Rechneraddresse} in the opened tab. The basename
#' of the url is the required dataset id!
#' @param service_type either "WFS" or "WMS" (default: "WFS")
#' @seealso \url{https://fbinter.stadt-berlin.de/fb/berlin/service_intern.jsp?id=s_wfs_alkis_bezirk@@senstadt&type=WFS}
#' @return tibble with metadata for provided dataset_id
#' @export
#' @importFrom dplyr bind_rows left_join mutate rename
#' @importFrom stringr str_replace str_remove
#' @importFrom xml2 read_html
#' @importFrom rvest html_elements html_text
#' @importFrom tibble tibble
#' @importFrom tidyr fill
#' @importFrom rlang .data
#' @examples
#' berlin_bezirke_metadata <- read_metadata(dataset_id = "s_wfs_alkis_bezirk")
#' berlin_bezirke_metadata
#' 
read_metadata <- function(dataset_id = "s_wfs_alkis_bezirk", 
                          service_type = "WFS") {
  
url <- sprintf("%s/fb/berlin/service_intern.jsp?id=%s@senstadt&type=%s",
               get_urls()$base,
               dataset_id, 
               service_type)

x <- xml2::read_html(url)

commentline <- function() {paste0(rep("#", 80), collapse = "")}

commentlines <- function() {
  sprintf("%s\n%s\n%s", 
          commentline(),
          commentline(),
          commentline())
}
text <- rvest::html_text(x)
if(stringr::str_detect(text, "Fehler")) {
  msg <- sprintf(paste0("\nMetadata for URL '%s' is not available!\n\n", 
                        commentlines(), 
                        "\n\n%s\n",
                        commentlines()),
                 url, 
                 text)
  
  stop(msg)
}

x <- xml2::read_html(url)

headers <- x %>% 
  rvest::html_elements(css = "span.titel") %>% 
  rvest::html_text()

tables <- x %>%  
  rvest::html_elements(css = "table.noborder") %>%  
  rvest::html_table()


tables[[1]] <- tables[[1]] %>%  
  dplyr::mutate(X1 = stringr::str_replace(.data$X1, 
                     pattern = "\xa0", NA_character_) %>% 
                  stringr::str_remove(":")) %>% 
tidyr::fill(.data$X1) %>% 
  dplyr::rename(parameter = .data$X1, 
                value = .data$X2)

tables[[2]] <- tables[[2]] %>%   
  dplyr::mutate(X1 = stringr::str_replace(.data$X1,
                                          pattern = "^\\s*$", NA_character_) %>% 
                                  stringr::str_remove(":")) %>% 
  tidyr::fill(.data$X1) %>% 
  dplyr::rename(parameter = .data$X1, 
                value = .data$X2)

table <- dplyr::bind_rows(setNames(tables,
                          nm = headers),
                 .id = "title_name")

tibble::tibble(title_id = length(headers),
               title_name = headers) %>%  
  dplyr::left_join(table, 
                   by = "title_name")


}
