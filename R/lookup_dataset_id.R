# lookup_dataset_id ------------------------------------------------------------

#' Lookup Dataset ID Required for Download
#' 
#' @param overview overview tibble as retrieved by \code{\link{get_dataset_overview}}
#' @param dataset_id one dataset_id from column "dataset_id" of provided overview 
#' @param type dataset type, i.e. "ATOM", "WFS" or "WMS"
#' @importFrom dplyr filter pull
#' @importFrom kwb.utils createAccessor getAttribute selectColumns stopFormatted
#' @importFrom rlang .data
#' @return dataset id required for \code{\link{read_wfs}}
#' @export
lookup_dataset_id <- function(overview, dataset_id, type)
{
  fetch <- kwb.utils::createAccessor(overview)
  
  type <- match.arg(type, c("WFS", "WMS", "ATOM"))
  
  selected <- fetch("dataset_id") == dataset_id &
    fetch("type") == type
  
  if (sum(selected) != 1L) {
    kwb.utils::stopFormatted(
      "No dataset available for dataset_id = %d and type = '%s'",
      dataset_id,
      type
    )
  }
  
  record <- overview[selected, ]
  
  url <- get_urls(
    key. = "href_type",
    sid = kwb.utils::getAttribute(overview, "session_id"), 
    id = kwb.utils::selectColumns(record, "identifier"), 
    type = type
  )
  
  url_parameters <- c("Rechneradresse", "ATOM-Feed-Url")
  
  read_metadata(url = url, dbg = FALSE) %>% 
    dplyr::filter(.data$parameter %in% url_parameters) %>%
    dplyr::pull(.data$value) %>%
    basename()
}
