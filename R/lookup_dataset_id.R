# lookup_dataset_id ------------------------------------------------------------
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
  
  read_metadata(url = url, debug = FALSE) %>% 
    dplyr::filter(.data$parameter %in% url_parameters) %>%
    dplyr::pull(.data$value) %>%
    basename()
}
