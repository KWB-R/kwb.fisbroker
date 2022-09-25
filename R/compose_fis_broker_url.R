# compose_fis_broker_url -------------------------------------------------------
#' Compose FIS-Broker URL
#'
#' @param cmd command (default: "user login")
#' @param session_id as retrieved by \code{\link{login_to_fis_broker}},
#' (default: NULL)
#' @param type dataset type, i.e. "ATOM", "WFS" or "WMS" (default: NULL)
#' @param id dataset id (default: NULL)
#' @return composed FIS-Broker URL
#' @export
#' @importFrom httr build_url
compose_fis_broker_url <- function(
    cmd = "user_login", session_id = NULL, type = NULL, id = NULL
)
{
  url <- httr::parse_url(get_urls()$do)
  
  url$params <- if (!is.null(session_id)) {
    paste0("jsessionid=", session_id)
  }
  
  url$query$cmd <- cmd
  
  url$query$type <- if (!is.null(type)) {
    type
  }
  
  url$query$id <- if (!is.null(id)) {
    id
  }
  
  httr::build_url(url)
}