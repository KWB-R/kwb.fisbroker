#' Get Urls
#'
#' @param \dots further arguments passed to \code{\link[kwb.utils]{resolve}}
#' @param key. optional. Name of element to be returned from the URL dictionary.
#'   If \code{NULL} (the default), the whole dictionary (a list) is returned. 
#' @return return `base` and `wfs` urls of FIS-Broker
#' @export
#' @importFrom kwb.utils resolve selectElements
#' @examples
#' # List of all defined (partial) URls
#' get_urls()
#' 
#' # One specific URL with placeholders replaced as given
#' get_urls(
#'   key. = "href_type", sid = "my-session-id", type = "WFS", id = "my-id"
#' )
get_urls <- function(..., key. = NULL)
{
  dictionary <- list(
    scheme = "https",
    hostname = "fbinter.stadt-berlin.de",
    base = "<scheme>://<hostname>",
    path__gisbroker = "fb/gisbroker.do",
    path__atom_feed = "fb/feed/senstadt",
    path__wfs_data = "fb/wfs/data/senstadt",
    path__wms_data = "fb/wms/data/senstadt",
    path__intern = "fb/berlin/service_intern.jsp",
    query__result = to_query_string(
      cmd = "navigationShowResult", 
      mid = "<id>"
    ),
    query__service = to_query_string(
      cmd = "navigationShowService", 
      type = "<type>", 
      id = "<id>"
    ),
    query__intern = to_query_string(
      id = "<id>@senstadt", 
      type = "<type>"
    ),
    params = "jsessionid=<sid>",
    href_atom  = "<base>/<path__atom_feed>/<id>?<query__atom>",
    href_wfs  = "<base>/<path__wfs_data>/<id>?<query__wfs>",
    href_wms  = "<base>/<path__wms_data>/<id>?<query__wms>",
    href_meta = "<base>/<path__intern>?<query__intern>",
    href_gisbroker = "<base>/<path__gisbroker>",
    href_name = "<href_gisbroker>;<params>?<query__result>",
    href_type = "<href_gisbroker>;<params>?<query__service>"
  )
  
  resolved <- kwb.utils::resolve(dictionary, ...)
  
  if (is.null(key.)) {
    return(resolved)
  }
  
  kwb.utils::selectElements(resolved, key.)
}
