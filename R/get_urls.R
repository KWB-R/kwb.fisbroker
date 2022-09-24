#' Get Urls
#'
#' @param \dots further arguments passed to \code{\link[kwb.utils]{resolve}}
#' @return return `base` and `wfs` urls of FIS-Broker
#' @export
#' @importFrom kwb.utils resolve
#' @examples
#' fisbroker_urls <- get_urls()
#' fisbroker_urls
#'
get_urls <- function(...) {
  
  urls_list <- list(
    base = "https://fbinter.stadt-berlin.de",
    do = "<base>/fb/gisbroker.do",
    wfs = "<base>/fb/wfs/data/senstadt",
    meta = "<base>/fb/berlin/service_intern.jsp?id=<id>@senstadt&type=<type>"
  )
  
  kwb.utils::resolve(urls_list, ...)
}
