#' Get Urls
#'
#' @return return `base` and `wfs` urls of FIS-Broker
#' @export
#' @importFrom kwb.utils resolve
#' @examples
#' fisbroker_urls <- get_urls()
#' fisbroker_urls
#'
get_urls <- function() {
urls_list <- list(
  base = "https://fbinter.stadt-berlin.de",
  wfs = "<base>/fb/wfs/data/senstadt"
)

kwb.utils::resolve(urls_list)
}

