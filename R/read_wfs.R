#' Read WFS Dataset from FIS-Broker
#'
#' @param dataset_id id of wfs dataset (default: "s_wfs_alkis_bezirk"). Needs to
#' checked out manually as described below:
#' 1. Go to: \url{https://fbinter.stadt-berlin.de/fb/}
#' 2. Click on a WFS Dataset
#' 3. Read the content of \code{Rechneraddresse} in the opened tab. The basename
#' of the url is the required dataset id!
#' @param service_version one of "1.0.0", "1.1.0" or "2.0.0" (default: "2.0.0")
#' @param srs one of "EPSG:4258" or "EPSG:25833" (default: "EPSG:25833")
#' @param encoding default: UTF-8
#' @param debug prints debug messages if TRUE (default: TRUE)
#' @return imports selected WFS dataset into R
#' @export
#' @importFrom fs path_join
#' @importFrom httr build_url content GET parse_url status_code
#' @importFrom sf read_sf
#' @importFrom xml2 write_xml
#' @seealso \url{https://fbinter.stadt-berlin.de/fb/berlin/service_intern.jsp?id=s_wfs_alkis_bezirk@@senstadt&type=WFS}
#' @examples
#' berlin_bezirke <- kwb.fisbroker::read_wfs(dataset_id = "s_wfs_alkis_bezirk")
read_wfs <- function(dataset_id = "s_wfs_alkis_bezirk",
                     service_version = "2.0.0",
                     srs = "EPSG:25833",
                     encoding = "UTF-8",
                     debug = TRUE) {
  
  stopifnot(service_version %in% c("1.0.0", "1.1.0", "2.0.0"))
  stopifnot(srs %in% paste0("EPSG:", c(25833, 4258)))
  
  service_type <- "WFS"
  fisbroker_urls <- get_urls()

  msg <- sprintf("Importing %s dataset_id '%s' from FIS-Broker",
                 service_type,
                 dataset_id)
  kwb.utils::catAndRun(messageText = msg,
                       expr = {
                         logger <- if (debug) {
                           "INFO"
                         } else {
                           NULL
                         }
                         
                         url_dataset  <- sprintf("%s/%s",
                                                 fisbroker_urls$wfs,
                                                 dataset_id)

                         url <- httr::parse_url(url_dataset)

                         url$query <- list(
                           service = service_type,
                           version = service_version,
                           request = "GetFeature",
                           typenames = sprintf("fis:%s", dataset_id),
                           srsName = srs
                         )

                         request <- httr::build_url(url)
                         
                         response <- httr::GET(request)
                         
                         if(httr::status_code(response)!="200") {
                           stop(sprintf("Request '%s' failed", request))
                         }
                         
                         content <- httr::content(response, encoding = encoding)
                         
                         temp_file <-
                           fs::path_join(c(tempdir(), paste0(basename(tempfile(
                           )), ".xml")))
                         xml2::write_xml(content, temp_file)

                         sf::read_sf(temp_file)
                       },
                       dbg = debug)
}
