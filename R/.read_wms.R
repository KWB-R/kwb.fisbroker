#' Read WMS Dataset from FIS-Broker
#'
#' @param dataset_id id of wms dataset (default: "k_alkis_bezirke"). Needs to
#' checked out manually as described below:
#' 1. Go to: \url{https://fbinter.stadt-berlin.de/fb/}
#' 2. Click on a WMS Dataset
#' 3. Read the content of \code{Rechneraddresse} in the opened tab. The basename
#' of the url is the required dataset id!
#' @param map_format file format of the map to be requested. Either "jpeg" or 
#' "png" (default: jpeg)
#' @param service_version one of "1.0.0", "1.1.0", "1.1.1" or "1.3.0" 
#' (default: "1.3.0")
#' @param srs only "EPSG:25833" (default: "EPSG:25833")
#' @param encoding default: UTF-8
#' @param debug prints debug messages if TRUE (default: TRUE)
#' @return imports selected WMS dataset into R
#' @export
#' @importFrom fs path_join
#' @importFrom httr build_url content GET parse_url status_code
#' @importFrom sf read_sf
#' @importFrom xml2 write_xml
#' @seealso \url{https://fbinter.stadt-berlin.de/fb/berlin/service_intern.jsp?id=s_wfs_alkis_bezirk@@senstadt&type=WFS}
#' @examples
#' berlin_bezirke <- kwb.fisbroker::read_wfs(dataset_id = "k_alkis_bezirke")
read_wms <- function(dataset_id = "k_alkis_bezirke",
                     map_format = "jpeg", 
                     service_version = "1.3.0",
                     srs = "EPSG:25833",
                     encoding = "UTF-8",
                     debug = TRUE) {
  
  stopifnot(map_format %in% c("jpeg", "png"))
  stopifnot(service_version %in% c("1.0.0", "1.1.0", "1.1.1", "1.3.0"))
  stopifnot(srs %in% c("EPSG:25833"))
  
  service_type <- "WMS"
  fisbroker_urls <- get_urls()
  map_format_name <- sprintf("image/%s", tolower(map_format))
  
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
                                                 fisbroker_urls$wms,
                                                 dataset_id)
                         
                         url <- httr::parse_url(url_dataset)
                         
                         url$query <- list(
                           service = service_type,
                           version = service_version,
                           request = "GetMap",
                           typenames = sprintf("fis:%s", dataset_id),
                           srs = srs,
                           format = map_format_name,
                           width = 1500, 
                           height = 
                         )
                         
                         url$query <- list(
                           service = service_type,
                           version = service_version,
                           request = "GetCapabilities",
                           typenames = sprintf("fis:%s", dataset_id)
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