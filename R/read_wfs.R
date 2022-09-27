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
#' @param dbg prints debug messages if TRUE (default: TRUE)
#' @return imports selected WFS dataset into R
#' @export
#' @importFrom httr content
#' @importFrom kwb.utils catAndRun
#' @importFrom sf read_sf
#' @importFrom utils URLencode
#' @seealso \url{https://fbinter.stadt-berlin.de/fb/berlin/service_intern.jsp?id=s_wfs_alkis_bezirk@@senstadt&type=WFS}
#' @examples
#' berlin_bezirke <- kwb.fisbroker::read_wfs(dataset_id = "s_wfs_alkis_bezirk")
read_wfs <- function(
    dataset_id = "s_wfs_alkis_bezirk",
    service_version = "2.0.0",
    srs = "EPSG:25833",
    encoding = "UTF-8",
    dbg = TRUE
)
{
  #kwb.utils::assignPackageObjects("kwb.fisbroker")
  
  stopifnot(service_version %in% c("1.0.0", "1.1.0", "2.0.0"))
  stopifnot(srs %in% paste0("EPSG:", c(25833, 4258)))
  
  service_type <- "WFS"
  
  msg <- sprintf(
    "Importing %s dataset_id '%s' from FIS-Broker",
    service_type,
    dataset_id
  )
  
  kwb.utils::catAndRun(messageText = msg, dbg = dbg, expr = {
    
    full_url <- get_urls(
      key. = "href_wfs", 
      id = dataset_id,
      query__wfs = utils::URLencode(to_query_string(
        service = service_type,
        version = service_version,
        request = "GetFeature",
        typenames = paste0("fis:", dataset_id),
        srsName = srs
      ))
    )
    
    content <- httr_get_or_fail(full_url) %>%
      httr::content(encoding = encoding, options = "Huge")
    
    temp_file <- write_temp_xml_file(content)

    sf::read_sf(temp_file)
  })
}
