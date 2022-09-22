#' Read WFS Dataset from FIS-Broker
#'
#' @param dataset_id id of wfs dataset (default: "s_wfs_alkis_bezirk"). Needs to
#' checked out manually as described below:
#' 1. Go to: \url{https://fbinter.stadt-berlin.de/fb/}
#' 2. Click on a WFS Dataset
#' 3. Read the content of \code{Rechneraddresse} in the opened tab. The basename
#' of the url is the required dataset id!
#' @param service_version one of "1.0.0", "1.1.0" or "2.0.0" (default: "2.0.0")
#' @param srs_name one of "EPSG:4258" or "EPSG:25833" (default: "EPSG:4258")
#' @param debug prints debug messages if TRUE (default: TRUE)
#' @return imports selected WFS dataset into R
#' @export
#' @importFrom fs path_join
#' @importFrom httr build_url content GET parse_url
#' @importFrom ows4R WFSClient
#' @importFrom sf read_sf
#' @importFrom xml2 write_xml
#' @seealso \url{https://fbinter.stadt-berlin.de/fb/berlin/service_intern.jsp?id=s_wfs_alkis_bezirk@@senstadt&type=WFS}
#' @examples
#' berlin_bezirke <- kwb.fisbroker::read_wfs(dataset_id = "s_wfs_alkis_bezirk")
read_wfs <- function(dataset_id = "s_wfs_alkis_bezirk",
                     service_version = "2.0.0",
                     srs_name = "EPSG:4258",
                     debug = TRUE) {
  fisbroker_urls <- get_urls()

  ## not used yet. Provides metadata for WFS dataset
  url_docu <-
    sprintf(
      "%s/fb/berlin/service_intern.jsp?id=%s@senstadt&type=WFS",
      fisbroker_urls$base,
      dataset_id
    )
  #browseURL(url_docu)

  msg <- sprintf("Importing WFS dataset_id '%s' from FIS-Broker",
                 dataset_id)
  kwb.utils::catAndRun(messageText = msg,
                       expr = {
                         logger <- if (debug) {
                           "INFO"
                         } else {
                           NULL
                         }



                         url_wfs_dataset  <- sprintf("%s/%s",
                                                     fisbroker_urls$wfs,
                                                     dataset_id)

                         wfs_dataset_client <- ows4R::WFSClient$new(url_wfs_dataset ,
                                                                    serviceVersion = service_version,
                                                                    logger = logger)

                         dataset <- wfs_dataset_client$getFeatureTypes(pretty = TRUE)

                         url <- httr::parse_url(url_wfs_dataset)

                         url$query <- list(
                           service = "wfs",
                           version = service_version,
                           request = "GetFeature",
                           typenames = dataset$name,
                           srsName = srs_name
                         )

                         request <- httr::build_url(url)

                         # Works
                         response <- httr::GET(request)
                         content <- httr::content(response)


                         temp_file <-
                           fs::path_join(c(tempdir(), paste0(basename(tempfile(
                           )), ".xml")))
                         xml2::write_xml(content, temp_file)

                         sf::read_sf(temp_file)
                       },
                       dbg = debug)
}
