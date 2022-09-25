# open_response ----------------------------------------------------------------
#' Open Response in Browser
#'
#' @param response response 
#' @return open response in browser 
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom httr content
#' @importFrom kwb.utils writeText
#' @importFrom utils browseURL
open_response <- function(response)
{
  html_text <- httr::content(response, as = "text")
  
  charset <- get_charset(html_text)
  
  con <- file(tempfile(fileext = ".html"), encoding = charset)
  
  con_back <- kwb.utils::writeText(html_text, file = con)
  
  file <- summary(con_back)$description
  
  utils::browseURL(file)
  
  file
}