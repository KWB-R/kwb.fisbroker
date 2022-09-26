# columns_to_factor ------------------------------------------------------------
columns_to_factor <- function(data, columns)
{
  data[columns] <- lapply(data[columns], kwb.utils::toFactor)
  data
}
                              
# commentlines -----------------------------------------------------------------

#' Commentlines
#'
#' @return commentlines
#' @keywords internal
#' @noRd
#' @noMd
#' @importFrom kwb.utils repeated
commentlines <- function()
{
  commentline <- kwb.utils::repeated("#", 80)
  sprintf("%s\n%s\n%s", commentline, commentline, commentline)
}

# extract_hrefs ----------------------------------------------------------------

#' Extract hrefs
#'
#' @param x document, node set or single node, see
#'   \code{\link[rvest]{html_element}}
#'
#' @return extracted hrefs
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom rvest html_attr html_element
extract_hrefs <- function(x)
{
  x %>%
    rvest::html_element("a") %>%
    rvest::html_attr("href")
}

# extract_single_string --------------------------------------------------------

#' Extract Single String
#'
#' @param pattern pattern
#' @param x x
#' @return extracted substring
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom kwb.utils extractSubstring
extract_single_string <- function(pattern, x)
{
  kwb.utils::extractSubstring(pattern, x, index = 1L)
}

# get_charset ------------------------------------------------------------------
get_charset <- function(html_text)
{
  extract_single_string('charset=([^"]+)"', html_text)
}

# get_html_as_text -------------------------------------------------------------

#' Get HTML as Text
#' 
#' @param url url
#' @param handle passed to \code{\link[httr]{GET}}
#' @param dbg dbg
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom httr content
#' @importFrom kwb.utils catAndRun
get_html_as_text <- function(url, handle = NULL, dbg = TRUE)
{
  msg <- sprintf("Getting HTML text from '%s'", kwb.utils::shorten(url, 50L))
  
  kwb.utils::catAndRun(msg, dbg = dbg, expr = {
    httr_get_or_fail(url, handle) %>%
      httr::content(as = "text")
  })
}

# httr_get_or_fail -------------------------------------------------------------

#' GET Request with Check for Error
#' 
#' The function stops with error if the GET request returns status != 200
#' 
#' @param url URL to which to send a GET request
#' @param handle passed to \code{\link[httr]{GET}}
#' @return If the status was not 200, the function returns what 
#' \code{\link[httr]{GET}} returned
#' @importFrom httr GET status_code
#' @export
httr_get_or_fail <- function(url, handle = NULL)
{
  response <- httr::GET(url, handle = handle)
  
  if (httr::status_code(response) != "200") {
    stop(sprintf("Request '%s' failed", url))
  }

  response
}

# replace_non_breaking_spaces --------------------------------------------------
replace_non_breaking_spaces <- function(x)
{
  gsub(pattern = "\ua0", replacement = " ", x)
}

# to_query_string --------------------------------------------------------------
to_query_string <- function(...)
{
  x <- list(...)
  
  if (length(x) == 0L) {
    return("")
  }
  
  paste0(names(x), "=", as.character(x), collapse = "&")
}

# trim_end_of_lines ------------------------------------------------------------
trim_end_of_lines <- function(x)
{
  gsub("(^\r\n)|(\r\n$)", "", x)
}

# write_temp_xml_file ----------------------------------------------------------

#' @importFrom xml2 write_xml
write_temp_xml_file <- function(content)
{
  temp_file <-   fs::path_join(c(
    tempdir(), 
    paste0(basename(tempfile()), ".xml")
  ))
  
  xml2::write_xml(content, temp_file)

  temp_file  
}

#' Write to JSON or CSV
#'
#' @param format either 'json' or 'csv'
#' @param df data frame
#'
#' @return writes provided df to working directory with same name  
#' @export
#' @importFrom jsonlite write_json
#' @importFrom readr write_csv
write_to <- function(format, df) {
  df_name <- deparse(substitute(df)) 
  format <- tolower(format)
  file <- sprintf("%s.%s", df_name, format)
  if(format == "json") {
    jsonlite::write_json(df, file)
  } else if (format == "csv") {
    readr::write_csv(df, file)
  } else {
    stop("Only export formats 'json' or 'csv' are defined")
  }
}

