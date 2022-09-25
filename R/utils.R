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
#' @param url url
#' @param dbg dbg
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom httr GET content
#' @importFrom kwb.utils catAndRun
get_html_as_text <- function(url, dbg = TRUE)
{
  msg <- sprintf("Getting HTML text from '%s'", kwb.utils::shorten(url, 50L))
  
  kwb.utils::catAndRun(msg, dbg = dbg, expr = {
    url %>%
      httr_get_or_fail() %>%
      httr::content(as = "text")
  })
}

# httr_get_or_fail -------------------------------------------------------------
#' @importFrom httr GET status_code
httr_get_or_fail <- function(url)
{
  response <- httr::GET(url)
  
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

# to_url -----------------------------------------------------------------------
to_url <- function(x)
{
  kwb.utils::addClass(x, "url")  
}

# write_temp_xml_file ----------------------------------------------------------
write_temp_xml_file <- function(content)
{
  temp_file <-   fs::path_join(c(
    tempdir(), 
    paste0(basename(tempfile()), ".xml")
  ))
  
  xml2::write_xml(content, temp_file)

  temp_file  
}
