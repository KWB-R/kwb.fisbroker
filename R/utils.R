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
#' @param x x 
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
  kwb.utils::catAndRun(
    sprintf("Getting HTML text from '%s'", url),
    httr::content(httr::GET(url), as = "text"),
    dbg = dbg
  )
}

# get_session_id ---------------------------------------------------------------
get_session_id <- function(x) 
{
  extract_single_string('jsessionid=([^?]+)\\?', x)
}

# get_mid ----------------------------------------------------------------------
get_mid <- function(x) 
{
  extract_single_string(pattern = '&mid=(.*)$', x)
}

# replace_non_breaking_spaces --------------------------------------------------
replace_non_breaking_spaces <- function(x)
{
  gsub(pattern = "\ua0", replacement = " ", x)
}
