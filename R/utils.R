# commentlines -----------------------------------------------------------------
commentlines <- function()
{
  commentline <- kwb.utils::repeated("#", 80)
  sprintf("%s\n%s\n%s", commentline, commentline, commentline)
}

# get_html_as_text -------------------------------------------------------------

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

# replace_non_breaking_spaces --------------------------------------------------
replace_non_breaking_spaces <- function(x)
{
  gsub(pattern = "\ua0", replacement = " ", x)
}
