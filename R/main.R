if (FALSE)
{
  library(magrittr)
  
  response <- login_to_fis_broker() %>%
    compose_fis_broker_url(cmd = "navigationFrameResult") %>%
    httr_get_or_fail()
  
  html_tree <- response %>%
    httr::content(as = "text") %>%
    rvest::read_html()
  
  overview <- extract_overview_table(html_tree)
  
  #View(overview)
  
  #open_response(response)
}

# login_to_fis_broker ----------------------------------------------------------
login_to_fis_broker <- function(dbg = TRUE)
{
  kwb.utils::catAndRun(dbg = dbg, "Login to FIS-Broker", {
    compose_fis_broker_url(cmd = "user_login") %>%
      httr_get_or_fail() %>%
      httr::content(as = "text") %>%
      get_session_id()
  })
}

# compose_fis_broker_url -------------------------------------------------------
compose_fis_broker_url <- function(
    cmd = "user_login", session_id = NULL, type = NULL, id = NULL
)
{
  url <- httr::parse_url(get_urls(key. = "href_gisbroker"))
  
  url$params <- if (!is.null(session_id)) {
    paste0("jsessionid=", session_id)
  }
  
  url$query$cmd <- cmd
  url$query$type <- type
  url$query$id <- id

  httr::build_url(url)
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


# extract_single_string --------------------------------------------------------
extract_single_string <- function(pattern, x)
{
  kwb.utils::extractSubstring(pattern, x, index = 1L)
}

# extract_overview_table -------------------------------------------------------
extract_overview_table <- function(html_tree)
{
  nav_tables <- html_tree %>%
    rvest::html_nodes(xpath = '//table[@class="nav_tabelle"]')
  
  stopifnot(length(nav_tables) == 1L)
  
  overview <- nav_tables[[1L]] %>% 
    rvest::html_table() %>%
    clean_overview()
  
  ids <- html_tree %>%
    rvest::html_nodes(xpath = '//table[@class="nav_tabelle"]/tr/td/div/div/div/a[1]') %>%
    rvest::html_attr("href") %>%
    get_mid() %>%
    utils::URLdecode()
  
  stopifnot(length(ids) == nrow(overview))
  
  kwb.utils::setColumns(overview, id = ids, dbg = FALSE)
}

# clean_overview ---------------------------------------------------------------
clean_overview <- function(overview)
{
  overview[] <- lapply(overview, kwb.utils::naToLastNonNa)
  
  stopifnot(kwb.utils::allAreIdentical(overview[-(1:2)]))
  
  is_header <- apply(as.matrix(overview), 1L, kwb.utils::allAreEqual)
  
  overview <- overview[!is_header, 1:3] %>%
    stats::setNames(c("name", "formats", "topic")) %>%
    kwb.utils::moveColumnsToFront(c("topic"))
  
  overview$topic <- kwb.utils::toFactor(overview$topic)
  
  overview$formats <- kwb.utils::toFactor(overview$formats)
  
  overview
}

# extract_hrefs ----------------------------------------------------------------
extract_hrefs <- function(x)
{
  x %>%
    rvest::html_element("a") %>%
    rvest::html_attr("href")
}

# get_charset ------------------------------------------------------------------
get_charset <- function(html_text)
{
  extract_single_string('charset=([^"]+)"', html_text)
}

# open_response ----------------------------------------------------------------
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
