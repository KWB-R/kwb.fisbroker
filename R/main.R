if (FALSE)
{
  library(magrittr)
  
  response <- login_to_fis_broker() %>%
    compose_fis_broker_url(cmd = "navigationFrameResult") %>%
    httr::GET()
  
  html_tree <- response %>%
    httr::content(as = "text") %>%
    rvest::read_html()
  
  overview <- extract_overview_table(html_tree)
  
  #View(overview)
  
  #open_response(response)
}