#' Login to FIS-Broker
#'
#' @param dbg print debug messages (default: TRUE)
#'
#' @return session id
#' @export
#' @importFrom kwb.utils catAndRun
#' @importFrom httr content GET
#' @examples
#' login_to_fis_broker()
#' 
login_to_fis_broker <- function(dbg = TRUE)
{
  kwb.utils::catAndRun(dbg = dbg, "Login to FIS-Broker", {
    compose_fis_broker_url(cmd = "user_login") %>%
      get_html_as_text(dbg = FALSE) %>%
      extract_single_string(pattern = 'jsessionid=([^?]+)\\?')
  })
}
