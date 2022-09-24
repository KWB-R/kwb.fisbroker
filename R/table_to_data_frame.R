# table_to_data_frame ----------------------------------------------------------
table_to_data_frame <- function(table_node)
{
  #table_node <- tables[[3L]]
  stopifnot(inherits(table_node, "xml_node"))
  
  table_fields <- table_node %>%
    rvest::html_elements("tr") %>%
    lapply(function(x) {
      sapply(rvest::html_elements(x, "td"), rvest::html_text)
    })
  
  has_two_fields <- lengths(table_fields) == 2L
  
  if (!all(has_two_fields)) {
    table_fields <- kwb.utils::catAndRun(
      sprintf(
        "Removing %d row(s) with not exactly two fields",
        sum(!has_two_fields)
      ),
      table_fields[has_two_fields]
    )
  }
  
  data <- table_fields %>%
    unlist() %>%
    replace_non_breaking_spaces() %>%
    matrix(ncol = 2L, byrow = TRUE) %>%
    kwb.utils::asNoFactorDataFrame() %>%
    stats::setNames(c("parameter", "value"))
  
  data[["parameter"]] <- data[["parameter"]] %>%
    kwb.utils::multiSubstitute(list(":$" =  "", "^\\s*$" = NA)) %>%
    kwb.utils::naToLastNonNa()
  
  data
}
