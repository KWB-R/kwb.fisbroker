
# clean_overview ---------------------------------------------------------------
#' Clean Overview
#'
#' @param overview overview
#'
#' @return cleaned overview
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom kwb.utils allAreEqual allAreIdentical moveColumnsToFront 
#' naToLastNonNa toFactor
#' @importFrom stats setNames 
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
