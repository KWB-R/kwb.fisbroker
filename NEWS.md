# kwb.fisbroker 0.0.0.9000

* Add function `get_datasets_overview()` for generating `overview tibble` of available 
datasets at [https://fbinter.stadt-berlin.de/fb/index.jsp](https://fbinter.stadt-berlin.de/fb/index.jsp)
* Add function `read_all_metadata()` requiring an `overview tibble` for fetching 
metadata for all datasets
* Add function `read_metadata()` for metadata for `WFS` or `WMS` datasets
* Add function `read_wfs()` for importing `WFS` datasets from [FIS-Broker](https://fbinter.stadt-berlin.de/fb/index.jsp) into R as `sf` 
object. For details on using this function checkout the [Read WFS Dataset](../articles/wfs.html)
article.

* Added a `NEWS.md` file to track changes to the package.

* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`


