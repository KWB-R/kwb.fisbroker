#' FIS-Broker Datasets
#'
#' An overview of available FIS-Broker datasets (based on an offline download 
#' version of the table on FIS-Broker). Generated with R code in file 
#' data-raw/fb_datasets.R
#'
#' @format A data frame with 1186 rows and 7 variables:
#' \describe{
#'   \item{category_id}{Category id of dataset (artifically generated)}
#'   \item{category_name}{Category name of dataset)}
#'   \item{dataset_id}{Dataset id, artifically generated!!!}
#'   \item{dataset_name}{Dataset name as listed on table}
#'   \item{dataset_name_href}{Link of dataset name. JS session Id was replaced with 
#'   a placeholder}
#'   \item{dataset_type}{Dataset type as listed on table one of `ATOM`, `WFS` or 
#'   `WMS`}
#'   \item{dataset_type_href}{Link of dataset type. JS session Id was replaced with 
#' } 
#' @examples
#' fb_datasets
"fb_datasets"