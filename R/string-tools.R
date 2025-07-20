#Wrapper functions for stringr package methods

# -- stringr wrapper ----------------------------------------------------------
## usethis namespace: start
#' @importFrom stringr str_trim
#' @importFrom stringr str_detect
#' @importFrom stringr str_match
#' @importFrom stringr str_split
#' @importFrom stringr str_length
#' @importFrom stringr str_replace_all
#' @importFrom stringr fixed
#' @importFrom stringr str_c
## usethis namespace: end
NULL


empty <- function(x) UseMethod("empty")

empty.character <- function(x) str_length(x) == 0
empty.address_list <- function(x) length(x) == 0
empty.NULL <- function(x) TRUE
