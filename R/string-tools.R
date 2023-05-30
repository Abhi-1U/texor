#Wrapper functions for stringr package methods

# -- stringr wrapper ----------------------------------------------------------

str_trim <- function(x) {
    return(stringr::str_trim(x))
}
str_detect <- function(x, re) {
    return(stringr::str_detect(x, re))
}
str_match <- function(x, re) {
    return(stringr::str_match(x,re))
}
str_split <- function(x,patt) {
    return(stringr::str_split(x,patt))
}
str_length <- function(x) {
    return(stringr::str_length(x))
}
str_replace_all <- function(x, patt, rep) {
    return(stringr::str_replace_all(x, patt, rep))
}
fixed <- function(x) {
    return(stringr::fixed(x))
}
str_c <- function(x, sep) {
    return(stringr::str_c(x, sep))
}
empty <- function(x) UseMethod("empty")

empty.character <- function(x) str_length(x) == 0
empty.address_list <- function(x) length(x) == 0
empty.NULL <- function(x) TRUE
