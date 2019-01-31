# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Encode a URL
#'
#' @param string A character string
#'
#' @return A character-encoded string, as required by some web APIs.
#' @export
#'
#' @examples
#' url_encode("http://localhost:8080")
url_encode <- function(string) {
    if (is.null(string) == TRUE) {
        NULL
    } else {
        httpuv::encodeURIComponent(httpuv::decodeURIComponent(string))  }
}

#' This decodes an option contract symbol into its useful constituent data
#'
#' @param option_string A
#'
#' @return A 1x7 tibble with the following fields: stock, strike, type,
#' expire_month, expire_day. expire_year, and expire_date
#' @export
#'
#' @examples
#' option_name_parser("TSLA_110119C315")
option_name_parser <- function(option_string) {
    if (stringr::str_detect(option_string, "_") == F | is.na(option_string == T)) {
        return(
            tibble::tibble(stock = NA, strike = NA, type = NA, expire_month = NA, expire_day = NA, expire_year = NA))
    } else {
        type <- stringr::str_extract(option_string, "[$C$P]{1}")
        x <- stringr::str_split(option_string, type)[[1]]
        xx <- stringr::str_split(x[[1]], "\\_")[[1]]
        stock <- xx[[1]]
        strike <- as.numeric(x[[2]])
        expire_month <- stringr::str_sub(xx[[2]], start = 1, end = 2) %>% as.integer()
        expire_day <- stringr::str_sub(xx[[2]], start = 3, end = 4) %>% as.integer()
        expire_year <- stringr::str_sub(xx[[2]], start = 5, end = 6) %>% as.numeric() + 2000
        expire_date <- lubridate::ymd(paste(expire_year, expire_month, expire_day, sep = "-"))

        return (
            tibble(stock, strike, type, expire_month, expire_day, expire_year, expire_date)
        )
    }
}

