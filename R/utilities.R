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

#' Decode an option contract symbol into its constituents
#'
#' @description The TD API uses a certain format for its options symbology (), and this function
#' decomposes the symbol into a tibble with seven fields
#' @param option_string A TD style option symbol
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


#' Helper functions used within master_parser()
#'
#' @description The function \code{master_parser()} is the handler for a stream of json from the TD API.
#' Once having been initially parsed by an internal call to \code{jsonlite::fromJSON()}, master_parser
#' uses a call to \code{switch()} to one of these functions.
#'
#' @param raw_parse The heavily nested list/df object returned from \code{jsonlite::fromJSON()}
#'
#' @return A tibble
#' @export
#'
#' @examples
#' parser_response(jsonlite::fromJSON(some_json))
parser_response <- function(raw_parse){
    service <- raw_parse %>% map("service") %>% pluck(1)
    timestamp <- raw_parse %>% map("timestamp") %>% pluck(1) %>% as.character()
    content <- raw_parse %>% map("content") %>% pluck(1)

    data.frame(service, timestamp, content)
}

#' @rdname parser_response
parser_notify <- function(raw_parse){
    service <- "heartbest"
    timestamp <- raw_parse %>% map("heartbeat") %>% pluck(1) %>% as.character()
    tibble(service, timestamp, .rows = 1)
}

#' @rdname parser_response
parser_data <- function(raw_parse) {


    helper_fn <- function(service, content_df) {
        lookup <- switch(service, "OPTION" = option_field_defn, "QUOTE" = stock_field_defn)
        ## this post https://stackoverflow.com/questions/45535157/difference-between-dplyrrename-and-dplyrrename-all
        # was helpful for how to use rename_all()
        content_df %>% set_names( lookup[names(content_df)] )
    }

    service <- raw_parse %>% map("service") %>% pluck(1)
    timestamp <- raw_parse %>% map("timestamp") %>% pluck(1) %>% as.character()
    body <- raw_parse %>% map("content") %>% pluck(1)


    body <- map2(service, body, .f = helper_fn)

    tibble(service, timestamp, body) %>% unnest()

}

