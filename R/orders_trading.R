

#' Primary trading function USE WITH CAUTION
#'
#' @param symbol A security symbol, eg "IBM", "GS", "TSLA_110119C315"
#' @param orderType Can be "LIMIT" or "MARKET
#' @param quantity Number of shares OR contracts in case of options
#' @param price Applies to LIMIT orders only; the price at which to execute
#' @param instruction "BUY" or "SELL" or *MANY* others
#' @param account_no TD account #
#'
#' @return The reply to the POST command includes an order # in the header
#' @export
#'
#' @examples
#' td_order(symbol = "TSLA", "LIMIT", 10, price = 300, "SELL")
td_order <- function(symbol, orderType = "LIMIT", quantity = 0, price, instruction = "BUY", account_no = "489837238") {

    #apiverb needs to contemplate "GET, PUT, POST, DELETE, OPTIONS, HEAD, PATCH"

    url <- paste0("https://api.tdameritrade.com/v1/accounts/",account_no, "/orders") ## need to generalize this

    json_to_send <- jsonlite::toJSON(
        tibble(session = "NORMAL",
               duration = "DAY",
               orderType = orderType,
               complexOrderStrategyType = "NONE",
               quantity = quantity,
               price = price,
               requestedDestination = "AUTO",
               orderLegCollection = list(tibble(orderLegType = "EQUITY",
                                                instrument = tibble(assetType = "EQUITY",
                                                                    symbol = "TSLA"),
                                                instruction = instruction,
                                                quantity = quantity)),
               orderStrategyType = "SINGLE")
    ) %>% str_sub(2,-2) %>% jsonlite::minify()

    #json_to_send
    r <- httr::POST(url = url, body = json_to_send, httr::add_headers( .headers =
                                                                           c("Authorization" = paste0("Bearer ", refresh_tokens$access_token),
                                                                             "Content-Type" = "application/json"))
    )
    r
}


#' Retrieve outstanding orders
#'
#' @return A tibble
#' @export
#'
#' @examples
#' td_get_orders(refresh_tokens$access_tokens)
td_get_orders <- function(access_token) {
    url <- 'https://api.tdameritrade.com/v1/accounts/489837238?fields=orders'
    r <- httr::GET(url = url, httr::add_headers(
        "Authorization" = paste0("Bearer ", access_token))
    )
    r
}

#' This will eventually get order status
#'
#' @return Will return a tibble
#' @export
#'
#' @examples
#' td_order_status(access_token)
td_order_status <- function(access_token) {}

#' This will eventually delete an order
#'
#' @param order_url
#'
#' @return Will return a tibble with a confirm #(?) and a timestamp
#' @export
#'
#' @examples
#' td_delete_order(order_number = "123456")
td_delete_order <- function(order_number) {
    httr::DELETE(order_url, httr::add_headers(Authorization  =
                                                  paste0("Bearer ", refresh_tokens$access_token))
    )
}

td_get_account <- function(account_no = "489837238", fields = c("positions", "orders")) {

    url <- paste0("https://api.tdameritrade.com/v1/accounts/",
                  account_no,
                  "fields=",
                  paste(fields, collapse = ","))

    #json_to_send
    r <- httr::GET(url = url, httr::add_headers( .headers =
                                                c("Authorization" = paste0("Bearer ", refresh_tokens$access_token),
                                                                             "Content-Type" = "application/json"))
    )
    r
}
