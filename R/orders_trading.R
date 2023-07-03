

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
td_order <- function(symbol,
                     orderType = "LIMIT",
                     quantity = 0,
                     price,
                     instruction = "BUY",
                     account_no = "489837238",
                     access_token) {

    #apiverb needs to contemplate "GET, PUT, POST, DELETE, OPTIONS, HEAD, PATCH"

    url <- paste0("https://api.tdameritrade.com/v1/accounts/",account_no, "/orders") ## need to generalize this

    json_to_send <- jsonlite::toJSON(
        tibble::tibble(session = "NORMAL",
               duration = "DAY",
               orderType = orderType,
               complexOrderStrategyType = "NONE",
               quantity = quantity,
               price = price,
               requestedDestination = "AUTO",
               orderLegCollection = list(tibble::tibble(orderLegType = "EQUITY",
                                                instrument = tibble::tibble(assetType = "EQUITY",
                                                                    symbol = symbol),
                                                instruction = instruction,
                                                quantity = quantity)),
               orderStrategyType = "SINGLE")
    ) |> str_sub(2,-2) |> jsonlite::minify()

    #json_to_send
    r <- httr::POST(url = url, body = json_to_send, httr::add_headers( .headers =
                                                                           c("Authorization" = paste0("Bearer ", access_token),
                                                                             "Content-Type" = "application/json"))
    )
    r
}


#' Retrieve outstanding orders
#'
#' @return A tibble
#' @export
td_get_orders <- function(access_token) {
    url <- 'https://api.tdameritrade.com/v1/accounts/489837238?fields=orders'
    r <- httr::RETRY(verb = "GET", url = url, httr::add_headers(
        "Authorization" = paste0("Bearer ", access_token))
    )
    r
}

#' This will eventually get order status
#'
#' @return Will return a tibble
#' @export
td_order_status <- function(access_token) {}

#' This will eventually delete an order
#'
#' @param order_number
#' @param access_token A valid access token (they expire within 30 mins of refresh)
#'
#' @return Will return a tibble with a confirm #(?) and a timestamp
#' @export
td_delete_order <- function(order_number, access_token) {
    httr::RETRY(verb = "DELETE", order_url, httr::add_headers(Authorization  =
                                                  paste0("Bearer ", access_token))
    )
}

#' Takes a snapshot of account balance
#'
#' @param account_no Funciton currently defaults to sidePocket's act #
#' @param access_token Valid access_token, typically generated via update_refresh_tokens()
#'
#' @return a tibble()
#' @export
td_get_account_balances <- function(account_no = "489837238", access_token) {

    url <- paste0("https://api.tdameritrade.com/v1/accounts/",
                  account_no,
                  "?fields=",
                  url_encode("positions"))

    #json_to_send
    r <- httr::RETRY(verb = "GET", url = url, httr::add_headers(
        "Authorization" = paste0("Bearer ", access_token))
    )

    some_json <- httr::content(r, as = "text", encoding = "UTF-8")

    jsonlite::fromJSON(some_json)$securitiesAccount$currentBalances |>
        tibble::as_tibble() |>
        dplyr::mutate(timestamp = indratools2::datetime_to_ms(Sys.time()))
}
