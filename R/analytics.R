#' Return a df of volume, mkt cap and mkt value traded
#'
#' @param symbol
#'
#' @return A tibble of the last n days' trading volume, HLC, and mkt cap
#' @export
#'
#' @examples
#' td_market_value_traded("MSFT")
td_market_value_traded <- function(symbol) {

    url1 <- paste0("https://api.tdameritrade.com/v1/marketdata/", symbol,"/pricehistory") # for price history
    url2 <- "https://api.tdameritrade.com/v1/instruments" # for mkt cap

    url1_w_params <- paste0(url1,"?apikey=",httpuv::encodeURIComponent("moonriver@AMER.OAUTHAP"),
                            "&periodType=", "day",
                            "&period=", 10, #
                            "&frequencyType=","daily",
                            "&frequency=",1)


    r_url1 <- httr::GET(url = url1_w_params, httr::add_headers(Authorization = paste0("Bearer ", refresh_tokens$access_token)))
    #r_url1
    httr::content(r_url1)

    # url2_w_params <- paste0(url2, "?symbol=", symbol, "&projection=", "fundamental")
    # r_url2 <- httr::GET(url = url2_w_params, httr::add_headers(Authorization = paste0("Bearer ", refresh_tokens$access_token)))
    # httr::content(r_url2)#$symbol$fundamental
}

#' Get a term structure of extant option contract symbols
#'
#' @param apikey The TD developer api key, aka the app name
#' @param symbol Symbol of the underlying, eg "TSLA"
#' @param contractType Default is "ALL"
#' @param strikeCount Number of strikes to return.  Default is 30.
#' @param includeQuotes Default is FALSE
#' @param strategy Default is "SINGLE"
#' @param interval Default is NULL
#' @param strike Default is NULL
#' @param range Default is "NTM" or near-the-money
#' @param monthsToGet Number of months into the future to get, defualt is 12
#' @param volatility Used for getting options greeks. Default is NULL
#' @param underlyingPrice Used for getting options greeks. Default is NULL
#' @param interestRate Used for getting options greeks. Default is NULL
#' @param daysToExpiration Used for getting options greeks. Default is NULL
#' @param expMonth Default is "ALL"
#' @param optionType "CALL", "PUT", or "ALL".  Default is "ALL"
#'
#' @return A chr vector of option symbol names.
#' @export
#'
#' @examples
#' td_get_option_chain("TSLA")
td_get_option_chain <- function(  symbol = "TSLA",
                                  apikey ="moonriver@AMER.OAUTHAP",
                                  contractType = "ALL",
                                  strikeCount = 30, # strikes above and below
                                  includeQuotes = FALSE,
                                  strategy = "SINGLE",
                                  interval = NULL,
                                  strike = NULL,
                                  range = "NTM",
                                  fromDate = Sys.Date(),
                                  monthsToGet = 12,
                                  volatility = NULL,
                                  underlyingPrice = NULL,
                                  interestRate = NULL,
                                  daysToExpiration = NULL,
                                  expMonth = "ALL",
                                  optionType = "ALL",
                                  access_token) {



    fields <- paste0("?apikey=", my_url_encode(apikey), "&symbol=", symbol,
                     "&contractType=", contractType, "&strikeCount=", strikeCount,
                     "&includeQuotes=", includeQuotes, "&strategy=", strategy, "&interval=", interval,
                     "&strike=", strike, "&range=", range, "&fromDate=", fromDate,
                     "&toDate=", fromDate + months(monthsToGet), "&volatility=", volatility,
                     "&underlyingPrice=", underlyingPrice, "&interestRate=", interestRate,
                     "&daysToExpiration=", daysToExpiration, "&expMonth=", expMonth, "&optionType=", optionType)


    url <- paste0(resource_root, 'marketdata/chains',fields)
    #url
    r <- httr::GET(url = url, httr::add_headers(
        .headers = c("Authorization" = paste0("Bearer ", access_token),
                     "Content-Type" = "application/json")
    )
    )
    #return(r)
    helper_f <- function(strike) { map_chr(strike, pluck, "symbol")}

    httr::content(r, as = "text") %>% jsonlite::fromJSON()
    the_calls <- httr::content(r, as = "text") %>% jsonlite::fromJSON() %>%
        pluck("callExpDateMap") %>% purrr:map(.f = helper_f) %>% unlist() %>% unname()

    the_puts <- httr::content(r, as = "text") %>% jsonlite::fromJSON() %>%
        purrr::pluck("putExpDateMap") %>% map(.f = helper_f) %>% unlist() %>% unname()

    c(the_calls, the_puts)

}
