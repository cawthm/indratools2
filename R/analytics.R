#' Return a df of volume, mkt cap and mkt value traded
#'
#' @param symbol
#'
#' @return A tibble of the last n days' trading volume, HLC, and mkt cap
#' @export
#'
#' @examples
#' td_market_value_traded("MSFT")
#' @importFrom magrittr "%>%"
td_market_value_traded <- function(symbol, refresh_tokens = .token_set1, sleep = .1) {

    url1 <- paste0("https://api.tdameritrade.com/v1/marketdata/", symbol,"/pricehistory") # for price history
    url2 <- "https://api.tdameritrade.com/v1/instruments" # for mkt cap

    url1_w_params <- paste0(url1,"?apikey=",httpuv::encodeURIComponent("moonriver@AMER.OAUTHAP"),
                            "&periodType=", "month",
                            "&period=", 1, # will give one month
                            "&frequencyType=","daily",
                            "&frequency=",1)

    #url1_w_params
    r_url1 <- httr::RETRY("GET", url = url1_w_params)#, httr::add_headers(Authorization = paste0("Bearer ", refresh_tokens$access_token)))
    #r_url1
    returned_json <- httr::content(r_url1, as = "text")

    tidy_df <- jsonlite::fromJSON(returned_json)[[1]] %>% dplyr::mutate(stock = symbol, pretty_date = ms_to_datetime(datetime))
    #tidy_df

    url2_w_params <- paste0(url2,"?apikey=",httpuv::encodeURIComponent("moonriver@AMER.OAUTHAP"),
                                  "&symbol=", symbol,
                                  "&projection=", "fundamental")
    Sys.sleep(sleep)
    r_url2 <- httr::RETRY("GET", url = url2_w_params) %>% httr::content() %>% purrr::flatten()
    #r_url2
    # flatten takes the symbol name out of the returned list, which is ugly from content()

    tidy_df <- tidy_df %>% dplyr::mutate(shares_out_mm = r_url2$fundamental$marketCapFloat,
                                  market_cap_bn = shares_out_mm * close/1000,
                                  value_traded_bn = volume * close /1000000000,
                                  val_div_mkt_cap = value_traded_bn/ market_cap_bn)
    Sys.sleep(sleep)
    tidy_df
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
                                  access_token = .token_set$access_token) {



    fields <- paste0("?apikey=", url_encode(apikey), "&symbol=", symbol,
                     "&contractType=", contractType, "&strikeCount=", strikeCount,
                     "&includeQuotes=", includeQuotes, "&strategy=", strategy, "&interval=", interval,
                     "&strike=", strike, "&range=", range, "&fromDate=", fromDate,
                     "&toDate=", fromDate + months(monthsToGet), "&volatility=", volatility,
                     "&underlyingPrice=", underlyingPrice, "&interestRate=", interestRate,
                     "&daysToExpiration=", daysToExpiration, "&expMonth=", expMonth, "&optionType=", optionType)

    resource_root <- "https://api.tdameritrade.com/v1/"
    url <- paste0(resource_root, 'marketdata/chains',fields)
    #url
    r <- httr::GET(url = url, httr::add_headers(
        .headers = c("Authorization" = paste0("Bearer ", access_token),
                     "Content-Type" = "application/json"))
        )

    #r

     the_calls <- httr::content(r, as = "text") %>% jsonlite::fromJSON() %>%
         purrr::pluck("callExpDateMap") %>%
         purrr::map(pluck) %>%
         map_df(dplyr::bind_rows)
    #
     the_puts <- httr::content(r, as = "text") %>% jsonlite::fromJSON() %>%
         purrr::pluck("putExpDateMap") %>%
         purrr::map(pluck) %>%
         map_df(dplyr::bind_rows)
    #
     tibble::as_tibble(dplyr::bind_rows(the_calls, the_puts))

}
