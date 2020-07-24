#' Return a df of volume, mkt cap and mkt value traded
#'
#' @param symbol
#' @param n_years
#'
#' @return A tibble of the last n days' trading volume, HLC, and mkt cap
#' @export
#'
#' @examples
#' td_market_value_traded("MSFT", 2)
#' @importFrom magrittr "%>%"
td_market_value_traded <- function(symbol, n_years = 2) {

    url1 <- paste0("https://api.tdameritrade.com/v1/marketdata/", symbol,"/pricehistory") # for price history
    url2 <- "https://api.tdameritrade.com/v1/instruments" # for mkt cap

    url1_w_params <- paste0(url1,"?apikey=",
                            httpuv::encodeURIComponent("moonriver@AMER.OAUTHAP"),
                            "&periodType=", "year",
                            "&period=", 1, # will give one month
                            "&frequencyType=","daily",
                            "&frequency=",1)

    r_url1 <- httr::RETRY("GET", url = url1_w_params, times = 20)#, httr::add_headers(Authorization = paste0("Bearer ", refresh_tokens$access_token)))
    #r_url1
    returned_json <- httr::content(r_url1, as = "text")

    tidy_df <- jsonlite::fromJSON(returned_json)[[1]] %>%
        dplyr::mutate(stock = symbol, pretty_date = ms_to_datetime(datetime))

    url2_w_params <- paste0(url2,"?apikey=",httpuv::encodeURIComponent("moonriver@AMER.OAUTHAP"),
                                  "&symbol=", symbol,
                                  "&projection=", "fundamental")

    r_url2 <- httr::RETRY("GET", url = url2_w_params, times = 20) %>% httr::content() %>% purrr::flatten()

    ### TD doesn't offer historical mkt cap as a datum
    ### therefore we must estimate using hist px + shares out

    adj_shares_out <- r_url2$fundamental$marketCap / dplyr::last(tidy_df$close)

    tidy_df <- tidy_df %>% dplyr::mutate(shares_out_mm = adj_shares_out,
                                  market_cap_bn = shares_out_mm * close,
                                  value_traded_bn = volume * close /1000000000,
                                  val_div_mkt_cap = value_traded_bn/ market_cap_bn)

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
#' @param fromDate
#' @param access_token A valid access token (they expire within 30 mins of refresh)
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
    #r <- httr::GET(url = url, httr::add_headers(
        #.headers = c("Authorization" = paste0("Bearer ", access_token),
        #             "Content-Type" = "application/json"))
       #)

    r <- httr::RETRY("GET", url = url, httr::add_headers(
        .headers = c("Authorization" = paste0("Bearer ", access_token),
                     "Content-Type" = "application/json"))
    )

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

#' Get HLCO price history for a symbol
#'
#' @param apikey The TD developer api key, aka the app name
#' @param periodType Valid values are `day`, `month`, `year,` or `ytd`; default is day if blank
#' @param period The number of periods to show of PeriodType.
#' @param frequencyType Valid frequencyTypes for PeriodType: day: 1,2,3,4,5,10*
#'                                                           month: 1*, 2, 3, 6
#'                                                           year: 1*, 2, 3, 5, 10, 15, 20
#'                                                           ytd: 1*
#'
#' @param frequency Valid frequencies by frequencyType: minute: 1*, 5, 10, 15, 30
#'                                                      daily: 1*
#'                                                      weekly: 1*
#'                                                      monthly: 1*



#' @param endDate End date as milliseconds since epoch.
#' If startDate and endDate are provided, period should not be provided.
#' Default is previous trading day.
#' @param startDate Start date as milliseconds since epoch.
#' If startDate and endDate are provided, period should not be provided.
#' @param needExtendedHoursData Default is `true`. `false` for reg mkt hours only
#' @param access_token A valid access token (they expire within 30 mins of refresh)
#'
#' @return A tibble of dates and HLCO prices
#' @export
#'
#' @examples td_get_price_history("$SPX.X")
td_get_price_history <- function(symbol = "TSLA",
                                                       apikey = "moonriver@AMER.OAUTHAP",
                                                       periodType = "day",
                                                       period = 1,
                                                       frequencyType = "minute",
                                                       frequency=1,
                                                       endDate="",
                                                       startDate="",
                                                       needExtendedHoursData = "true",
                                                       access_token) {
    #  https://api.tdameritrade.com/v1/marketdata/$SPX.X/pricehistory?apikey=EXAMPLE&periodType=month&period=1&frequencyType=daily

    resource <- paste0("https://api.tdameritrade.com/v1/marketdata/", symbol, "/pricehistory")


    ## TO DO: need to add logic capturing "If startDate and endDate are provided,
    ## period should not be provided. Default is previous trading day." from TD API

    fields <- paste0("?apikey=", apikey,
                     "&periodType=", url_encode(periodType),
                     "&period=", url_encode(period),
                     "&frequencyType=", url_encode(frequencyType),
                     "&frequency=", url_encode(frequency),
                     #"&endDate=", url_encode(endDate),
                     #"&startDate=", url_encode(startDate),
                     "&needExtendedHoursData=", url_encode(needExtendedHoursData)
    )
    complete_url <- paste0(resource, fields)

    #complete_url

    # complete_url1 <- paste0("https://api.tdameritrade.com/v1/marketdata/$SPX.X/pricehistory?apikey=",
    #                         url_encode("moonriver@AMER.OAUTHAP"),
    #                         "&periodType=month&period=1&frequencyType=daily",
    #                         "&frequency=1&needExtendedHoursData=true")

    r <- httr::RETRY("GET", url = complete_url, httr::add_headers(
        .headers = c("Authorization" = paste0("Bearer ", access_token),
                     "Content-Type" = "application/json")))
    the_list <- httr::content(r)

    ## need to parse this into a df/ tibble
    the_list %>% purrr::pluck("candles") %>% purrr::map_df(as_tibble)

}
