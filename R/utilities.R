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
#'
#' @param df A dataframe
#' @param key The column dame of the df with the option symbols
#'
#' @return Modifies in a place a tibble
#' @export
#'
#' @examples
#' option_name_parser(data.frame(key = "TSLA_110119C315"), "key")
option_name_parser <- function(df, key) {
    # break up the key field in stages; ROS = 'Rest Of String'
    df2 <- tidyr::separate(df, key, into = c("stock", "ROS"), sep = "_", remove = F) %>%
        mutate(stock = stringr::str_remove(stock, '[0-9]'))

    df3 <- tidyr::separate(df2, ROS, into = c("expiry_date", "type", "strike"), sep = c(6, 7), convert = T)

    dplyr::mutate(df3, expiry_date = lubridate::mdy(expiry_date))

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
#' parser_response(jsonlite::fromJSON('{\"response\":[{\"service\":\"ADMIN\",\"requestid\":\"0\",\"command\":\"LOGIN\",\"timestamp\":1541541928754,\"content\":{\"code\":0,\"msg\":\"07-2\"}}]}'))
#' this is found at login after sending user principals
parser_response <- function(raw_parse){
    service <- raw_parse %>% map("service") %>% pluck(1)
    timestamp <- raw_parse %>% map("timestamp") %>% pluck(1) %>% as.character()
    content <- raw_parse %>% map("content") %>% pluck(1)

    data.frame(service, timestamp, content, stringsAsFactors = F)
}



# III. a list of length 1 named 'data':
# eg: "{\"data\":[{\"service\":\"OPTION\", \"timestamp\":1541604208281,\"command\":\"SUBS\",\"content\":[{\"key\":\"TSLA_111618P365\",\"11\":37407},{\"key\":\"TSLA_113018C332.5\",\"11\":37407},{\"key\":\"TSLA_113018P327.5\",\"10\":52.71542,\"11\":37407},{\"key\":\"TSLA_111618P320\",\"3\":3.2,\"10\":61.75262,\"11\":37408},{\"key\":\"TSLA_112318C337.5\",\"10\":47.973118,\"11\":37407},{\"key\":\"TSLA_110918P342.5\",\"11\":37407},{\"key\":\"TSLA_111618P315\",\"11\":37407},{\"key\":\"TSLA_110918P335\",\"2\":1.78,\"3\":1.84,\"10\":54.21732,\"11\":37408},{\"key\":\"TSLA_112318P352.5\",\"11\":37407},{\"key\":\"TSLA_111618C380\",\"2\":1.81,\"11\":37407},{\"key\":\"TSLA_111618C355\",\"10\":49.72311,\"11\":37408},{\"key\":\"TSLA_111618C350\",\"2\":10.8,\"11\":37407},{\"key\":\"TSLA_112318C327.5\",\"10\":50.44973,\"11\":37407},{\"key\":\"TSLA_111618C340\",\"11\":37408},{\"key\":\"TSLA_113018C390\",\"11\":37407},{\"key\":\"TSLA_110918C355\",\"10\":48.113895,\"11\":37407},{\"key\":\"TSLA_111618C315\",\"2\":35.9,\"11\":37408},{\"key\":\"TSLA_112318P390\",\"2\":40.65,\"3\":44.05,\"10\":43.714043,\"11\":37407},{\"key\":\"TSLA_111618C305\",\"2\":45.05,\"11\":37408},{\"key\":\"TSLA_113018C330\",\"11\":37407},{\"key\":\"TSLA_110918P317.5\",\"3\":0.38,\"10\":63.093018,\"11\":37407}]},{\"service\":\"QUOTE\", \"timestamp\":1541604208281,\"command\":\"SUBS\",\"content\":[{\"key\":\"TSLA\",\"4\":2,\"7\":\"V\",\"11\":37408}]}]}"

#' @rdname parser_response
parser_notify <- function(raw_parse){
    service <- "heartbeat"
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

#' @title Parse one line of received json into a tibble
#'
#' @description The TD streamer sends json which \code{master_parser()} 1), converts to a nested
#' list/df object per jsonlite::fromJSON() and then 2) using one of the associated helper functions
#' \code{parser_*} to flatten into a nicely rectangular tibble
#'
#' @param my_json Some json, as received from the TD streaming API
#'
#' @return A tibble
#' @export
#'
#' @examples
#' some_json <- '{\"notify\":[{\"heartbeat\":\"1541541939136\"}]}'
#' master_parser(some_json)
master_parser <- function(my_json) {
    raw_parse <- jsonlite::fromJSON(my_json)
    type <- names(raw_parse)
    #type
    result <- switch(type,
                     "data" = parser_data(raw_parse),
                     "notify" = parser_notify(raw_parse),
                     "response" = parser_response(raw_parse))
    result
}

#' Send requests to TD streamer
#'
#' @param parameter_keys Vector of symbols requested
#' @param requestid Default is "2"
#' @param service Defaults to "QUOTE"; Common values are "ADMIN", "QUOTE", "NEWS_HEADLINE", though a
#' complete listing can be found in section 4.4 of the
#' \href{https://developer.tdameritrade.com/content/streaming-data#_Toc504640602}{TD Ameritrade streaming API} docs.
#' @param command Default is "SUBS"
#' @param parameter_fields A numeric vector of requested fields, default is 0:11
#' @param pretty Defaults to FALSE. Used for inspecting the output/ debugging.
#' @param user_prins some user_principals; a list
#'
#' @return Output will be a string of json that is expected by the streamer
#' @export
#'
#' @examples
#' requestor(c("TSLA", "MSFT"))
#
#' requestor("TSLA_122118P400", service = "OPTION")
requestor <- function(parameter_keys,
                      requestid = "2",
                      service = "QUOTE",
                      command = "SUBS",
                      parameter_fields = c(0:11) ,
                      pretty = FALSE,
                      user_prins) {

    request_object <- list(requests =
                               tibble(service = service,
                                      requestid = requestid,
                                      command = command,
                                      account = user_prins[["accounts"]][[1]][["accountId"]],
                                      source = user_prins[["streamerInfo"]][["appId"]])
    )


    request_object$requests$parameters <- tibble(keys = paste(parameter_keys, collapse = ","),
                                                 fields = paste(parameter_fields, collapse = ","))

    jsonlite::toJSON(request_object, pretty = pretty)
}

#' Convert milliseconds since unix epoch to a datetime object
#'
#' @param milliseconds_since_epoch
#' @param tz
#'
#' @return a Date object
#' @export
#'
#' @examples ms <- 1559235837234
#' ms_to_datetime(ms)
ms_to_datetime <- function(milliseconds_since_epoch, tz = "America/Chicago") {
    tt <-  as.numeric(milliseconds_since_epoch)/ 1000

    #t <- strftime(tt,'%Y-%m-%d %H:%M:%OS3', tz = tz)

    as.POSIXct(tt, origin = "1970-01-01", tz = tz)
}

#' Convert a datetime object to milliseconds since unix epoch
#'
#' @param dttm
#'
#' @return a double
#' @export
#'
#' @examples
datetime_to_ms <- function(dttm) {
  options(digits = 13)
  t <- dttm %>% as.POSIXct() %>% as.numeric()
  t * 1000
}

