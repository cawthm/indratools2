#' Obtain a new refresh token (which don't expire) OR a new access token (lasts 20 mins).
#'
#' Following Tradestation's \[api documention for auth\](https://api.tradestation.com/docs/fundamentals/authentication/auth-code/#1-redirect-user-for-authenticationauthorization),
#' we will usually use this function with a persistent \code{"refresh_token"}.
#'
#'
#' @param grant_type The grant type.
#' Possible values are \code{"refresh_token"} (the usual case) or \code{"authorization_code"}, used
#' if a new refresh_token is needed.
#' @param client_id The TS api key for the account
#' @param client_secret The TS secret associated with with account
#' @param code A special one-use code obtained from manual browser auth flow (see example below)
#' @param redirect_uri Typically localhost
#' @param refresh_token The refresh token
#' @param path_to_file Where the token(s) should be saved
#'
#' @return Has a side effect of saving a new .rds file(s), named "ts_token.rds" and,
#' if \code{"client_id"} is set to "\code{"authorization_code"}"
#' @export
ts_token_handler <- function(grant_type = "authorization_code",
                             client_id,
                             client_secret,
                             code = code,
                             redirect_uri = "http://localhost:3001/",
                             refresh_token = NULL,
                             path_to_file = "") {

    url <- "https://signin.tradestation.com/oauth/token"

    # Set the headers
    headers <- c('Content-Type' = 'application/x-www-form-urlencoded')

    # Set the body
    params <- list(
        grant_type = grant_type,
        client_id = client_id,
        client_secret = client_secret
    )

    if (!is.null(code)) {
        params$code <- code
        params$redirect_uri <- redirect_uri
    } else if (!is.null(refresh_token)) {
        params$refresh_token <- refresh_token
    }

    # Make the POST request
    response <- httr::POST(url, httr::add_headers(headers), body = params, encode = "form")
    response2 <- httr::content(response)
    #

    readr::write_rds(response2, paste0(path_to_file, "ts_tokens.rds"))

    if (grant_type == "authorization_code" && httr::status_code(response) < 300) {
        readr::write_rds(response2$refresh_token, paste0(path_to_file,"refresh_token.rds"))
    }
    response2
}



#' Obtain access or refresh tokens from the TD API
#'
#' @param grant_type The grant type of the oAuth scheme.
#' Possible values are \code{"authorization_code"} or \code{"refresh_token"}
#' @param refresh_token Required if using refresh token grant
#' @param access_type Set to \code{"offline"} to receive a refresh token
#' @param code Required if trying to use authorization code grant
#' @param client_id OAuth User ID of the application, a parameter also known as 'apikey' in the
#' TD API documentation
#' @param redirect_uri Required if trying to use authorization code grant, will typically be
#' \code{"http://localhost:8080"} on the local machine
#'
#'
#' @return A five element named list: \code{access_token, refresh_token, expires_in, refresh_token_expires_in, token_type }
#' @export
#'
td_post_access_token <- function(grant_type, refresh_token, access_type,
                                 code, client_id, redirect_uri) {

    resource_root <- "https://api.tdameritrade.com/v1/"
    url <- paste0(resource_root, "oauth2/token")


    data_payload <- paste0("grant_type=", grant_type,
                           "&refresh_token=", url_encode(refresh_token),
                           "&access_type=", access_type,
                           "&code=", url_encode(code),
                           "&client_id=", url_encode(client_id),
                           "&redirect_uri=", url_encode(redirect_uri))


    r <- httr::POST(url = url, body = data_payload, httr::content_type('application/x-www-form-urlencoded'))

    return(httr::content(r))

}

#' Get user principals method from TD API.
#' @param access_token Authentication token obtained from successful call to \code{td_post_access_token()}
#' @param fields Typically always "streamerSubscriptionKeys,streamerConnectionInfo"; See the
#' \href{https://developer.tdameritrade.com/user-principal/apis/get/userprincipals-0}{TD documentation} for more.
#'
#' @description This is part of the TD authentication handshake process, retrieving authentication
#' credentials to the streaming TD API.
#' @return Returns a nested list of authentication parameters
#' @export
#'
td_get_user_principals <- function(access_token,
                                   fields = "streamerSubscriptionKeys,streamerConnectionInfo") {
    resource_root <- "https://api.tdameritrade.com/v1/"
    url <- paste0(resource_root, "userprincipals","?fields=", url_encode(fields))
    # url
    r <- httr::GET(url = url, httr::add_headers(Authorization = paste0("Bearer ", access_token)))
    httr::content(r)

}

#' Wrapper around \code{websocket::WebSocket$new}
#'
#' @param socket_url Typically always "streamer-ws.tdameritrade.com", this is nevertheless returned
#' by a successful call to \code{td_get_user_principals()}, nested at \code{user_prins[["streamerInfo"]][["streamerSocketUrl"]]}
#' @param autoConnect Default is FALSE, given that we'll typically set up out handler functions (eg
#' \code{ws$onMessage(), ws$onClose}) before invoking \code{ws$connect()}
#'
#' @return An R6 websocket object, against which methods are called.  See \href{https://github.com/rstudio/websocket}{the rstudio/websocket github repo}
#' for more info.
#' @export

create_websocket <- function(socket_url, autoConnect = FALSE) {
    websocket::WebSocket$new(paste0("wss://", socket_url, "/ws"), autoConnect = autoConnect)
}


#' Update a token set
#'
#' @param path_to_file Local file path to token set.
#'
#' @return A new set of tokens.
#' @export
#'
#' @description This function does two things simultaneously: 1) returns an updated set of tokens
#' and 2), as a side effect, saves/ replaces the old token set via \code{saveRDS()} at the same location
#' indicated by \code{path_to_file}
#'
update_refresh_tokens <- function(path_to_file = "") {
    old_token <- readRDS(path_to_file) # (old but still valid, that is)

    new_token <- td_post_access_token(grant_type = "refresh_token",
                                      refresh_token = old_token$refresh_token,
                                      access_type = "offline",
                                      code = NULL,
                                      client_id = "moonriver@AMER.OAUTHAP",
                                      redirect_uri = NULL)

    saveRDS(new_token, path_to_file)

    new_token
}

#' Create json string for inital websocket handshake
#'
#' @param user_prins The list of login credentials returned by a successful call to \code{td_get_user_principals()}.
#' @param pretty
#'
#' @return json
#' @export
credentials_request_string <- function(user_prins, pretty = FALSE) {
    tokenTimeStampSeconds <-
        user_prins[["streamerInfo"]][["tokenTimestamp"]] |>
        lubridate::as_datetime(.) |> as.numeric()

    tokenTimeStampAsMs <-
        (tokenTimeStampSeconds * 1000) |> toString()

    credentials <-
        tibble::tibble(
            userid = user_prins[["accounts"]][[1]][["accountId"]],
            token = user_prins[["streamerInfo"]][["token"]],
            company = user_prins[["accounts"]][[1]][["company"]],
            segment = user_prins[["accounts"]][[1]][["segment"]],
            cddomain = user_prins[["accounts"]][[1]][["accountCdDomainId"]],
            usergroup = user_prins[["streamerInfo"]][["userGroup"]],
            accesslevel = user_prins[["streamerInfo"]][["accessLevel"]],
            authorized = "Y",
            timestamp = tokenTimeStampAsMs,
            appid = user_prins[["streamerInfo"]][["appId"]],
            acl = user_prins[["streamerInfo"]][["acl"]]
        )

    ## the first login to ws needs a request object
    # constructing the request object from the login stuff above

    credentials_to_string <-
        paste(names(credentials),
              credentials,
              sep = "=",
              collapse = "&") |>
        httpuv::encodeURIComponent()

    request_build <- list(requests = tibble::tibble(service =  "ADMIN",
                                            command =  "LOGIN",
                                            requestid = 0,
                                            account = user_prins[["accounts"]][[1]][["accountId"]],
                                            source = user_prins[["streamerInfo"]][["appId"]]))

    request_build$requests$parameters <- tibble::tibble(
        credential = credentials_to_string,
        token = user_prins[["streamerInfo"]][["token"]],
        version = "1.0",
        quoslevel = 0)

    jsonlite::toJSON(request_build, pretty = pretty)
}

#' Wrapper to manage \code{ws$connect()} in a script
#'
#' @param ws A websocket connection
#' @param timeout
#'
#' @return No return value, but ensures that the ws$readyState() == T before proceeding with script
#' @export
poll_until_connected <- function(ws, timeout = 5) {
    connected <- FALSE
    end <- Sys.time() + timeout
    while (!connected && Sys.time() < end) {
        # Need to run the event loop for websocket to complete connection.
        later::run_now(0.1)

        ready_state <- ws$readyState()
        if (ready_state == 0L) {
            # 0 means we're still trying to connect.
            # For debugging, indicate how many times we've done this.
            cat(".")
        } else if (ready_state == 1L) {
            connected <- TRUE
        } else {
            break
        }
    }

    if (!connected) {
        stop("Unable to establish websocket connection.")
    }
}

poll_until_disconnected <- function(ws, timeout = 5) {
    disconnected <- FALSE
    end <- Sys.time() + timeout
    while (disconnected == 0L && Sys.time < end) {
        later::run_now(0.1)
        state <- func(...)[[1]]
    }
    if (done_state == 0L) {
        cat(".")
    } else if (done_state == 1L) {
        done_state <- TRUE
    } else {
        break
    }
}
