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

    url <- paste0(resource_root, "oauth2/token")


    data_payload <- paste0("grant_type=", grant_type,
                           "&refresh_token=", my_url_encode(refresh_token),
                           "&access_type=", access_type,
                           "&code=", my_url_encode(code),
                           "&client_id=", my_url_encode(client_id),
                           "&redirect_uri=", my_url_encode(redirect_uri))


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
#'
#' @examples
#' ws <- create_websocket("the_socket_url")
create_websocket <- function(socket_url, autoConnect = FALSE) {
    websocket::WebSocket$new(paste0("wss://", socket_url, "/ws"), autoConnect = autoConnect)
}

