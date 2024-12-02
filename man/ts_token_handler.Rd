% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auth_connect.R
\name{ts_token_handler}
\alias{ts_token_handler}
\title{Obtain a new refresh token (which don't expire) OR a new access token (lasts 20 mins).}
\usage{
ts_token_handler(
  grant_type = "authorization_code",
  client_id,
  client_secret,
  code = code,
  redirect_uri = "http://localhost:3001/",
  refresh_token = NULL,
  path_to_file = ""
)
}
\arguments{
\item{grant_type}{The grant type.
Possible values are \code{"refresh_token"} (the usual case) or \code{"authorization_code"}, used
if a new refresh_token is needed.}

\item{client_id}{The TS api key for the account}

\item{client_secret}{The TS secret associated with with account}

\item{code}{A special one-use code obtained from manual browser auth flow (see example below)}

\item{redirect_uri}{Typically localhost}

\item{refresh_token}{The refresh token}

\item{path_to_file}{Where the token(s) should be saved}
}
\value{
Has a side effect of saving a new .rds file(s), named "ts_token.rds" and,
if \code{"client_id"} is set to "\code{"authorization_code"}"
}
\description{
Following Tradestation's \[api documention for auth\](https://api.tradestation.com/docs/fundamentals/authentication/auth-code/#1-redirect-user-for-authenticationauthorization),
we will usually use this function with a persistent \code{"refresh_token"}.
}
