% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auth_connect.R
\name{poll_until_connected}
\alias{poll_until_connected}
\title{Wrapper to manage \code{ws$connect()} in a script}
\usage{
poll_until_connected(ws, timeout = 5)
}
\arguments{
\item{ws}{A websocket connection}

\item{timeout}{}
}
\value{
No return value, but ensures that the ws$readyState() == T before proceeding with script
}
\description{
Wrapper to manage \code{ws$connect()} in a script
}
