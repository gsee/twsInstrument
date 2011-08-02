\name{twsClock}
\alias{twsClock}
\title{A clock utility to display the TWS server time...}
\usage{twsClock(format = "\%H:\%M:\%S", refresh = 1, verbose=TRUE)}
\description{A clock utility to display the TWS server time}
\details{adapted from the code in the links in references}
\value{live updating current time as reported by \code{reqCurrentTime}. Press escape to exit clock utility.}
\references{\url{http://stackoverflow.com/questions/5953718/overwrite-current-output-in-the-r-console}

\url{http://4dpiecharts.com/2011/05/11/a-clock-utility-via-console-hackery/}

\url{http://www.asciitable.com/}}
\arguments{\item{format}{format}
\item{refresh}{refresh rate}
\item{verbose}{be verbose about what clientId you are connected with?}}
\examples{\dontrun{
twsClock()
twsClock("\%A \%d \%B \%Y \%I:\%M:\%OS3 \%p", 1e-3)
}}
