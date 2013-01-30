\name{EHF}
\alias{EHF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EHF(analyte = data_subset, exposurename = "air_temperature_in_degrees_c_max_climatezone_av", datename = "date", referencePeriodStart = as.Date("1971-1-1"), referencePeriodEnd = as.Date("2000-12-31"), nlags = 32)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{analyte}{
%%     ~~Describe \code{analyte} here~~
}
  \item{exposurename}{
%%     ~~Describe \code{exposurename} here~~
}
  \item{datename}{
%%     ~~Describe \code{datename} here~~
}
  \item{referencePeriodStart}{
%%     ~~Describe \code{referencePeriodStart} here~~
}
  \item{referencePeriodEnd}{
%%     ~~Describe \code{referencePeriodEnd} here~~
}
  \item{nlags}{
%%     ~~Describe \code{nlags} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (analyte = data_subset, exposurename = "air_temperature_in_degrees_c_max_climatezone_av", 
    datename = "date", referencePeriodStart = as.Date("1971-1-1"), 
    referencePeriodEnd = as.Date("2000-12-31"), nlags = 32) 
{
    analyte$temp_lag0 <- analyte[, exposurename]
    exposuresList <- "temp_lag0"
    analyte <- arrange(analyte, analyte[, datename])
    for (lagi in 1:nlags) {
        exposuresList <- c(exposuresList, gsub("lag0", paste("lag", 
            lagi, sep = ""), exposuresList[1]))
        analyte[, (ncol(analyte) + 1)] <- Lag(analyte[, exposuresList[1]], 
            lagi)
    }
    exposuresList <- exposuresList[-1]
    names(analyte) <- c(names(analyte[, 1:(ncol(analyte) - nlags)]), 
        exposuresList)
    analyte$temp_movav <- rowMeans(analyte[, c("temp_lag0", "temp_lag1", 
        "temp_lag2")], na.rm = FALSE)
    analyte$temp30_movav <- rowMeans(analyte[, c("temp_lag3", 
        "temp_lag4", "temp_lag5", "temp_lag6", "temp_lag7", "temp_lag8", 
        "temp_lag9", "temp_lag10", "temp_lag11", "temp_lag12", 
        "temp_lag13", "temp_lag14", "temp_lag15", "temp_lag16", 
        "temp_lag17", "temp_lag18", "temp_lag19", "temp_lag20", 
        "temp_lag21", "temp_lag22", "temp_lag23", "temp_lag24", 
        "temp_lag25", "temp_lag26", "temp_lag27", "temp_lag28", 
        "temp_lag29", "temp_lag30", "temp_lag31", "temp_lag32")], 
        na.rm = FALSE)
    analyte <- na.omit(analyte)
    analyte$EHIaccl <- analyte$temp_movav - analyte$temp30_movav
    referencestart <- referencePeriodStart
    referenceend <- referencePeriodEnd
    analyte$dateidCol <- analyte[, datename]
    reference <- subset(analyte, dateidCol >= referencestart & 
        dateidCol <= referenceend, select = c("dateidCol", exposurename))
    head(reference)
    tail(reference)
    T95 <- quantile(reference[, exposurename], 0.95, na.rm = T)
    T95
    analyte$EHIsig <- analyte$temp_movav - T95
    analyte$EHF <- abs(analyte$EHIaccl) * analyte$EHIsig
    x <- analyte$EHIaccl >= 0
    xx <- (cumsum(!x) + 1) * x
    x2 <- (seq_along(x) - match(xx, xx) + 1) * x
    analyte$EHIacclCount <- x2
    x <- analyte$EHIsig >= 0
    xx <- (cumsum(!x) + 1) * x
    x2 <- (seq_along(x) - match(xx, xx) + 1) * x
    analyte$EHIsigCount <- x2
    EHFinverted <- analyte$EHF * -1
    y <- ifelse(EHFinverted >= 0, 0, analyte$EHF)
    f <- EHFinverted < 0
    f <- (cumsum(!f) + 1) * f
    z <- unsplit(lapply(split(y, f), cumsum), f)
    analyte$EHFintegrated <- z
    return(analyte)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
