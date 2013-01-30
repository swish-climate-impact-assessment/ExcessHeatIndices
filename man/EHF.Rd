
\name{EHF}
\alias{EHF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Excess Heat Factor
}
\description{
The EHF is an extension to a high pass filter, compared with long term percentiles.
}
\usage{
EHF(analyte = data_subset, exposurename = "air_temperature_in_degrees_c_max_climatezone_av", datename = "date", referencePeriodStart = as.Date("1971-1-1"), referencePeriodEnd = as.Date("2000-12-31"), nlags = 32)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{analyte}{
dataframe
}
  \item{exposurename}{
the name of the exposure variable in the dataframe
}
  \item{datename}{
usually just date
}
  \item{referencePeriodStart}{
start of baseline climate reference period
}
  \item{referencePeriodEnd}{
end of baseline
}
  \item{nlags}{
number of lags, default is 32
}
}
\details{

}
\value{
A dataframe.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
ivanhanigan, original by John Nairn (Australian Bureau of Meteorology)
}
\note{
%%  ~~further notes~~
}



\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

output <- EHF(analyte = data_subset, exposurename = "air_temperature_in_degrees_c_max_climatezone_av", 
    datename = "date", referencePeriodStart = as.Date("1971-1-1"), 
    referencePeriodEnd = as.Date("2000-12-31"), nlags = 32) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
