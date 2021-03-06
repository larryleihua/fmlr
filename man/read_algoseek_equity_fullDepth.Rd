% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadData.R
\name{read_algoseek_equity_fullDepth}
\alias{read_algoseek_equity_fullDepth}
\title{Load AlgoSeek equity Full Depth data from zip files}
\usage{
read_algoseek_equity_fullDepth(zipdata, whichData = NULL)
}
\arguments{
\item{zipdata}{the original zip data provided by AlgoSeek}

\item{whichData}{the specific data to be loaded; by default load all data in the zip file}
}
\description{
Load AlgoSeek equity Full Depth data from zip files
}
\examples{
\donttest{
zipdata <- tempfile()
download.file("https://www.algoseek.com/static/files/sample_data/equity_and_etf_etn/IBM.FullDepth.20140128.csv.zip",zipdata)
dat <- read_algoseek_equity_fullDepth(zipdata)
}

# Do not run unless the file 20180108.zip is avaliable
# dat <- read_algoseek_equity_fullDepth("20180108.zip", whichData="AMZN.csv")

}
\author{
Larry Lei Hua
}
