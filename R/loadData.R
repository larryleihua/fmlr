#-------------------------#
# Larry Lei Hua 2019-2020 #
#-------------------------#

# Internal function used to process data loaded by read_algoseek_futures_fullDepth()
read_algoseek_futures_fullDepth_ <- function(data)
{
  data$h <- floor(data$Timestamp / 1e7) %% 1e2
  data$m <- floor(data$Timestamp / 1e5) %% 1e2
  data$s <- floor(data$Timestamp / 1e3) %% 1e2
  data$ms <- data$Timestamp %% 1e3
  
  data$p1 <- as.numeric(gsub(" .*$", "", trimws(data$Level1)))
  data$p2 <- as.numeric(gsub(" .*$", "", trimws(data$Level2)))
  data$p3 <- as.numeric(gsub(" .*$", "", trimws(data$Level3)))
  data$p4 <- as.numeric(gsub(" .*$", "", trimws(data$Level4)))
  data$p5 <- as.numeric(gsub(" .*$", "", trimws(data$Level5)))
  data$p6 <- as.numeric(gsub(" .*$", "", trimws(data$Level6)))
  data$p7 <- as.numeric(gsub(" .*$", "", trimws(data$Level7)))
  data$p8 <- as.numeric(gsub(" .*$", "", trimws(data$Level8)))
  data$p9 <- as.numeric(gsub(" .*$", "", trimws(data$Level9)))
  data$p10 <- as.numeric(gsub(" .*$", "", trimws(data$Level10)))
  
  data$v1 <- as.numeric(gsub( " .*.$", "", gsub("*.* x ", "", data$Level1)))
  data$v2 <- as.numeric(gsub( " .*.$", "", gsub("*.* x ", "", data$Level2)))
  data$v3 <- as.numeric(gsub( " .*.$", "", gsub("*.* x ", "", data$Level3)))
  data$v4 <- as.numeric(gsub( " .*.$", "", gsub("*.* x ", "", data$Level4)))
  data$v5 <- as.numeric(gsub( " .*.$", "", gsub("*.* x ", "", data$Level5)))
  data$v6 <- as.numeric(gsub( " .*.$", "", gsub("*.* x ", "", data$Level6)))
  data$v7 <- as.numeric(gsub( " .*.$", "", gsub("*.* x ", "", data$Level7)))
  data$v8 <- as.numeric(gsub( " .*.$", "", gsub("*.* x ", "", data$Level8)))
  data$v9 <- as.numeric(gsub( " .*.$", "", gsub("*.* x ", "", data$Level9)))
  data$v10 <- as.numeric(gsub( " .*.$", "", gsub("*.* x ", "", data$Level10)))
  
  tmp <- stringr::str_extract_all(data$Level1, "\\([^()]+\\)")
  data$o1 <- as.numeric(substring(tmp,2,nchar(tmp)-1))
  
  tmp <- stringr::str_extract_all(data$Level2, "\\([^()]+\\)")
  data$o2 <- as.numeric(substring(tmp,2,nchar(tmp)-1))
  
  tmp <- stringr::str_extract_all(data$Level3, "\\([^()]+\\)")
  data$o3 <- as.numeric(substring(tmp,2,nchar(tmp)-1))
  
  tmp <- stringr::str_extract_all(data$Level4, "\\([^()]+\\)")
  data$o4 <- as.numeric(substring(tmp,2,nchar(tmp)-1))
  
  tmp <- stringr::str_extract_all(data$Level5, "\\([^()]+\\)")
  data$o5 <- as.numeric(substring(tmp,2,nchar(tmp)-1))
  
  tmp <- stringr::str_extract_all(data$Level6, "\\([^()]+\\)")
  data$o6 <- as.numeric(substring(tmp,2,nchar(tmp)-1))
  
  tmp <- stringr::str_extract_all(data$Level7, "\\([^()]+\\)")
  data$o7 <- as.numeric(substring(tmp,2,nchar(tmp)-1))
  
  tmp <- stringr::str_extract_all(data$Level8, "\\([^()]+\\)")
  data$o8 <- as.numeric(substring(tmp,2,nchar(tmp)-1))
  
  tmp <- stringr::str_extract_all(data$Level9, "\\([^()]+\\)")
  data$o9 <- as.numeric(substring(tmp,2,nchar(tmp)-1))
  
  tmp <- stringr::str_extract_all(data$Level10, "\\([^()]+\\)")
  data$o10 <- as.numeric(substring(tmp,2,nchar(tmp)-1))

  data[, c(intersect(c("Ticker","Date","Side","Flags"), names(data)),
          "h","m","s","ms",
          "p1","p2","p3","p4","p5","p6","p7","p8","p9","p10",
          "v1","v2","v3","v4","v5","v6","v7","v8","v9","v10",
          "o1","o2","o3","o4","o5","o6","o7","o8","o9","o10")]
}

#' Load AlgoSeek Futures Full Depth data from zip files
#' 
#' @param zipdata the original zip data provided by AlgoSeek
#' @param whichData the specific data to be loaded; by default load all data in the zip file
#' 
#' 
#' @examples
#' \donttest{
#' zipdata <- tempfile()
#' download.file("https://www.algoseek.com/static/files/sample_data/
#' futures_and_future_options/ESH5.Futures.FullDepth.20150128.csv.zip",zipdata)
#' dat <- read_algoseek_futures_fullDepth(zipdata)
#' }
#'
#' # Do not run unless the file 20160104.zip is avaliable
#' # dat <- read_algoseek_futures_fullDepth("20160104.zip", whichData="ES/ESH6.csv")
#' 
#' @author Larry Lei Hua
#' 
#' @export
read_algoseek_futures_fullDepth <- function(zipdata, whichData=NULL)
{
  col_types <- readr::cols(
    Timestamp = readr::col_integer(),
    Ticker = readr::col_character(),
    Side = readr::col_character(),
    Flags = readr::col_integer(),
    Depth = readr::col_integer(),
    Level1 = readr::col_character(),
    Level2 = readr::col_character(),
    Level3 = readr::col_character(),
    Level4 = readr::col_character(),
    Level5 = readr::col_character(),
    Level6 = readr::col_character(),
    Level7 = readr::col_character(),
    Level8 = readr::col_character(),
    Level9 = readr::col_character(),
    Level10 = readr::col_character()
  )
  
  if(is.null(whichData))
  {
    file_names <- utils::unzip(zipdata, list = TRUE)
    data_file_names <- subset(file_names, file_names$Length>0)
    output_file_names <- gsub("/", "_", data_file_names$Name)
    alldata <- lapply(data_file_names$Name, function(file){
      rawdata <- readr::read_csv(unz(zipdata, file), col_types=col_types)
      read_algoseek_futures_fullDepth_(rawdata)
    })
    names(alldata) <- output_file_names
  }else
  {
    output_file_names <- gsub("/", "_", whichData)
    alldata <- lapply(whichData, function(file){
      rawdata <- readr::read_csv(unz(zipdata, file), col_types=col_types)
      read_algoseek_futures_fullDepth_(rawdata)
    })
    names(alldata) <- output_file_names
  }
  alldata
}

# Internal function used to process data loaded by read_algoseek_equity_taq()
read_algoseek_equity_taq_ <- function(data)
{
  data$h <- lubridate::hour(data$Timestamp)
  data$m <- lubridate::minute(data$Timestamp)
  data$s <- lubridate::second(data$Timestamp)
  data$ms <- data$s%%1
  data$s <- data$s - data$ms


  # covert conditions to readable codes
  # R.utils::intToBin(strtoi(c('A0','00','20','20'), base = 16L))

  data
}


#' Load AlgoSeek Equity TAQ data from zip files
#' 
#' @param zipdata the original zip data provided by AlgoSeek
#' @param whichData the specific data to be loaded; by default load all data in the zip file
#' 
#' @examples
#' \donttest{
#' zipdata <- tempfile()
#' download.file("https://www.algoseek.com/static/files/sample_data/equity_and_etf_etn/IBM.TradesQuotes.012815.zip",zipdata)
#' dat <- read_algoseek_equity_taq(zipdata)
#' }
#
#' # Do not run unless the file 20180108.zip is avaliable
#' # dat <- read_algoseek_equity_taq("20180108.zip", whichData="AMZN.csv")
#' 
#' @author Larry Lei Hua
#' 
#' @export
read_algoseek_equity_taq <- function(zipdata, whichData=NULL)
{
  col_types <- readr::cols(
    Date = readr::col_date(format="%Y%m%d"),
    Timestamp = readr::col_time(format="%H:%M:%OS"),
    EventType = readr::col_character(),
    Ticker = readr::col_character(),
    Quantity = readr::col_integer(),
    Exchange = readr::col_character(),
    Conditions = readr::col_character()
  )
  
  if(is.null(whichData))
  {
    file_names <- utils::unzip(zipdata, list = TRUE)
    data_file_names <- subset(file_names, file_names$Length>0)
    output_file_names <- gsub("/", "_", data_file_names$Name)
    alldata <- lapply(data_file_names$Name, function(file){
      rawdata <- readr::read_csv(unz(zipdata, file), col_types=col_types)
      read_algoseek_equity_taq_(rawdata)
    })
    names(alldata) <- output_file_names
  }else
  {
    output_file_names <- gsub("/", "_", whichData)
    alldata <- lapply(whichData, function(file){
      rawdata <- readr::read_csv(unz(zipdata, file), col_types=col_types)
      read_algoseek_equity_taq_(rawdata)
    })
    names(alldata) <- output_file_names
  }
  alldata
}


# Internal function used to process data loaded by read_algoseek_equity_fullDepth()
read_algoseek_equity_fullDepth_ <- function(data)
{
  data$h <- lubridate::hour(data$Timestamp)
  data$m <- lubridate::minute(data$Timestamp)
  data$s <- lubridate::second(data$Timestamp)
  data$ms <- data$s %% 1
  data$s <- data$s - data$ms
  data
}

#' Load AlgoSeek equity Full Depth data from zip files
#' 
#' @param zipdata the original zip data provided by AlgoSeek
#' @param whichData the specific data to be loaded; by default load all data in the zip file
#' 
#' 
#' @examples
#' \donttest{
#' zipdata <- tempfile()
#' download.file("https://www.algoseek.com/static/files/sample_data/equity_and_etf_etn/IBM.FullDepth.20140128.csv.zip",zipdata)
#' dat <- read_algoseek_equity_fullDepth(zipdata)
#' }
#'
#' # Do not run unless the file 20180108.zip is avaliable
#' # dat <- read_algoseek_equity_fullDepth("20180108.zip", whichData="AMZN.csv")
#' 
#' @author Larry Lei Hua
#' 
#' @export
read_algoseek_equity_fullDepth <- function(zipdata, whichData=NULL)
{
  col_types <- readr::cols(
    Date = readr::col_number(),
    Timestamp = readr::col_time(format="%H:%M:%OS"),
    OrderNumber = readr::col_number(),
    EventType = readr::col_character(),
    Ticker = readr::col_character(),
    Price = readr::col_number(),
    Quantity = readr::col_integer(),
    MPID = readr::col_character(),
    Exchange = readr::col_character()
  )
  
  if(is.null(whichData))
  {
    file_names <- utils::unzip(zipdata, list = TRUE)
    data_file_names <- subset(file_names, file_names$Length>0)
    output_file_names <- gsub("/", "_", data_file_names$Name)
    alldata <- lapply(data_file_names$Name, function(file){
      rawdata <- readr::read_csv(unz(zipdata, file), col_types=col_types)
      read_algoseek_equity_fullDepth_(rawdata)
    })
    names(alldata) <- output_file_names
  }else
  {
    output_file_names <- gsub("/", "_", whichData)
    alldata <- lapply(whichData, function(file){
      rawdata <- readr::read_csv(unz(zipdata, file), col_types=col_types)
      read_algoseek_equity_fullDepth_(rawdata)
    })
    names(alldata) <- output_file_names
  }
  alldata
}

#' Obtain readable condition codes for AlgoSeek equity TAQ
#' 
#' @param con raw condition codes such as "a0000020" provided with Algoseek data
#' @return a vector of integers for the condition codes defined by algoseek
#' 
#' @examples
#' condition_algoseek_equity_taq("a0000020")
#' 
#' @author Larry Lei Hua
#' 
#' @export
condition_algoseek_equity_taq <- function(con)
{
  tmp <- strsplit(con, "")[[1]]
  tmp <- paste0(tmp[c(TRUE, FALSE)], tmp[c(FALSE, TRUE)])
  codes <- rev((31:0)[unlist(binaryLogic::as.binary(strtoi(tmp, base = 16L), n=8))])
  codes
}


#' Map conditions to conditions codes for AlgoSeek equity TAQ
#' 
#' @param codes a vector of integers for readable condition codes
#' @param tq a string to indicate "trade" or "quote"; default is "trade"
#' @return a vector of meanings of the condition codes defined by algoseek
#' 
#' @examples
#' condition_maps_algoseek_equity_taq(c(0, 9), tq="trade")
#' condition_maps_algoseek_equity_taq(c(0, 9), tq="quote")
#' 
#' @author Larry Lei Hua
#' 
#' @export
condition_maps_algoseek_equity_taq <- function(codes, tq="trade")
{
  cond_trade <- c("tRegular", 
                  "tCash",
                  "tNextDay",
                  "tSeller",
                  "tYellowFlag",
                  "tIntermarketSweep",
                  "tOpeningPrints",
                  "tClosingPrints",
                  "tReOpeningPrints",
                  "tDerivativelyPriced",
                  "tFormT",
                  "tSold",
                  "tStopped",
                  "tExtendedHours",
                  "tOutOfSequence",
                  "tSplit",
                  "tAcquisition",
                  "tBunched",
                  "tStockOption",
                  "tDistribution",
                  "tAveragePrice",
                  "tCross",
                  "tPriceVariation",
                  "tRule155",
                  "tOfficialClose",
                  "tPriorReferencePrice",
                  "tOfficialOpen",
                  "tCapElection",
                  "tAutoExecution",
                  "tTradeThroughExempt",
                  "",
                  "tOddLot")
  
  cond_quote <- c("qRegular",
                  "qSlow",
                  "qGap",
                  "qClosing",
                  "qNewsDissemination",
                  "qNewsPending",
                  "qTradingRangeIndication",
                  "qOrderImbalance",
                  "qClosedMarketMaker",
                  "qVolatilityTradingPause",
                  "qNonFirmQuote",
                  "qOpeningQuote",
                  "qDueToRelatedSecurity",
                  "qResume",
                  "qInViewOfCommon",
                  "qEquipmentChangeover",
                  "qSubPennyTrading",
                  "qNoOpenNoResume",
                  "qLimitUpLimitDownPriceBand",
                  "qRepublishedLimitUpLimitDownPriceBand",
                  "qManual",
                  "qFastTrading",
                  "qOrderInflux")
  if(tq=="trade")
  {
    out <- cond_trade[codes+1]
  }else
  {
    out <- cond_quote[codes+1]
  }
  out
}








