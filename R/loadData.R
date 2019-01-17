#--------------------#
# LarryHua.com, 2019 #
#--------------------#

# Internal function used to process data loaded by read_algoseek_futures_fullDepth()
read_algoseek_futures_fullDepth_ <- function(rawdata)
{
  data <- rawdata
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

  data[, c(intersect(c("Ticker","Date","Side","Flags"), names(rawdata)),
          "h","m","s","ms",
          "p1","p2","p3","p4","p5","p6","p7","p8","p9","p10",
          "v1","v2","v3","v4","v5","v6","v7","v8","v9","v10",
          "o1","o2","o3","o4","o5","o6","o7","o8","o9","o10")]
}

#' Load AlgoSeek Futures Full Depth data from zip files
#' 
#' @param zipdata the original zip data provided by AlgoSeek
#' @param whichData the specific data to be loaded; by default load all data in the zip file
#' @examples
#' zipdata <- tempfile()
#' download.file("https://www.algoseek.com/static/files/sample_data/futures_and_future_options/ESH5.Futures.FullDepth.20150128.csv.zip",zipdata)
#' dat <- read_algoseek_futures_fullDepth(zipdata)
#' 
#' @export
read_algoseek_futures_fullDepth <- function(zipdata, whichData=NULL)
{
  if(is.null(whichData))
  {
    file_names <- unzip(zipdata, list = TRUE)
    data_file_names <- subset(file_names, Length>0)
    output_file_names <- gsub("/", "_", data_file_names$Name)
    
    alldata <- lapply(data_file_names$Name, function(file){
      rawdata <- read.table(unz(zipdata, file), header=T, sep=",")
      read_algoseek_futures_fullDepth_(rawdata)
    })
    names(alldata) <- output_file_names
  }else
  {
    output_file_names <- gsub("/", "_", whichData)
    alldata <- lapply(whichData, function(file){
      rawdata <- read.table(unz(zipdata, file), header=T, sep=",")
      read_algoseek_futures_fullDepth_(rawdata)
    })
    names(alldata) <- output_file_names
  }
  alldata
}
