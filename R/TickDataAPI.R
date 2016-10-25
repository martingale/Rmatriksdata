#' Fetch Data
#'
#' Fetches data from historical data service
#'
#' @param dateRange A character vector of length two \code{c("YYYY-MM-DD","YYYY-MM-DD")}.
#' @param symbol A character vector of length 1 that denotes a symbol.
#' @param dataType A character value to analyze various types of data .One of " \code{trade}"," \code{bestbidoffer}"," \code{depth}", " \code{openinterest}" and " \code{bar}".
#' @param period A character value is used with bar datatype to analyze data in different time segments.One of "\code{1min}","\code{5min}","\code{1hour}" and "\code{1day}".
#' @return A data frame object is returned in "\code{trade}","\code{bestbidoffer}","\code{depth}","\code{openinterest}" and "\code{bar}" dataTypes.
#' @seealso \code{\link{trade}}, \code{\link{bestbidoffer}}, \code{\link{depth}}, \code{\link{openinterest}}, \code{\link{bar}}
#' @import httr
#' @importFrom jsonlite fromJSON
getData<-function(dateRange,symbol,type,period=NULL,...){
  auth<-suppressWarnings(tryCatch(strsplit(readBin("~/matriks/.tkn","character"),",")[[1]][2],
                                  error=function(e) {auth<-getToken()}))

  # require(httr)
  # require(jsonlite)

  attemptCount <- 10
  startdate <- dateRange[1]
  enddate <- dateRange[2]
  zipbegin <- paste(type,"data",sep = "_")
  zipend <- symbol
  dir.create("~/matriks/data",showWarnings = F)
  pathHead<-"~/matriks/data/"

  if(type=="bar"){
    path <- paste(zipbegin,startdate,enddate,zipend,period,sep="_")
    urlhead <- paste("http://web1.matriksdata.com/dumrul-api/v1/tick/",type,".gz?",sep = "")
  }else{
    path <- paste(zipbegin,startdate,enddate,zipend,sep="_")
    urlhead <- paste("http://web1.matriksdata.com/dumrul-api/v1/tick/",type,".zip?",sep = "")
  }

  start <- paste("start=",startdate,sep="")
  end <- paste("end=",enddate,sep="")
  urlend <-paste("symbol=",symbol,sep = "")
  if(type == "bar"){
    urlperiod <- paste("period=",period,sep = "")
    mainurl <- paste(urlend,urlperiod,start,end,sep="&")
    mainurl <- paste(urlhead,mainurl,sep="")
  }else{
    mainurl <- paste(urlend,start,end,sep = "&")
    mainurl <- paste(urlhead,mainurl,sep="")
  }

  counter <- 1
  while(T){
    req <- GET(mainurl, add_headers(Authorize = paste("jwt",auth)))
    if(req$status_code == 200){
      break
    }else{
      if(counter >= attemptCount){
        stop("Maximum number of attempts reached!")
      }
      if(req$status_code == 401){
        auth<-getToken()
      }else if(req$status_code == 400){
        stop("Bad Request")
      }else if(req$status_code == 503){
        stop("Service Unavailable")
      }

      counter <- counter + 1
    }
  }

  if(type == "bar"){
    return(fromJSON(content(req,as = "text")))
  }else{
    writeBin(content(req, "raw"), paste(pathHead,path,".zip",sep = ""))
    meta <- read.csv2(unz(paste(pathHead,path,".zip",sep=""), paste(path,".csv",sep = "")), header=TRUE ,sep=";",as.is = T)
    file.remove(paste(pathHead,path,".zip",sep=""))
    return(meta)
  }
}

#' Fetch Trade Data
#'
#' Fetches trade data from historical data service.
#'
#' @param symbol A character vector of length 1 that denotes a symbol.
#' @param dateRange A character vector of length two  \code{c("YYYY-MM-DD","YYYY-MM-DD")}.
#' @return A data frame object is returned that contains bid or ask,trade id, price quantity and signal time columns.
#' @seealso \code{\link{bestbidoffer}}, \code{\link{depth}}, \code{\link{openinterest}}, \code{\link{bar}}.
trade <- function (dateRange,symbol){
  meta<-getData(dateRange,symbol,"trade")
  meta[,5]<-as.numeric(meta[,5])

  meta[,1]<-as.POSIXct(strftime(sub("T"," ",strtrim(meta[,1],width = 19)),"%Y-%m-%d %H:%M:%S",tz = "GMT"),tz = "GMT")
  tableIndex<-table(meta[,1])

  tableDuration<-sapply(tableIndex,function(x)cumsum(rep(1000/x,x))-1000/x)

  meta[,1]<-as.POSIXct((as.numeric(meta[,1])*1000+unlist(tableDuration))/1000,origin="1970-01-01 00:00:00",tz = "GMT")

  closeAllConnections()
  return(meta)
}

#' Fetch Bestbidoffer Data
#'
#' Fetches bestbidoffer data from historical data service
#'
#' @param symbol  A character vector of length 1 that denotes a symbol.
#' @param dateRange A character vector of length two \code{c("YYYY-MM-DD","YYYY-MM-DD")}.
#' @return A data frame object is returned that contains best bid and offer prices with quantity.
#' @seealso  \code{\link{trade}}, \code{\link{depth}}, \code{\link{openinterest}}, \code{\link{bar}}.
bestbidoffer <- function (dateRange,symbol){
  meta<-getData(dateRange,symbol,"bestbidoffer")
  meta[,1]<-as.character(meta[,1])
  meta[,2]<-as.character(meta[,2])
  meta[,3]<-as.numeric(as.character(meta[,3]))
  meta[,4]<-as.numeric(meta[,4])
  meta[,5]<-as.numeric(as.character(meta[,5]))
  meta[,6]<-as.numeric(meta[,6])

  meta[,1]<-as.POSIXct(strftime(sub("T"," ",strtrim(meta[,1],width = 19)),"%Y-%m-%d %H:%M:%S",tz = "GMT"),tz = "GMT")
  tableIndex<-table(meta[,1])
  tableDuration<-sapply(tableIndex,function(x)cumsum(rep(1000/x,x))-1000/x)

  meta[,1]<-as.POSIXct((as.numeric(meta[,1])*1000+unlist(tableDuration))/1000,origin="1970-01-01 00:00:00",tz = "GMT")
  colnames(meta) <- c("timestamp","symbol","best_bid_price","best_bid_size","best_ask_price","best_ask_price")

  closeAllConnections()
  return(meta)
}

#' Fetchn Depth Data
#'
#' Fetches depth data from historical data service
#'
#' @param symbol A character vector of length 1 that denotes a symbol.
#' @param dateRange A character vector of length two \code{c("YYYY-MM-DD","YYYY-MM-DD")}.
#' @return A data frame object is returned with row number,action,bid or ask,order count,price and quantity columns.
#' @details
#' Symbols in \bold{BISTPP} and \bold{BISTVIOP}, are available on depth dataType.
#' @seealso  \code{\link{trade}}, \code{\link{bestbidoffer}}, \code{\link{openinterest}}, \code{\link{bar}}.
depth <- function (dateRange,symbol){
  meta<-getData(dateRange,symbol,"depth")

  meta[,7]<-as.numeric(meta[,7])

  meta[,1]<-as.POSIXct(strftime(sub("T"," ",strtrim(meta[,1],width = 19)),"%Y-%m-%d %H:%M:%S",tz = "GMT"),tz = "GMT")
  tableIndex<-table(meta[,1])
  tableDuration<-sapply(tableIndex,function(x)cumsum(rep(1000/x,x))-1000/x)

  meta[,1]<-as.POSIXct((as.numeric(meta[,1])*1000+unlist(tableDuration))/1000,origin="1970-01-01 00:00:00",tz = "GMT")

  closeAllConnections()
  return(meta[,-2])
}

#' Fetch Openinterest Data
#'
#' Fetches openinterest data from historical data service
#'
#' @param symbol A character vector of length 1 that denotes a symbol.
#' @param dateRange A character vector of length two \code{c("YYYY-MM-DD","YYYY-MM-DD")}.
#' @return An xts object is returned that contains numbers of open positions.
#' @details
#' "Symbols in BISTVIOP","FUTURES" and "SERBEST", are available on openinterest dataType.
#' @seealso \code{\link{trade}}, \code{\link{bestbidoffer}}, \code{\link{depth}},\code{\link{bar}}.
openinterest <- function (dateRange,symbol){
  auth<-suppressWarnings(tryCatch(strsplit(readBin("~/matriks/.tkn","character"),"==")[[1]][2],
                                  error=function(e) {auth<-getToken()}))
  startdate<-dateRange[1]
  enddate<-dateRange[2]
  zipbegin <-"openinterest_data"
  zipend <- symbol
  dir.create("~/matriks/data",showWarnings = F)
  pathHead<-"~/matriks/data/"
  path <-paste(zipbegin,startdate,enddate,zipend,sep="_")

  urlhead <- "https://web1.matriksdata.com/dumrul-api/v1/tick/openinterest.zip?"

  start <- paste("start=",startdate,sep="")
  end <- paste("end=",enddate,sep="")
  urlend <-paste("symbol=",symbol,sep = "")
  mainurl <- paste(urlend,start,end,sep="&")
  mainurl <- paste(urlhead, mainurl,sep="")

  req <- GET(mainurl, add_headers(Authorize = paste("jwt",auth)))
  if(req$status_code!=200){
    auth<-getToken()
    req <- GET(mainurl, add_headers(Authorize = paste("jwt",auth)))
  }

  writeBin(content(req, "raw"), paste(pathHead,path,".zip",sep = ""))

  meta <- read.csv2(unz(paste(pathHead,path,".zip",sep=""), paste(path,".csv",sep = "")), header=TRUE ,sep=";")

  file.remove(paste(pathHead,path,".zip",sep=""))

  meta[,1]<-as.character(meta[,1])
  meta[,2]<-as.character(meta[,2])
  meta[,3]<-as.numeric(as.character(meta[,3]))

  meta[,1]<-as.POSIXct(strftime(sub("T"," ",strtrim(meta[,1],width = 19)),"%Y-%m-%d %H:%M:%S",tz = "GMT"),tz = "GMT")
  tableIndex<-table(meta[,1])
  tableDuration<-sapply(tableIndex,function(x)cumsum(rep(1000/x,x))-1000/x)

  meta[,1]<-as.POSIXct((as.numeric(meta[,1])*1000+unlist(tableDuration))/1000,origin="1970-01-01 00:00:00",tz = "GMT")
  Sys.setenv(TZ = "GMT")

  colnames(meta) <- "Open Positions"
  closeAllConnections()
  return(meta)
}

#' Fetch Bar Data
#'
#' Fetches  bar data from historical data service
#'
#' @param symbol A character vector of length 1 that denotes a symbol.
#' @param dateRange A character vector of length two \code{c("YYYY-MM-DD","YYYY-MM-DD")}.
#' @param period A character value is used with bar datatype to analyze data in different time segments.One of "\code{1min}","\code{5min}","\code{1hour}" and "\code{1day}".
#' @return A data frame object is returned that contains open,high,low,close,quantity and weighted average price columns.
#' @seealso \code{\link{trade}}, \code{\link{bestbidoffer}}, \code{\link{depth}}, \code{\link{openinterest}}.
bar <- function (dateRange,symbol,period){
  meta<-getData(dateRange,symbol,"bar",period)
  date <- as.POSIXlt(meta$time/1000,tz = "GMT",origin = "1970-01-01")
  meta <- meta[-c(1:2)]
  meta <- cbind(date,meta)
  closeAllConnections()
  return(meta)
}
