#' Token Request
#'
#' Get authorization token to get started using Matriks services
#'
#' @return The  token is returned.
#' @export
#' @import base64enc
getToken<-function(){
  authCond <- F
  while(!authCond){
    dir.create("~/matriks",showWarnings = F)
    if(file.exists("~/matriks/.tkn")){
      up <- readBin("~/matriks/.tkn","character")
      up <- strsplit(up,",")[[1]][1]
    }else{
      username <- readline("Username:")
      pass <- readline("Password:")
      up <- base64encode(charToRaw(paste(username,pass,sep = ":")))
      print(paste("encoded user-pass:", up))
    }
    reqH <- add_headers(Authorize = paste("Basic",up), dummy = "D")
    names(reqH$headers)[2] <- "X-Client-Type"
    req <- GET("http://api.matriksdata.com/login", reqH)
    if(req$status_code==401){
      stop("Matriks login service status: 401")
    }
    response <- content(req,encoding = "UTF-8")
    token <- response[[2]]
    authCond <- response[[1]]
    if(!authCond){
      print("Wrong username or password")
      file.remove("~/matriks/.tkn")
    }else{
      print("Correct username and password")
      token <- token[nchar(token)==max(nchar(token))]
    }
  }
  writeBin(paste(up,token,sep = ","),"~/matriks/.tkn")
  closeAllConnections()
  cat(paste("Token:",token))
  token
}

#' matriksData
#'
#' Fetches various price data from Matriks Historical Data Service
#'
#' @param ticker A character variable for ticker symbol.
#' @param dataType A character type variable. One of the followings: "\code{trade}","\code{bestbidoffer}","\code{depth}", "\code{openinterest}", or "\code{bar}".
#' @param startDate A character object as "\code{YYYY-MM-DD}"
#' @param endDate A character object as "\code{YYYY-MM-DD}"
#' @param period A character variable. Valid only when datatype arguement is set to "\code{bar}", otherwise ignored. One of the following character variables: "\code{1min}","\code{5min}","\code{1hour}" , or "\code{1day}".
#' @details A \code{matriksData} object is a data frame that contains \code{trade}, \code{bestbidoffer}, \code{depth}, \code{openinterest}
#'  and \code{bar}.
#'
#' If dataType is \code{trade}, data frame contains seven column: \code{timestamp},
#' \code{symbol}, \code{bid_or_ask}, \code{trade_no}, \code{price}, \code{quantity} and \code{signal_time}. \code{timestamp} column includes
#' transaction time.\code{bid_or_ask} column delineates whether the trade occured on the b(id) or on the a(sk) side. \code{signal_time} shows the number of seconds elapsed
#' since \code{01/01/1970}.
#'
#' By setting dataType to \code{bestbidoffer}  one has the following columns:
#' six columns:\code{timestamp},\code{symbol},\code{best_bid_price},\code{best_bid_size},\code{best_ask_price},\code{best_ask_size}.
#'
#' If dataType is set to \code{openinterest}, total amount of nominal positions on options or futures are returned.
#'
#' If dataType is set to \code{bar}, the function fetches aggregated prices (open, high, close, low) and total volume information for the chosen \code{period} level of aggregation.
#' @return A data.frame object is is returned. See details above.
#' @note Although, as of now, Matriks Data Services can provide with historical data since the year 2013, the data coverage is more likely to expand in time.
#' @seealso \code{\link{trade}}, \code{\link{bestbidoffer}}, \code{\link{depth}}, \code{\link{openinterest}}, \code{\link{bar}}
#' @examples ##NOT RUN
#' # matriksData("GARAN","bar","2016-01-22","2016-01-23","1hour")
#' # matriksData("GARAN","trade","2016-01-22","2016-01-23")
#' ##NOT RUN
#' @export
matriksData<-function(ticker, dataType, startDate, endDate=Sys.Date(), period=NULL) {
  if(is.null(endDate)){
    endDate <- startDate
  }
  if(dataType=="bar"){
    if((is.null(period)) | !any(c('1min','5min','1hour','1day',NULL)==period)){
      stop("Please choose period time one of the followings: '1min','5min','1hour' or '1day'")
    }
  }
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  if(dataType!="bar") endDate <- endDate + 1; startDate <- startDate - 1

  if(endDate<startDate) {

    tempDate <- startDate
    startDate <- endDate
    endDate<-tempDate
    warning("endDate and startDate values are swapped because endDate is earlier than startDate")
  }
  dateRange <- c(startDate,endDate)
  if(!(endDate-startDate)<1){ # 7
    dateRange<-seq(startDate,endDate,by="weeks") # weeks
    if(!dateRange[length(dateRange)]==endDate){
      dateRange <- c(dateRange,endDate)
    }
  }


  tmp<-lapply(1:(length(dateRange)-1),FUN = function(x){
    if(dataType=="bar"){
      temp<-do.call(dataType,list(dateRange=c(dateRange[x] + 1,dateRange[x+1]),symbol = ticker,period=period))
    }else{
      temp<-do.call(dataType,list(dateRange=c(dateRange[x],dateRange[x+1]),symbol = ticker))
    }
    return(temp)
  })
  tmp<-do.call(rbind,tmp)
  # try(saveRDS(tmp,file = paste(symbol,".rds",sep = "")))

  if(nrow(tmp)==0) {
    warning("The time range of the data may be public holiday and/or it may be future dates and/or there is no Matriks data available corresponding to time range chosen.")
  }
  tmp
}

