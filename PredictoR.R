#' @title Predicts Stock Price Movement
#'
#' @description This package predicts where the stock price will be at tomorrow compared to today's closing price.
#'
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples stock_predict('AAPL')
#'
#' @export

stock_predict <- function(symbol)
{
 options(warn=-1)
 options("getSymbols.warning4.0"=FALSE)

 data<-data.frame(xts::as.xts(get(quantmod::getSymbols(symbol))))

 colnames(data) <- c("data.Open","data.High","data.Close","data.Volume","data.Adjusted")

 data <- xts::xts(data,order.by=as.Date(rownames(data)))
 data <- as.data.frame(merge(data, lm1=stats::lag(data[,'data.Adjusted'],c(-1,1,3,5,10))))

 data$Date<-as.Date(rownames(data))
 data$Day_of_month<-as.integer(format(as.Date(data$Date),"%d"))
 data$Month_of_year<-as.integer(format(as.Date(data$Date),"%m"))
 data$Year<-as.integer(format(as.Date(data$Date),"%y"))
 data$Day_of_week<-as.factor(weekdays(data$Date))

 today <- 'data.Adjusted'
 tommorow <- 'data.Adjusted.5'

 data$up_down <- as.factor(ifelse(data[,tommorow] > data[,today], 1, 0))

 train<-data[stats::complete.cases(data),]
 test<-data[nrow(data),]

 model<-stats::glm(up_down~data.Open+data.High+data.Low+data.Close+
                     data.Volume+data.Adjusted+data.Adjusted.1+
                     data.Adjusted.2+data.Adjusted.3+data.Adjusted.4+
                     Day_of_month+Month_of_year+Year+Day_of_week,
                   family=binomial(link='logit'),data=train)

 pred<-as.numeric(stats::predict(model,test[,c('data.Open','data.High','data.Low','data.Close','data.Volume','data.Adjusted','data.Adjusted.1','data.Adjusted.2','data.Adjusted.3','data.Adjusted.4','Day_of_month','Month_of_year','Year','Day_of_week')],type = 'response'))

 print("Probability of Stock price going up tommorow:")
 print(pred)
}
