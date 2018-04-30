#' @title Predicts Stock Price Movement
#'
#' @description This package predicts where the stock price will be at tomorrow compared to today's closing price.
#'
#' @param symbol
#'
#' @return NULL
#'
#' @export

stock_predict <- function(symbol)
{
 options(warn=-1)
 options("getSymbols.warning4.0"=FALSE)

 data<-data.frame(xts::as.xts(get(quantmod::getSymbols(symbol))))

 colnames(data) <- c("data.Access","data.High","data.Close","data.Amount","data.Adjust")

 data <- xts::xts(data,order.by=as.Date(rownames(data)))
 data <- as.data.frame(merge(data, lm1=stats::lag(data[,'data.Adjust'],c(-1,1,3,5,10))))

 data$Date<-as.Date(rownames(data))
 data$Day_of_month<-as.integer(format(as.Date(data$Date),"%d"))
 data$Month_of_year<-as.integer(format(as.Date(data$Date),"%m"))
 data$Year<-as.integer(format(as.Date(data$Date),"%y"))
 data$Day_of_week<-as.factor(weekdays(data$Date))

 today <- 'data.Adjust'
 tommorow <- 'data.Adjust.5'

 data$up_down <- as.factor(ifelse(data[,tommorow] > data[,today], 1, 0))

 train<-data[stats::complete.cases(data),]
 test<-data[nrow(data),]

 model<-stats::glm(up_down~data.Access+data.High+data.Low+data.Close+
                     data.Amount+data.Adjust+data.Adjust.1+
                     data.Adjust.2+data.Adjust.3+data.Adjust.4+
                     Day_of_month+Month_of_year+Year+Day_of_week,
                   family=binomial(link='logit'),data=train)

 pred<-as.numeric(stats::predict(model,test[,c('data.Access','data.High','data.Low','data.Close','data.Amount','data.Adjust','data.Adjust.1','data.Adjust.2','data.Adjust.3','data.Adjust.4','Day_of_month','Month_of_year','Year','Day_of_week')],type = 'response'))

 print("Probability of Stock price going up:")
 print(pred)
}
