dffundamental <- read.csv("C:/Users/Muhammad Shahbaz/Downloads/nyse/fundamentals.csv")
dfprices <-read.csv("C:/Users/Muhammad Shahbaz/Downloads/nyse/prices.csv")
dfspadjprices <-read.csv("C:/Users/Muhammad Shahbaz/Downloads/nyse/prices-split-adjusted.csv")
dfsecurities <- read.csv("C:/Users/Muhammad Shahbaz/Downloads/nyse/securities.csv")
dfsecurities <- dfsecurities[-c(3,6,7)]

#Total Companies by Sector

factor(dfsecurities$GICS.Sector)
tblsecfreq=as.data.frame(table(dfsecurities$GICS.Sector))
names(tblsecfreq) = c("Sector","nComp")
library(ggplot2)
ggplot(data=tblsecfreq,aes(x=tblsecfreq$Sector,y=tblsecfreq$nComp,fill=tblsecfreq$Sector))+
  geom_bar(stat="identity")+
  labs(x="Sector",y="No. of Companies",title="Number of Companies in Each Sector of NYSE for the Year 2016",
       fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))


# Getting Stock Prices fro 2016

dfprices_2k16 <- subset(dfprices,dfprices$year=="2016")
dfspadjprices_2k16<-subset(dfspadjprices, substring(dfspadjprices$date,1,4)=="2016")

#Merging on NYSE Symbols to get data in one data frame for analysis
dfstkdata <- merge(dfprices_2k16,dfsecurities,by.x="symbol",by.y="Ticker.symbol")

#Categorizing by sector
as.factor(dfstkdata$GICS.Sector)

#Adding Daily Growth to data frame for further analysis
dfstkdata$dailygrowth <- dfstkdata$close - dfstkdata$open

#Getting sector-wise mean/average for daily growth of stock prices
dfsecgrmean <- aggregate(dfstkdata$dailygrowth,by=dfstkdata["GICS.Sector"],FUN=mean)
library(ggplot2)
ggplot(data =dfsecgrmean,
       aes(x=dfsecgrmean$GICS.Sector,y=x,fill=dfsecgrmean$GICS)) + geom_bar(stat = "identity") + 
       labs(x="Sector",y="Average Growth Rate",title="Sector-Wise Average Stock Price Growth in NYSE for the Year 2016",
            fill="Legend")+
       scale_color_gradientn(colours = rainbow(6))


#Categorizing data by month first
dfstkdata$month <- substring(dfstkdata$date,6,7)
as.factor(dfstkdata$month)
levels(dfstkdata$month)<- month.abb[1:12]

#Calculating month-wise Average of daily growth for all sectors
dfmswise<-aggregate(dfstkdata$dailygrowth,by=list(id1=dfstkdata$GICS.Sector,
                                                  id2=dfstkdata$month),FUN=mean)
#Adding Month Abbreviations
dfmswise$month <- month.abb[as.numeric(dfmswise$id2)]

#Calculating month-wise Average of daily growth of all Companies
dfmscwise <- aggregate(dfstkdata$dailygrowth,by=list(id1=dfstkdata$symbol,
                                                     id2=dfstkdata$month),FUN=mean)
#Adding month abbreviations
dfmscwise$month <- month.abb[as.numeric(dfmscwise$id2)]


#Creating data frames Sector-Wise
dfIT <- subset(dfmswise,dfmswise$id1 == "Information Technology")
dfHealCare <- subset(dfmswise,dfmswise$id1=="Health Care")
dfConDis <- subset(dfmswise,dfmswise$id1 == "Consumer Discretionary")
dfConSta <- subset(dfmswise,dfmswise$id1 == "Consumer Staples")
dfEnergy <- subset(dfmswise,dfmswise$id1 == "Energy")
dfFinancials <- subset(dfmswise,dfmswise$id1 == "Financials")
dfIndustrials <- subset(dfmswise,dfmswise$id1== "Industrials")
dfMaterials <- subset(dfmswise,dfmswise$id1 == "Materials")
dfRealEstate <- subset(dfmswise,dfmswise$id1 == "Real Estate")
dfTelSer <- subset(dfmswise,dfmswise$id1=="Telecommunications Services")
dfUtil <- subset(dfmswise,dfmswise$id1 == "Utilities")

#Sorting Month-wise showing the month in right sequence in the plot
dfmswise <- dfmswise[order(dfmswise$id2),]

#plot for month wise stock value growth for all nyse sectors
ggplot(dfmswise, aes(x=dfmswise$month, y=dfmswise$x, group=dfmswise$id1)) +
  geom_line(aes(color=dfmswise$id1))+
  geom_point(aes(color=dfmswise$id1))+
  scale_x_discrete(limits = month.abb)+
  scale_color_discrete("Legend")+
  labs(x="Months",y="Average Growth Rate",
       title="Month-wise Stocks Value Growth for all NYSE Sectors the Year 2016",fill="Legend")



#Functions to return top 6 companies sector wise
top5SecWise <- function(sector){
              dfsectorDG <- subset(dfstkdata,dfstkdata$GICS.Sector == sector)
              dfsectorDGMean <- aggregate(dfsectorDG$dailygrowth,dfsectorDG["symbol"],FUN = mean)
              #dfsectorDGMean$name <- dfsecurities$Security[as.character(dfsecurities$Ticker.symbol) %in% as.character(dfsectorDGMean$id1) ]
              dftop5 <- tail(dfsectorDGMean[order(dfsectorDGMean$x),])
              return(dftop5)
}
totalSecWise <- function(sector){
                dfsectorDG <- subset(dfstkdata,dfstkdata$GICS.Sector == sector)
                dfsectorDGMean <- aggregate(dfsectorDG$dailygrowth,dfsectorDG["symbol"],FUN = mean)
                return(dfsectorDGMean)
}

#Function for making line chart
makelinechart <- function(dfSector){
  
  ggplot(dfSector, aes(x=dfSector$month, y=dfSector$x, group=dfSector$id1)) +
    geom_line(aes(color=dfSector$id1))+
    geom_point(aes(color=dfSector$id1))+
    scale_x_discrete(limits = month.abb)+
    scale_color_discrete("Legend")+
    labs(x="Months",y="Average Growth Rate",
         title="Month-wise Stocks Value Growth for the Year 2016",fill="Legend")
}

makelinechart2 <- function(dfSector){
  dfSector$name <- dfsecurities$Security[as.character(dfsecurities$Ticker.symbol) %in% as.character(dfSector$id1) ]
  ggplot(dfSector, aes(x=dfSector$month, y=dfSector$x, group=dfSector$name)) +
    geom_line(aes(color=dfSector$name))+
    geom_point(aes(color=dfSector$name))+
    scale_x_discrete(limits = month.abb)+
    scale_color_discrete("Legend")+
    labs(x="Months",y="Average Growth Rate",
         title="Month-wise Stocks Value Growth for the Year 2016",fill="Legend")
}
#Analysis for IT
dftop5IT <- top5SecWise("Information Technology")
dftop5IT$symbol <- factor(dftop5IT$symbol)
dftop5ITdetail <-subset(dfmscwise,as.character(dfmscwise$id1) %in% as.character(dftop5IT$symbol))
makelinechart(dfIT)


ggplot(dftop5IT,aes(x=dftop5IT$symbol,y=x,fill= dftop5IT$symbol))+
  geom_bar(stat = "identity")+ labs(x="Companies",y="Average Growth Rate",
                                     title="Top 6 Stock Values in Information Sector",fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))
library(ggplot2)
makelinechart2(dftop5ITdetail)

#Plot of for Healtcare and Graph of Top 5
makelinechart(dfHealCare)
dftop5hc <- top5SecWise("Health Care")
makelinechart2(subset(dfmscwise,as.character(dfmscwise$id1) %in% as.character(dftop5hc$symbol)))


ggplot(dftop5hc,aes(x=dftop5hc$symbol,y=x,fill=dftop5hc$symbol))+
geom_bar(stat = "identity")+ labs(x="Companies",y="Average Growth Rate",
                                  title="Top 6 Stock Values in Health Care Sector",fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))

#Analysis for consumer discretionary


dftop5condis <- top5SecWise("Consumer Discretionary")
makelinechart(dfConDis)
makelinechart2(subset(dfmscwise,as.character(dfmscwise$id1) %in% as.character(dftop5condis$symbol)))

ggplot(dftop5condis,aes(x=dftop5condis$symbol,y=x,fill=dftop5condis$symbol))+
  geom_bar(stat = "identity")+ labs(x="Companies",y="Average Growth Rate",
                                    title="Top 6 Stock Values in Consumer Discretionary Sector",fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))

#Analysis for Consumer Staples

dftop5consta <- top5SecWise("Consumer Staples")
makelinechart(dfConSta)
makelinechart2(subset(dfmscwise,as.character(dfmscwise$id1) %in% as.character(dftop5consta$symbol)))
ggplot(dftop5consta,aes(x=dftop5consta$symbol,y=x,fill=dftop5consta$symbol))+
  geom_bar(stat = "identity")+ labs(x="Companies",y="Average Growth Rate",
                                    title="Top 6 Stock Values in Consumer Staples Sector",fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))

#Analysis for Real Estate
dftop5realest <- top5SecWise("Real Estate")
makelinechart(dfRealEstate)
makelinechart2(subset(dfmscwise,as.character(dfmscwise$id1) %in% as.character(dftop5realest$symbol)))


ggplot(dftop5realest,aes(x=dftop5realest$symbol,y=x,fill=dftop5realest$symbol))+
  geom_bar(stat = "identity")+ labs(x="Companies",y="Average Growth Rate",
                                    title="Top 6 Stock Values in Real Estate Sector",fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))

#Analysis for Materials


dftop5Materials <- top5SecWise("Materials")
makelinechart(dfMaterials)
makelinechart2(subset(dfmscwise,as.character(dfmscwise$id1) %in% as.character(dftop5Materials$symbol)))
ggplot(dftop5Materials,aes(x=dftop5Materials$symbol,y=x,fill=dftop5Materials$symbol))+
  geom_bar(stat = "identity")+ labs(x="Companies",y="Average Growth Rate",
                                     title="Top 6 Stock Values in Materials Sector",fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))

#Analysis for Telecommunication Services


dftop5TelServ <- top5SecWise("Telecommunications Services")
makelinechart(dfTelSer)
makelinechart2(subset(dfmscwise,as.character(dfmscwise$id1) %in% as.character(dftop5TelServ$symbol)))

ggplot(dftop5TelServ,aes(x=dftop5TelServ$symbol,y=x,fill=dftop5TelServ$symbol))+
  geom_bar(stat = "identity")+ labs(x="Companies",y="Average Growth Rate",
                                    title="Top 6 Stock Values in Tele-Comm Sector",fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))

#Analysis for Utilities

dftop5Util <- top5SecWise("Utilities")
makelinechart(dfUtil)
makelinechart2(subset(dfmscwise,as.character(dfmscwise$id1) %in% as.character(dftop5Util$symbol)))
ggplot(dftop5Util,aes(x=dftop5Util$symbol,y=x,fill=dftop5Util$symbol))+
  geom_bar(stat = "identity") + labs(x="Companies",y="Average Growth Rate",
                                     title="Top 6 Stock Values in Utilities Sector",fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))

#Analysis for Industrial Sector
dftop5Ind <- top5SecWise("Industrials")
makelinechart(dfIndustrials)
makelinechart2(subset(dfmscwise,as.character(dfmscwise$id1) %in% as.character(dftop5Ind$symbol)))

ggplot(dftop5Ind,aes(x=dftop5Ind$symbol,y=x,fill=dftop5Ind$symbol))+
  geom_bar(stat = "identity")+ labs(x="Companies",y="Average Growth Rate",
                                    title="Top 6 Stock Values in Industrial Sector",fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))

#Analysis for Energy
dftop5Energy <- top5SecWise("Energy")
makelinechart(dfEnergy)
makelinechart2(subset(dfmscwise,as.character(dfmscwise$id1) %in% as.character(dftop5Energy$symbol)))
ggplot(dftop5Energy,aes(x=dftop5Energy$symbol,y=x,fill=dftop5Energy$symbol))+
  geom_bar(stat = "identity") + labs(x="Companies",y="Average Growth Rate",
                                       title="Top 6 Stock Values in Energy Sector",fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))

#Analysis for Financials

dftop5Fin <- top5SecWise("Financials")
makelinechart(dfFinancials)
makelinechart2(subset(dfmscwise,as.character(dfmscwise$id1) %in% as.character(dftop5Fin$symbol)))

ggplot(dftop5Fin,aes(x=dftop5Fin$symbol,y=x,fill=dftop5Fin$symbol))+
  geom_bar(stat = "identity") + labs(x="Companies",y="Average Growth Rate",
                                     title="Top 6 Stock Values in Finance Sector",fill="Legend")+
  scale_color_gradientn(colours = rainbow(6))
  





