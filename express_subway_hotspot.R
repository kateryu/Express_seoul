setwd("c:/work/PJT_acorn_express")

library(plyr)

Subway_201710<-read.csv(file="./CARD_SUBWAY_MONTH_201710_a.csv", fileEncoding = "EUC-kr")
Subway_201709<-read.csv(file="./CARD_SUBWAY_MONTH_201709_a.csv", fileEncoding = "EUC-kr")
Subway_201708<-read.csv(file="./CARD_SUBWAY_MONTH_201708_a.csv", fileEncoding = "EUC-kr")
Subway_201707<-read.csv(file="./CARD_SUBWAY_MONTH_201707_a.csv", fileEncoding = "EUC-kr")
Subway_201706<-read.csv(file="./CARD_SUBWAY_MONTH_201706_a.csv", fileEncoding = "EUC-kr")
Subway_201705<-read.csv(file="./CARD_SUBWAY_MONTH_201705_a.csv", fileEncoding = "EUC-kr")
Subway_201704<-read.csv(file="./CARD_SUBWAY_MONTH_201704_a.csv", fileEncoding = "EUC-kr")
Subway_201703<-read.csv(file="./CARD_SUBWAY_MONTH_201703_a.csv", fileEncoding = "EUC-kr")
Subway_201702<-read.csv(file="./CARD_SUBWAY_MONTH_201702_a.csv", fileEncoding = "EUC-kr")
Subway_201701<-read.csv(file="./CARD_SUBWAY_MONTH_201701_a.csv", fileEncoding = "EUC-kr")
Subway_201612<-read.csv(file="./CARD_SUBWAY_MONTH_201612_a.csv", fileEncoding = "EUC-kr")
Subway_201611<-read.csv(file="./CARD_SUBWAY_MONTH_201611_a.csv", fileEncoding = "EUC-kr")
Subway_201610<-read.csv(file="./CARD_SUBWAY_MONTH_201610_a.csv", fileEncoding = "EUC-kr")

head(Subway_201710)

colnames(Subway_201710) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colnames(Subway_201709) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colnames(Subway_201708) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colnames(Subway_201707) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colnames(Subway_201706) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colnames(Subway_201705) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colnames(Subway_201704) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colnames(Subway_201703) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colnames(Subway_201702) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colnames(Subway_201701) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colnames(Subway_201612) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colnames(Subway_201611) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")


#9개: 잠실, 강남, 고속터미널, 홍대입구, 서울역, 사당, 신림, 신도림 ==> express_171113.R에서 도출
#9개 역의 역 아이디를 찾고, 지난 1년간 데이터에서 9개 역의 승하차 데이터를 추출한다. 

subwaySt<-distinct(Subway_201709, StID, StName)
str(subwaySt);  head(subwaySt)
subwaySt[grep("^잠실", subwaySt$StName), ]   #216, 2815 
subwaySt[grep("^강남", subwaySt$StName), ]   #222
subwaySt[grep("^고속터미널", subwaySt$StName), ] ### 329, 2736, 4123
subwaySt[grep("^홍대입구", subwaySt$StName), ] # 239, 1264, 4203 
subwaySt[grep("^서울역", subwaySt$StName), ] # 150, 426, 1001, 1251, 4201
subwaySt[grep("^사당", subwaySt$StName), ]  # 226, 433
subwaySt[grep("^신림", subwaySt$StName), ] # 230
subwaySt[grep("^신도림", subwaySt$StName), ]  # 234, 1007
subwaySt[grep("^건대입구", subwaySt$StName), ] # 212, 2729 

findStID <- c(216, 2815, 222,329, 2736, 4123, 239, 1264, 4203, 
              150, 426, 1001, 1251, 4201, 226, 433,230, 234, 1007, 212, 2729)  # 찾아야 할 역번호 


#9개 역의 데이터 뽑기 
subwayhot_1611<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201611[which(Subway_201611$StID==x),]
  subwayhot_1611 <- rbind(subwayhot_1611, y)
}

subwayhot_1612<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201612[which(Subway_201612$StID==x),]
  subwayhot_1612 <- rbind(subwayhot_1612, y)
}

subwayhot_1701<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201701[which(Subway_201701$StID==x),]
  subwayhot_1701 <- rbind(subwayhot_1701, y)
}

subwayhot_1702<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201702[which(Subway_201702$StID==x),]
  subwayhot_1702 <- rbind(subwayhot_1702, y)
}

subwayhot_1703<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201703[which(Subway_201703$StID==x),]
  subwayhot_1703 <- rbind(subwayhot_1703, y)
}

subwayhot_1704<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201704[which(Subway_201704$StID==x),]
  subwayhot_1704 <- rbind(subwayhot_1704, y)
}

subwayhot_1705<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201705[which(Subway_201705$StID==x),]
  subwayhot_1705 <- rbind(subwayhot_1705, y)
}

subwayhot_1706<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201706[which(Subway_201706$StID==x),]
  subwayhot_1706 <- rbind(subwayhot_1706, y)
}

subwayhot_1707<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201707[which(Subway_201707$StID==x),]
  subwayhot_1707 <- rbind(subwayhot_1707, y)
}

subwayhot_1708<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201708[which(Subway_201708$StID==x),]
  subwayhot_1708 <- rbind(subwayhot_1708, y)
}

subwayhot_1709<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201709[which(Subway_201709$StID==x),]
  subwayhot_1709 <- rbind(subwayhot_1709, y)
}

subwayhot_1710<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  y<-Subway_201710[which(Subway_201710$StID==x),]
  subwayhot_1710 <- rbind(subwayhot_1710, y)
}

##vector로 인식 (error)
subwayhot_17<-0
for (i in 1:length(findStID)) {
  x<- findStID[i]
  for (j in 1:10) {
    k<-c(1700)+j
    dfname<-paste("Subway_",k, sep="")
    y<-dfname[which(dfname$StID==x),]   
  }
  subwayhot_17 <- rbind(subwayhot_17, y)
}

###############
subwayhot_1y<- rbind(subwayhot_1611, subwayhot_1612, subwayhot_1701, subwayhot_1702, 
                     subwayhot_1703, subwayhot_1704, subwayhot_1705, subwayhot_1706, 
                     subwayhot_1707, subwayhot_1708, subwayhot_1709, subwayhot_1710)
subwayhot_1y<-na.omit(subwayhot_1y)  #결측치 제거
str(subwayhot_1y);head(subwayhot_1y);tail(subwayhot_1y) #7511개 


#동일날짜, 역이름 기준으로 묶는다. (환승역은 하나로)
subwayhot_1ysum<- ddply(subwayhot_1y, .(UseDate, StName), summarise,
                          GetOn = sum(GetOn), GetOff = sum(GetOff))  
nrow(subwayhot_1ysum); head(subwayhot_1ysum)  #3528개


#역별 일 평균을 계산한다. 
UseMonth<-substr(subwayhot_1ysum$UseDate, 1, 6)
subwayhot_1ysum<-cbind(subwayhot_1ysum, UseMonth)
subwayhot_1ymean<- ddply(subwayhot_1ysum, .(UseMonth, StName), summarise, 
                          GetOn = round(mean(GetOn), 0), GetOff = round(mean(GetOff),0))  
nrow(subwayhot_1ymean); head(subwayhot_1ymean)

#Get 합계를 구한다. 
GetSum<-c(subwayhot_1ymean$GetOn+subwayhot_1ymean$GetOff)
subwayhot_1yonoff<-cbind(subwayhot_1ymean, GetSum )
subwayhot_1yonoff
nrow(subwayhot_1yonoff); head(subwayhot_1yonoff); tail(subwayhot_1yonoff)





#시계열 자료 전환
#install.packages("tseries")
library(tseries)
subwayhot_ts<-arrange(subwayhot_1yonoff, UseMonth, StName)
head(subwayhot_ts, 10)

ts1<- subwayhot_ts[c(subwayhot_ts$StName=="강남"), 5]
ts2<- subwayhot_ts[c(subwayhot_ts$StName=="건대입구"), 5]
ts3<- subwayhot_ts[c(subwayhot_ts$StName=="고속터미널"), 5]
ts4<- subwayhot_ts[c(subwayhot_ts$StName=="사당"), 5]
ts5<- subwayhot_ts[c(subwayhot_ts$StName=="서울역"), 5]
ts6<- subwayhot_ts[c(subwayhot_ts$StName=="신도림"), 5]
ts7<- subwayhot_ts[c(subwayhot_ts$StName=="신림"), 5]
ts8<- subwayhot_ts[c(subwayhot_ts$StName=="잠실(송파구청)"), 5]
ts9<- subwayhot_ts[c(subwayhot_ts$StName=="홍대입구"), 5]

subway_ts1 <- ts(ts1, start=c(2016, 11), end=c(2017, 10), frequency=12) 
subway_ts2 <- ts(ts2, start=c(2016, 11), end=c(2017, 10), frequency=12) 
subway_ts3 <- ts(ts3, start=c(2016, 11), end=c(2017, 10), frequency=12) 
subway_ts4 <- ts(ts4, start=c(2016, 11), end=c(2017, 10), frequency=12) 
subway_ts5 <- ts(ts5, start=c(2016, 11), end=c(2017, 10), frequency=12) 
subway_ts6 <- ts(ts6, start=c(2016, 11), end=c(2017, 10), frequency=12) 
subway_ts7 <- ts(ts7, start=c(2016, 11), end=c(2017, 10), frequency=12) 
subway_ts8 <- ts(ts8, start=c(2016, 11), end=c(2017, 10), frequency=12) 
subway_ts9 <- ts(ts9, start=c(2016, 11), end=c(2017, 10), frequency=12) 

subway_ts1

par(mfrow=c(3,3)) 
plot(subway_ts1);plot(subway_ts2);plot(subway_ts3)
plot(subway_ts4);plot(subway_ts5);plot(subway_ts6)
plot(subway_ts7);plot(subway_ts8);plot(subway_ts9)

#not stationary
adf.test(diff(log(subway_ts1)), alternative="stationary", k=0) #Dickey-Fuller = -2.565, Lag order = 0, p-value = 0.3571
adf.test(diff(log(subway_ts5)), alternative="stationary", k=0) #Dickey-Fuller = -3.5397, Lag order = 0, p-value = 0.05838


#stationary
adf.test(diff(log(subway_ts2)), alternative="stationary", k=0) #Dickey-Fuller = -3.7051, Lag order = 0, p-value = 0.04249
adf.test(diff(log(subway_ts3)), alternative="stationary", k=0) #Dickey-Fuller = -6.5637, Lag order = 0, p-value = 0.01
adf.test(diff(log(subway_ts4)), alternative="stationary", k=0) #Dickey-Fuller = -4.1128, Lag order = 0, p-value = 0.01932
adf.test(diff(log(subway_ts6)), alternative="stationary", k=0) #Dickey-Fuller = -4.1391, Lag order = 0, p-value = 0.0184
adf.test(diff(log(subway_ts7)), alternative="stationary", k=0) #Dickey-Fuller = -4.3705, Lag order = 0, p-value = 0.01033
adf.test(diff(log(subway_ts8)), alternative="stationary", k=0) #Dickey-Fuller = -6.895, Lag order = 0, p-value = 0.01
adf.test(diff(log(subway_ts9)), alternative="stationary", k=0) #Dickey-Fuller = -7.3774, Lag order = 0, p-value = 0.01









