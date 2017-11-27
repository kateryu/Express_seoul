setwd("./express")

#install.packages("plyr")
library(plyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("data.table")
library(data.table)

######################################################
########################
#서울시 동별 인구

popu<-read.csv(file="./seoul_popu_2016_raw.csv", header = T)
str(popu)
colnames(popu) <-c("district", "total_popu_2016", "total_age_2016", 
                   "2024", "2529", "3034", "3539", "4044", "4549", 
                   "total_popu_M2016", "total_age_M2016", "M2024", "M2529", "M3034", "M3539", "M4044", "M4549",
                   "total_popu_W2016", "total_age_W2016", "W2024", "W2529", "W3034", "W3539", "W4044", "W4549")
popu$y_ratio<- round(c(popu$total_age_2016 / popu$total_popu_2016), 2)
head(popu)
plot(popu$district, popu$total_age_2016)

boxplot(popu$total_age_2016)
title(main = "2016년2040 인구")

# 2040 인구수 기준, 이상치 찾기
(Iqr<-quantile(popu$total_age_2016)[4] - quantile(popu$total_age_2016)[2])
popu$outlierdong<-c(popu$total_age_2016 >= (quantile(popu$total_age_2016)[4]+ 1.5*Iqr))  #21894.25명 
popu_outlier<-subset(popu, popu$outlierdong==TRUE )
(popu_outlier$district)  
#9개 지역: 은평구 역촌동/진관동, 양천구 신정3동, 강서구 화곡1동/우장산동, 
#동작구 상도1동, 서초구 양재1동, 강남구 역삼1동, 강동구 길동 

#2040 인구비율이 높은 지역 찾기
popu_y<-arrange(popu, desc(popu$y_ratio))
head(popu_y[,c(1,2,3,26)], 10)   
# 신림동(73%), 화양동(68%), 역삼1동(66%), 낙성대동(63%), 관악구 대학동(63%), 논현1동(63%), 신촌동(61%) 

#############
#2033년 서울시 동별 2040 인구 추이 확인 

popu2033<-read.csv(file="./popu_2033est.csv", header = T)
str(popu2033)
colnames(popu2033) <-c("gu", "dong", "ratio2033", "popu_2033")

par(mfrow=c(1,2)) 
boxplot(popu$total_age_2016)
title(main = "2016년2040 인구")
boxplot(popu2033$popu_2033)
title(main = "2033년 2040 인구")


# 2040 인구수 기준, 이상치 찾기. 2016년의 구내 동별 비중을 2033년도 동일하게 설정함. 
(Iqr<-quantile(popu2033$popu_2033)[4] - quantile(popu2033$popu_2033)[2])
popu2033$outlierdong<-c(popu2033$popu_2033 >= (quantile(popu2033$popu_2033)[4]+ 1.5*Iqr))  #17548.25명 
popu2033_outlier<-subset(popu2033, popu2033$outlierdong==TRUE )
(popu2033_outlier)
#8개 지역: 강동구 길동, 암사1동, 천호2동, 강서구 화곡동
#서초구 양재1동, 양천구 신정3동, 은평구 역촌동/ 진관동  


######################################################
######################
#지하철 승하차 인원이 많은 곳, 이상치 찾기

#subway<-read.csv(file.choose(), header = T, stringsAsFactors = T, fileEncoding = "UTF-8")
subway<-read.csv(file.choose(), header = T, stringsAsFactors = F, sep="\t")
subway<-read.csv(file=".//subway_stName_raw.csv", header = T, stringsAsFactors = F, sep="\t")


subway<-read.csv(file="./CARD_SUBWAY_MONTH_201709_a.csv", fileEncoding = "EUC-kr")
colnames(subway) <-c("UseDate","Line", "StID", "StName","GetOn", "GetOff", "RegDate")
colSums(is.na(subway))  #결측치 확인 
str(subway);head(subway)  #16797개 

#동일날짜, 역이름 기준으로 묶는다. (환승역은 하나로)
subway_dsum<- ddply(subway, .(UseDate, StName), summarise,
                        GetOn = sum(GetOn), GetOff = sum(GetOff))  
nrow(subway_dsum); head(subway_dsum)  #14548개

#역별 일 평균을 계산한다. 
subway_dmean<- ddply(subway_dsum, .(StName), summarise, 
                         GetOn = round(mean(GetOn), 0), GetOff = round(mean(GetOff),0))  
nrow(subway_dmean); head(subway_dmean)  #502개 

#Get 합계를 구한다. 
subway_dmean$GetSum<-c(subway_dmean$GetOn + subway_dmean$GetOff)
str(subway_dmean)

subway<-arrange(subway_dmean, desc(GetSum))
head(subway, 10)  #강남, 잠실, 고속터미널, 홍대입구, 서울역, 사당, 신림, 신도림, 건대입구, 구로디지털단지 

plot(subway$GetOn)
plot(subway$GetOff)
plot(subway$GetOn, subway$GetOff)  #승하차 지역은 연관관계 있는 것으로 보임 
boxplot(subway$GetOn, subway$GetOff)

#승차 이상치 찾기 
(Iqr<-quantile(subway$GetOn)[4] - quantile(subway$GetOn)[2])
subway$OutGetOn<-c(subway$GetOn >= (quantile(subway$GetOn)[4]+ 1.5*Iqr))  #outlier 38838.25
subway_outlier1<-subset(subway, subway$OutGetOn==TRUE )
subwaysort1<-arrange(subway_outlier1, desc(subway_outlier1$GetOn))
(subwaysort1$StName)  #42개 지역 ( 잠실, 강남, 고속터미널, 홍대입구, 서울역, 사당, 신림, 신도림 )

#하차 이상치 찾기 
(Iqr<-quantile(subway$GetOff)[4] - quantile(subway$GetOff)[2])
subway$OutGetOff<-c(subway$GetOff >= (quantile(subway$GetOff)[4]+ 1.5*Iqr))  #outlier 39199.62
subway_outlier2<-subset(subway, subway$OutGetOff==TRUE )
subwaysort2<-arrange(subway_outlier2, desc(subway_outlier2$GetOff))
(subwaysort2$StName)   #43개 지역 (강남, 고속터미널, 잠실, 홍대입구, 서울역, 사당, 신림, 건대입구 ) 

#승하차 합계 이상치 찾기 
(Iqr<-quantile(subway$GetSum)[4] - quantile(subway$GetSum)[2])
subway$OutGetSum<-c(subway$GetSum >= (quantile(subway$GetSum)[4]+ 1.5*Iqr))  #outlier 77058.38
subway_outlier3<-subset(subway, subway$OutGetSum==TRUE )
subwaysort3<-arrange(subway_outlier3, desc(subway_outlier3$GetSum))
(subwaysort3$StName)

######################################################
#######################################################
#bus 승하차 인원이 많은 곳, 이상치 찾기 

#1. bus data 불러오기
bus_raw<-read.csv(file="./bus_raw.csv", header = T, sep=",", quote="'\"", fileEncoding = "UTF-8")
colSums(is.na(bus_raw))  #결측치 확인 
str(bus_raw) ;head(bus_raw)  #1,138,022개 관측치
colnames(bus_raw) <-c("UseDate","Line","LineName", "StopID","StopARS", "StopName", "GetOn", "GetOff", "RegDate") 

#table 형태로 변경 
bus_tbl<-data.table(bus_raw)
setkey(bus_tbl, StopID)
tables()
str(bus_tbl)


###############
#2. 버스 정류장 정보만 추출 
busStop <- bus_tbl[,c(4:6)] 
busStop_u<-distinct(busStop, StopID, .keep_all = TRUE)   #dplyr
nrow(busStop_u);head(busStop_u) #12850개 

#3. bus stop 기준으로 일 평균 승하차 구하기
# 속도가 느림. 멈출수 있으니, 일평균 승하차 최종파일, 저장해둔 파일(bus_dmean.csv)로 불러들여서 읽기 권장
#bus_dmean을 구하려면, 아래 3-1, 3-2 수해
bus_dmean<-read.csv(file="./bus_dmean.csv", header = T, sep=",", quote="'\"", fileEncoding = "UTF-8")
str(bus_dmean); head(bus_dmean)


#3-1. 동일 UseDate& StopID 기준으로 승하차 합치기 
bus_tbl2<-bus_tbl[,c(1,4,7,8)]
str(bus_tbl2)
#속도가 느려서 분리해서 실행함. 
bus_dsum1 <- ddply(bus_tbl2, .(UseDate, StopID), summarise, 
                      GetOn = sum(GetOn))  
nrow(bus_dsum1); head(bus_dsum1)     #381,514 건 
bus_dsum2 <- ddply(bus_tbl2, .(UseDate, StopID), summarise, 
                      GetOff = sum(GetOff))  
nrow(bus_dsum2); head(bus_dsum2)     #381,514 건 
bus_dsum<-cbind(bus_dsum1, bus_dsum2)

#3-2. 정류장별로 승하차 일평균 구하기 
length(unique(bus_raw$StopID)) #bus stop => 12850개 

bus_dmean <- ddply(bus_dsum, .(StopID), summarise, 
               GetOn = round(mean(GetOn), 0), GetOff = round(mean(GetOff), 0))    
nrow(bus_dmean);head(bus_dmean)  #12850개 
#write.csv(bus_dmean, file="./bus_dmean2.csv")

boxplot(bus_dmean$GetOn, bus_dmean$GetOff) 



#4. 위치가 같은 지역의 정류장은 같은 지역으로 묶기 
#4-1. 버스정류장 위치 정보 불러오기 (서울시 시내버스 정류장 위치정보)
StopAddr<-read.csv(file="./stationlist_raw.csv")   
str(StopAddr); nrow(StopAddr)   #노선별 정류장 38670개 
StopAddr<-StopAddr[,c(4:8)] #정류장 정보만 가져온다. 
colnames(StopAddr) <-c("StopName","x_coord", "y_coord","StopID","StopARS")
StopAddr_u<-distinct(StopAddr, StopID, .keep_all=TRUE)     
str(StopAddr_u) ;head(StopAddr_u) # 정류장 기준 13249개


#4-2. 일평균 승하차 정보 +  xy 좌표 정보 합치기 
bus1 <- join(bus_dmean, StopAddr_u, by=c("StopID"="StopID"), type="left", match="all")
str(bus1) ; head(bus1)
colSums(is.na(bus1)) #NA 225개 

#NA가 어떤 정류장인지 확인
z1<-subset(bus1, is.na(StopName)==T)
nrow(z1)
z2<-join(z1, busStop_u, by="StopID", type="left", match="all")
head(z2)  #결측 데이터 확인

#데이터 보강하면 다시 계산하고, na.omit 형태로 일단 진행  
bus2<-na.omit(bus1)
nrow(bus1)-nrow(bus2)


#4-3. 가까운 거리 묶어주기. 전체를 클러스터화 함. 클러스터 만들고, hotspot을 cluster로 정한다. 
str(bus2);head(bus2, 30)     #12625 개 

bus2c<-bus2[, c(6, 7)]  #클러스터 생성기준 x,y좌표. 
str(bus2c); nrow(bus2c)

kmeans.result <- kmeans(bus2c, 1200)

par(mfrow=c(1,1)) 
plot(bus2c[c("x_coord", "y_coord")], col = kmeans.result$cluster)
title(main = "서울시 버스정류장 클러스터(2017년 9월 승하차 기준)")
summary(kmeans.result)

#bus 정류장별 클러스터 정보 추가
clusterNum<-c(kmeans.result$cluster) ; #nrow(clusterNum)
busCluster<-cbind(clusterNum, bus2[, c(2:5)])  #index번호, geton/getoff, StopName 
busCluster$GetSum <- c(busCluster$GetOn + busCluster$GetOff)
busCluster<-busCluster[, c(1,2,5,3,4,6)]
str(busCluster); head(busCluster)  

#클러스터 정보 (사이즈, 중심좌표) 
cluster_info<-cbind("Size"=c(kmeans.result$size), 
                    "center_x"=c(kmeans.result$centers[,1]), 
                    "center_y"=c(kmeans.result$centers[,2])) #1200개 클러스터 
head(cluster_info)


#4-4. 클러스터별로 승하차 인원 합계한 후에 정렬. hotspot 찾기 
cluster_OnOff<-ddply(busCluster, .(clusterNum), summarize, 
      CluGetOn=sum(GetOn), CluGetOff=sum(GetOff), CluGetSum=sum(GetSum))
cluster_OnOff <-cbind(cluster_OnOff, cluster_info)
cluster_OnOff<-arrange(cluster_OnOff, desc(CluGetSum))    #승하차 합계 기준으로 정렬
str(cluster_OnOff); head(cluster_OnOff); tail(cluster_OnOff)


ClusterHotspot<-0
for (i in 1:10) {
  x<- cluster_OnOff$clusterNum[i]
  y<-busCluster[which(busCluster$clusterNum==x),]
  ClusterHotspot <- rbind(ClusterHotspot, y)
}
str(ClusterHotspot);head(ClusterHotspot)   
ClusterHotspot<-na.omit(ClusterHotspot) #168개

###########################
#가까운 정류장 합쳐서 보정된 데이터로 이상치 찾기 
#ClusterHotspot<-read.csv(file="./ClusterHotspot.csv")

boxplot(cluster_OnOff$CluGetSum)

#이상치 찾기 
(Iqr<-quantile(cluster_OnOff$CluGetSum)[4] - quantile(cluster_OnOff$CluGetSum)[2])
cluster_OnOff$Outlier<-c(cluster_OnOff$CluGetSum >= (quantile(cluster_OnOff$CluGetSum)[4]+ 1.5*Iqr))  #outlier 26278.38
CluOutlier<-subset(cluster_OnOff, cluster_OnOff$Outlier==TRUE )
(CluOutlier) #71개 

##########################
#distance analysis
#cpDist<- read.csv(file.choose())
cpDist<- read.csv(file="./distance_C_P.csv")  
str(cpDist) ; head(cpDist, 10)

