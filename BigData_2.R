setwd("E:/빅데이터응용/기말 파일")
library("ggplot2")
library("ggmap")
library("readxl")
files <- c("202003","202009","202103","202109")
columns <- c("상가업소번호","상호명","상권업종대분류명","상권업종중분류명","상권업종소분류명","시군구명","행정동명","경도","위도")
columns2 <- c("상가업소번호","상호명","상권업종대분류명","상권업종중분류명","상권업종소분류명","시군구명","법정동명","경도","위도")
ds.total <- NULL
ds.total2 <- NULL
for (i in 1:length(files)){
       filename <- paste("gyeongbuk_", files[i], ".xlsx", sep="")
       cat("read",filename, "...\n")
       
         ds <- read_excel(filename)
         ds <- data.frame(ds)
         ds <- ds[,columns]
         ds$수집연월 <- rep(i, nrow(ds))
         ds.total <- rbind(ds.total,ds)}
for (i in 1:length(files)){
  filename <- paste("gyeongbuk_", files[i], ".xlsx", sep="")
  cat("read",filename, "...\n")
  
  ds1 <- read_excel(filename)
  ds1 <- data.frame(ds1)
  ds1 <- ds1[,columns2]
  ds1$수집연월 <- rep(i, nrow(ds1))
  ds.total2 <- rbind(ds.total2,ds1)}
head(ds.total)
str(ds.total)
unique(ds.total$수집연월)
unique(ds.total$상권업종대분류명)
unique(ds.total$상권업종중분류명)
unique(ds.total$상권업종소분류명)
sum(is.na(ds.total))
ds.202109 <- subset(ds.total,ds.total$수집연월==4)
dim(ds.202109)
store.level_1 <- aggregate(ds.202109[,1],
                                                       by=list(대분류=ds.202109$상권업종대분류명),
                                                       FUN=length)
store.level_1
names(store.level_1)[2]=c("count")
ggplot(store.level_1, aes(x=대분류, y=count)) +
       geom_bar(stat="identity", width=0.7, fill="steelblue") +
       ggtitle("업종별 점포수") +
       theme(plot.title = element_text(color="black", size=14, face="bold"))
store.region <- aggregate(ds.202109[,1],
                                                     by=list(시이름=ds.202109$시군구명),
                                                     FUN=length)
store.region
names(store.region)[2]=c("count")
ggplot(store.region, aes(x=시이름, y=count)) +
       geom_bar(stat="identity", width=0.7, fill="steelblue")+
       ggtitle("시별 점포수") +
       theme(plot.title = element_text(color="black",size=14,face="bold"),
                         axis.text.x = element_text(angle = 45))
store.region.loc <- aggregate(ds.202109[,c("경도","위도")],
                                                             by=list(시이름=ds.202109$시군구명),
                                                             FUN=mean)
store.region <- data.frame(store.region, store.region.loc[,2:3])
register_google(key='AIzaSyAOXe2cBsxpcmDFvhBeQi1WFIepelpJNWY')
cen <- c(mean(store.region$경도),mean(store.region$위도))
map <- get_googlemap(center=cen,
                                           maptype="roadmap",
                                           size=c(640,640),
                                           zoom=9)
gmap <- ggmap(map)
gmap+geom_point(data=store.region,
                aes(x=경도, y=위도,size=count),
                alpha=0.5, col="red") +
  scale_size_continuous(range=c(1,15))+
  geom_text(data=store.region,
            aes(x=경도,y=위도),
            size=3,
            label=store.region$시이름)
store.dong <- aggregate(ds.202109[,1],
                                                 by=list(동이름=ds.202109$행정동명),
                                                 FUN=length)
store.dong
names(store.dong)[2]=c("count")
store.dong <- store.dong[order(store.dong$count,decreasing=T),]
dong.top10 <- store.dong[1:10,]
dong.top10
ggplot(dong.top10, aes(x=reorder(동이름, -count), y=count)) +
  geom_bar(stat="identity", width=0.7, fill="steelblue") +
  ggtitle("점포수 많은 상위 10개동") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.text.x = element_text(angle=45))
store.change <- aggregate(ds.total[,1],
                          by=list(연월=ds.total$수집연월,
                                    업종대분류=ds.total$상권업종대분류명),
                          FUN=length)
head(store.change)
names(store.change)[3] <- c("count")
ggplot(store.change, aes(x=연월, y=count, colour=업종대분류, gruop=업종대분류)) +
  geom_line() +
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle("업종별 점포수 변화(대분류)") +
  ylab("점포수") +
  scale_x_continuous(breaks=1:4,labels=files) +
  theme(plot.title = element_text(color="black", size=14, face="bold"))
store.tmp <- aggregate(ds.total[,1],
                       by=list(연월=ds.total$수집연월,
                                 업종소분류=ds.total$상권업종소분류명),
                       FUN=length)
names(store.tmp)[3] <- c("count")
store.202003 <- store.tmp[store.tmp$연월==1,]
names(store.202003)[3] = c("cnt_202003")
store.202109 <- store.tmp[store.tmp$연월==4,]
names(store.202109)[3] = c("cnt_202109")
store.diff <- merge(store.202003[,2:3], store.202109[,2:3])
store.diff$diff <- abs(store.diff$cnt_202003-store.diff$cnt_202109)
store.diff <- store.diff[order(by=store.diff$diff, decreasing=T),]
top10 <- store.diff[1:10,1]
top10
store.change <- subset(store.tmp, store.tmp$업종소분류 %in% top10)
ggplot(store.change, aes(x=연월, y=count, colour=업종소분류, gruop=업종소분류)) +
  geom_line() +
  geom_point(size=6,shape=19,alpha=0.5) +
  ggtitle("점포수 변화 Top 10 업종(소분류)") +
  ylab("점포수") +
  scale_x_continuous(breaks=1:4,
                     labels=files) +
  theme(plot.title=element_text(color="black", size=14, face="bold"))
store.si <- aggregate(ds.total[,1],
                      by=list(연월=ds.total$수집연월,
                                시이름=ds.total$시군구명),
                      FUN=length)
names(store.si)[3] <- c("count")
ggplot(store.si, aes(x=연월, y=count, colour=시이름, gruop=시이름)) +
  geom_line() +
  geom_point(size=6,shape=19,alpha=0.5) +
  ggtitle("시,군,구별 점포수 변화 (대분류)") +
  ylab("점포수") + 
  scale_x_continuous(breaks=1:4,
                     labels=files) +
  theme(plot.title=element_text(color="black", size=14, face="bold"))
ds.yongsang <- subset(ds.total2, ds.total2$수집연월 == 4 &
                        ds.total2$법정동명 == "용상동")
cen <- c(mean(ds.yongsang$경도),mean(ds.yongsang$위도))
map <- get_googlemap(center = cen,
                     maptype = "roadmap",
                     size = c(640,640),
                     zoom=15)
gmap <- ggmap(map)
gmap+geom_point(data=ds.yongsang,
                aes(x =경도, y = 위도, color=상권업종대분류명),size=2,alpha=0.7) +
                labs(x="Longitude", y="Latitude",
                     title="용상동 업종별 점포", color="업종")
ds.yongsang2 <- subset(ds.yongsang, ds.yongsang$상권업종소분류명=="커피전문점/카페/다방")
gmap+geom_point(data=ds.yongsang2,
               aes(x=경도,y=위도), size=2, alpha=0.5, col="red") +
  labs(x="Longitude", y="Latitude",
       title="용상동 커피점")
ds.indong <- subset(ds.total,ds.total$수집연월 ==4 &
                    ds.total$행정동명 == "인동동")
cen1 <- c(mean(ds.indong$경도),mean(ds.indong$위도))
map1 <- get_googlemap(center=cen1,
                      maptype = "roadmap",
                      size=c(640,640),
                      zoom=14)
gmap1 <- ggmap(map1)
ds.indong2 <- subset(ds.indong,ds.indong$상권업종소분류명=="한식/백반/한정식")
gmap1+geom_point(data=ds.indong2,
                 aes(x=경도,y=위도), size=2, alpha=0.5, color="red") +
  labs(x="Longitude", y="Latitude",
       title="인동동 한식음식점_202109")
ds.indong3 <- subset(ds.total,ds.total$수집연월 ==3 &
                      ds.total$행정동명 == "인동동")
cen2 <- c(mean(ds.indong3$경도),mean(ds.indong3$위도))
map2 <- get_googlemap(center=cen2,
                      maptype = "roadmap",
                      size=c(640,640),
                      zoom=14)
gmap2 <- ggmap(map2)
ds.indong4 <- subset(ds.indong3,ds.indong3$상권업종소분류명=="한식/백반/한정식")
gmap1+geom_point(data=ds.indong4,
                 aes(x=경도,y=위도), size=2, alpha=0.5, color="red") +
  labs(x="Longitude", y="Latitude",
       title="인동동 한식음식점_202103")

  



