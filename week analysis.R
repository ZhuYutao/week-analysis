library("data.table")
library("tidyr")
library("plyr")

data1<- fread("D:/work/雷鸟/日数据/周数据814-820.csv", header=TRUE, sep=",", encoding="UTF-8")
sum1<-fread("D:/work/雷鸟/日数据/数据汇总.csv", header=TRUE, sep=",", encoding="UTF-8")
#sum<-fread("D:/work/雷鸟/日数据/dailyData.csv", header=TRUE, sep=",", encoding="UTF-8")
#view<-fread("D:/work/雷鸟/日数据/pv25.csv", header=TRUE, sep=",", encoding="UTF-8")
download<-fread("D:/work/雷鸟/日数据/download814-820.csv", header=TRUE, sep=",", encoding="UTF-8")

download<-separate(download,3,into = c("name","activity"),sep = "（")
download<-download[,-4]
download$name[which(download$name=="小麦云游戏")]<-"游戏GO"
download$name[which(download$name=="百变捕鱼")]<-"大圣电玩捕鱼"
download$name[which(download$name=="阿尔法任务2")]<-"阿尔发任务2"
download$name[which(download$name=="三国战纪")]<-"三国战纪（求生之路）"
download$name[which(download$name=="咪咕快游")]<-"咪咕游戏TV"
download$name[which(download$name=="饿狼传说-狼之印记")]<-"饿狼传说"
download$name[which(download$name=="射门!射门!射门!")]<-"射门射门射门"
#download$游戏名[which(download$游戏名=="三国战纪")]<-"三国战纪（求生之路）"
#download$游戏名[which(download$游戏名=="饿狼传说-狼之印记")]<-"饿狼传说"
#download$游戏名[which(download$游戏名=="饿狼传说-狼之印记")]<-"饿狼传说"



dataa<-data1[-c(grep("虚拟",data1$支付金额),grep("实物",data1$支付金额),grep("测试",data1$支付金额))]
if (nrow(dataa)==0) {
  dataa<-data1
}
dataa$支付金额<-dataa$支付金额/100
data2<-separate(separate(separate(dataa,2,into = c("name","supplier"),sep = "_"),8,into = c("year","month","day","hour"),sep = "-"),10,into = c("day","hour"),sep = " ")
data2[is.na(data2)] <- "T2"
for (i in 1:nrow(data2)) {
  if (data2$name[i]=="T2游戏平台") {
    data2$name[i]<-data2$商品名称[i]
  }
}
data2[which(data2$name=="49.9元购游戏大礼包（10款火爆游戏+手柄1只）")]$supplier<-"小悠"
data2[which(data2$name=="49.9元购游戏大礼包（10款火爆游戏 手柄1只）")]$supplier<-"小悠"
data2[which(data2$name=="北通蝙蝠2无线版游戏手柄")]$supplier<-"北通"
data2[which(data2$name=="北通阿修罗2无线智能版游戏手柄")]$supplier<-"北通"
data2[which(data2$name=="Gamesir盖世小鸡T4电脑游戏无线手柄")]$supplier<-"小鸡"
data2[which(data2$name=="飞机大逃杀")]$supplier<-"锋狼科技"
data2[which(data2$name=="弹球砖块")]$supplier<-"锋狼科技"
data2[which(data2$name=="夜惊魂")]$supplier<-"锋狼科技"
data2[which(data2$name=="疯狂的胖鸟")]$supplier<-"锋狼科技"
data2[which(data2$name=="气球大战")]$supplier<-"锋狼科技"
data2[which(data2$name=="火箭快跑")]$supplier<-"锋狼科技"
data2[which(data2$name=="疯狂马戏团")]$supplier<-"锋狼科技"
data2[which(data2$name=="全民战机")]$supplier<-"锋狼科技"
data2[which(data2$name=="推个球啊")]$supplier<-"锋狼科技"
data2[which(data2$name=="小小飞机空战")]$supplier<-"锋狼科技"
data2[which(data2$name=="小小蜜蜂机")]$supplier<-"锋狼科技"
data2[which(data2$name=="咪咕游戏TV")]$supplier<-"咪咕游戏"
data2[which(data2$name=="极品飞车2019")]$supplier<-"刀传"
data2[which(data2$name=="僵尸吃鸡")]$supplier<-"刀传"
data2[which(data2$name=="蛇蛇出击")]$supplier<-"游谷"
data2[which(data2$name=="音乐跳一跳")]$supplier<-"刀传"
data2[which(data2$name=="饿狼传说")]$supplier<-"小悠"
data2[which(data2$name=="八戒捕鱼")]$supplier<-"游谷"

data3<-data2[!duplicated(data2$name)]
data4<-data2[which(data2$支付状态=="支付成功")]
sum1<-sum1[c(1:nrow(data3))]

for (i in 1:nrow(data3)) {
  sum1$游戏名称[i]<-data3$name[i]
  data5<-data4[name==sum1$游戏名称[i]]
  data6<-data2[name==sum1$游戏名称[i]]
  sum1$支付人数[i]<-as.numeric(nrow(data5[!duplicated(data5$用户id),]))
  sum1$产生订单人数[i]<-as.numeric(nrow(data6[!duplicated(data6$用户id),]))
  sum1$流水[i]<-sum(data4[which(data4$name==sum1$游戏名称[i])]$支付金额)
  sum1$支付订单数[i]<-nrow(data4[which(data4$name==sum1$游戏名称[i]),1])
  sum1$订单数[i]<-nrow(data2[which(data2$name==sum1$游戏名称[i]),1])
  sum1$支付成功率[i]<- paste(round((sum1$支付订单数[i]/sum1$订单数[i])*100,2),"%",sep = "")
  sum1$付费率[i]<-paste(round((sum1$支付人数[i]/sum1$产生订单人数[i])*100,2),"%",sep = "")
  sum1$ARPPU[i]<-round(as.numeric(sum1$流水[i])/as.numeric(sum1$支付人数[i]),2)
  sum1$供应商[i]<-data3$supplier[i]
  sum1$下载人数[i]<-sum(download[which(download$name==sum1$游戏名称[i])]$成功下载人数)
}

for (i in 1:nrow(sum1)) {
  sum1$SCORE[i]<-(sum1$流水[i]-mean(sum1$流水))/sd(sum1$流水)*0.4+(sum1$下载人数[i]-mean(sum1$下载人数))/sd(sum1$下载人数)*0.2+(sum1$支付人数[i]-mean(sum1$支付人数))/sd(sum1$支付人数)*0.2+(as.numeric(gsub("\\%", "", sum1$支付成功率[i]))-mean(as.numeric(gsub("\\%", "", sum1$支付成功率))))/sd(as.numeric(gsub("\\%", "", sum1$支付成功率)))*0.2
  
}


write.csv(sum1,"D:/work/雷鸟/周数据/Week Analysis8.14-8.20.csv", row.names = F)
          