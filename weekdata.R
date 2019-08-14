data1<- fread("D:/work/雷鸟/周数据/周数据717-723.csv", header=TRUE, sep=",", encoding="UTF-8")
sum1<-fread("D:/work/雷鸟/周数据/联运SDK.csv", header=TRUE, sep=",", encoding="UTF-8")

data1$支付金额<-data1$支付金额/100
data1<-data1[-grep("测试",data1$商品名称)]
data2<-separate(separate(separate(data1,2,into = c("name","supplier"),sep = "_"),7,into = c("year","month","day","hour"),sep = "-"),9,into = c("day","hour"),sep = " ")
data2[is.na(data2)] <- "T2"
#data2<-sumdata[which(as.numeric(sumdata$day)>23)]

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
for (i in 1:nrow(data2)) {
  if (data2$name[i]=="T2游戏平台") {
    data2$name[i]<-data2$商品名称[i]
  }
}
for(i in 1:nrow(sum1)){
  sum1$pay[i]<-sum(data2[which(data2$supplier==sum1$name[i])]$支付金额)
}

data3<-data2[grep("49.9",data2$name)]
sum1$pay[20]<-sum(data3$支付金额)
data4<-data2[grep("运动加加",data2$name)]
sum1$pay[21]<-sum(data4$支付金额)
data5<-data2[grep("北通",data2$name)]
sum1$pay[22]<-sum(data5$支付金额)
data6<-data2[grep("小鸡",data2$name)]
sum1$pay[23]<-sum(data6$支付金额)


#重要游戏周数据
sum2<-data2[!duplicated(data2$name),c(2,5,4,3)]
names(sum2)[3]<-"payPer"
data2$支付状态<-1

for (i in 1:nrow(sum2)) {
  sum2$支付金额[i]<-sum(data2[which(data2$name==sum2$name[i])]$支付金额)
  sum2$payPer[i]<-sum(as.numeric(data2[which(data2$name==sum2$name[i])]$支付状态))
}

write.csv(sum1,"D:/work/雷鸟/周数据/t1.csv", row.names = F)
write.csv(sum2,"D:/work/雷鸟/周数据/t2.csv", row.names = F)
