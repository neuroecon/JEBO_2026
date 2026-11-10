

setwd("/Users/chennan/Box Sync/DDM/Data/raw_data_session4")
setwd("/Users/chennan/Download")

library(plyr)
library(ggplot2)



## Clean data
N=180
dta=read.delim("session_4_all.txt", fileEncoding="UCS-2LE",fill=T,header=F)
item_new=read.csv("item_dic.csv")
#dic_3afc=read.csv("3AFC.csv")
#dic_3afc=lapply(dic_3afc, function(x) {
#                      gsub("MaCafe", "McCafe", x)
#                })
##################################### Y/N
dta.all= dta[,c("V2","V3","V32","V33","V34","V35","V36","V43","V44","V52","V53","V73","V74","V75","V76","V104","V105","V106","V107")]

colnames(dta.all)=c("subject","session","block","brand","brand1","brand2","brand3","resp1","rt1","resp2","rt2","discount","discount1","discount2","discount3","type","type1","type2","type3")

dta.all=dta.all[5:nrow(dta.all),]

dta.all$rt1=as.numeric(as.character(dta.all$rt1))/1000
dta.all$rt2=as.numeric(as.character(dta.all$rt2))/1000
dta.all$block=as.numeric(dta.all$block)


dta.all$resp1=ifelse(dta.all$resp1=="f",1,ifelse(dta.all$resp1=="j",0,-1))
dta.all$resp2=ifelse(dta.all$resp2=="j",1,ifelse(dta.all$resp2=="k",2,ifelse(dta.all$resp2=="l",3,-1)))

dta.all$id_old =as.numeric(paste0(dta.all$subject,dta.all$session))
id_new=as.data.frame(cbind(unique(dta.all$id_old),seq(1,length(unique(dta.all$id_old)))))
colnames(id_new)=c("id_old","ID")
dta.all=merge(dta.all,id_new)

dta.all$Discount_25=ifelse(dta.all$discount=="25",1,0)
dta.all$Discount_99=ifelse(dta.all$discount=="99",1,0)





##################################### Y/N
yn.dta=subset(dta.all,resp1>-1)
yn.dta= yn.dta[,c("subject","session","ID","block","resp1","rt1","brand","type","discount","Discount_25","Discount_99")]
colnames(yn.dta)=c("subject","session","ID","block","response","rt","brand","type","discount","Discount_25","Discount_99")


##################################### Y/N
yn.dta$item_old=paste(yn.dta$brand,yn.dta$type,sep="_")
yn.dta=merge(yn.dta,item_new)
##################################### Y/N


ggplot(subset(yn.dta,rt<5),aes(x=rt, colour=as.factor(ID)))+geom_density(alpha=.2,position="identity")
yn.logit=glm(response ~brand+type+discount,family="binomial",data= yn.dta)
summary(yn.logit)
yn.logit=glm(response ~item,family="binomial",data= yn.dta)
summary(yn.logit)


yn_rt=aggregate(rt~ID,yn.dta,FUN=median)
hist(yn_rt$rt,breaks=40,xlab="RT",main="Median of reaction time (Y/N)")
abline(v=0.49,col="red")
bad_ids=subset(yn_rt,rt<0.5)$ID
#### Bad guys
bad_ids=c(4,6,18,29)

#yn.dta=subset(yn.dta,!(ID%in%bad_ids))
#write.csv(yn.dta,file="yn.csv",row.names=F)
#write.csv(item_new,file="item_dic.csv",row.names=F)
##################################### Y/N








##################################### Y/N
fc.dta=subset(dta.all,resp2>-1)
fc.dta= fc.dta[,c("subject","session","ID","block","resp2","rt2","brand1","type1","discount1","brand2","type2","discount2","brand3","type3","discount3")]
colnames(fc.dta)=c("subject","session","ID","block","response","rt","brand1","type1","discount1","brand2","type2","discount2","brand3","type3","discount3")



fc.dta$discount1=as.numeric(as.character(fc.dta$discount1))
fc.dta$discount2=as.numeric(as.character(fc.dta$discount2))
fc.dta$discount3=as.numeric(as.character(fc.dta$discount3))
fc.dta$item_old1=paste(fc.dta$brand1,fc.dta$type1,sep="_")
fc.dta$item_old2=paste(fc.dta$brand2,fc.dta$type2,sep="_")
fc.dta$item_old3=paste(fc.dta$brand3,fc.dta$type3,sep="_")
fc.dta$Discount_25_1=ifelse(fc.dta$discount1=="25",1,0)
fc.dta$Discount_99_1=ifelse(fc.dta$discount1=="99",1,0)
fc.dta$Discount_25_2=ifelse(fc.dta$discount2=="25",1,0)
fc.dta$Discount_99_2=ifelse(fc.dta$discount2=="99",1,0)
fc.dta$Discount_25_3=ifelse(fc.dta$discount3=="25",1,0)
fc.dta$Discount_99_3=ifelse(fc.dta$discount3=="99",1,0)



colnames(item_new)=c("item_old1","item1")
fc.dta=merge(fc.dta,item_new)
colnames(item_new)=c("item_old2","item2")
fc.dta=merge(fc.dta,item_new)
colnames(item_new)=c("item_old3","item3")
fc.dta=merge(fc.dta,item_new)







fc_rt=aggregate(rt~ID,fc.dta,FUN=median)
hist(fc_rt$rt,breaks=40,xlab="RT",main="Median of reaction time (2AFC)")
abline(v=0.49,col="red")
subset(fc_rt,rt<0.5)
#### Bad guys
ID     rt
1   1 0.3590
3   3 0.4200
6   6 0.4305
10 10 0.1770
11 11 0.0630
12 12 0.4275
25 25 0.0485
27 27 0.2060
28 28 0.4935
32 32 0.4725
35 35 0.1300
39 39 0.3980
41 41 0.3820
45 45 0.3780
50 50 0.4630
54 54 0.3630

fc.dta=subset(fc.dta,!(ID%in%bad_ids))


write.csv(fc.dta,file="fc.csv",row.names=F)












