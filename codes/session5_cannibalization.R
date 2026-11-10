workdir='/Users/chennan/Dropbox/DDM + Market Share/Analysis/All/Results'
setwd(workdir)
#########################################################################
share_2afc=read.csv("share_2afc.csv")
share_3afc=read.csv("share_3afc.csv")

share_all=merge(share_2afc,share_3afc,by.x = c("item1","item2","discount1","discount2"),by.y = c("item1","item2","discount1","discount2"))
#########################################################################
attach(share_all)
share_all$change_1=prob_1.y-prob_1.x
share_all$change_1_l=prob_1_l.y-prob_1.x
share_all$change_1_d=prob_1_d.y-prob_1.x
share_all$change_2=prob_2.y-prob_2.x
share_all$change_2_l=prob_2_l.y-prob_2.x
share_all$change_2_d=prob_2_d.y-prob_2.x
detach(share_all)

attach(share_all)
share_all$error_1_l=abs(change_1_l-change_1)
share_all$error_1_d=abs(change_1_d-change_1)
share_all$error_2_l=abs(change_2_l-change_2)
share_all$error_2_d=abs(change_2_d-change_2)
detach(share_all)
par(pty="s")
plot(share_all$error_1_d,share_all$error_1_l,pch = 20,xlim=c(0,0.3),ylim=c(0,0.3), 
     asp=1,cex=.7,col="red",
     main="Entry effect",xlab="DDM prediction error",ylab="Logit prediction error")
points(share_all$error_2_d,share_all$error_2_l,pch = 20,cex =.7,col="blue")
abline(0,1)
#########################################################################

#########################################################################
share$competitiveness=(share$prob_1_l-share$prob_2_l)^2
a=loess(ddm_advantage ~ competitiveness, share)
summary(a)
y=predict(a, data.frame(competitiveness = seq(0, 1, 0.01)), se = TRUE)$fit
x=seq(0, 1, 0.01)
plot(x,y,type="l",ylim=c(-0.3,0.15))
par(new=T)
points(share$competitiveness,share$ddm_advantage,pch = 20)
#########################################################################

#########################################################################
share$closeness_1=(share$prob_1_l-share$prob_2_l)^2
share$closeness_2=(share$prob_1_l-share$prob_3_l)^2
share$closeness_3=(share$prob_2_l-share$prob_3_l)^2
share$competitiveness=(share$closeness_1+share$closeness_2+share$closeness_3)/3
a=loess(ddm_advantage ~ competitiveness, share)
summary(a)
y=predict(a, data.frame(competitiveness = seq(0, 0.5, 0.001)), se = TRUE)$fit
x=seq(0, 0.5, 0.001)
plot(x,y,type="l",ylim=c(-0.3,0.15))
par(new=T)
points(share$competitiveness,share$ddm_advantage,pch = 20)
#########################################################################

