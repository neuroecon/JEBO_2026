

workdir='/Users/nanchen/Dropbox (Zhong Lab)/DDM + Market Share/Figures/Data'
setwd(workdir)
share_2afc =read.csv(file.path(workdir,'session5_2afc_market_share.csv'))

attach(share_2afc)
share_2afc $ddm_advantage= logit_distance- ddm_distance 
share_2afc $largest=pmax( prob_1_d,  prob_2_d)#,share$prob_3)
share_2afc $smallest=pmin( prob_1_d,  prob_2_d)#,share$prob_3)
share_2afc $competitiveness= pmax( prob_1_d,  prob_2_d)
#largest-smallest-(prob_1+ prob_2+ prob_3-largest- smallest)
detach(share_2afc)

loess_mod <- loess(ddm_advantage ~ competitiveness, share_2afc)
pred <- predict(loess_mod, share_2afc, se=TRUE)
share_2afc $lwl <- pred$fit-1.96*pred$se.fit
share_2afc $upl <- pred$fit+1.96*pred$se.fit
share_2afc $fit <- pred$fit
    
share_plot= share_2afc[order(share_2afc$competitiveness),]
attach(share_plot)
plot(competitiveness, fit,type="l",ylim=c(-0.1,0.2),xlim=c(0.3,1),
    lwd=1,lty=1,col="purple4",xlab="DDM Choice Probabilities for the most popular item (market level)",ylab="MAE (Logit-DDM)")
	#points(competitiveness, ddm_advantage,col=2,pch=3,cex=0.5)
lines(competitiveness,lwl,
      lwd=1,lty=2,col="purple4")
lines(competitiveness,upl,
      lwd=1,lty=2,col="purple4")
detach(share_plot)
legend("topleft",c("3AFC","2AFC"),col=c("palevioletred1","purple4"),lty=1)



share_3afc =read.csv(file.path(workdir,'session5_3afc_market_share.csv'))
attach(share_3afc)
share_3afc $largest=pmax( prob_1_d,  prob_2_d, prob_3_d)
#share$smallest=pmin(share$prob_1_d, share$prob_2_d,share$prob_3_d)
share_3afc $ddm_advantage= logit_distance- ddm_distance 
share_3afc $competitiveness=pmax( prob_1_d,  prob_2_d, prob_3_d)
detach(share_3afc)


loess_mod <- loess(ddm_advantage ~ competitiveness, share_3afc)
pred <- predict(loess_mod, share_3afc, se=TRUE)
share_3afc $lwl <- pred$fit-1.96*pred$se.fit
share_3afc $upl <- pred$fit+1.96*pred$se.fit
share_3afc $fit <- pred$fit
    
share_plot= share_3afc[order(share_3afc $competitiveness),]
attach(share_plot)
lines(
      competitiveness, fit,type="l",
    lwd=1,lty=1,col="palevioletred1")
	#points(competitiveness, ddm_advantage,col=1,pch=2,cex=0.5)
lines(competitiveness,lwl,
      lwd=1,lty=2,col="palevioletred1")
lines(competitiveness,upl,
      lwd=1,lty=2,col="palevioletred1")
detach(share_plot)




