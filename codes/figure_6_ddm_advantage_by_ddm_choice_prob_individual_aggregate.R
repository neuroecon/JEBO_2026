

workdir='/Users/nanchen/Dropbox (Zhong Lab)/DDM + Market Share/Figures/Data'
setwd(workdir)
fc.dta =read.csv(file.path(workdir,'session3_2afc_data_and_estimates_matched_at_individual_level.csv'))

attach(fc.dta)
fc.dta$competitiveness= prob_d #abs(prob_d-0.5)#-(prob_1+ prob_2+ prob_3-largest- smallest)
fc.dta$ddm_advantage= deviation_l-deviation_d
detach(fc.dta)

loess_mod <- loess(ddm_advantage ~ competitiveness, fc.dta)
pred <- predict(loess_mod, fc.dta, se=TRUE)
fc.dta $lwl <- pred$fit-1.96*pred$se.fit
fc.dta $upl <- pred$fit+1.96*pred$se.fit
fc.dta $fit <- pred$fit
pdf("~/Desktop/ddm/2afc_individual.pdf",width=6,height=5,paper='special')       
    fc.dta = fc.dta[order(fc.dta $competitiveness),]
    attach(fc.dta)
    plot(
      competitiveness, fit,type="l",ylim=c(0.,0.06),
    lwd=1,lty=1,col=2,xlab="DDM Choice Probabilities (individual level)",ylab="MAE (Logit-DDM)")
	#points(competitiveness, ddm_advantage,col=2,pch=3,cex=0.5)
    lines(competitiveness,lwl,
      lwd=1,lty=2,col=2)
    lines(competitiveness,upl,
      lwd=1,lty=2,col=2)
    detach(fc.dta)
    #legend("topleft",c("3AFC","2AFC"),col=c(1,2),lty=1)
dev.off()


share =read.csv(file.path(workdir,'session2_yn_predict_session2_2afc_market_share.csv'))
share$diff_marketshare_logit_minus_ddm=share$error_l-share$error_d

loess_mod <- loess(diff_marketshare_logit_minus_ddm ~ prob_d_new, share)
pred <- predict(loess_mod, share0, se=TRUE)
share $lwl <- pred$fit-1.96*pred$se.fit
share $upl <- pred$fit+1.96*pred$se.fit
share $fit <- pred$fit
pdf("~/Desktop/ddm/2afc_aggregate.pdf",width=6,height=5,paper='special')     
    share_plot= share[order(share $prob_d_new),]
    attach(share_plot)
    plot(
      prob_d_new, fit,type="l",ylim=c(-0.05,0.10),xlab="DDM Choice Probabilities (market level)",ylab="MAE (Logit-DDM)",
    lwd=1,lty=1,col="blue")
	#points(response_new, diff_marketshare_logit_minus_ddm,col=1)
    lines(prob_d_new,lwl,
      lwd=1,lty=2,col="blue")
    lines(prob_d_new,upl,
      lwd=1,lty=2,col="blue")
    detach(share_plot)
dev.off()

