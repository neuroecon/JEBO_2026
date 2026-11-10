


### Let item 2 be the competitor, whose price is always 0
fc.dta$discount1_new=ifelse(fc.dta$discount2>0,fc.dta$discount2,fc.dta$discount1)
fc.dta$discount2_new=ifelse(fc.dta$discount2>0,fc.dta$discount1,fc.dta$discount2)
fc.dta$response_new=ifelse(fc.dta$discount2>0,1-fc.dta$response,fc.dta$response)
fc.dta$prob_l_new=ifelse(fc.dta$discount2>0,1-fc.dta$prob_l,fc.dta$prob_l)
fc.dta$prob_d_new=ifelse(fc.dta$discount2>0,1-fc.dta$prob_d,fc.dta$prob_d)
fc.dta$item1_new=ifelse(fc.dta$discount2>0,paste(substr(fc.dta$brand2,1,1),substr(fc.dta$type2,1,5)),paste(substr(fc.dta$brand1,1,1),substr(fc.dta$type1,1,5)))
fc.dta$item2_new=ifelse(fc.dta$discount2>0,paste(substr(fc.dta$brand1,1,1),substr(fc.dta$type1,1,5)),paste(substr(fc.dta$brand2,1,1),substr(fc.dta$type2,1,5)))


# market shares
share=aggregate(response_new~item1_new+item2_new+discount1_new,fc.dta,FUN=mean)
share_l=aggregate(prob_l_new ~item1_new+item2_new+discount1_new,fc.dta,FUN=mean)
share_d=aggregate(prob_d_new ~item1_new+item2_new+discount1_new,fc.dta,FUN=mean)
share=merge(share,share_l)
share=merge(share,share_d)
share$error_l=abs(share$response_new-share$prob_l_new)
share$error_d=abs(share$response_new-share$prob_d_new)
share$diff_marketshare_logit_minus_ddm=share$error_l-share$error_d

# market shares by discount
plot_data=summarySE(share, measurevar="diff_marketshare_logit_minus_ddm", groupvars=c("discount1_new"))
pdf("diff_in_market_share_error_2afc.pdf",width=10,height=10)
ggplot(plot_data, aes(x= as.factor(discount1_new), y= diff_marketshare_logit_minus_ddm))+
geom_errorbar(aes(ymin= diff_marketshare_logit_minus_ddm-ci, ymax= diff_marketshare_logit_minus_ddm +ci), width=.1)+
geom_point()+
labs(title = "diff_marketshare_logit_minus_ddm conditional on discount (with CI)", x = "Discount", y = "Diff in market share prediction error", color = "Model \n")+
theme_bw()
dev.off()

# market shares by market
share$flag=1
share$market_id=paste(share$item1_new,share$item2_new)
share$flag=ave(share$flag,share$market_id,FUN=sum)
share=subset(share,flag==3)

pdf("deamand_curve_2afc.pdf",width=14,height=10)

par(mfrow=c(4,5))
for (market in unique(share$market_id)[1:20] ){
    
        market_data=subset(share,market_id==market)
        item_1=unique(market_data$item1_new)
        item_2=unique(market_data$item2_new)
        y_min=min(market_data$response_new,market_data$prob_l_new,market_data$prob_d_new)-0.01
        y_max=max(market_data$response_new,market_data$prob_l_new,market_data$prob_d_new)+0.01
        market_data$price=c(0,0.25,0.99)
        market_data$price_l=c(-0.01,0.24,0.98)
        market_data$price_d=c(0.01,0.26,1.00)
        plot(response_new~price,data=market_data,type="l",lty=2,pch=24,col="red",ylim=c(y_min,y_max),xlab=paste(item_1,"vs", item_2),xlim=c(-0.02,1.02),ylab="share",cex.lab=0.7)
        par(new=T)
        plot(prob_l_new~price_l,data=market_data,type="l",lty=2,pch=24,col="blue",ylim=c(y_min,y_max),ylab="",xlim=c(-0.02,1.02),xlab=" ")
        par(new=T)
        plot(prob_d_new~price_d,data=market_data,type="l",lty=2,pch=24,col="green",ylim=c(y_min,y_max),ylab="",xlim=c(-0.02,1.02),xlab=" ")

   
}

dev.off()











