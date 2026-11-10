
library(readr)

#################### Market share within sample
fc.dta$item1= parse_number(fc.dta$item1)
fc.dta$item2= parse_number(fc.dta$item2)
fc.dta$item1_new=ifelse(fc.dta$item2>fc.dta$item1,fc.dta$item2,fc.dta$item1)
fc.dta$item2_new=ifelse(fc.dta$item2>fc.dta$item1,fc.dta$item1,fc.dta$item2)
fc.dta$discount1_new=ifelse(fc.dta$item2>fc.dta$item1,fc.dta$discount2,fc.dta$discount1)
fc.dta$discount2_new=ifelse(fc.dta$item2>fc.dta$item1,fc.dta$discount1,fc.dta$discount2)
fc.dta$response_new=ifelse(fc.dta$item2>fc.dta$item1,1-fc.dta$response,fc.dta$response)
fc.dta$prob_l_new=ifelse(fc.dta$item2>fc.dta$item1,1-fc.dta$prob_l,fc.dta$prob_l)
fc.dta$prob_d_new=ifelse(fc.dta$item2>fc.dta$item1,1-fc.dta$prob_d,fc.dta$prob_d)


share=aggregate(response_new~item1_new+item2_new+discount1_new+discount2_new,fc.dta,FUN=mean)
share_l=aggregate(prob_l_new~item1_new+item2_new+discount1_new+discount2_new,fc.dta,FUN=mean)
share_d=aggregate(prob_d_new~item1_new+item2_new+discount1_new+discount2_new,fc.dta,FUN=mean)
share=merge(share,share_l)
share=merge(share,share_d)
share$error_l=abs(share$response_new-share$prob_l_new)
share$error_d=abs(share$response_new-share$prob_d_new)
share$diff_marketshare_logit_minus_ddm=share$error_l-share$error_d

#pdf("market_share_within_sample_2afc.pdf",width=10,height=10)
hist(share$diff_marketshare_logit_minus_ddm,breaks=30,xlim=c(-0.25,0.25),main="market share error (logit minus ddm)",col="blue")
abline(v=0,col="red",lwd=3)
abline(v=mean(share$diff_marketshare_logit_minus_ddm),col="red",lwd=3)
#dev.off()
#################### Market share within sample



#################### Market share out of sample
N=500
Error=vector()
T_stats=vector()
for (n in 1:N){
	
	set.seed(n)
	id_pred = sample(id_list,30,replace=F)
	id_est = setdiff(id_list, id_pred)
	# market shares
	share=aggregate(response~item1+item2++discount1+discount2,subset(fc.dta,ID%in% id_pred),FUN=mean)
	share_l=aggregate(prob_l ~item1+item2+discount1+discount2,subset(fc.dta,ID%in% id_est),FUN=mean)
	share_d=aggregate(prob_d ~item1+item2+discount1+discount2,subset(fc.dta,ID%in% id_est),FUN=mean)
	share=merge(share,share_l)
	share=merge(share,share_d)
	share$error_l=abs(share$response-share$prob_l)
	share$error_d=abs(share$response-share$prob_d)
	error=mean(share$error_l-share$error_d)
	Error=c(Error, error)

}
#pdf("market_share_out_of_sample_2afc.pdf",width=10,height=10)
hist( Error ,breaks=100,main="500 30-30 simulations ", xlab="Mean Logit market share error minus Mean DDM market share error",col="blue")
abline(v=quantile(Error, 0.05),col="red",lwd=3)
abline(v=quantile(Error, 0.01),col="red",lwd=3)
#dev.off()
#################### Market share out of sample

n=7
set.seed(n)
id_pred = sample(id_list,30,replace=F)
id_est = setdiff(id_list, id_pred)
# market shares
share=aggregate(response~item1+item2++discount1+discount2,subset(fc.dta,ID%in% id_pred),FUN=mean)
share_l=aggregate(prob_l ~item1+item2+discount1+discount2,subset(fc.dta,ID%in% id_est),FUN=mean)
share_d=aggregate(prob_d ~item1+item2+discount1+discount2,subset(fc.dta,ID%in% id_est),FUN=mean)
share=merge(share,share_l)
share=merge(share,share_d)
share$error_l=abs(share$response-share$prob_l)
share$error_d=abs(share$response-share$prob_d)

Error=share$error_l-share$error_d
hist( Error ,breaks=100,main="500 30-30 simulations ", xlab="Mean Logit market share error minus Mean DDM market share error",col="blue")
abline(v=mean(share$error_l-share$error_d),col="red",lwd=3)














