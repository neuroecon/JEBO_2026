

############################### Set up
workdir='/Users/chennan/Dropbox/DDM + Market Share/Analysis/All/Results'
setwd(workdir)
para_dir='/Users/chennan/Dropbox/DDM + Market Share/Analysis/All/Results/session3_random/02-01-2017'


id_para=
c(24,43,42,43,57,19,51,39,20,54,3,30,13,35,52,61,21,31,7,49,44,8,28,29,40,63,47,33,58,12,22,46,5,45,50,60,15,9,6,10,34,4,62,18,56,17,55,26,59,32,23,16,38,48,1,36,14,2,37,53) # session 3 estimated parameters


############################## Prepare real data (session 4)
# read real data
fc.dta=read.csv('/Users/chennan/Dropbox/DDM + Market Share/Analysis/All/Data/Session4/fc.csv')
bad_ids=c(4,6,18,29)
fc.dta=subset(fc.dta,!(ID%in%bad_ids))
#look at market share for the first item only
#fc.dta$response=ifelse(fc.dta$response==1,1,0)
attach(fc.dta)
fc.dta$chocice_1=ifelse(response==1,1,0)
fc.dta$chocice_2=ifelse(response==2,1,0)
fc.dta$chocice_3=ifelse(response==3,1,0)
detach(fc.dta)


# 1st item matrix
Item1 = as.data.frame(model.matrix(~fc.dta$item1+0))
colnames(Item1) = c('v0_1','v1_1','v2_1','v3_1','v4_1','v5_1')
Item1$v0_1 =1 # intercept
Item1$v6_1 = fc.dta$Discount_25_1 # 25 cent dummy
Item1$v7_1 = fc.dta$Discount_99_1 # 99 cent dummy

# 2nd item matrix
Item2= as.data.frame(model.matrix(~fc.dta$item2+0))
colnames(Item2)= c('v0_2','v1_2','v2_2','v3_2','v4_2','v5_2')
Item2$v0_2 =1 # intercept
Item2$v6_2 = fc.dta$Discount_25_2 # 25 cent dummy
Item2$v7_2 = fc.dta$Discount_99_2 # 99 cent dummy

# 3rd item matrix
Item3= as.data.frame(model.matrix(~fc.dta$item3+0))
colnames(Item3)= c('v0_3','v1_3','v2_3','v3_3','v4_3','v5_3')
Item3$v0_3 =1 # intercept
Item3$v6_3 = fc.dta$Discount_25_3 # 25 cent dummy
Item3$v7_3 = fc.dta$Discount_99_3 # 99 cent dummy

fc.dta=cbind(fc.dta,Item1)
fc.dta=cbind(fc.dta,Item2)
fc.dta=cbind(fc.dta,Item3)
############################## Prepare real data (session 4)


############################## Prepare prediction data (choice menues + predicted parameters)
# dataframe of chocie menue (all subjects face same choices, so enough to use subject 1)
choices.dta=subset(fc.dta,ID==1)[,c('item1','item2','item3','discount1','discount2','discount3',
colnames(Item1),colnames(Item2),colnames(Item3))]
# dataframe for predicted parameters
coefficients.dta = as.data.frame(matrix(0,nrow=0,ncol=19))
colnames(coefficients.dta) = c('ID',
'intercept_l','V1_l','V2_l','V3_l','V4_l','V5_l','V6_l','V7_l',
'Barrier_d','NRT_d','intercept_d','V1_d','V2_d','V3_d','V4_d','V5_d','V6_d','V7_d')

for (id in id_para){
    
    dir_logit=file.path(para_dir,id,paste0(id,'_logit_estimates.csv'))
    dir_ddm=file.path(para_dir,id,paste0(id,'_ddm_estimates.csv'))
    logit_est=read.csv(dir_logit)
    ddm_est=read.csv(dir_ddm)
    estimates=c(id,logit_est[,2],ddm_est[,2])
    coefficients.dta=rbind(coefficients.dta,setNames(as.list(estimates), names(coefficients.dta)))
  
}
# merge choice menues and predicted parameters
fc.pred=merge(choices.dta,coefficients.dta)
############################## Prepare prediction data (choice menues + predicted parameters)


############################## Calculate and compare prediction with real data
# calculate utilities
attach(fc.pred)
fc.pred$U_1_logit=
v0_1* intercept_l+v1_1* V1_l+v2_1* V2_l+v3_1* V3_l+v4_1* V4_l+v5_1* V5_l+v6_1* V6_l+v7_1* V7_l
fc.pred$U_2_logit=
v0_2* intercept_l+v1_2* V1_l+v2_2* V2_l+v3_2* V3_l+v4_2* V4_l+v5_2* V5_l+v6_2* V6_l+v7_2* V7_l
fc.pred$U_3_logit=
v0_3* intercept_l+v1_3* V1_l+v2_3* V2_l+v3_3* V3_l+v4_3* V4_l+v5_3* V5_l+v6_3* V6_l+v7_3* V7_l

fc.pred$U_1_ddm=
(v0_1* intercept_d+v1_1* V1_d+v2_1* V2_d+v3_1* V3_d+v4_1* V4_d+v5_1* V5_d+v6_1* V6_d+v7_1* V7_d)*Barrier_d
fc.pred$U_2_ddm=
(v0_2* intercept_d+v1_2* V1_d+v2_2* V2_d+v3_2* V3_d+v4_2* V4_d+v5_2* V5_d+v6_2* V6_d+v7_2* V7_d)*Barrier_d
fc.pred$U_3_ddm=
(v0_3* intercept_d+v1_3* V1_d+v2_3* V2_d+v3_3* V3_d+v4_3* V4_d+v5_3* V5_d+v6_3* V6_d+v7_3* V7_d)*Barrier_d
detach(fc.pred)

# calculate choice probabilities
attach(fc.pred)
fc.pred$prob_1_l=exp(U_1_logit)/(exp(U_1_logit) + exp(U_2_logit) + exp(U_3_logit))
fc.pred$prob_1_d=exp(U_1_ddm)/(exp(U_1_ddm) + exp(U_2_ddm) + exp(U_3_ddm))
fc.pred$prob_2_l=exp(U_2_logit)/(exp(U_1_logit) + exp(U_2_logit) + exp(U_3_logit))
fc.pred$prob_2_d=exp(U_2_ddm)/(exp(U_1_ddm) + exp(U_2_ddm) + exp(U_3_ddm))
fc.pred$prob_3_l=exp(U_3_logit)/(exp(U_1_logit) + exp(U_2_logit) + exp(U_3_logit))
fc.pred$prob_3_d=exp(U_3_ddm)/(exp(U_1_ddm) + exp(U_2_ddm) + exp(U_3_ddm))
detach(fc.pred)







# calculate market share
share_1=aggregate(chocice_1~item1+item2+item3+discount1+discount2+discount3,fc.dta,FUN=mean)   #real data
share_1_l=aggregate(prob_1_l ~item1+item2+item3+discount1+discount2+discount3,fc.pred,FUN=mean) #logit
share_1_d=aggregate(prob_1_d ~item1+item2+item3+discount1+discount2+discount3,fc.pred,FUN=mean) #ddm

share_2=aggregate(chocice_2~item1+item2+item3+discount1+discount2+discount3,fc.dta,FUN=mean)   #real data
share_2_l=aggregate(prob_2_l ~item1+item2+item3+discount1+discount2+discount3,fc.pred,FUN=mean) #logit
share_2_d=aggregate(prob_2_d ~item1+item2+item3+discount1+discount2+discount3,fc.pred,FUN=mean) #ddm

share_3=aggregate(chocice_3~item1+item2+item3+discount1+discount2+discount3,fc.dta,FUN=mean)   #real data
share_3_l=aggregate(prob_3_l ~item1+item2+item3+discount1+discount2+discount3,fc.pred,FUN=mean) #logit
share_3_d=aggregate(prob_3_d ~item1+item2+item3+discount1+discount2+discount3,fc.pred,FUN=mean) #ddm

share=merge(share_1,share_1_l)
share=merge(share,share_1_d)
share=merge(share,share_2)
share=merge(share,share_2_l)
share=merge(share,share_2_d)
share=merge(share,share_3)
share=merge(share,share_3_l)
share=merge(share,share_3_d)

share$error_1_l=abs(share$chocice_1-share$prob_1_l)
share$error_1_d=abs(share$chocice_1-share$prob_1_d)
share$error_2_l=abs(share$chocice_2-share$prob_2_l)
share$error_2_d=abs(share$chocice_2-share$prob_2_d)
share$error_3_l=abs(share$chocice_3-share$prob_3_l)
share$error_3_d=abs(share$chocice_3-share$prob_3_d)

attach(share)
share$ddm_advatage=( (abs(error_1_l)+abs(error_2_l)+abs(error_3_l)) - (abs(error_1_d)+abs(error_2_d)+abs(error_3_d)) )/3
detach(share)


hist(share$ddm_advatage,xlim=c(-0.25,0.25),breaks=30,main="market share error (logit minus ddm)",col="blue")
abline(v=0,col="red",lwd=3)
abline(v=mean(share$ddm_advatage),col="yellow",lwd=3)
median(share$ddm_advatage)
t.test(share$ddm_advatage)

#abline(v=quantile(share$diff_marketshare_logit_minus_ddm, 0.01),col="yellow",lwd=3)
############################## Calculate and compare prediction with real data


random=share$ddm_advatage

correct=share$ddm_advatage

random_error=data.frame(error=random)
correct_error=data.frame(error=correct)
random_error$Estimation="Random rt"
correct_error$Estimation="Correct rt"

#and combine into your new data frame vegLengths
mkt_error <- rbind(random_error, correct_error)

ggplot(mkt_error, aes(error, fill = Estimation))+
geom_histogram(alpha = 0.8,binwidth=0.002,aes(y = ..density..), position = 'identity')+
geom_vline(xintercept=mean(random),color='red')+
geom_vline(xintercept=mean(correct),color='red')












