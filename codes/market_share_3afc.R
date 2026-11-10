
library(ggplot2)
library(lfe)

########################################################################
workdir='/Users/chennan/Dropbox/DDM + Market Share/Analysis'
para_dir='/Users/chennan/Dropbox/DDM + Market Share/Analysis/All/Results/session4_random/02-02-2017'
setwd(workdir)
id_data=seq(1,46)
bad_ids=c(4,6,18,29)
id_data=setdiff(id_data,bad_ids)


# read data
fc.dta=read.csv('/Users/chennan/Dropbox/DDM + Market Share/Analysis/All/Data/Session4/fc.csv')
fc.dta =subset(fc.dta,ID%in% id_data)


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

fc.dta$response=ifelse(fc.dta$response==1,1,0)


############################ Read Coefficients ###################
coefficients = as.data.frame(matrix(0,nrow=nrow(fc.dta),ncol=18))
colnames(coefficients) = c('intercept_l','V1_l','V2_l','V3_l','V4_l','V5_l','V6_l','V7_l','intercept_d','V1_d','V2_d','V3_d','V4_d','V5_d','V6_d','V7_d','NRT_d','Barrier_d')
fc.dta = cbind(fc.dta, coefficients)



for (i in id_data){
    
    dir_logit= file.path(para_dir,i,paste0(i,'_logit_estimates.csv'))
    
    dir_ddm= file.path(para_dir,i,paste0(i,'_ddm_estimates.csv'))
    
    dta_logit=read.csv(dir_logit)
    dta_ddm=read.csv(dir_ddm)
    
    fc.dta $intercept_l =ifelse(fc.dta$ID==i,dta_logit[1,2],fc.dta$intercept_l)
    fc.dta $V1_l =ifelse(fc.dta$ID==i,dta_logit[2,2],fc.dta $V1_l)
    fc.dta $V2_l =ifelse(fc.dta$ID==i,dta_logit[3,2],fc.dta $V2_l)
    fc.dta $V3_l =ifelse(fc.dta$ID==i,dta_logit[4,2],fc.dta $V3_l)
    fc.dta $V4_l =ifelse(fc.dta$ID==i,dta_logit[5,2],fc.dta $V4_l)
    fc.dta $V5_l =ifelse(fc.dta$ID==i,dta_logit[6,2],fc.dta $V5_l)
    fc.dta $V6_l =ifelse(fc.dta$ID==i,dta_logit[7,2],fc.dta $V6_l)
    fc.dta $V7_l =ifelse(fc.dta$ID==i,dta_logit[8,2],fc.dta $V7_l)
    
    
    fc.dta $Barrier_d= ifelse(fc.dta$ID==i,as.numeric(as.character(dta_ddm[1,2])),fc.dta $Barrier_d)
    fc.dta $NRT_d= ifelse(fc.dta$ID==i,as.numeric(as.character(dta_ddm[2,2])),fc.dta $NRT_d)
    fc.dta $intercept_d=ifelse(fc.dta$ID==i,as.numeric(as.character(dta_ddm[3,2])),fc.dta $intercept_d)
    fc.dta $V1_d =ifelse(fc.dta$ID==i,as.numeric(as.character(dta_ddm[4,2])),fc.dta $V1_d)
    fc.dta $V2_d =ifelse(fc.dta$ID==i,as.numeric(as.character(dta_ddm[5,2])),fc.dta $V2_d)
    fc.dta $V3_d =ifelse(fc.dta$ID==i,as.numeric(as.character(dta_ddm[6,2])),fc.dta $V3_d)
    fc.dta $V4_d =ifelse(fc.dta$ID==i,as.numeric(as.character(dta_ddm[7,2])),fc.dta $V4_d)
    fc.dta $V5_d =ifelse(fc.dta$ID==i,as.numeric(as.character(dta_ddm[8,2])),fc.dta $V5_d)
    fc.dta $V6_d =ifelse(fc.dta$ID==i,as.numeric(as.character(dta_ddm[9,2])),fc.dta $V6_d)
    fc.dta $V7_d =ifelse(fc.dta$ID==i,as.numeric(as.character(dta_ddm[10,2])),fc.dta $V7_d)
    
}




# calculate choice probabilities
attach(fc.dta)

fc.dta$U_1_logit=
v0_1* intercept_l+v1_1* V1_l+v2_1* V2_l+v3_1* V3_l+v4_1* V4_l+v5_1* V5_l+v6_1* V6_l+v7_1* V7_l

fc.dta$U_2_logit=
v0_2* intercept_l+v1_2* V1_l+v2_2* V2_l+v3_2* V3_l+v4_2* V4_l+v5_2* V5_l+v6_2* V6_l+v7_2* V7_l

fc.dta$U_3_logit=
v0_3* intercept_l+v1_3* V1_l+v2_3* V2_l+v3_3* V3_l+v4_3* V4_l+v5_3* V5_l+v6_3* V6_l+v7_3* V7_l



fc.dta$U_1_ddm=
(v0_1* intercept_d+v1_1* V1_d+v2_1* V2_d+v3_1* V3_d+v4_1* V4_d+v5_1* V5_d+v6_1* V6_d+v7_1* V7_d)*fc.dta$Barrier_d

fc.dta$U_2_ddm=
(v0_2* intercept_d+v1_2* V1_d+v2_2* V2_d+v3_2* V3_d+v4_2* V4_d+v5_2* V5_d+v6_2* V6_d+v7_2* V7_d)*fc.dta$Barrier_d

fc.dta$U_3_ddm=
(v0_3* intercept_d+v1_3* V1_d+v2_3* V2_d+v3_3* V3_d+v4_3* V4_d+v5_3* V5_d+v6_3* V6_d+v7_3* V7_d)*fc.dta$Barrier_d


detach(fc.dta)

attach(fc.dta)

fc.dta$prob_l=exp(U_1_logit)/(exp(U_1_logit) + exp(U_2_logit) + exp(U_3_logit))

fc.dta$prob_d=exp(U_1_ddm)/(exp(U_1_ddm) + exp(U_2_ddm) + exp(U_3_ddm))

detach(fc.dta)















