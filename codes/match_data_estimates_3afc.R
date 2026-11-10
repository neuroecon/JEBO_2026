########################################################################
workdir="/Users/chennan/Box Sync/DDM/Analysis/session_4"
setwd(work_dir)
id_data=seq(1,46)
bad_ids=c(4,6,18,29)
id_data=setdiff(id_data,bad_ids)
estimation_date='12-17-2016'

# read data
fc.dta=read.csv('/Users/chennan/Box Sync/DDM/Data/session4/fc.csv')
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
    
    
    dir_logit= file.path(workdir,estimation_date,i,paste0(i,'_logit_estimates.csv'))
    dir_ddm= file.path(workdir,estimation_date,i,paste0(i,'_ddm_estimates.csv'))
    
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

