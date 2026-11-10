

########################################################################
workdir='/Users/chennan/Dropbox/DDM + Market Share/Analysis/All/Results'
para_dir='/Users/chennan/Dropbox/DDM + Market Share/Analysis/All/Results/session3_correct/02-01-2017'
setwd(workdir)

############################ Data preparation ###################
id_list = c(24,43,42,43,57,19,51,39,20,54,3,30,13,35,52,61,21,31,7,49,44,8,28,29,40,63,47,33,58,12,22,46,5,45,50,60,15,9,6,10,34,4,62,18,56,17,55,26,59,32,23,16,38,48,1,36,14,2,37,53)
# read 2afc data 
fc.dta = read.csv('/Users/chennan/Dropbox/DDM + Market Share/Analysis/All/Data/Session3/fc.csv')
fc.dta = subset(fc.dta,ID%in% id_list)

# generate dummies for left and right items

# left item matrix
Item1 = as.data.frame(model.matrix(~fc.dta$item1+0))
colnames(Item1) = c('v0_1','v1_1','v2_1','v3_1','v4_1','v5_1')
Item1$v0_1 =1 # intercept
Item1$v6_1 = fc.dta$Discount_25_1 # 25 cent dummy
Item1$v7_1 = fc.dta$Discount_99_1 # 99 cent dummy

# right item matrix
Item2= as.data.frame(model.matrix(~fc.dta$item2+0))
colnames(Item2)= c('v0_2','v1_2','v2_2','v3_2','v4_2','v5_2')
Item2$v0_2 =1 # intercept
Item2$v6_2 = fc.dta$Discount_25_2 # 25 cent dummy
Item2$v7_2 = fc.dta$Discount_99_2 # 99 cent dummy

# merge
fc.dta = cbind(fc.dta,Item1)
fc.dta = cbind(fc.dta,Item2)

### checked manually
#unique(fc.dta[,c('item1',colnames(Item1),'discount1')])
#unique(fc.dta[,c('item2',colnames(Item2),'discount2')])
############################ Data preparation ###################



############################ Read Coefficients ###################
coefficients = as.data.frame(matrix(0,nrow=nrow(fc.dta),ncol=18))
colnames(coefficients) = c('intercept_l','V1_l','V2_l','V3_l','V4_l','V5_l','V6_l','V7_l','intercept_d','V1_d','V2_d','V3_d','V4_d','V5_d','V6_d','V7_d','NRT_d','Barrier_d')
fc.dta = cbind(fc.dta, coefficients)


for (i in id_list){
    
    # files' locations
    dir_logit = file.path(para_dir,i,paste0(i,'_logit_estimates.csv'))
	dir_ddm = file.path(para_dir,i,paste0(i,'_ddm_estimates.csv'))
	# read files
	dta_logit = read.csv(dir_logit)
	dta_ddm = read.csv(dir_ddm)
	
    attach(fc.dta)
    # read logit coeeficients for subject i
	fc.dta$intercept_l = ifelse(ID==i,dta_logit[1,2],fc.dta$intercept_l)
	fc.dta$V1_l = ifelse(ID==i,dta_logit[2,2], fc.dta$V1_l)
	fc.dta$V2_l = ifelse(ID==i,dta_logit[3,2], fc.dta$V2_l)
	fc.dta$V3_l = ifelse(ID==i,dta_logit[4,2], fc.dta$V3_l)
	fc.dta$V4_l = ifelse(ID==i,dta_logit[5,2], fc.dta$V4_l)
	fc.dta$V5_l = ifelse(ID==i,dta_logit[6,2], fc.dta$V5_l)
	fc.dta$V6_l = ifelse(ID==i,dta_logit[7,2], fc.dta$V6_l)
	fc.dta$V7_l = ifelse(ID==i,dta_logit[8,2], fc.dta$V7_l)
	# read ddm coeeficients for subject i
	fc.dta$Barrier_d = ifelse(ID==i,as.numeric(as.character(dta_ddm[1,2])), fc.dta$Barrier_d)
	fc.dta$NRT_d = ifelse(ID==i,as.numeric(as.character(dta_ddm[2,2])), fc.dta$NRT_d)
	fc.dta$intercept_d = ifelse(ID==i,as.numeric(as.character(dta_ddm[3,2])), fc.dta$intercept_d)
	fc.dta$V1_d = ifelse(ID==i,as.numeric(as.character(dta_ddm[4,2])), fc.dta$V1_d)
	fc.dta$V2_d = ifelse(ID==i,as.numeric(as.character(dta_ddm[5,2])), fc.dta$V2_d)
	fc.dta$V3_d = ifelse(ID==i,as.numeric(as.character(dta_ddm[6,2])), fc.dta$V3_d)
	fc.dta$V4_d = ifelse(ID==i,as.numeric(as.character(dta_ddm[7,2])), fc.dta$V4_d)
	fc.dta$V5_d = ifelse(ID==i,as.numeric(as.character(dta_ddm[8,2])), fc.dta$V5_d)
	fc.dta$V6_d = ifelse(ID==i,as.numeric(as.character(dta_ddm[9,2])), fc.dta$V6_d)
	fc.dta$V7_d = ifelse(ID==i,as.numeric(as.character(dta_ddm[10,2])), fc.dta$V7_d)
    detach(fc.dta)

}
############################ Read Coefficients ###################


############################ end ###################
#write.csv(fc.dta,"data_estimates_2afc.csv") ### checked manually
############################ end ###################









