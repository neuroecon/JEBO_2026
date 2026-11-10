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


# naive correct
fc.dta$response_l=ifelse(fc.dta$prob_l>0.5,1,0)
fc.dta$response_d=ifelse(fc.dta$prob_d>0.5,1,0)
fc.dta$naivecorrect_l=ifelse(fc.dta$response_l==fc.dta$response,1,0)
fc.dta$naivecorrect_d=ifelse(fc.dta$response_d==fc.dta$response,1,0)
a=aggregate(naivecorrect_l~ID, fc.dta,FUN=mean)
b=aggregate(naivecorrect_d~ID, fc.dta,FUN=mean)
c1=merge(a,b)
c1$ddm_did_better=ifelse(c1$naivecorrect_d>c1$naivecorrect_l,"better",ifelse(c1$naivecorrect_d==c1$naivecorrect_l,"same","worse"))
table(c1$ddm_did_better)
#write.csv(c1,"naive_correct_by_id_3afc.csv")


# absolute deviation by id
fc.dta$deviation_l=abs(fc.dta$prob_l-fc.dta$response)
fc.dta$deviation_d=abs(fc.dta$prob_d-fc.dta$response)
a=aggregate(deviation_l ~ID, fc.dta,FUN=mean)
b=aggregate(deviation_d ~ID, fc.dta,FUN=mean)
c2=merge(a,b)
c2$ddm_did_better=ifelse(c2$deviation_d<c2$deviation_l,"better",ifelse(c2$deviation_d ==c2$deviation_l,"same","worse"))
table(c2$ddm_did_better)
#write.csv(c2,"mean_abosolute_error_by_id_3afc.csv")

#################### Model comparision
pdf("naive_correct_by_id_3afc.pdf",width=10,height=10)
plot(c1$naivecorrect_l,c1$naivecorrect_d,pch=19,asp=1,col="blue",main="naive correct by subjects",xlab="Logit",ylab="ddm", cex = 1)
abline(0,1)
dev.off()
pdf("mean_abosolute_error_by_id_3afc.pdf",width=10,height=10)
plot(c2$deviation_l,c2$deviation_d,pch=19,asp=1,col="blue",main="absolute deviation by subjects",xlab="Logit",ylab="ddm", cex = 1)
abline(0,1)
dev.off()
