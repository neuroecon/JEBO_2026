session3_estimates=read.csv(file.path(data_dir, "partworth_data.csv"))
excluded_ids=c(11,25,27,41)
session3_estimates=subset(session3_estimates, !(ID%in%excluded_ids))

item_partworth=read.csv(file.path(data_dir, "item_dictionary.csv"))
item_partworth $logit=0
item_partworth $ddm=0

attach(session3_estimates)
item_partworth$logit[1]=mean(intercept_l)
item_partworth$logit[2]=mean(V1_l)+item_partworth$logit[1]
item_partworth$logit[3]=mean(V2_l)+item_partworth$logit[1]
item_partworth$logit[4]=mean(V3_l)+item_partworth$logit[1]
item_partworth$logit[5]=mean(V4_l)+item_partworth$logit[1]
item_partworth$logit[6]=mean(V5_l)+item_partworth$logit[1]

item_partworth$ddm[1]=mean(intercept_d* Barrier_d)
item_partworth$ddm[2]=mean(V1_d* Barrier_d)+item_partworth$ddm[1]
item_partworth$ddm[3]=mean(V2_d* Barrier_d) +item_partworth$ddm[1]
item_partworth$ddm[4]=mean(V3_d* Barrier_d)+item_partworth$ddm[1]
item_partworth$ddm[5]=mean(V4_d* Barrier_d)+item_partworth$ddm[1]
item_partworth$ddm[6]=mean(V5_d* Barrier_d)+item_partworth$ddm[1]
detach(session3_estimates)
Starbucks_Latte_l=item_partworth$logit[1]
Starbucks_Cappuccino_l=item_partworth$logit[6]
McCafe_Latte_l=item_partworth$logit[3]
McCafe_Cappuccino_l=item_partworth$logit[2]
Peets_Latte_l=item_partworth$logit[5]
Peets_Cappuccino_l=item_partworth$logit[4]

Starbucks_Latte_d=item_partworth$ddm[1]
Starbucks_Cappuccino_d=item_partworth$ddm[6]
McCafe_Latte_d=item_partworth$ddm[3]
McCafe_Cappuccino_d=item_partworth$ddm[2]
Peets_Latte_d=item_partworth$ddm[5]
Peets_Cappuccino_d=item_partworth$ddm[4]

McCafe_l= 1/2*(McCafe_Latte_l+ McCafe_Cappuccino_l)
Starbucks_l= 1/2*(Starbucks_Latte_l+ Starbucks_Cappuccino_l)-McCafe_l
Peets_l= 1/2*(Peets_Latte_l + Peets_Cappuccino_l)-McCafe_l
McCafe_l=0

McCafe_d= 1/2*(McCafe_Latte_d+ McCafe_Cappuccino_d)
Starbucks_d= 1/2*(Starbucks_Latte_d+ Starbucks_Cappuccino_d)-McCafe_d
Peets_d= 1/2*(Peets_Latte_d + Peets_Cappuccino_d)-McCafe_d
McCafe_d=0

Cappuccino_l=1/3*(Starbucks_Cappuccino_l + McCafe_Cappuccino_l + Peets_Cappuccino_l)
Latte_l=1/3*(Starbucks_Latte_l+ McCafe_Latte_l + Peets_Latte_l)-Cappuccino_l
Cappuccino_l=0

Cappuccino_d=1/3*(Starbucks_Cappuccino_d + McCafe_Cappuccino_d + Peets_Cappuccino_d)
Latte_d=1/3*(Starbucks_Latte_d+ McCafe_Latte_d + Peets_Latte_d)-Cappuccino_d
Cappuccino_d=0

attach(session3_estimates)
discount_0_l=0
discount_0_d=0
discount_25_l=mean(V6_l)
discount_99_l=mean(V7_l)
discount_25_d=mean(V6_d* Barrier_d)
discount_99_d=mean(V7_d* Barrier_d)
detach(session3_estimates)

ylb=-2
yub=6
xlb=0.
xub=6
delta=0.5

plot(1-delta, McCafe_l,type="p",col="darkblue", pch=18, cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="Attribute",ylab="Utility", xaxt='n',
	main="Estimates averaged over all subjects")
text(1-delta, ylb,"McCafe",col="black", pos=3, cex= 0.7)
par(new=T)
plot(1, Peets_l,type="p",col="darkblue", pch=18,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
text(1, ylb,"Peets",col="black", pos=3, cex= 0.7)
par(new=T)
plot(1+ delta, Starbucks_l,type="p",col="darkblue", pch=18,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
text(1+ delta, ylb,"Starbucks",col="black", pos=3, cex= 0.7)
par(new=T)
text(1, yub,"Brand",col="black", pos=1, cex= 1.)
x=c(1-delta,1,1+delta)
y=c(McCafe_l, Peets_l, Starbucks_l)
lines(x,y,col="darkblue")
par(new=T)
plot(0.98-delta, McCafe_d,type="p",col="darkred", pch=20,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
par(new=T)
plot(0.98, Peets_d,type="p",col="darkred", pch=20,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
par(new=T)
plot(0.98 + delta, Starbucks_d,type="p",col="darkred", pch= 20,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
x=c(0.98-delta, 0.98, 0.98 +delta)
y=c(McCafe_d, Peets_d, Starbucks_d)
lines(x,y,col="darkred")
par(new=T)

plot(3-delta, Cappuccino_l,type="p",col="darkblue", pch=18,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
text(3-delta, ylb,"Cappuccino",col="black", pos=3, cex= 0.7)
par(new=T)
plot(3+delta, Latte_l,type="p",col="darkblue", pch=18,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
text(3+delta, ylb,"Latte",col="black", pos=3, cex= 0.7)
par(new=T)
text(3, yub,"Flavor",col="black", pos=1, cex= 1.)
x=c(3-delta,3+delta)
y=c(Cappuccino_l, Latte_l)
lines(x,y,col="darkblue")
par(new=T)
plot(2.98-delta, Cappuccino_d,type="p",col="darkred", pch=20,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
par(new=T)
plot(2.98+delta, Latte_d,type="p",col="darkred", pch= 20,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
x=c(2.98-delta, 2.98 +delta)
y=c(Cappuccino_d, Latte_d)
lines(x,y,col="darkred")
par(new=T)

plot(5-delta, discount_0_l,type="p",col="darkblue", pch=18,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
text(5-delta, ylb,"0 cent",col="black", pos=3, cex= 0.7)
par(new=T)
plot(5, discount_25_l,type="p",col="darkblue", pch=18,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
text(5, ylb,"25 Cent",col="black", pos=3, cex= 0.7)
par(new=T)
plot(5+delta, discount_99_l,type="p",col="darkblue", pch=18,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
text(5+delta, ylb,"99 Cent",col="black", pos=3, cex= 0.7)
par(new=T)
text(5., yub,"Discount",col="black", pos=1, cex= 1.)
x=c(5-delta,5,5+delta)
y=c(0,discount_25_l, discount_99_l)
lines(x,y,col="darkblue")
par(new=T)
plot(4.98-delta, discount_0_d,type="p",col="darkred", pch= 20,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
par(new=T)
plot(4.98, discount_25_d,type="p",col="darkred", pch= 20,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
par(new=T)
plot(4.98 +delta, discount_99_d,type="p",col="darkred", pch= 20,  cex=1.3,
	xlim=c(xlb,xub),ylim=c(ylb, yub),xlab="",ylab="", xaxt='n',
	main="Estimates averaged over all subjects")
x=c(4.98-delta, 4.98, 4.98 +delta)
y=c(discount_0_d,discount_25_d,discount_99_d)
lines(x,y,col="darkred")

legend(-0.1,5.,legend=c("Logit","DDM"),pch=c(18,20),
	col=c("darkblue","darkred"), cex=1.3,y.intersp=1.5)
