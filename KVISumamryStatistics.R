# kvi_lst=read.csv(file = "ComFlag_20161014.csv")
# names(kvi_lst)
# unique(kvi_lst$Division.Name)
# plot(kvi_lst$Commodity.Indicator,col=kvi_lst$Division.Name)
# 
# 
# library(ggplot2)
# ggplot(kvi_lst, aes(x = Commodity.Indicator, y="count", colour = Division.Name)) +
#           geom_point() +
#           facet_wrap( ~ Division.Name)
# 
# summary(kvi_lst$Commodity.Indicator)
# 
# dim(kvi_lst)
# plot(kvi_lst$Product.Status.Desc...Current.)
# 
# dir()
threshold=read.csv("kvi_threshold.csv")

names(threshold)
summary(threshold[,23:34])
boxplot(threshold[,23], ylim=c(0,1000))
plot(density(threshold[,23]),xlim=c(0,5000))

memory.size()
memory.limit()


#ttl_qty_cs<100 non-commodity
# less than 8 cust_cnt< non-commodity
# ttl_sales<1000
#qty_penetration <0.0000440
# cust_penetration<0.0039185
# sales penetration<0.0000440
# non-sensitive, weighted_elasticity>-2.44
# psi count<1 likely non-commodity
#lpc change more than 1.5 times in 13 wks


# library(dplyr)
# dply(threshold, .(ttl_qty_cs), colwise(sd))
# ?ddply
# 
# library(data.table)
# threshold[, sapply(threshold$ttl_qty_cs, function(x) list(mean=mean(x), sd=sd(x))), by=div_nm]
# 
# 
# library(dplyr)
# threshold[,c(4,23:33)] %>% group_by(div_nm) %>% summarise_each(funs(quantile))


# par(mfrow=c(1,2))
# hist(treering,prob=1,breaks=20)
# lines(density(treering,kernel=”gaussian”),col=2)


##################################################################
##################################################################
#qty threshold
##################################################################
##################################################################
require ("lattice")
# densityplot(~ ttl_qty_cs | pim_brnd_typ_desc, threshold, pch= 20, plot.points=FALSE)

densityplot(~ ttl_qty_cs, threshold, pch= 20, plot.points=T)

#qty plots
quantile(threshold$ttl_qty_cs, .98)
qty.less.98=subset(threshold,threshold$ttl_qty_cs<1500)

par(mfrow=c(1,2))
hist(threshold$ttl_qty_cs,prob=1,breaks=200, main = "qty impacted, all")
lines(density(threshold$ttl_qty_cs,kernel="gaussian"),col=2)

hist(qty.less.98$ttl_qty_cs,prob=1,breaks=100, main = "qty impacted, 98% low volume")
lines(density(qty.less.98$ttl_qty_cs,kernel="gaussian"),col=2)

# bottom 98% sumamry
summary(qty.less.98$ttl_qty_cs)
length(summary(qty.less.98$ttl_qty_cs))
densityplot(~ ttl_qty_cs | div_nm, qty.less.98, pch= 20, plot.points=FALSE)

#qty threshold:max of 98 percentile mean and division Q3
library(plyr)
qty.threshold=ddply(qty.less.98, .(div_nm), function(x) round(max(quantile(x$ttl_qty_cs,.75),summary(qty.less.98$ttl_qty_cs)[4]),0))
colnames(qty.threshold)=c("div_nm","qty_threshold")
# ddply(qty.less.98, .(div_nm), function(x) max(mean(x$ttl_qty_cs),153))

##################################################################
##################################################################
#cust_cnt
##################################################################
##################################################################
densityplot(~ cust_cnt, threshold, pch= 20, plot.points=T)

quantile(threshold$cust_cnt, .98)
cust.cnt.less.98=subset(threshold,threshold$cust_cnt<162)

par(mfrow=c(1,2))
hist(threshold$cust_cnt,prob=1,breaks=200, main = "cust count impacted, all")
lines(density(threshold$cust_cnt,kernel="gaussian"),col=2)

hist(cust.cnt.less.98$cust_cnt,prob=1,breaks=100, main = "cust count impacted, 98% low volume")
lines(density(cust.cnt.less.98$cust_cnt,kernel="gaussian"),col=2)


summary(cust.cnt.less.98$cust_cnt)

densityplot(~ cust_cnt | div_nm, cust.cnt.less.98, pch= 20, plot.points=FALSE)

cust.cnt.threshold=ddply(cust.cnt.less.98, .(div_nm), function(x) round(max(quantile(x$cust_cnt,.75),summary(cust.cnt.less.98$cust_cnt)[4]),0))
colnames(cust.cnt.threshold)=c("div_nm","cust_cnt_threshold")


##################################################################
##################################################################
#ttl sales
##################################################################
##################################################################
densityplot(~ ttl_sales, threshold, pch= 20, plot.points=T)

a=quantile(threshold$ttl_sales, .95)
ttl_sales.less.98=subset(threshold,threshold$ttl_sales<a)

par(mfrow=c(1,2))
hist(threshold$ttl_sales,prob=1,breaks=200, main = "cust count impacted, all")
lines(density(threshold$ttl_sales,kernel="gaussian"),col=2)

hist(ttl_sales.less.98$ttl_sales,prob=1,breaks=100, main = "cust count impacted, 95% low volume")
lines(density(ttl_sales.less.98$ttl_sales,kernel="gaussian"),col=2)


summary(ttl_sales.less.98$ttl_sales)

densityplot(~ ttl_sales | div_nm, ttl_sales.less.98, pch= 20, plot.points=FALSE)

#ttl_sales.threshold=ddply(ttl_sales.less.98, .(div_nm), function(x) quantile(x$ttl_sales))

ttl_sales.threshold=ddply(ttl_sales.less.98, .(div_nm), function(x) round(max(quantile(x$ttl_sales,.75),summary(ttl_sales.less.98$ttl_sales)[4]),0))
colnames(ttl_sales.threshold)=c("div_nm","ttl_sales_threshold")


##################################################################
##################################################################
#qty_penetration
##################################################################
##################################################################

densityplot(~ qty_penetration, threshold, pch= 20, plot.points=T)
summary(threshold$qty_penetration)
a.qty_penetration=quantile(threshold$qty_penetration, .98)


qty_penetration.less.98=subset(threshold,threshold$qty_penetration<a.qty_penetration)

densityplot(~ qty_penetration, qty_penetration.less.98, pch= 20, plot.points=T)

par(mfrow=c(1,2))
hist(threshold$qty_penetration,prob=1,breaks=200, main = "qty_penetration impacted, all")
lines(density(threshold$qty_penetration,kernel="gaussian"),col=2)

hist(qty_penetration.less.98$qty_penetration,prob=1,breaks=100, main = "qty_penetration impacted, 98% low volume")
lines(density(qty_penetration.less.98$qty_penetration,kernel="gaussian"),col=2)


summary(qty_penetration.less.98$qty_penetration)

densityplot(~ qty_penetration | div_nm, qty_penetration.less.98, pch= 20, plot.points=FALSE)

#qty_penetration.threshold=ddply(qty_penetration.less.98, .(div_nm), function(x) quantile(x$qty_penetration))
#changed to median for qty penetration
qty_penetration.threshold=ddply(qty_penetration.less.98, .(div_nm), function(x) round(max(quantile(x$qty_penetration,.75),summary(qty_penetration.less.98$qty_penetration)[3]),5))
colnames(qty_penetration.threshold)=c("div_nm","qty_penetration_threshold")


##################################################################
##################################################################
#cust_penetration
##################################################################
##################################################################

densityplot(~ cust_penetration, threshold, pch= 20, plot.points=T)
summary(threshold$cust_penetration)
a.cust_penetration=quantile(threshold$cust_penetration, .98)


cust_penetration.less.98=subset(threshold,threshold$cust_penetration<a.cust_penetration)

densityplot(~ cust_penetration, cust_penetration.less.98, pch= 20, plot.points=T)

par(mfrow=c(1,2))
hist(threshold$cust_penetration,prob=1,breaks=200, main = "cust_penetration impacted, all")
lines(density(threshold$cust_penetration,kernel="gaussian"),col=2)

hist(cust_penetration.less.98$cust_penetration,prob=1,breaks=100, main = "cust_penetration impacted, 98% low volume")
lines(density(cust_penetration.less.98$cust_penetration,kernel="gaussian"),col=2)


summary(cust_penetration.less.98$cust_penetration)

densityplot(~ cust_penetration | div_nm, cust_penetration.less.98, pch= 20, plot.points=FALSE)

#cust_penetration.threshold=ddply(cust_penetration.less.98, .(div_nm), function(x) quantile(x$cust_penetration))
#changed to median for qty penetration
cust_penetration.threshold=ddply(cust_penetration.less.98, .(div_nm), function(x) round(max(quantile(x$cust_penetration,.75),summary(cust_penetration.less.98$cust_penetration)[3]),5))
colnames(cust_penetration.threshold)=c("div_nm","cust_penetration_threshold")



##################################################################
##################################################################
#weighted_elasticity
##################################################################
##################################################################

densityplot(~ weighted_elasticity, threshold, pch= 20, plot.points=T)
summary(threshold$weighted_elasticity)

a.weighted_elasticity=quantile(threshold$weighted_elasticity, .05,na.rm =T)


weighted_elasticity.less.98=subset(threshold,threshold$weighted_elasticity>a.weighted_elasticity)

densityplot(~ weighted_elasticity, weighted_elasticity.less.98, pch= 20, plot.points=T)

par(mfrow=c(1,2))
hist(threshold$weighted_elasticity,prob=1,breaks=200, main = "weighted_elasticity impacted, all")
lines(density(threshold$weighted_elasticity,kernel="gaussian"),col=2)

hist(weighted_elasticity.less.98$weighted_elasticity,prob=1,breaks=100, main = "weighted_elasticity impacted, 98% low volume")
lines(density(weighted_elasticity.less.98$weighted_elasticity,kernel="gaussian"),col=2)


summary(weighted_elasticity.less.98$weighted_elasticity)

densityplot(~ weighted_elasticity | div_nm, weighted_elasticity.less.98, pch= 20, plot.points=FALSE)

#weighted_elasticity.threshold=ddply(weighted_elasticity.less.98, .(div_nm), function(x) quantile(x$weighted_elasticity))
#elasticity -1 or division 25 percentile (75 fr absolute elasticity)
weighted_elasticity.threshold=ddply(weighted_elasticity.less.98, .(div_nm), function(x) round(min(quantile(x$weighted_elasticity,.25),-1),2))
colnames(weighted_elasticity.threshold)=c("div_nm","weighted_elasticity_threshold")













thresholds.by.market=cbind(qty.threshold,cust.cnt.threshold[,2],ttl_sales.threshold[,2],qty_penetration.threshold[,2],cust_penetration.threshold[,2])
colnames(thresholds.by.market)=c("div_nm","qty.threshold","cust.cnt.threshold","ttl_sales.threshold","qty_penetration.threshold","cust_penetration.threshold")
thresholds.by.market <- join( as.data.frame(thresholds.by.market),as.data.frame(weighted_elasticity.threshold), by = "div_nm")
