##Set working directory
setwd("C:/Users/elwel/OneDrive/Desktop/Dung-Beetle-Kansas-Montana/")

library(lme4)

db <- read.csv("RawData/bison_cattle_comp_KS.csv")
head(db)
db$doy <- as.numeric(db$doy)
am <- lmer(abund ~ trt + poly(doy,2) + (1|rep:sub),data=db)
rm <- lmer(richness ~ trt + poly(doy,2) + (1|rep:sub),data=db)
# extract coefficients
coefs <- data.frame(coef(summary(rm)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

boxplot(db$abund~db$trt)
boxplot(db$richness~db$trt)

plot(db$abund ~ db$doy)
plot(db$rich ~ db$doy)

#calculate estimates for mo and trt combos

######abundance
db$trt_mo <- paste(db$trt,db$doy)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(abund ~ 1 + (1|rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="abund_est"
colnames(ests)[3] ="abund_SE"
ab_ests <- ests
head(ab_ests)

######richness
db$trt_mo <- paste(db$trt,db$doy)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(richness ~ 1 + (1|rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="rich_est"
colnames(ests)[3] ="rich_SE"
rich_ests <- ests
head(rich_ests)
ests_s <- merge(ab_ests,rich_ests,by=c("trt_mo"))

library(stringr)
ests_s[c('trt', 'doy')] <- str_split_fixed(ests$trt_mo, ' ', 2)

write.csv(ests_s, "Outputs/trtmo_ests_KS.csv")

