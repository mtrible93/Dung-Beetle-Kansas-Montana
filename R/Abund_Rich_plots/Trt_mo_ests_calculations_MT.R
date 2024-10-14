##Set working directory
setwd("C:/Users/elwel/OneDrive/Desktop/Dung-Beetle-Kansas-Montana/")

library(lme4)

db <- read.csv("RawData/full_communities/bison_cattle_comp_MT.csv")
head(db)
db$doy <- as.numeric(db$doy)
am <- lmer(abund ~ trt + poly(doy,2) + (1|rep:sub),data=db)
# extract coefficients
coefs <- data.frame(coef(summary(am)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
rm <- lmer(richness ~ trt + poly(doy,2) + (1|rep:sub),data=db)
# extract coefficients
coefs <- data.frame(coef(summary(rm)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

boxplot(db$abund~db$trt)
boxplot(db$richness~db$trt)
boxplot(db$dwell_abund~db$trt)
boxplot(db$dwell_rich~db$trt)
boxplot(db$roll_abund~db$trt)
boxplot(db$roll_rich~db$trt)
boxplot(db$tunn_abund~db$trt)
boxplot(db$tunn_rich~db$trt)
boxplot(db$native_abund~db$trt)
boxplot(db$native_rich~db$trt)
boxplot(db$alien_abund~db$trt)
boxplot(db$alien_rich~db$trt)

plot(db$abund ~ db$doy)
plot(db$rich ~ db$doy)

#calculate estimates for mo and trt combos

######abundance
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(abund ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="abund_est"
colnames(ests)[3] ="abund_SE"
ab_ests <- ests
head(ab_ests)

######richness
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(richness ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="rich_est"
colnames(ests)[3] ="rich_SE"
rich_ests <- ests
head(rich_ests)
ests_s <- merge(ab_ests,rich_ests,by=c("trt_mo"))

######dweller abundance
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(dwell_abund ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="dwell_ab_est"
colnames(ests)[3] ="dwell_ab_SE"
dwell_ab_ests <- ests
ests_s <- merge(ests_s,dwell_ab_ests,by=c("trt_mo"))

######roller abundance
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(roll_abund ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="roll_ab_est"
colnames(ests)[3] ="roll_ab_SE"
roll_ab_ests <- ests
ests_s <- merge(ests_s,roll_ab_ests,by=c("trt_mo"))

######tunneller abundance
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(tunn_abund ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="tunn_ab_est"
colnames(ests)[3] ="tunn_ab_SE"
tunn_ab_ests <- ests
ests_s <- merge(ests_s,tunn_ab_ests,by=c("trt_mo"))

######native abundance
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(native_abund ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="native_ab_est"
colnames(ests)[3] ="native_ab_SE"
native_ab_ests <- ests
ests_s <- merge(ests_s,native_ab_ests,by=c("trt_mo"))

######alien abundance
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(alien_abund ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="alien_ab_est"
colnames(ests)[3] ="alien_ab_SE"
alien_ab_ests <- ests
ests_s <- merge(ests_s,alien_ab_ests,by=c("trt_mo"))

######dweller richness
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(dwell_rich ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="dwell_rich_est"
colnames(ests)[3] ="dwell_rich_SE"
dwell_rich_ests <- ests
ests_s <- merge(ests_s,dwell_rich_ests,by=c("trt_mo"))

######roller richness
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(roll_rich ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="roll_rich_est"
colnames(ests)[3] ="roll_rich_SE"
roll_rich_ests <- ests
ests_s <- merge(ests_s,roll_rich_ests,by=c("trt_mo"))

######tunneller richness
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(tunn_rich ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="tunn_rich_est"
colnames(ests)[3] ="tunn_rich_SE"
tunn_rich_ests <- ests
ests_s <- merge(ests_s,tunn_rich_ests,by=c("trt_mo"))

######native richness
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(native_rich ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="native_rich_est"
colnames(ests)[3] ="native_rich_SE"
native_rich_ests <- ests
ests_s <- merge(ests_s,native_rich_ests,by=c("trt_mo"))

######alien richness
db$trt_mo <- paste(db$trt,db$month.picked.up)
ests <- NULL
for(i in unique(db$trt_mo)){
  subs <- db[db$trt_mo == i, ]
  ests.i <- coef(summary(lmer(alien_rich ~ 1 + (1|sub:rep), data = subs, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, subs)
} ; rm(i)
ests
colnames(ests)[2] ="alien_rich_est"
colnames(ests)[3] ="alien_rich_SE"
alien_rich_ests <- ests
ests_s <- merge(ests_s,alien_rich_ests,by=c("trt_mo"))

library(stringr)
ests_s[c('trt', 'month')] <- str_split_fixed(ests$trt_mo, ' ', 2)

write.csv(ests_s, "Outputs/trtmo_ests_MT.csv")

