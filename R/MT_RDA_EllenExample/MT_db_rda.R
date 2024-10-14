##Set working directory
##e.g.:
setwd("C:/Users/elwel/OneDrive/Desktop/Dung-Beetle-Kansas-Montana/")

#####################################
#Load libraries
#####################################
#install.packages("vegan")
library(vegan)

#####################################
#Attach data MT
#####################################
##attach community data
com<-read.csv(file="RawData/transect_summary/db_mt_comp.csv", header = TRUE, sep = ",", row.names = c(1))
attach(com)
head(com)

##attach habitat data
factors<-read.csv(file="RawData/transect_summary/db_mt_factors.csv", header = TRUE, sep = ",", row.names = c(1))
factors$trt <- as.factor(factors$trt)
attach(factors)
head(factors)

#####################################
#run RDA
#####################################
#Full full RDA
base_rda<-rda(com~ trt+ doy,factors, scale=TRUE)
summary(base_rda)

#####################################
# Look at the variance inflation factors, values > 10 indicate there is redundancy in the model and need to remove factors
#####################################
vif.cca(base_rda) 

#####################################
# Produce a triplot of the ordination (quick check)
#####################################
#ordiplot(base_rda, type="t")
#ordihull(base_rda, trt, scaling = 2, label = TRUE)

#####################################
# Perform an overall permutive ANOVA of the RDA
#####################################
# Note that the order of variables in the model makes a difference here
anova(base_rda)
# Test the significance of each variable in the model
anova(base_rda, by = "terms", perm = 1000) 
# Test the significance of each axis
anova(base_rda, by = "axis", perm = 1000)

#####################################
#make a nicer plot

tiff(filename = "plots/DB_RDAs.tiff", width = 10, height = 5, units = 'in', res = 600, compression = 'lzw')

par(mfrow=c(1,2),mar=c(4,4.4,0.7,0.2))

levels(factors$trt)
colvec <- c("sienna","gray0")
shpvec <- c(21,22)

#####################################
fig <- ordiplot(base_rda, type="none", scaling=3, xlab="RDA axis 1", ylab="RDA axis 2",xlim=c(-2.7,1.5),cex.lab=1.4)
ordihull(base_rda, trt, scaling = 3, label = F, lwd=2,col="grey80")
points(fig, "sites", pch=shpvec[trt], col=colvec[trt], bg=colvec[trt], cex=1)
#text(base_rda, dis="cn",cex=1.3)
text(fig, "species", cex=0.7, col="red4")
legend("bottomleft",legend=c("Bison","Cattle"), bty="n", cex=2, pch=c(21,22), col=c("sienna","gray0"), pt.bg=c("sienna","gray0"))
#
##
legend("topright",legend=c("A) MT"), bty="n", cex=1.5)

#####################################
#Attach data KS
#####################################
##attach community data
com<-read.csv(file="RawData/transect_summary/db_ks_comp.csv", header = TRUE, sep = ",", row.names = c(1))
attach(com)
head(com)

##attach habitat data
factors<-read.csv(file="RawData/transect_summary/db_ks_factors.csv", header = TRUE, sep = ",", row.names = c(1))
factors$trt <- as.factor(factors$trt)
attach(factors)
head(factors)

#####################################
#run RDA
#####################################
#Full full RDA
base_rda<-rda(com~ trt+ DOY,factors, scale=TRUE)
summary(base_rda)

#####################################
# Look at the variance inflation factors, values > 10 indicate there is redundancy in the model and need to remove factors
#####################################
vif.cca(base_rda) 

#####################################
# Produce a triplot of the ordination (quick check)
#####################################
#ordiplot(base_rda, type="t")
#ordihull(base_rda, trt, scaling = 2, label = TRUE)

#####################################
# Perform an overall permutive ANOVA of the RDA
#####################################
# Note that the order of variables in the model makes a difference here
anova(base_rda)
# Test the significance of each variable in the model
anova(base_rda, by = "terms", perm = 1000) 
# Test the significance of each axis
anova(base_rda, by = "axis", perm = 1000)

#####################################
#make a nicer plot

levels(factors$trt)
colvec <- c("sienna","gray0")
shpvec <- c(21,22)

#####################################
fig <- ordiplot(base_rda, type="none", scaling=3, xlab="RDA axis 1", ylab="RDA axis 2",xlim=c(-2.2,0.6),cex.lab=1.4)
ordihull(base_rda, trt, scaling = 3, label = F, lwd=2,col="grey80")
points(fig, "sites", pch=shpvec[trt], col=colvec[trt], bg=colvec[trt], cex=1)
#text(base_rda, dis="cn",cex=1.3)
text(fig, "species", cex=0.7, col="red4")
#legend("bottomleft",legend=c("Bison","Cattle"), bty="n", cex=2, pch=c(21,22), col=c("sienna","gray0"), pt.bg=c("sienna","gray0"))
##
legend("topright",legend=c("B) KS"), bty="n", cex=1.5)
#

dev.off()
######################################################
##########
