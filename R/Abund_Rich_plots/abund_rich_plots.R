##Set working directory
setwd("C:/Users/elwel/OneDrive/Desktop/Dung-Beetle-Kansas-Montana/")

ests <- read.csv("Outputs/trtmo_ests_MT.csv")
head(ests)
ests$mo_jit <- (ests$month + c(-0.08,-0.08,-0.08,0.08,0.08,0.08))

tiff(filename = "plots/Total_abund_rich.tiff", width = 10, height = 7, units = 'in', res = 600, compression = 'lzw')

par(mfrow=c(2,2),mar=c(4,5.5,0.2,0.5))
plot(1, type="n", xlim=c(5.7,8.3), ylim=c(25,230),las=1,ylab="",xlab="",xaxt="n", log="y")
marks <- c("June","July","Aug")
axis(1, at=c(6,7,8),cex.axis=1.5,labels=marks)
box(lwd=2)
title(ylab="Dung beetle abundance/ MT site", line=3.5, cex.lab=1.3)
##bison
points(ests$abund_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(ests$abund_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"], col="sienna",type="l",lwd=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$abund_est[ests$trt=="bison"]-ests$abund_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$abund_est[ests$trt=="bison"]+ests$abund_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(ests$abund_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=21,col="gray0",bg="gray0",cex=2)
points(ests$abund_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],col="gray0",type="l",lwd=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$abund_est[ests$trt=="cattle"]-ests$abund_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$abund_est[ests$trt=="cattle"]+ests$abund_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##

plot(1, type="n", xlim=c(5.7,8.3), ylim=c(1,6.5),las=1,ylab="",xlab="",xaxt="n")
marks <- c("June","July","Aug")
axis(1, at=c(6,7,8),cex.axis=1.5,labels=marks)
box(lwd=2)
title(ylab="Dung beetle richness/ MT site", line=3.5, cex.lab=1.3)
##bison
points(ests$rich_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(ests$rich_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],col="sienna",type="l",lwd=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$rich_est[ests$trt=="bison"]-ests$rich_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$rich_est[ests$trt=="bison"]+ests$rich_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(ests$rich_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=21,col="gray0",bg="gray0",cex=2)
points(ests$rich_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],col="gray0",type="l",lwd=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$rich_est[ests$trt=="cattle"]-ests$rich_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$rich_est[ests$trt=="cattle"]+ests$rich_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

legend("topright",legend=c("Bison","Cattle"), bty="n", pt.cex=2,cex=1.3, pch=c(21), pt.bg=c("sienna","gray0"),col=c("sienna","gray0"))

##
ests <- read.csv("Outputs/trtmo_ests_KS.csv")
head(ests)
max(ests$abund_est)
options(scipen = 999)

plot(1, type="n", xlim=c(110,345), ylim=c(0.1,3200),las=1,ylab="",xlab="",log="y")
box(lwd=2)
title(ylab="Dung beetle abundance/ KS site", line=3.5, cex.lab=1.3)
title(xlab="Day of year", line=2.5, cex.lab=1.3)
##bison
points(ests$abund_est[ests$trt=="bison"] ~ ests$doy[ests$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(ests$abund_est[ests$trt=="bison"] ~ ests$doy[ests$trt=="bison"], col="sienna",type="l",lwd=2)
arrows(ests$doy[ests$trt=="bison"], ests$abund_est[ests$trt=="bison"]-ests$abund_SE[ests$trt=="bison"], ests$doy[ests$trt=="bison"], ests$abund_est[ests$trt=="bison"]+ests$abund_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(ests$abund_est[ests$trt=="cattle"] ~ ests$doy[ests$trt=="cattle"],pch=21,col="gray0",bg="gray0",cex=2)
points(ests$abund_est[ests$trt=="cattle"] ~ ests$doy[ests$trt=="cattle"],col="gray0",type="l",lwd=2)
arrows(ests$doy[ests$trt=="cattle"], ests$abund_est[ests$trt=="cattle"]-ests$abund_SE[ests$trt=="cattle"], ests$doy[ests$trt=="cattle"], ests$abund_est[ests$trt=="cattle"]+ests$abund_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##

max(ests$rich_est)
plot(1, type="n", xlim=c(110,345), ylim=c(0.1,11),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Dung beetle richness/ KS site", line=3.5, cex.lab=1.3)
title(xlab="Day of year", line=2.5, cex.lab=1.3)
##bison
points(ests$rich_est[ests$trt=="bison"] ~ ests$doy[ests$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(ests$rich_est[ests$trt=="bison"] ~ ests$doy[ests$trt=="bison"],col="sienna",type="l",lwd=2)
arrows(ests$doy[ests$trt=="bison"], ests$rich_est[ests$trt=="bison"]-ests$rich_SE[ests$trt=="bison"], ests$doy[ests$trt=="bison"], ests$rich_est[ests$trt=="bison"]+ests$rich_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(ests$rich_est[ests$trt=="cattle"] ~ ests$doy[ests$trt=="cattle"],pch=21,col="gray0",bg="gray0",cex=2)
points(ests$rich_est[ests$trt=="cattle"] ~ ests$doy[ests$trt=="cattle"],col="gray0",type="l",lwd=2)
arrows(ests$doy[ests$trt=="cattle"], ests$rich_est[ests$trt=="cattle"]-ests$rich_SE[ests$trt=="cattle"], ests$doy[ests$trt=="cattle"], ests$rich_est[ests$trt=="cattle"]+ests$rich_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

dev.off()
######################################################
##########