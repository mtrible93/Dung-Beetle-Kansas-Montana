##Set working directory
setwd("C:/Users/elwel/OneDrive/Desktop/DBcomp/")

ests <- read.csv("MT_db/trtmo_ests.csv")
head(ests)
ests$mo_jit <- (ests$month + c(-0.08,-0.08,-0.08,0.08,0.08,0.08))

tiff(filename = "plots/Total_abund_rich.tiff", width = 8, height = 4, units = 'in', res = 600, compression = 'lzw')

par(mfrow=c(1,2),mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.7,8.3), ylim=c(25,230),las=1,ylab="",xlab="",xaxt="n", log="y")
marks <- c("June","July","Aug")
axis(1, at=c(6,7,8),cex.axis=1.5,labels=marks)
box(lwd=2)
title(ylab="Dung beetle abundance/site", line=3, cex.lab=1.3)
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
title(ylab="Dung beetle richness/site", line=3, cex.lab=1.3)
##bison
points(ests$rich_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(ests$rich_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],col="sienna",type="l",lwd=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$rich_est[ests$trt=="bison"]-ests$rich_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$rich_est[ests$trt=="bison"]+ests$rich_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(ests$rich_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=21,col="gray0",bg="gray0",cex=2)
points(ests$rich_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],col="gray0",type="l",lwd=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$rich_est[ests$trt=="cattle"]-ests$rich_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$rich_est[ests$trt=="cattle"]+ests$rich_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

legend("topright",legend=c("Bison","Cattle"), bty="n", pt.cex=2,cex=1.3, pch=c(21), pt.bg=c("sienna","gray0"),col=c("sienna","gray0"))

dev.off()
######################################################
##func groups
tiff(filename = "plots/FuncGroup_abund_rich.tiff", width = 10, height = 7, units = 'in', res = 600, compression = 'lzw')

par(mfrow=c(2,2),mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.4,8.6), ylim=c(0.04,160),las=1,ylab="",xlab="",xaxt="n")#, log="y")
marks <- c("June","July","Aug")
axis(1, at=c(6,7,8),cex.axis=1.5,labels=marks)
box(lwd=2)
title(ylab="Dung beetle abundance", line=3, cex.lab=1.6)
##bison
ests$mo_jit <- (ests$month -0.25)
points(ests$dwell_ab_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=24,col="sienna",bg="sienna",cex=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$dwell_ab_est[ests$trt=="bison"]-ests$dwell_ab_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$dwell_ab_est[ests$trt=="bison"]+ests$dwell_ab_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month -0.05)
points(ests$roll_ab_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$roll_ab_est[ests$trt=="bison"]-ests$roll_ab_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$roll_ab_est[ests$trt=="bison"]+ests$roll_ab_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month +0.15)
points(ests$tunn_ab_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=25,col="sienna",bg="sienna",cex=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$tunn_ab_est[ests$trt=="bison"]-ests$tunn_ab_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$tunn_ab_est[ests$trt=="bison"]+ests$tunn_ab_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
ests$mo_jit <- (ests$month -0.15)
points(ests$dwell_ab_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=24,col="gray0",bg="gray0",cex=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$dwell_ab_est[ests$trt=="cattle"]-ests$dwell_ab_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$dwell_ab_est[ests$trt=="cattle"]+ests$dwell_ab_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month +0.05)
points(ests$roll_ab_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=21,col="gray0",bg="gray0",cex=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$roll_ab_est[ests$trt=="cattle"]-ests$roll_ab_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$roll_ab_est[ests$trt=="cattle"]+ests$roll_ab_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month +0.25)
points(ests$tunn_ab_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=25,col="gray0",bg="gray0",cex=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$tunn_ab_est[ests$trt=="cattle"]-ests$tunn_ab_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$tunn_ab_est[ests$trt=="cattle"]+ests$tunn_ab_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

legend("topleft",legend=c("Bison","Cattle"), bty="n", pt.cex=2,cex=1.3, pch=22, pt.bg=c("sienna","gray0"),col=c("sienna","gray0"))

################

plot(1, type="n", xlim=c(5.4,8.6), ylim=c(0,3.2),las=1,ylab="",xlab="",xaxt="n")
marks <- c("June","July","Aug")
axis(1, at=c(6,7,8),cex.axis=1.5,labels=marks)
box(lwd=2)
title(ylab="Dung beetle richness", line=3, cex.lab=1.6)
##bison
ests$mo_jit <- (ests$month -0.25)
points(ests$dwell_rich_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=24,col="sienna",bg="sienna",cex=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$dwell_rich_est[ests$trt=="bison"]-ests$dwell_rich_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$dwell_rich_est[ests$trt=="bison"]+ests$dwell_rich_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month -0.05)
points(ests$roll_rich_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$roll_rich_est[ests$trt=="bison"]-ests$roll_rich_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$roll_rich_est[ests$trt=="bison"]+ests$roll_rich_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month +0.15)
points(ests$tunn_rich_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=25,col="sienna",bg="sienna",cex=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$tunn_rich_est[ests$trt=="bison"]-ests$tunn_rich_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$tunn_rich_est[ests$trt=="bison"]+ests$tunn_rich_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
ests$mo_jit <- (ests$month -0.15)
points(ests$dwell_rich_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=24,col="gray0",bg="gray0",cex=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$dwell_rich_est[ests$trt=="cattle"]-ests$dwell_rich_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$dwell_rich_est[ests$trt=="cattle"]+ests$dwell_rich_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month +0.05)
points(ests$roll_rich_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=21,col="gray0",bg="gray0",cex=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$roll_rich_est[ests$trt=="cattle"]-ests$roll_rich_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$roll_rich_est[ests$trt=="cattle"]+ests$roll_rich_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month +0.25)
points(ests$tunn_rich_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=25,col="gray0",bg="gray0",cex=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$tunn_rich_est[ests$trt=="cattle"]-ests$tunn_rich_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$tunn_rich_est[ests$trt=="cattle"]+ests$tunn_rich_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

legend("topright",legend=c("Dweller","Roller","Tunneller"), bty="n", pt.cex=2,cex=1.3, pch=c(24,21,25), pt.bg=c("gray60"),col=c("gray60"))

##

plot(1, type="n", xlim=c(5.4,8.6), ylim=c(2,160),las=1,ylab="",xlab="",xaxt="n")#, log="y")
marks <- c("June","July","Aug")
axis(1, at=c(6,7,8),cex.axis=1.5,labels=marks)
box(lwd=2)
title(ylab="Dung beetle abundance", line=3, cex.lab=1.6)
##bison
ests$mo_jit <- (ests$month -0.15)
points(ests$native_ab_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=22,col="sienna",bg="sienna",cex=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$native_ab_est[ests$trt=="bison"]-ests$native_ab_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$native_ab_est[ests$trt=="bison"]+ests$native_ab_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month +0.05)
points(ests$alien_ab_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=23,col="sienna",bg="sienna",cex=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$alien_ab_est[ests$trt=="bison"]-ests$alien_ab_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$alien_ab_est[ests$trt=="bison"]+ests$alien_ab_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
ests$mo_jit <- (ests$month -0.05)
points(ests$native_ab_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$native_ab_est[ests$trt=="cattle"]-ests$native_ab_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$native_ab_est[ests$trt=="cattle"]+ests$native_ab_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month +0.15)
points(ests$alien_ab_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=23,col="gray0",bg="gray0",cex=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$alien_ab_est[ests$trt=="cattle"]-ests$alien_ab_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$alien_ab_est[ests$trt=="cattle"]+ests$alien_ab_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

######################
plot(1, type="n", xlim=c(5.4,8.6), ylim=c(0.3,3.5),las=1,ylab="",xlab="",xaxt="n")
marks <- c("June","July","Aug")
axis(1, at=c(6,7,8),cex.axis=1.5,labels=marks)
box(lwd=2)
title(ylab="Dung beetle richness", line=3, cex.lab=1.6)
##bison
ests$mo_jit <- (ests$month -0.15)
points(ests$native_rich_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=22,col="sienna",bg="sienna",cex=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$native_rich_est[ests$trt=="bison"]-ests$native_rich_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$native_rich_est[ests$trt=="bison"]+ests$native_rich_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month +0.05)
points(ests$alien_rich_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=23,col="sienna",bg="sienna",cex=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$alien_rich_est[ests$trt=="bison"]-ests$alien_rich_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$alien_rich_est[ests$trt=="bison"]+ests$alien_rich_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
ests$mo_jit <- (ests$month -0.05)
points(ests$native_rich_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$native_rich_est[ests$trt=="cattle"]-ests$native_rich_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$native_rich_est[ests$trt=="cattle"]+ests$native_rich_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
ests$mo_jit <- (ests$month +0.15)
points(ests$alien_rich_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=23,col="gray0",bg="gray0",cex=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$alien_rich_est[ests$trt=="cattle"]-ests$alien_rich_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$alien_rich_est[ests$trt=="cattle"]+ests$alien_rich_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

legend("topright",legend=c("Native","Non-native"), bty="n", pt.cex=2,cex=1.3, pch=c(22,23), pt.bg=c("gray60"),col=c("gray60"))

dev.off()
##########