############## Crab Maturity Analysis Part C: SM50 calculation ###########################
# A program to separate Mature from Immature crabs based on allometric growth of body parts.  
# Code logic is based on Somerton (1980) Program MATURE for allometry types 2-4.
# Which separates two overlapping groups of points  by minimizing RSS of two regression lines
# Developed by Brad Stevens, Univ. of MD Eastern Shore
#
# Part C: This Module estimates SM50 using logistic regression
#    Analysis is run on raw data vs maturity within 10 mm groups.
# Run Parts A and B before running this module
# No input is required, so this module can be run as a single block.
#
###############  Section C1: Calculate SM50 #################

library(dplyr)  

# First, combine data into new data frame and sort by length
xp<-exp(lx)
sm <-tindex-1  # converts groups [1,2] to maturity [0,1]
xym <- data.frame(cbind(xvar, yvar, sm))   # combine into new data frame
xym <- arrange(xym, xvar)   # sort data frame

# divide Length into intervals
minx <- 10*floor(min(xvar)/10)
maxx <- 10*ceiling(max(xvar)/10)
diffx <- maxx-minx; units=diffx/10 

interval <- diffx/units; halfint <- interval/2
len.grp <- cut(xym$xvar, seq(minx,maxx,interval))
len.mids <- seq(minx+halfint, maxx-halfint, interval)

## Logistic regression Function  ####
SM50 <- function (data, indices) { 
  dset<-data[indices,]
  glm.mat <- glm(dset[,3] ~ dset[,1], family=binomial(link=logit), data=dset)
  B.0<-coef(glm.mat)[1]; B.1<-coef(glm.mat)[2] 
  sm50 <-((-1)*B.0)/B.1  	# Calculate SM50
  return(sm50)
}

## Calculate SM50 ####
# NOTE:  This may generate errors if there is no overlap of points
smat <- SM50(xym);smat

# Boot strap the SD
library("boot")
boot.1 <- boot(data=xym, statistic=SM50, R=1000)
boot.1; plot(boot.1)

SM50 <- boot.1$t0 ; SM50 # original value
boot.m <- mean(boot.1$t) ;boot.m # mean of simulations
bias <- boot.m - SM50 ;bias# bias 
boot.se <- sd(boot.1$t);boot.se
boot.B <- 1.96*boot.se; boot.B
boot.lo <- SM50 - boot.B; boot.lo
boot.hi <- SM50 + boot.B; boot.hi

### bind and print
Tab1 <- rbind( Original = SM50, mean = boot.m, Bound = boot.B, Lower = boot.lo, Upper = boot.hi)
Tab1.name <- "Bootstrap estimate of Maturity"
Tab1.name; signif(Tab1,4)

############# Section C2: Plot fitted curve  ###########################
LogMat <- glm(sm ~ xvar, family=binomial(link=logit), data=xym)
lab.pmat <- "Proportion mature"
par(mar=c(6,5,4,3))
plot(fitted(LogMat)~xym$xvar, 
     xlim=c(minx,maxx), ylim=c(0,1), 
     xlab=lab.x, ylab=lab.pmat, 
     cex.axis=1.2, cex.lab = 1.5,
     pch="", main=lab.sex)  
lines(fitted(LogMat)~xym$xvar, col="blue",lty=1)
# NOTE: this only works with sorted data. otherwise it is chaos

# create table of proportions at each interval and plot
tab.mat <- ftable(len.grp, xym$sm);tab.mat
mat.freq <- prop.table(tab.mat,1) ; mat.freq
points(mat.freq[,2] ~ len.mids, pch=16, cex=1.5)

rangex <- maxx-minx
xpos4 <- minx+rangex*0.2
# add some text and labels; location is centered
text4 <- paste("SM50 = ",signif(SM50,4))
  text(xpos4, 0.8, text4)
text5 <- paste("C.I. = ",signif(boot.B,3))
  text(xpos4, 0.7, text5)
abline(h= 0.5)  # draws horizontal line
abline(v= smat)  # draws vertical line

# Export maturity plot
dev.copy(png,'Crab Maturity Plot 4 SM50.png', width=600,height=500);dev.off()

# End of Module 3
