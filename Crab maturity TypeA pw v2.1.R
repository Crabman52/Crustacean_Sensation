#### Crab Maturity Analysis Type A ####
# A program to separate Mature from Immature crabs based on allometric growth of body parts.  
# Code logic is based on Somerton (1980) Program MATURE for allometry type 1.
# Which iteratively seeks a common endpoint for two regression lines.  
# Developed by Brad Stevens, Univ. of MD Eastern Shore

############### Section 1: Read Data #####################
library(dplyr)  

# Clear out old data if necessary
rm(list=ls(all=TRUE))

### Read Data
setwd("C:/Users/bgstevens/Documents/R/R Projects/Crab Maturity")  #laptop
data.1 <- read.csv("JonahCrab_testData.csv", header=T) #

### MAKE SEX or Species INTO A FACTOR ###
data.1$Sex<-factor(data.1$Sex); summary(data.1$Sex)
data.1$ID <- factor(data.1$ID)
colnames(data.1)

#summary(data.1)
# what sex do you want to work with? Do you use numbers (1,2) or letters (F, M)?
sex.code <- 2 #"M"

# make labels
lab.sex <- ifelse(sex.code=="1",yes="Males", no= "Females")
lab.pmat <- "Proportion mature"
txt.sig <- " ++++++++  Two lines better than one"
txt.nsig <- " xxxxxxxx  One line better than two"

##  Select data
datfile <- subset(data.1,data.1$Sex==sex.code) #Sp==sp.code & 

############### Section 2a: Answer some questions ##########

# What is the length measurement?
datfile$xraw <-datfile$Width
lab.x <- "Width"
# What is the response variable
datfile$yraw <- datfile$AbWid
lab.y <- "Abdomen width"

# if NaNs are present, run this code
datfile$yraw[is.na(datfile$yraw)]<-0 #turn y variable NAs to 0 
NoNA <- !(is.na(datfile$yraw)|(datfile$yraw==0))  # finds NA in Y variable
datfile<- datfile[NoNA,]   # removes NA from data file

datfile <- datfile[order(datfile$xraw),]  # sort by X
attach(datfile)

N <- nrow(datfile);N
summary(xraw) 
summary(yraw) 

###############  Section 2b: Choose endpoints #################
########  Select lo/hi ranges for maturity 
plot(yraw~xraw)
lo.lim <- lo <- 45
hi.lim <- hi <- 110
limits <-c(lo.lim, hi.lim)
llimits <- log10(limits)

# find highest lo point
rm(min.x, min.y)
# Note - above line gives errors on first run.

left.x <- xraw <= lo.lim  # T/F vector
lo.ndx <- sum(left.x)  # largest group 1 point
right.x <- xraw >= hi.lim  # T/F vector
hi.ndx <- (N- sum(right.x))+1   # smallest group 2 point
min.x <- xraw[lo.ndx]  # lowest T value
min.y <- yraw[lo.ndx]  # lowest T value
abline(v=lo);abline(v=hi)

datfile$xvar<-datfile$xraw
datfile$yvar<-datfile$yraw
attach(datfile);head(datfile)

# log transform X and Y?  T (true) or F (false)
ansx <- ansy <- "T"

# transform if requested
if (ansx == "T") {datfile$xvar <- log10(xvar)
lo <- log10(lo);hi <- log10(hi)
min.x <- log10(min.x) 
lab.x <- "log10 (Length)"}
if (ansy == "T") {datfile$yvar <- log10(yvar)
min.y <- log10(min.y);   
lab.y <- "log10 (Chela)"}

plot(datfile$yvar~ datfile$xvar, pch="*", ylab=lab.y, xlab=lab.x)
abline(v=lo)
abline(v=hi)

############### Section 3: Single line regression ###############
run = 0

lm.0 <- lm(yvar~xvar, data = datfile)
anova(lm.0)
df.0 <- anova(lm.0)[[1]]  # degrees of freedom
ss.0 <- anova(lm.0)[[2]]  # sum of squares
rss.0 <- ss.0[2]          # resid SS
ms.0 <- anova(lm.0)[[3]]  # mean squared error
F.0 <- ms.0[1]/ms.0[2]; F.0    # F value
n.0 <- dim(datfile)[1]
rss.min <- rss.0
mse.0 <- summary(lm.0)[[6]]

# assign group membership
# 1 = left line, 2= right line
memb <-rep(1,nrow(datfile))
memb.lo <- datfile$xvar <= min.x   # T/F list if less than low range
memb.hi <- datfile$xvar > min.x   # T/F list if GT than high range
memb[memb.lo] <- 1    # assign 1 to those < low
memb[memb.hi] <- 2    # assign 2 to those > high
memb.sum1 <- summary(as.factor(memb))
memb.sum1; sum(memb.sum1)
datfile$prior <- memb
datfile$group <- memb
detach(datfile);attach(datfile)

plot(yvar ~ xvar, pch=memb,  main=lab.sex, ylab = lab.y, xlab=lab.x)
abline(lm.0)

##### Iterative Piece-wise Regression #####
## Segmented regression brute-force #### tests each observed X point
n.loops <- length(xvar)
mse <- numeric(n.loops)

mse <- numeric(length(xvar))
for(i in 1:n.loops){
  piecewise1 <- lm(yvar ~ xvar*(xvar < xvar[i]) + xvar*(xvar>=xvar[i]), data = datfile)
  mse[i] <- summary(piecewise1)[6]
}
mse <- as.numeric(mse)

### find breakpoint (bp) that gives lowest mse
bp.ind <- which(mse==min(mse))
bp <- xvar[bp.ind]

## rerun piecewise regression at best bp
piecewise2 <- lm(yvar ~ xvar*(xvar < bp) + xvar*(xvar > bp),data = datfile)
mse.2 <- summary(piecewise2)[[6]]
df.2 <- summary(piecewise2)[[7]][2]

## F test
piece.F <- anova(lm.0,piecewise2)$F[2]
piece.P <- anova(lm.0,piecewise2)[[6]][2]


pw.vals <- coef(piecewise2)
a.lo <- pw.vals[1]+pw.vals[3]
b.lo <- pw.vals[2]+pw.vals[5]
a.hi <- pw.vals[1]+pw.vals[4]
b.hi <- pw.vals[2]

jx.1 <- (a.lo-a.hi)/(b.hi-b.lo)
jy.1 <-a.lo + b.lo *jx.1

####  Reassign group membership ####
memb.pw <- rep(1,n.loops)
memb.pw[xvar>= bp] <-2
n.tot <- dim(datfile)[1]

#### Plot results ####
plot(yvar ~ xvar, pch=memb.pw, ylab = lab.y, xlab=lab.x)#, type="n")
abline(a = a.lo,b =b.lo)
abline(a = a.hi,b =b.hi, lty=2)
abline(v=bp, lty=1)
abline(v=jx.1, lty=2)

### pretty the plot  ####
textp1 <- "Piecewise Regression"
textp2 <- paste("MSE (1 line) = " , signif(mse.0, 4), sep="")
textp3 <- paste("MSE (2 line) = " , signif(mse.2, 4), sep="")
textp4 <- paste("F = " , signif(piece.F,3),"    P = " , signif(piece.P,4), sep="")
xrange <- max(xvar)-min(xvar)
xpos <- min(xvar) + xrange/4; ypos <- max(yvar)*.95
text(xpos, ypos*1.0, textp1)
text(xpos, ypos*.95, textp2)
text(xpos, ypos*.90, textp3)
text(xpos, ypos*.85, textp4)

if (ansx=="T") {SM50.pw = 10^bp  #
text4 <- paste("raw SM50 = ",signif(SM50.pw,3))
text(xpos, ypos*.80, text4)}

dev.copy(png,'Maturity Plot A2 Piece-wise.png', 
         width=600,height=500, );dev.off()

### Plot Differential results for Piece-wise ####
pred.0 <-predict(lm.0,newdata = data.frame(datfile$xvar)) # if not done
pred.1p <- a.lo + xvar*b.lo
pred.2p <- a.hi + xvar*b.hi
diff.1p <- pred.1p-pred.0
diff.2p <- pred.2p-pred.0
lab.ydif <- "One-line Residuals"

plot(resid(lm.0) ~ xvar, pch=memb.pw, ylab = lab.ydif, xlab=lab.x)#, type="n")
abline(0,0, lty=1)
n.imm <- bp.ind  #index of largest immature
lines(x = xvar, y = diff.1p)
lines(x = xvar, y =diff.2p)

segments(x0 = xvar[1], y0 = diff.1p[1],lty=1, col='red',
         x1 = xvar[n.imm], y1 = diff.1p[n.imm])
segments(x0 = xvar[n.imm], y0 = diff.2p[n.imm],lty=1, col='red',
         x1 = xvar[n.tot], y1 = diff.2p[n.tot])
abline(v=bp)
abline(v=jx.1, lty=2)

text2.1 <- paste("SM50 = ",signif(bp,4),sep="")
ypos <- max(resid(lm.0))*.95
text(xpos, ypos, text2.1)

if (ansx=="T") {SM50.pw = 10^bp
text2.2 <- paste("raw SM50 = ",signif(10^bp,3))
text(xpos, ypos*0.8, text2.2)}

dev.copy(png,'Maturity Plot A2 piecewise residual.png', width=600,height=500);dev.off()



#### Section 3b. Segmented regression ####
## NOTE: This defines two different regression lines that may overlap.
# bp is the point at which two regression lines have the minimum MSE, but those lines may have different 
#    intercepts and slopes, i.e. they may overlap
# jx is the point at which the two lines actually intercept, i.e. the regression equations predict the same y value
#    and this point may be less than bp
# In this version, jx is selected as the SM50, where both lines meet.

## determine increment for loop
lim.rng <- llimits[2]-llimits[1];lim.rng
#inc <- lim.rng/n.loops
inc <- 0.001
start <- round(llimits[1], 2)
end <- round(llimits[2], 2)
steps <- seq(start,end,inc)
#steps <- seq(llimits[1],llimits[2],inc)
nsteps <- length(steps)

mse <- numeric(nsteps)
for(i in 1:nsteps){
  piecewise.1 <- lm(datfile$yvar ~ datfile$xvar*(datfile$xvar < steps[i]) +
                     datfile$xvar*(datfile$xvar>=steps[i]), data = datfile)
  mse[i] <- summary(piecewise.1)[6]
}
mse <- as.numeric(mse)
bp.ind <- which(mse==min(mse))
bp <- steps[bp.ind]
piecewise.2 <- lm(datfile$yvar ~ datfile$xvar*(datfile$xvar < bp) + 
                   datfile$xvar*(datfile$xvar > bp),data = datfile)
piecewise.2 <- lm(yvar ~ xvar*(xvar < bp) + xvar*(xvar > bp),data = datfile)
mse.2 <- summary(piecewise.2)[[6]]
df.2 <- summary(piecewise.2)[[7]][2]

## F test
piece.F <- anova(lm.0,piecewise.2)$F[2]
piece.P <- anova(lm.0,piecewise.2)[[6]][2]

# assemble regression coefficients
pw.vals <- coef(piecewise.2)
pw.vals[which(is.na(pw.vals))]<-0
a.lo <- pw.vals[[1]]+pw.vals[[3]]
b.lo <- pw.vals[[2]]+pw.vals[[5]]
a.hi <- pw.vals[[1]]+pw.vals[[4]]
b.hi <- pw.vals[[2]]

j.x <- (a.lo-a.hi)/(b.hi-b.lo)
j.y <-a.lo + b.lo *j.x
  
### Assign groups ####
memb.pw <-rep(1,nrow(datfile))
pw.lo <- datfile$xvar <= j.x   # T/F list if less than low range
pw.hi <- datfile$xvar > j.x   # T/F list if GT than high range
memb.pw[pw.hi] <- 2    # assign 2 to those > high
datfile$group <- memb.pw    
memb.pwsum2 <- summary(as.factor(datfile$group))
memb.pwsum2; sum(memb.pwsum2)

#### Plotting ####

plot(datfile$yvar ~ datfile$xvar, pch=datfile$group,  main=lab.sex, ylab = lab.y, xlab=lab.x)# , type="n")
# plot(datfile$yvar ~ datfile$xvar, pch=datfile$group,  main=lab.sex, ylab = lab.y, xlab=lab.x, 
#      xlim=c(1.6,1.8), ylim=c(1.0,1.5))

abline(a=a.lo, b=b.lo, v=j.x)
abline(a=a.hi, b=b.hi, lty=2)
points(j.x, j.y, pch="+", col='red', cex = 2)  
abline(v=bp, lty=2)

maxx <- max(datfile$xvar)
minx <- min(datfile$xvar)
xrange <- maxx-minx
maxy <- max(datfile$yvar)
miny <- min(datfile$yvar)
### pretty the plot  ####
textp1 <- "Piecewise Regression"
textp2 <- paste("MSE (1 line) = " , signif(mse.0, 4), sep="")
textp3 <- paste("MSE (2 line) = " , signif(mse.2, 4), sep="")
textp4 <- paste("F = " , signif(piece.F,3),"    P = " , signif(piece.P,4), sep="")
xpos <- minx + xrange/4; ypos <- maxy*.95
text(xpos, ypos*1.0, textp1)
text(xpos, ypos*.95, textp2)
text(xpos, ypos*.90, textp3)
text(xpos, ypos*.85, textp4)
if (ansx=="T") {SM50.pw = 10^bp
  text4 <- paste("raw SM50 = ",signif(SM50.pw,3))
  text(xpos, ypos*.80, text4)}

dev.copy(png,'Female Maturity Piecewise 2.1 Length.png', width=600,height=500);dev.off()


