#### Crab Maturity Analysis Type A ####
# A program to separate Mature from Immature crabs based on allometric growth of body parts.  
# Code logic is based on Somerton (1980) Program MATURE for allometry type 1.
# Which iteratively seeks a common endpoint for two regression lines.  
# Developed by Brad Stevens, Univ. of MD Eastern Shore

############### Section 1: Read Data #####################
#install.packages("dplyr")
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

# find highest lo point
rm(min.x, min.y)
# Note - above line gives errors on first run. Ignore.

left.x <- xraw <= lo.lim  # T/F vector
lo.ndx <- sum(left.x)  # largest group 1 point
right.x <- xraw >= hi.lim  # T/F vector
hi.ndx <- (N- sum(right.x))+1   # smallest group 2 point
min.x <- xraw[lo.ndx]  # lowest T value
min.y <- yraw[lo.ndx]  # lowest T value
abline(v=lo);abline(v=hi) # Range of unknowns

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

###### Section 4: Calculate two regression lines ######
# Occurs in a loop until no further changes occur

while (min.x < hi) {
  run = run + 1
  
  # Left regression 
  lm.1 <- lm( I(yvar[memb==1]-min.y) ~ 0 + I(xvar[memb==1]-min.x))
  b1 <- coef(lm.1)[[1]]
  a1 <- min.y - (b1*min.x)
  df.1 <- anova(lm.1)[[1]]  
  ss.1 <- anova(lm.1)[[2]]
  rss.1 <- ss.1[2]
  ms.1 <- anova(lm.1)[[3]]

  # Right regression 
  lm.2 <- lm( I(yvar[memb==2]-min.y) ~ 0 + I(xvar[memb==2]-min.x))
  b2 <- coef(lm.2)[[1]]
  a2 <- min.y - (b2*min.x)
  df.2 <- anova(lm.2)[[1]]  
  ss.2 <- anova(lm.2)[[2]]
  rss.2 <- ss.2[2]
  ms.2 <- anova(lm.2)[[3]]

# plot initial lines
if (run==1) {
  plot(yvar ~ xvar, pch=memb,  main=lab.sex, ylab = lab.y, xlab=lab.x)
#  points(min.x, min.y, pch="+", col='red', cex = 2)  
  abline(a = a1, b=b1, lty=1)
  abline(a = a2, b=b2, lty=2)
  dev.copy(png,'Maturity Plot A1 Initial.png', width=600,height=500);dev.off()
  }  # End if
  
# calculate combined RSS and F
  rss.pool <- rss.1 +rss.2   # add residual sum of    
  ms.diff <- (rss.0 - rss.pool) / 2
  ms.pool <- rss.pool/(n.0-4)
  F.two <- ms.diff/ms.pool  
  F.two.p <- 1-pf(F.two, df1 = 2, df = n.0-4, lower.tail = F)
#  F.two; F.two.p
  
if ( run==1 | (rss.pool < rss.min)) {       # Run 1 OR pooled RSS
      rss.min <- rss.pool       
      joint.x <-  min.x
      joint.y <-  min.y
      a1.1<-a1; a2.1<-a2; b1.1<-b1; b2.1<-b2  # reset old values
   }    # end if   

# next point
  lo.ndx <- lo.ndx + 1
  min.x <- xvar[lo.ndx]
  min.y <- yvar[lo.ndx]
  memb.lo <- xvar <= min.x   # T/F list if less than low range
  memb.hi <- xvar > min.x   # T/F list if GT than high range
  memb[memb.lo] <- 1    # assign 1 to those < low
  memb[memb.hi] <- 2    # assign 2 to those > high
      
}# end loop

SM50 <- joint.x

memb.lo <- xvar <= joint.x   # T/F list if less than low range
memb.hi <- xvar > joint.x   # T/F list if GT than high range
memb[memb.lo] <- 1    # assign 1 to those < low
memb[memb.hi] <- 2    # assign 2 to those > high
datfile$group <- memb    
memb.sum2 <- summary(as.factor(datfile$group))
n.tot <- sum(memb.sum2)
memb.sum2; n.tot

### Plot final results ####

plot(yvar ~ xvar, pch=datfile$group,  main=lab.sex, ylab = lab.y, xlab=lab.x)#, type="n")
points(joint.x, joint.y, pch="+", col='red', cex = 2)  
abline(a = a1.1, b=b1.1, lty=1)
abline(a = a2.1, b=b2.1, lty=2)
abline(v=SM50)
text1 <- paste("Runs  = ", run,"   SM50 = ",signif(SM50,4),sep="")
text2 <- paste("RSS (1 line) = " , signif(rss.0, 4), sep="")
text3 <- paste("RSS (2 line) = " , signif(rss.min, 4), sep="")
text4 <- paste("F = " , signif(F.two,3),"    P = " , signif(F.two.p,4), sep="")
xrange <- max(xvar)-min(xvar)
xpos <- min(xvar) + xrange/4; ypos <- max(yvar)*.95
text(xpos, ypos, text1)
text(xpos, ypos*.95, text2)
text(xpos, ypos*.90, text3)
text(xpos, ypos*.85, text4)
if (ansx=="T") {SM50.t = 10^SM50
  text4 <- paste("raw SM50 = ",signif(SM50.t,3))
  text(xpos, ypos*.80, text4)}

dev.copy(png,'Maturity Plot A2 final.png', width=600,height=500);dev.off()

### Plot Differential results ####
pred.0 <-predict(lm.0,newdata = data.frame(datfile$xvar))
pred.1 <- a1.1 + xvar*b1.1
pred.2 <- a2.1 + xvar*b2.1
diff.1 <- pred.1-pred.0
diff.2 <- pred.2-pred.0
lab.ydif <- "One-line Residuals"

plot(resid(lm.0) ~ xvar, pch=datfile$group, ylab = lab.ydif, xlab=lab.x)#, type="n")
abline(0,0, lty=1)
n.imm <- sum(memb==1)  #index of largest immature

segments(x0 = xvar[1], y0 = diff.1[1],lty=1, col='red',
         x1 = xvar[n.imm], y1 = diff.1[n.imm])
segments(x0 = xvar[n.imm+1], y0 = diff.2[n.imm+1],lty=1, col='red',
         x1 = xvar[n.tot], y1 = diff.2[n.tot])
abline(v=SM50)

text2.1 <- paste("SM50 = ",signif(SM50,4),sep="")
ypos <- max(resid(lm.0))*.95
text(xpos, ypos, text2.1)

if (ansx=="T") {SM50.t = 10^SM50
text2.2 <- paste("raw SM50 = ",signif(SM50.t,3))
text(xpos, ypos*0.8, text2.2)}

dev.copy(png,'Maturity Plot A2 residual.png', width=600,height=500);dev.off()


##### Iterative Piece-wise Regression #####
## Segmented regression brute-force #### tests each observed X point
mse <- numeric(length(xvar))
for(i in 1:n.tot){
  piecewise1 <- lm(yvar ~ xvar*(xvar < xvar[i]) + xvar*(xvar>=xvar[i]), data = datfile)
  mse[i] <- summary(piecewise1)[6]
}
mse <- as.numeric(mse)

### find breakpoint (bp) that gives lowest mse
bp.ind <- which(mse==min(mse))
bp <- xvar[bp.ind] # this is not nec where the lines cross

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

jx <- (a.lo-a.hi)/(b.hi-b.lo) #the point where 2 lines meet
jy <-a.lo + b.lo *jx

####  Reassign group membership ####
memb.pw <- rep(1,n.tot)
memb.pw[xvar>= bp] <-2

#### Plot results ####
plot(yvar ~ xvar, pch=memb.pw, ylab = lab.y, xlab=lab.x)#, type="n")
abline(a = a.lo,b =b.lo)
abline(a = a.hi,b =b.hi, lty=2)
abline(v=bp, lty=1)
abline(v=jx, lty=2)

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
#pred.0 <-predict(lm.0,newdata = data.frame(datfile$xvar)) # if not done
pred.1p <- a.lo + xvar*b.lo
pred.2p <- a.hi + xvar*b.hi
diff.1p <- pred.1p-pred.0
diff.2p <- pred.2p-pred.0
#lab.ydif <- "One-line Residuals"

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
abline(v=jx, lty=2)

text2.1 <- paste("SM50 = ",signif(bp,4),sep="")
ypos <- max(resid(lm.0))*.95
text(xpos, ypos, text2.1)

if (ansx=="T") {SM50.pw = 10^bp
text2.2 <- paste("raw SM50 = ",signif(10^bp,3))
text(xpos, ypos*0.8, text2.2)}

dev.copy(png,'Maturity Plot A2 piecewise residual.png', width=600,height=500);dev.off()

# End of Code
