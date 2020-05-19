############## Crab Maturity Analysis Part B: Maturity assignment ###############
# A program to separate Mature from Immature crabs based on allometric growth of body parts.  
# Code logic is based on Somerton (1980) Program MATURE for allometry types 2-4.
# Which separates two overlapping groups of points  by minimizing RSS of two regression lines
# Developed by Brad Stevens, Univ. of MD Eastern Shore
#
# Part B: This Module assigns maturity to unknown crabs
#    Data are ln-transformed prior to regression analysis and maturity assignment.
# Run Part A as source before running this module
#
# Instructions: 
# 1. Run Section B1. Stop after first plot and input lower and upper bounds for unknowns
# 2. Run Section B2. 
# 3. Run Sections B3 and B4.
#
###############  Section B1: Run single line regression #####
library(dplyr)  

# Plot raw data for setting limits
plot(yvar ~ xvar, main=lab.sex, ylab = lab.y, xlab=lab.x)

# make labels
lab.pmat <- "Proportion mature"
sig <- "  Two lines better than one"
nsig <- "  One line better than two"

# log-Transform data 
lx<-log(datfile$xvar)
ly<-log(datfile$yvar) 

### Single line regression
lm.0 <- lm(ly~lx)
anova(lm.0)
df.0 <- anova(lm.0)[[1]]  # degrees of freedom
ss.0 <- anova(lm.0)[[2]]  # sum of squares
rss.0 <- ss.0[1]          # resid SS
ms.0 <- anova(lm.0)[[3]]  # mean squared error
F.0 <- ms.0[1]/ms.0[2]    # F value
n.0 <- dim(datfile)[1]

# Save plot
plot(lx,ly,  ylab="log y",xlab="log x", main="Single Line")
abline(lm.0)
F.1 <- anova(lm.0)[4]
rsq.1<-summary(lm.0)$r.squared
text1.1 <- paste("F = " , signif(F.1[[1,1]],4), sep="")
text1.2 <- paste("R.sq = " , signif(rsq.1,4), sep="")
xpos <- min(lx) + (max(lx)-min(lx))*0.1
ypos1 <- min(ly) + (max(ly)-min(ly))*0.90
ypos2 <- min(ly) + (max(ly)-min(ly))*0.80
text(xpos, ypos1, labels = text1.1)
text(xpos, ypos2, labels = text1.2)

dev.copy(png,'Crab Maturity Plot 1 - Raw.png', width=600,height=500);dev.off()

######  Section B2: Function to Fit unknowns #####

fitunk <- function(lx,ly,tindex,index,unkn,rss.p) {
  
  # fit known juveniles
  x<-lx[tindex==1]
  y<-ly[tindex==1]
  fitlow<-lm(y~x)
  a1 <- coef(fitlow)[[1]]
  b1 <- coef(fitlow)[[2]]
  df.1 <- anova(fitlow)[[1]]  
  ss.1 <- anova(fitlow)[[2]]
  rss.1 <- ss.1[1]
  ms.1 <- anova(fitlow)[[3]]
  
  # fit known adults
  x<-lx[tindex==2]
  y<-ly[tindex==2]
  fithi <-lm(y~x)
  a2 <- coef(fithi)[[1]]
  b2 <- coef(fithi)[[2]]
  df.2 <- anova(fithi)[[1]]  
  ss.2 <- anova(fithi)[[2]]
  rss.2 <- ss.2[1]
  ms.2 <- anova(fithi)[[3]]
  rss.p <- ss.1[2] + ss.2[2]   # add residual sum of squares
  
  # assign unknowns to closest  line
  predlow<-predict(fitlow,newdata=unkn)
  ydiflow<-abs(unkn$y-predlow)
  predhigh<-predict(fithi,newdata=unkn)
  ydifhigh<-abs(unkn$y-predhigh)
  ydif<-ydiflow-ydifhigh
  ydif[ydif>0]<-2
  ydif[ydif<=0]<-1
  tindex[index==0]<-ydif
  return(list(tindex, rss.p))
}  # end fitunk 


###### Section B3: Set range limits and iterate ##########
# B3a: Set range limits and increments (lo, hi, inc)
min.vals <- seq(50,80,5) # bounds and increment for lower limit
max.vals <- seq(115,140,5) # bounds and increment for upper limit 
min.i <- length(min.vals) #number of lower bounds
max.i <- length(max.vals) #number of upper bounds
Fvals <-array(dim=c(min.i,max.i)) # set up array
rownames(Fvals)<-min.vals # name rows
colnames(Fvals)<-max.vals # name columns
Fmax <- 0
m<-n<-1
m=n=1

#### B3b: run loop to find best range limits
for (m in 1:min.i){
  minx <- log(min.vals[m]) 
  for(n in 1:max.i){
    maxx <- log(max.vals[n]) 

##### Select lower and upper bounds for unknowns  
# assign initial group membership (index) and plot
index<-rep(0,length(lx))
index[lx<minx]<-1   # known juves
index[lx>maxx]<-2   # known adults
unkn<-data.frame(lx[index==0],ly[index==0])  # unknown maturity
names(unkn)<-c("x","y")

# Run fit function and statistics  #####
# runs iteratively (up to 15 x) til no change

tindex<-index  # this index will be modified
n.i <- length(index)
njuv<-length(index[index==1])

for(i in 1:20){
  fit.u <- fitunk(lx,ly,tindex,index,unkn,rss.p)
  tindex <- unlist(fit.u)[1:n.i]
  njuvnew <- length(tindex[tindex==1])
  if(njuvnew==njuv)break
  njuv <- njuvnew
} # end function loop

rss.p <- fit.u[[2]]

# calculate combined F
ms.diff <- (ss.0[2] - rss.p) / 2
ms.pool <- rss.p/(nrow(data.1)-4)
F.com <- ms.diff/ms.pool
if(F.com>=Fmax) {
  Fmax <- F.com
  Fmax.m <- m
  Fmax.n <- n
}  # end if 
  Fvals[m,n] <- F.com
  }
}  # end of loop B3b

######## B4: find best fit #################
Fvals
Fmax
F.select <- array(NA, dim=dim(Fvals))
rmax<- NULL

# Find largest low range and smallest high range that provides Fmax
for (m in 1:min.i){
    for(n in 1:max.i){
      if (Fvals[m,n]>=Fmax) Fmax.m<-m 
}}
for (n in max.i:1){
  if (Fvals[Fmax.m,n]>=Fmax) Fmax.n<-n} 

Fmax.m; Fmax.n
min.best <- min.vals[Fmax.m];minx <-log(min.best);min.best
max.best <- max.vals[Fmax.n];maxx <-log(max.best);max.best

#### Rerun with best boundaries
index<-rep(0,length(lx))
index[lx<log(min.best)]<-1   # known juves
index[lx>log(max.best)]<-2   # known adults
unkn<-data.frame(lx[index==0],ly[index==0])  # unknown maturity
names(unkn)<-c("x","y")
tindex<-index  # this index will be modified
n.i <- length(index)
njuv<-length(index[index==1])

for(i in 1:15){
  fit.u <- fitunk(lx,ly,tindex,index,unkn,rss.p)
  tindex <- unlist(fit.u)[1:n.i]
  njuvnew <- length(tindex[tindex==1])
  if(njuvnew==njuv)break
  njuv <- njuvnew
} # end function loop

rss.p <- fit.u[[2]]

# calculate F for best range
ms.diff <- (ss.0[2] - rss.p) / 2
ms.pool <- rss.p/(nrow(data.1)-4)
F.best <- ms.diff/ms.pool

## Statistical test of one vs two lines
rss1 <- rss.0
Tval <- ((rss1-rss.p)/2) /(rss.p/(n.0-4))
F.prob <- pf(Tval, df1 = 2, df = n.0-4, lower.tail = F)
T.prob <- 2*pt(-abs(Tval),df = n.0-4)
Fout <- paste("F Probability =", signif(F.prob,4))
if (F.prob < 0.05)  sig else nsig; Fout

#  fit lines to assigned categories
x1<-lx[tindex==1]
y1<-ly[tindex==1]
x2<-lx[tindex==2]
y2<-ly[tindex==2]

fit.1<-lm(y1~x1)
coeff.1 <- fit.1[[1]]
fit.2<-lm(y2~x2)
coeff.2 <- fit.2[[1]]

pred.1 <- predict(fit.1)
pred.2 <- predict(fit.2)

### Set x, y positions for plotting text
range.lx <- max(lx)-min(lx)
xpos <- NULL
for (i in 1:9){
  xpos[i] <- min(lx) + range.lx*i*0.1
}

range.ly <- max(ly)-min(ly)
ypos <- NULL
for (i in 1:9){
  ypos[i] <- min(ly) + range.ly*i*0.1
}

# Plot 2: crabs as known juveniles, unknowns, and known adults 
plot(lx,ly, type="n", ylab="ln(CH) (mm)",xlab="ln(CW) (mm)",
     main="Initial Assignments", bty="l")
plotchar <-index+(index==0)*3 # plot characters
points(lx,ly, pch=plotchar)  # plot unknowns
points(unkn$x,unkn$y, pch=3)  # plot unknowns
abline(v=minx, lt=3, col="black", lwd=2)
abline(v=maxx, lt=3, col='black', lwd=2)
legend("topleft", pch=c(1,3,2), c("Known Juvenile","Unknown", "Known Adult"), bty="n")
text2.1 <- paste("Unknown range:",min.best,"to", max.best)
text(xpos[7], ypos[2], labels = text2.1)

dev.copy(png,'Crab Maturity Plot 2 - Initial.png', width=600,height=500);dev.off()

# Plot graph No. 3
plot(lx,ly, type="n",ylab=llab.y, xlab=llab.x, main=lab.sex)
points(x1,y1, pch=1, cex=1.2)
points(x2,y2, pch=2, cex=1.2)
segments(x0 = min(x1), y0 = min(pred.1), lty = 1,
         x1 = max(x1), y1 = max(pred.1), col="red")
segments(x0 = min(x2), y0 = min(pred.2), lty = 2,
         x1 = max(x2), y1 = max(pred.2), col="red")

### add text
text3.1 <- paste("Iterations = ", i,"; F = " , signif(F.com,4), sep="") 
text3.2 <- paste("P = " , signif(F.prob,4), sep="")
text3.3 <- paste("Immature = ", round(coeff.1[1], 4), " + ", 
               round(coeff.1[2], 4),"(X)", sep='')
text3.4 <- paste("Mature = ", round(coeff.2[1], 4), " + ", 
                 round(coeff.2[2], 4),"(X)", sep='')

legend(min(lx), ypos[9], legend=c(text3.3, text3.4),
       pch=c(1:2), lty=1:2, cex=1, bty = "n")
text(min(lx), ypos[6], labels = text3.1, adj=0)
text(min(lx), ypos[5], labels = text3.2, adj=0)

# output graph
dev.copy(png,'Crab Maturity Plot 3 - Assigned.png', width=600,height=500);dev.off()

# End of Part B
