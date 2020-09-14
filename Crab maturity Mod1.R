############# Crab Maturity Analysis Part A: Data Preparation  #############
# A program to separate Mature from Immature crabs based on allometric growth of body parts.  
# Code logic is based on Somerton (1980) Program MATURE for allometry types B-D.
# Which separates two overlapping groups of points  by minimizing RSS of two regression lines
# Developed by Brad Stevens, Univ. of MD Eastern Shore
#
# Part A: This Module reads and modifies data prior to analysis.
#     
# Input required: 
# 1. Path for reading input data file
# 2. Define your input variable names
# 3. Input Sex code as requested ("M" or 1 for males, "F or 2 for females)
#
# For reruns with different settings, change values and run as source.
#
############### Section A1: Read Data #####################################
#install.packages(dplyr)
library(dplyr)  
library(car)

######## Set working path #################
# Rewrite this section to fit your own needs

### Read Jonah Crab Data
setwd("C:/Users/bgstevens/Documents/R Data/R Projects/Crab Maturity")   #Desktop
#setwd("C:/Users/bgstevens/Documents/R/R Projects/Crab Maturity")  #laptop

# Clear out old data if necessary
rm(list=ls(all=TRUE))

# Read Data 
data.1 <- read.csv("JonahCrab_testData.csv", header=T) #
#head(data.1)

# Convert Sex to a factor
data.1$fsex<-factor(data.1$Sex); summary(data.1$fsex)
#sex.code <- readline("Enter Sex code = ")

###### Select sex ###########################
sex.code <- "1" # or "M"
lab.sex <- ifelse( (sex.code=="1" |sex.code=="M"),yes="Males", no= "Females")

##  Select data
datfile <- data.frame(subset(data.1,Sex==sex.code)) 

colnames(datfile)
############### Section A2: Answer some questions ##########

# What is the independent variable? (Choose correct line)
datfile$xvar <-datfile$Width ; meas <- "W"
#datfile$xvar <-datfile$CW ; meas <- "W"
#datfile$xvar <-datfile$Length; meas <- "L"
#datfile$xvar <-datfile$CL; meas <- "L"

lab.x <- ifelse(meas == "W",yes = "Width", no = "Length")
llab.x <- ifelse(meas == "W",yes = "ln(Width)", no = "ln(Length)")

# If Male: What is the dependent variable(s)
if ( sex.code=="1" |sex.code=="M"){
  depvar.1 <- datfile$R_ChH # if side 1 measured
  depvar.1[is.na(depvar.1)]<-0 #turn y variable NAs to 0 

  # if side 2  
  depvar.2 <- datfile$L_ChH # if side 2 measured
  depvar.2[is.na(depvar.2)]<-0

# If  both claws measured, use the larger:
  datfile$yvar <- ifelse((depvar.1 < depvar.2), depvar.2, depvar.1)  
  lab.y <- "Chela Height"; llab.y <- "ln(Chela Ht)" # if male
}  # end if.male section

  # If only one claw measured
#  datfile$yvar <- datfile$R_ChH; lab.y <- "Chela Height"; llab.y <- "log (Chela Ht)" # if male

#  If sex = Female, use abdo width only
  if ( (sex.code=="2" |sex.code=="F")){
    datfile$yvar <- datfile$AbWid
    lab.y <- "Abdomen width"
    llab.y <- "ln(Abdo width)" 
  }
  
# if NaNs are present, run this code
NoNA <- !(is.na(datfile$yvar)|is.na(datfile$xvar))  # finds NA in variables
datfile<- filter(datfile, NoNA)   # removes NA from data file
NoZero<- !((datfile$yvar==0)|(datfile$xvar==0)) # finds 0s in variables
datfile<- filter(datfile, NoZero)

## Use Cooks distance to remove outliers
fit1 <- lm(log(datfile$yvar)~log(datfile$xvar))
plot(log(datfile$yvar)~log(datfile$xvar))
abline(fit1)
cook <- cooks.distance(fit1)
plot(cook)
  cooks.lim <- 0.2 #  FILL IN MANUALLY !!!
  abline(h=cooks.lim, col="red")

  # ### This section removes outliers ####
#
 # keepers <- cook<cooks.lim
 # datfile[!keepers,] #show data to drop
 # datfile<- filter(datfile, keepers)

attach(datfile)

# Ready to run next module 

# Some diagnostic tools
#which[cooks.distance(fit1)>0.3]
#which(datfile$yvar==0)
