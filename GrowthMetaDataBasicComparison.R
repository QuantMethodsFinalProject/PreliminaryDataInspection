#rm(list=ls())

#### Load libraries
library(FSA)
library(mgcv)

#read in data
setwd("S:/Flathead Otolith Project/Data/R projects")
dat <- read.csv("FHCGrowthMetaData.csv")

#look over
head(dat)
tail(dat)
str(dat)
dat$Age <- as.numeric(dat$Age)
#dat$River <- as.numeric(dat$River)
J <- levels(dat$River)

######Plot of length at age for entire dataset
plot(dat$TL~dat$Age, ylab="TL(mm)", xlab="Age(yrs)")



#####Plot length at age of all rivers
pdf("S:/Flathead Otolith Project/Data/Plots/MetaAnalysis/LengthAgeMetaData.pdf")
for (i in J){
  plot(dat$TL[dat$River == i] ~ jitter(dat$Age[dat$River == i]),main = paste(i), pch=16,
       xlab = "Age (yrs)",
       ylab = "TL (mm)")
}
dev.off()


#####Plot length frequency dist'n of all rivers
pdf("S:/Flathead Otolith Project/Data/Plots/MetaAnalysis/LenghtFreqDisnMetaData.pdf")
for (i in J){
  hist(dat$TL[dat$River == i],main = paste(i), pch=16,
       xlab = "TL (mm)")
}
dev.off()

##### Nls for entire dataset
vbTyp <- vbFuns()
svTyp<- list( Linf=950, k=0.03, t0=-2)
fitTyp <- nls(dat$TL~ vbTyp(dat$Age,Linf,k,t0),data=dat,  start=svTyp)
plot(dat$TL~dat$Age, ylab="TL(mm)", xlab="Age(yrs)")
curve(vbTyp(x,coef(fitTyp)),from=0,to=35,add=TRUE,lwd=6, col="indianred2")


####Run nls for each river #not complete!
vbTyp <- vbFuns()
svTyp<- list( Linf=950, k=0.03, t0=-2)
#i <- subset(dat, River=="Altamaha")
#fitTyp <- nls(dat$TL~ vbTyp(dat$Age,Linf,k,t0),data=dat, start=svTyp)
#946, 0.16, 0.04 overall coef
#curve(vbTyp(x,coef(fitTyp[i])),from=0,to=20,add=TRUE,lwd=6, col="indianred2")
dat$River<- as.numeric(as.factor(as.factor(dat$River)))
fitTyp <- list()
 for(i in J){
   fitTyp[[i]] <- nls(dat$TL~ vbTyp(dat$Age,Linf,k,t0),data=dat[dat$River==i,],  start=svTyp)
   }

 #this code works but all coef's are same...?



########


dat$River<- as.numeric(as.factor(as.factor(dat$River)))

fitTyp <- c()
for(i in J){
  fitTyp[[i]] <- nls(dat$TL~ vbTyp(dat$Age,Linf,k,t0),data=dat[dat$River==i,], start=svTyp)
}



#################################
for(i in 1:max(dat$River)){
  
  s1=subset(dat,dat$River==i) 
  TL= s1$TL
  Age= s1$Age
  fitTyp=nls(TL~ vbTyp(Age,Linf,k,t0), data=dat,  start=svTyp)
}

summary(fitTyp)
