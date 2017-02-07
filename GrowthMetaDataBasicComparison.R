#rm(list=ls())

#### Load libraries
library(FSA)
library(mgcv)
library(MCMCpack) # rwish function
library(R2jags)


#read in data
setwd("S:/Flathead Otolith Project/Data/R projects/MetaAnalysis")
dat <- read.csv("FHCGrowthMetaData.csv")

#look over
head(dat)
tail(dat)
str(dat)
dat$Age <- as.numeric(dat$Age)

# Sort by river
dat <- dat[order(dat$River), ]
head(dat)

# Create new variable to retain river name
dat$River_name <- dat$River

dat$River <- as.numeric(dat$River)
J <- length(unique(dat$River))
# J <- levels(dat$River)

######Plot of length at age for entire dataset
plot(dat$TL~dat$Age, ylab="TL(mm)", xlab="Age(yrs)")



#####Plot length at age of all rivers
pdf("LengthAgeMetaData.pdf")
for (i in unique(dat$River_name)){
  plot(dat$TL[dat$River_name == i] ~ jitter(dat$Age[dat$River_name== i]),main = paste(i), pch=16,
       xlab = "Age (yrs)",
       ylab = "TL (mm)")
}
dev.off()


#####Plot length frequency dist'n of all rivers
pdf("LenghtFreqDisnMetaData.pdf")
#K <- levels(dat$River_name)
for (i in unique(dat$River_name)){
  hist(dat$TL[dat$River_name == i],main = paste(i), pch=16,
       xlab = "TL (mm)")
}
dev.off()


######### matrix plot of Length-at-age for each River
names <- levels(dat$River_name)

res <- 6
name_figure <- "RiverSpecificLengthAge.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
def.par <- par(no.readonly = TRUE)

size.labels = 1
size.text = 1
axissize <- 1
x.label = 'Age (yrs)'
y.label = "Length (mm)"

nf <- layout(matrix(c(1:J),nrow=7,ncol=3,byrow=TRUE),  TRUE) 
layout.show(nf)
par(mar=c(0.0,0.1,0.1,0.1),oma=c(3,3.5,0,1),mai=c(0.0,0.05,0.05,0) )


for(i in 1:J){
  
  plot(dat$TL[dat$River == i] ~ dat$Age[dat$River == i], 
       ylim=c(min(dat$TL,na.rm=T),max(dat$TL,na.rm=T)),
       xlim=c(min(dat$Age, na.rm=T),max(dat$Age, na.rm=T)), 
       axes=F, ylab='', xlab='', type='n')
  
  points(jitter(dat$Age[dat$River == i]), dat$TL[dat$River == i], cex=0.8, pch=16,col='black' )
  
  
  
  if( i <=18){
    axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F ) 
  } else {
    axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01)
  }	
  
  if( i ==1 | i==4 | i==7 | i==10 | i==13 | i==16 | i==19){
    axis(side=2,cex.axis=axissize , mgp=c(0,0.3,0),tck= -0.01, las=1)
  } else {
    axis(side=2,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F)
  }	
  
  
  text(25, 1100, names[i], cex=1.5)
  
  box()
  
}

mtext(y.label, line = 1.9, side = 2, cex = size.text,outer=T)
mtext(x.label, line = 1.5, side = 1, cex = size.text, outer=T)


par(def.par)
dev.off()



##### Nls for entire dataset

pdf("NLSofallPops.pdf")

vbTyp <- vbFuns() #equation taken from loaded lib
svTyp<- list( Linf=950, k=0.03, t0=-2) #starting values
fitTyp <- nls(dat$TL~ vbTyp(dat$Age,Linf,k,t0),data=dat,  start=svTyp) #runs equation

plot(dat$TL~dat$Age, ylab="TL(mm)", xlab="Age(yrs)", main="nls Von Bert of all Populations", pch=16)
curve(vbTyp(x,coef(fitTyp)),from=0,to=35,add=TRUE,lwd=6, col="indianred2") #plots curve of coef's

coef1 <- coef(fitTyp) #remove coef from junk

text(25, 350, paste("Linf =", round(coef1[1], 2), "\n k =", 
                    round(coef1[2], 2), "\n t0 =", round(coef1[3],2)))   #adds coef to plot w/ titles        
dev.off()

#####Run nls for each river 
vbTyp <- vbFuns()#equation taken from loaded lib
svTyp <- list( Linf=950, k=0.04, t0=-2) #starting values, pop 20 had prob so changed startvalues slightly
fitTypR <- list()
for(i in 1:J){ #loop works until river 19, then blows up, try adding in code to tell it to continue
  tryCatch({ #added to tell R to continue looping when theres an error
    fitTypR[[i]] <- nls(TL~ vbTyp(Age,Linf,k,t0), data=dat, subset=River==i,  start=svTyp)
  }, error= function(ex){ #not sure what this is, 20 messes up entirely
  })
}
#create matrix of growth params for each river
coefs <- matrix(NA, nrow=J, ncol=3)
for(i in 1:21){
  coefs[i,] <- coef(fitTypR[[i]])
}



### plot nls for each river

names <- levels(dat$River_name)

res <- 6
name_figure <- "RiverNLS.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
def.par <- par(no.readonly = TRUE)

size.labels = 1
size.text = 1
axissize <- 1
x.label = 'Age (yrs)'
y.label = "Length (mm)"

nf <- layout(matrix(c(1:J),nrow=7,ncol=3,byrow=TRUE),  TRUE) 
layout.show(nf)
par(mar=c(0.0,0.1,0.1,0.1),oma=c(3,3.5,0,1),mai=c(0.0,0.05,0.05,0) )


for(i in 1:J){
  
  plot(dat$TL[dat$River == i] ~ dat$Age[dat$River == i], 
       ylim=c(min(dat$TL,na.rm=T),max(dat$TL,na.rm=T)),
       xlim=c(min(dat$Age, na.rm=T),max(dat$Age, na.rm=T)), 
       axes=F, ylab='', xlab='', type='n')
  
  points(jitter(dat$Age[dat$River == i]), dat$TL[dat$River == i], cex=0.8, pch=16,col='black' )
  curve(vbTyp(x,coefs[i,]),from=0,to=max(dat$Age[dat$River == i])+ 2,add=TRUE,lwd=2, col="indianred2")
  text(25, 350, paste("Linf =", round(coefs[i,1], 2), "\n k =", 
                      round(coefs[i,2], 2), "\n t0 =", round(coefs[i,3],2)), cex=0.75)   
  
  
  
  if( i <=18){
    axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F ) 
  } else {
    axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01)
  }	
  
  if( i ==1 | i==4 | i==7 | i==10 | i==13 | i==16 | i==19){
    axis(side=2,cex.axis=axissize , mgp=c(0,0.3,0),tck= -0.01, las=1)
  } else {
    axis(side=2,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F)
  }	
  
  
  text(5, 1200, names[i], cex=1, font=2)
  
  box()
  
}

mtext(y.label, line = 1.9, side = 2, cex = size.text,outer=T)
mtext(x.label, line = 1.5, side = 1, cex = size.text, outer=T)


par(def.par)
dev.off()


##### create plot with overlaid river nls lines

#to be continued.....




#### run Bayes model across all pops

#################################################################
########## BUGS CODE ############################################
#################################################################


sink("MetavonBmodel_nonHM.txt")
cat("
    model{
    for(i in 1:n){
    y[i] ~ dnorm(y.hat[i], tau.y)
    y.hat[i] <- Linf * (1-exp(-k * (age[i] - t0)))
    }
    
    tau.y <- pow(sigma.y,-2)
    sigma.y ~ dunif(0,100)
    
    Linf ~ dnorm(0,0.001)
    k ~ dnorm(0,0.001)
    t0 ~ dnorm(0,0.001)
    
    
    } # end model
    ",fill=TRUE)
sink()

# load data
data <- list(y = dat$TL, age = dat$Age, n = dim(dat)[1])


# Initial values
inits <- function(){list(Linf = rnorm(1,950,0.001), k = rnorm(1,0.03,0.001), t0 = rnorm(1,-2,0.001),
                         sigma.y = runif(1,1,100)) }


# Parameters monitored
params1 <- c("Linf", "k", "t0", "sigma.y")


# MCMC settings
ni <- 100000
nt <- 3
nb <- 50000
nc <- 3


############################################

out <- jags(data = data, inits = inits, parameters.to.save = params1, 
            model.file = "MetavonBmodel_nonHM.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb)

####Plot of different model outputs
plot(dat$Age, dat$TL, pch=16,xlab='Age (yrs)',ylab='Length (mm)', cex=0.5)
curve(vbTyp(x,coef(fitTyp)),from=0,to=30,add=TRUE,lwd=3, col="indianred2",lty=1 )
a<-out$BUGSoutput$mean$Linf
b<-out$BUGSoutput$mean$k
c<-out$BUGSoutput$mean$t0
bayescoef<-c(a,b,c)
names(bayescoef)[1] <- "Linf" #need rownames?
names(bayescoef)[2] <- "k"
names(bayescoef)[3] <- "t0"
curve(vbTyp(x,bayescoef),from=0,to=30,lwd=3,add=TRUE, col="blue")
legend(12,550, c("nls", "Bayes"), lty=c(2, 1), lwd=c(3,3), col=c("indianred2", "blue"),  box.lwd = 1,box.col = "white",bg = "white")
#to be continued

##Tys plotting method 