
################# PLOT ############################

names <- levels(dat$River_name)

res <- 6
name_figure <- "RiverSpecificPlots.png"
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
  
  
  text(25, 1100, names[i])
  
   box()
  
}

mtext(y.label, line = 1.9, side = 2, cex = size.text,outer=T)
mtext(x.label, line = 1.5, side = 1, cex = size.text, outer=T)


par(def.par)
dev.off()
