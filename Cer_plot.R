library(plotrix)

cerplot <- function(x,interval=50,ymult=2,year.size=NULL,type.size=NULL) { 

xrange <- c(min(x$Begin)-interval,max(x$End)+interval)
par(oma=c(0.5,1,0.5,1))
plot(0, xlim=c(0, (xrange[2]-xrange[1])+0.5), ylim=c(0, dim(x)[1]+0.5), axes=FALSE, type="n", xlab="", ylab="",main=x$Site[1])

x <- x[order(x$Begin),]
z <- x$Count/sum(x$Count)
years <- seq(xrange[1],xrange[2],by=interval)-xrange[1]
y1 <- seq(1:dim(x)[1])-((z/2)*ymult) 
y2 <- seq(1:dim(x)[1])+((z/2)*ymult)
x1 <- x$Begin-(xrange[1]-1)
x2 <- x$End-(xrange[1]-1)
if(length(type.size)==0) {type.size <- (100-(ceiling(dim(x)[1]/5)*5))/100}
if(length(year.size)==0) {year.size <- ((100-(ceiling(length(years)/5)*5))/100)+0.2}

for(i in 1:length(years)) {
  abline(v=years[i],lty=2,lwd=0.5,col='gray')}
for (i in 1:dim(x)[1]) {
  rect(x1[i],y1[i],x2[i],y2[i],col='red',border='red')}

par(xpd=T)
staxlab(1,years,years+xrange[1],cex=year.size)
text(x1-0.1,seq(1:dim(x)[1]),labels=x$Type,pos=2,cex=type.size)
par(xpd=F)}

x <- read.csv(file.choose(),header=T)
pdf(file='cerplot_out.pdf',width=11,height=8)
by(x,x$Site,cerplot)
dev.off()

