#SVD demo for generated data
#By Sam Shen at SDSU February 23, 2017

#Demonstrate SVD for a simple 2X3 matrix
a1<-matrix(c(1,1,0,-1,-1,0),nrow=2)
a1#The data matrix
#     [,1] [,2] [,3]
#[1,]    1    0   -1
#[2,]    1   -1    0

svda1<-svd(a1)
U<-svda1$u
D<-svda1$d
V<-svda1$v
U
#           [,1]       [,2]
#[1,] -0.7071068 -0.7071068
#[2,] -0.7071068  0.7071068
V
#           [,1]          [,2]
#[1,] -0.8164966  1.110223e-16
#[2,]  0.4082483 -7.071068e-01
#[3,]  0.4082483  7.071068e-01
D
#[1] 1.732051 1.000000

#Verification of SVD: A = UDV'
round(U%*%diag(D)%*%t(V))
#     [,1] [,2] [,3]
#[1,]    1    0   -1
#[2,]    1   -1    0
#round() removes decimal places
#This is the original data matrix a1 

#Graphically show the U column vectors, aka EOFs
plot.new()
par(mfrow=c(1,2))
par(mgp=c(2,1,0),mar=c(4,4,3,1))
plot(1:2, U[,1],type="o", ylim=c(-1,1), 
     xlab="Spatial position: x",ylab="Mode values [Dimensionless]",
     main="Spatial modes: EOFs",lwd=5)
lines(1:2, U[,2],type="o",col="blue",lwd=5)
text(1.2,0.1,"EOF2", col="blue", cex=1.5)
text(1.8,-0.40,"EOF1", cex=1.5)
par(mgp=c(2,1,0),mar=c(4,4,3,1))
plot(1:3, V[,1],type="o", ylim=c(-1,1), col="red",
     xlab="Temporal position: t",ylab="Mode values [Dimensionless]",
     main="Temporal modes: PCs",lwd=5)
lines(1:3, V[,2],type="o",col="darkgreen",lwd=5)
text(2.4,-0.6,"PC2", col="darkgreen", cex=1.5)
text(1.7,0.5,"PC1", col="red", cex=1.5)

#SVD demo for generated data
#By Sam Shen at SDSU February 23, 2017

#Generate random data on a 10-by-15 grid with 20 time points
#SVD for 2D spatial dimension and 1D time
rm(list=ls())#remove the R console history
dat<- matrix(rnorm(10*15*20),ncol=20)
x<- 1:10
y<- 1:15
udv<- svd(dat)
U<-udv$u
D<-udv$d
V<-udv$v
dim(U)
dim(V)
length(D)
#Plot spatial pattern for EOF1
umat<- matrix(U[,1],nrow=15)
dim(umat)
plot.new() #start a new figure from blank
par(mar=c(3,4,2,1))
filled.contour(x, y, t(umat), 
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(0,10, by = 2), cex.axis=1.3) 
                 axis(2,seq(2, 15, by = 3), cex.axis=1.3)},
               plot.title = title(main = "Spatial pattern: EOF1",
                                  xlab="Spatial x position: 1 to 10",
                                  ylab="Spatial y position: 1 to 15", cex.lab=1.5),
               color.palette =
                 colorRampPalette(c("red", "white", "blue"))
)
#Plot time pattern PC1
par(mfrow=c(1,1))
par(mar=c(3,4,2,1))
plot(1:20, V[,1],type="o",col="red",lwd=2,
     main="Temporal pattern: Time series",xlab="Time", 
     ylab="PC1 values: dimensionless",
     cex.lab=1.3, cex.axis=1.3)


# Read the txt data
Pta<-read.table("~/Desktop/MyDocs/teach/336MathModel-2016SP/BookMathModeling2016/R-code4MathModelBook/Ch5-SOI/PSTANDtahiti.txt", header=F)
# Remove the first column that is the year
ptamon<-Pta[,seq(2,13)]
#Convert the matrix into a vector according to mon: Jan 1951, Feb 1951, ..., Dec 2015
ptamonv<-c(t(ptamon))
xtime<-seq(1951, 2016-1/12, 1/12)
# Plot the Tahiti standardized SLP anomalies
plot(xtime, ptamonv,type="l",xlab="Year",ylab="Presure", 
     main="Standardized Tahiti SLP Anomalies", col="red",
     xlim=range(xtime), ylim=range(ptamonv))
# Do the same for Darwin SLP
Pda<-read.table("~/Desktop/MyDocs/teach/336MathModel-2016SP/BookMathModeling2016/R-code4MathModelBook/Ch5-SOI/PSTANDdarwin.txt", header=F)
pdamon<-Pda[,seq(2,13)]
pdamonv<-c(t(pdamon))
plot(xtime, pdamonv,type="l",xlab="Year",ylab="Presure", 
     main="Standardized Darwin SLP Anomalies", col="blue",
     xlim=range(xtime), ylim=range(pdamonv))
#Plot the SOI index 
plot(xtime, ptamonv-pdamonv,type="l",xlab="Year",
     ylab="SOI index", col="black",xlim=range(xtime), ylim=c(-4,4), lwd=1)
#Add ticks on top edge of the plot box
axis (3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
# Add ticks on the right edge of the plot box
axis (4, at=seq(-4,4,2), labels=seq(-4,4,2))
# If put a line on a plot, use the command below
lines(xtime,ptamonv-pdamonv,col="black", lwd=1)

#Darwin-Tahiti SLP data SVD
ptada <-cbind(ptamonv,pdamonv)
ptada<-t(ptada)
svdptd<-svd(ptada)
Energy=svdptd$d
EOF=svdptd$u
PC=svdptd$v
Energy
#[1] 31.34582 22.25421
D=diag(svdptd$d)
D
#[,1]     [,2]
#[1,] 31.34582  0.00000
#[2,]  0.00000 22.25421



#Display the two ENSO modes on a world map
library(maps)
library(mapdata)

plot.new()
par(mfrow=c(2,1))

par(mar=c(0,0,0,0)) #Zero space between (a) and (b)
map(database="world2Hires",ylim=c(-70,70), mar = c(0,0,0,0))
grid(nx=12,ny=6)
points(231, -18,pch=16,cex=2, col="red")
text(231, -30, "Tahiti 0.61", col="red")
points(131, -12,pch=16,cex=2.6, col="blue")
text(131, -24, "Darwin -0.79", col="blue")
axis(2, at=seq(-70,70,20), 
     col.axis="black", tck = -0.05, las=2, line=-0.9,lwd=0)
axis(1, at=seq(0,360,60),  
     col.axis="black",tck = -0.05, las=1, line=-0.9,lwd=0)
text(180,30, "El Nino Southern Oscillation Mode 1",col="purple",cex=1.3)
text(10,-60,"(a)", cex=1.4)
box()

par(mar=c(0,0,0,0)) #Plot mode 2
map(database="world2Hires",ylim=c(-70,70),  mar = c(0,0,0,0))
grid(nx=12,ny=6)
points(231, -18,pch=16,cex=2.6, col="red")
text(231, -30, "Tahiti 0.79", col="red")
points(131, -12,pch=16,cex=2, col="red")
text(131, -24, "Darwin 0.61", col="red")
text(180,30, "El Nino Southern Oscillation Mode 2",col="purple",cex=1.3)
axis(2, at=seq(-70,70,20), 
     col.axis="black", tck = -0.05, las=2, line=-0.9,lwd=0)
axis(1, at=seq(0,360,60),  
     col.axis="black",tck = -0.05, las=1, line=-0.9,lwd=0)
text(10,-60,"(b)", cex=1.4)
box()

#Plot WSOI1
xtime<-seq(1951, 2016-1/12, 1/12)
wsoi1=D[1,1]*t(PC)[1,]
plot(xtime, wsoi1,type="l",xlab="Year",ylab="Weighted SOI 1", 
     col="black",xlim=range(xtime), ylim=range(wsoi1), lwd=1)
axis (3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
#Plot WSOI2
wsoi2=D[2,2]*t(PC)[2,]
plot(xtime, wsoi2,type="l",xlab="Year",ylab="Weighted SOI 2", 
     col="black",xlim=range(xtime), ylim=c(-2,2), lwd=1)
axis (3, at=seq(1951,2015,4), labels=seq(1951,2015,4))

cwsoi1=cumsum(wsoi1)
plot(xtime,cwsoi1,type="l",xlab="Year",ylab="Cumulated weighted SOI 1", 
     col="black",xlim=range(xtime), ylim=range(cwsoi1), lwd=1)
axis (3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
#Plot cumulative WSOI2
cwsoi2=cumsum(wsoi2)
plot(xtime, cwsoi2,type="l",xlab="Year",ylab="Cumulated weighted SOI 2", 
     col="black",xlim=range(xtime), ylim=range(cwsoi2), lwd=1)
axis (3, at=seq(1951,2015,4), labels=seq(1951,2015,4))

