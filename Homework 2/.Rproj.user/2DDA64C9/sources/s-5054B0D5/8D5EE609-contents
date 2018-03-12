
#data = read.csv("C:/Users/Tyler Brandt/Desktop/Math-336/Homework 2/5_5.csv")

pac_indx = which(data[,1]>-20 & data[,1]<20 & data[,2]>160 & data[,2]<260)
cleaned_data = data[pac_indx, 54:103] #1951-2000

svd = svd(cleaned_data)
U=svd$u
D=svd$d
V=svd$v

#a
D[1:10]


#b
library(maps)
Lat=seq(-17.5,17.5, by=5)
Lon=seq(162.5, 257.5, by=5)
plot.new()
par(mar=c(4,5,3,0))
mapmat=matrix(U[,1], nrow=20)
int=seq(-5,5,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen',
                               'green', 'yellow','pink','red','maroon'),interpolate='spline')

filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="Pacific Precipitation [in]: Annual 1951",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

par(mar=c(4,5,3,0))
mapmat=matrix(U[,2], nrow=20)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="Pacific Precipitation [in]: Annual 1952",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

par(mar=c(4,5,3,0))
mapmat=matrix(U[,3], nrow=20)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="Pacific Precipitation [in]: Annual 1953",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

#c
par(mfrow=c(1,1))
par(mar=c(3,4,2,1))
plot(1:50, V[,1],type="o",col="red",lwd=2,
     main="Temporal pattern: Time series",xlab="Time",
     ylab="PC1 values: dimensionless",
     cex.lab=1.3, cex.axis=1.3)

par(mfrow=c(1,1))
par(mar=c(3,4,2,1))
plot(1:50, V[,2],type="o",col="red",lwd=2,
     main="Temporal pattern: Time series",xlab="Time",
     ylab="PC1 values: dimensionless",
     cex.lab=1.3, cex.axis=1.3)

par(mfrow=c(1,1))
par(mar=c(3,4,2,1))
plot(1:50, V[,3],type="o",col="red",lwd=2,
     main="Temporal pattern: Time series",xlab="Time",
     ylab="PC1 values: dimensionless",
     cex.lab=1.3, cex.axis=1.3)

par(mfrow=c(1,1))
par(mar=c(3,4,2,1))
plot(1:150, V[,1:3],type="o",col="red",lwd=2,
     main="Temporal pattern: Time series",xlab="Time",
     ylab="PC1 values: dimensionless",
     cex.lab=1.3, cex.axis=1.3)
