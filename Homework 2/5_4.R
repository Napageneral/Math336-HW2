#Same thing as last problem, except you hardly have to clean any data

data = read.csv("C:/Users/Tyler Brandt/Desktop/Math-336/Homework 2/5_5.csv")

#All your cleaning is done here
#Pick coordinates
#Pick dates
pac_indx = which(data[,1]>-20 & data[,1]<20 & data[,2]>160 & data[,2]<260)
cleaned_data = data[pac_indx, 54:103] #1951-2000

svd = svd(cleaned_data)
U=svd$u
D=svd$d
V=svd$v

#a
D[1:10]

#b
x <- 1:20 #100 degrees change in long/ 5 degree steps
y <- 1:8  #40 degrees change in lat/ 5 degree steps
plot.new() #start a new figure from blank
par(mar=c(5,5,2,1))
filled.contour(x, y, matrix(U[,1], ncol = 8), 
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(1,20, length = 6), cex.axis=1.3, labels = c(160, 180, -160, -140, -120, -100))  
                 axis(2,at = seq(1,8, length=9), cex.axis=1.3, labels = c('-20','-15','-10','-5','0','5','10','15','20'))},
               plot.title = title(main = "Spatial pattern (EOF): Pacific 1951",
                                  xlab="Longitude",
                                  ylab="Latitude", cex.lab=1.5),
               color.palette = colorRampPalette(c("red", "white", "blue")))

par(mar=c(5,5,2,1))
filled.contour(x, y, matrix(U[,2], ncol = 8), 
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(1,20, length = 6), cex.axis=1.3, labels = c(160, 180, -160, -140, -120, -100))  
                 axis(2,at = seq(1,8, length=9), cex.axis=1.3, labels = c('-20','-15','-10','-5','0','5','10','15','20'))},
               plot.title = title(main = "Spatial pattern (EOF): Pacific 1952",
                                  xlab="Longitude",
                                  ylab="Latitude", cex.lab=1.5),
               color.palette = colorRampPalette(c("red", "white", "blue")))

par(mar=c(5,5,2,1))
filled.contour(x, y, matrix(U[,3], ncol = 8), 
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(1,20, length = 6), cex.axis=1.3, labels = c(160, 180, -160, -140, -120, -100))  
                 axis(2,at = seq(1,8, length=9), cex.axis=1.3, labels = c('-20','-15','-10','-5','0','5','10','15','20'))},
               plot.title = title(main = "Spatial pattern (EOF): Pacific 1953",
                                  xlab="Longitude",
                                  ylab="Latitude", cex.lab=1.5),
               color.palette = colorRampPalette(c("red", "white", "blue")))

#c
par(mfrow=c(1,1))
par(mar=c(5,4,2,1))
plot(1:50, V[,1],type="o",col="red",lwd=2,
     main="Temporal Pattern (Principal Components): Time series 1",xlab="Time (Years)",
     ylab="PC1 values: dimensionless",
     cex.lab=1.3, cex.axis=1.3)

par(mfrow=c(1,1))
par(mar=c(5,4,2,1))
plot(1:50, V[,2],type="o",col="red",lwd=2,
     main="Temporal Pattern (Principal Components): Time series 2",xlab="Time (Years)",
     ylab="PC2 values: dimensionless",
     cex.lab=1.3, cex.axis=1.3)

par(mfrow=c(1,1))
par(mar=c(5,4,2,1))
plot(1:50, V[,3],type="o",col="red",lwd=2,
     main="Temporal Pattern (Principal Components): Time series 3",xlab="Time (Years)",
     ylab="PC3 values: dimensionless",
     cex.lab=1.3, cex.axis=1.3)


#d
library(maps)
Lat=seq(-17.5,17.5, by=5)
Lon=seq(162.5, 257.5, by=5)
par(mar=c(4,5,3,0))
mapmat=matrix(cleaned_data[,1], nrow=20)
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
mapmat=matrix(cleaned_data[,2], nrow=20)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="Pacific Precipitation [in]: Annual 1952",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

par(mar=c(4,5,3,0))
mapmat=matrix(cleaned_data[,3], nrow=20)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="Pacific Precipitation [in]: Annual 1953",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})