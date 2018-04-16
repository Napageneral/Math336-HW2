#Scan in the data, since it is in .asc format, use the command "scan(path_to_file.asc)"
data = scan("5_4.asc")

#Upon inspection of the readme accompanying the data, you learn:
#that the dimensions of the array the data is in are 72x36
#the data is organized in grids based on
#latitude: -87.5 to 87.5, with a step of 5 degrees
#longitude: 2.5 to 357.5, with a step of 5 degrees
#This makes sense based on the dimensions of the data since 
#latitude: 87.5-(-87.5) = 180, 180/5 = 36
#longitude: 357.5-(-2.5) = 360, 360/5 = 72

#The data is organized monthly, so one 72x36 block of data for each month in each year from 1880-2010
#Upon inspecting the actual data in a text editor, you find that the first two values are: 1, 1880
#These values correspond to the month and year of the succeeding 36x72 block of data.
#This means each 36x72 block of data is preceeded by two values, the month and the year of that data.
#2 values (month, year) + 36x72 values = 2+2592 = 2594 values make up each chunk of data.

#Using this information, we can create a sequence of numbers:
#starting at 1 and increasing by the chunk size (2594)
#in order to generate the indices of every month value in the data
month_indices = seq(1,length(data), by=2594)
#We create a similar sequence, just starting at 2 this time, to generate the year value indices
year_indices = seq(2,length(data), by=2594)

#We can then copy the values of those indices into new sets for easier access
months = data[month_indices]
years = data[year_indices]

#We can recreate our own date labelling here
date = paste(years, sep = "-", months)

#Now that we have our own date labels 
#We can combine the indices of month and year into one vector
tm3 = as.vector(t(cbind(month_indices, year_indices)))
#And create a new dataset without the month and year values
data2 = data[-tm3]

#Now we have just the raw anomaly data in one continuous string
#We need to break it up into a 36x72 matrix to make it usable
#So we will create another new dataset by making a 36x72 matrix of our dataless dataset
da3 = matrix(data2, ncol=length(data2)/(36*72))
#We can now append our own date labels we created earlier as column names
colnames(da3) = date

#Similar to how we generated the indices for the months and years
#We will generate the lats and lons for the data using the info from the readme
#Create the sequence
lat1 = seq(-87.5, 87.5, length = 36)
lon1 = seq(2.5, 357.5, length = 72)
#Repeat it to create a 2D grid
lat = rep(lat1, each=72) #repeats each 72 times
lon = rep(lon1, 36) #repeats the whole thing 36 times

#We will now combine our data with our coordinate labels to make the data more understandable
cleaned_data = cbind(lat,lon, da3)

#Now that we have cleaned our data and made it easily accessible by date and coordinate
#We can select what coordinates and what dates we actually want to look at
#Coordinates
pac_indx = which(cleaned_data[,1]>-20 & cleaned_data[,1]<20 & cleaned_data[,2]>160 & cleaned_data[,2]<260)

#Dates for those coordinates
#Notice I'm choosing July 1951 through June 2001
#This will become important later
dat_pac = cleaned_data[pac_indx, 861:1460] #july 1951- june 2001


#So we end up with a range of 40 degrees lon and 100 degrees lat
#Since we have a data point every 5 degrees, we have:
#40/5 = 8 values
#100/5 = 20 values
#All these values form a grid, so  we have:
#8x20 = 160 total values for each date
#We have 50 years of this data, with 12 months a year
#50*12 = 600 dates
#so our final data set is 160 potential coordinates over 600 dates -> 160x600

#In order to get accurate calculations, we need to scrub some null values from the set
#Wherever a value should be Null, it has the value -999
#So if we go through our data and replace any anomalies with 0
#We wont have those big numbers messing up our calculations
#There are potentially better values to use as a replacement
#But this will do for now

#Iterate through every value
#Check if it's above/below a threshold
#If so, set it equal to 0
for (i in 1:160) {
  for (j in 1:600) {
    if(dat_pac[i,j]< -300){
      dat_pac[i,j] = 0
    }
  }
}


#Compress monthly data into the yearly mean
#Now the assignment asks for the (July-June) mean anomalies
#So we need to compress our monthly-yearly values into a July-June yearly mean
#Our mean dataset will only have 50 dates, one for each year
#So we create a new 160x50 matrix
pacificMeans = matrix(0, nrow = 160, ncol = 50)

#Generate a new sequence to compress our 600 dates by a factor of 12
tmp = seq(1,600, by=12)

#Iterate through our new matrix
#Setting the values equal to the july-june mean of the uncompressed set
for (i in 1:160) {
  for (j in 1:50) {
    pacificMeans[i,j] = mean(dat_pac[i,tmp[j]:tmp[j]+11])
  }
}


#Now that the hard part is done, we can actually start the assignment

#a
#Now that we have our data how we need it, all we have to do is call svd on it
#Save the results as a new variable
pMeansvd=svd(pacificMeans)
#Save the components of the results by extracting columns by name
#svd decomposes a space-time matrix into:
#a spatial pattern matrix U
#a diagonal energy level matrix D
#and a temporal matrix V
U = pMeansvd$u
D = pMeansvd$d
V = pMeansvd$v

#Since D is our diagonal matrix, it is composed of the eigenvalues of our data
#Print out the first 10
D[1:10]

#b
#Create sequences in order to plot our data
x <- 1:20 #100 degrees change in long/ 5 degree steps
y <- 1:8  #40 degrees change in lat/ 5 degree steps


plot.new() #start a new figure from blank
par(mar=c(5,5,2,1)) #set margins
#We will be plotting the first column of our U matrix
#Which is comprised of 160 values representing 20x8 matrix
#Really no idea why in matrix(U[,1], ncol=8), ncol=8, seems like ncol=20 makes more sense
#Repeat this for each column
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
#Plot your temporal V[,1-3] columns here
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
#Show how your svd analysis is indicative of El Nino
library(maps)
Lat=seq(-17.5,17.5, by=5)
Lon=seq(162.5, 257.5, by=5)
par(mar=c(4,5,3,0))
mapmat=matrix(pacificMeans[,1], nrow=20)
int=seq(-5,5,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen','green', 'yellow','red','maroon'),interpolate='spline')

filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="Pacific SST Anomalies [deg C]: Annual 1951",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

par(mar=c(4,5,3,0))
mapmat=matrix(pacificMeans[,2], nrow=20)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="Pacific SST Anomalies [deg C]: Annual 1952",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

par(mar=c(4,5,3,0))
mapmat=matrix(pacificMeans[,3], nrow=20)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="Pacific SST Anomalies [deg C]: Annual 1953",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})
