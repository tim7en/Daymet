library (ncdf4)
library (ggplot2)

x <- nc_open(file.choose())
myvar <- ncvar_get(x, 'prcp')
lat <- ncvar_get (x, 'lat')
lon <- ncvar_get (x, 'lon')

matDim <- dim(myvar)
tempSplit <- length(myvar)/(matDim[1]*matDim[2])

tempData <- split (myvar, 1:tempSplit)

tempData <- lapply (tempData, na.approx, na.rm = FALSE)

mymat <- matrix(tempData[[1]], nrow = (dim(myvar))[1], ncol = dim(myvar)[2])

x <- array(lon)
y <- array(lat)
z <- array(mymat)





for (i in seq (1, length(tempData))){
  if (length (which(df$Variable>0))>15000){
    print (i)
    print ('hey it is more')
    #df <- na.omit (df)
  }
}

df <- data.frame (as.vector(lon), as.vector(lat),as.vector(tempData[[361]]))
df[is.na(df)] <- 0

df <- apply (df, 2, as.character)
df <- apply (df ,2 , as.numeric)
colnames(df) <- c('Lon', 'Lat','Variable')
ggplot(as.data.frame(df), aes(Lon, Lat, fill = Variable))+geom_tile() +
  xlab("X Coordinate (feet)") + ylab("Y Coordinate (feet)")


library(raster)
library (data.table)
dt <- data.table(as.data.frame(df))

dt <- data.frame (dt)
# set up an 'empty' raster, here via an extent object derived from your data
colnames (dt) <- c('x', 'y', 'z')
e <- extent(dt[,1:2])
e <- e + 1 # add this as all y's are the same

r <- raster(e, nrow=290, ncol=243)

x <- rasterize(dt[, 1:2], r, dt[,3], mean)
plot(x)






dfr <- rasterFromXYZ(dt_x)  #Convert first two columns as lon-lat and third as value                

plot(dfr)

#df$Variable <- as.numeric(as.character(df$Variable))
#df$Variable <- na.approx(df$Variable)

df <- as.table(as.matrix(df))

levelplot(Variable ~ Lon + Lat, data=df)


image (df)

levelplot(Variable~Lon+Lat, data = df)

library (lattice)








levelplot(Height ~ x*y, data = elevation.fit)


image(seq(10, 300, 1), seq(10, 300, 1),as.numeric(tempData[[1]]),
      xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
      main = "Surface elevation data")
box()
