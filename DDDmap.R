#automatically adds geotagged durstlöschers to map. Can handle further mapping by type, damage, place if given extra table
#200628.3

# Version 1: Added Date plotting
# Version 2: Added PlotMainArea, added getMonthData
# version 3: Changes Datesearch to search in filename (prone for errors when pictures taken at 20:20 etc)
# under development: Density Grid function.

library(rgdal)
library(exifr)


setwd(dir = "") #set to folder containing DDD .csv file

readLoescherTable <- function() {
    read.csv(file="./DDD.csv", sep =",", header = T)
}

readExifData <- function() {
    files <- list.files(pattern = "*.jpg")
    info <- read_exif(files, tags = c("GPSLongitude", "GPSLatitude"))
    write.csv(info, file = "gpsdata.csv")
}

writeLoescherTable <- function() {
    ID <- seq(1,length(dat$ID),1)
    dat$ID <- ID
    write.csv(dat, file ="DDD.csv", row.names = F)
}

#plots map with saved shapefiles
plotMap <- function()  {
	#read shapefiles for NRW
	#map was converted to wps84 using qgis -> layer -> save layer as -> blabla
	map <- readOGR("") # Set to map shapefile (outline)
	int <- readOGR("") #Set to internal borders shapefile
	riv <- readOGR("") # set to river shapefile
	#plot map
	par(mar = c(0,0,0,0))
	plot(map, lwd = 2, border = "lightgrey")
	lines(int, lwd = 1, col = "grey")
	lines(riv, lwd = 3, col = "lightblue")
	#text(pla@coords, labels = pla$place, font = 2, cex = 0.7, col = "grey")
	#show coordinates for debug
	axis(1)
	axis(2)
}

plotMainArea <- function() {
	par(mar = c(0,0,0,0))
	plot(x = 51.4, y = 6.8, type = "n", xlim = c(6.755, 6.777), ylim = c(51.425, 51.435))
}

#add color coded points, choose column for category to color code
addPointsCol <- function(data = dat, cat = 5) {
	palette(rainbow(length(unique(data[,cat]))))
	points(data[,3], data[,4], pch = 20, col = data[,cat])
	legend(x = 6.755, y = 51.435, legend = sort(unique(data[,cat])), col = sort(unique(data[,cat])), pch = 20, bty = "n", bg = "n",  horiz = F)
}

#add only points with particular attribute
addPointsIf <- function(data = dat, cat = 6, selector = "Du", fill = "blue") {
	data <- subset(data, data[,cat] == selector)
	points(data$GPSLongitude, data$GPSLatitude, pch = 20, col = fill)
}

#add all points in a csv file to the map
addPointsAll <- function(fill = "green") {
	data <- dat
	points(data$GPSLongitude, data$GPSLatitude, pch = 20, col = fill, cex = 0.5)
}

#add all points of particular year/month/day. Format: Day - YYYY:MM:DD, Month - YYYY:MM, Year - YYYY
addPointsDateOld <- function(data = dat, date = "2020:01", fill = "purple") {
	for (i in 1:length(data[,1])) {
		if(grepl(date, data[i,2]) == T) {
			points(data[i,3], data[i,4], pch = 20, col = fill)
		}
	}
}

#add all points of particular year/month/day. Format: Day - YYYYMMDD, Month - YYYYMM, Year - YYYY
addPointsDate <- function(data = dat, date = "202001", fill ="purple") {
	term = paste(c("IMG_", date), collapse="")
	for (i in 1:length(data[,1])) {
		if(grepl(term, data[i,1]) == T) {
			points(as.numeric(data[i,3]), as.numeric(data[i,4]), pch = 20, col = fill, cex = 0.8)
		}
	}
}

#creates findings per month
getMonthData <- function(date = "202001") {
	sub <- subset(dat, grepl(date, dat$X))
	print(c("total: ",length(sub[,1])))
	cities <- unique(sub$City)
	types <- unique(sub$Type)
	print("Cities:")
	for (i in cities) {
		print (c(i, length(which(sub$City == i))))	
	}
	print("Types:")
	for (i in types) {
		print (c(i, length(which(sub$Type == i))))	
	}
	
}

addDensityGrid <- function(intervals = 8) {
	xVal <- c(6.755, 6.777)
	yVal <- c(51.425, 51.435)
	abline (v=seq(xVal[1], xVal[2], length.out = intervals ))
	abline (h=seq(yVal[1], yVal[2], length.out = intervals ))
}
dat <- readLoescherTable()
