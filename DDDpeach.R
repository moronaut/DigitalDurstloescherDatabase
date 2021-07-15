#create nice graphs for DDD
#version: 201027.1

# load csv
setwd(dir = "") #Set to dir that contains .csv of DDD 
dat <- read.csv(file = "", sep =",", header = T) #Set to propper .csv

peachQuot <- function() {
	# ratio of peach löschers per year plus growth/loss of ratio per year
	# split by year
	d16 <- subset(dat, grepl(2016, dat$FileCreateDate))
	d17 <- subset(dat, grepl(2017, dat$FileCreateDate))
	d18 <- subset(dat, grepl(2018, dat$FileCreateDate))
	d19 <- subset(dat, grepl(2019, dat$FileCreateDate))
	d20 <- subset(dat, grepl(2020, dat$FileCreateDate))
	# split peach from rest
	pd16 <- subset(d16, d16$Type == "Pf")
	pd17 <- subset(d17, d17$Type == "Pf")
	pd18 <- subset(d18, d18$Type == "Pf")
	pd19 <- subset(d19, d19$Type == "Pf")
	pd20 <- subset(d20, d20$Type == "Pf")
	# calc ratios
	r16 <- (length(pd16[,1])/length(d16[,1])*100)
	r17 <- (length(pd17[,1])/length(d17[,1])*100)
	r18 <- (length(pd18[,1])/length(d18[,1])*100)
	r19 <- (length(pd19[,1])/length(d19[,1])*100)
	r20 <- (length(pd20[,1])/length(d20[,1])*100)
	# calc percentage growth
	g17 <- (100*(r17 - r16)/r16)
	g18 <- (100*(r18 - r17)/r17)
	g19 <- (100*(r19 - r18)/r18)
	g20 <- (100*(r20 - r19)/r19)
	# plotting time
	bp <- barplot(c(r16,r17,r18,r19,r20), axes = F, names.arg = c("2016", "2017", "2018", "2019", "2020"), main = "peachQuot")
	abline(h=c(10,20,30))
	text(x = bp[2:5,1], y = 31, labels = round(c(g17, g18, g19, g20), digits = 1))
	text(x = bp[,1], y = 1, labels = round(c(r16, r17, r18, r19, r20), digits = 1))
}

yearTotal <- function() {
	# plain total amount of durstlöscher added + groth/loss per year
	# split by year
	d16 <- length(subset(dat, grepl(2016, dat$FileCreateDate))[,1])
	d17 <- length(subset(dat, grepl(2017, dat$FileCreateDate))[,1])
	d18 <- length(subset(dat, grepl(2018, dat$FileCreateDate))[,1])
	d19 <- length(subset(dat, grepl(2019, dat$FileCreateDate))[,1])
	d20 <- length(subset(dat, grepl(2020, dat$FileCreateDate))[,1])
	# calc percentage growth Prozentuale Veränderung = 100 × (gegenwärtiger oder zukünftiger Wert - vergangener oder gegenwärtiger Wert) / vergangener oder gegenwärtiger Wert 
	g17 <- (100*(d17 - d16)/d16)
	g18 <- (100*(d18 - d17)/d17)
	g19 <- (100*(d19 - d18)/d18)
	g20 <- (100*(d20 - d19)/d19)
	# plotting time
	bp <- barplot(c(d16,d17,d18,d19,d20), axes = F, names.arg = c("2016", "2017", "2018", "2019", "2020"), main = "yearTotal")
	abline(h=c(200,400,600))
	text(x = bp[2:5,1], y = 550, labels = round(c(g17, g18, g19, g20)))
	text(x = bp[,1], y = 15, labels = c(d16, d17, d18, d19, d20))
}

compQuot <- function() {
	# bargraph of total löscher per manufacturer + part pfirsich (+ percentage)
	# split by company
	qv <- subset(dat, dat$Design == "0" | dat$Design == "B")
	wg <- subset(dat, dat$Design == "A" | dat$Design == "C")
	# pfirsich
	qv20 <- length(subset(qv, grepl("Pf", qv$Type))[,1])
	wg20 <- length(subset(wg, grepl("Pf", wg$Type))[,1])
	# not Pfirsich
	qvN20 <- length(subset(qv, !grepl("Pf", qv$Type))[,1])
	wgN20 <- length(subset(wg, !grepl("Pf", wg$Type))[,1])
	
	plotDat <- matrix(nrow = 2, ncol = 2, byrow = F, c(qvN20, qv20, wgN20, wg20))
	bp <- barplot(plotDat, names.arg = c("QV", "WG"), main = "compQuot")
	text(x = bp, y = 50, labels = c(qvN20,wgN20))
	text(x = bp, y = 600, labels = c(qv20,wg20))
	
	print(qv20/(qv20+qvN20))
	print(wg20/(wg20+wgN20))
} 
