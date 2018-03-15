#canum plot#

catches <- read.csv("C:/Work/HAWG/stock_num.csv", header = T)
par(mfrow = c(9,1), mai = c(0.2,0,0,0))

scalar <- sqrt(max(catches, na.rm=T))
years <- max(catches$Year) - min(catches$Year) + 1
plot.cols <- sample(colours(), years +9)
barplot(height = sqrt(catches$Age.1)/scalar, col = plot.cols[9:70], ylim = c(0,1), ylab = "Age 1")
barplot(height = sqrt(catches$Age.2)/scalar, col = plot.cols[8:69], ylim = c(0,1), ylab = "Age 2")
barplot(height = sqrt(catches$Age.3)/scalar, col = plot.cols[7:68], ylim = c(0,1), ylab = "Age 3")
barplot(height = sqrt(catches$Age.4)/scalar, col = plot.cols[6:67], ylim = c(0,1), ylab = "Age 4")
barplot(height = sqrt(catches$Age.5)/scalar, col = plot.cols[5:66], ylim = c(0,1), ylab = "Age 5")
barplot(height = sqrt(catches$Age.6)/scalar, col = plot.cols[4:65], ylim = c(0,1), ylab = "Age 6")
barplot(height = sqrt(catches$Age.7)/scalar, col = plot.cols[3:64], ylim = c(0,1), ylab = "Age 7")
barplot(height = sqrt(catches$Age.8)/scalar, col = plot.cols[2:63], ylim = c(0,1), ylab = "Age 8")
barplot(height = sqrt(catches$Age.9)/scalar, col = plot.cols[1:62], ylim = c(0,1), ylab = "Age 9+")



dev.off()
