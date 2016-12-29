#20161229 Suehs.  R script for EDA week 1 project assignment: plot4.

#__________________WORKING DIRECTORY AND LIBRARY CALLS_________________________

setwd("C:/Users/csuehs/Documents/Coursera/Exploratory_Data_Analysis/Project_Assignment_Week_1")

#__________________READ IN THE PROVIDED DATASET________________________________

#!!!!!!!!!!!!!!!!!!!!!!!!can take a few minutes!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#Download the zip files and unzip to the working directory.

temp <- tempfile()
zip_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(zip_url, temp)
unzip(temp, exdir = ".")
unlink(temp)

#We will only be using data from the dates 2007-02-01 and 2007-02-02. One 
#alternative is to read the data from just those dates rather than reading in 
#the entire dataset and subsetting to those dates.

#____________EXTRACT LINES CONTAINING PERTINENT DATES___________________________


filenaem <- "household_power_consumption.txt"

goodlines1 <- 0; goodlines2 <- 0
index <- 0
ChunkSize <- 50000
con <- file(description = filenaem, open = "r")
datachunk <- read.table(con, nrows = ChunkSize, sep = ";",  fill = TRUE, 
                        header = TRUE)
headernames <- colnames(datachunk)
goodlines1 <- subset(datachunk, datachunk$Date == "1/2/2007")
goodlines2 <- subset(datachunk, datachunk$Date == "2/2/2007")
goodlines <- rbind(goodlines1, goodlines2)

repeat {
    index <- index+1
    print(paste("Processing rows: ", index * ChunkSize))
    if (nrow(datachunk) != ChunkSize){
        print ("Processed all chunks!")
        break}
    datachunk <- read.table(con, nrows = ChunkSize, sep = ";",  fill = TRUE, 
                            header = FALSE, col.names = headernames)
    goodlines1 <- subset(datachunk, datachunk$Date == "1/2/2007")
    goodlines2 <- subset(datachunk, datachunk$Date == "2/2/2007")
    goodlines <- rbind(goodlines, goodlines1, goodlines2)
}

close(con)

colnames(goodlines) <- headernames

#variable cleanup

rm(temp); rm(zip_url)
rm(goodlines1); rm(goodlines2)
rm(index); rm(ChunkSize)
rm(con); rm(datachunk)

#______________________DATA CLEANING___________________________________________

#first replace "?" with "NA" throughout the ugly_subset

#can't find any "?"s

#establish proper classes
date_1 <- as.Date(goodlines$Date, "%d/%m/%Y")

time_sub <- paste(goodlines$Date, goodlines$Time)
date_time_2 <- strptime(time_sub, "%d/%m/%Y %H:%M:%S")

numerics <- goodlines[,3:9]

numerics2 <- sapply(numerics, as.character)

V3 <- as.numeric(numerics2[,1])
V4 <- as.numeric(numerics2[,2])
V5 <- as.numeric(numerics2[,3])
V6 <- as.numeric(numerics2[,4])
V7 <- as.numeric(numerics2[,5])
V8 <- as.numeric(numerics2[,6])
V9 <- as.numeric(numerics2[,7])

clean_data <- data.frame(date_1, date_time_2, V3, V4, V5, V6, V7, V8, V9)
colnames(clean_data) <- headernames

#variable clean up

rm(date_1); rm(time_sub); rm(date_time_2) 
rm(numerics); rm(numerics2)
rm(V3); rm(V4); rm(V5); rm(V6)
rm(V7); rm(V8); rm(V9)
rm(goodlines); rm(headernames); rm(filenaem)

#_____________________CREATE plot4_______________________________________

png(file = "plot4.png", width = 480, height = 480)    #open graphics device

par(mfrow=c(2,2))
plot(clean_data$Time, clean_data$Global_active_power, 
     type = "l",
     xlab = "",
     ylab = "Global Active Power")
plot(clean_data$Time, clean_data$Voltage,
     type = "l",
     xlab = "datetime",
     ylab = "Voltage")
plot(clean_data$Time, clean_data$Sub_metering_1, 
     type = "l",
     xlab = "",
     ylab = "Energy sub metering")
lines(clean_data$Time, clean_data$Sub_metering_2, col = "red")
lines(clean_data$Time, clean_data$Sub_metering_3, col = "blue")
legend("topright", col = c("black", "red", "blue"), lty = 1, bty = "n",
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
plot(clean_data$Time, clean_data$Global_reactive_power,
     type = "l",
     xlab = "datetime",
     ylab = "Global_reactive_power")

dev.off()           #close graphics device


