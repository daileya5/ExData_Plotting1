## This script will read in household power consumption data retrieved from the UC Irvine Machine Learning Repository
## http://archive.ics.uci.edu/ml/
## and create a lineplot of Global Active Power over two days in February, 2007.

## The following is a brief summary of the dataset used in this script
## Description: Measurements of electric power consumption in one household with a one-minute sampling rate over a period of almost 4 years. Different electrical quantities and some sub-metering values are available.

## The following descriptions of the 9 variables in the dataset are taken from the UCI web site:
## https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption

## Date: Date in format dd/mm/yyyy
## Time: time in format hh:mm:ss
## Global_active_power: household global minute-averaged active power (in kilowatt)
## Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
## Voltage: minute-averaged voltage (in volt)
## Global_intensity: household global minute-averaged current intensity (in ampere)
## Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
## Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
## Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.


## This script is 4 / 4 completed for Course Project 1 in the Coursera course:
## Exploratory Data Analysis in R, through Johns Hopkins University
## Instructor: Roger Peng


## First we must change the working directory to the class project 1 directory
## setwd("ExData_Plotting1/")

## Then execute this command to run this script
## source("plot4.R")




## Read data

## This dataset is separated with ";" instead of "," so we must use sep = ";" to specify that
## This dataset does not contain quotes in the txt file, so use quote="" to avoid problems related to this
## This dataset has NA values coded as ?; use na.strings="?" to handle those values

## Using colClasses = ... will force each column to a specific class, defined here:
power.classes <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
## Defining colClasses will also speed up the time it takes to read in the data from the txt file

## Now that we have established some parameters for reading in the data, we will read in the data from the text file
HH.Power.data <- read.csv("household_power_consumption.txt", sep=";", quote="", na.strings="?", header=TRUE, colClasses = power.classes)




## Now we will remove rows with missing values from the dataset
## The original dataset had the missing values coded as ?, but
## we used na.strings="?" to specify any ? = NA when we read in the data set

## Remove rows containing NA values using na.omit
HH.Power.data <- na.omit(HH.Power.data)

## If we hadn't used na.strings="?", perhaps this is how we would remove rows containing ?
##HH.Power.subset <- HH.Power.subset[-grep("\\?",HH.Power.subset),]




## Next we will need to convert the date and time characters to data / time classes that can be understood by R

## Convert date character to a date class
## In this case the original dates are in the format "dd/mm/yyyy", so we will use format="%d/%m/%Y"
## Use %Y for a four digit year
## Use %y for a two digit year
HH.Power.data$Date <- as.Date(HH.Power.data$Date, format="%d/%m/%Y")




## No we will subset the original dataset to the two days specified in the assingment:
## 2007-02-01 and 2007-02-02
HH.Power.subset <- HH.Power.data[ which(HH.Power.data$Date == "2007-02-01" | HH.Power.data$Date == "2007-02-02"), ]




## Convert time character to a time class
## We are doing this after subsetting data to save time

## In this case the dataset uses 24 hour time, so we use %H for 24 hour time
## The times in the datset are in the format hours:minutes:seconds, so we use format="%H:%M:%S"

## Create a new datetime column that contains both date and time
HH.Power.subset <- within(HH.Power.subset, {datetime = format(as.POSIXct(paste(Date, Time)), "%Y/%m/%d %H:%M:%S") })

## Re-assign datetime column to Time column for use in plotting time series data
HH.Power.subset$Time <- strptime(HH.Power.subset$datetime, format="%Y/%m/%d %H:%M:%S")




## Now that data has been read in, missing values removed, date / time converted from characters to date / time class,
## and the data have been subset to specified days
## we will begin creating the plot




## Create new png for saving plot
png("plot4.png", width=480, height=480)  ## , res=120


## set parameters for plot

## Set backgroung to transparent, as in example
par(bg=NA) 

## Set parameter for 2 rows of plots with 2 columns, plots will be loaded by row
par(mfrow=c(2,2))

## specify axis text is plain (1=plain, 2=bold, 3=italic, 4=bold italic, 5=symbol)
par(font.axis=1)

## specify label test is plain (1=plain, 2=bold, 3=italic, 4=bold italic, 5=symbol)
par(font.lab=1)

## specify main title text is bold (1=plain, 2=bold, 3=italic, 4=bold italic, 5=symbol)
par(font.main=2)

## specify font family used in plot is Arial
par(family="Arial")




## Plot 4-1: Do plot 2

## ToDo: Remove redundant code by creating function for creating 
## plot 2 that uses parameters for Y axis variable, X and Y axis variables, main title, etc.

## create line plot showing global active power use over two days
plot(HH.Power.subset$Time, HH.Power.subset$Global_active_power, type="l",
     
     ## specify x label
     xlab= "",
     
     ## specify y label
     ylab = "Global Active Power") 

## End of Plot 4-1



## Plot 4-2: Do plot 2 with voltage instead of global active power - xlab=datetime

## create line plot showing voltage over two days
plot(HH.Power.subset$Time, HH.Power.subset$Voltage, type="l",
     
     ## specify x label
     xlab= "datetime",
     
     ## specify y label
     ylab = "Voltage") 

## End of Plot 4-2



## Plot 4-3: Do plot 3, no box around legend

## Create line plot showing sub metering 1-3 over two days
## First create an empty plot, to which we will add each sub metering one at a time
plot(HH.Power.subset$Time, HH.Power.subset$Sub_metering_1, type="n",
     
     ## specify x label
     xlab= "",
     
     ## specify y label
     ylab = "Energy sub metering") 

## Add Sub_metering_1 data to plot, with color = black
lines(HH.Power.subset$Time, HH.Power.subset$Sub_metering_1, col="black")

## Add Sub_metering_2 data to plot, with color = red
lines(HH.Power.subset$Time, HH.Power.subset$Sub_metering_2, col="red")

## Add Sub_metering_3 data to plot, with color = blue
lines(HH.Power.subset$Time, HH.Power.subset$Sub_metering_3, col="blue")


## Add legend to Plot 4-3

## Define names of items in legend here to make code more readable
legend.names <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

## Define colors used in legend here to make code more readable
legend.colors <- c("black", "red", "blue")

## Now add the legend to the plot, using legend.names, legend.colors,
## lwd = 1 defines lineweight of lines used in legend, cex = 1 defines relative scaling of legend text,
## btn = "n" removes box around legend
legend("topright", legend = legend.names, col = legend.colors, lwd = 1, cex=1, bty = "n")

## End of Plot 4-3



## Plot 4-4: Do plot 2 with global_reactive_power - xlab=datetime

## create line plot showing global reactive power use over two days
plot(HH.Power.subset$Time, HH.Power.subset$Global_reactive_power, type="l",
     
     ## specify x label
     xlab= "datetime",
     
     ## specify y label
     ylab = "Global_reactive_power") 

## End of Plot 4-4


## Turn plotting device off
dev.off()


## The end