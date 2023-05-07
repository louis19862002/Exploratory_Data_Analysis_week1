
### Read data file
setwd("C:/Users/louis/OneDrive/Desktop/Coursera/Exploratory_Data_Analysis")
df <- read.delim("household_power_consumption.txt",header = TRUE, sep = ";", stringsAsFactors = FALSE)

### Reformat Data
df$Date <- as.Date(df$Date, format = "%d/%m/%Y")

### Reformat Time

df$Time <- strptime(df$Time,format = "%H:%M:%S")
df$Time <- format(df$Time, format = "%H:%M:%S")

### Change data to numeric
df[,3:9] <- lapply(df[,3:9],function(x)as.numeric(as.character(x)))

### replace "?" by NA
replaceQuestionMarks <- function(df) {
  # Loop through each column in the data frame
  for (col in colnames(df)) {
    # Identify cells with question marks and replace them with empty spaces
    df[is.na(df[[col]]) | df[[col]] == "?", col] <- " "
  }
  
  # Return the updated data frame
  return(df)
}

replaceQuestionMarks(df[3:9])
df <- na.omit(df)

### Select data from the dates 2007-02-01 and 2007-02-02
df2 <- subset(df, Date == "2007/02/01" | Date == "2007/02/02")

# extract and combine both date and time, separated by space
Date_Time <- strptime(paste(df2$Date, df2$Time, sep=" "), "%Y-%m-%d %H:%M:%S")

# combined df with Date_Time data
df2 <-cbind(df2, Date_Time)

### Plot date and Energy sub metering

# Generate plot3
png("plot3.png", width = 480, height = 480)
plot(df2$Date_Time, df2$Sub_metering_1,
     type = "l", xlab = "Day" , ylab = "Energy sub metering", col = "black")
lines(df2$Date_Time, df2$Sub_metering_2, type = "l", col = "red")
lines(df2$Date_Time, df2$Sub_metering_3, type = "l", col = "blue")
legend(c("topright"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty= 1, lwd=1, col = c("black", "red", "blue"))

dev.off()


