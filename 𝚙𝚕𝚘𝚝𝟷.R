
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

### Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.

png("plot1.png", width = 480, height = 480)
hist(df2$Global_active_power,
     freq = NULL,
     col = "red",
     border = NULL,
     main = paste("Global_active_power"),
     xlab ="Global_active_power (kilowatts)",
     ylab = "Frequency"
)
dev.off()


