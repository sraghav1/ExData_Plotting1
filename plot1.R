library(lubridate)

# read_data accepts name of the data file and vector of string-dates
# Returns records with Date in the 'dates' vector
read_data <- function(file, dates){
        l_dates <- sapply(dates, ymd)        
        df <- read.csv(file, sep=";", stringsAsFactors=F)
        return(df[dmy(df$Date) %in% l_dates, ])
}

# Accepts data file name and plots the histogram of Global active power
# for the dates 2007-02-01, 2007-02-02
plot1 <- function(file) {
        png(file="plot1.png")
        dates <- c('2007-02-01', '2007-02-02')
        df <- read_data(file, dates)
        df$Global_active_power = as.numeric(df$Global_active_power)
        hist(df$Global_active_power, col="red", 
             xlab="Global Active Power (kilowatts)", main="Global Active Power")
        dev.off()
}