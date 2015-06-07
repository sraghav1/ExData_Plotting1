library(lubridate)

# read_data accepts name of the data file and vector of string-dates
# Returns records with Date in the 'dates' vector
read_data <- function(file, dates){
        l_dates <- sapply(dates, ymd)  
        df <- read.csv(file, sep=";", stringsAsFactors=F)
        df$Date = dmy(df$Date)
        return(df[df$Date %in% l_dates, ])
}

# Accepts data file name and multiple characteristics against datetime
# for the dates 2007-02-01, 2007-02-02
plot4 <- function(file) {
        png(file="plot4.png")
        dates <- c('2007-02-01', '2007-02-02')
        df <- read_data(file, dates)
        draw_plot(df)
        dev.off()
}

draw_plot <- function(df){
        par(mfrow = c(2,2), cex=0.66)
        with(df, {
                DateTime = ymd_hms(paste(Date, Time))
                plot(DateTime, Global_active_power, type="l", xlab="",
                     ylab="Global Active Power")                                
                plot(DateTime, Voltage, type="l", xlab="",
                     ylab="Voltage")                                
                plot(DateTime, Sub_metering_1, type="l", xlab="",
                     ylab="Energy sub metering", col="gray")
                lines(DateTime, Sub_metering_2, col="red")
                lines(DateTime, Sub_metering_3, col="blue")
                legend("topright", lwd=1, col=c("gray", "red", "blue"), 
                       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                       bty="n")
                plot(DateTime, Global_reactive_power, type="l", xlab="")                
        })
}