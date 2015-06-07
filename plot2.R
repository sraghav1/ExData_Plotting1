library(lubridate)

# read_data accepts name of the data file and vector of string-dates
# Returns records with Date in the 'dates' vector
read_data <- function(file, dates){
        l_dates <- sapply(dates, ymd)  
        df <- read.csv(file, sep=";", stringsAsFactors=F)
        df$Date = dmy(df$Date)
        return(df[df$Date %in% l_dates, ])
}

# Accepts data file name and plots Global active power against datetime
# for the dates 2007-02-01, 2007-02-02
plot2 <- function(file) {
        png(file="plot2.png")
        dates <- c('2007-02-01', '2007-02-02')
        df <- read_data(file, dates)
        draw_plot(df)
        dev.off()
}

draw_plot <- function(df){
        df$DateTime = ymd_hms(paste(df$Date, df$Time))
        df$Global_active_power = as.numeric(df$Global_active_power)
        plot(df$DateTime, df$Global_active_power, type="l", xlab="",
             ylab="Global Active Power (kilowatts)")
}