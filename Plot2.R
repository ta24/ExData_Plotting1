Plot2 <- function(){
    data1 <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
    date1 <- data1$Date
    count <- 0
    
    for (i in 1: length(date1)) {
        if (date1[i] == "1/2/2007" | date1[i] == "2/2/2007") {
            if (count != 0) {
                count = count + 1
                Date1[count] <- as.Date(data1$Date[i], "%d/%m/%Y")
                Date2[count] <- strptime(paste(data1$Date[i], data1$Time[i]), "%d/%m/%Y %H:%M:%S")
                Global_active_power[count] <- as.numeric(as.character(data1$Global_active_power[i]))
                Global_reactive_power[count] <- as.numeric(as.character(data1$Global_reactive_power[i]))
                Sub_Metering_1[count] <- as.numeric(as.character(data1$Sub_metering_1[i]))
                Sub_Metering_2[count] <- as.numeric(as.character(data1$Sub_metering_2[i]))
                Sub_Metering_3[count] <- as.numeric(as.character(data1$Sub_metering_3[i]))
                Voltage[count] <- as.numeric(as.character(data1$Voltage[i]))
            }
            if (count == 0) {
                count = count + 1
                Date1 <- as.Date(data1$Date[i], "%d/%m/%Y")
                Date2 <- strptime(paste(data1$Date[i], data1$Time[i]), "%d/%m/%Y %H:%M:%S")
                Global_active_power <- as.numeric(as.character(data1$Global_active_power[i]))
                Global_reactive_power <- as.numeric(as.character(data1$Global_reactive_power[i]))
                Sub_Metering_1 <- as.numeric(as.character(data1$Sub_metering_1[i]))
                Sub_Metering_2 <- as.numeric(as.character(data1$Sub_metering_2[i]))
                Sub_Metering_3 <- as.numeric(as.character(data1$Sub_metering_3[i]))
                Voltage <- as.numeric(as.character(data1$Voltage[i]))
            }
        }
    }
    
    plot(Date2, Global_active_power, type = "n", xlab = "", ylab = "Global Active Power (kilowatts)")
    lines(Date2, Global_active_power)
    
    png(file = "Plot2.png")
    plot(Date2, Global_active_power, type = "n", xlab = "", ylab = "Global Active Power (kilowatts)")
    lines(Date2, Global_active_power)
    dev.off()
}
