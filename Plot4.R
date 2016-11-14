Plot4 <- function(){
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
    
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
    plot(Date2, Global_active_power, type = "n", xlab = "", ylab = "Global Active Power")
    lines(Date2, Global_active_power)
    plot(Date2, Voltage, type = "n", xlab = "datetime", ylab = "Voltage")
    lines(Date2, Voltage)
    plot(Date2, Sub_Metering_1, type = "n", lty = 1, xlab = "", ylab = "Energy sub metering")
    lines(Date2, Sub_Metering_1, col = "black")
    lines(Date2, Sub_Metering_2, col = "red")
    lines(Date2, Sub_Metering_3, col = "blue")
    legend( "topright", c("Sub_Metering_1", "Sub_Metering_2", "Sub_Metering_3"), lty = c(1, 1, 1), col= c("black", "red", "blue"))
    plot(Date2, Global_reactive_power, type = "n", xlab = "datetime", ylab = "Global_rective_power")
    lines(Date2, Global_reactive_power)
    plot(Date2, Sub_Metering_1, type = "n", lty = 1, ylab = "Energy sub metering")
    lines(Date2, Sub_Metering_1, col = "black")
    lines(Date2, Sub_Metering_2, col = "red")
    lines(Date2, Sub_Metering_3, col = "blue")
    legend( "topright", c("Sub_Metering_1", "Sub_Metering_2", "Sub_Metering_3"), lty = c(1, 1, 1), col= c("black", "red", "blue"))
    
    png(file = "Plot4.png")
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
    plot(Date2, Global_active_power, type = "n", xlab = "", ylab = "Global Active Power")
    lines(Date2, Global_active_power)
    plot(Date2, Voltage, type = "n", xlab = "datetime", ylab = "Voltage")
    lines(Date2, Voltage)
    plot(Date2, Sub_Metering_1, type = "n", lty = 1, xlab = "", ylab = "Energy sub metering")
    lines(Date2, Sub_Metering_1, col = "black")
    lines(Date2, Sub_Metering_2, col = "red")
    lines(Date2, Sub_Metering_3, col = "blue")
    legend( "topright", c("Sub_Metering_1", "Sub_Metering_2", "Sub_Metering_3"), lty = c(1, 1, 1), col= c("black", "red", "blue"))
    plot(Date2, Global_reactive_power, type = "n", xlab = "datetime", ylab = "Global_rective_power")
    lines(Date2, Global_reactive_power)
    dev.off()
}
