plot2 <- function(){
  
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  filename <- "household_power_consumption.txt"
  zip <- "hcpw.zip"
  ## check if file already exists; if not check if zip folder exists, 
  ## if not, download zip file and unzip zip folder
  
  if (file.exists(filename)){
    ## no need to bother
  }
  else{
    if (file.exists("hcpw.zip")){
      ## no need to download zip file
    }
    else{
      download.file(url, zip)
    }
    unzip(zip)  
  }
  
  ## read data from the file
  
  hcpw <- read.table(filename, header = TRUE, sep = ";", na.strings = "?", stringsAsFactors = FALSE)
  
  ## subset to 2 relevant dates
  
  final_hcpw <- hcpw[hcpw$Date %in% c("1/2/2007", "2/2/2007"),]
  
  ## filter out NA rows
  
  final_hcpw <- final_hcpw[complete.cases(final_hcpw), ]
  
  ## convert Date field into type Date
  
  final_hcpw$Date <- as.Date(final_hcpw$Date, "%d/%m/%Y")
  
  ## create a new field called timestamp
  
  timestamp <- paste(final_hcpw$Date, final_hcpw$Time)
  
  ## add to existing data set
  
  final_hcpw <- cbind(final_hcpw,timestamp)
  
  final_hcpw$timestamp <- strptime(timestamp,"%Y-%m-%d %H:%M:%S")
  
  ## plot xyplot
  
  plot(final_hcpw$timestamp, final_hcpw$Global_active_power, 
       type = "l", xlab = " ", ylab = "Global Active Power (kilowatts)")
  
  ## copy the xyplot to png device
  
  dev.copy(png, file = "plot2.png", height = 480, width = 480)
  
  ## close the device
  
  dev.off()
}