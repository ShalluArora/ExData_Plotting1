plot1 <- function(){
  
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
  
  ## filter out NA rows
  
  hcpw <- hcpw[complete.cases(hcpw), ]
     
  ## subset to 2 relevant dates
  
  final_hcpw <- hcpw[hcpw$Date %in% c("1/2/2007", "2/2/2007"),]
    
  ## create histogram to screen
  
  hist(final_hcpw$Global_active_power, main = "Global Active Power", xlab = "Global Active Power(kilowatts)", col = "Red")
  
  ## copy the histogram to png device
  
  dev.copy(png, file = "plot1.png", height = 480, width = 480)
  
  ## close the device
  
  dev.off()
}
