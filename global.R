library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(lubridate)
library(data.table)
library(xts)
library(shinydashboard)
library(scales)
library(devtools)
library(dygraphs)
library(readxl)
library(rdrop2)
### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0N

read.patsplus<- function(x, tzone="America/Los_Angeles"){
	#confirm file contains data
	fileCheck <- file.info(x)$size>0
	if(fileCheck){
		raw <- read.delim(x)
		kLines <- as.numeric(sapply(raw, function(x) grep('[0-9/0-9/0-9]{2,} [0-9:]{6,},[0-9.,]{3,}',x)))
		rare <- as.character(raw[kLines,])
		fn <- tempfile()
		write(rare, file=fn)
		mediumwell <- fread(fn)
		unlink(fn)
		if(ncol(mediumwell)==12){
			setnames(mediumwell, c('datetime','V_power','degC_sys','degC_air','RH_air','degC_thermistor','usb_pwr','fanSetting','filterSetting','ref_sigDel','low20','high320'))}else{
			setnames(mediumwell, c('datetime','V_power','degC_sys','degC_air','RH_air','degC_CO','mV_CO','status','ref_sigDel','low20','high320'))				
		}
		mediumwell[,datetime:=ymd_hms(datetime, tz=tzone)]
	}else{warning(paste("File", x, "does not contain valid data", sep=" "))}
}

# install missing packages.
list.of.packages <- c("shiny","ggplot2","reshape2","plyr","lubridate","data.table","dygraphs","xts","devtools","shinydashboard","scales",'dygraphs','readxl')
	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))(print(paste("The following packages are not installed: ", new.packages, sep="")))else(print("All packages installed"))
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 

#global functions
alt.diff <- function (x, n = 1, na.pad = TRUE) {
  NAs <- NULL
  if (na.pad) {NAs <- rep(NA, n)}
  diffs <- c(NAs, diff(x, n))
}

round.minutes <- function(x, noOfMinutes=5){
	tz <- tz(x[1])
	class <- class(x[1])
	structure((noOfMinutes*60) * (as.numeric(x + (noOfMinutes*60*0.5)) %/% (noOfMinutes*60)), class=class,tz=tz)
}

db_loadData <- function(){
  # Read all the files into a list
  filesInfo <- drop_dir('filter_room_status')
  filePaths <- filesInfo$path
  filePaths <- grep('tty', filePaths, value=T)
  filePaths <- grep('2016', filePaths, value=T)
  data <- lapply(filePaths, drop_read_patsplus)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

drop_read_patsplus <- function (file, dest = tempdir(), dtoken = readRDS('droptoken.rds'), ...) {
    localfile = paste0(dest, "/", basename(file))
    drop_get(file, local_file = localfile, overwrite = TRUE, 
        dtoken = dtoken)
    read.patsplus(localfile, ...)
}

#create the data
all <- db_loadData()
all <- all[,c('datetime', 'degC_air', 'RH_air'), with=F]

