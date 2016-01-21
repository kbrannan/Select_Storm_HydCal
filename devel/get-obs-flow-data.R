## script reads in and adds date (in POSIXct format) observed flow data
## for OWRD station 14306030 on the Yaquina River north of Chitwood, OR.
## I am using publised mean data daily flow data (in cfs)
## for the period of 1995-10-01 to 2015-09-30. I downloaded the data 
## manually in 2016-01-20 from:
## http://apps.wrd.state.or.us/apps/sw/hydro_report/gage_data_request.aspx?station_nbr=14306030
## downloaded data as "Formatted Text", which is a fixed format using multiple 
## spaces to maintain columns

# check environment for existing data to keep
if(length(ls()) > 0) dont.del <- paste0("(",ls(all.names=TRUE),")",collapse="|")

## explcitly set path for location of raw data 
chr.flow.dir <- "M:/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/data"
##chr.flow.dir <- paste0(strsplit(getwd(),split="/R_projs",fixed=TRUE)[[1]][1],
##                       "/data")

## default file name changed
##chr.flow.file <- "Station_14306030_mean_daily_flow.txt"
chr.flow.file <- "station_14306030_mdfDaily_time_series.txt"

## read as fixed format file exclude header info (skip=15)
chr.obs.data <- read.fwf(file = paste0(chr.flow.dir, "/", chr.flow.file), 
                 widths = c(31,24,26), skip = 15, stringsAsFactors = FALSE)

## exclude "Estimated" column
chr.obs.data <- chr.obs.data[ , -3]

## name variables
names(chr.obs.data) <- c("date", "mean_daily_flow_cfs")

## exclude last line which is all "-"
chr.obs.data <- chr.obs.data[-1 * length(chr.obs.data$flow), ]

## convert character to POSIXct for dates
chr.obs.data$date <- as.POSIXct(chr.obs.data$date, format = "%m/%d/%Y")

## convert flows to numeric
chr.obs.data$mean_daily_flow_cfs <- as.numeric(chr.obs.data$flow)

## data.frame to return
df.flow.obs <- chr.obs.data

# clean up
if (1*exists("dont.del") == 0) 
  rm(list = ls(all.names = TRUE)[-grep("df.flow.obs",ls(all.names = TRUE))])
if (1*exists("dont.del") == 1) 
  rm(list = ls(all.names = TRUE)[-grep(paste0("(df.flow.obs)|",dont.del),
                                       ls(all.names = TRUE))])
