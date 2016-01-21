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

chr.obs.data <- scan(file = paste0(chr.flow.dir, "/", chr.flow.file),
                     what = "caharacter", sep = "\n")

lil.junk <- head(chr.obs.data,100)

chr.obs.data <- read.fwf(file = paste0(chr.flow.dir, "/", chr.flow.file), 
                 widths = c(31,24,26), skip = 15, stringsAsFactors = FALSE)

chr.obs.data <- chr.obs.data[ , -3]

names(chr.obs.data) <- c("date", "flow")

junk <- chr.obs.data

strptime(junk$date[1:5], format = "%m/%d/%Y")
junk$date <- as.POSIXct(junk$date, format = "%m/%d/%Y")
junk$flow <- as.numeric(junk$flow)

lng.NAs <- is.na(junk$flow)

## variable names in row 14
chr.var <- do.call(c, 
                   strsplit(
                     gsub(" {1, }$","", 
                          gsub("^ {1, }", "", chr.obs.data[14])),
                     split = " {2, }"))


# get data Problem: some of the rows will have three columns instead of two if
# there is a flag for estimated data
# try using fixed format to split string

chr.data <- 
  do.call(rbind,
          strsplit(
            gsub(" {1, }$","", 
                 gsub("^ {1, }","", 
                      chr.obs.data[16:length(chr.obs.data)]
                      )
                 ), 
            split = " {2, }")
          )

chr.data <- 
  do.call(rbind,
          strsplit(
            gsub(" {1, }$","", 
                 gsub("^ {1, }","", 
                      lil.junk[16:length(lil.junk)]
                 )
            ), 
            split = " {2, }")
  )


df.flow.obs <- read.table(file=paste0(chr.flow.dir,"/",chr.flow.file), 
                          header = TRUE, sep="\t")

df.flow.obs <-cbind(df.flow.obs,
                    date = strptime(df.flow.obs$record_date, 
                                    format = "%m-%d-%Y"))


# clean up
if (1*exists("dont.del") == 0) 
  rm(list = ls(all.names = TRUE)[-grep("df.flow.obs",ls(all.names = TRUE))])
if (1*exists("dont.del") == 1) 
  rm(list = ls(all.names = TRUE)[-grep(paste0("(df.flow.obs)|",dont.del),
                                       ls(all.names = TRUE))])
