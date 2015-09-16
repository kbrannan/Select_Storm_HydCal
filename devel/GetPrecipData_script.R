## script reads in and adds date (in POSIXct format) observed flow data
## for OWRD station 14306030 on the Yaquina River north of Chitwood, OR.
## I am using publised and provisional mean data daily flow data (in cfs)
## for the period of 1995-10-01 to 2014-09-30. I downloaded the data 
## manually from:

# load packages
library(doBy)

# check environment for existing data to keep
if(length(ls(all.names=TRUE)) > 0) tmp.hold <- paste0("(",ls(all.names=TRUE),")",collapse="|")

# set path for the precip data files
chr.prec.dir <- paste0(strsplit(getwd(),split="/R_projs",fixed=TRUE)[[1]][1],
                   "/data")

# get the precip data file names
chr.prec.files <- list.files(chr.prec.dir, pattern="^DSN.*exp$")

# read and process precip files to get a long data frame of daily precip 
for(ii in 1:length(chr.prec.files)) {
  tmp.prec.file <- scan(file=paste0(chr.prec.dir,"/",chr.prec.files[ii]),
                        what="character",sep="\n")
  tmp.prec.file <- tmp.prec.file[(grep("^Date",tmp.prec.file) + 1):
                                   length(tmp.prec.file)]
  tmp.df.precip <- data.frame(
    date=format(as.Date(substr(tmp.prec.file,start=1,stop=11)),fmt="%Y-%m-%d"),
    prec=as.numeric(do.call(rbind, 
                              lapply(tmp.prec.file, 
                                     function(x) substr(x, start=15, 
                                                        stop=nchar(x))
                              )
                            )
                    ),
    src=chr.prec.files[ii], 
    stringsAsFactors=TRUE)
  if(exists("df.daily.precip") == FALSE) {
    df.daily.precip <- summaryBy(.~date+src,data=tmp.df.precip,FUN=sum)
  } else{
    df.daily.precip <- rbind(df.daily.precip,
                             summaryBy(.~date+src,data=tmp.df.precip,FUN=sum)
    )
  }
  
# clean up within loop
  rm(list=ls(pattern="^tmp\\."))
}

# create rename "date" variable to "date_org". The orginal dat is a factor 
# class. I added a new date variable that is a POSIXct class and later functions
# use this class of date 
 
names(df.daily.precip) <- gsub("^date$", "date_org", names(df.daily.precip))
df.daily.precip <- cbind(df.daily.precip,
                         date=as.POSIXct(strptime(df.daily.precip[,1], 
                                                  format="%Y-%m-%d")))
# clean up
if(1*exists("tmp.hold")==0) rm(list=ls(all.names=TRUE)[-grep("df.daily.precip",ls(all.names=TRUE))])
if(1*exists("tmp.hold")==1) rm(list=ls(all.names=TRUE)[-grep(paste0("(df.daily.precip)|",tmp.hold),ls(all.names=TRUE))])
