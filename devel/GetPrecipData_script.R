# load packages
library(doBy)

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

# cleanup in script
rm(list=c(ls(pattern="^chr.prec"),"ii"))
