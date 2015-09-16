## script reads in and adds date (in POSIXct format) observed flow data
## for OWRD station 14306030 on the Yaquina River north of Chitwood, OR.
## I am using publised and provisional mean data daily flow data (in cfs)
## for the period of 1995-10-01 to 2014-09-30. I downloaded the data 
## manually from:
## http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/display_hydro_graph.aspx?station_nbr=14306030

# check environment for existing data to keep
if(length(ls()) > 0) tmp.hold <- paste0("(",ls(),")",collapse="|")

chr.flow.dir <- paste0(strsplit(getwd(),split="/R_projs",fixed=TRUE)[[1]][1],
                       "/data")
chr.flow.file <- "Station_14306030_mean_daily_flow.txt"

df.flow.obs <- read.table(file=paste0(chr.flow.dir,"/",chr.flow.file), header=TRUE, sep="\t")

df.flow.obs <-cbind(df.flow.obs,date=strptime(df.flow.obs$record_date,format="%m-%d-%Y"))


# clean up
if(1*exists("tmp.hold")==0) rm(list=ls()[-grep("df.flow.obs",ls())])
if(1*exists("tmp.hold")==1) rm(list=ls()[-grep(paste0("(df.flow.obs)|",tmp.hold),ls())])

