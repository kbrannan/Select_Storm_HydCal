## Estimate flow for Big Elk Crrek from observed flow from the gage on the Yaquina River.
## I used the drainage area methof modified with average annual precip to estimate the
## stream flow in Big Elk Creek. This is the method used by Cadmus in the previous calibration.

# check environment for existing data to keep
if(length(ls()) > 0) tmp.hold <- paste0("(",ls(),")",collapse="|")

## the drainage area Ar (sqr mi) and Pr average annual precip (inches) for the upper 
## Yaquina River watershed where the gage is. This data was taken from Table 5 in the 
## Cadmus report.
Ar <- 70.8
Pr <- 74

## the drainage area Ar (sqr mi) and Pr average annual precip (inches) for the Big 
## Elk Creek watershed st the most downstream water quality station. This data 
## was taken from Table 5 in the Cadmus report.
Au <- 88.8
Pu <- 84

## create data frame for estimated flows from the data frame of the observed flows
df.flow.est <- df.flow.obs

## estimate the flows for the Big Elk Creek.
df.flow.est$mean_daily_flow_cfs <- round((Au/Ar)*(Pu/Pr)*df.flow.est$mean_daily_flow_cfs, digits=1)

## clean up
if(1*exists("tmp.hold")==0) rm(list=ls()[-grep("df.flow.est",ls())])
if(1*exists("tmp.hold")==1) rm(list=ls()[-grep(paste0("(df.flow.est)|",tmp.hold),ls())])
  