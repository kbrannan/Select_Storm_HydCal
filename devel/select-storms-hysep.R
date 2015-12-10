# local functions
source(file=paste0(getwd(),"/devel/functions.R"))

# get precip data
source(file=paste0(getwd(),"/devel/get-precip-data.R"))

# get observed flow data
source(file=paste0(getwd(),"/devel/get-obs-flow-data.R"))

# estimate flow data for Big Elk Cree watershed
source(file=paste0(getwd(),"/devel/estimate-flow.R"))

# the precip data period is shorter than the flow data, so I used the end of the 
# precip data to clip (shorten) the estimated flow data
dt.max.p <- max(df.daily.precip$date)
df.flow.est.clp <- df.flow.est[df.flow.est$date <= dt.max.p, ]

# use max precip between the two gages (src) for daily precip
df.daily.precip.max.stations <- cbind(summaryBy(prec.sum ~ date_org, 
                                                df.daily.precip, FUN = max), 
                                      date = df.flow.est.clp$date)

# check environment for existing data to keep
keep <- clean_up()

require(DVstats)
# baseflow seperation using USGS-HySep R version
df.hysep88.8 <- hysep(Flow = df.flow.est.clp$mean_daily_flow_cfs, 
                  Dates = as.Date(df.flow.est.clp$date), da = 88.8)

# save hysep results for later use
save(df.hysep88.8, file = "hysep88_8.RData")

# add base flow to flow data frame
df.flow.est.clp <- data.frame(df.flow.est.clp, base.flow = df.hysep88.8$BaseQ)

# calculate runoff as flow - base.flow
df.flow.est.clp <- data.frame(df.flow.est.clp, 
                              runoff = df.flow.est.clp$mean_daily_flow_cfs -
                                df.flow.est.clp$base.flow)

# clean up
clean_up(prev.kp = keep, new.kp = c("df.pot.strm"))

# only use storms with duration greater than 5 days, this lower limit 
# from HSPFEXP Guidence

# check environment for existing data to keep
keep <- clean_up()

# get potential storm-list
## need to added storm summary info to list from get_potential_storm_data_hysep
## function

lst.pot.strm <- get_potential_storm_data_hysep(spn = 5, 
                                                  dates = df.hysep88.8$Dates, 
                                                  flow = df.hysep88.8$Flow,
                                                  baseflow = df.hysep88.8$BaseQ)

# plot result
storms_plot_to_file(
  y.b = as.numeric(unique(format(df.flow.est.clp$date, format = "%Y"))),
  pot.strm = lst.pot.strm, 
  dates = df.flow.est.clp$date, 
  flow = df.flow.est.clp$mean_daily_flow, 
  precip = df.daily.precip.max.stations[ , 2], 
  out.file = paste0(getwd(), "/figures/strmPlots.pdf"))


# subset storms for storms with durations greter than or equal to 5 days
tmp.length <- doBy::summaryBy(date ~ strm.num, data=lst.pot.strm$pot.strm, FUN=length)
tmp.length.5 <- tmp.length[tmp.length$date.length >= 5, "strm.num"]
lst.pot.strm.5 <- lst.pot.strm
lst.pot.strm.5$pot.strm <- lst.pot.strm$pot.strm[lst.pot.strm$pot.strm$strm.num 
                                                 %in% tmp.length.5, ]
# plot result
storms_plot_to_file(
       y.b = as.numeric(unique(format(df.flow.est.clp$date, format = "%Y"))),
  pot.strm = lst.pot.strm.5, 
     dates = df.flow.est.clp$date, 
      flow = df.flow.est.clp$mean_daily_flow, 
    precip = df.daily.precip.max.stations[ , 2], 
  out.file = paste0(getwd(), "/figures/strmPlots5day.pdf"))

# clean up
clean_up(prev.kp = keep, new.kp = "lst.pot.strm.5")

# only use storms with a single peak and exclude storms with multiple peaks

# check environment for existing data to keep
keep <- clean_up()

# count the peaks within eacyh storm
df.peak.counts <- summaryBy(flow ~ strm.num, data = lst.pot.strm.5$pot.strm, 
                            FUN = storm_count_peaks)

# keep storms with only one peak
keep.strm.nums <- as.numeric(df.peak.counts[df.peak.counts[ , 2] == 1,
                                            "strm.num"])

# subset the potential storm data.frame in the previous storm-list
df.pot.strm.5.single <- lst.pot.strm.5$pot.strm[lst.pot.strm.5$pot.strm$strm.num
                                                %in% keep.strm.nums, ]

# make copy of previous storm-list
lst.pot.strm.5.single <- lst.pot.strm.5

# create new storm-list using the data.frame of storms that have only one peak
lst.pot.strm.5.single$pot.strm <- df.pot.strm.5.single

# plot results
storms_plot_to_file(
       y.b = as.numeric(unique(format(df.flow.est.clp$date, format = "%Y"))),
  pot.strm = lst.pot.strm.5.single, 
     dates = df.flow.est.clp$date, 
      flow = df.flow.est.clp$mean_daily_flow, 
    precip = df.daily.precip.max.stations[ , 2], 
  out.file = paste0(getwd(), "/figures/strmPlots5daySingle.pdf"))

# clean up
clean_up(prev.kp = keep, new.kp = "lst.pot.strm.5.single")

# only use storms with precip > 0

# check environment for existing data to keep
keep <- clean_up()

# create data.frame with precip add to storms
df.pot.00 <- merge(x = lst.pot.strm.5.single$pot.strm, 
                   y = df.daily.precip.max.stations, 
                   by = "date")

# summarize the precip for each storm
df.pot.00.sum <- summaryBy(prec.sum.max ~ strm.num, data = df.pot.00, 
                           FUN = sum)

# select storms with precip greater than 0
keep.strm.nums <- as.numeric(
  df.pot.00.sum$strm.num[df.pot.00.sum$prec.sum.max > 0])

# make copy of previous storm-list
lst.pot.strm.5.single.gt0 <- lst.pot.strm.5.single

# create new storm-list using the data.frame of storms with precip greater 
# than 0
lst.pot.strm.5.single.gt0$pot.strm <- 
  lst.pot.strm.5.single$pot.strm[as.numeric(
    lst.pot.strm.5.single$pot.strm$strm.num) %in% 
      keep.strm.nums, ]

# plot results
storms_plot_to_file(
       y.b = as.numeric(unique(format(df.flow.est.clp$date, format = "%Y"))),
  pot.strm = lst.pot.strm.5.single.gt0, 
     dates = df.flow.est.clp$date, 
      flow = df.flow.est.clp$mean_daily_flow, 
    precip = df.daily.precip.max.stations[ , 2], 
  out.file = paste0(getwd(), "/figures/strmPlots5daySinglegt0.pdf"))

# clean up
clean_up(prev.kp = keep, new.kp = "lst.pot.strm.5.single")

## select storms where max precip timing occurs before peak flow

# check environment for existing data to keep
keep <- clean_up()

# create data.frame with precip add to storms
df.pot.00 <- merge(x = lst.pot.strm.5.single.gt0$pot.strm, 
                   y = df.daily.precip.max.stations, by = "date")

# find the max precip for each storm
df.pot.00.p.max <- summaryBy(prec.sum.max ~ strm.num, data = df.pot.00, 
                             FUN = max)

# find the max flow for each storm
df.pot.00.f.max <- summaryBy(flow ~ strm.num, data = df.pot.00, FUN = max)

# create data.frame for the results
junk <- data.frame(strm.num = unique(df.pot.00$strm.num), 
                     date.p = as.POSIXct("1967-07-02"), 
                     date.f = as.POSIXct("1967-07-02"), diff=0)

# calculate the time difference between the max precip and the max flow 
# for each storm
for( ii in junk$strm.num) {

# get info for storm ii
  tmp.d <- df.pot.00[df.pot.00$strm.num == ii, ]

# get dates of max precip and max flow for storm ii
  junk$date.p[junk$strm.num == ii] <- min(tmp.d$date[tmp.d$p == max(tmp.d$p)])
  junk$date.f[junk$strm.num == ii] <- 
    min(tmp.d$date[tmp.d$flow == max(tmp.d$flow)])

# calaculate difference between the times of the max precip and max flow
  junk$diff[junk$strm.num == ii] <- junk$date.p[junk$strm.num == ii] - 
    junk$date.f[junk$strm.num == ii]
}

# select storms where the max flow occurred on the same day or after max precip
keep.strm.nums <- as.numeric(junk$strm.num[junk$diff <= 0])
df.pot.01 <- df.pot.00[df.pot.00$strm.num %in% keep.strm.nums, ]

# make copy of previous storm-list
lst.pot.strm.5.single.gt0.fafp <- lst.pot.strm.5.single.gt0

# create new storm-list using the data.frame of storms where the max flow 
# occurred on the same day or after max precip 
lst.pot.strm.5.single.gt0.fafp$pot.strm <- df.pot.01

# plot results
storms_plot_to_file(
  y.b = as.numeric(unique(format(df.flow.est.clp$date, format = "%Y"))),
  pot.strm = lst.pot.strm.5.single.gt0.fafp, 
  dates = df.flow.est.clp$date, 
  flow = df.flow.est.clp$mean_daily_flow, 
  precip = df.daily.precip.max.stations[ , 2], 
  out.file = paste0(getwd(), "/figures/strmPlots5daySinglegt0fafp.pdf"))

# clean up
clean_up(prev.kp = keep, new.kp = "lst.pot.strm.5.single.gt0.fafp")

# keep storms with precip >= 0.1 inch

# check environment for existing data to keep
keep <- clean_up()

# summarize total precip for storms
df.pot.00 <- summaryBy(prec.sum.max ~ strm.num, 
                       data=lst.pot.strm.5.single.gt0.fafp$pot.strm, FUN = sum)

# select storms with total precip greater than or equal 0.1 inch
keep.strm.nums <- as.numeric(df.pot.00$strm.num[df.pot.00$prec.sum.max >= 0.1])

# make a copy of the previous storm-list
lst.pot.strm.5.single.gt0.fafp.pgt01 <- lst.pot.strm.5.single.gt0.fafp

# create new storm-list using the data.frame of storms where total precip is 
# greater than or equal to 0.1 inch
lst.pot.strm.5.single.gt0.fafp.pgt01$pot.strm <- lst.pot.strm.5.single.gt0.fafp$
  pot.strm[lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% 
             keep.strm.nums, ]

# plot results
storms_plot_to_file(
  y.b = as.numeric(unique(format(df.flow.est.clp$date, format = "%Y"))),
  pot.strm = lst.pot.strm.5.single.gt0.fafp.pgt01, 
  dates = df.flow.est.clp$date, 
  flow = df.flow.est.clp$mean_daily_flow, 
  precip = df.daily.precip.max.stations[ , 2], 
  out.file = paste0(getwd(), "/figures/strmPlots5daySinglegt0fafppgt01.pdf"))

# clean up
clean_up(prev.kp = keep, new.kp = "lst.pot.strm.5.single.gt0.fafp.pgt01")

# keep storms where begin flow greater than end flow

# check environment for existing data to keep
keep <- clean_up()

# make a copy of the previous storm-list
lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte <- 
  lst.pot.strm.5.single.gt0.fafp.pgt01

# get storm data
df.tmp <- lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte$pot.strm

# functions to get first and last elements in a vector
get_first <- function(x) x[1]

get_last <- function(x) x[length(x)]

# get the begin and end flow for each storm
df.pot.00 <- summaryBy(flow ~ strm.num, 
                       data=df.tmp, 
                       FUN = c(get_first, get_last))
names(df.pot.00) <- c("strm.num", "f.bg", "f.ed")

# select storms with total precip greater than or equal 0.1 inch
keep.strm.nums <- as.numeric(df.pot.00$strm.num[df.pot.00$f.bg >= 
                                                  df.pot.00$f.ed])

# create new storm-list using the data.frame of storms where total precip is 
# greater than or equal to 0.1 inch
lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte$pot.strm <- 
  lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte$pot.strm[
    lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte$pot.strm$strm.num %in% 
             keep.strm.nums, ]

# plot results
storms_plot_to_file(
  y.b = as.numeric(unique(format(df.flow.est.clp$date, format = "%Y"))),
  pot.strm = lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte, 
  dates = df.flow.est.clp$date, 
  flow = df.flow.est.clp$mean_daily_flow, 
  precip = df.daily.precip.max.stations[ , 2], 
  out.file = paste0(getwd(), 
                    "/figures/strmPlots5daySinglegt0fafppgt01fbgte.pdf"))


# keep storms with ration of runoff to precip <= 1

# check environment for existing data to keep
keep <- clean_up()

# make a copy of the previous storm-list
lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1 <- 
  lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte

# get storm data
df.tmp <- lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1$pot.strm


# summarize flow for storms
df.strm.flow <- storms_with_SRO_to_table(
  yr.b = NULL, pot.strm = lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1)
#names(df.strm.flow) <- c(names(df.strm.flow)[1:3], "f.length", "f.peak", 
#                        "f.total.cuft")

# summarize precip for storms
df.strm.precip <- summaryBy(prec.sum.max ~ strm.num, 
                            data=lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1$pot.strm, 
                            FUN = c(length,max,mean,sum))
names(df.strm.precip) <- c("strm.num","p.length","p.max","p.mean","p.total")

# combine flow and precip summaries
df.strm.sum <- merge(x = df.strm.flow, y = df.strm.precip, suffixes = FALSE)

df.tmp <- df.strm.sum

cf.fact <- 88.8 * 27878400 / 12
df.tmp <- data.frame(df.tmp, 
                          f.p.ratio = round(df.tmp$sum.cuft.SRO / 
                                              (df.tmp$p.total * cf.fact),
                                            digits = 2))
keep.storms <- as.numeric(df.tmp$strm.num[df.tmp$f.p.ratio <= 1])

lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1$pot.strm <- 
  lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1$pot.strm[
    lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1$pot.strm$strm.num %in%
      keep.storms, 
  ]

# plot results
storms_plot_to_file(
  y.b = as.numeric(unique(format(df.flow.est.clp$date, format = "%Y"))),
  pot.strm = lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1, 
  dates = df.flow.est.clp$date, 
  flow = df.flow.est.clp$mean_daily_flow, 
  precip = df.daily.precip.max.stations[ , 2], 
  out.file = paste0(getwd(), 
                    "/figures/strmPlots5daySinglegt0fafppgt01fbgterorle1.pdf"))


storm_plot_indiviudal_to_file(
  pot.strm = lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1,
  dates  = df.flow.est.clp$date,
  flow   = df.flow.est.clp$mean_daily_flow,
  precip = df.daily.precip.max.stations[ , 2],
  out.file  = paste0(getwd(),
                     "/figures/indstrmPlots5daySinglegt0fafppgt01fbgterorle1.pdf"))

# clean up
clean_up(prev.kp = keep, new.kp = 
           "lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1")
  

# a few storms had storm flow that seemed to be above the stream flow during
# the falling limb of the hydrograph
# Storms to check are strm.num 16, 80, 187, 216 and 428
# check environment for existing data to keep
keep <- clean_up()

df.tmp <- lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1$pot.strm

tmp.strm.nums <- c(16, 80, 187, 216, 428)

df.strm.flows.chk <- data.frame(strm.num = tmp.strm.nums,
                                date.chk ="", flow.chk ="",
                                stringsAsFactors = FALSE)
for(ii in 1:length(df.strm.flows.chk$strm.num)) {
  
  tmp.strm.num <- df.strm.flows.chk$strm.num[ii]
  
  #get storm date
  df.tmp.strm <- df.tmp[ df.tmp$strm.num == tmp.strm.num, ]
  tmp.bg.d <- df.tmp.strm$date[1]
  tmp.ed.d <- df.tmp.strm$date[length(df.tmp.strm[ ,1])]
  
  # get flow data
  df.tmp.flow <- df.flow.est.clp[df.flow.est.clp$date >= tmp.bg.d & 
                                   df.flow.est.clp$date <= tmp.ed.d, ]
  names(df.tmp.flow) <- gsub("^date$","fl.date", names(df.tmp.flow))
  
  # make temp df of storm and flow data
  junk <- cbind(df.tmp.strm[ , c("date", "flow")], 
                df.tmp.flow[ , c("fl.date", "mean_daily_flow_cfs")])
  # check dates
  if(sum((junk$date - junk$fl.date) == 0) == length(junk[ , 1])) {
    df.strm.flows.chk$date.chk[ii] <- "yes"
  } else {
    df.strm.flows.chk$date.chk[ii] <- "no"
  }
  
  # check flows
  if(sum((junk$flow - junk$mean_daily_flow_cfs) == 0) == length(junk[ , 1])) {
    df.strm.flows.chk$flow.chk[ii] <- "yes"
  } else {
    df.strm.flows.chk$flow.chk[ii] <- "no"
  }
}

# The flows for the storms and the orginal flow time-series match. The apparent
# storm flow (yellow polygon) portion above the flow time-series line in the 
# plots of the individual storm may be an artifact of the graphics processing
# of the plot command to a pdf file or something

# clean up
clean_up(prev.kp = keep, new.kp = "df.strm.flows.chk")

# check storms by seasons
# seasons as defined by HSPEXP
# Summer Jun-Aug
# Winter Dec-Feb
# remaing seasons defined by me
# Fall Sep-Nov
# Spring Mar-May
# I am excluding storms that occur accross the boundaries of the seasons for 
# this part of the investigation, but I will include these storms in the 
# selection process later

# monthly, season and year facors added information

# check environment for existing data to keep
keep <- clean_up()

# current best storm set
pot.strm.cur <- lst.pot.strm.5.single.gt0.fafp.pgt01.fbgte.rorle1

# summarize flow for storms
df.strm.flow <- storms_with_SRO_to_table(
  yr.b = NULL, pot.strm = pot.strm.cur)

# summarize precip for storms
df.strm.precip <- summaryBy(prec.sum.max ~ strm.num, 
                       data=lst.pot.strm.5.single.gt0.fafp.pgt01$pot.strm, 
                       FUN = c(length,max,mean,sum))
names(df.strm.precip) <- c("strm.num","p.length","p.max","p.mean","p.total")

# combine flow and precip summaries
df.strm.sum <- merge(x = df.strm.flow, y = df.strm.precip, suffixes = FALSE)

# clean up
clean_up(prev.kp = keep, new.kp = c("pot.strm.cur","df.strm.sum"))

# check environment for existing data to keep
keep <- clean_up()

# get seasons for storms
df.strm.months <- data.frame(
  strm.num  = df.strm.sum$strm.num, 
  month.bgn = format(df.strm.sum$date.bgn,format = "%b"), 
  month.end = format(df.strm.sum$date.end,format = "%b"),
  season = NA)

# use for-loop to get seasons for each storm
for(ii in 1:length(df.strm.months$strm.num)) {
  df.strm.months$season[ii] <- get_season(
    bgn = df.strm.months$month.bgn[ii], end = df.strm.months$month.end[ii])
}

# add season information to the storm data.frame
df.strm.sum <- merge(x = df.strm.sum, y = df.strm.months, suffixes = FALSE)

# make season a factor to allow for summaries later
df.strm.sum$season <- factor(df.strm.sum$season)

# add year to the storm data.frame
df.strm.sum <- data.frame(df.strm.sum, 
                          year = format(df.strm.sum$date.bgn, format = "%Y"))

# add flow-rainfall ratio to storm data.frame 
# need to convert cu.ft to wtsd-inches
# coversion of precip-inches to precip-cuft. Watershed area 88.8 sqr miles
cf.fact <- 88.8 * 27878400 / 12
df.strm.sum <- data.frame(df.strm.sum, 
                    f.p.ratio = round(df.strm.sum$sum.cuft.SRO / 
                                        (df.strm.sum$p.total * cf.fact),
                                         digits = 2))

df.strm.sum$season <- factor(df.strm.sum$season, levels = c("fall", "winter", 
                                                            "spring", "summer",
                                                            "cross"))

# clean up
clean_up(prev.kp = keep, new.kp = c("df.strm.sum"))

# summarize by season
df.sum.season <- summaryBy(length.days + peak.tfl + sum.cuft.tfl +
                          peak.SRO + sum.cuft.SRO + f.p.ratio ~ season, 
                          data = df.strm.sum, 
                          FUN = c(max, median, mean, min, length))
# plots
library(ggplot2)

# open pdf file for output
pdf(file=paste0(getwd(), "/figures/barplots of stormstats.pdf")
    , width = 11, height = 8.5, onefile = TRUE)

# length in days by season
plt.len.days <- ggplot(data = df.strm.sum, aes(season, length.days)) +
  geom_boxplot() + geom_point(position = position_jitter(w = 0.1, h = 0.1))
plt.len.days <- plt.len.days + xlab("Season") + ylab("Storm Duration (days)")
plt.len.days <- plt.len.days + geom_text(data = df.sum.season, 
                                         aes(label = length.days.length, 
                                             x = season, y = 20))
plot(plt.len.days)

# peak flow by season
plt.peak.flow <- ggplot(data = df.strm.sum, aes(season, peak.tfl)) +
  geom_boxplot() + geom_point(position = position_jitter(w = 0.1, h = 0.1)) +
  scale_y_log10()
plt.peak.flow <- plt.peak.flow + xlab("Season") + ylab("Storm Peak Flow (cfs)")
plt.peak.flow <- plt.peak.flow + geom_text(data = df.sum.season, 
                                         aes(label = peak.tfl.length, 
                                             x = season, y = 5000))
plot(plt.peak.flow)

# flow volume by season
plt.vol.flow <- ggplot(data = df.strm.sum, aes(season, sum.cuft.tfl)) +
  geom_boxplot() + geom_point(position = position_jitter(w = 0.1, h = 0.1)) +
  scale_y_log10()
plt.vol.flow <- plt.vol.flow + xlab("Season") + ylab("Storm Flow Volume (cu. ft.)")
plt.vol.flow <- plt.vol.flow + geom_text(data = df.sum.season, 
                                           aes(label = sum.cuft.tfl.length, 
                                               x = season, y = 1e+10))
plot(plt.vol.flow)

# flow to precip ratio by season
plt.fpr.flow <- ggplot(data = df.strm.sum, aes(season, f.p.ratio)) +
  geom_boxplot() + geom_point(position = position_jitter(w = 0.1, h = 0.1))
plt.fpr.flow <- plt.fpr.flow + xlab("Season") + ylab("Runoff to Precip Ratio")
plt.fpr.flow <- plt.fpr.flow + geom_text(data = df.sum.season, 
                                         aes(label = sum.cuft.tfl.length, 
                                             x = season, y = 1.1))
plot(plt.fpr.flow)

# close file done
dev.off()

# save results for devloping PEST input
image.list <- c("pot.strm.cur", "df.strm.sum", "df.sum.season")
save(list = image.list, 
     file = paste0(getwd(), "/data/storm-data-for-PEST.RData"))
