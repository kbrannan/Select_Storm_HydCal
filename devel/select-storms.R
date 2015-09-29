# load libraries
#library(xtable)
#library(gridExtra)

# local functions
source(file=paste0(getwd(),"/devel/functions.R"))

# get precip data
source(file=paste0(getwd(),"/devel/get-precip-data.R"))

# get observed flow data
source(file=paste0(getwd(),"/devel/get-obs-flow-data.R"))

# estimate flow data for Big Elk Cree watershed
source(file=paste0(getwd(),"/devel/estimate-flow.R"))

# set inital pars
# the span sets the number of days used to identify inflextion points in the
# peaks function
spn <- 5 ## span of 5 days to find mins and maxs

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

# get boundaries of potential storms, see function for rules to get boudaries
lst.pot.strm <- get_potential_storm_data(
  spn   =   spn, 
  dates = df.flow.est.clp$date,
  flow  = df.flow.est.clp$mean_daily_flow_cfs)

# get plots of the potential storms
storms_plot_to_file(
       y.b = as.numeric(unique(format(df.flow.est.clp$date, format = "%Y"))),
  pot.strm = lst.pot.strm, 
     dates = df.flow.est.clp$date, 
      flow = df.flow.est.clp$mean_daily_flow, 
    precip = df.daily.precip.max.stations[ , 2], 
  out.file = paste0(getwd(), "/figures/strmPlots.pdf"))

# get storm summary table
df.strm.sum <- storms_to_table(yr.b = NULL, pot.strm = lst.pot.strm)

# clean up
clean_up(prev.kp = keep, new.kp = c("lst.pot.strm","df.strm.sum"))

# only use storms with duration greater than 5 days, this lower limit 
# from HSPFEXP Guidence

# check environment for existing data to keep
keep <- clean_up()

# make copy of previous potential storm-list
lst.pot.strm.5 <- lst.pot.strm

# subset storms for storms with durations greter than or equal to 5 days
df.strm.sum.5 <- lst.pot.strm$pot.strm[df.strm.sum$length.days >= 5, ]
lst.pot.strm.5$pot.strm <- lst.pot.strm$pot.strm[lst.pot.strm$pot.strm$strm.num 
                                                 %in% df.strm.sum.5$strm.num, ]
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

storm_plot_indiviudal_to_file(
  pot.strm = lst.pot.strm.5.single.gt0, 
  dates = df.flow.est.clp$date, 
  flow = df.flow.est.clp$mean_daily_flow, 
  precip = df.daily.precip.max.stations[ , 2],
  out.file = paste0(getwd(),"/figures/strmInvdPlots5singlegt0.pdf"))

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

storm_plot_indiviudal_to_file(
  pot.strm = lst.pot.strm.5.single.gt0.fafp, 
  dates = df.flow.est.clp$date, 
  flow = df.flow.est.clp$mean_daily_flow, 
  precip = df.daily.precip.max.stations[ , 2],
  out.file = paste0(getwd(),"/figures/strmInvdPlots5singlegt0fafp.pdf"))

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

storm_plot_indiviudal_to_file(
  pot.strm = lst.pot.strm.5.single.gt0.fafp.pgt01, 
  dates = df.flow.est.clp$date, 
  flow = df.flow.est.clp$mean_daily_flow, 
  precip = df.daily.precip.max.stations[ , 2],
  out.file = paste0(getwd(),"/figures/strmInvdPlots5singlegt0fafppgt01.pdf"))

# clean up
clean_up(prev.kp = keep, new.kp = "lst.pot.strm.5.single.gt0.fafp.pgt01")

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

# monthly information
rm(df.strm.sum)

df.strm.sum <- storms_to_table(
  yr.b = NULL, z = lst.pot.strm.5.single.gt0.fafp.pgt01)

df.strm.months <- data.frame(
  strm.num  = df.strm.sum$strm.num, 
  month.bgn = format(df.strm.sum$date.bgn,format = "%b"), 
  month.end = format(df.strm.sum$date.end,format = "%b"))

# summer strorms (Months- Jun, Jul, Aug)
df.months.00 <- df.strm.months[df.strm.months$month.bgn %in% 
                                 c("Jun","Jul","Aug"),]

df.months.01 <- df.months.00[df.months.00$month.end %in% 
                               c("Jun","Jul","Aug"), ]

lst.pot.strm.sum <- lst.pot.strm.5.single.gt0.fafp.pgt01

lst.pot.strm.sum$pot.strm <- 
  lst.pot.strm.5.single.gt0.fafp.pgt01$pot.strm[
    lst.pot.strm.5.single.gt0.fafp.pgt01$pot.strm$strm.num %in% 
      df.months.01$strm.num, ]

storms_plot_to_file(
  as.numeric(unique(format(df.flow.est.clp$date,format = "%Y"))),
  lst.pot.strm.sum, df.flow.est.clp$date, 
  df.flow.est.clp$mean_daily_flow, df.daily.precip.max.stations[ , 2], 
  paste0(getwd(), "/figures/strmPlotsSummer.pdf"))

storm_plot_indiviudal_to_file(
  tmp.lst.pot.strm = lst.pot.strm.sum, 
  df.flow.est.clp$date, 
  df.flow.est.clp$mean_daily_flow, 
  df.daily.precip.max.stations[ , 2],
  out.file = paste0(getwd(), "/figures/strmInvdPlotsSummer.pdf"))

rm(df.months.00,df.months.01)

## fall strorms (Months- Sep, Oct, Nov)
df.months.00 <- df.strm.months[df.strm.months$month.bgn %in% 
                                 c("Sep","Oct","Nov"), ]

df.months.01 <- df.months.00[df.months.00$month.end %in% 
                               c("Sep","Oct","Nov"), ]

lst.pot.strm.fal <- lst.pot.strm.5.single.gt0.fafp

lst.pot.strm.fal$pot.strm <- 
  lst.pot.strm.5.single.gt0.fafp$pot.strm[
    lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% 
      df.months.01$strm.num, ]

storms_plot_to_file(
  as.numeric(unique(format(df.flow.est.clp$date,format="%Y"))),
  lst.pot.strm.fal, df.flow.est.clp$date, 
  df.flow.est.clp$mean_daily_flow, df.daily.precip.max.stations[ , 2], 
  paste0(getwd(), "/figures/strmPlotsFall.pdf"))

storm_plot_indiviudal_to_file(
  tmp.lst.pot.strm = lst.pot.strm.fal, 
  df.flow.est.clp$date, 
  df.flow.est.clp$mean_daily_flow, 
  df.daily.precip.max.stations[ , 2],
  out.file = paste0(getwd(),"/figures/strmInvdPlotsFall.pdf"))

rm(df.months.00,df.months.01)

## winter strorms (Months- Dec, Jan, Feb)
df.months.00 <- df.strm.months[df.strm.months$month.bgn %in% 
                                 c("Dec","Jan","Feb"), ]

df.months.01 <- df.months.00[df.months.00$month.end %in% 
                                 c("Dec","Jan","Feb"), ]

lst.pot.strm.win <- lst.pot.strm.5.single.gt0.fafp

lst.pot.strm.win$pot.strm <- 
  lst.pot.strm.5.single.gt0.fafp$pot.strm[
    lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% 
      df.months.01$strm.num, ]

storms_plot_to_file(
  as.numeric(unique(format(df.flow.est.clp$date,format = "%Y"))),
  lst.pot.strm.win, df.flow.est.clp$date, 
  df.flow.est.clp$mean_daily_flow, df.daily.precip.max.stations[ , 2], 
  paste0(getwd(), "/figures/strmPlotsWinter.pdf"))

storm_plot_indiviudal_to_file(
  tmp.lst.pot.strm = lst.pot.strm.win, 
  df.flow.est.clp$date, 
  df.flow.est.clp$mean_daily_flow, 
  df.daily.precip.max.stations[ , 2],
  out.file = paste0(getwd(),"/figures/strmInvdPlotsWinter.pdf"))

rm(df.months.00,df.months.01)

## Spring strorms (Months- Mar, Apr, May)
df.months.00 <- df.strm.months[df.strm.months$month.bgn %in% 
                                 c("Mar","Apr","May"), ]

df.months.01 <- df.months.00[df.months.00$month.end %in% 
                                 c("Mar","Apr","May"), ]

lst.pot.strm.spr <- lst.pot.strm.5.single.gt0.fafp

lst.pot.strm.spr$pot.strm <- 
  lst.pot.strm.5.single.gt0.fafp$pot.strm[
    lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% 
      df.months.01$strm.num, ]

storms_plot_to_file(
  as.numeric(unique(format(df.flow.est.clp$date,format = "%Y"))),
  lst.pot.strm.spr, df.flow.est.clp$date, 
  df.flow.est.clp$mean_daily_flow, df.daily.precip.max.stations[ , 2], 
  paste0(getwd(), "/figures/strmPlotsSring.pdf"))

storm_plot_indiviudal_to_file(
  tmp.lst.pot.strm = lst.pot.strm.spr, 
  df.flow.est.clp$date, 
  df.flow.est.clp$mean_daily_flow, 
  df.daily.precip.max.stations[ , 2],
  out.file = paste0(getwd(),"/figures/strmInvdPlotsSring.pdf"))

# get storms that cross seasons
chr.strm.sea <- c(unique(as.character(lst.pot.strm.sum$pot.strm$strm.num)),
  unique(as.character(lst.pot.strm.fal$pot.strm$strm.num)),
  unique(as.character(lst.pot.strm.win$pot.strm$strm.num)),
  unique(as.character(lst.pot.strm.spr$pot.strm$strm.num)))

chr.strm.sea <- chr.strm.sea[order(as.numeric(chr.strm.sea))]

df.xse <- df.strm.sum$strm.num[as.character(df.strm.sum$strm.num) %in% 
                                  chr.strm.sea == FALSE]

lst.pot.strm.xse <- lst.pot.strm.5.single.gt0.fafp

lst.pot.strm.xse$pot.strm <- 
  lst.pot.strm.5.single.gt0.fafp$pot.strm[
    lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% 
      df.xse, ]

storms_plot_to_file(
  as.numeric(unique(format(df.flow.est.clp$date,format = "%Y"))),
  lst.pot.strm.xse, df.flow.est.clp$date, 
  df.flow.est.clp$mean_daily_flow, df.daily.precip.max.stations[ , 2], 
  paste0(getwd(), "/figures/strmPlotsCrossSeason.pdf"))

storm_plot_indiviudal_to_file(
  tmp.lst.pot.strm = lst.pot.strm.xse, 
  df.flow.est.clp$date, 
  df.flow.est.clp$mean_daily_flow, 
  df.daily.precip.max.stations[ , 2],
  out.file = paste0(getwd(), "/figures/strmInvdPlotsCrossSeason.pdf"))

## final storms
df.strm.summary <- storms_to_table(
  yr.b = NULL, z = lst.pot.strm.5.single.gt0.fafp.pgt01)

df.strm.summary.chr <- data.frame(
  Number = sprintf("%i",df.strm.summary$strm.num),
  Start  = format(df.strm.summary$date.bgn, format = "%Y-%m-%d"),
  End    = format(df.strm.summary$date.end, format = "%Y-%m-%d"),
  Length = sprintf("%i", round(df.strm.summary$length.days, 0)),
  Peak   = sprintf("%i", round(df.strm.summary$peak, 0)),
  Volume = sprintf("%1.2E", df.strm.summary$sum.cuft),
  stringsAsFactors=FALSE)

# print to pretty table
xt <- xtable(df.strm.summary.chr)

print(xt, type="html",file=paste0(getwd(),"/tables/strm_sum.html"))

