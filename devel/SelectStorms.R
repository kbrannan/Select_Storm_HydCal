
# set inital pars

# the span sets the number of days used to identify inflextion points in the
# peaks function
spn <- 5 ## span of 5 days to find mins and maxs

# the precip data period is shorter than the flow data, so I used the end of the 
# precip data to clip (shorten) the estimated flow data
dt.max.p <- max(df.daily.precip$date)
df.flow.est.clp <- df.flow.est[df.flow.est$date <= dt.max.p, ]

# use max precip between the two gages (src) for daily precip
df.daily.precip.max.stations <- summaryBy(prec.sum ~ date_org, df.daily.precip,FUN=max)

## get boundaries of potential storms, see function for rules to get boudaries
lst.pot.strm <- getPotentialStormData(spn=spn, dates=df.flow.est.clp$date,
                                      flow=df.flow.est.clp$mean_daily_flow_cfs)





## get plots of the potential storms
plotToFile(as.numeric(unique(format(df.flow.est.clp$date,format="%Y"))),
           lst.pot.strm, df.flow.est.clp$date, df.flow.est.clp$mean_daily_flow, 
           df.daily.precip.max.stations[ , 2], 
           paste0(getwd(),"/figures/strmPlots.pdf"))













## get storm summary table
df.strm.sum <- table.me(yr.b=NULL, z=lst.pot.strm)

## only use storms with duration greater than 5 days, this lower limit from HSPFEXP Guidence
lst.pot.strm.5 <- lst.pot.strm
df.strm.sum.5 <- lst.pot.strm$pot.strm[df.strm.sum$length.days >= 5, ]
lst.pot.strm.5$pot.strm <- lst.pot.strm$pot.strm[lst.pot.strm$pot.strm$strm.num 
                                                 %in% df.strm.sum.5$strm.num, ]
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))),
           lst.pot.strm.5,df.est.clp$date, df.est.clp$flow, 
           df.daily.precip.max.stations[ , 2], 
           paste0(getwd(), "/figures/strmPlots5day.pdf"))

plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))),
           lst.pot.strm.5,df.est.clp, df.daily.precip, 
           paste0(getwd(), "/figures/strmPlots5day.pdf"))



## check for multuiple peaks with storms and exclude sorms with multiple peaks 
countPeak <- function(flow) sum(peaks(flow, span=3)*1)
df.peak.counts <- summaryBy(flow ~ strm.num, data=lst.pot.strm.5$pot.strm, 
                            FUN=countPeak)
keep.strm.nums <- as.numeric(df.peak.counts[df.peak.counts$flow.countPeak == 1,
                                            "strm.num"])
df.pot.strm.5.single <- lst.pot.strm.5$pot.strm[lst.pot.strm.5$pot.strm$strm.num
                                                %in% keep.strm.nums, ]
lst.pot.strm.5.single <- lst.pot.strm.5
lst.pot.strm.5.single$pot.strm <- df.pot.strm.5.single
plotToFile(as.numeric(unique(format(df.est.clp$date, format="%Y"))),
           lst.pot.strm.5.single, df.est.clp, df.daily.precip,
           paste0(getwd(),"/figures/strmPlots5Single.pdf"))

## only use storms with precip > 0
df.pot.00 <- merge(x=lst.pot.strm.5.single$pot.strm, y=df.daily.precip, 
                   by="date")
df.pot.01 <- data.frame(df.pot.00, p=pmax(df.pot.00$prec11, df.pot.00$prec31))
df.pot.01.sum <- summaryBy(p ~ strm.num, data=df.pot.01, FUN=sum)
keep.strm.nums <- as.numeric(df.pot.01.sum$strm.num[df.pot.01.sum$p.sum > 0])
lst.pot.strm.5.single.gt0 <- lst.pot.strm.5.single
lst.pot.strm.5.single.gt0$pot.strm <- 
  lst.pot.strm.5.single$pot.strm[
    as.numeric(lst.pot.strm.5.single$pot.strm$strm.num) %in% keep.strm.nums, ]

plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))),
           lst.pot.strm.5.single.gt0,df.est.clp,df.daily.precip,
           paste0(getwd(),"/figures/strmPlots5Singlegt0.pdf"))
rm(list=ls(pattern="df.pot"))

## check max precip timing relative to peak flow
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.5.single.gt0, 
               df.est=df.est. clp,df.daily.precip=df.daily.precip,
               out.file=paste0(getwd(),"/figures/strmInvdPlots5singlegt0.pdf"))
df.pot.00 <- merge(x=lst.pot.strm.5.single.gt0$pot.strm, 
                   y=df.daily.precip, by="date")
df.pot.01 <- data.frame(df.pot.00, p=pmax(df.pot.00$prec11, df.pot.00$prec31))
dtpeak <- function(x.date, x.val) x.date[x.val == max(x.val)]
df.pot.00.p.max <- summaryBy(p ~ strm.num, data=df.pot.01, FUN=max)
df.pot.00.f.max <- summaryBy(flow ~ strm.num, data=df.pot.01, FUN=max)
junk <- data.frame(strm.num=unique(df.pot.00$strm.num), 
                   date.p=as.Date("1967-07-02"), date.f=as.Date("1967-07-02"), 
                   diff=0)
for( ii in junk$strm.num) {
  tmp.d <- df.pot.01[df.pot.01$strm.num == ii, ]
  junk$date.p[junk$strm.num == ii] <- tmp.d$date[tmp.d$p == max(tmp.d$p)]
  junk$date.f[junk$strm.num == ii] <- tmp.d$date[tmp.d$flow == max(tmp.d$flow)]
  junk$diff[junk$strm.num == ii] <- tmp.d$date[tmp.d$p == max(tmp.d$p)] - 
    tmp.d$date[tmp.d$flow == max(tmp.d$flow)]
}
keep.strm.nums <- as.numeric(junk$strm.num[junk$diff <= 0])
df.pot.02 <- df.pot.01[df.pot.01$strm.num %in% keep.strm.nums, ]
lst.pot.strm.5.single.gt0.fafp <- lst.pot.strm.5.single.gt0
lst.pot.strm.5.single.gt0.fafp$pot.strm <- df.pot.02
plotToFile(as.numeric(unique(format(df.est.clp$date, format="%Y"))), 
           lst.pot.strm.5.single.gt0.fafp, df.est.clp,df.daily.precip,
           paste0(getwd(), "/figures/strmPlots5Singlegt0fafp.pdf"))
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.5.single.gt0.fafp, 
               df.est=df.est.clp, df.daily.precip=df.daily.precip,
               out.file=paste0(getwd(), 
                               "/figures/strmInvdPlots5singlegt0fafp.pdf"))

## keep storms with precip >= 0.1 inch
df.pot.00 <- summaryBy(p ~ strm.num, 
                       data=lst.pot.strm.5.single.gt0.fafp$pot.strm, FUN=sum)
keep.strm.nums <- as.numeric(df.pot.00$strm.num[df.pot.00$p >= 0.1])
lst.pot.strm.5.single.gt0.fafp.pgt01 <- lst.pot.strm.5.single.gt0.fafp
lst.pot.strm.5.single.gt0.fafp.pgt01$pot.strm <- lst.pot.strm.5.single.gt0.fafp$
  pot.strm[lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% 
             keep.strm.nums, ]
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))),
           lst.pot.strm.5.single.gt0.fafp.pgt01, df.est.clp,df.daily.precip, 
           paste0(getwd(),"/figures/strmPlots5Singlegt0fafppgt01.pdf"))
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.5.single.gt0.fafp.pgt01, 
               df.est=df.est.clp, df.daily.precip=df.daily.precip,
               out.file=paste0(getwd(), 
                               "/figures/strmInvdPlots5singlegt0fafppgt01.pdf"))


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
df.strm.sum <- table.me(yr.b=NULL, z=lst.pot.strm.5.single.gt0.fafp)
df.strm.months <- data.frame(strm.num=df.strm.sum$strm.num, 
                             month.bgn=format(df.strm.sum$date.bgn,format="%b"), 
                             month.end=format(df.strm.sum$date.end,format="%b"))
## summer strorms (Months- Jun, Jul, Aug)
df.months.00 <- df.strm.months[df.strm.months$month.bgn %in% 
                                 c("Jun","Jul","Aug"),]
df.months.01 <- df.months.00[df.months.00$month.end %in% c("Jun","Jul","Aug"), ]
lst.pot.strm.sum <- lst.pot.strm.5.single.gt0.fafp
lst.pot.strm.sum$pot.strm <- lst.pot.strm.5.single.gt0.fafp$pot.strm[
  lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% df.months.01$strm.num, ]
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))), 
           lst.pot.strm.sum,df.est.clp,df.daily.precip,
           paste0(getwd(), "/figures/strmPlotsSummer.pdf"))
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.sum,
               df.est=df.est.clp, df.daily.precip=df.daily.precip,
               out.file=paste0(getwd(),"/figures/strmInvdPlotsSummer.pdf"))
rm(df.months.00,df.months.01)

## fall strorms (Months- Sep, Oct, Nov)
df.months.00 <- df.strm.months[df.strm.months$month.bgn %in% 
                                 c("Sep","Oct","Nov"), ]
df.months.01 <- df.months.00[df.months.00$month.end %in% c("Sep","Oct","Nov"), ]
lst.pot.strm.fal <- lst.pot.strm.5.single.gt0.fafp
lst.pot.strm.fal$pot.strm <- lst.pot.strm.5.single.gt0.fafp$pot.strm[
  lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% df.months.01$strm.num, ]
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))), 
           lst.pot.strm.fal, df.est.clp,df.daily.precip, 
           paste0(getwd(), "/figures/strmPlotsFall.pdf"))
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.fal, df.est=df.est.clp, 
               df.daily.precip=df.daily.precip,
               out.file=paste0(getwd(), "/figures/strmInvdPlotsFall.pdf"))
rm(df.months.00,df.months.01)

## winter strorms (Months- Dec, Jan, Feb)
df.months.00 <- df.strm.months[df.strm.months$month.bgn %in% 
                                 c("Dec","Jan","Feb"), ]
df.months.01 <- df.months.00[df.months.00$month.end %in% c("Dec","Jan","Feb"), ]
lst.pot.strm.win <- lst.pot.strm.5.single.gt0.fafp
lst.pot.strm.win$pot.strm <- lst.pot.strm.5.single.gt0.fafp$pot.strm[
  lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% df.months.01$strm.num, ]
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))), 
           lst.pot.strm.win,df.est.clp,df.daily.precip,
           psate0(getwd(), "/figures/strmPlotsWinter.pdf"))
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.win, df.est=df.est.clp, 
               df.daily.precip=df.daily.precip,
               out.file=paste0(getwd(), "/figures/strmInvdPlotsWinter.pdf"))
rm(df.months.00,df.months.01)

## Spring strorms (Months- Mar, Apr, May)
df.months.00 <- df.strm.months[df.strm.months$month.bgn %in% 
                                 c("Mar","Apr","May"), ]
df.months.01 <- df.months.00[df.months.00$month.end %in% c("Mar","Apr","May"), ]
lst.pot.strm.spr <- lst.pot.strm.5.single.gt0.fafp
lst.pot.strm.spr$pot.strm <- lst.pot.strm.5.single.gt0.fafp$pot.strm[
  lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% df.months.01$strm.num, ]
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))), 
           lst.pot.strm.spr, df.est.clp,df.daily.precip,
           paste0(getwd(),"/figures/strmPlotsSpring.pdf"))
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.spr, df.est=df.est.clp, 
               df.daily.precip=df.daily.precip,
               out.file=paste0(getwd(),"/figures/strmInvdPlotsSpring.pdf"))
rm(df.months.00,df.months.01)

# get storms that cross seasons
chr.strm.sea <- c(unique(as.character(lst.pot.strm.sum$pot.strm$strm.num)),
  unique(as.character(lst.pot.strm.fal$pot.strm$strm.num)),
  unique(as.character(lst.pot.strm.win$pot.strm$strm.num)),
  unique(as.character(lst.pot.strm.spr$pot.strm$strm.num))
)
chr.strm.sea <- chr.strm.sea[order(as.numeric(chr.strm.sea))]
df.xsea <- df.strm.sum$strm.num[as.character(df.strm.sum$strm.num) %in% 
                                  chr.strm.sea == FALSE]
lst.pot.strm.xsea <- lst.pot.strm.5.single.gt0.fafp
lst.pot.strm.xsea$pot.strm <- lst.pot.strm.5.single.gt0.fafp$pot.strm[
  lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% df.xsea, ]
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))), 
           lst.pot.strm.xsea, df.est.clp, df.daily.precip,
           paste0(getwd(),"/figures/strmPlotsXSeasons.pdf"))
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.xsea, df.est=df.est.clp, 
               df.daily.precip=df.daily.precip,
               out.file=paste0(getwd(), "/figures/strmInvdPlotsXSeasons.pdf"))

# Review general charateristics storms
names(df.strm.sum)
df.strm.sum <- data.frame(df.strm.sum,
                          year=as.factor(format(df.strm.sum$date.end, 
                                                format="%Y")))
df.strm.sum <- data.frame(df.strm.sum,season=NA)
df.strm.sum$season[as.character(df.strm.sum$strm.num) %in% 
                     unique(as.character(lst.pot.strm.xsea$pot.strm$strm.num))] 
<- "cross-season"
df.strm.sum$season[as.character(df.strm.sum$strm.num) %in% 
                     unique(as.character(lst.pot.strm.sum$pot.strm$strm.num))]
<- "summer"
df.strm.sum$season[as.character(df.strm.sum$strm.num) %in% 
                     unique(as.character(lst.pot.strm.fal$pot.strm$strm.num))]
<- "fall"
df.strm.sum$season[as.character(df.strm.sum$strm.num) %in% 
                     unique(as.character(lst.pot.strm.win$pot.strm$strm.num))]
<- "winter"
df.strm.sum$season[as.character(df.strm.sum$strm.num) %in% 
                     unique(as.character(lst.pot.strm.spr$pot.strm$strm.num))] 
<- "spring"

## final storms
df.strm.sum <- table.me(yr.b=NULL, z=lst.pot.strm.5.single.gt0.fafp.pgt01)
library(xtable)
xt <- xtable(df.strm.sum)
pdf(file="strm_sum.pdf")
dev.off()
