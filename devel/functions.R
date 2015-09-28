# using http://adv-r.had.co.nz/Style.html for style guide

get_potential_storm_data <- function(spn, dates, flow) {
# identify storms by finding changes in sign of the slope of flow time-series. 
# the positive (rise or end recession) and negative (peak) changes in slopes 
# are found and the dates of the these chnages set the boudaries of the storm
# help identify storm characteristcis. identified the changes in the slope using
# "peaks" function in the smwrBase package and use local function 
# "get_storm_segments"
# input:
# spn -  span is the number of days used to calculate change in slopes, min is 
#        3 and max not limited. larger the span the more smoothing occurs and 
#        could miss some storms. smaller span increases sensitivity but also
#        increases identification of non-storm fluctuations. 5-days safe value
#        and use additional criteria outside this function
# date - vector of POSIXct dates that correspond to flow values
# flow - vector of numeric flow values in cfs
#
# output:
# lst.pot.strm - list of data.frames of dates and flows for storm info 
#    concave  - identified as negative change in slope
#    convex   - identified as positive change in slope
#    rises    - identfied as the start of the storm hydrograph from convex
#    pot.strm - flow segments of potential storms. storms assigned number as a
#               facotr to facilitate summary anaysis and further processing
#
# created by Kevin Brannan 2015-09-22
# 

  # load libraries
  require(smwrBase)
  
  # create local data.frame of date and flow for processing
  df.tmp <- data.frame(dates = as.POSIXct(dates), flow = flow)

  # find the potential rises and peaks using smwrBase::peaks
  df.concave <- df.tmp[peaks(df.tmp$flow, span = spn) == TRUE, ]
  df.convex  <- df.tmp[peaks(-1 * df.tmp$flow, span = spn) == TRUE, ]

  # identify the rises from convex by the sign of the inflextion using 
  # difference between the convex flow and the imdiately proceeding flow. 
  # negative or zero identifies rising hydrograph
  tmp.diff <- diff(df.convex$flow, lag = 1)
  df.rises <- df.convex[tmp.diff <= 0, ]

  # get potential storm segments using "get_storm_segments" function
  df.pot.strms <- get_storm_segments(df.tmp$dates, df.tmp$flow, df.convex, df.rises)

  # assemble output list from concave, convex, rises and potential storm
  # data.frames
  lst.pot.strm <- list(concave = df.concave, convex = df.convex, 
                       rises = df.rises, pot.strm = df.pot.strms)
  
  # return output
  return(lst.pot.strm)
}

get_storm_segments <- function(dates, flows, convex, rises) {
  # identify storm segements from convex inflections in flow time-series and a 
  # subset of the convex inflections identified as rises in flow.The main point
  # of the function is to identify the end of a storm segment using the 
  # "get_next_rise" function. The "get_storm_flows" function extracts the flow
  # for the storm segment. The "get_potential_storm_data" function calls this 
  # function and provides input
  #
  # input:
  # date - vector of POSIXct dates that correspond to flow values
  # flows - vector of numeric flow values in cfs
  # convex   - identified as positive change in slope
  # rises    - identfied as the start of the storm hydrograph from convex
  #
  # output:
  # pot.strms - data.frame of strom segements with the elements
  #     date     - date of flow in strom segment
  #     flow     - flow in strom segment
  #     strm.num - storm identifier as factor from a sequence 1 to length 
  #                of rises
  #
  # created by Kevin Brannan 2015-09-22
  # 
  
  # find the next rise for each of the ones in rises. these are the potential 
  # ends of the storm segments. use "get_next_rise" function
  df.ends <- sapply(rises$date, get_next_rise, convex)

  # data.frame of the boundaires of the storm segements
  df.pot.strm.bnds <- data.frame(date.bgn = rises$date,
                                 date.end = do.call("c",df.ends[1,]),
                                 flow.bgn = rises$flow)

  # get storm segments using "get_storm_flows" function
  pot.strms <- do.call(rbind, 
                           lapply(seq(1:(length(df.pot.strm.bnds[, 1])-1)),
                                  get_storm_flows,
                                  dates, flows, df.pot.strm.bnds))

  # make strm.num a factor
  pot.strms$strm.num <- factor(pot.strms$strm.num)

  # return output
  return(pot.strms)
}

get_next_rise <- function(cur.date, rises) {
  # return the next rise for the current one from a data.frame of rises in flow.
  # The "get_storm_segments" function calls this function and provides input
  #
  # input:
  # cur.date - POSIXct date of current rise
  # rises    - identfied as the start of the storm hydrograph from convex
  #
  # output:
  # next.rise - data.frame of date and flow of the next rise
  #
  # created by Kevin Brannan 2015-09-22
  # 
  
  # subtract the dates and select the next rise more than one day after 
  # the current one
  next.rise <- rises[(rises$date - cur.date) > 1, ][1, ]
  
  # return output
  return(next.rise)
}

get_storm_flows <- function(lng.strm, dates, flows, strm.bnds) {
  # get dates and flows given a the begin and end of a storm segment
  # The "get_potential_storm_data" function calls this function and 
  # provides input
  #
  # input:
  # lng.strm - storm identifier as integer
  # dates    - vector of POSIXct dates that correspond to flow values
  # flows    - vector of numeric flow values in cfs
  # strm.bnds - data frame containing stomr boundary information with elements
  #     date.bgn - begining date of storm segment
  #     date.end - ending date of storm segment
  #     flow.bgn - flow at begining of storm segment
  #
  # output:
  # storm - data.frame for storm segements with the elements
  #     date     - date of flow in storm segment
  #     flow     - flow in strom segment
  #     strm.num - storm identifier as integer
  #
  # created by Kevin Brannan 2015-09-22
  # 

  # create temporary data.frame for local use
  tmp.0 <- data.frame(date = dates, flow = flows)

  # subset dates and flows within begin and end of storm segment dates
  tmp.1 <- tmp.0[tmp.0$date >= strm.bnds$date.bgn[lng.strm] & 
                 tmp.0$date <= strm.bnds$date.end[lng.strm], ]

  # get flows in storm segment above the flow at start of storm
  tmp.2 <- tmp.1[tmp.1$flow >= strm.bnds$flow.bgn[lng.strm], ]

  # use row numbers (row.names) to get the row date-flow data of the begning of 
  # the storm segment
  rw.max <- max(as.numeric(row.names(tmp.1)))

  # use row numbers (row.names) to get the row date-flow data of the end of 
  # the storm segment
  rw.flow.end <- max(as.numeric(row.names(tmp.2))) + 1

  # end storm segment once the flows are at or below the flow at start of storm
  if (rw.flow.end > rw.max) rw.flow.end <- rw.max

  # set end storm segement at date of flow at or below flow of start of storm
  tmp.date.end <- tmp.1$date[grep(as.character(rw.flow.end), row.names(tmp.1))]

  # sub-set storm segment for date of flow at or below fow at start of storm
  tmp.strm <- tmp.1[tmp.1$date <= tmp.date.end, ]

  # create storm segement output data.frame
  storm <- data.frame(date=tmp.strm$date, flow=tmp.strm$flow, 
                         strm.num=lng.strm)

  # return output
  return(storm)  
}

storm_count_peaks <- function(flow) {
  # count the number of peaks in a flow segment. the segment neeads at 3 points
  #
  # input:
  # flow - vector of numeric flow values in cfs
  #
  # output:
  # count.peaks - number of peaks in the flow segment
  #
  # identified the changes in the slope using "peaks" function in the 
  # smwrBase package
  #
  # created by Kevin Brannan 2015-09-22
  
  # load libraries
  require(smwrBase)
  
  # find peaks using smwrBase::peaks
  count.peaks <- sum(peaks(flow, span = 3) * 1)
  
  # return output
  return(count.peaks)
  
}

storms_plot_to_file <- function(y.b = NULL, pot.strm = pot.strm, 
                                dates = dates, flow = flow, 
                                precip = precip, 
                                out.file = "strmPlots.pdf") {

  # plot flow and precip times series with storm hydrographs highlighted for 
  # for each year of data period and send figure to pdf file
  # input:
  #      y.b - vector of numeric years to plot
  # pot.strm - list of data.frames of dates and flows for storm info
  #    dates - vector of POSIXct dates that correspond to flow values
  #     flow - vector of numeric flow values in cfs
  #   precip - vector of numeric precip values in inches
  # out.file - string for pdf filename (and path) where figure is sent
  #
  # output:
  # No output in R environment. Figure is sent to pdf file at location sepcified
  # in out.file vairable

  # creating temporary data sets for flow and precip
  df.f <- data.frame(date = dates, flow = flow)
  df.p <- data.frame(date = dates, p = precip)

  # creating temporary data sets for storm list data set
  tmp.peaks <- pot.strm$convex
  tmp.peaks$dates <- pot.strm$convex$dates
  tmp.rises <- pot.strm$concave
  tmp.rises$dates <- pot.strm$concave$dates
  tmp.rises.sel <- pot.strm$rises.sel
  tmp.rises.sel$dates <- pot.strm$rises.sel$dates
  tmp.pot.strms <- pot.strm$pot.strm
  tmp.pot.strms$date <- pot.strm$pot.strm$date
  
  # open pdf file for output
  pdf(file=out.file, width = 11, height = 8.5, onefile = TRUE)

  # loop to print a figure for each year in "y.b" to a signle page in "out.file"
  for (ii in 1:(length(y.b) -1 )) {
    
  # use water year for period of plot
    dt.b <- as.POSIXct(paste0(y.b[ii], "/10/01"))
    dt.e <- as.POSIXct(paste0(as.numeric(format(dt.b, "%Y")) + 1, "/09/30"))

  # subset precip data for current year
    df.p.yr <- df.p[df.p$date >= dt.b & df.p$date <= dt.e & df.p$p > 0, ]

  # subset flow data for the current year
    df.f.yr <- df.f[df.f$date >= dt.b & df.f$date <= dt.e, ]

  # set y-limits for current year
    df.yr.ylims <- c(10 ^ (floor(log10(min(df.f.yr$flow)) - 1)),
                     10 ^ (ceiling(log10(max(df.f.yr$flow)) + 1)))
    
  # set x-limits for current year
    df.yr.xlims <- c(dt.b, dt.e)
    
  # subset storm peaks for current year
    df.peak <- tmp.peaks[tmp.peaks$date >= dt.b & tmp.peaks$date <= dt.e, ]
    
  # subset storm rises for current year
    df.rise <- tmp.rises[tmp.rises$date >= dt.b & tmp.rises$date <= dt.e, ]
    
  # subset storm flows for current year
    df.pot.strms <- tmp.pot.strms[tmp.pot.strms$date >= dt.b & 
                                  tmp.pot.strms$date <= dt.e, ]
    
  # set plot area matrix for 2 rows and one column along with other pars
    par(mfrow = c(2,1), tck = 0.01,  mar = c(0,1,0,0.5),oma = c(7,5,7,2))
    
  # vary height of plots. Make precip hight smaller than flow
    layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), heights = c(1,3), 
           widths = c(1,1))
    
  # precip plot set up, don't plot data
    plot(x = df.p.yr$date, y = df.p.yr$p, xlab = "",pch = "", 
         xlim = df.yr.xlims, xaxt = "n")
    
  # title for plot is current year
    title(xlab  = "", ylab = "", main = paste0("for year ", y.b[ii]), 
          outer = TRUE, line = 3)
    
  # plot vertical lines for each precip obs
    lines(x = df.p.yr$date, y = df.p.yr$p, type = "h")
    
  # add grid lines in plot for dates
    grid(nx = 30, ny = NULL)
    
  # set up plot for flow, don't plot data. Plot flow data after the storm
  # polygons are plotted so flow lines are borders of polygons
    plot(x = df.f.yr$date, y = df.f.yr$flow, type = "l", log = "y", lty = "blank",
         xlim = df.yr.xlims, ylim = df.yr.ylims)
    
  # plot each storm
    for (ii in as.numeric(unique(df.pot.strms$strm.num))) {
    # storm flow as a filled in polygon
      polygon(x = df.pot.strms[df.pot.strms$strm.num == ii, "date"], 
              y = df.pot.strms[df.pot.strms$strm.num == ii, "flow"],
              col="yellow", lty="blank")
    }
    
  # flow data for year plotted over storm polygon
    lines(x = df.f.yr$date, y = df.f.yr$flow, type = "l",col = "blue")
    
  # points for rises ploted over storm polygon and flow data
    points(x = df.rise$date, y = df.rise$flow)
    
  # points for peaks ploted over storm polygon and flow data
    points(x = df.peak$date, y = df.peak$flow)
    
  # add grid lines in plot for dates
    grid(nx = 30, ny = NULL)    
  }
  # close file done
  dev.off()
}

storm_plot_indiviudal_to_file <- function(pot.strm = pot.strm, 
                                          dates  = dates,
                                          flow   = flow, 
                                          precip = precip,
                                          out.file  = "strmInvdPlots.pdf") {

  # plot flow and precip times series with storm hydrographs highlighted for 
  # each individula storm and send figure to pdf file
  # input:
  # pot.strm - list of data.frames of dates and flows for storm info
  #    dates - vector of POSIXct dates that correspond to flow values
  #     flow - vector of numeric flow values in cfs
  #   precip - vector of numeric precip values in inches
  # out.file - string for pdf filename (and path) where figure is sent
  #
  # output:
  # No output in R environment. Figure is sent to pdf file at location sepcified
  # in out.file vairable
  
  # creating temporary data sets for flow and precip  
  df.f <- data.frame(dates = dates, flow = flow)
  df.p <- data.frame(date = dates, p = precip)

  # creating temporary data sets for storm list data set
  tmp.peaks <- pot.strm$convex
  tmp.rises <- pot.strm$concave
  tmp.rises.sel <- pot.strm$rises.sel
  tmp.pot.strms <- pot.strm$pot.strm
  strm.nums <- as.numeric(unique(as.character(tmp.pot.strms$strm.num)))

  # open pdf file for output
  pdf(file = out.file, width = 11, height = 8.5, onefile = TRUE)
  
  # loop to print a figure for each storm to a signle page in "out.file"
  for(ii in 1:(length(strm.nums))) {
  # get info for storm ii
    x <- tmp.pot.strms[tmp.pot.strms$strm.num == strm.nums[ii], ]

  # set y-limits for current storm
    tmp.ylims <- c(10 ^ (floor(log10(min(x$flow))   - 1)), 
                   10 ^ (ceiling(log10(max(x$flow)) + 1)))
  
  # set x-limits for current storm
    tmp.xlims <- c(min(x$date) - 1, max(x$date) + 1)
  
  # subset precip and flow for current storm
    tmp.p <- df.p[df.p$date >= tmp.xlims[1] & df.p$date <= tmp.xlims[2], ]
    tmp.f <- df.f[df.f$date >= tmp.xlims[1] & 
                      df.f$date <= tmp.xlims[2], ]
  
  # set plot area matrix for 2 rows and one column along with other pars
    par(mfrow = c(2, 1), tck = 0.01,  mar = c(0, 1, 0, 0.5), 
        oma = c(7, 5, 7, 2))
  
  # vary height of plots. Make precip hight smaller than flow
    layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE), heights = c(1, 3), 
           widths = c(1, 1))
  
  # precip plot set up, don't plot data
    plot(x = tmp.p$date, y = tmp.p$p, xlab = "",pch = "", xlim = tmp.xlims,
         ylim = c(0, max(c(tmp.p$p, 0.1))), xaxt = "n")
  
  # title for plot is current sorm
    title(xlab = "", ylab = "", main = paste0("storm num ", strm.nums[ii]), 
          outer = TRUE, line = 3)
  
  # plot vertical lines for each precip obs
    lines(x = tmp.p$date, y = tmp.p$p, type = "h")
  
  # add grid lines in plot for dates
    grid(nx = 30, ny = NULL)
  
  # set up plot for flow, don't plot data. Plot flow data after the storm
  # polygons are plotted so flow lines are borders of polygons  
    plot(x = tmp.f$date, y = tmp.f$flow, type = "l", log = "y", lty = "blank",
         xlim = tmp.xlims, ylim = tmp.ylims, xaxt = "n")
    
  # storm flow as a filled in polygon
    polygon(x = x$date, y = x$flow, col = "yellow", lty = "blank")
  
  # flow data for year plotted over storm polygon
    lines(x = tmp.f$date, y = tmp.f$flow, type = "l", col = "blue")
  
  # points for rises ploted over storm polygon and flow data
    points(x = tmp.rises$date, y = tmp.rises$flow)
  
  # points for peaks ploted over storm polygon and flow data
    points(x = tmp.peaks$date, y = tmp.peaks$flow)
  
  # add grid lines in plot for dates
    grid(nx = 30, ny = NULL)
  
  # format x-axis on flow plot to include day, month and year
    axis.Date(side = 1, x = tmp.f$date, format = "%m-%d-%Y")
  }
  
  # close file done
  dev.off()
}


storms_to_table <- function(yr.b, pot.strm) {
  # summarize storm data into a data frame
  # input:
  # y.b - single year to summarize 
  #       (optional, if left out summary done for all the years)
  # pot.strm - list of data.frames of dates and flows for storm info
  # output:
  # df.table - summary of the storms with columns
  #         strm.num - number assigned to storm from get_potential_storm_data
  #                    function
  #      length.days - length of storm in days
  #             peak - peak flow of storms in cfs
  #         sum.cuft - flow volume of storm in cubic feet
  
  # load package for SummaryBy function
  require(doBy)
  
  # create local temporary data frame fo storm flows
  y <- pot.strm$pot.strm
  
  # if done for single year
  if(is.null(yr.b) != TRUE) {
    
  # start date for water year
    dt.b <- as.POSIXct(paste0(yr.b, "/10/01"))
  
  # end date for water year
    dt.e <- as.POSIXct(paste0(as.numeric(format(dt.b, "%Y")) + 1, "/09/30"))
    x <- y[y$date >= dt.b & y$date <= dt.e, ]
  } else x <- y # do all years

  # summarize storms
  # get storm number
  df.table <- data.frame(strm.num = summaryBy(strm.num ~ 
                                                strm.num,x,FUN=max)[ , 2], 
                         date.bgn = x[firstobs(~strm.num, x), "date"], 
                         date.end = x[lastobs(~strm.num,  x), "date"])
  
  # add length of storms in days
  df.table <- data.frame(df.table,
                         length.days = as.numeric(df.table$date.end - 
                                                    df.table$date.bgn))
  
  # add peak flow and flow volume for storms
  df.table <- data.frame(df.table,
                         peak = summaryBy(flow ~ strm.num, x, FUN = max)[ , 2],
                         sum.cuft=summaryBy(flow ~ strm.num, x, 
                                            FUN = sum)[ , 2] * 
                           (3600 * 24) * df.table$length.days)
  # done retirn result
  return(df.table)
}

get_storm_peak_date <- function(dates, flow) {
  # get date of storm peak
  # input:
  # dates - vector of POSIXct dates for storm
  # flow - vector of numeric values for flows in cfs
  # output:
  # peak.date - POSIXct date of peak flow
  
  # subset dates for row of maximum flow, which is the peak flow
  peak.date <- dates[flow == max(flow)]
  
  # done return output
  return(peak.date)
  
}