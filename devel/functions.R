# using http://adv-r.had.co.nz/Style.html for style guide

get_potential_storm_data <- function(spn, dates, flow) {

  require(smwrBase)
  
  df.tmp <- data.frame(dates = as.POSIXct(dates), flow = flow)
  
  df.peaks <- df.tmp[peaks(df.tmp$flow, span = spn) == TRUE, ]
  df.rises <- df.tmp[peaks(-1 * df.tmp$flow, span = spn) == TRUE, ]
  
  tmp.diff <- diff(df.rises$flow, lag = 1)
  df.rises.sel <- df.rises[tmp.diff <= 0, ]
  
  df.pot.strms <- get_storm_polys(df.tmp, df.rises, df.rises.sel)

  lst.pot.strm <- list(peaks = df.peaks, rises = df.rises, 
                       rises.sel = df.rises.sel,pot.strm = df.pot.strms)
}

get_storm_polys <- function(df.flow, df.rises, df.rises.sel) {
  
  df.ends <- sapply(df.rises.sel$date, get_next_rise, df.rises)

  df.pot.strm.bnds <- data.frame(date.bgn = df.rises.sel$date,
                                 date.end = do.call("c",df.ends[1,]),
                                 flow.bgn = df.rises.sel$flow)

  df.pot.storms <- do.call(rbind, 
                           lapply(seq(1:(length(df.pot.strm.bnds[, 1])-1)),
                                  get_storm_flows,
                                  df.flow, df.pot.strm.bnds))

  df.pot.storms$strm.num <- factor(df.pot.storms$strm.num)

  return(df.pot.storms)
}

get_next_rise <- function(dt.sel, df.rises) {
  
  df.next <- df.rises[(df.rises$date - dt.sel) > 1,][1, ]
  
  return(df.next)
}

get_storm_flows <- function(lng.strm, df.flow, df.pot.strm.bnds) {

  tmp.1 <- df.flow[as.POSIXct(df.flow$date) >= 
                     df.pot.strm.bnds$date.bgn[lng.strm] 
                   & as.POSIXct(df.flow$date) <= 
                     df.pot.strm.bnds$date.end[lng.strm], ]

  tmp.2 <- tmp.1[tmp.1$flow >= df.pot.strm.bnds$flow.bgn[lng.strm], ]

  rw.max <- max(as.numeric(row.names(tmp.1)))

  rw.flow.end <- max(as.numeric(row.names(tmp.2))) + 1

  if (rw.flow.end > rw.max) rw.flow.end <- rw.max

  tmp.date.end <- tmp.1$date[grep(as.character(rw.flow.end), row.names(tmp.1))]

  tmp.strm <- tmp.1[tmp.1$date <= tmp.date.end, ]

  df.storm <- data.frame(date=as.POSIXct(tmp.strm$date), flow=tmp.strm$flow, 
                         strm.num=lng.strm)

  return(df.storm)  
}


storms_plot_to_file <- function(y.b = NULL, lst.pot.strm = lst.pot.strm, 
                                df.dates = df.dates, df.flow = df.flow, 
                                df.precip = df.precip, 
                                out.file = "strmPlots.pdf") {

  df.tmp <- data.frame(dates = df.dates, flow = df.flow)

  df.p <- data.frame(date = df.dates, p = df.precip)

  tmp.peaks <- lst.pot.strm$peaks
  tmp.peaks$dates <- as.POSIXct(lst.pot.strm$peaks$dates)
  
  tmp.rises <- lst.pot.strm$rises
  tmp.rises$dates <- as.POSIXct(lst.pot.strm$rises$dates)
  
  tmp.rises.sel <- lst.pot.strm$rises.sel
  tmp.rises.sel$dates <- as.POSIXct(lst.pot.strm$rises.sel$dates)
  
  tmp.pot.strms <- lst.pot.strm$pot.strm
  tmp.pot.strms$date <- as.POSIXct(lst.pot.strm$pot.strm$date)
  
  pdf(file=out.file, width = 11, height = 8.5, onefile = TRUE)
  
  for (ii in 1:(length(y.b) -1 )) {
    
    dt.b <- as.POSIXct(paste0(y.b[ii], "/10/01"))
    
    dt.e <- as.POSIXct(paste0(as.numeric(format(dt.b, "%Y")) + 1, "/09/30"))
    
    df.p.yr <- df.p[df.p$date >= dt.b & df.p$date <= dt.e & df.p$p > 0, ]
    
    df.yr <- df.tmp[df.tmp$date >= dt.b & df.tmp$date <= dt.e, ]

    df.yr.ylims <- c(10 ^ (floor(log10(min(df.yr$flow)) - 1)),
                     10 ^ (ceiling(log10(max(df.yr$flow)) + 1)))
    
    df.yr.xlims <- c(dt.b, dt.e)
    
    df.peak <- tmp.peaks[tmp.peaks$date >= dt.b & tmp.peaks$date <= dt.e, ]
    
    df.rise <- tmp.rises[tmp.rises$date >= dt.b & tmp.rises$date <= dt.e, ]
    
    df.pot.strms <- tmp.pot.strms[tmp.pot.strms$date >= dt.b & 
                                  tmp.pot.strms$date <= dt.e, ]
    
    par(mfrow = c(2,1), tck = 0.01,  mar = c(0,1,0,0.5),oma = c(7,5,7,2))
    
    layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), heights = c(1,3), 
           widths = c(1,1))
    
    plot(x = df.p.yr$date, y = df.p.yr$p, xlab = "",pch = "", 
         xlim = df.yr.xlims, xaxt = "n")
    
    title(xlab  = "", ylab = "", main = paste0("for year ", y.b[ii]), 
          outer = TRUE, line = 3)
    
    lines(x = df.p.yr$date, y = df.p.yr$p, type = "h")
    
    grid(nx = 30, ny = NULL)
    
    plot(x = df.yr$date, y = df.yr$flow, type = "l", log = "y", lty = "blank",
         xlim = df.yr.xlims, ylim = df.yr.ylims)
    
    for (ii in as.numeric(unique(df.pot.strms$strm.num))) {
      polygon(x = df.pot.strms[df.pot.strms$strm.num == ii, "date"], 
              y = df.pot.strms[df.pot.strms$strm.num == ii, "flow"],
              col="yellow", lty="blank")
    }
    
    lines(x = df.yr$date, y = df.yr$flow, type = "l",col = "blue")
    
    points(x = df.rise$date, y = df.rise$flow)
    
    points(x = df.peak$date, y = df.peak$flow)
    
    grid(nx = 30, ny = NULL)    
  }
  dev.off()
}

storms_to_table <- function(yr.b, z) {
  
  require(doBy)
  
  y <- z$pot.strm
  
  if(is.null(yr.b) != TRUE) {
    
    dt.b <- as.POSIXct(paste0(yr.b, "/10/01"))
    
    dt.e <- as.POSIXct(paste0(as.numeric(format(dt.b, "%Y")) + 1, "/09/30"))
    x <- y[y$date >= dt.b & y$date <= dt.e, ]
  } else x <- y
  df.table <- data.frame(strm.num = summaryBy(strm.num ~ 
                                              strm.num,x,FUN=max)[ , 2], 
                         date.bgn = x[firstobs(~strm.num, x), "date"], 
                         date.end = x[lastobs(~strm.num,  x), "date"])
  df.table <- data.frame(df.table,
                         length.days = as.numeric(df.table$date.end - 
                                                  df.table$date.bgn))
  df.table <- data.frame(df.table,
                         peak = summaryBy(flow ~ strm.num, x, FUN = max)[ , 2],
                         sum.cuft=summaryBy(flow ~ strm.num, x, 
                                            FUN = sum)[ , 2] * 
                           (3600 * 24) * df.table$length.days)
  return(df.table)
}

storm_count_peaks <- function(flow) {
  
  sum(peaks(flow, span = 3) * 1)
  
}

get_storm_peak_date <- function(x.date, x.val) {
  
  x.date[x.val == max(x.val)]
  
}

storm_plot_indiviudal_to_file <- function(tmp.lst.pot.strm = lst.pot.strm, 
                                          df.dates  = df.dates,
                                          df.flow   = df.flow, 
                                          df.precip = df.precip,
                                          out.file  = "strmInvdPlots.pdf") {
  
  df.tmp <- data.frame(dates = df.dates, flow = df.flow)
  
  df.p <- data.frame(date = df.dates, p = df.precip)
  
  tmp.peaks <- tmp.lst.pot.strm$peaks
  
  tmp.rises <- tmp.lst.pot.strm$rises
  
  tmp.rises.sel <- tmp.lst.pot.strm$rises.sel
  
  tmp.pot.strms <- tmp.lst.pot.strm$pot.strm
  
  strm.nums <- as.numeric(unique(as.character(tmp.pot.strms$strm.num)))
  
  pdf(file = out.file, width = 11, height = 8.5, onefile = TRUE)
  
  for(ii in 1:(length(strm.nums))) {
    
    x <- tmp.pot.strms[tmp.pot.strms$strm.num == strm.nums[ii], ]
    
    tmp.ylims <- c(10 ^ (floor(log10(min(x$flow))   - 1)), 
                   10 ^ (ceiling(log10(max(x$flow)) + 1)))
    
    tmp.xlims <- c(min(x$date) - 1, max(x$date) + 1)
    
    tmp.p <- df.p[df.p$date >= tmp.xlims[1] & df.p$date <= tmp.xlims[2], ]
    
    tmp.f <- df.tmp[df.tmp$date >= tmp.xlims[1] & 
                      df.tmp$date <= tmp.xlims[2], ]
    
    par(mfrow = c(2, 1), tck = 0.01,  mar = c(0, 1, 0, 0.5), 
        oma = c(7, 5, 7, 2))
    
    layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE), heights = c(1, 3), 
           widths = c(1, 1))
    
    plot(x = tmp.p$date, y = tmp.p$p, xlab = "",pch = "", xlim = tmp.xlims,
         ylim = c(0, max(c(tmp.p$p, 0.1))), xaxt = "n")
    
    title(xlab = "", ylab = "", main = paste0("storm num ", strm.nums[ii]), 
          outer = TRUE, line = 3)
    
    lines(x = tmp.p$date, y = tmp.p$p, type = "h")
    
    grid(nx = 30, ny = NULL)
    
    plot(x = tmp.f$date, y = tmp.f$flow, type = "l", log = "y", lty = "blank",
         xlim = tmp.xlims, ylim = tmp.ylims, xaxt = "n")
    
    polygon(x = x$date, y = x$flow, col = "yellow", lty = "blank")
    
    lines(x = tmp.f$date, y = tmp.f$flow, type = "l", col = "blue")
    
    points(x = tmp.rises$date, y = tmp.rises$flow)
    
    points(x = tmp.peaks$date, y = tmp.peaks$flow)
    
    grid(nx = 30, ny = NULL)
    
    axis.Date(side = 1, x = tmp.f$date, format = "%m-%d-%Y")
  }
  dev.off()
}
