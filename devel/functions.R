

getPotentialStormData <- function(spn, dates, flow) {

  require(smwrBase)
  
  df.tmp <- data.frame(dates=as.Date(dates), flow=flow)
  
  df.peaks <- df.tmp[peaks(df.tmp$flow, span=spn) == TRUE,]
  df.rises <- df.tmp[peaks(-1*df.tmp$flow, span=spn) == TRUE,]
  
  tmp.diff <- diff(df.rises$flow, lag=1)
  df.rises.sel <- df.rises[tmp.diff <= 0,]
  
  df.pot.strms <- getStormPolys(df.tmp, df.rises, df.rises.sel)

  lst.pot.strm <- list(peaks=df.peaks, rises=df.rises, rises.sel=df.rises.sel,
                       pot.strm=df.pot.strms)
}

getStormPolys <- function(df.flow, df.rises, df.rises.sel) {
  
  df.ends <- sapply(df.rises.sel$date, getNextRise, df.rises)

  df.pot.strm.bnds <- data.frame(date.bgn=df.rises.sel$date,
                                 date.end=do.call("c",df.ends[1,]),
                                 flow.bgn=df.rises.sel$flow)

  df.pot.storms <- do.call(rbind, 
                           lapply(seq(1:(length(df.pot.strm.bnds[, 1])-1)),
                                        getStormFlows,
                                        df.flow, df.pot.strm.bnds))

  df.pot.storms$strm.num <- factor(df.pot.storms$strm.num)

  return(df.pot.storms)
}

getStormFlows <- function(lng.strm, df.flow, df.pot.strm.bnds) {

  tmp.1 <- df.flow[as.Date(df.flow$date) >= df.pot.strm.bnds$date.bgn[lng.strm] 
                   & as.Date(df.flow$date) <= 
                     df.pot.strm.bnds$date.end[lng.strm], ]

  tmp.2 <- tmp.1[tmp.1$flow >= df.pot.strm.bnds$flow.bgn[lng.strm], ]

  rw.max <- max(as.numeric(row.names(tmp.1)))

  rw.flow.end <- max(as.numeric(row.names(tmp.2))) + 1

  if(rw.flow.end > rw.max) rw.flow.end <- rw.max

  tmp.date.end <- tmp.1$date[grep(as.character(rw.flow.end), row.names(tmp.1))]

  tmp.strm <- tmp.1[tmp.1$date <= tmp.date.end,]

  df.storm <- data.frame(date=as.Date(tmp.strm$date), flow=tmp.strm$flow, 
                         strm.num=lng.strm)

  return(df.storm)  
}

getNextRise <- function(dt.sel,df.rises){
  
  df.next <- df.rises[(df.rises$date - dt.sel) > 1,][1,]

  return(df.next)
}

plotToFile <- function(y.b=NULL, lst.pot.strm=lst.pot.strm, df.dates=df.dates, 
                       df.flow=df.flow, df.precip=df.precip,
                       out.file="strmPlots.pdf") {

  df.tmp <- data.frame(dates=df.dates,flow=df.flow)

  df.p <- data.frame(date=df.dates, p=df.precip)

  tmp.peaks <- lst.pot.strm$peaks
  tmp.peaks$dates <- as.POSIXct(lst.pot.strm$peaks$dates)
  
  tmp.rises <- lst.pot.strm$rises
  tmp.rises$dates <- as.POSIXct(lst.pot.strm$rises$dates)
  
  tmp.rises.sel <- lst.pot.strm$rises.sel
  tmp.rises.sel$dates <- as.POSIXct(lst.pot.strm$rises.sel$dates)
  
  tmp.pot.strms <- lst.pot.strm$pot.strm
  tmp.pot.strms$date <- as.POSIXct(lst.pot.strm$pot.strm$date)
  
  pdf(file=out.file, width=11, height=8.5, onefile=TRUE)
  
  for(ii in 1:(length(y.b)-1)) {
    
    dt.b <- as.POSIXct(paste0(y.b[ii], "/10/01"))
    
    dt.e <- as.POSIXct(paste0(as.numeric(format(dt.b,"%Y")) + 1,"/09/30"))
    
    df.p.yr <- df.p[df.p$date >= dt.b & df.p$date <= dt.e & df.p$p > 0, ]
    
    df.yr <- df.tmp[df.tmp$date >= dt.b & df.tmp$date <= dt.e, ]

    df.yr.ylims <- c(10^(floor(log10(min(df.yr$flow))-1)),
                     10^(ceiling(log10(max(df.yr$flow))+1)))
    
    df.yr.xlims <- c(dt.b, dt.e)
    
    str(tmp.peaks$date)
    
    df.peak <- tmp.peaks[tmp.peaks$date >= dt.b & tmp.peaks$date <= dt.e, ]
    
    df.rise <- tmp.rises[tmp.rises$date >= dt.b & tmp.rises$date <= dt.e, ]
    
    df.pot.strms <- tmp.pot.strms[tmp.pot.strms$date >= dt.b & 
                                    tmp.pot.strms$date <= dt.e, ]
    par(mfrow=c(2,1), tck=0.01,  mar=c(0,1,0,0.5),oma=c(7,5,7,2))
    
    layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), heights=c(1,3), 
           widths=c(1,1))
    
    plot(x=df.p.yr$date, y=df.p.yr$p, xlab="",pch="", xlim=df.yr.xlims,
         xaxt="n")
    
    title(xlab="", ylab="", main=paste0("for year ", y.b[ii]), 
          outer=TRUE, line=3)
    
    lines(x=df.p.yr$date, y=df.p.yr$p, type="h")
    
    grid(nx=30, ny=NULL)
    
#    plot(x=df.yr$date, y=df.yr$flow, type="l", log="y", lty="blank",
#         xlim=df.yr.xlims, ylim=df.yr.ylims)

plot(x=df.yr$date, y=df.yr$flow, type="l", log="y", lty="blank",
     xlim=df.yr.xlims)

    
    for(ii in as.numeric(unique(df.pot.strms$strm.num))) 
      polygon(x=df.pot.strms[df.pot.strms$strm.num == ii, "date"], 
              y=df.pot.strms[df.pot.strms$strm.num == ii, "flow"],
              col="yellow", lty="blank")
    
    lines(x=df.yr$date, y=df.yr$flow, type="l",col="blue")
    
    points(x=df.rise$date, y=df.rise$flow)
    
    points(x=df.peak$date, y=df.peak$flow)
    
    grid(nx=30, ny=NULL)    
  }
  dev.off()
}