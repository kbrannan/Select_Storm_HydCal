# write storm info for PEST files 

load(file = paste0(getwd(), "/data/storm-data-for-PEST.RData"))

# storm dates
# use format -" 02/05/1996  00:00:00    02/17/1996  24:00:00"
chr.strm.dates <- paste0(" ",format(df.strm.sum$date.bgn, format = "%m/%d/%Y"), "  00:00:00    ",
       format(df.strm.sum$date.end, format = "%m/%d/%Y"), "  24:00:00")
# write the storm dates to file
cat(chr.strm.dates, file = "dates_stm.dat", sep = "\n")

# create vector for storm numbers ti use as ids
n.strms <- 1:length(df.strm.sum[, 1])

# storm peaks in cfs
# use format - "mpeak1_max          7.91000E+03        1.000000E-02  mpeak"
# create character vector with the storm peaks in cfs
chr.stm.peaks <- paste0(" mpeak", sprintf(n.strms, fmt = "%02i"), "_max          ",
       sprintf(junk$peak.tfl, fmt="%1.5E"), "        1.000000E-02  mpeak")

# storm volumes in cubic feet
# use format - "mvol_stm_1            2.8778977E+09    1.000000E-02  mvol_stm"
# create character vector with the storm volumes in cubic feet
chr.stm.vols <- paste0("mvol_stm_", sprintf(n.strms, fmt = "%02i"), "            ",
       sprintf(junk$sum.cuft.tfl, fmt="%1.5E"),
       "        1.000000E-02  mvol_stm")

# write storm peaks and vols to file
cat(cbind(chr.stm.peaks,chr.stm.vols), file = "strm_peaks_vols.dat", sep = "\n")


