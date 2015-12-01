# write storm info for PEST files 

load(file = paste0(getwd(), "/data/storm-data-for-PEST.RData"))

# storm dates
# use format -" 02/05/1996  00:00:00    02/17/1996  24:00:00"
chr.strm.dates <- paste0(" ",format(df.strm.sum$date.bgn, format = "%m/%d/%Y"), "  00:00:00    ",
       format(df.strm.sum$date.end, format = "%m/%d/%Y"), "  24:00:00")

# storm peaks in cfs
# use format - "mpeak1_max             7910.000        1.000000E-02  mpeak"
# mpeak1_max             7910.000        1.000000E-02  mpeak
# mpeak1_max          7.91000E+03        1.000000E-02  mpeak