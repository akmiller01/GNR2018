library("jsonlite")
library("readr")
url = "http://geodata.grid.unep.ch/api/countries"

dat = fromJSON(url)
dat = subset(dat,!is.na(iso_3))
write_csv(dat,"regions.csv",na="")
