list.of.packages <- c("data.table","readr","survey","Hmisc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only=T)

setwd("C:/git/GNR2018")

load("gshs_all.RData")


meta_vars = read_csv("gshs_vars_meta.csv")
soda_files = subset(meta_vars,description=="soda")$filename

gshs$fruit = gshs$Q7
gshs$no.fruit = as.integer(gshs$fruit==1)
gshs$daily.fruit = as.integer(gshs$fruit>=3)
gshs$veg = gshs$Q8
gshs$no.veg = as.integer(gshs$veg==1)
gshs$daily.veg = as.integer(gshs$veg>=3)
gshs$soda = NA
gshs$soda[which(gshs$filename %in% soda_files)] = gshs$Q9[which(gshs$filename %in% soda_files)]
gshs$no.soda = as.integer(gshs$soda==1)
gshs$daily.soda = as.integer(gshs$soda>=3)

#Mongolia is missing survey weights
gshs = subset(gshs, country!="mongolia")
countries = unique(gshs$country)
tab.list = list()
for(ctry in countries){
  message(ctry)
  dat = subset(gshs,country==ctry)
  year = unique(dat$year)
  dsn = svydesign(ids = ~1, data = dat, weights = ~WEIGHT)
  if(sum(!is.na(dat$fruit))>0){
    no.fruit = svymean(~no.fruit, design = dsn,na.rm=T)[1]
    daily.fruit = svymean(~daily.fruit, design = dsn,na.rm=T)[1]
  }else{
    no.fruit = NA
    daily.fruit = NA
  }
  if(sum(!is.na(dat$veg))>0){
    no.veg = svymean(~no.veg, design = dsn,na.rm=T)[1]
    daily.veg = svymean(~daily.veg, design = dsn,na.rm=T)[1]
  }else{
    no.veg = NA
    daily.veg = NA
  }
  if(sum(!is.na(dat$soda))>0){
    no.soda = svymean(~no.soda, design = dsn,na.rm=T)[1]
    daily.soda = svymean(~daily.soda, design = dsn,na.rm=T)[1]
  }else{
    no.soda = NA
    daily.soda = NA
  }
  
  df = data.frame(country=ctry,year,no.fruit,daily.fruit,no.veg,daily.veg,no.soda,daily.soda)
  tab.list[[ctry]] = df
}

tab = rbindlist(tab.list, fill=T)
write_csv(tab,"gshs_tab.csv")


