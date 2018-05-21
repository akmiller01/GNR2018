setwd("C:/Users/Alex/Downloads/AFIR70DT")

library(foreign)
library(Hmisc)
library(survey)

af = read.dta("AFIR70FL.DTA",convert.factors=F)
var.labs = data.frame(var.name=names(af),var.lab=attributes(af)$var.labels)

na.in = function(x,in_c){
  if(is.na(x)){
    return(NA)
  }
  return(x %in% in_c)
}

# Unweighted children born in last 5 years
sum(af$v208)
# 32,718
af$weights = af$v005/1000000
# Weighted children born in last 5 years
sum(af$weights*af$v208)
# 31,812.19
# Survey designed children born in last 5 years
dsn = svydesign(ids=~v021,strata=~v023,data=af,weights=~weights,nest=T)
svytotal(~v208,dsn)
# 31,812 with a std err of 1,383

#Unweighted had diarrhea in last 2 weeks
sum(sapply(af$h11_1,na.in,in_c=c(1,2)),na.rm=T) +
  sum(sapply(af$h11_2,na.in,in_c=c(1,2)),na.rm=T) +
  sum(sapply(af$h11_3,na.in,in_c=c(1,2)),na.rm=T) +
  sum(sapply(af$h11_4,na.in,in_c=c(1,2)),na.rm=T) +
  sum(sapply(af$h11_5,na.in,in_c=c(1,2)),na.rm=T) +
  sum(sapply(af$h11_6,na.in,in_c=c(1,2)),na.rm=T)
# 7990

#Weighted had diarrhea in last 2 weeks
sum(af$weights*sapply(af$h11_1,na.in,in_c=c(1,2)),na.rm=T) +
  sum(af$weights*sapply(af$h11_2,na.in,in_c=c(1,2)),na.rm=T) +
  sum(af$weights*sapply(af$h11_3,na.in,in_c=c(1,2)),na.rm=T) +
  sum(af$weights*sapply(af$h11_4,na.in,in_c=c(1,2)),na.rm=T) +
  sum(af$weights*sapply(af$h11_5,na.in,in_c=c(1,2)),na.rm=T) +
  sum(af$weights*sapply(af$h11_6,na.in,in_c=c(1,2)),na.rm=T)
# 8687.131