country = "AF"
phase = "70"


setwd(paste0("C:/Users/Alex/Downloads/",country,"IR",phase,"DT"))

library(foreign)
library(Hmisc)
library(survey)

dat = read.dta(paste0(country,"IR",phase,"FL.DTA"),convert.factors=F)
var.labs = data.frame(var.name=names(dat),var.lab=attributes(dat)$var.labels)

na.in = function(x,in_c){
  if(is.na(x)){
    return(NA)
  }
  return(x %in% in_c)
}

# Unweighted children born in last 5 years
sum(dat$v208)
# 32,718
dat$weights = dat$v005/1000000
# Weighted children born in last 5 years
sum(dat$weights*dat$v208)
# 31,812.19
# Survey designed children born in last 5 years
dsn = svydesign(ids=~v021,strata=~v023,data=dat,weights=~weights,nest=T)
svytotal(~v208,dsn)
# 31,812 with a std err of 1,383

#Unweighted had diarrhea in last 2 weeks
sum(sapply(dat$h11_1,na.in,in_c=c(1,2)),na.rm=T) +
  sum(sapply(dat$h11_2,na.in,in_c=c(1,2)),na.rm=T) +
  sum(sapply(dat$h11_3,na.in,in_c=c(1,2)),na.rm=T) +
  sum(sapply(dat$h11_4,na.in,in_c=c(1,2)),na.rm=T) +
  sum(sapply(dat$h11_5,na.in,in_c=c(1,2)),na.rm=T) +
  sum(sapply(dat$h11_6,na.in,in_c=c(1,2)),na.rm=T)
# 7990

#Weighted had diarrhea in last 2 weeks
sum(dat$weights*sapply(dat$h11_1,na.in,in_c=c(1,2)),na.rm=T) +
  sum(dat$weights*sapply(dat$h11_2,na.in,in_c=c(1,2)),na.rm=T) +
  sum(dat$weights*sapply(dat$h11_3,na.in,in_c=c(1,2)),na.rm=T) +
  sum(dat$weights*sapply(dat$h11_4,na.in,in_c=c(1,2)),na.rm=T) +
  sum(dat$weights*sapply(dat$h11_5,na.in,in_c=c(1,2)),na.rm=T) +
  sum(dat$weights*sapply(dat$h11_6,na.in,in_c=c(1,2)),na.rm=T)
# 8687.131

# Another way of counting N
sum(dat$weights*!is.na(dat$h11_1),na.rm=T) +
  sum(dat$weights*!is.na(dat$h11_2),na.rm=T) +
  sum(dat$weights*!is.na(dat$h11_3),na.rm=T) +
  sum(dat$weights*!is.na(dat$h11_4),na.rm=T) +
  sum(dat$weights*!is.na(dat$h11_5),na.rm=T) +
  sum(dat$weights*!is.na(dat$h11_6),na.rm=T)

#Deaths for children born in the last 5 years?
d_u_5 = 0
for(i in 1:20){
  if(i<10){
    num = paste0("0",i)
  }else{num=i}
  dob = paste0("b3_",num)
  aad = paste0("b7_",num)
  d_u_5 = d_u_5 + sum(
    (dat[aad]<=60)*dat$weights*((dat$v008-dat[dob])<=1825)
    ,na.rm=T)
}
# 9701.538
