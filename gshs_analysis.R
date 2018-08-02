list.of.packages <- c("data.table","readr","survey","Hmisc","WDI","varhandle","ggplot2","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only=T)

setwd("~/GNR/")

load("gshs_all.RData")


meta_vars = read_csv("gshs_vars_meta.csv")
soda_files = subset(meta_vars,description=="soda")$filename
fastfood_files = subset(meta_vars,description=="fastfood")$filename

soap_files = subset(meta_vars,description=="soap")
hwtoilet_files = subset(meta_vars,description=="handwashtoilet")
hweating_files = subset(meta_vars,description=="handwasheating")

gshs$fruit = gshs$Q7
gshs$no.fruit = as.integer(gshs$fruit==1)
gshs$not.daily.fruit = as.integer(gshs$fruit<=2)
gshs$daily.fruit = as.integer(gshs$fruit>=3)
gshs$veg = gshs$Q8
gshs$no.veg = as.integer(gshs$veg==1)
gshs$not.daily.veg = as.integer(gshs$veg<=2)
gshs$daily.veg = as.integer(gshs$veg>=3)
gshs$soda = NA
gshs$soda[which(gshs$filename %in% soda_files)] = gshs$Q9[which(gshs$filename %in% soda_files)]
gshs$no.soda = as.integer(gshs$soda==1)
gshs$not.daily.soda = as.integer(gshs$soda<=2)
gshs$daily.soda = as.integer(gshs$soda>=3)

gshs$no.fruit.and.no.veg = as.integer(gshs$no.fruit & gshs$no.veg)
gshs$not.daily.fruit.and.not.daily.veg = as.integer(gshs$not.daily.fruit & gshs$not.daily.veg)
gshs$daily.fruit.and.daily.veg = as.integer(gshs$daily.fruit & gshs$daily.veg)

gshs$fruit.servings = gshs$fruit-2
gshs$fruit.servings[which(gshs$fruit.servings<0)] = 0
gshs$veg.servings = gshs$veg-2
gshs$veg.servings[which(gshs$veg.servings<0)] = 0
gshs$fruit.or.veg.servings = rowSums(gshs[,c("fruit.servings","veg.servings"),with=F],na.rm=T)
gshs$five.a.day = as.integer(gshs$fruit.or.veg.servings>=5)

gshs$fastfood = NA
gshs$fastfood[which(gshs$filename %in% fastfood_files)] = gshs$Q10[which(gshs$filename %in% fastfood_files)]
gshs$no.fastfood = as.integer(gshs$fastfood==1)
gshs$daily.fastfood = as.integer(gshs$fastfood==8)

gshs$hungry = gshs$Q6
gshs$never.or.rarely.hungry = as.integer(gshs$hungry<=2)
gshs$always.or.mostly.hungry = as.integer(gshs$hungry>=4)

gshs$soap = NA
soap.vars = unique(soap_files$variable.name)
for(soap.var in soap.vars){
  soap_files_var = subset(soap_files,variable.name==soap.var)$filename
  gshs$soap[which(gshs$filename %in% soap_files_var)] = data.frame(gshs)[which(gshs$filename %in% soap_files_var),soap.var]
}
gshs$never.or.rarely.soap = as.integer(gshs$soap<=2)
gshs$always.or.mostly.soap = as.integer(gshs$soap>=4)

gshs$hwtoilet = NA
hwtoilet.vars = unique(hwtoilet_files$variable.name)
for(hwtoilet.var in hwtoilet.vars){
  hwtoilet_files_var = subset(hwtoilet_files,variable.name==hwtoilet.var)$filename
  gshs$hwtoilet[which(gshs$filename %in% hwtoilet_files_var)] = data.frame(gshs)[which(gshs$filename %in% hwtoilet_files_var),hwtoilet.var]
}
gshs$never.or.rarely.hand.wash.after.toilet = as.integer(gshs$hwtoilet<=2)
gshs$always.or.mostly.hand.wash.after.toilet = as.integer(gshs$hwtoilet>=4)

gshs$hweating = NA
hweating.vars = unique(hweating_files$variable.name)
for(hweating.var in hweating.vars){
  hweating_files_var = subset(hweating_files,variable.name==hweating.var)$filename
  gshs$hweating[which(gshs$filename %in% hweating_files_var)] = data.frame(gshs)[which(gshs$filename %in% hweating_files_var),hweating.var]
}
gshs$never.or.rarely.hand.wash.before.eating = as.integer(gshs$hweating<=2)
gshs$always.or.mostly.hand.wash.before.eating = as.integer(gshs$hweating>=4)

#Gender
gshs$sex=NA
gshs$sex[which(gshs$Q2==1)] = "Male"
gshs$sex[which(gshs$Q2==2)] = "Female"
gshs$sex = factor(gshs$sex,labels=c("Male","Female"))

#Mongolia is missing survey weights
gshs = subset(gshs, country!="mongolia")
countries = unique(gshs$country)
tab.list = list()
for(ctry in countries){
  message(ctry)
  dat = subset(gshs,country==ctry)
  year = unique(dat$year)
  dsn = svydesign(ids = ~1, data = dat, weights = ~WEIGHT)
  if(sum(!is.na(dat$five.a.day))>0){
    five.a.day = svymean(~five.a.day,design=dsn,na.rm=T)[1]
  }else{
    five.a.day = NA
  }
  if(sum(!is.na(dat$fruit))>0){
    no.fruit = svymean(~no.fruit, design = dsn,na.rm=T)[1]
    not.daily.fruit = svymean(~not.daily.fruit, design = dsn,na.rm=T)[1]
    daily.fruit = svymean(~daily.fruit, design = dsn,na.rm=T)[1]
  }else{
    no.fruit = NA
    not.daily.fruit = NA
    daily.fruit = NA
  }
  if(sum(!is.na(dat$veg))>0){
    no.veg = svymean(~no.veg, design = dsn,na.rm=T)[1]
    not.daily.veg = svymean(~not.daily.veg, design = dsn,na.rm=T)[1]
    daily.veg = svymean(~daily.veg, design = dsn,na.rm=T)[1]
  }else{
    no.veg = NA
    not.daily.veg
    daily.veg = NA
  }
  if(sum(!is.na(dat$fruit))>0 && sum(!is.na(dat$veg))>0){
    no.fruit.and.no.veg = svymean(~no.fruit.and.no.veg, design = dsn,na.rm=T)[1]
    not.daily.fruit.and.not.daily.veg = svymean(~not.daily.fruit.and.not.daily.veg, design = dsn,na.rm=T)[1]
    daily.fruit.and.daily.veg = svymean(~daily.fruit.and.daily.veg, design = dsn,na.rm=T)[1]
  }else{
    no.fruit.and.no.veg = NA
    not.daily.fruit.and.not.daily.veg = NA
    daily.fruit.and.daily.veg = NA
  }
  if(sum(!is.na(dat$soda))>0){
    no.soda = svymean(~no.soda, design = dsn,na.rm=T)[1]
    daily.soda = svymean(~daily.soda, design = dsn,na.rm=T)[1]
  }else{
    no.soda = NA
    daily.soda = NA
  }
  if(sum(!is.na(dat$fastfood))>0){
    no.fastfood = svymean(~no.fastfood, design = dsn,na.rm=T)[1]
    daily.fastfood = svymean(~daily.fastfood, design = dsn,na.rm=T)[1]
  }else{
    no.fastfood = NA
    daily.fastfood = NA
  }
  if(sum(!is.na(dat$hungry))>0){
    never.or.rarely.hungry = svymean(~never.or.rarely.hungry, design = dsn,na.rm=T)[1]
    always.or.mostly.hungry = svymean(~always.or.mostly.hungry, design = dsn,na.rm=T)[1]
  }else{
    never.or.rarely.hungry = NA
    always.or.mostly.hungry = NA
  }
  if(sum(!is.na(dat$soap))>0){
    never.or.rarely.soap = svymean(~never.or.rarely.soap, design = dsn,na.rm=T)[1]
    always.or.mostly.soap = svymean(~always.or.mostly.soap, design = dsn,na.rm=T)[1]
  }else{
    never.or.rarely.soap = NA
    always.or.mostly.soap = NA
  }
  if(sum(!is.na(dat$hwtoilet))>0){
    never.or.rarely.hand.wash.after.toilet = svymean(~never.or.rarely.hand.wash.after.toilet, design = dsn,na.rm=T)[1]
    always.or.mostly.hand.wash.after.toilet = svymean(~always.or.mostly.hand.wash.after.toilet, design = dsn,na.rm=T)[1]
  }else{
    never.or.rarely.hand.wash.after.toilet = NA
    always.or.mostly.hand.wash.after.toilet = NA
  }
  if(sum(!is.na(dat$hweating))>0){
    never.or.rarely.hand.wash.before.eating = svymean(~never.or.rarely.hand.wash.before.eating, design = dsn,na.rm=T)[1]
    always.or.mostly.hand.wash.before.eating = svymean(~always.or.mostly.hand.wash.before.eating, design = dsn,na.rm=T)[1]
  }else{
    never.or.rarely.hand.wash.before.eating = NA
    always.or.mostly.hand.wash.before.eating = NA
  }
  
  #Male
  dat.male = subset(dat,sex=="Male")
  dsn.male = svydesign(ids = ~1, data = dat.male, weights = ~WEIGHT)
  if(sum(!is.na(dat.male$five.a.day))>0){
    five.a.day.male = svymean(~five.a.day,design=dsn.male,na.rm=T)[1]
  }else{
    five.a.day.male = NA
  }
  if(sum(!is.na(dat.male$fruit))>0){
    no.fruit.male = svymean(~no.fruit, design = dsn.male,na.rm=T)[1]
    not.daily.fruit.male = svymean(~not.daily.fruit, design = dsn.male,na.rm=T)[1]
    daily.fruit.male = svymean(~daily.fruit, design = dsn.male,na.rm=T)[1]
  }else{
    no.fruit.male = NA
    not.daily.fruit.male = NA
    daily.fruit.male = NA
  }
  if(sum(!is.na(dat.male$veg))>0){
    no.veg.male = svymean(~no.veg, design = dsn.male,na.rm=T)[1]
    not.daily.veg.male = svymean(~not.daily.veg, design = dsn.male,na.rm=T)[1]
    daily.veg.male = svymean(~daily.veg, design = dsn.male,na.rm=T)[1]
  }else{
    no.veg.male = NA
    not.daily.veg.male = NA
    daily.veg.male = NA
  }
  if(sum(!is.na(dat.male$fruit))>0 && sum(!is.na(dat.male$veg))>0){
    no.fruit.and.no.veg.male = svymean(~no.fruit.and.no.veg, design = dsn.male,na.rm=T)[1]
    not.daily.fruit.and.not.daily.veg.male = svymean(~not.daily.fruit.and.not.daily.veg, design = dsn.male,na.rm=T)[1]
    daily.fruit.and.daily.veg.male = svymean(~daily.fruit.and.daily.veg, design = dsn.male,na.rm=T)[1]
  }else{
    no.fruit.and.no.veg.male = NA
    not.daily.fruit.and.not.daily.veg.male = NA
    daily.fruit.and.daily.veg.male = NA
  }
  if(sum(!is.na(dat.male$soda))>0){
    no.soda.male = svymean(~no.soda, design = dsn.male,na.rm=T)[1]
    daily.soda.male = svymean(~daily.soda, design = dsn.male,na.rm=T)[1]
  }else{
    no.soda.male = NA
    daily.soda.male = NA
  }
  if(sum(!is.na(dat.male$fastfood))>0){
    no.fastfood.male = svymean(~no.fastfood, design = dsn.male,na.rm=T)[1]
    daily.fastfood.male = svymean(~daily.fastfood, design = dsn.male,na.rm=T)[1]
  }else{
    no.fastfood.male = NA
    daily.fastfood.male = NA
  }
  if(sum(!is.na(dat.male$hungry))>0){
    never.or.rarely.hungry.male = svymean(~never.or.rarely.hungry, design = dsn.male,na.rm=T)[1]
    always.or.mostly.hungry.male = svymean(~always.or.mostly.hungry, design = dsn.male,na.rm=T)[1]
  }else{
    never.or.rarely.hungry.male = NA
    always.or.mostly.hungry.male = NA
  }
  if(sum(!is.na(dat.male$soap))>0){
    never.or.rarely.soap.male = svymean(~never.or.rarely.soap, design = dsn.male,na.rm=T)[1]
    always.or.mostly.soap.male = svymean(~always.or.mostly.soap, design = dsn.male,na.rm=T)[1]
  }else{
    never.or.rarely.soap.male = NA
    always.or.mostly.soap.male = NA
  }
  if(sum(!is.na(dat.male$hwtoilet))>0){
    never.or.rarely.hand.wash.after.toilet.male = svymean(~never.or.rarely.hand.wash.after.toilet, design = dsn.male,na.rm=T)[1]
    always.or.mostly.hand.wash.after.toilet.male = svymean(~always.or.mostly.hand.wash.after.toilet, design = dsn.male,na.rm=T)[1]
  }else{
    never.or.rarely.hand.wash.after.toilet.male = NA
    always.or.mostly.hand.wash.after.toilet.male = NA
  }
  if(sum(!is.na(dat.male$hweating))>0){
    never.or.rarely.hand.wash.before.eating.male = svymean(~never.or.rarely.hand.wash.before.eating, design = dsn.male,na.rm=T)[1]
    always.or.mostly.hand.wash.before.eating.male = svymean(~always.or.mostly.hand.wash.before.eating, design = dsn.male,na.rm=T)[1]
  }else{
    never.or.rarely.hand.wash.before.eating.male = NA
    always.or.mostly.hand.wash.before.eating.male = NA
  }
  
  #Female
  dat.female = subset(dat,sex=="Female")
  dsn.female = svydesign(ids = ~1, data = dat.female, weights = ~WEIGHT)
  if(sum(!is.na(dat.male$five.a.day))>0){
    five.a.day.female = svymean(~five.a.day,design=dsn.female,na.rm=T)[1]
  }else{
    five.a.day.female = NA
  }
  if(sum(!is.na(dat.female$fruit))>0){
    no.fruit.female = svymean(~no.fruit, design = dsn.female,na.rm=T)[1]
    not.daily.fruit.female = svymean(~not.daily.fruit, design = dsn.female,na.rm=T)[1]
    daily.fruit.female = svymean(~daily.fruit, design = dsn.female,na.rm=T)[1]
  }else{
    no.fruit.female = NA
    not.daily.fruit.female = NA
    daily.fruit.female = NA
  }
  if(sum(!is.na(dat.female$veg))>0){
    no.veg.female = svymean(~no.veg, design = dsn.female,na.rm=T)[1]
    not.daily.veg.female = svymean(~not.daily.veg, design = dsn.female,na.rm=T)[1]
    daily.veg.female = svymean(~daily.veg, design = dsn.female,na.rm=T)[1]
  }else{
    no.veg.female = NA
    not.daily.veg.female = NA
    daily.veg.female = NA
  }
  if(sum(!is.na(dat.female$fruit))>0 && sum(!is.na(dat.female$veg))>0){
    no.fruit.and.no.veg.female = svymean(~no.fruit.and.no.veg, design = dsn.female,na.rm=T)[1]
    not.daily.fruit.and.not.daily.veg.female = svymean(~not.daily.fruit.and.not.daily.veg, design = dsn.female,na.rm=T)[1]
    daily.fruit.and.daily.veg.female = svymean(~daily.fruit.and.daily.veg, design = dsn.female,na.rm=T)[1]
  }else{
    no.fruit.and.no.veg.female = NA
    not.daily.fruit.and.not.daily.veg.female = NA
    daily.fruit.and.daily.veg.female = NA
  }
  if(sum(!is.na(dat.female$soda))>0){
    no.soda.female = svymean(~no.soda, design = dsn.female,na.rm=T)[1]
    daily.soda.female = svymean(~daily.soda, design = dsn.female,na.rm=T)[1]
  }else{
    no.soda.female = NA
    daily.soda.female = NA
  }
  if(sum(!is.na(dat.female$fastfood))>0){
    no.fastfood.female = svymean(~no.fastfood, design = dsn.female,na.rm=T)[1]
    daily.fastfood.female = svymean(~daily.fastfood, design = dsn.female,na.rm=T)[1]
  }else{
    no.fastfood.female = NA
    daily.fastfood.female = NA
  }
  if(sum(!is.na(dat.female$hungry))>0){
    never.or.rarely.hungry.female = svymean(~never.or.rarely.hungry, design = dsn.female,na.rm=T)[1]
    always.or.mostly.hungry.female = svymean(~always.or.mostly.hungry, design = dsn.female,na.rm=T)[1]
  }else{
    never.or.rarely.hungry.female = NA
    always.or.mostly.hungry.female = NA
  }
  if(sum(!is.na(dat.female$soap))>0){
    never.or.rarely.soap.female = svymean(~never.or.rarely.soap, design = dsn.female,na.rm=T)[1]
    always.or.mostly.soap.female = svymean(~always.or.mostly.soap, design = dsn.female,na.rm=T)[1]
  }else{
    never.or.rarely.soap.female = NA
    always.or.mostly.soap.female = NA
  }
  if(sum(!is.na(dat.female$hwtoilet))>0){
    never.or.rarely.hand.wash.after.toilet.female = svymean(~never.or.rarely.hand.wash.after.toilet, design = dsn.female,na.rm=T)[1]
    always.or.mostly.hand.wash.after.toilet.female = svymean(~always.or.mostly.hand.wash.after.toilet, design = dsn.female,na.rm=T)[1]
  }else{
    never.or.rarely.hand.wash.after.toilet.female = NA
    always.or.mostly.hand.wash.after.toilet.female = NA
  }
  if(sum(!is.na(dat.female$hweating))>0){
    never.or.rarely.hand.wash.before.eating.female = svymean(~never.or.rarely.hand.wash.before.eating, design = dsn.female,na.rm=T)[1]
    always.or.mostly.hand.wash.before.eating.female = svymean(~always.or.mostly.hand.wash.before.eating, design = dsn.female,na.rm=T)[1]
  }else{
    never.or.rarely.hand.wash.before.eating.female = NA
    always.or.mostly.hand.wash.before.eating.female = NA
  }
  
  df = data.frame(country=ctry,
                  year,
                  five.a.day,
                  no.fruit,
                  not.daily.fruit,
                  daily.fruit,
                  no.veg,
                  not.daily.veg,
                  daily.veg,
                  no.fruit.and.no.veg,
                  not.daily.fruit.and.not.daily.veg,
                  daily.fruit.and.daily.veg,
                  no.soda,
                  daily.soda,
                  no.fastfood,
                  daily.fastfood,
                  never.or.rarely.hungry,
                  always.or.mostly.hungry,
                  never.or.rarely.soap,
                  always.or.mostly.soap,
                  never.or.rarely.hand.wash.after.toilet,
                  always.or.mostly.hand.wash.after.toilet,
                  never.or.rarely.hand.wash.before.eating,
                  always.or.mostly.hand.wash.before.eating,
                  five.a.day.male,
                  no.fruit.male,
                  not.daily.fruit.male,
                  daily.fruit.male,
                  no.veg.male,
                  not.daily.veg.male,
                  daily.veg.male,
                  no.fruit.and.no.veg.male,
                  not.daily.fruit.and.not.daily.veg.male,
                  daily.fruit.and.daily.veg.male,
                  no.soda.male,
                  daily.soda.male,
                  no.fastfood.male,
                  daily.fastfood.male,
                  never.or.rarely.hungry.male,
                  always.or.mostly.hungry.male,
                  never.or.rarely.soap.male,
                  always.or.mostly.soap.male,
                  never.or.rarely.hand.wash.after.toilet.male,
                  always.or.mostly.hand.wash.after.toilet.male,
                  never.or.rarely.hand.wash.before.eating.male,
                  always.or.mostly.hand.wash.before.eating.male,
                  five.a.day.female,
                  no.fruit.female,
                  not.daily.fruit.female,
                  daily.fruit.female,
                  no.veg.female,
                  not.daily.veg.female,
                  daily.veg.female,
                  no.fruit.and.no.veg.female,
                  not.daily.fruit.and.not.daily.veg.female,
                  daily.fruit.and.daily.veg.female,
                  no.soda.female,
                  daily.soda.female,
                  no.fastfood.female,
                  daily.fastfood.female,
                  never.or.rarely.hungry.female,
                  always.or.mostly.hungry.female,
                  never.or.rarely.soap.female,
                  always.or.mostly.soap.female,
                  never.or.rarely.hand.wash.after.toilet.female,
                  always.or.mostly.hand.wash.after.toilet.female,
                  never.or.rarely.hand.wash.before.eating.female,
                  always.or.mostly.hand.wash.before.eating.female
                  )
  tab.list[[ctry]] = df
}

tab = rbindlist(tab.list, fill=T)
isos <- read_csv("gshs_isos.csv")
tab <- merge(tab,isos,by="country")
# write_csv(tab,"gshs_tab.csv")

tab$year = unfactor(tab$year)

pop = WDI("SP.POP.1317.TO.UN",country="all",extra=T,start=min(tab$year),end=max(tab$year))
pop = pop[c("iso3c","year","SP.POP.1317.TO.UN")]
names(pop) = c("ISO_A3","pop_year","pop")
regions = read_csv("unsd_regions_simple.csv")
regions = regions[c("ISO_A3","region","subregion")]
regions$region[which(regions$region=="Latin America and the Caribbean")] = "LAC"
regions$subregion[which(regions$subregion=="Latin America and the Caribbean")] = "LAC"

#Curacao and Tuvalu drop out due to no data
tab$pop_year = tab$year
tab$pop_year[which(tab$ISO_A3=="ATG")] = 2006
tab$pop_year[which(tab$ISO_A3=="CYM")] = 2006
tab$pop_year[which(tab$ISO_A3=="DMA")] = 2005
tab$pop_year[which(tab$ISO_A3=="KIR")] = 2006
tab$pop_year[which(tab$ISO_A3=="KNA")] = 2005
tab$pop_year[which(tab$ISO_A3=="SYC")] = 2005
tab$pop_year[which(tab$ISO_A3=="ATG")] = 2006

tab = merge(tab,pop,by=c("ISO_A3","pop_year"))
tab = merge(tab,regions,by="ISO_A3")
tab = subset(tab,!is.na(pop))

global = tab[,.(
  n = sum(!is.na(country))
  ,mean.five.a.day = weighted.mean(five.a.day,pop,na.rm=T)
  ,mean.five.a.day.male = weighted.mean(five.a.day.male,pop,na.rm=T)
  ,mean.five.a.day.female = weighted.mean(five.a.day.female,pop,na.rm=T)
  ,mean.no.fruit = weighted.mean(no.fruit,pop,na.rm=T)
  ,mean.not.daily.fruit = weighted.mean(not.daily.fruit,pop,na.rm=T)
  ,mean.daily.fruit = weighted.mean(daily.fruit,pop,na.rm=T)
  ,mean.no.veg = weighted.mean(no.veg,pop,na.rm=T)
  ,mean.not.daily.veg = weighted.mean(not.daily.veg,pop,na.rm=T)
  ,mean.daily.veg = weighted.mean(daily.veg,pop,na.rm=T)
  ,mean.no.fruit.and.no.veg = weighted.mean(no.fruit.and.no.veg,pop,na.rm=T)
  ,mean.not.daily.fruit.and.not.daily.veg = weighted.mean(not.daily.fruit.and.not.daily.veg,pop,na.rm=T)
  ,mean.daily.fruit.and.daily.veg = weighted.mean(daily.fruit.and.daily.veg,pop,na.rm=T)
  ,mean.no.soda = weighted.mean(no.soda,pop,na.rm=T)
  ,mean.daily.soda = weighted.mean(daily.soda,pop,na.rm=T)
  ,mean.no.fastfood = weighted.mean(no.fastfood,pop,na.rm=T)
  ,mean.daily.fastfood = weighted.mean(daily.fastfood,pop,na.rm=T)
  ,mean.never.or.rarely.hungry = weighted.mean(never.or.rarely.hungry,pop,na.rm=T)
  ,mean.always.or.mostly.hungry = weighted.mean(always.or.mostly.hungry,pop,na.rm=T)
  ,mean.never.or.rarely.soap = weighted.mean(never.or.rarely.soap,pop,na.rm=T)
  ,mean.always.or.mostly.soap = weighted.mean(always.or.mostly.soap,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.after.toilet = weighted.mean(never.or.rarely.hand.wash.after.toilet,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.after.toilet = weighted.mean(always.or.mostly.hand.wash.after.toilet,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.before.eating = weighted.mean(never.or.rarely.hand.wash.before.eating,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.before.eating = weighted.mean(always.or.mostly.hand.wash.before.eating,pop,na.rm=T)
  ,mean.no.fruit.male = weighted.mean(no.fruit.male,pop,na.rm=T)
  ,mean.not.daily.fruit.male = weighted.mean(not.daily.fruit.male,pop,na.rm=T)
  ,mean.daily.fruit.male = weighted.mean(daily.fruit.male,pop,na.rm=T)
  ,mean.no.veg.male = weighted.mean(no.veg.male,pop,na.rm=T)
  ,mean.not.daily.veg.male = weighted.mean(not.daily.veg.male,pop,na.rm=T)
  ,mean.daily.veg.male = weighted.mean(daily.veg.male,pop,na.rm=T)
  ,mean.no.fruit.and.no.veg.male = weighted.mean(no.fruit.and.no.veg.male,pop,na.rm=T)
  ,mean.not.daily.fruit.and.not.daily.veg.male = weighted.mean(not.daily.fruit.and.not.daily.veg.male,pop,na.rm=T)
  ,mean.daily.fruit.and.daily.veg.male = weighted.mean(daily.fruit.and.daily.veg.male,pop,na.rm=T)
  ,mean.no.soda.male = weighted.mean(no.soda.male,pop,na.rm=T)
  ,mean.daily.soda.male = weighted.mean(daily.soda.male,pop,na.rm=T)
  ,mean.no.fastfood.male = weighted.mean(no.fastfood.male,pop,na.rm=T)
  ,mean.daily.fastfood.male = weighted.mean(daily.fastfood.male,pop,na.rm=T)
  ,mean.never.or.rarely.hungry.male = weighted.mean(never.or.rarely.hungry.male,pop,na.rm=T)
  ,mean.always.or.mostly.hungry.male = weighted.mean(always.or.mostly.hungry.male,pop,na.rm=T)
  ,mean.never.or.rarely.soap.male = weighted.mean(never.or.rarely.soap.male,pop,na.rm=T)
  ,mean.always.or.mostly.soap.male = weighted.mean(always.or.mostly.soap.male,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.after.toilet.male = weighted.mean(never.or.rarely.hand.wash.after.toilet.male,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.after.toilet.male = weighted.mean(always.or.mostly.hand.wash.after.toilet.male,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.before.eating.male = weighted.mean(never.or.rarely.hand.wash.before.eating.male,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.before.eating.male = weighted.mean(always.or.mostly.hand.wash.before.eating.male,pop,na.rm=T)
  ,mean.no.fruit.female = weighted.mean(no.fruit.female,pop,na.rm=T)
  ,mean.not.daily.fruit.female = weighted.mean(not.daily.fruit.female,pop,na.rm=T)
  ,mean.daily.fruit.female = weighted.mean(daily.fruit.female,pop,na.rm=T)
  ,mean.no.veg.female = weighted.mean(no.veg.female,pop,na.rm=T)
  ,mean.not.daily.veg.female = weighted.mean(not.daily.veg.female,pop,na.rm=T)
  ,mean.daily.veg.female = weighted.mean(daily.veg.female,pop,na.rm=T)
  ,mean.no.fruit.and.no.veg.female = weighted.mean(no.fruit.and.no.veg.female,pop,na.rm=T)
  ,mean.not.daily.fruit.and.not.daily.veg.female = weighted.mean(not.daily.fruit.and.not.daily.veg.female,pop,na.rm=T)
  ,mean.daily.fruit.and.daily.veg.female = weighted.mean(daily.fruit.and.daily.veg.female,pop,na.rm=T)
  ,mean.no.soda.female = weighted.mean(no.soda.female,pop,na.rm=T)
  ,mean.daily.soda.female = weighted.mean(daily.soda.female,pop,na.rm=T)
  ,mean.no.fastfood.female = weighted.mean(no.fastfood.female,pop,na.rm=T)
  ,mean.daily.fastfood.female = weighted.mean(daily.fastfood.female,pop,na.rm=T)
  ,mean.never.or.rarely.hungry.female = weighted.mean(never.or.rarely.hungry.female,pop,na.rm=T)
  ,mean.always.or.mostly.hungry.female = weighted.mean(always.or.mostly.hungry.female,pop,na.rm=T)
  ,mean.never.or.rarely.soap.female = weighted.mean(never.or.rarely.soap.female,pop,na.rm=T)
  ,mean.always.or.mostly.soap.female = weighted.mean(always.or.mostly.soap.female,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.after.toilet.female = weighted.mean(never.or.rarely.hand.wash.after.toilet.female,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.after.toilet.female = weighted.mean(always.or.mostly.hand.wash.after.toilet.female,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.before.eating.female = weighted.mean(never.or.rarely.hand.wash.before.eating.female,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.before.eating.female = weighted.mean(always.or.mostly.hand.wash.before.eating.female,pop,na.rm=T)
)]

by_region = tab[,.(
  n = sum(!is.na(country))
  ,mean.five.a.day = weighted.mean(five.a.day,pop,na.rm=T)
  ,mean.five.a.day.male = weighted.mean(five.a.day.male,pop,na.rm=T)
  ,mean.five.a.day.female = weighted.mean(five.a.day.female,pop,na.rm=T)
  ,mean.no.fruit = weighted.mean(no.fruit,pop,na.rm=T)
  ,mean.not.daily.fruit = weighted.mean(not.daily.fruit,pop,na.rm=T)
  ,mean.daily.fruit = weighted.mean(daily.fruit,pop,na.rm=T)
  ,mean.no.veg = weighted.mean(no.veg,pop,na.rm=T)
  ,mean.not.daily.veg = weighted.mean(not.daily.veg,pop,na.rm=T)
  ,mean.daily.veg = weighted.mean(daily.veg,pop,na.rm=T)
  ,mean.no.fruit.and.no.veg = weighted.mean(no.fruit.and.no.veg,pop,na.rm=T)
  ,mean.not.daily.fruit.and.not.daily.veg = weighted.mean(not.daily.fruit.and.not.daily.veg,pop,na.rm=T)
  ,mean.daily.fruit.and.daily.veg = weighted.mean(daily.fruit.and.daily.veg,pop,na.rm=T)
  ,mean.no.soda = weighted.mean(no.soda,pop,na.rm=T)
  ,mean.daily.soda = weighted.mean(daily.soda,pop,na.rm=T)
  ,mean.no.fastfood = weighted.mean(no.fastfood,pop,na.rm=T)
  ,mean.daily.fastfood = weighted.mean(daily.fastfood,pop,na.rm=T)
  ,mean.never.or.rarely.hungry = weighted.mean(never.or.rarely.hungry,pop,na.rm=T)
  ,mean.always.or.mostly.hungry = weighted.mean(always.or.mostly.hungry,pop,na.rm=T)
  ,mean.never.or.rarely.soap = weighted.mean(never.or.rarely.soap,pop,na.rm=T)
  ,mean.always.or.mostly.soap = weighted.mean(always.or.mostly.soap,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.after.toilet = weighted.mean(never.or.rarely.hand.wash.after.toilet,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.after.toilet = weighted.mean(always.or.mostly.hand.wash.after.toilet,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.before.eating = weighted.mean(never.or.rarely.hand.wash.before.eating,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.before.eating = weighted.mean(always.or.mostly.hand.wash.before.eating,pop,na.rm=T)
  ,mean.no.fruit.male = weighted.mean(no.fruit.male,pop,na.rm=T)
  ,mean.not.daily.fruit.male = weighted.mean(not.daily.fruit.male,pop,na.rm=T)
  ,mean.daily.fruit.male = weighted.mean(daily.fruit.male,pop,na.rm=T)
  ,mean.no.veg.male = weighted.mean(no.veg.male,pop,na.rm=T)
  ,mean.not.daily.veg.male = weighted.mean(not.daily.veg.male,pop,na.rm=T)
  ,mean.daily.veg.male = weighted.mean(daily.veg.male,pop,na.rm=T)
  ,mean.no.fruit.and.no.veg.male = weighted.mean(no.fruit.and.no.veg.male,pop,na.rm=T)
  ,mean.not.daily.fruit.and.not.daily.veg.male = weighted.mean(not.daily.fruit.and.not.daily.veg.male,pop,na.rm=T)
  ,mean.daily.fruit.and.daily.veg.male = weighted.mean(daily.fruit.and.daily.veg.male,pop,na.rm=T)
  ,mean.no.soda.male = weighted.mean(no.soda.male,pop,na.rm=T)
  ,mean.daily.soda.male = weighted.mean(daily.soda.male,pop,na.rm=T)
  ,mean.no.fastfood.male = weighted.mean(no.fastfood.male,pop,na.rm=T)
  ,mean.daily.fastfood.male = weighted.mean(daily.fastfood.male,pop,na.rm=T)
  ,mean.never.or.rarely.hungry.male = weighted.mean(never.or.rarely.hungry.male,pop,na.rm=T)
  ,mean.always.or.mostly.hungry.male = weighted.mean(always.or.mostly.hungry.male,pop,na.rm=T)
  ,mean.never.or.rarely.soap.male = weighted.mean(never.or.rarely.soap.male,pop,na.rm=T)
  ,mean.always.or.mostly.soap.male = weighted.mean(always.or.mostly.soap.male,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.after.toilet.male = weighted.mean(never.or.rarely.hand.wash.after.toilet.male,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.after.toilet.male = weighted.mean(always.or.mostly.hand.wash.after.toilet.male,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.before.eating.male = weighted.mean(never.or.rarely.hand.wash.before.eating.male,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.before.eating.male = weighted.mean(always.or.mostly.hand.wash.before.eating.male,pop,na.rm=T)
  ,mean.no.fruit.female = weighted.mean(no.fruit.female,pop,na.rm=T)
  ,mean.not.daily.fruit.female = weighted.mean(not.daily.fruit.female,pop,na.rm=T)
  ,mean.daily.fruit.female = weighted.mean(daily.fruit.female,pop,na.rm=T)
  ,mean.no.veg.female = weighted.mean(no.veg.female,pop,na.rm=T)
  ,mean.not.daily.veg.female = weighted.mean(not.daily.veg.female,pop,na.rm=T)
  ,mean.daily.veg.female = weighted.mean(daily.veg.female,pop,na.rm=T)
  ,mean.no.fruit.and.no.veg.female = weighted.mean(no.fruit.and.no.veg.female,pop,na.rm=T)
  ,mean.not.daily.fruit.and.not.daily.veg.female = weighted.mean(not.daily.fruit.and.not.daily.veg.female,pop,na.rm=T)
  ,mean.daily.fruit.and.daily.veg.female = weighted.mean(daily.fruit.and.daily.veg.female,pop,na.rm=T)
  ,mean.no.soda.female = weighted.mean(no.soda.female,pop,na.rm=T)
  ,mean.daily.soda.female = weighted.mean(daily.soda.female,pop,na.rm=T)
  ,mean.no.fastfood.female = weighted.mean(no.fastfood.female,pop,na.rm=T)
  ,mean.daily.fastfood.female = weighted.mean(daily.fastfood.female,pop,na.rm=T)
  ,mean.never.or.rarely.hungry.female = weighted.mean(never.or.rarely.hungry.female,pop,na.rm=T)
  ,mean.always.or.mostly.hungry.female = weighted.mean(always.or.mostly.hungry.female,pop,na.rm=T)
  ,mean.never.or.rarely.soap.female = weighted.mean(never.or.rarely.soap.female,pop,na.rm=T)
  ,mean.always.or.mostly.soap.female = weighted.mean(always.or.mostly.soap.female,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.after.toilet.female = weighted.mean(never.or.rarely.hand.wash.after.toilet.female,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.after.toilet.female = weighted.mean(always.or.mostly.hand.wash.after.toilet.female,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.before.eating.female = weighted.mean(never.or.rarely.hand.wash.before.eating.female,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.before.eating.female = weighted.mean(always.or.mostly.hand.wash.before.eating.female,pop,na.rm=T)
),by=.(region)]
by_region = subset(by_region,!is.nan(mean.no.fruit) & !is.na(mean.no.fruit))

by_subregion = tab[,.(
  n = sum(!is.na(country))
  ,mean.five.a.day = weighted.mean(five.a.day,pop,na.rm=T)
  ,mean.five.a.day.male = weighted.mean(five.a.day.male,pop,na.rm=T)
  ,mean.five.a.day.female = weighted.mean(five.a.day.female,pop,na.rm=T)
  ,mean.no.fruit = weighted.mean(no.fruit,pop,na.rm=T)
  ,mean.not.daily.fruit = weighted.mean(not.daily.fruit,pop,na.rm=T)
  ,mean.daily.fruit = weighted.mean(daily.fruit,pop,na.rm=T)
  ,mean.no.veg = weighted.mean(no.veg,pop,na.rm=T)
  ,mean.not.daily.veg = weighted.mean(not.daily.veg,pop,na.rm=T)
  ,mean.daily.veg = weighted.mean(daily.veg,pop,na.rm=T)
  ,mean.no.fruit.and.no.veg = weighted.mean(no.fruit.and.no.veg,pop,na.rm=T)
  ,mean.not.daily.fruit.and.not.daily.veg = weighted.mean(not.daily.fruit.and.not.daily.veg,pop,na.rm=T)
  ,mean.daily.fruit.and.daily.veg = weighted.mean(daily.fruit.and.daily.veg,pop,na.rm=T)
  ,mean.no.soda = weighted.mean(no.soda,pop,na.rm=T)
  ,mean.daily.soda = weighted.mean(daily.soda,pop,na.rm=T)
  ,mean.no.fastfood = weighted.mean(no.fastfood,pop,na.rm=T)
  ,mean.daily.fastfood = weighted.mean(daily.fastfood,pop,na.rm=T)
  ,mean.never.or.rarely.hungry = weighted.mean(never.or.rarely.hungry,pop,na.rm=T)
  ,mean.always.or.mostly.hungry = weighted.mean(always.or.mostly.hungry,pop,na.rm=T)
  ,mean.never.or.rarely.soap = weighted.mean(never.or.rarely.soap,pop,na.rm=T)
  ,mean.always.or.mostly.soap = weighted.mean(always.or.mostly.soap,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.after.toilet = weighted.mean(never.or.rarely.hand.wash.after.toilet,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.after.toilet = weighted.mean(always.or.mostly.hand.wash.after.toilet,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.before.eating = weighted.mean(never.or.rarely.hand.wash.before.eating,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.before.eating = weighted.mean(always.or.mostly.hand.wash.before.eating,pop,na.rm=T)
  ,mean.no.fruit.male = weighted.mean(no.fruit.male,pop,na.rm=T)
  ,mean.not.daily.fruit.male = weighted.mean(not.daily.fruit.male,pop,na.rm=T)
  ,mean.daily.fruit.male = weighted.mean(daily.fruit.male,pop,na.rm=T)
  ,mean.no.veg.male = weighted.mean(no.veg.male,pop,na.rm=T)
  ,mean.not.daily.veg.male = weighted.mean(not.daily.veg.male,pop,na.rm=T)
  ,mean.daily.veg.male = weighted.mean(daily.veg.male,pop,na.rm=T)
  ,mean.no.fruit.and.no.veg.male = weighted.mean(no.fruit.and.no.veg.male,pop,na.rm=T)
  ,mean.not.daily.fruit.and.not.daily.veg.male = weighted.mean(not.daily.fruit.and.not.daily.veg.male,pop,na.rm=T)
  ,mean.daily.fruit.and.daily.veg.male = weighted.mean(daily.fruit.and.daily.veg.male,pop,na.rm=T)
  ,mean.no.soda.male = weighted.mean(no.soda.male,pop,na.rm=T)
  ,mean.daily.soda.male = weighted.mean(daily.soda.male,pop,na.rm=T)
  ,mean.no.fastfood.male = weighted.mean(no.fastfood.male,pop,na.rm=T)
  ,mean.daily.fastfood.male = weighted.mean(daily.fastfood.male,pop,na.rm=T)
  ,mean.never.or.rarely.hungry.male = weighted.mean(never.or.rarely.hungry.male,pop,na.rm=T)
  ,mean.always.or.mostly.hungry.male = weighted.mean(always.or.mostly.hungry.male,pop,na.rm=T)
  ,mean.never.or.rarely.soap.male = weighted.mean(never.or.rarely.soap.male,pop,na.rm=T)
  ,mean.always.or.mostly.soap.male = weighted.mean(always.or.mostly.soap.male,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.after.toilet.male = weighted.mean(never.or.rarely.hand.wash.after.toilet.male,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.after.toilet.male = weighted.mean(always.or.mostly.hand.wash.after.toilet.male,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.before.eating.male = weighted.mean(never.or.rarely.hand.wash.before.eating.male,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.before.eating.male = weighted.mean(always.or.mostly.hand.wash.before.eating.male,pop,na.rm=T)
  ,mean.no.fruit.female = weighted.mean(no.fruit.female,pop,na.rm=T)
  ,mean.not.daily.fruit.female = weighted.mean(not.daily.fruit.female,pop,na.rm=T)
  ,mean.daily.fruit.female = weighted.mean(daily.fruit.female,pop,na.rm=T)
  ,mean.no.veg.female = weighted.mean(no.veg.female,pop,na.rm=T)
  ,mean.not.daily.veg.female = weighted.mean(not.daily.veg.female,pop,na.rm=T)
  ,mean.daily.veg.female = weighted.mean(daily.veg.female,pop,na.rm=T)
  ,mean.no.fruit.and.no.veg.female = weighted.mean(no.fruit.and.no.veg.female,pop,na.rm=T)
  ,mean.not.daily.fruit.and.not.daily.veg.female = weighted.mean(not.daily.fruit.and.not.daily.veg.female,pop,na.rm=T)
  ,mean.daily.fruit.and.daily.veg.female = weighted.mean(daily.fruit.and.daily.veg.female,pop,na.rm=T)
  ,mean.no.soda.female = weighted.mean(no.soda.female,pop,na.rm=T)
  ,mean.daily.soda.female = weighted.mean(daily.soda.female,pop,na.rm=T)
  ,mean.no.fastfood.female = weighted.mean(no.fastfood.female,pop,na.rm=T)
  ,mean.daily.fastfood.female = weighted.mean(daily.fastfood.female,pop,na.rm=T)
  ,mean.never.or.rarely.hungry.female = weighted.mean(never.or.rarely.hungry.female,pop,na.rm=T)
  ,mean.always.or.mostly.hungry.female = weighted.mean(always.or.mostly.hungry.female,pop,na.rm=T)
  ,mean.never.or.rarely.soap.female = weighted.mean(never.or.rarely.soap.female,pop,na.rm=T)
  ,mean.always.or.mostly.soap.female = weighted.mean(always.or.mostly.soap.female,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.after.toilet.female = weighted.mean(never.or.rarely.hand.wash.after.toilet.female,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.after.toilet.female = weighted.mean(always.or.mostly.hand.wash.after.toilet.female,pop,na.rm=T)
  ,mean.never.or.rarely.hand.wash.before.eating.female = weighted.mean(never.or.rarely.hand.wash.before.eating.female,pop,na.rm=T)
  ,mean.always.or.mostly.hand.wash.before.eating.female = weighted.mean(always.or.mostly.hand.wash.before.eating.female,pop,na.rm=T)
),by=.(region,subregion)]
by_subregion = subset(by_subregion,!is.nan(mean.no.fruit) & !is.na(mean.no.fruit))

write_csv(global,"gshs_global.csv",na="")
write_csv(by_region,"gshs_by_region.csv",na="")
write_csv(by_subregion,"gshs_by_subregion.csv",na="")

titleize = function(x){
  split = strsplit(x,".",fixed=T)[[1]]
  return(capitalize(paste(split,collapse=" ")))
}

vars = c(
  "mean.five.a.day"
  ,"mean.five.a.day.male"
  ,"mean.five.a.day.female"
  ,"mean.no.fruit"
  ,"mean.not.daily.fruit"
  ,"mean.daily.fruit"
  ,"mean.no.veg"
  ,"mean.not.daily.veg"
  ,"mean.daily.veg"
  ,"mean.no.fruit.and.no.veg"
  ,"mean.not.daily.fruit.and.not.daily.veg"
  ,"mean.daily.fruit.and.daily.veg"
  ,"mean.no.soda"
  ,"mean.daily.soda"
  ,"mean.no.fastfood"
  ,"mean.daily.fastfood"
  ,"mean.never.or.rarely.hungry"
  ,"mean.always.or.mostly.hungry"
  ,"mean.never.or.rarely.soap"
  ,"mean.always.or.mostly.soap"
  ,"mean.never.or.rarely.hand.wash.after.toilet"
  ,"mean.always.or.mostly.hand.wash.after.toilet"
  ,"mean.never.or.rarely.hand.wash.before.eating"
  ,"mean.always.or.mostly.hand.wash.before.eating"
  ,"mean.no.fruit.male"
  ,"mean.not.daily.fruit.male"
  ,"mean.daily.fruit.male"
  ,"mean.no.veg.male"
  ,"mean.not.daily.veg.male"
  ,"mean.daily.veg.male"
  ,"mean.no.fruit.and.no.veg.male"
  ,"mean.not.daily.fruit.and.not.daily.veg.male"
  ,"mean.daily.fruit.and.daily.veg.male"
  ,"mean.no.soda.male"
  ,"mean.daily.soda.male"
  ,"mean.no.fastfood.male"
  ,"mean.daily.fastfood.male"
  ,"mean.never.or.rarely.hungry.male"
  ,"mean.always.or.mostly.hungry.male"
  ,"mean.never.or.rarely.soap.male"
  ,"mean.always.or.mostly.soap.male"
  ,"mean.never.or.rarely.hand.wash.after.toilet.male"
  ,"mean.always.or.mostly.hand.wash.after.toilet.male"
  ,"mean.never.or.rarely.hand.wash.before.eating.male"
  ,"mean.always.or.mostly.hand.wash.before.eating.male"
  ,"mean.no.fruit.female"
  ,"mean.not.daily.fruit.female"
  ,"mean.daily.fruit.female"
  ,"mean.no.veg.female"
  ,"mean.not.daily.veg.female"
  ,"mean.daily.veg.female"
  ,"mean.no.fruit.and.no.veg.female"
  ,"mean.not.daily.fruit.and.not.daily.veg.female"
  ,"mean.daily.fruit.and.daily.veg.female"
  ,"mean.no.soda.female"
  ,"mean.daily.soda.female"
  ,"mean.no.fastfood.female"
  ,"mean.daily.fastfood.female"
  ,"mean.never.or.rarely.hungry.female"
  ,"mean.always.or.mostly.hungry.female"
  ,"mean.never.or.rarely.soap.female"
  ,"mean.always.or.mostly.soap.female"
  ,"mean.never.or.rarely.hand.wash.after.toilet.female"
  ,"mean.always.or.mostly.hand.wash.after.toilet.female"
  ,"mean.never.or.rarely.hand.wash.before.eating.female"
  ,"mean.always.or.mostly.hand.wash.before.eating.female"
)
setwd("~/GNR/plots")
for(var in vars){
  tmp1 = subset(by_region,!is.nan(get(var)))
  p1 = ggplot(tmp1,aes(x=region,y=get(var),fill=region)) +
    geom_bar(stat="identity") +
    scale_y_continuous(labels = scales::percent) +
    # facet_grid(. ~ region, space="free_x", scales="free_x", switch="x") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.placement = "outside",
          strip.background = element_rect(fill=NA, colour="grey50"),
          panel.spacing.x=unit(0,"cm")) +
    labs(x="", y=titleize(var)) +
    guides(fill=F)
  tmp2 = subset(by_subregion,!is.nan(get(var)))
  p2 = ggplot(tmp2,aes(x=subregion,y=get(var),fill=region)) +
    geom_bar(stat="identity") +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(. ~ region, space="free_x", scales="free_x", switch="x") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.placement = "outside",
          strip.background = element_rect(fill=NA, colour="grey50"),
          panel.spacing.x=unit(0,"cm")) +
    labs(x="", y=titleize(var)) +
    guides(fill=F)
  ggsave(paste0(var,"_region.jpg"),p1)
  ggsave(paste0(var,"_subregion.jpg"),p2)
}
