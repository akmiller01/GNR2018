list.of.packages <- c("data.table","readr","survey","Hmisc","WDI","varhandle","ggplot2","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only=T)

setwd("C:/git/GNR2018")

load("gshs_all.RData")


meta_vars = read_csv("gshs_vars_meta.csv")
soda_files = subset(meta_vars,description=="soda")$filename
fastfood_files = subset(meta_vars,description=="fastfood")$filename

soap_files = subset(meta_vars,description=="soap")
hwtoilet_files = subset(meta_vars,description=="handwashtoilet")
hweating_files = subset(meta_vars,description=="handwasheating")

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
  
  df = data.frame(country=ctry,
                  year,
                  no.fruit,
                  daily.fruit,
                  no.veg,
                  daily.veg,
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
                  always.or.mostly.hand.wash.before.eating
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
regions = read_csv("regions.csv")
regions = regions[c("iso_3","region","subregion")]
setnames(regions,"iso_3","ISO_A3")

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

by_region = tab[,.(
  n = sum(!is.na(country))
  ,mean.no.fruit = weighted.mean(no.fruit,pop,na.rm=T)
  ,mean.daily.fruit = weighted.mean(daily.fruit,pop,na.rm=T)
  ,mean.no.veg = weighted.mean(no.veg,pop,na.rm=T)
  ,mean.daily.veg = weighted.mean(daily.veg,pop,na.rm=T)
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
),by=.(region,subregion)]
by_region = subset(by_region,!is.nan(mean.no.fruit) & !is.na(mean.no.fruit))

write_csv(by_region,"gshs_by_region.csv",na="")

titleize = function(x){
  split = strsplit(x,".",fixed=T)[[1]]
  return(capitalize(paste(split,collapse=" ")))
}

vars = c(
  "mean.no.fruit"
  ,"mean.daily.fruit"
  ,"mean.no.veg"
  ,"mean.daily.veg"
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
)
setwd("C:/git/GNR2018/plots")
for(var in vars){
  tmp = subset(by_region,!is.nan(get(var)))
  p = ggplot(tmp,aes(x=subregion,y=get(var),fill=region)) +
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
  ggsave(paste0(var,".jpg"),p)
}
