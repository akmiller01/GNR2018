list.of.packages <- c("survey","data.table","readr","foreign","Hmisc","varhandle","reshape2","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("C:/Users/Alex/Documents/Data/GNR2018/DHS")
# 
# zips = list.files(pattern="*.zip",ignore.case=T)
# 
# for(zip in zips){
#   unzip(zip)
# }

savs = list.files(pattern="*.sav",ignore.case=T)


data.list = list()
lab.list = list()
region.list = list()
data.index = 1
for(sav in savs){
  dat.tmp = read.spss(sav,use.value.labels=F)
  var.labs = data.frame(var.name=names(dat.tmp),var.lab=attributes(dat.tmp)$variable.labels)
  dhscc = substr(basename(sav),1,2)
  phase = substr(basename(sav),5,6)
  if(dhscc=="NP" & phase %in% c("60","7H")){
    regions = attributes(dat.tmp)$label.table$SHDEVREG
  }else{
    regions = attributes(dat.tmp)$label.table$HV024 
  }
  regions.df = data.frame(region.name=names(regions),region=regions)
  message(dhscc,phase)
  dat.df = data.frame(dat.tmp)
  dat.df$dhscc = dhscc
  dat.df$phase = phase
  var.labs$dhscc = dhscc
  var.labs$phase = phase
  regions.df$dhscc = dhscc
  regions.df$phase = phase
  data.list[[data.index]] = dat.df
  lab.list[[data.index]] = var.labs
  region.list[[data.index]] = regions.df
  data.index = data.index + 1
}

dat = rbindlist(data.list,fill=T)
labs = rbindlist(lab.list)
regions = rbindlist(region.list)

# save(dat,labs,regions,file="stunting_et_bd_np.RData")
# load("stunting_et_bd_np.RData")

names(dat) = tolower(names(dat))

keep = c("hv105","hc1","hv104","hv001","hv002","hv024","hvidx","hv112","hv271","hv270"
         ,"hv005", "hv025", "shdevreg","hc70","dhscc","phase")

must.have = c("hv271","hv024","hv005","hv025","hc70")

dat <- dat[,keep,with=F]
message(nrow(dat))
dat = dat[complete.cases(dat[,must.have,with=F]),]
message(nrow(dat))

gc()

#Rename age var
names(dat)[which(names(dat)=="hv105")] <- "age"
names(dat)[which(names(dat)=="hc1")] <- "age.months"

#Rename sex var
names(dat)[which(names(dat)=="hv104")] <- "sex"

#Rename cluster/hh var
names(dat)[which(names(dat)=="hv001")] <- "cluster"
names(dat)[which(names(dat)=="hv002")] <- "household"
names(dat)[which(names(dat)=="hv024")] <- "region"
dat$region[which(dat$dhscc=="NP" & dat$phase %in% c("60","7H"))] = dat$shdevreg[which(dat$dhscc=="NP" & dat$phase %in% c("60","7H"))]
names(dat)[which(names(dat)=="hvidx")] <- "line"
names(dat)[which(names(dat)=="hv112")] <- "mother.line"

names(dat)[which(names(dat)=="hv271")] <- "wealth"
dat$wealth <- dat$wealth/100000
names(dat)[which(names(dat)=="hv270")] <- "wealth.q"

#Rename sample.weights var
names(dat)[which(names(dat)=="hv005")] <- "sample.weights"
dat$weights <- dat$sample.weights/1000000

#Rename urban var
names(dat)[which(names(dat)=="hv025")] <- "urban.rural"
dat$urban <- NA
dat$urban[which(dat$urban.rural==1)] <- 1
dat$urban[which(dat$urban.rural==2)] <- 0

names(dat)[which(names(dat)=="hc70")] <- "child.height.age"
dat$child.height.age <- dat$child.height.age/100
dat$child.height.age[which(dat$child.height.age==99.99)] <- NA
dat$stunted = dat$child.height.age<=-2

regions$region = unfactor(regions$region)
survey_years = read_csv("survey_years.csv")

regions$region.name[which(regions$region.name=="Addis Adaba")] = "Addis Ababa"
regions$region.name[which(regions$region.name=="Afar")] = "Affar"
regions$region.name[which(regions$region.name=="Benishangul")] = "Benishangul-Gumuz"
regions$region.name[which(regions$region.name=="Oromia")] = "Oromiya"
regions$region.name[which(regions$region.name=="SNNP")] = "SNNPR"

#by region
table1 = dat[,.(stunting.prev=weighted.mean(stunted,weights,na.rm=T)),by=.(dhscc,phase,region)]
table1 <- merge(table1,regions,by=c("region","dhscc","phase"))
table1 <- merge(table1,survey_years,by=c("dhscc","phase"))
table1[, c("region","dhscc","phase"):=NULL]
table1 = melt(table1,id.vars=c("country","region.name","year"))
table1 = dcast(table1, country+region.name~year+variable)

#by urban
table2 = dat[,.(stunting.prev=weighted.mean(stunted,weights,na.rm=T)),by=.(dhscc,phase,urban)]
table2 <- merge(table2,survey_years,by=c("dhscc","phase"))
table2[, c("dhscc","phase"):=NULL] 
table2 = melt(table2,id.vars=c("country","urban","year"))
table2 = dcast(table2,country+urban~year+variable)

#by wealth
table3 = dat[,.(stunting.prev=weighted.mean(stunted,weights,na.rm=T)),by=.(dhscc,phase,wealth.q)]
table3 <- merge(table3,survey_years,by=c("dhscc","phase"))
table3[, c("dhscc","phase"):=NULL] 
table3 = melt(table3,id.vars=c("country","wealth.q","year"))
table3 = dcast(table3, country+wealth.q~year+variable)

#by region and urban
table4 = dat[,.(stunting.prev=weighted.mean(stunted,weights,na.rm=T)),by=.(dhscc,phase,region,urban)]
table4 <- merge(table4,regions,by=c("region","dhscc","phase"))
table4 <- merge(table4,survey_years,by=c("dhscc","phase"))
table4[, c("region","dhscc","phase"):=NULL] 
table4 = melt(table4,id.vars=c("country","region.name","urban","year"))
table4 = dcast(table4, country+region.name+urban~year+variable)

#by region and wealth
table5 = dat[,.(stunting.prev=weighted.mean(stunted,weights,na.rm=T)),by=.(dhscc,phase,region,wealth.q)]
table5 <- merge(table5,regions,by=c("region","dhscc","phase"))
table5 <- merge(table5,survey_years,by=c("dhscc","phase"))
table5[, c("region","dhscc","phase"):=NULL] 
table5 = melt(table5,id.vars=c("country","region.name","wealth.q","year"))
table5 = dcast(table5, country+region.name+wealth.q~year+variable)

#by region and urban and wealth
table6 = dat[,.(stunting.prev=weighted.mean(stunted,weights,na.rm=T)),by=.(dhscc,phase,region,urban,wealth.q)]
table6 <- merge(table6,regions,by=c("region","dhscc","phase"))
table6 <- merge(table6,survey_years,by=c("dhscc","phase"))
table6[, c("region","dhscc","phase"):=NULL] 
table6 = melt(table6,id.vars=c("country","region.name","urban","wealth.q","year"))
table6 = dcast(table6, country+region.name+urban+wealth.q~year+variable)

wb = createWorkbook()
tables = list(
  "region"=table1
  ,"urban"=table2
  ,"wealth"=table3
  ,"region and urban"=table4
  ,"region and wealth"=table5
  ,"region, urban, and wealth"=table6)
for(i in 1:length(tables)){
  table.name = names(tables)[i]
  table = tables[[i]]
  addWorksheet(wb,table.name)
  writeData(wb,table.name,table)
}
saveWorkbook(wb,"BD_ET_NP_regional_stunting.xlsx",overwrite=T)
