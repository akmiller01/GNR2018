list.of.packages <- c("foreign","readr","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only=T)

wd = "~/GNR/gshs_data"
setwd(wd)

meta_vars = read_csv("../gshs_vars_meta.csv")
all_file_names = read_csv("../all_file_names.csv")
soda_files = subset(meta_vars,description=="soda")$filename

data.list = list()
var.lab.list = list()
data.index = 1

savs = list.files(path=wd, pattern="*.sav", full.names= T, recursive = T, ignore.case= T)
for(sav in savs){
  file_base = basename(sav)
  message(file_base)
  gshs_nat = read.spss(sav, use.value.labels = F)
  var_labs = data.frame(names(gshs_nat),attributes(gshs_nat)$variable.labels)
  names(var_labs) = c("variable.name","variable.label")
  var_labs$filename = file_base
  gshs_nat = data.frame(gshs_nat)
  gshs_nat$filename = file_base
  data.list[[data.index]] = gshs_nat
  var.lab.list[[data.index]] = var_labs
  data.index = data.index + 1 
}

gshs = rbindlist(data.list,fill=T)
gshs_vars = rbindlist(var.lab.list)

gshs = merge(gshs,all_file_names,by="filename")
gshs_vars = merge(gshs_vars,all_file_names,by="filename")

# save(gshs,gshs_vars,file="../gshs_all.RData")
# write_csv(gshs_vars,"../gshs_vars_all.csv")

gshs$fruit = gshs$Q7
gshs$veg = gshs$Q8
gshs$soda = NA
gshs$soda[which(gshs$filename %in% meta_vars$filename)] = gshs$Q9[which(gshs$filename %in% meta_vars$filename)] 
