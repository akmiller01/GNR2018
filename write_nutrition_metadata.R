list.of.packages <- c("data.table","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- "C:/Users/Alex/Documents/Data/GNR2018/Cleaned"
setwd(wd)

csvs = list.files(path=".", pattern= "*.csv", full.names=T, recursive=T,ignore.case=T)

make.sql.names <- function(x){
  return(gsub(".","_",make.names(x),fixed=T))
}


meta.data.list = list()
meta.data.index = 1
for(csv in csvs){
  meta.data = list()
  meta.data[["table_name"]] = make.sql.names(basename(csv))
  meta.data[["table_name_len"]] = nchar(meta.data[["table_name"]])
  meta.data[["source_location"]] = csv
  meta.data.list[[meta.data.index]] = data.frame(meta.data)
  meta.data.index = meta.data.index + 1
    
    
}

all.meta.data = rbindlist(meta.data.list)
write_csv(all.meta.data, "../sources_meta.csv")
