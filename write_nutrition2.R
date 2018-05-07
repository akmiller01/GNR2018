list.of.packages <- c("RPostgreSQL","data.table","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("C:/Users/Alex/Documents/Data/GNR2018/Cleaned")

cred <- readLines("C:/git/GNR2018/cred.txt")

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv
                 ,dbname=cred[1]
                 ,host = cred[2]
                 ,port = cred[3]
                 ,user = cred[4]
                 ,password = cred[5]
)

#Set client encoding to UTF8
dbGetQuery(con,"SET CLIENT_ENCODING TO 'UNICODE';")

schemaListTables <- function(schema){
  query <- paste0("SELECT table_name FROM information_schema.tables
                  WHERE table_schema='",schema,"'")
  return(dbGetQuery(con,query)$table_name)
}

make.sql.names <- function(x){
  return(substr(iconv(gsub(".","_",make.names(x),fixed=T),to="ASCII",sub=""),1,63))
}

csvs_meta = read_csv("../sources_meta.csv")


meta.data.list = list()
meta.data.index = 1
for(i in 1:nrow(csvs_meta)){
  csv = csvs_meta[i,]$source_location
  table.name = csvs_meta[i,]$table_name
  message(table.name)
  table.quote = c("source",table.name)
  
  if(!dbExistsTable(con, table.quote)){
    meta.data = list()
    meta.data[["table_name"]] = table.name
    meta.data[["source_name"]] = basename(csv)
    meta.data[["source_location"]] = csv
    meta.data[["date_uploaded"]] = Sys.time()
    meta.data[["source_url"]] = "www.example.com"
    meta.data[["source_contact"]] = "example@example.com"
    meta.data.list[[meta.data.index]] = data.frame(meta.data)
    meta.data.index = meta.data.index + 1
    
    data = read.csv(csv,na.strings="",check.names=F,as.is = T)
    names(data) = make.sql.names(names(data))
    # Write the data frame to the database
    dbWriteTable(con, name = table.quote, value = data, row.names = FALSE)
  }
}

all.meta.data = rbindlist(meta.data.list)
meta.table.quote = c("meta","source_tables")
if(!dbExistsTable(con, meta.table.quote)){
  dbWriteTable(con, name = meta.table.quote, value = all.meta.data, row.names = F)
}else{
  dbWriteTable(con, name = meta.table.quote, value = all.meta.data, row.names = F, append = T)
}

miss = csvs_meta$table_name[which(!csvs_meta$table_name %in% schemaListTables("source"))]
message(length(miss))

dbDisconnect(con)
