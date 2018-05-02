list.of.packages <- c("RPostgreSQL","data.table","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("C:/git/GNR2018")

cred <- readLines("C:/git/GNR2018/cred.txt")

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv
                 ,dbname=cred[1]
                 ,host = cred[2]
                 ,port = cred[3]
                 ,user = cred[4]
                 ,password = cred[5]
)

schemaListTables <- function(schema){
  query <- paste0("SELECT table_name FROM information_schema.tables
                  WHERE table_schema='",schema,"'")
  return(dbGetQuery(con,query)$table_name)
}

make.sql.names <- function(x){
  return(gsub(".","_",make.names(x),fixed=T))
}

csvs = list.files(path=".", pattern= "*.csv", full.names=T, recursive=T,ignore.case=T)


meta.data.list = list()
meta.data.index = 1
for(csv in csvs){
  table.name = make.sql.names(basename(csv))
  table.quote = c("source",table.name)
  
  if(!dbExistsTable(con, table.quote)){
    meta.data = list()
    meta.data[["source_name"]] = basename(csv)
    meta.data[["date_uploaded"]] = Sys.time()
    meta.data[["source_url"]] = "www.example.com"
    meta.data[["source_contact"]] = "example@example.com"
    meta.data.list[[meta.data.index]] = data.frame(meta.data)
    meta.data.index = meta.data.index + 1
    
    data = read_csv(csv)
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

dbDisconnect(con)
