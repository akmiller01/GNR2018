list.of.packages <- c("RPostgreSQL","data.table","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~")

make.sql.names <- function(x){
  return(substr(iconv(gsub(".","_",make.names(x),fixed=T),to="ASCII",sub=""),1,63))
}

cred <- readLines("C:/git/cred.txt")

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv
                 ,dbname=cred[1]
                 ,host = cred[2]
                 ,port = cred[3]
                 ,user = cred[4]
                 ,password = cred[5]
)

table.name = "an_example_table"
table.quote = c("source",table.name)

#Fill out this list with metadata
meta.data = list()
meta.data[["table_name"]] = table.name
meta.data[["source_name"]] = "source_name"
meta.data[["source_location"]] = "S drive location, if applicable"
meta.data[["date_uploaded"]] = Sys.time()
meta.data[["source_url"]] = "www.example.com"
meta.data[["source_contact"]] = "example@example.com"
meta.data.df = data.frame(meta.data)

if(!dbExistsTable(con, table.quote)){
  #Set your data equal to 'data' before running this
  data = "data"
  names(data) = make.sql.names(names(data))
  dbWriteTable(con, name = table.quote, value = data, row.names = FALSE)
  meta.table.quote = c("meta","source_tables")
  dbWriteTable(con, name = meta.table.quote, value = meta.data.df, row.names = F, append = T)
}
dbDisconnect(con)