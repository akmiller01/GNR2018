list.of.packages <- c("RPostgreSQL","sqldf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

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

getTable <- function(table_name){
  query <- paste("SELECT * FROM",table_name,";")
  return(dbGetQuery(con,query))
}

# Run this after you're done to close the connection
# dbDisconnect(con)