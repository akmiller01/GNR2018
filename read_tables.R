list.of.packages <- c("RPostgreSQL","data.table","readr","rgdal","leaflet","htmlwidgets","Hmisc")
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

schemaListTables("public")

gshs = dbReadTable(con,c("public","gshs_by_country"))

isos <- read_csv("gshs_isos.csv")
gshs <- merge(gshs,isos,by="country")

gshs <- subset(gshs,!is.na(no.fruit))

url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"

tmp <- tempdir()

file <- basename(url)

if(!file.exists(file)){
  download.file(url, file)
}
unzip(file, exdir = tmp)

countries <- readOGR(dsn = tmp, 
                     layer = "ne_50m_admin_0_countries", 
                     encoding = "UTF-8",
                     verbose = FALSE)

m_countries = merge(countries,gshs,by="ISO_A3",all.x=TRUE)

titleize = function(x){
  split = strsplit(x,".",fixed=T)[[1]]
  return(capitalize(paste(split,collapse=" ")))
}

vars = c("no.fruit","daily.fruit","no.veg","daily.veg","no.soda","daily.soda")
setwd("C:/git/GNR2018/leaflet")
for(var in vars){
  message(var)
  m_countries$hover = paste("<b>",m_countries$NAME,"</b>"
                            ,'<br><b>Survey year:</b>',m_countries$year
                            ,'<br><b>',titleize(var),'</b>:',paste0(round(m_countries@data[,var]*100),"%")
  )
  if(var %in% c("no.fruit","no.veg","daily.soda")){
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = round(m_countries@data[,var]*100)
    )
  }else{
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = round(m_countries@data[,var]*100),
      reverse = T
    )
  }
  
  m = leaflet(data=m_countries) %>%
    setView(0, 0, zoom=2) %>%
    addPolygons(color = pal(round(m_countries@data[,var]*100))
                ,fillOpacity = 1
                ,stroke=F
                ,smoothFactor=0.2
                ,popup=m_countries$hover) %>%
    addLegend("bottomright"
              , pal=pal
              , values =round(m_countries@data[,var]*100)
              , opacity = 1
              , title=titleize(var)
              ,labFormat = labelFormat(suffix="%")
              )
  saveWidget(m, file=paste0(var,".html")) 
}

dbDisconnect(con)

by_region = data.table(m_countries@data)[,.(
  mean.no.fruit = weighted.mean(no.fruit,POP_EST,na.rm=T)
  ,mean.daily.fruit = weighted.mean(daily.fruit,POP_EST,na.rm=T)
  ,mean.no.veg = weighted.mean(no.veg,POP_EST,na.rm=T)
  ,mean.daily.veg = weighted.mean(daily.veg,POP_EST,na.rm=T)
  ,mean.no.soda = weighted.mean(no.soda,POP_EST,na.rm=T)
  ,mean.daily.soda = weighted.mean(daily.soda,POP_EST,na.rm=T)
),by=.(SUBREGION)]
by_region = subset(by_region,!is.nan(mean.no.fruit))
write_csv(by_region,"gshs_by_region.csv")
write_csv(gshs,"gshs_by_country.csv")
