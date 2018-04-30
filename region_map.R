list.of.packages <- c("rgdal","leaflet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = T)

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

map_data = data.frame(
  SUBREGION = c(
    "Eastern Africa"
    ,"Middle Africa"
    ,"Northern Africa"
    ,"Southern Africa"
    ,"Western Africa"
    ,"Central Asia"
    ,"Eastern Asia"
    ,"South-Eastern Asia"
    ,"Southern Asia"
    ,"Western Asia"
    ,"Caribbean"
    ,"Central America"
    ,"South America"
    ,"Australia and New Zealand"
    ,"Northern America"
  ),
  value = c(
    6
    ,7.1
    ,8.1
    ,4
    ,8.1
    ,3.7
    ,1.8
    ,8.7
    ,15.3
    ,3.9
    ,3.2
    ,0.9
    ,1.3
    ,9.2
    ,0.5
  )
)

m_countries = merge(countries,map_data,by="SUBREGION",all.x=TRUE)
m_countries$hover = paste(m_countries$SUBREGION, '<br>Value:',m_countries$value)

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = countries$gdp_md_est
)

leaflet(data=m_countries) %>%
  setView(0, 0, zoom=2) %>%
  addPolygons(color = ~pal(value)
              ,fillOpacity = 1
              ,stroke=F
              ,smoothFactor=0.2
              ,popup=m_countries$hover) %>%
  addLegend("bottomright", pal=pal, values = ~value, opacity = 1)
