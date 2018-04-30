list.of.packages <- c("foreign","readr","scrapeR","data.table","stringr", "tools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only=T)

wd = "~/GNR/gshs_data"
unlink("~/GNR/gshs_data/*",recursive=T, force=T)
setwd(wd)

meta_vars = read_csv("../gshs_vars_meta.csv")
soda_files = subset(meta_vars,description=="soda")$filename

base_link = "http://www.who.int"
datasets_page_source = scrape("http://www.who.int/ncds/surveillance/gshs/datasets/en/",headers=T,follow=T,parse=T)[[1]]
link_elems = getNodeSet(datasets_page_source,"//ul[@class='list_dash']/li/a")
link_suffixes = sapply(link_elems,xmlGetAttr,"href")

data.list = list()
var.lab.list = list()
data.index = 1

full_links = paste0(base_link,link_suffixes)
for(full_link in full_links){
  country_name = substr(strsplit(full_link,"/")[[1]][7],1,nchar(strsplit(full_link,"/")[[1]][7])-7)
  country_page_source = scrape(full_link,headers=T,follow=T,parse=T)[[1]]
  media_link_elems = getNodeSet(country_page_source,"//a[@class='link_media']")
  media_links = sapply(media_link_elems,xmlGetAttr,"href")
  media_links = subset(media_links,grepl("spss",media_links,ignore.case=T))
  national_link = paste0(base_link,media_links[1])
  year = str_extract(national_link,"([1-2][0-9][0-9][0-9])")
  message(paste(country_name, year))
  tmp = tempfile()
  download.file(national_link,tmp)
  
  unzipped = unzip(tmp)
  if(tolower(file_ext(unzipped))=="sav"){
    #Unzipped is a file
    file_base = basename(unzipped)
    gshs_nat = read.spss(unzipped, use.value.labels = F)
    var_labs = data.frame(names(gshs_nat),attributes(gshs_nat)$variable.labels)
    names(var_labs) = c("variable.name","variable.label")
    var_labs$country = country_name
    var_labs$year = year
    var_labs$filename = file_base
    gshs_nat = data.frame(gshs_nat)
    gshs_nat$country = country_name
    gshs_nat$year = year
    gshs_nat$filename = file_base
    data.list[[data.index]] = gshs_nat
    var.lab.list[[data.index]] = var_labs
    data.index = data.index + 1 
  }else{
    #Unzipped is a directory
    message("Directory! Recursively searching")
    savs = list.files(path=unzipped, pattern="*.sav", full.names= T, recursive = T, ignore.case= T)
    for(sav in savs){
      file_base = basename(sav)
      gshs_nat = read.spss(sav, use.value.labels = F)
      var_labs = data.frame(names(gshs_nat),attributes(gshs_nat)$variable.labels)
      names(var_labs) = c("variable.name","variable.label")
      var_labs$country = country_name
      var_labs$year = year
      var_labs$filename = file_base
      gshs_nat = data.frame(gshs_nat)
      gshs_nat$country = country_name
      gshs_nat$year = year
      gshs_nat$filename = file_base
      data.list[[data.index]] = gshs_nat
      var.lab.list[[data.index]] = var_labs
      data.index = data.index + 1 
    }
  }
}

gshs = rbindlist(data.list,fill=T)
gshs_vars = rbindlist(var.lab.list)

save(gshs,gshs_vars,file="../gshs_all.RData")
write_csv(gshs_vars,"../gshs_vars_all.csv")
all_file_names = unique(data.frame(gshs)[c("filename","country","year")])
write_csv(all_file_names,"../all_file_names.csv")
