list.of.packages <- c("foreign","readr","scrapeR","data.table","stringr", "tools","devtools","rJava","varhandle")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only=T)

# install_github("ropenscilabs/tabulizerjars")
# install_github("ropenscilabs/tabulizer")
library(tabulizer)

wd = "~/GNR/gshs_data"
unlink("~/GNR/gshs_data/*",recursive=T, force=T)
setwd(wd)

base_link = "http://www.who.int"
datasets_page_source = scrape("http://www.who.int/ncds/surveillance/gshs/datasets/en/",headers=T,follow=T,parse=T)[[1]]
link_elems = getNodeSet(datasets_page_source,"//ul[@class='list_dash']/li/a")
link_suffixes = sapply(link_elems,xmlGetAttr,"href")
link_suffixes = unique(c(link_suffixes,"/ncds/surveillance/gshs/myanmardataset/en/","/ncds/surveillance/gshs/bahamasdataset/en/"))

skip_standardization = c("benin")

data.list = list()
data.index = 1

full_links = paste0(base_link,link_suffixes)
for(full_link in full_links){
  country_name = substr(strsplit(full_link,"/")[[1]][7],1,nchar(strsplit(full_link,"/")[[1]][7])-7)
  country_page_source = scrape(full_link,headers=T,follow=T,parse=T)[[1]]
  media_link_elems = getNodeSet(country_page_source,"//a[@class='link_media']")
  media_links = sapply(media_link_elems,xmlGetAttr,"href")
  pdf_links = subset(media_links,grepl("pdf",media_links,ignore.case=T))
  nat_pdf = paste0(base_link,pdf_links[1])
  media_links = subset(media_links,grepl("spss",media_links,ignore.case=T))
  national_link = paste0(base_link,media_links[1])
  year = str_extract(national_link,"([1-2][0-9][0-9][0-9])")
  message(paste(country_name, year))
  tmp = tempfile()
  download.file(national_link,tmp)
  
  unzipped = unzip(tmp)
  if(!country_name %in% skip_standardization){
    tabs = extract_tables(nat_pdf, guess=F, area=list(c(168,44,792,220)))
    # tabs = extract_tables(nat_pdf, guess=F, columns = list(c(54,102,162)), area=list(c(162,54,792,220)))
    tabs.list = list()
    tabs.index = 1
    for(tab in tabs){
      if(ncol(tab)==4){
        tabs.list[[tabs.index]] = data.frame(tab)[c("X1","X2","X3")]
        tabs.index = tabs.index + 1
      }else if(ncol(tab)==3){
        tabs.list[[tabs.index]] = data.frame(tab)
        tabs.index = tabs.index + 1
      }else if(ncol(tab)==2){
        temp.tab = data.frame(tab)
        temp.tab$X3 = temp.tab$X2
        tabs.list[[tabs.index]] = temp.tab
        tabs.index = tabs.index + 1
      }
    }
    standard.names = rbindlist(tabs.list)
    standard.names[which(standard.names$X3=="" | is.na(standard.names$X3))]$X3 = standard.names[which(standard.names$X3=="" | is.na(standard.names$X3))]$X2
    standard.names = standard.names[,c("X2","X3"),with=FALSE]
    names(standard.names) = c("standard","non.standard")
    standard.names$standard = unfactor(standard.names$standard)
    standard.names$non.standard = unfactor(standard.names$non.standard) 
  }else{
    if(exists("standard.names")){
      rm(standard.names)
    }
  }
  if(tolower(file_ext(unzipped))=="sav"){
    #Unzipped is a file
    file_base = basename(unzipped)
    gshs_nat = read.spss(unzipped, use.value.labels = F)
    gshs_nat = data.frame(gshs_nat)
    names(gshs_nat) = toupper(names(gshs_nat))
    if(!country_name %in% skip_standardization){
      message("Number of standard names not in names: ",sum(!standard.names$standard %in% names(gshs_nat)))
      message("Number of nonstandard names not in names: ",sum(!standard.names$non.standard %in% names(gshs_nat)))
      if(sum(!standard.names$standard %in% names(gshs_nat))>0){
        #Not already standardized
        message("Standardizing!")
        standard.names = subset(standard.names, non.standard %in% names(gshs_nat))
        gshs_nat = gshs_nat[standard.names$non.standard]
        names(gshs_nat) = standard.names$standard 
      }
    }
    gshs_nat$country = country_name
    gshs_nat$year = year
    gshs_nat$filename = file_base
    data.list[[data.index]] = gshs_nat
    data.index = data.index + 1 
  }else{
    #Unzipped is a directory
    message("Directory! Recursively searching")
    savs = list.files(path=unzipped, pattern="*.sav", full.names= T, recursive = T, ignore.case= T)
    for(sav in savs){
      file_base = basename(sav)
      gshs_nat = read.spss(unzipped, use.value.labels = F)
      gshs_nat = data.frame(gshs_nat)
      names(gshs_nat) = toupper(names(gshs_nat))
      if(!country_name %in% skip_standardization){
        message("Number of standard names not in names: ",sum(!standard.names$standard %in% names(gshs_nat)))
        message("Number of nonstandard names not in names: ",sum(!standard.names$non.standard %in% names(gshs_nat)))
        if(sum(!standard.names$standard %in% names(gshs_nat))>0){
          #Not already standardized
          message("Standardizing!")
          standard.names = subset(standard.names, non.standard %in% names(gshs_nat))
          gshs_nat = gshs_nat[standard.names$non.standard]
          names(gshs_nat) = standard.names$standard 
        }
      }
      gshs_nat$country = country_name
      gshs_nat$year = year
      gshs_nat$filename = file_base
      data.list[[data.index]] = gshs_nat
      data.index = data.index + 1 
    }
  }
}

gshs = rbindlist(data.list,fill=T)

save(gshs,gshs_vars,file="../gshs_all.RData")
all_file_names = unique(data.frame(gshs)[c("filename","country","year")])
write_csv(all_file_names,"../all_file_names.csv")
