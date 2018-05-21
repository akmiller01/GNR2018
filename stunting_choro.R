list.of.packages <- c("rgeos","maptools","gpclib","ggplot2","readr","data.table","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("C:/Users/Alex/Documents/Data/GNR2018/stunting_map")

bd_shp = readShapeSpatial("shps_bd/sdr_subnational_boundaries.shp")
et_shp = readShapeSpatial("shps_et/sdr_subnational_boundaries.shp")
np_shp = readShapeSpatial("shps_np/sdr_subnational_boundaries2.shp")

dat = read_csv("stunting_rates.csv")
setnames(dat,"region.name","DHSREGEN")

bd_dat = fortify(bd_shp, region="DHSREGEN")
setnames(bd_dat,"id","DHSREGEN")
bd_dat = merge(bd_dat,dat,by="DHSREGEN")
et_dat = fortify(et_shp, region="DHSREGEN")
setnames(et_dat,"id","DHSREGEN")
et_dat = merge(et_dat,dat,by="DHSREGEN")
np_dat = fortify(np_shp, region="DHSREGEN")
setnames(np_dat,"id","DHSREGEN")
np_dat = merge(np_dat,dat,by="DHSREGEN")

vars = names(dat)[c(13:20)]

setwd("C:/Users/Alex/Documents/Data/GNR2018/stunting_map/statics")

for(var in vars){
  bd_map = ggplot(
    bd_dat,aes(long,lat,group=group,fill=get(var),colour="dimgrey")
  ) +
    geom_polygon() +
    expand_limits(x =bd_dat$long, y = bd_dat$lat) +
    scale_colour_identity() + 
    scale_fill_gradient2(
      labels = scales::percent,
      low = muted("green"),
      mid = "white",
      midpoint = 0,
      high = "red",
      limits = c(-0.06, 0.06),
      na.value="dimgrey") +
    guides(fill=guide_legend(title="")) +
    labs(x="",y="",title=paste("Bangladesh",paste(subset(dat,country=="Bangladesh")[1,c("Year 1","Year 2")],collapse="-"),var,"% change")) +
    theme_classic() +
    theme(axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
  
  et_map = ggplot(
    et_dat,aes(long,lat,group=group,fill=get(var),colour="dimgrey")
  ) +
    geom_polygon(data=subset(et_dat,!(DHSREGEN %in% c("Addis Abeba","Harari","Ben-Gumz")) )) +
    geom_polygon(data=subset(et_dat,(DHSREGEN %in% c("Addis Abeba","Harari","Ben-Gumz")) )) +
    expand_limits(x =et_dat$long, y = et_dat$lat) +
    scale_colour_identity() + 
    scale_fill_gradient2(
      labels = scales::percent,
      low = muted("green"),
      mid = "white",
      midpoint = 0,
      high = "red",
      limits = c(-0.06, 0.06),
      na.value="dimgrey") +
    guides(fill=guide_legend(title="")) +
    labs(x="",y="",title=paste("Ethiopia",paste(subset(dat,country=="Ethiopia")[1,c("Year 1","Year 2")],collapse="-"),var,"% change")) +
    theme_classic() +
    theme(axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
  
  np_map = ggplot(
    np_dat,aes(long,lat,group=group,fill=get(var),colour="dimgrey")
  ) +
    geom_polygon() +
    expand_limits(x =np_dat$long, y = np_dat$lat) +
    scale_colour_identity() + 
    scale_fill_gradient2(
      labels = scales::percent,
      low = muted("green"),
      mid = "white",
      midpoint = 0,
      high = "red",
      limits = c(-0.06, 0.06),
      na.value="dimgrey") +
    guides(fill=guide_legend(title="")) +
    labs(x="",y="",title=paste("Nepal",paste(subset(dat,country=="Nepal")[1,c("Year 1","Year 2")],collapse="-"),var,"% change")) +
    theme_classic() +
    theme(axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
  
  ggsave(paste0("Bangladesh ",var,".png"),plot=bd_map, width=6, height=5)
  ggsave(paste0("Ethiopia ",var,".png"),plot=et_map, width=8, height=5)
  ggsave(paste0("Nepal ",var,".png"),plot=np_map, width=9, height=5)
}
