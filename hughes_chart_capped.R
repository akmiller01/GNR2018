list.of.packages <- c("data.table","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/GNR")

chart.data = read.csv("Food per income group data.csv")
chart.data$class = factor(chart.data$class,levels=c("Low income", "Lower-middle", "Upper-middle","High income"))
food.order = c(
  "Calcium",
  # "Eggs",
  # "Fish",
  "Fruit",
  "Legumes",
  "Milk",
  "Nuts",
  "Omega 3",
  # "Poultry",
  "Processed Meat",
  "Red Meat",
  # "Refined Grain",
  "Sodium",
  "SSB",
  # "Starchy Veg",
  # "Total Dairy",
  # "Total Sugar",
  "Veg",
  "Whole Grain",
  "Polyunsaturated Fat",
  "Saturated Fat",
  "Trans Fat"
)
chart.data$percent = chart.data$value/chart.data$recommended
chart.data = subset(chart.data, food %in% food.order)
mean.tab = data.table(chart.data)[,.(mean_val=mean(percent)),by=.(food)]
mean.tab = mean.tab[order(-mean.tab$mean_val),]
food.order = mean.tab$food
chart.data = merge(chart.data,mean.tab,by="food",all.x=T)
chart.data = chart.data[order(-chart.data$mean_val),]
# food.order = food.order[order(food.order)]
# chart.data = chart.data[order(chart.data$food),]
chart.data$column = c(rep(1,30),rep(1,30))
chart.data$food = factor(chart.data$food,levels=rev(food.order))
max.percent = min(max(chart.data$percent,na.rm=T),2)

# Outliers
chart.data$outlier = 0
chart.data$outlier[which(chart.data$percent>2)] = 1
chart.data$percent[which(chart.data$percent>2)] = 2.1
# Manual jitter
chart.data$percent[which(chart.data$food=="Sodium" & chart.data$class=="High income")] = 2.2
chart.data$percent[which(chart.data$food=="Sodium" & chart.data$class=="Upper-middle")] = 2.3
chart.data$percent[which(chart.data$food=="Eggs" & chart.data$class=="High income")] = 2.2
chart.data$percent[which(chart.data$food=="SSB")] = c(2.4, 2.1, 2.2, 2.3)

bar.dat = unique(chart.data[c("food","recommended","column")])
bar.dat$class = "Low income"

# for(i in 1:2){
i = 1
  chart.data.sub = subset(chart.data,column==i)
  bar.dat.sub = subset(bar.dat,column==i)
  p = ggplot(chart.data.sub,aes(x=food,colour=class)) +
    geom_bar(data=bar.dat.sub,aes(y=max.percent),fill="white",color="black",stat="identity",width=0.4) +
    geom_bar(data=bar.dat.sub,aes(y=1),fill="white",color="black",stat="identity",width=0.4) +
    geom_point(aes(y=percent),alpha=0.8,size=3) +
    geom_text(aes(y=1,label=paste(recommended,unit)),color="black",vjust=-1.3) +
    geom_text(data=subset(chart.data.sub,outlier==1),aes(y=percent,label=round(value)),color="black",size=2) +
    coord_flip() +
    theme_classic() +
    theme(
      axis.title=element_blank(),
      axis.line=element_blank(),
      axis.ticks=element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size=12),
      legend.title=element_blank(),
      legend.text = element_text(size=12)
    )
  
  ggsave(paste0("recommended_values.png"),p,width=7.5,height=7.5)
# }


