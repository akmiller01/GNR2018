#### Startup ####

list.of.packages <- c("data.table","ggplot2","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Change WD to git repo if needed
wd <- "~/GNR"
setwd(wd)

dat = read.xlsx("Figure 3.5 Availability of fiber, iron, zinc and vitamin A at the global level and by income classification.xlsx",startRow = 2)

nutrients = unique(dat$nutrient)
for(the_nutrient in nutrients){
  dat.sub = subset(dat,nutrient==the_nutrient)
  unit_lab = paste0(dat.sub$unit[1],"/person/day")
  chart_title = paste0(the_nutrient,".png")
  p = ggplot(dat.sub,aes(x=year, group=income.classification)) + 
    geom_line(aes(y=mean, color=income.classification)) + 
    geom_ribbon(aes(ymin=lower.bound, ymax=upper.bound), alpha=0.2) +
    theme_classic() +
    labs(x="Year",y=unit_lab) + 
    guides(color=guide_legend(title=""))
  ggsave(chart_title,p,width=10,height=5)
}