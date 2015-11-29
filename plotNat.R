library(magrittr)
library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)
# natural amenities usda xls files
source('natam.R')
# mergeCounties - get polygons for FIPS
source('mergeCounty.R')
# http://www.outsideonline.com/2006426/americas-best-towns-2015 
source('bestTowns.R')
# USPres2012 
source('politics.R')

# get FIPS dfs
# -- natural amenities usda xls files
nat  <- natam()

# get percents for Dem_Obama,GOP_Romney,Grn_Stein,Lib_Johnson
politics <- USPres2012()

# get shapes by merging politics and nat
natpo <- politics[nat,]

####### SCORE

# natdf %>% filter(RuralUrban > 2,RuralUrban < 5, Topo>5,WaterPrct>3,Temp.Jul< 80,Temp.Jan<32) # this filter!
natpo %<>% mutate( wfscore = 
           ( (Grn_Stein > .01) + (Dem_Obama > 1.3*GOP_Romney) )/2 + 
           (Topo >18) + 
           ( (Temp.Jan < 32) + (Temp.Jan > 20)  )/2 + 
           (Temp.Jul < 80 ) + 
           (RuralUrban>2)   + 
           (RuralUrban<5)   +
           .5*(SunDays.Jan > 150 ) +
           (WaterPrct>3) )

####

# add map to FIPS
natmap <- mergeCounties(natpo)

# get best cities, unreated to FIPS
OObest <- besttowns()

# get read of lake placid because it renders incorrectly
OObest %<>% filter(!grepl('Lake P',town))

########

# panel-less map with state outlinse
states<-map_data("state")
maptheme <- function(p) {
  p +
  geom_polygon(data=states,color='black',alpha=0) + 
  ylab("")+xlab("")+
  geom_point(data=OObest,color='black',fill='white',aes(group=NA)) +
  geom_text(data=OObest,color='black',size=3,vjust=.8,hjust=.4,aes(label=gsub(',.*','',town))) +
  theme_bw() +
  theme(axis.text=element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) 
}

# general plot object
plotMap <- function(metric,low="white",high="green",mtitle=metric,...) {
  p<- ggplot(natmap,aes(x=long,y=lat,group=group)) +
   geom_polygon(aes_string(fill=metric)) + 
   scale_fill_gradient(low=low,high=high,...)   + ggtitle(mtitle)
  p %>% maptheme
}

mapplots <- list(
  plotMap('z.water',high="blue",mtitle="Water"),
  plotMap('Humid.Jul',mtitle="Humidity",high="orange"),
  plotMap('Temp.Jul',low="blue",high="orange",mtitle="Summer Temp") ,
  plotMap('Temp.Jan',low="blue",high="orange",mtitle="Winter Temp") ,
  plotMap('z.topo',mtitle="Topography"),
  plotMap('SunDays.Jan',mtitle="Winter Sun Hours",high="orange"),
  plotMap('RuralUrban',mtitle="Rural",high="white",low="grey"),
  plotMap('wfscore',high='blue',low='white',mtitle='Composite Score')
)



showsub <- function(d,...){
  d %>% mutate(Dem = sprintf('%02f',Dem_Obama*100)) %>%
   select(State,County,wfscore,TopoName,Topo,WaterPrct,RuralUrbanName,RuralUrban,Temp.Jan,Temp.Jul,Dem,SunDays.Jan,...) 

}

natpo %>% showsub %>%
  arrange(desc(wfscore),desc(WaterPrct),desc(Topo)) %>% head(n=20) %>% print.data.frame

natpo %>% filter(grepl('ALLEG',County),State=='PA') %>% showsub(FIPS) %>% print.data.frame


