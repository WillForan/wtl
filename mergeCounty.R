# merge natdf (with FIPS as key) to counties shape data frame for plotting
require(rgdal)
require(ggmap)
require(data.table)
#library(sp)
#library(maps)
mergeCounties <- function(natdf) {
   ######
   # http://opensciencecafe.org/tag/ggplot2/
   # http://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
   
   shape <- readOGR("countyshape/cb_2014_us_county_5m.shp","cb_2014_us_county_5m")
   # remove islands and alaska
   shape <- shape[!(shape@data$STATEFP %in% c("02","15","72","52","78","03","60","07","14","43","74","79","69","66")),] 
   
   #get data
   mapdata<-shape@data
   # useful to merge  fortify polygon
   mapdata$id <- rownames(mapdata)
   
   # add id so when we fortify we can merege back
   # set FIPS from state and county FP
   mapdata <- data.table(mapdata)
   mapdata[,FIPS:=paste0(STATEFP,COUNTYFP)]
   # merge data
   setkey(mapdata,FIPS)
   natmap<-mapdata[natdf,]
   
   gm <- data.table(fortify(shape))
   setkey(gm,id)
   setkey(natmap,id)
   alldata<-natmap[gm]
   return(alldata)
}

#coord_map(xlim=c(-125,-65),ylim=c(25,50)) + theme_bw()

##
# http://gis.stackexchange.com/questions/106944/ggmap-fortify-start-and-end-jumble
#WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#Counties <- readShapeSpatial("countyshape/cb_2014_us_county_5m.shp", proj4string = WGS84)

###
# http://stackoverflow.com/questions/23714052/ggplot-mapping-us-counties-problems-with-visualization-shapes-in-r
# m.usa <- map_data("county")
