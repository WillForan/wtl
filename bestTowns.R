# get coords of best towns 
require(data.table)
require(ggmap) # for geocode
#########
besttowns <- function(txtfile='besttownGPS.txt') {
  if( file.exists(txtfile) ){
   return(read.table(txtfile))
  }

  # best towns
  # curl http://www.outsideonline.com/2006426/americas-best-towns-2015 | perl -lne 's/&nbsp;/ /g; print $1 while m:<h2>(.*?)</h2>:g'
  OObest <- read.table('OustideOnline_best2015',sep=".",stringsAsFactors=F)
  names(OObest)<-c('rank','town')
  OObest <- data.table(cbind(OObest,t(sapply(OObest$town,geocode))))
  setnames(OObest,c('lon','rank'),c('long', 'group') )
  OObest$lat <- unlist(OObest$lat)
  OObest$long <- unlist(OObest$long)
  if(!is.null(txtfile) ) write.table(txtfile)

  # lake placid look up fails, instead find new york city
  lakepidx <- grep('Lake Placid',OObest$town)
  OObest[lakepidx,c('lat','long') ] <- c(-73.99543,44.30449)

  return(OObest)
}
