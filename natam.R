##
# read natural amenities usda xls files
# add some columns containing metric explinations
##
require(data.table)
require(gdata) #read.xls

natam <- function() {
  if(file.exists('natdf.csv') ){
     natdf<-read.table('natdf.csv')

     
  } else {
     #########
     # read in natural amenties files
     # http://www.ers.usda.gov/datafiles/Natural_Amenities_Scale/natamenf_1_.xls
     natdf <- read.xls('natamenf_1_.xls',skip=105) 
     names(natdf) <- c('FIPS','CombCounty','State','County','Census.Division',
                       'RuralUrban','UrbanInfluence',
                       'Temp.Jan','SunDays.Jan',
                       'Temp.Jul','Humid.Jul', 
                       'Topo','WaterPrct','lnH2O',
                       'z.Temp.Jan', 'z.Sun.Jan','z.Temp.Jul','z.Humid.Jul',
                       'z.topo','z.water','NAS','NAS.rank' )
     toplabels <-  c('Flat plains','Smooth plains','Irregular plains, slight relief', 'Irregular plains', 
          'Tablelands, moderate relief', 'Tablelands, considerable relief', 'Tablelands, high relief','Tablelands, very high relief',
          'Plains with hills','Plains with high hills','Plains with low mountains','Plains with high mountains',
          'Open low hills', 'Open hills','Open high hills','Open low mountains', 'Open high mountains',
          'Hills','High hills','Low mountains','High mountain') 
     
     urbanlabels <- c('Central Metro >10^6','Fringe Metro >10^6','in metro >250000','in metro <250000',
                      '>20000 adj. metro','>20000 no metro','20000 > metro adj. > 2500', '20000 > no metro > 2500',
                      '<2500 adj metro', 'rural')
     
     urbaninfLabels <- c('Metro > 10^6','Metro < 10^6',
                         'Adj to large metro, city >10000','Adj to large metro, no city <10000',
                         'Adj to small metro, city >10000','Adj to small metro, no city <10000',
                         'no metro, city>10000','no metro, city<10000','no metro, city<2500')
     
     
     # name and group things
     natdf$TopoGrp <- cut(natdf$Topo, breaks=c(0,4,8,12,17,21),labels=c('Plains','Tablelands','Plains.Hills','OpenHills.Mnt','Hills.Mnt'))
     natdf$TopoName <- factor(natdf$Topo, 1:21,toplabels )
     
     natdf$RuralUrbanGrp  <- cut(natdf$RuralUrban, breaks=c(-1,3,9),labels=c('Metro','nonmetro'))
     natdf$RuralUrbanName <- factor(natdf$RuralUrban, 0:9, urbanlabels )
     
     natdf$UrbanInfGrp  <- cut(natdf$UrbanInfluence, breaks=c(0,2,9),labels=c('Metro','nonmetro'))
     natdf$UrbanInfName <- factor(natdf$UrbanInfluence, 1:9, urbaninfLabels )
     write.table(natdf,'natdf.csv')
  }
  # set FIPS to 5 digit string (get leading zeros)
  natdf$FIPS <- sprintf("%05d",natdf$FIPS)

  natdf<-data.table(natdf)
  setkey(natdf,FIPS)
  return(natdf)
}
