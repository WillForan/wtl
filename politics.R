# 2012 presidential: https://www.google.com/fusiontables/DataSource?docid=1qcQLqrAIAe3RcEfdWSm_QcXMLmteVg4uSpSs1rM#rows:id=1
# https://www.google.com/fusiontables/exporttable?query=select+*+from+1qcQLqrAIAe3RcEfdWSm_QcXMLmteVg4uSpSs1rM&o=csv
require(data.table)
require(dplyr)
require(tidyr)
USPres2012 <- function(txtfile="USpres2012_percentVote.csv") {
   if( file.exists(txtfile) ){
      phs   <- read.table(txtfile) %>% mutate(FIPS=sprintf("%05d",FIPS)) 
   } else {

      pres <- data.table(read.csv2('USpresident2012.csv',sep=",",header=T))

      ph <- pres
      # winning canidate does not have dot notation for column names
      names(ph)[12:22] <- paste0(names(ph)[12:22],'.0')

      # each metric is it's own row
      phh <- gather(ph,metric,value,c(12:203)) %>% 
              # reduce columns
              select(FIPS.Code,County.Number,Precincts.Reporting,Total.Precincts,TOTAL.VOTES.CAST,metric,value)  %>%
              # remove redudant rows
              filter( !grepl('TOTAL.VOTES.CAST',metric) ) %>%
              filter( !grepl('Order',metric) ) %>%
              filter( FIPS.Code != 0) %>%
              mutate( FIPS.Code = sprintf("%05d",FIPS.Code)) %>%
              # rename values so we can reextract order
              mutate( metric = gsub('\\.\\+$','',metric) ) %>% 
              mutate( metric = gsub('\\.(\\d+)$','_\\1',metric) ) %>% 
              separate( metric, c('metric', 'order'),'_')  %>%
              # remove empty values
              filter(!is.na(value) & value != "" )  
      phw <- phh %>%
              unite(FIPS.County,FIPS.Code,County.Number) %>%
              # put back into semiwide format
              spread(metric,value) %>% 
              filter(!is.na(Votes))%>% 
              # reduce columns
              select(FIPS.County,Last.name,Party,TOTAL.VOTES.CAST,Votes) %>%
              unite(Canidate,Party,Last.name) %>% 
              spread(Canidate,Votes) %>% 
              separate(FIPS.County,c('FIPS','County'),'_')

      # collapse accross FIPS
      phs <- phw %>% group_by(FIPS) %>% select(-County) %>%
             summarise_each(funs(sum(as.numeric(.),na.rm=T))) %>%
             mutate_each(funs( ./TOTAL.VOTES.CAST),-FIPS,-TOTAL.VOTES.CAST)

      # save file
      write.table(file=txtfile,phs)
   }
   # see top canidates
   #phs %>% summarise <- each(funs(mean(.))) %>% gather() %>% filter(value > .001)

   r <- data.table( phs %>% select(FIPS,Dem_Obama,GOP_Romney,Grn_Stein,Lib_Johnson) )
   setkey(r,FIPS)
   return(r)

}
