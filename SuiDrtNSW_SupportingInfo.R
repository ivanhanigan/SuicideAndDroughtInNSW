### R code from vignette source 'SuiDrtNSW_SupportingInfo.Rnw'

###################################################
### code chunk number 1: SuiDrtNSW_SupportingInfo.Rnw:109-121
###################################################
######################
#tools,  Drought tools
######################



        if (!require(rgdal)) install.packages('rgdal'); require(rgdal)
        if (!require(geosphere)) install.packages('geosphere'); require(geosphere)
        if (!require(plyr)) install.packages('plyr'); require(plyr)
        if (!require(rgeos)) install.packages('rgeos'); require(rgeos)




###################################################
### code chunk number 2: SuiDrtNSW_SupportingInfo.Rnw:127-203
###################################################
######################
#tools,  dlMonthly
######################



        dlMonthly <- function(site, dataDir){
        # a function designed to download up to date rainfall station data from bom website
        wd <- getwd()
        setwd(dataDir)
        readLines(  sprintf('http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=139&p_display_type=dataFile&p_startYear=&p_stn_num=%s',site),
        n=1)
        download.file(sprintf('http://www.bom.gov.au/tmp/cdio/IDCJAC0001_%s.zip',site),
        sprintf('IDCJAC0001_%s.zip',site))
        # system(sprintf('sh getZipContents.sh IDCJAC0001_%s.zip',site))
        unzip(paste('IDCJAC0001_',site,'.zip',sep=''),junkpaths=T)
        setwd(wd)
        }

# THIS IS BROKEN
# USE THE DOWNLOADED HQ_PRCP DATA
################################################################
# name:ZipFunctions.R
uncompress_linux <- function(filename)
  {
    print(filename)
    system(sprintf('uncompress %s',filename))
  }

# tries to find 7 zip exe
ExecutableFileName7Zip <- function()
{
  executableName <- "C:\\Program Files\\7-Zip\\7z.exe"

  if(file.exists(executableName))
  {
    return (executableName)
  }

  #other executable file names and ideas go here ...
  stop("failed to find 7zip")
}

# simple function to extract 7zip file
# need to have 7zip installed
Decompress7Zip <- function(zipFileName, outputDirectory, delete)
{
  executableName <- ExecutableFileName7Zip()

# fileName = GetFileName(zipFileName)
# fileName = PathCombine(outputDirectory, fileName)


# if(file.exists(fileName))
# {
# unlink(zipFileName);
# }

  arguments <- paste(sep="",
                    "e ",
                    "\"", zipFileName, "\" ",
                    "\"-o", outputDirectory, "\" ",
    "")

  print( arguments)

  RunProcess(executableName, arguments)

  if(delete)
  {
    unlink(zipFileName);
  }
}

#test
# Decompress7Zip("D:\\Development\\Awap Work\\2013010820130108.grid.Z", "D:\\Development\\Awap Work\\", TRUE)


###################################################
### code chunk number 3: SuiDrtNSW_SupportingInfo.Rnw:208-318
###################################################
######################
#tools,  droughtIndex
######################



        droughtIndex<-function(data,years,droughtThreshold=.375){
        # a drought index based on integrated six-monthly rainfall percentiles.
        # based on Professor Mike Hutchinson's work described in
        # Smith D, Hutchinson M, McArthur R. Climatic and Agricultural Drought: Payments and Policy.
        # Canberra, ACT: Centre for Resource and Environmental Studies, Australian National University. 1992.

        # Ivan C Hanigan
        # June 2011.
        # GPL2
        # for updates please see https://github.com/ivanhanigan/HutchinsonDroughtIndex.

        # my input data are always a data.frame with 4 columns 'date','year','month','rain'

        #calculate M month totals
        # started with 6 (current and prior months)
        x<-ts(data[,4],start=1,end=c(years,12),frequency=12)
        x<-c(rep(NA,5),x+lag(x,1)+lag(x,2)+lag(x,3)+lag(x,4)+lag(x,5))
        data$sixmnthtot<-x
        data<-na.omit(data)

        # rank in percentage terms with respect to the rainfall totals
        # for the same sequence of 6-months over all years of record
        dataout_final=matrix(nrow=0,ncol=7)

        for(i in 1:12){
        x<-data[data$month==i,5]
        #x<-na.omit(x)
        y<-(rank(x)-1)/(length(x)-1)
        # checkpct<-cbind(data[data$month==i,],y)
        # plot(checkpct$sixmnthtot,checkpct$y)
        # rescale between -4 and +4 to replicate palmer index
        z<-8*(y-.5)
        # defualts set the threshold at -1 which is upper limit of mild drought in palmer index (3/8ths, or the 37.5th percentile)
        drought<-x<=quantile(x,droughtThreshold)
        # calculate the drought index for any months that fall below the threshold
        zd<-z*drought
        # save out to the data
        dataout<-data[data$month==i,]
        dataout$index<-z
        dataout$indexBelowThreshold<-zd
        dataout_final=rbind(dataout_final,dataout)
        }

        data<-dataout_final[order(dataout_final$date),]

        # now calculate the indices
        data$count<-as.numeric(0)

        for(j in 2:nrow(data)){
        data$count[j]<-ifelse(data$indexBelowThreshold[j]==0,0,
        ifelse(data$indexBelowThreshold[j-1]!=0,1+data$count[j-1],
        1)
        )
        }

        # enhanced drought revocation threshold
        # In the enhanced version rather than stop counting when the rescaled percentiles rise above -1.0,
        # we keep counting the months (or adding the negative anomalies)
        # if the rescaled percentile is below 0.0 AND the drought threshold has already been reached.
        # If the threshold has not been reached, then stop counting (or adding) as before
        # if the rescaled percentile rises above -1.0.

        data$count2<-data$count
        # j=1080 # 1980-06
        # data[j,]

        for(j in 2:nrow(data)){
        data$count2[j] <- if(data$count2[j-1] >= 5 & data$index[j] <= 0){
        data$count2[j-1] + 1
        } else {
        # ifelse(data$count[j-1] > 0 & data$index[j] < 0, 1+data$count[j-1],
        data$count2[j]
        }
        }


        data$sums<-as.numeric(0)

        for(j in 2:nrow(data)){
        data$sums[j]<-ifelse(data$indexBelowThreshold[j]==0,0,
        ifelse(data$indexBelowThreshold[j-1]!=0,
        data$indexBelowThreshold[j]+data$sums[j-1],
        data$indexBelowThreshold[j]))
        }


        data$sums2<-data$sums
        # j=1069 # 1980-06
        # data[j,]

        for(j in 2:nrow(data)){
        data$sums2[j] <- if(data$sums2[j-1] <= -17.5 & data$index[j] <= 0){
        data$sums2[j-1] + data$index[j]
        } else {
        # ifelse(data$count[j-1] > 0 & data$index[j] < 0, 1+data$count[j-1],
        data$sums2[j]
        }
        }

        droughtIndices<-data
        return(droughtIndices)
        }




###################################################
### code chunk number 4: SuiDrtNSW_SupportingInfo.Rnw:323-341
###################################################
######################
#tools,  create download directories
######################



        # create a data storage directory to store downloaded data

        bomDir <- file.path('data/bom_HQ_monthly_prcp')
        dir.create(bomDir, recursive = T)

        absDir <- file.path('data/abs_sd')
        dir.create(absDir, recursive = T)

        # and remember the project root directory
        rootdir <- getwd()




###################################################
### code chunk number 5: SuiDrtNSW_SupportingInfo.Rnw:349-387
###################################################
######################
#load,  Download spatial data
######################



        # newnode data download notes.
        # newnode change work dir to download area
        if(!file.exists(file.path(absDir,'aussd.Rdata'))){
        setwd(absDir)

        download.file('http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&SD06aAUST.zip&1259.0.30.002&Data%20Cubes&56AEC033DFC11A5CCA2571BF007E5185&0&2006&04.08.2006&Latest', 'SD06aAUST.zip', mode = 'wb')
        unzip('SD06aAUST.zip',junkpaths=T)

        sink('readme.txt')
        cat(paste('Australian Bureau of Statistics Statistical Divisions 2006
  downloaded on', Sys.Date(),
  '
  from http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1259.0.30.0022006?OpenDocument')
  )
        sink()

        # and load
        sd <- readOGR('SD06aAUST.mif', layer = 'SD06aAUST')
        # might take a while
        head(sd@data)
        plot(sd)
        dev.off()
        save.image('aussd.Rdata')

        } else {
        # OR if already loaded
        setwd(absDir)
        load('aussd.Rdata')
        }
        # NB You may want to change the code to calculate the index for another type of ABS spatial unit.  If so you can find data at http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1259.0.30.0022006?OpenDocument




###################################################
### code chunk number 6: SuiDrtNSW_SupportingInfo.Rnw:392-413
###################################################
######################
#load,  subset the SDs to NSW
######################


 # sd@data$SD_NAME_2006 == 'Central West' &
        sd2 <-  sd[ sd@data$STATE_CODE_2006 == 1,]
        plot(sd2)
        axis(1);axis(2); box()
        plot(sd, add = T)
        names(sd2@data)
        # writeOGR(sd2,'centralwestsd.shp','centralwestsd','ESRI Shapefile')
        # test <- readShapePoly('centralwestsd.sd')
        # not work? ignore
        rm(sd)
        save.image('nswsd.Rdata')

        # newnode get the centroid of the Central West
        coords <- centroid(sd2[sd2@data$SD_NAME_2006 == 'Central West' &  sd2@data$STATE_CODE_2006 == 1,])




###################################################
### code chunk number 7: SuiDrtNSW_SupportingInfo.Rnw:418-443
###################################################
######################
#load,  subset the SDs to Vic
######################


 load('aussd.Rdata')

        sd2 <-  sd[ sd@data$STATE_CODE_2006 == 2,]
        plot(sd2)
        axis(1);axis(2); box()
        # Look up Seymour coordinates from Wikipedia
        points(145.13, -37.03, pch = 16)
        names(sd2@data)
        sd3 <- sd2[sd2@data$SD_NAME_2006 == 'Goulburn' &  sd2@data$STATE_CODE_2006 == 2,]
        plot(sd3, add = T, col = 'grey')
        points(145.13, -37.03, pch = 16)

        rm(sd)
        save.image('vicsd.Rdata')

        # newnode get the centroid of the Seymour SD
        coords <- centroid(sd3)

        setwd(rootdir)



###################################################
### code chunk number 8: SuiDrtNSW_SupportingInfo.Rnw:449-489
###################################################
######################
#load,  Download the Rainfall Station location data
######################


if(!file.exists(file.path(bomDir,'HQ_monthly_prcp_stations.csv'))){

        setwd(bomDir)
        sink('readme.txt')
        cat(paste('Bureau of Meteorology High Quality Monthly precipitation data
 downloaded on', Sys.Date(),
 '
 from ftp://ftp.bom.gov.au/anon/home/ncc/www/change/HQmonthlyR/HQ_monthly_prcp_txt.tar')
 )
        sink()
        download.file('ftp://ftp.bom.gov.au/anon/home/ncc/www/change/HQmonthlyR/HQ_monthly_prcp_txt.tar','HQ_monthly_prcp_txt.tar',mode='wb')
        untar('HQ_monthly_prcp_txt.tar', exdir= 'HQ_monthly_prcp_txt')

        # check
        d <- read.table('HQ_monthly_prcp_txt/HQMR_stations.txt',header=F,skip=0,nrow=1,as.is=T)
        d
        # ok fixed width
        nchar(d)

        # V1 V2 V3 V4 V5 V6
        # 4  6  6  2  9 11
        # actually not correct
        widths <- c(7,7,7,7,41)

        d2 <- read.fwf('HQ_monthly_prcp_txt/HQMR_stations.txt',widths=widths,header=F,skip=0,as.is=T,comment.char='|',strip.white=T)
        str(d2)
        head(d2)
        tail(d2)
        write.csv(d2,'HQ_monthly_prcp_stations.csv', row.names = F)
} else {
        setwd(bomDir)
        d2 <- read.csv('HQ_monthly_prcp_stations.csv')
}




###################################################
### code chunk number 9: SuiDrtNSW_SupportingInfo.Rnw:494-501
###################################################
######################
#load,  revert to project root dir
######################


 setwd(rootdir)



###################################################
### code chunk number 10: SuiDrtNSW_SupportingInfo.Rnw:506-544
###################################################
######################
#load,  Plot the NSW SD and stations
######################



        load(file.path(absDir,'nswsd.Rdata'))
        epsg <- make_EPSG()

        d2 <- read.csv(file.path(bomDir,'HQ_monthly_prcp_stations.csv'))
        ## Treat data frame as spatial points
        pt.stations <- SpatialPointsDataFrame(cbind(d2$V3,d2$V2),d2,
        proj4string=CRS(epsg$prj4[epsg$code %in% '4283']))


        # get distances
        coords <- gCentroid(sd2[sd2@data$SD_NAME_2006 == 'Central West' &  sd2@data$STATE_CODE_2006 == 1,])
        summary(pt.stations)
        dist2pt <- distVincentyEllipsoid(pt.stations,coords)

        d <- pt.stations[which(dist2pt<150000),]
        head(d@data)

        d@data

        # make a map of the region
        png(file.path(rootdir,'nswsds.png'),res=200,width = 1500, height = 1000)
        plot(sd2, col = 'grey', xlim = c(140,155))
        box();axis(1);axis(2)
        plot(pt.stations, add = T)
        points(coords)
        # plot(sd2, col = 'darkgrey', add= T)
        plot(d, pch = '*', cex = 2.5, add = T, col = 'red')
        # text(pt.stations$V3,pt.stations$V2,pt.stations$V5,cex=0.5)
        points(coords, pch = 16)
        dev.off()




###################################################
### code chunk number 11: SuiDrtNSW_SupportingInfo.Rnw:549-625
###################################################
######################
#load,  go for a SD wide average rainfall using these stations
######################



        setwd(file.path(bomDir,"HQ_monthly_prcp_txt"))
        df4 <- matrix(nrow=0,ncol=4)
        for(i in 2:nrow(d@data)){
        # i <- 2
        filename <- paste('0',as.character(d@data[i,1]),sep='')
        fname_i <- paste("prcphq.",filename,'.month.txt.Z',sep='')
        fname_out <- gsub("\\.", "_", gsub(".Z", "", fname_i))
        if(!file.exists(fname_out)){
        #this no work anymore
          # dlMonthly(filename, getwd())
          
        Decompress7Zip(zipFileName=fname_i, 
                       outputDirectory=fname_out,
                       delete=T)
        }
        #df <- read.csv(paste('IDCJAC0001_', filename,'_Data1.csv',sep=''))
        df <- read.table(dir(fname_out, full.names = T), skip = 1)
        names(df) <- c("startdate", "enddate", "rainfall")
        #df$date <- as.Date(paste(df$Year,df$Month,1,sep='-'))
        df$date <- as.Date(paste(
          substr(df$startdate,1,4)
          ,
          substr(df$startdate,5,6)
          ,1,sep='-'))
        #df<-subset(df,Quality == 'Y',select=c(date,Year,Month,Monthly.Precipitation.Total..millimetres.))
        head(df)
        fulldaterange <- as.data.frame(seq(min(df$date),max(df$date),1))
        fulldaterange$day <- substring(fulldaterange[,1],9,10)
        fulldaterange <- subset(fulldaterange, day == '01')
        names(fulldaterange) <- c('date','day')
        df2 <- merge(fulldaterange,df,all.x=T)
        #df2 <- subset(df2, select = c(date,Year, Month,Monthly.Precipitation.Total..millimetres.))
        df2 <- subset(df2, select = c(date,rainfall))
        df2$Year <- substr(df2$date,1,4)
        df2$Month <- substr(df2$date,6,7)
        tail(df2)
        # what happens with NAs?
        # stupid impute
        df2$rain <- as.numeric(0)
        # subset(df2,is.na(df2$Monthly.Precipitation.Total..millimetres.))
        df2$month <- substring(df2[,'date'],6,7)
        df2$year  <- substring(df2[,'date'],1,4)

        for(i in 1:nrow(df2)){
        # i <- 1
        mm <- df2[i,'month']
        #df2$rain[i] <- ifelse(is.na(df2$Monthly.Precipitation.Total..millimetres.[i]),
        #mean(subset(df2, month == mm,select = c(Monthly.Precipitation.Total..millimetres.)),na.rm=T),
        #df2$Monthly.Precipitation.Total..millimetres.[i])
        df2$rain[i] <- ifelse(is.na(df2$rainfall[i]),
        mean(subset(df2, month == mm,select = c(rainfall)),na.rm=T),
        df2$rainfall[i])

        }

        tail(df2)

        table(df2$year)
        df3 <- subset(df2, year > min(year) & year < max(year), select = c(date, year, month, rain))
        df3$year <- as.numeric(df3$year)
        df3$month <- as.numeric(df3$month)

        df4 <- rbind(df4, df3)
        }
        setwd(rootdir)
        # newnode average for entire sd
        df5 <- ddply(df4, c('date','year', 'month'), function(df)mean(df$rain))
        names(df5) <- c('date',  'year' , 'month' ,'rain')




###################################################
### code chunk number 12: SuiDrtNSW_SupportingInfo.Rnw:644-677
###################################################
######################
#do,  Calculate the Drought Index
######################




        drt <- droughtIndex(data=df5,years=length(names(table(df5$year))))
        write.csv(drt, "CentralWestNSWDrought18812012.csv", row.names = F)
        qc3=drt[drt$year>=1979 & drt$year < 1984,]

        png(file.path(rootdir,'CentralWestDrought8283.png'),res=200,width = 2100, height = 1000)
        par(mfrow=c(4,1),mar=c(2.5,2,1.5,1))
        plot(qc3$date,qc3$rain,type='l',main='Central West NSW: raw monthly rainfall')
        #points(qc3$date,qc3$rain)

        lines(qc3$date,qc3$sixmnthtot/6, lwd = 2) #,type='l',main='6-monthly total rainfall')
        points(qc3$date,qc3$sixmnthtot/6)

        plot(qc3$date,qc3$index,type='l',main='Rescaled percentiles -4 to +4, -1 is Palmer Index Mild Drought',ylim=c(-4,4))
        points(qc3$date,qc3$index)
        segments(min(qc3$date),-1,max(qc3$date),-1)
        segments(min(qc3$date),0,max(qc3$date),0,lty=2)
        plot(qc3$date,qc3$count,type='l',main='Counts below -1 threshold, count of 5 or more is a drought')
        points(qc3$date,qc3$count)
        segments(min(qc3$date),5,max(qc3$date),5)

        plot(qc3$date,qc3$count2,type='l',main='Enhanced counts of months if already passed count of 5 and percentiles less than 50%')
        points(qc3$date,qc3$count2)
        segments(min(qc3$date),5,max(qc3$date),5)
        dev.off()




###################################################
### code chunk number 13: SuiDrtNSW_SupportingInfo.Rnw:682-690
###################################################
######################
#do,  replicate Fig3.5 from Hutchinson
######################


# Figure 3.5 from the original paper shows the index at Seymour Victoria 1966-1986.
        # This town is in the Goulburn Statistical Division



###################################################
### code chunk number 14: SuiDrtNSW_SupportingInfo.Rnw:695-732
###################################################
######################
#load,  plot the Victorian SD and stations
######################



        load(file.path(absDir,'vicsd.Rdata'))
        epsg <- make_EPSG()

        ## Treat data frame as spatial points
        pt.stations <- SpatialPointsDataFrame(cbind(d2$V3,d2$V2),d2,
        proj4string=CRS(epsg$prj4[epsg$code %in% '4283']))


        # get distances
        coords <- centroid(sd2[sd2@data$SD_NAME_2006 == 'Goulburn' &  sd2@data$STATE_CODE_2006 == 2,])
        summary(pt.stations)
        dist2pt <- distVincentyEllipsoid(pt.stations,coords)

        d <- pt.stations[which(dist2pt<150000),]
        head(d@data)

        d@data

        # make a map of the region
        png(file.path(rootdir,'vicsds.png'),res=200,width = 1500, height = 1000)
        plot(sd2, col = 'grey', xlim = c(140,155))
        box();axis(1);axis(2)
        plot(pt.stations, add = T)
        points(coords)
        # plot(sd2, col = 'darkgrey', add= T)
        plot(d, pch = '*', cex = 2.5, add = T, col = 'red')
        # text(pt.stations$V3,pt.stations$V2,pt.stations$V5,cex=0.5)
        points(coords, pch = 16)
        dev.off()




###################################################
### code chunk number 15: SuiDrtNSW_SupportingInfo.Rnw:737-791
###################################################
######################
#load,  SD wide average
######################



        setwd(bomDir)
        df4 <- matrix(nrow=0,ncol=4)
        for(i in 1:nrow(d@data)){
        # i <- 1
        filename <- paste('0',as.character(d@data[i,1]),sep='')
        if(!file.exists(paste('IDCJAC0001_', filename,'_Data1.csv',sep=''))){
        dlMonthly(filename, getwd())
        }
        df <- read.csv(paste('IDCJAC0001_', filename,'_Data1.csv',sep=''))
        df$date <- as.Date(paste(df$Year,df$Month,1,sep='-'))
        df<-subset(df,Quality == 'Y',select=c(date,Year,Month,Monthly.Precipitation.Total..millimetres.))
        head(df)
        fulldaterange <- as.data.frame(seq(min(df$date),max(df$date),1))
        fulldaterange$day <- substring(fulldaterange[,1],9,10)
        fulldaterange <- subset(fulldaterange, day == '01')
        names(fulldaterange) <- c('date','day')
        df2 <- merge(fulldaterange,df,all.x=T)
        df2 <- subset(df2, select = c(date,Year, Month,Monthly.Precipitation.Total..millimetres.))
        # what happens with NAs?
        # stupid impute
        df2$rain <- as.numeric(0)
        # subset(df2,is.na(df2$Monthly.Precipitation.Total..millimetres.))
        df2$month <- substring(df2[,'date'],6,7)
        df2$year  <- substring(df2[,'date'],1,4)

        for(i in 1:nrow(df2)){
        # i <- 1
        mm <- df2[i,'month']
        df2$rain[i] <- ifelse(is.na(df2$Monthly.Precipitation.Total..millimetres.[i]),
        mean(subset(df2, month == mm,select = c(Monthly.Precipitation.Total..millimetres.)),na.rm=T),
        df2$Monthly.Precipitation.Total..millimetres.[i])
        }

        tail(df2)

        table(df2$year)
        df3 <- subset(df2, year > min(year) & year < max(year), select = c(date, year, month, rain))
        df3$year <- as.numeric(df3$year)
        df3$month <- as.numeric(df3$month)

        df4 <- rbind(df4, df3)
        }
        setwd(rootdir)
        # newnode average for entire sd
        df5 <- ddply(df4, c('date','year', 'month'), function(df)mean(df$rain))
        names(df5) <- c('date',  'year' , 'month' ,'rain')




###################################################
### code chunk number 16: SuiDrtNSW_SupportingInfo.Rnw:796-829
###################################################
######################
#do,  Seymour drought index
######################




        drt <- droughtIndex(data=df5,years=length(names(table(df5$year))))
        qc3=drt[drt$year>=1966 & drt$year < 1986,]

        png(file.path(rootdir,'SeymourDrought6686.png'),res=200,width = 1500, height = 1000)
        par(mfrow=c(4,1),mar=c(2.5,2,1.5,1))
        plot(qc3$date,qc3$rain,type='l',main='Seymour VIC: raw monthly rainfall')
        #points(qc3$date,qc3$rain)
        axis(1,at=as.Date(paste(1966:1985,1,1,sep='-')), labels = 1966:1985)
        lines(qc3$date,qc3$sixmnthtot/6, lwd = 2) #,type='l',main='6-monthly total rainfall')
        points(qc3$date,qc3$sixmnthtot/6)
        axis(1,at=as.Date(paste(1966:1985,1,1,sep='-')), labels = 1966:1985)
        plot(qc3$date,qc3$index,type='l',main='rescaled percentiles -4 to +4, -1 is Palmer Index Mild Drought',ylim=c(-4,4))
        points(qc3$date,qc3$index)
        segments(min(qc3$date),-1,max(qc3$date),-1)
        segments(min(qc3$date),0,max(qc3$date),0,lty=2)
        plot(qc3$date,qc3$sums,type='l',main='sums below -1 threshold, sums of -17.5 or less is a drought')
        points(qc3$date,qc3$sums)
        segments(min(qc3$date),-17.5,max(qc3$date),-17.5)
        axis(1,at=as.Date(paste(1966:1985,1,1,sep='-')), labels = 1966:1985)
        plot(qc3$date,qc3$sums2,type='l',main='enhanced sums of months if already passed threshold of -17.5 and percentiles less than 50%')
        points(qc3$date,qc3$sums2)
        segments(min(qc3$date),-17.5,max(qc3$date),-17.5)
        axis(1,at=as.Date(paste(1966:1985,1,1,sep='-')), labels = 1966:1985)
        dev.off()




###################################################
### code chunk number 17: SuiDrtNSW_SupportingInfo.Rnw:848-882
###################################################
######################
#do,  Integration by Conditional Summation
######################



        # when is there an example of the enhancement making a drought longer?
        tail(drt[drt$sums2!=drt$sums,])
        # plot this one
        qc3=drt[drt$year>=1994 & drt$year < 1999,]

        png(file.path(rootdir,'SeymourDrought9499enhanced.png'),res=200,width = 2100, height = 1000)
        par(mfrow=c(4,1),mar=c(2.5,2,1.5,1))
        plot(qc3$date,qc3$rain,type='l',main='Seymour VIC: raw monthly rainfall')
        #points(qc3$date,qc3$rain)
        axis(1,at=as.Date(paste(1994:1998,1,1,sep='-')), labels = 1994:1998)
        lines(qc3$date,qc3$sixmnthtot/6, lwd = 2) #,type='l',main='6-monthly total rainfall')
        points(qc3$date,qc3$sixmnthtot/6)
        axis(1,at=as.Date(paste(1994:1998,1,1,sep='-')), labels = 1994:1998)
        plot(qc3$date,qc3$index,type='l',main='rescaled percentiles -4 to +4, -1 is Palmer Index Mild Drought',ylim=c(-4,4))
        points(qc3$date,qc3$index)
        segments(min(qc3$date),-1,max(qc3$date),-1)
        segments(min(qc3$date),0,max(qc3$date),0,lty=2)
        plot(qc3$date,qc3$sums,type='l',main='sums below -1 threshold, sums of -17.5 or less is a drought')
        points(qc3$date,qc3$sums)
        segments(min(qc3$date),-17.5,max(qc3$date),-17.5)
        axis(1,at=as.Date(paste(1994:1998,1,1,sep='-')), labels = 1994:1998)
        plot(qc3$date,qc3$sums2,type='l',main='enhanced sums of months if already passed threshold of -17.5 and percentiles less than 50%')
        points(qc3$date,qc3$sums2)
        segments(min(qc3$date),-17.5,max(qc3$date),-17.5)
        axis(1,at=as.Date(paste(1994:1998,1,1,sep='-')), labels = 1994:1998)
        dev.off()




###################################################
### code chunk number 18: SuiDrtNSW_SupportingInfo.Rnw:892-947
###################################################
######################
#analysis_data,  Extract preprocessed data from database
######################



        # The data were pre-processed using a PostgreSQL 8.4 database with PostGIS 1.5.
        # The analyses were performed on a linux R server R 2.13.1 with mgcv 1.7-6
        # if you have ethical approval from the ANU and the ABS then you will be able to access these data
        # and replicate the results using the following code.

        connect2postgres <- function(hostip=NA,db=NA,user=NA, p=NA, os = 'linux', pgutils = c('/home/ivan/tools/jdbc','c:/pgutils')){
        if(is.na(hostip)){
        hostip=readline('enter hostip: ')
        }
        if(is.na(db)){
        db=readline('enter db: ')
        }
        if(is.na(user)){
        user=readline('enter user: ')
        }
        if(is.na(p)){
        pwd=readline('enter password (! it will be shown, use ctrl-L to clear the console after): ')
        } else {
        pwd <- p
        }
        if(os == 'linux'){
        if (!require(RPostgreSQL)) install.packages('RPostgreSQL', repos='http://cran.csiro.au'); require(RPostgreSQL)
        con <- dbConnect(PostgreSQL(),host=hostip, user= user, password=pwd, dbname=db)
        } else {
        if (!require(RJDBC)) install.packages('RJDBC'); require(RJDBC)
        # This downloads the JDBC driver to your selected directory if needed
        if (!file.exists(file.path(pgutils,'postgresql-8.4-701.jdbc4.jar'))) {
        dir.create(pgutils,recursive =T)
        download.file('http://jdbc.postgresql.org/download/postgresql-8.4-701.jdbc4.jar',file.path(pgutils,'postgresql-8.4-701.jdbc4.jar'),mode='wb')
        }
        # connect
        pgsql <- JDBC( 'org.postgresql.Driver', file.path(pgutils,'postgresql-8.4-701.jdbc4.jar'))
        con <- dbConnect(pgsql, paste('jdbc:postgresql://',hostip,'/',db,sep=''), user = user, password = pwd)
        }
        # clean up
        rm(pwd)
        return(con)
        }
        ch <- connect2postgres()
        # enter password at console

        data <- dbGetQuery(ch,
        "
  select cast(dthyy || '-' || dthmm || '-' || 1 as date) as time, *
        from ivan_hanigan.suicidedroughtnsw19702007_rates_drought
        ")





###################################################
### code chunk number 19: SuiDrtNSW_SupportingInfo.Rnw:952-1022
###################################################
######################
#analysis_data,  Load data to R server
######################


 # preprocessing the data, some libraries, define some helpful functions and variables
        if (!require(mgcv)) install.packages('mgcv', repos='http://cran.csiro.au'); require(mgcv)
        if (!require(splines)) install.packages('splines', repos='http://cran.csiro.au'); require(splines)

        # Log transform drought variable, see data preparation for that diagnostic
        data$logDroughtCount = log1p(data$avcount)

        # set up the formats of these variables
        data$time=as.Date(paste(data$dthyy,data$dthmm,1,sep='-'))
        data$dthmm=as.factor(data$dthmm)
        data$mm=as.numeric(data$dthmm)

        # set up timevar for sinusoidal want
        timevar <- as.data.frame(names(table(data$time)))
        index <- 1:length(names(table(data$time)))
        timevar$time2 <- index/ (length(index) / (length(index)/12))
        names(timevar) <- c('time','timevar')
        timevar$time <- as.Date(timevar$time)
        data <- merge(data,timevar)
        data$time <- as.numeric(data$time)
        data$agegp <- as.factor(data$agegp)
        data$sd_group <- as.factor(data$sd_group)
        str(data)


        # a function to get Akaike's and Schwarz's Bayesian information criteria.
        # named after stata function
        estat <- function(modGLM,modName,createCsv=F){
        if(!exists('aic_table')) {aic_table=matrix(ncol=5,nrow=0)}

        estats <- c(modName,
        length(coef(modGLM)),
        AIC(modGLM),
        AIC(modGLM, k = log(nrow(data))),
        ((modGLM$null.deviance - modGLM$deviance)/ modGLM$null.deviance)*100
        )

        aic_table=rbind(aic_table,estats)

        # write to csv
        if(createCsv==T){
        write.table(as.data.frame(t(estats)), 'aic_table.csv', sep=',', row.names=F, append=F, col.names=F)
        } else {
        write.table(as.data.frame(t(estats)), 'aic_table.csv', sep=',', row.names=F, append=T, col.names=F)
        }
        return(aic_table)
        }

        #############################################################################
        # GS Medalla
        # original S code from http://www.math.yorku.ca/Who/Faculty/Monette/S-news/0422.html
        # The formula for pseudo-R^2 is taken from G. S. Maddalla,
        # Limited-dependent and Qualitative Variables in Econometrics, Cambridge:Cambridge Univ. Press, 1983. page 40, equation 2.50.
        Rsquared.glm.gsm <- function(o) {
        n <- length(o$residuals)
        ll <- logLik(o)[1]
        ll_0 <- logLik(update(o,~1))[1]
        R2 <- (1 - exp(((-2*ll) - (-2*ll_0))/n))/(1 - exp( - (-2*ll_0)/n))
        names(R2) <- 'pseudo.Rsquared'
        R2
        }






###################################################
### code chunk number 20: SuiDrtNSW_SupportingInfo.Rnw:1028-1132
###################################################
######################
#data,  Descriptive Statistics of Drought and Suicide
######################


 # descriptives
        if (!require(sqldf)) install.packages('sqldf', repos='http://cran.csiro.au'); require(sqldf)

        data$time=as.Date(paste(data$dthyy,data$dthmm,1,sep='-'))
        descstatOut <- matrix(nrow=0, ncol = 16)

        for(sdgrp in names(table(data$sd_group))){
        #  sdgrp <- 'Central West'
        descstat <- subset(data, sd_group == sdgrp & agegp == '10_19' & sex == 1)
        drt <- 1
        descstat$drought <- as.numeric(0)
        for(i in 2:nrow(descstat)){
        # i <- 417
        ith <- names(table(descstat$time))[i]
        iminusone <- names(table(descstat$time))[i-1]
        if(min(descstat[descstat$time == ith,'avcount']) >= 5){
        descstat[descstat$time == ith,'drought'] <-  drt
        } else {
        descstat[descstat$time == ith,'drought'] <-  0
        }
        if(min(descstat[descstat$time == ith,'avcount']) < 5 & min(descstat[descstat$time == iminusone,'avcount']) >= 5){
        drt <- drt + 1
        }

        }

        descstatOut <- rbind(descstatOut, descstat)
        }

        str(descstatOut)

        subset(descstatOut, sd_group == 'Central West' & drought == 3)
        # TASK CHECK THIS IS BECAUSE OF THE AVERAGING OVER MANY PIXELS

        sqldf("select sd_group, drought,  max(avcount), min(dthyy), max(dthyy)
 from descstatOut
        where drought >= 1 and sd_group = 'Central West'
        group by sd_group, drought
        ")

        # avg length and number of droughts

        descDrt <- sqldf('
 select sd_group, max(drought) as numberOfDroughts, avg(maxavcount) as avgDuration, max(maxavcount) as maxDuration
        from (
        select sd_group, drought,  max(avcount) as maxavcount, min(dthyy), max(dthyy)
        from descstatOut
        where drought >= 1
        group by sd_group, drought
        ) t1
        group by sd_group
        ')
 descDrt
        # sd_group max(drought) avg(maxavcount) max(maxavcount)
        # 1           Central West            9        7.994281        11.56618
        # 2                 Hunter           11        7.375160        14.77465
        # 3              Illawarra            7        8.920000        16.48000
        # 4        Mid-North Coast            8        8.420635        14.69841
        # 5                 Murray            7        7.732348        11.01724
        # 6           Murrumbidgee           10        7.010448        11.02239
        # 7  North and Far Western            8        7.197887        11.53284
        # 8               Northern            5        8.465896        10.99422
        # 9         Richmond-Tweed           13        8.230769        16.95238
        # 10         South Eastern            8        7.906818        11.02727
        # 11                Sydney            9        8.764706        20.00000
        write.table(descDrt, 'descriptivesDrought.csv', sep = ',', row.names=F)

        # Descriptive statistics for suicide.
        desc <- sqldf('
 select sd_group, avg(summary) as avgMonthlyDeaths, avg(pop) as avgPop, ((12 * avg(summary)) / avg(pop)) * 100000 as rate
        from (
        select sd_group, dthyy, dthmm, sum(deaths) as summary, sum(pop) as pop
        from data
        group by sd_group, dthyy, dthmm
        ) t1
        group by sd_group
        ')
 desc
        # sd_group avg(summary)  avg(pop)     rate
        # 1           Central West    1.5198238  138202.2 13.19653
        # 2                 Hunter    4.5308370  430402.7 12.63237
        # 3              Illawarra    3.0462555  280036.9 13.05366
        # 4        Mid-North Coast    1.9074890  183520.6 12.47264
        # 5                 Murray    0.9735683   86220.5 13.54993
        # 6           Murrumbidgee    1.3303965  118778.3 13.44081
        # 7  North and Far Western    1.5462555  114460.5 16.21089
        # 8               Northern    1.7268722  146464.5 14.14845
        # 9         Richmond-Tweed    1.6629956  139356.2 14.32010
        # 10         South Eastern    1.5748899  135091.0 13.98959
        # 11                Sydney   34.0044053 3040952.3 13.41859
        write.table(desc, 'descriptives.csv', sep = ',', row.names=F)
        # qc
        sqldf("select sd_group, dthyy, dthmm, sum(deaths) as summary, sum(pop) as pop
 from data
        group by sd_group, dthyy, dthmm
        having sd_group = 'Sydney'
        ")




###################################################
### code chunk number 21: SuiDrtNSW_SupportingInfo.Rnw:1202-1218
###################################################
######################
#data,  Correlation between Temperature and Drought
######################



        correlations <- as.data.frame(matrix(c(
        'cor(logDroughtCount,tmax)',  cor(data$logDroughtCount,data$tmax),
        'cor(logDroughtCount,tmaxanomaly)',  cor(data$logDroughtCount,data$tmax_anomaly),
        'cor(tmax,tmaxanomaly)', cor(data$tmax,data$tmax_anomaly)
        ),
        ncol=2, byrow=TRUE))
        names(correlations) <- c('Variables','Correlation')
        write.table(correlations[order(correlations[,2]),],'correlations.csv',row.names=F,sep=',')




###################################################
### code chunk number 22: Correlations
###################################################
library(xtable)
foo <- read.csv('correlations.csv')

print(xtable(foo, caption = 'Correlations', label = 'tab:Correlations',
        digits = , align = ), table.placement = 'ht',
        caption.placement = 'top',include.rownames=F)


###################################################
### code chunk number 23: SuiDrtNSW_SupportingInfo.Rnw:1243-1252
###################################################
######################
#do,  Core Model Diagnostics and Variable Selection
######################








###################################################
### code chunk number 24: SuiDrtNSW_SupportingInfo.Rnw:1269-1307
###################################################
######################
#do,  core model
######################



        # first check full interaction of age, sex, region and trend, season as sinusoidal
        ageSexRegionTrend <- glm(deaths ~ sin(timevar*2*pi) + cos(timevar*2*pi) +
        sd_group * agegp *  sex * ns(time,3) +
        offset(log(pop)), data=data,family=poisson)

        aic_table <- estat(ageSexRegionTrend, 'sd_group*age*sex*ns(time,df=3)')

        # then drop the interaction with trend
        ageSexTrend <- glm(deaths ~ sin(timevar*2*pi) + cos(timevar*2*pi) +
        sd_group +
        agegp *  sex * ns(time,3) +
        offset(log(pop)), data=data,family=poisson)

        aic_table <- estat(ageSexTrend, 'age*sex*ns(time,df=3)')

        # significance of terms
        drop1(ageSexTrend, test='Chisq', k = log(nrow(data)))

        # check sinusoidal with extra wiggle
        ageSexTrendSineXtra <- glm(deaths ~ ns(sin(timevar*2*pi),3) + cos(timevar*2*pi) +
        sd_group +
        agegp *  sex * ns(time,3) +
        offset(log(pop)), data=data,family=poisson)
        aic_table <- estat(ageSexTrendSineXtra , 'ageSexTrendSineXtra')

        # diagnostic plots
        png('coreModelDiagnosticPlot.png',res=200,width = 1500, height = 1000)
        par(mfrow=c(2,2))
        plot(ageSexTrend)
        dev.off()




###################################################
### code chunk number 25: SuiDrtNSW_SupportingInfo.Rnw:1312-1329
###################################################
######################
#do,  check for overdispersion
######################



        # check for overdispersion using quasipoisson
        ageSexTrendQuasi <- glm(deaths ~ sin(timevar*2*pi) + cos(timevar*2*pi) +
        sd_group +
        agegp *  sex * ns(time,3) +
        offset(log(pop)), data=data,family=quasipoisson)

        summary(ageSexTrendQuasi)
        # NOTE: (Dispersion parameter for quasipoisson family taken to be 0.9424658)
        # so Poisson model will be OK




###################################################
### code chunk number 26: SuiDrtNSW_SupportingInfo.Rnw:1345-1397
###################################################
######################
#do,  check climate
######################


# dont seem to be able to fit the sine wave in GAMs?
        # fit as spline instead just for these.

        droughtModel <- gam(deaths ~ s(logDroughtCount) +
        agegp * sex * ns(time,3) +
        sd_group +
        s(mm, k=4, fx=T, bs = 'cc') +
        offset(log(pop)), data=data,family=poisson)

        # drtedf <- summary(droughtModel)$edf
        #[1] 3.824836
        # summary(droughtModel)

        # now do for tmax, and then tmax anomaly.
        tmaxModel <- gam(deaths ~ s(tmax) +
        agegp * sex * ns(time,3) +
        sd_group +
        s(mm, k=4, fx=T, bs = 'cc')     +
        offset(log(pop)), data=data,family=poisson)
        # tmaxedf <- summary(tmaxModel)$edf
        #[1] 2.184725
        #summary(tmaxModel)


        tmaxanomModel <- gam(deaths ~ s(tmax_anomaly) +
        agegp * sex * ns(time,3) +
        sd_group +
        s(mm, k=4, fx=T, bs = 'cc')     +
        offset(log(pop)), data=data,family=poisson)
        # tmaxanomedf <- summary(tmaxanomModel)$edf
        #[1] 1.2653
        #summary(tmaxanomModel)



        # plot of all three
        png('droughtTmaxAnomGAMS.png',res=200,width = 1000, height = 1000)
        par(mfrow=c(2,2),mar=c(4,5,1,1))
        plot(droughtModel,se=T,select=1)
        abline(0,0)
        plot(tmaxModel,se=T,select=1)
        abline(0,0)
        plot(tmaxanomModel,se=T,select=1)
        abline(0,0)
        dev.off()




###################################################
### code chunk number 27: SuiDrtNSW_SupportingInfo.Rnw:1402-1542
###################################################
######################
#do,  check interaction combinations
######################




        # the following models are presented in order of their ranked BIC scores

        # sd_group*sex
        fit <- glm(deaths ~ sd_group * sex + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'sd_group*sex')
        # drop1(fit , test='Chisq', k = log(nrow(data)))


        # agegp*sex*ns(time,3) # THIS IS THE CORE MODEL
        # fit <- glm(deaths ~ sin(timevar*2*pi) + cos(timevar*2*pi)
        #        + agegp *  sex * ns(time,3) + sd_group + offset(log(pop)), data=data,family=poisson)
        # aic_table <- estat(fit, 'age*sex*ns(time,df=3)')

        # tmaxanomModel
        fit <- glm(deaths ~ tmax_anomaly + sin(timevar * 2 * pi) + cos(timevar * 2 * pi) +
        agegp * sex * ns(time,3) + sd_group + offset(log(pop)), data=data,family=poisson)
        aic_table <- estat(fit, 'tmaxanomModel')

        #tmaxModel  # NOTE WE CONSIDER THIS MODEL TO BE CONFUSED WITH SPRING/SUMMER EFFECT
        fit <- glm(deaths ~ ns(tmax,3) + sin(timevar * 2 * pi) + cos(timevar * 2 * pi) +
        agegp * sex * ns(time,3) + sd_group + offset(log(pop)), data=data,family=poisson)
        aic_table <- estat(fit, 'tmaxModel')

        # ns(tmax,3)*sex
        fit <- glm(deaths ~ ns(tmax, 3) * sex + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'ns(tmax,3)*sex')


        # droughtModel
        fit <- glm(deaths ~ ns(logDroughtCount,5) + sin(timevar * 2 * pi) + cos(timevar * 2 * pi) +
        agegp * sex * ns(time,3) + sd_group + offset(log(pop)), data=data,family=poisson)
        aic_table <- estat(fit, 'droughtModel')

        # tmax_anomaly*sex
        fit <- glm(deaths ~ tmax_anomaly * sex + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'tmax_anomaly*sex')


        # sd_group*ns(time,3)
        fit <- glm(deaths ~ sd_group * ns(time, 3) + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'sd_group*ns(time,3)')

        # ns(logDroughtCount,5)*sex
        fit <- glm(deaths ~ ns(logDroughtCount, 5) * sex + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'ns(logDroughtCount,5)*sex')

        # tmax_anomaly*ns(time,3)
        fit <- glm(deaths ~ tmax_anomaly * ns(time, 3) + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'tmax_anomaly*ns(time,3)')

        # ns(tmax,3)*ns(time,3)
        fit <- glm(deaths ~ ns(tmax, 3) * ns(time, 3) + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'ns(tmax,3)*ns(time,3)')

        # ns(tmax,3)*tmax_anomaly
        fit <- glm(deaths ~ ns(tmax, 3) * tmax_anomaly + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'ns(tmax,3)*tmax_anomaly')

        # ns(logDroughtCount,5)*tmax_anomaly
        fit <- glm(deaths ~ ns(logDroughtCount, 5) * tmax_anomaly + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'ns(logDroughtCount,5)*tmax_anomaly')

        # tmax_anomaly*agegp
        fit <- glm(deaths ~ tmax_anomaly * agegp + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'tmax_anomaly*agegp')

        # ns(logDroughtCount,5)*ns(time,3)
        fit <- glm(deaths ~ ns(logDroughtCount, 5) * ns(time, 3) + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'ns(logDroughtCount,5)*ns(time,3)')

        # ns(logDroughtCount,5)*ns(tmax,3)
        fit <- glm(deaths ~ ns(logDroughtCount, 5) * ns(tmax, 3) + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'ns(logDroughtCount,5)*ns(tmax,3)')

        # ns(tmax,3)*agegp
        fit <- glm(deaths ~ ns(tmax, 3) * agegp + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'ns(tmax,3)*agegp')

        # tmax_anomaly*sd_group
        fit <- glm(deaths ~ tmax_anomaly * sd_group + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'tmax_anomaly*sd_group')

        # ns(logDroughtCount,5)*agegp
        fit <- glm(deaths ~ ns(logDroughtCount, 5) * agegp + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'ns(logDroughtCount,5)*agegp')

        # ns(tmax,3)*sd_group
        fit <- glm(deaths ~ ns(tmax, 3) * sd_group + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'ns(tmax,3)*sd_group')

        # sd_group*sex*ns(time,3)
        fit <- glm(deaths ~ sd_group*sex*ns(time,3) + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'sd_group*sex*ns(time,3)')

        # ns(logDroughtCount,5)*sd_group
        fit <- glm(deaths ~ ns(logDroughtCount, 5) * sd_group + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'ns(logDroughtCount,5)*sd_group')

        # agegp*sd_group
        fit <- glm(deaths ~ agegp * sd_group + sin(timevar * 2 * pi) + cos(timevar * 2 * pi)
        + agegp * sex * ns(time, 3) + sd_group + offset(log(pop)),data=data,family=poisson)
        aic_table <- estat(fit, 'agegp*sd_group')

        # NOTE in the next section we explore an interaction model for drought effects by age, sex and region
        # to incorporate into the aic table we must have run that before adding the result here.
        # aic_table <- estat(interactionDrtAgeSexRuralModel2, 'interactionDrtAgeSexRuralModel2')

        # make presentable
        aic_table <- as.data.frame(aic_table)
        names(aic_table) <- c('model','param','aic','bic','percentChDev')
        aic_table <- aic_table[order(aic_table$bic),]
        aic_table
        write.csv(aic_table, 'aic_table.csv',row.names=F)




###################################################
### code chunk number 28: tab3
###################################################
library(xtable)

foo1=read.csv('aic_table.csv', as.is=T)


foo1=cbind(foo1,c(foo1$aic-min(foo1$aic)))
foo1=cbind(foo1,c(foo1$bic-min(foo1$bic)))

names(foo1)=c('Model','Parameters','AIC','BIC','percentChDev','deltaAIC','deltaBIC')
foo1=foo1[order(foo1$BIC),]
print(xtable(foo1[,c('Model','Parameters','BIC','AIC')], caption = 'Models ranked by Bayesian Information Criterion (BIC).', label = 'tab:tab3',
digits = c(0,0, 0, 0,0)), table.placement = '!ht',
caption.placement = 'top', include.rownames = FALSE)


###################################################
### code chunk number 29: SuiDrtNSW_SupportingInfo.Rnw:1593-1661
###################################################
######################
#do,  Suicide and Drought Model by Age, Sex and Region
######################



        # create a drought variable for each category
        # ie pre-calculated Drought by Age, Sex and Rural/Urban Region terms, constructed to have the value of the drought index in the specified groups (with Ages grouped by 20 year age brackets) and zero otherwise.
        # NOTE that we initially fitted this model with a drought effect in each 10 year age bracket, however the 20 year age brackets give essentially the same results, and is simpler to calculate.

        require(mgcv)
        require(splines)

        data$rural <-ifelse(data$sd_group %in% c('Central West','Mid-North Coast','Murray','Murrumbidgee','North and Far Western','Northern','Richmond-Tweed','South Eastern'), 1, 0)

        data$agegp2 <-ifelse(data$agegp %in% c('10_19','20_29'), '10_29',
        ifelse(data$agegp %in% c('30_39','40_49'), '30_49',
        ifelse(data$agegp %in% c('50_59','60_69','70plus'), '50plus',
        0)))

        data$agegp2 <- as.factor(data$agegp2)

        ages <- c('10_19','20_29','30_39','40_49','50_59','60_69','70plus')
        ages2 <- c('10_29','30_49','50plus')

        # step thru each
        for(sexs in 1:2){
        # sexs <- c(2)#,2)
        if(sexs == 1) {sexid <- 'Males'} else {sexid <- 'Females'}
        #sexid <- c('Females')#,'Females')
        for(rural in 0:1){
        # rural <- c(1)#,0)
        if(rural == 0) {ruralid <- c('urban')} else {ruralid<-'rural'} #,'urban')

        cat(
        paste(
        'data$Drt',sexid,ages2,ruralid,' <- ifelse(data$agegp2 == ',ages2,' & data$sex == ',sexs,' & data$rural == ',rural,', data$logDroughtCount, 0)',
        collapse = '
',sep='')
        )
        cat('

')
        }

        }

        # need to add ' to each agegp
        data$DrtMales10_29urban <- ifelse(data$agegp2 == '10_29' & data$sex == 1 & data$rural == 0, data$logDroughtCount, 0)
        data$DrtMales30_49urban <- ifelse(data$agegp2 == '30_49' & data$sex == 1 & data$rural == 0, data$logDroughtCount, 0)
        data$DrtMales50plusurban <- ifelse(data$agegp2 == '50plus' & data$sex == 1 & data$rural == 0, data$logDroughtCount, 0)

        data$DrtMales10_29rural <- ifelse(data$agegp2 == '10_29' & data$sex == 1 & data$rural == 1, data$logDroughtCount, 0)
        data$DrtMales30_49rural <- ifelse(data$agegp2 == '30_49' & data$sex == 1 & data$rural == 1, data$logDroughtCount, 0)
        data$DrtMales50plusrural <- ifelse(data$agegp2 == '50plus' & data$sex == 1 & data$rural == 1, data$logDroughtCount, 0)

        data$DrtFemales10_29urban <- ifelse(data$agegp2 == '10_29' & data$sex == 2 & data$rural == 0, data$logDroughtCount, 0)
        data$DrtFemales30_49urban <- ifelse(data$agegp2 == '30_49' & data$sex == 2 & data$rural == 0, data$logDroughtCount, 0)
        data$DrtFemales50plusurban <- ifelse(data$agegp2 == '50plus' & data$sex == 2 & data$rural == 0, data$logDroughtCount, 0)

        data$DrtFemales10_29rural <- ifelse(data$agegp2 == '10_29' & data$sex == 2 & data$rural == 1, data$logDroughtCount, 0)
        data$DrtFemales30_49rural <- ifelse(data$agegp2 == '30_49' & data$sex == 2 & data$rural == 1, data$logDroughtCount, 0)
        data$DrtFemales50plusrural <- ifelse(data$agegp2 == '50plus' & data$sex == 2 & data$rural == 1, data$logDroughtCount, 0)







###################################################
### code chunk number 30: SuiDrtNSW_SupportingInfo.Rnw:1666-1760
###################################################
######################
#do,  Final drought model
######################




        strt=Sys.time()
        interactionDrtAgeSexRuralModel2 <- gam(deaths ~  s(mm, k=3, fx=T, bs = 'cp')
        + s(DrtMales10_29rural)
        + s(DrtMales30_49rural)
        + s(DrtMales50plusrural)
        + s(DrtFemales10_29rural)
        + s(DrtFemales30_49rural)
        + s(DrtFemales50plusrural)
        + s(DrtMales10_29urban)
        + s(DrtMales30_49urban)
        + s(DrtMales50plusurban)
        + s(DrtFemales10_29urban)
        + s(DrtFemales30_49urban)
        + s(DrtFemales50plusurban)
        + s(tmax_anomaly)
        + agegp2
        + rural
        + sd_group
        + sex
        + agegp
        + agegp*sex*ns(time,df = 3)
        + offset(log(pop)), data=data,family=poisson)
        save.image()

        endd=Sys.time()
        print(endd-strt)

        # great so how many degrees of freedom to allow in the parametric glm splines?
        summary(interactionDrtAgeSexRuralModel2)
        #  Approximate significance of smooth terms:
        #                            edf Ref.df Chi.sq  p-value
        # s(mm)                    2.000  2.000 18.750 8.48e-05 ***
        # s(DrtMales10_29rural)    1.001  1.003  4.874 0.027381 *
        # s(DrtMales30_49rural)    1.915  2.371 20.566 5.81e-05 ***
        # s(DrtMales50plusrural)   1.655  2.047  2.467 0.300079
        # s(DrtFemales10_29rural)  1.015  1.029  0.315 0.586602
        # s(DrtFemales30_49rural)  1.019  1.038  4.323 0.039790 *
        # s(DrtFemales50plusrural) 3.976  4.895 42.760 3.64e-08 ***
        # s(DrtMales10_29urban)    4.903  5.943 21.243 0.001588 **
        # s(DrtMales30_49urban)    1.025  1.050  3.122 0.082686 .
        # s(DrtMales50plusurban)   2.633  3.270  6.538 0.105906
        # s(DrtFemales10_29urban)  1.001  1.001  0.987 0.321001
        # s(DrtFemales30_49urban)  2.112  2.630  1.091 0.715930
        # s(DrtFemales50plusurban) 1.002  1.004  0.906 0.342291
        # s(tmax_anomaly)          1.098  1.191 12.249 0.000662 ***
        # ---

        #
        # R-sq.(adj) =   0.66   Deviance explained = 24.7%
        # UBRE score = -0.44662  Scale est. = 1         n = 69916

        # just checking the Adjusted R-squared
        Rsquared.glm.gsm(interactionDrtAgeSexRuralModel2)


        # fit the GLM with recommended df
        require(splines)
        strt=Sys.time()
        interactionDrtAgeSexRuralModel3 <- glm(deaths ~ sin(timevar*2*pi) + cos(timevar*2*pi)
        + DrtMales10_29rural
        + DrtMales30_49rural
        + DrtMales50plusrural
        + DrtFemales10_29rural
        + DrtFemales30_49rural
        + ns(DrtFemales50plusrural, df = 5)
        + ns(DrtMales10_29urban, df = 6)
        + DrtMales30_49urban
        + ns(DrtMales50plusurban, df = 4)
        + DrtFemales10_29urban
        + ns(DrtFemales30_49urban, df = 3)
        + DrtFemales50plusurban
        + tmax_anomaly
        + agegp2
        + rural
        + sd_group
        + sex
        + agegp
        + agegp*sex*ns(time,3)
        + offset(log(pop)), data=data,family=poisson)
        save.image()
        endd=Sys.time()
        print(endd-strt)

        summary(interactionDrtAgeSexRuralModel3)
        Rsquared.glm.gsm(interactionDrtAgeSexRuralModel3)




###################################################
### code chunk number 31: SuiDrtNSW_SupportingInfo.Rnw:1775-1860
###################################################
######################
#do,  summary of model
######################



        # season

        png('season4.png',res=400,width = 1200, height = 1000)
        par(mar=c(2.5,3,.5,1))
        plot(interactionDrtAgeSexRuralModel2,select=1,rug = F,se=T,ylab = 'log Relative Risk',xlab='Month', ylim=c (-.07,0.07), shade=TRUE,shade.col='grey',cex.axis=0.5,cex.lab=0.5)
        abline(0,0,lwd=1.5)
        segments(9,-.1,9,.1)
        segments(11,-.1,11,.1)
        dev.off()

        # make a plot of each group effect
        ylimits <- 0.5
        # for the names
        summary(interactionDrtAgeSexRuralModel2)$formula[3]
        #  s(mm, k = 4, fx = T, bs = 'cc') + s(DrtMales10_29rural) + s(DrtMales30_49rural) +
        #     s(DrtMales50plusrural) + s(DrtFemales10_29rural) + s(DrtFemales30_49rural) +
        #     s(DrtFemales50plusrural) + s(DrtMales10_29urban) + s(DrtMales30_49urban) +
        #     s(DrtMales50plusurban) + s(DrtFemales10_29urban) + s(DrtFemales30_49urban) +
        #     s(DrtFemales50plusurban) + s(tmax_anomaly) + agegp2 + rural +
        #     sd_group + sex + agegp + agegp * sex * ns(time, df = 3) +
        #     offset(log(pop))()

        png('interactionDrtAgeSexRuralModel2.png',res=200,width = 1000, height = 1600)#, pointsize =30)
        par(mfcol=c(6,2),mar=c(4,5,2,1), cex = .5)
        # rural males
        plot(interactionDrtAgeSexRuralModel2,select=2,se=T, ylim = c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        abline(0,0)
        title('DrtMales10_29rural')
        plot(interactionDrtAgeSexRuralModel2,select=3,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        abline(0,0)
        title('DrtMales30_49rural')
        plot(interactionDrtAgeSexRuralModel2,select=4,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        abline(0,0)
        title('DrtMales50plusrural')

        # urban males
        plot(interactionDrtAgeSexRuralModel2,select=8,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        abline(0,0)
        title('DrtMales10_29urban')
        plot(interactionDrtAgeSexRuralModel2,select=9,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        abline(0,0)
        title('DrtMales30_49urban')
        plot(interactionDrtAgeSexRuralModel2,select=10,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        abline(0,0)
        title('DrtMales50plusurban')

        # rural females
        plot(interactionDrtAgeSexRuralModel2,select=5,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        title('DrtFemales10_29rural')
        abline(0,0)
        plot(interactionDrtAgeSexRuralModel2,select=6,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        abline(0,0)
        title('DrtFemales30_49rural')
        plot(interactionDrtAgeSexRuralModel2,select=7,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        abline(0,0)
        title('DrtFemales50plusrural')

        # urban females
        plot(interactionDrtAgeSexRuralModel2,select=11,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        abline(0,0)
        title('DrtFemales10_29urban')
        plot(interactionDrtAgeSexRuralModel2,select=12,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        abline(0,0)
        title('DrtFemales30_49urban')
        plot(interactionDrtAgeSexRuralModel2,select=13,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey',ylab = 'log Relative Risk', xlab = 'Drought Index: log(1 + count months)')
        abline(0,0)
        title('DrtFemales50plusurban')
        dev.off()

        # tmax_anomaly
        png('tmaxanom2.png',res=300,width = 1200, height = 1000)
        par(mar=c(4,3.5,.5,1))
        plot(interactionDrtAgeSexRuralModel2,select=14,rug = T, all.terms=T,se=T,ylab = 'log Relative Risk',xlab='Maximum Temperature Anomaly', ylim=c (-.15,0.15))
        abline(0,0,lwd=1.5)
        dev.off()






###################################################
### code chunk number 32: SuiDrtNSW_SupportingInfo.Rnw:1865-1915
###################################################
######################
#do,  best figures
######################



        png('RuralMales2.0.png',res=200,width = 600, height = 1000)
        layout(matrix(c(1:4),ncol=1),heights=c(1,1,1,0.2))
        par(mfrow=c(4,1), mar=c(0.1,4,1.5,0.5), cex=.7)

        plot(interactionDrtAgeSexRuralModel2,select=2,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = '',xlab = '', xaxt='n')
        abline(0,0)
        title('Rural Males aged 10-29', cex=.5, font.main = 1)

        plot(interactionDrtAgeSexRuralModel2,select=3,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = 'log(Relative Risk)',xlab = '', xaxt='n')
        abline(0,0)
        title('Rural Males aged 30-49', cex=.5, font.main = 1)

        plot(interactionDrtAgeSexRuralModel2,select=4,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = '',xlab = '')
        abline(0,0)
        title('Rural Males aged 50 plus', cex=.5, font.main = 1)

        par(mar=c(1,4,6,0.5))
        plot(1,1,type = 'n', xaxt = 'n', yaxt='n',ylab='',xlab='', axes = F)
        title(main = 'Drought Index: log(1 + count months)',  font.main = 1,cex.main=.9)
        dev.off()


        png('RuralFemales2.0.png',res=200,width = 600, height = 1000)
        layout(matrix(c(1:4),ncol=1),heights=c(1,1,1,0.2))
        par(mfrow=c(4,1), mar=c(0.1,4,1.5,0.5), cex=.7)

        plot(interactionDrtAgeSexRuralModel2,select=5,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = '',xlab = '', xaxt='n')
        abline(0,0)
        title('Rural Females aged 10-29', cex=.5, font.main = 1)

        plot(interactionDrtAgeSexRuralModel2,select=6,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = 'log(Relative Risk)',xlab = '', xaxt='n')
        abline(0,0)
        title('Rural Females aged 30-49', cex=.5, font.main = 1)

        plot(interactionDrtAgeSexRuralModel2,select=7,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = '',xlab = '')
        abline(0,0)
        title('Rural Females aged 50 plus', cex=.5, font.main = 1)

        par(mar=c(1,4,6,0.5))
        plot(1,1,type = 'n', xaxt = 'n', yaxt='n',ylab='',xlab='', axes = F)
        title(main = 'Drought Index: log(1 + count months)',  font.main = 1,cex.main=.9)
        dev.off()




###################################################
### code chunk number 33: SuiDrtNSW_SupportingInfo.Rnw:1925-2026
###################################################
######################
#do,  The final drought model estimates by age, sex and region
######################




        # fit the GLM with recommended df
        require(splines)
        strt=Sys.time()
        interactionDrtAgeSexRuralModel3 <- glm(deaths ~ sin(timevar*2*pi) + cos(timevar*2*pi)
        + tmax_anomaly
        + DrtMales10_29rural
        + DrtMales30_49rural
        + DrtMales50plusrural
        + DrtFemales10_29rural
        + DrtFemales30_49rural
        + ns(DrtFemales50plusrural, df = 5)
        + ns(DrtMales10_29urban, df = 6)
        + DrtMales30_49urban
        + ns(DrtMales50plusurban, df = 4)
        + DrtFemales10_29urban
        + ns(DrtFemales30_49urban, df = 3)
        + DrtFemales50plusurban
        + agegp2
        + rural
        + sd_group
        + sex
        + agegp
        + agegp*sex*ns(time,3)
        + offset(log(pop)), data=data,family=poisson)
        save.image()
        endd=Sys.time()
        print(endd-strt)

        summary(interactionDrtAgeSexRuralModel3)
        Rsquared.glm.gsm(interactionDrtAgeSexRuralModel3)

        # EDIT 2011-10-19
        # new version of termplot requires better naming terms
        # termplot(interactionDrtAgeSexRuralModel3, se = T)
        # returns Error in `[.data.frame`(mf, , i) : undefined columns selected
        # so use termplot(interactionDrtAgeSexRuralModel3, terms = attr(terms(interactionDrtAgeSexRuralModel3),'term.labels') [4:5], se = T)

        glmest<-summary(interactionDrtAgeSexRuralModel3)$coefficients
        betai <- glmest[which(row.names(glmest)=='DrtMales30_49rural'),1]
        sei <- glmest[which(row.names(glmest)=='DrtMales30_49rural'),2]
        xvar.q1q3 <- IQR(data$logDroughtCount)

        xvar.q1q3
        # [1] 1.122753
        exp(1.122753)-1
        # [1] 2.073303

        exp(betai*xvar.q1q3)
        #[1] 1.149829
        exp((betai - sei * 1.96) * xvar.q1q3)
        #[1] 1.080688
        exp((betai + sei * 1.96) * xvar.q1q3)
        #[1] 1.223392
        # RR 1.15, 95%CI 1.08 to 1.22)
        drop1(interactionDrtAgeSexRuralModel3, test = 'Chisq')

        # Single term deletions
        #
        # Model:
        # deaths ~ sin(timevar * 2 * pi) + cos(timevar * 2 * pi) + tmax_anomaly +
        #     DrtMales10_29rural + DrtMales30_49rural + DrtMales50plusrural +
        #     DrtFemales10_29rural + DrtFemales30_49rural + ns(DrtFemales50plusrural,
        #     df = 5) + ns(DrtMales10_29urban, df = 6) + DrtMales30_49urban +
        #     ns(DrtMales50plusurban, df = 4) + DrtFemales10_29urban +
        #     ns(DrtFemales30_49urban, df = 3) + DrtFemales50plusurban +
        #     agegp2 + rural + sd_group + sex + agegp + agegp * sex * ns(time,
        #     3) + offset(log(pop))
        #                                   Df Deviance   AIC    LRT   Pr(Chi)
        # <none>                                  38565 69127
        # sin(timevar * 2 * pi)              1    38572 69133  7.496 0.0061829 **
        # cos(timevar * 2 * pi)              1    38581 69142 16.275 5.478e-05 ***
        # tmax_anomaly                       1    38575 69136 10.388 0.0012687 **
        # DrtMales10_29rural                 1    38572 69132  7.191 0.0073256 **
        # DrtMales30_49rural                 1    38583 69144 18.677 1.548e-05 ***
        # DrtMales50plusrural                1    38565 69126  0.410 0.5221160
        # DrtFemales10_29rural               1    38565 69126  0.308 0.5790212
        # DrtFemales30_49rural               1    38569 69129  4.144 0.0417870 *
        # ns(DrtFemales50plusrural, df = 5)  1    38593 69154 28.533 9.210e-08 ***
        # ns(DrtMales10_29urban, df = 6)     1    38566 69127  1.388 0.2386939
        # DrtMales30_49urban                 1    38567 69128  2.776 0.0956671 .
        # ns(DrtMales50plusurban, df = 4)    1    38568 69129  3.490 0.0617570 .
        # DrtFemales10_29urban               1    38565 69126  0.955 0.3284023
        # ns(DrtFemales30_49urban, df = 3)   1    38565 69125  0.170 0.6800571
        # DrtFemales50plusurban              1    38565 69125  0.159 0.6901503
        # agegp2                             0    38565 69127  0.000
        # rural                              0    38565 69127  0.000
        # sd_group                           9    38596 69141 31.883 0.0002086 ***
        # sex:agegp:ns(time, 3)             18    38613 69140 48.602 0.0001225 ***
        # ---



        #



###################################################
### code chunk number 34: SuiDrtNSW_SupportingInfo.Rnw:2065-2324
###################################################
######################
#do,  Attributable Number of Deaths
######################



        # newnode get estimate as attributable deaths
        # need to calculate
        # y(attributableToX) = sum((y0 x (exp(beta * X) - 1) x Pop))
        # where y0 is the baseline incidence rate for the health endpoint being quantified;
        # Pop is the population affected and
        # beta is the effect coefficient drawn from the model.


        # get a test dataset
        paste(names(data)[c(2:9,17)],sep='', collapse="','")
  data2<- data[,c('sd_group','rural','sex','agegp','agegp2','dthyy','dthmm','deaths','pop','logDroughtCount')]
        head(data2)
        # use the average rates deaths/person/month
        # newnode get descriptive deaths by age/sex/month/zone groups
        # calculate baseline incidence

        names(data)
        desc <- sqldf('
    select sd_group, sex, agegp,avg(deaths) as avgMonthlyDeaths, avg(pop) as avgPop,
        avg(deaths)/avg(pop) as avgRate
        from data
        group by sd_group, sex, agegp
        order by sd_group, sex, agegp
        ')
  head(desc)
        desc[1:40,]
        sqldf(
        'select sd_group, sum(avgMonthlyDeaths), sum(avgPop)
   from desc
        group by sd_group
        order by sd_group
        ')
  subset(desc, sd_group == 'Sydney')
        with(subset(data, sd_group == 'Sydney' & sex == 1), plot(agegp,deaths/pop))
        with(subset(data, sd_group == 'Sydney' & sex == 1 & agegp == '70plus'),
        plot(as.Date(paste(dthyy, dthmm, 1, sep='-')), deaths, type = 'l', col = 'grey')
        )
        abline(2.3392070,0)
        # ok merge with the test dataset
        data2 <- merge(data2, desc, by =  c('sd_group', 'sex', 'agegp'))
        subset(desc, sd_group == 'Central West')
        head(data2)

        # now use the coefficient in
        # y(attributable) = baselineIncidence x (exp(beta * X) - 1) x Pop
        # recall I used
        glmest<-summary(interactionDrtAgeSexRuralModel3)$coefficients
        betai <- glmest[which(row.names(glmest)=='DrtMales30_49rural'),1]
        sei <- glmest[which(row.names(glmest)=='DrtMales30_49rural'),2]
        # estimate only for  DrtMales30_49rural
        attributable <- subset(data2, rural == 1 & sex ==1 & agegp2 == '30_49')
        table(attributable$sd_group)
        attach(attributable)

        attributable$deathsAttributable <-
        (avgMonthlyDeaths/avgPop) * (exp(betai * logDroughtCount) - 1) * pop
        # SE
        #LCI
        attributable$deathsAttributableLower <-
        (avgMonthlyDeaths/avgPop) * (exp((betai - sei * 1.96) *  logDroughtCount) - 1) * pop
        #UCI
        attributable$deathsAttributableUpper <-
        (avgMonthlyDeaths/avgPop) * (exp((betai + sei * 1.96) * logDroughtCount) - 1) * pop

        detach(attributable)
        head(attributable)


        # now summarise by year
        summaryAttributable <- sqldf(
        'select dthyy, sum(deathsAttributable) as deathsAttributable,
     sum(deaths) as deaths,
        sum(pop) as pop,
        round(avg(logDroughtCount),0) as logDroughtCount
        from attributable
        group by dthyy
        order by dthyy
        ')
  summaryAttributable
        # plot the estimated deaths
        with(summaryAttributable,
        plot(dthyy, deathsAttributable/deaths, type = 'l')
        )
        par(new=T)
        with(summaryAttributable,
        plot(dthyy, logDroughtCount, type = 'l',col = 'blue')
        )
        par(new=T)
        with(summaryAttributable,
        plot(dthyy, deaths, type = 'b',col = 'darkblue', pch=16)
        )
        # calcualte estimate

        estOut <- sqldf(
        'select sum(deaths) as deaths,
    sum(deathsAttributable) as deathsAttributable,
        sum(deathsAttributableLower) as deathsAttributableLower,
        sum(deathsAttributableUpper) as deathsAttributableUpper
        from attributable
        ')

        # The predicted number of rural male suicides aged 30-49 per annum associated with droughts over our study period was 4.01 (95%CI 2.14 to 6.05)
        estOut$deathsAttributable
        # 152.3477
        length(2007:1970)
        estOut$deathsAttributable / 38
        # [1] 4.009151
        estOut$deathsAttributableLower / 38
        # [1] 2.136019
        estOut$deathsAttributableUpper / 38
        # [1] 6.046266

        # This is not as good a representation as by drought year.
        # to calculate number of drought years get average of the number of drought years by Rural Regions
        # DROUGHT MONTHS DEFINED AS ANY MONTH WHERE THE DROUGHT INDEX IS
        # GREATER THAN OR EQUAL TO 5.
        droughtyears <- sqldf("select sd_group, sum(droughtmonth)/12 as droughtyears
       from
        (
        select sd_group, agegp, sex, time, avcount,
        case when avcount >= 5 then 1 else 0 end as droughtmonth
        from data
        where agegp = '10_19' and sex = 1
        order by sd_group
        ) t1
        group by sd_group
        ")

        # sanity check
        qc <- sqldf("select sd_group, agegp, sex, time, avcount,
              case when avcount >= 5 then 1 else 0 end as droughtmonth
        from data
        where agegp = '10_19' and sex = 1 and sd_group = 'Central West'
        order by sd_group
        ")

        png(file.path(rootdir,'CentralWestDrought19702007.png'),res=200,width = 2100, height = 1000)
        with(qc, plot(time, avcount, type = 'l', axes=F))
        with(qc, points(time, avcount, pch = 16, cex=.5))
        box();axis(2);
        axis(1,at=as.Date(paste(1970:2007,'-01-01',sep='')),labels=NA)
        axis(1,at=as.Date(paste(seq(1970, 2007,5),'-01-01',sep='')),labels=seq(1970, 2007,5))
        segments(as.Date(paste(1970:2007,'-01-01',sep='')),0,as.Date(paste(1970:2007,'-01-01',sep='')),12,lty=3)
        segments(min(qc$time),5,max(qc$time),5)

        # calculate beginning and end of drougths
        indicator <- cbind(qc$avcount,c(NA,qc[1:(nrow(qc)-1),'avcount']))
        drtstrt <- which(indicator[,1] >=5 & indicator[,2] <5)
        #points(qc$time[drtstrt],qc$avcount[drtstrt], col = 'red')
        drtend <- which(indicator[,1] <5 & indicator[,2] >=5)
        #points(qc$time[drtend-1],rep(5,length(drtend)))

        cbind(rep(c(min(qc$time)-(5*365),max(qc$time)+(5*365),max(qc$time)+(5*365),min(qc$time)-(5*365)),3),
        c(drtstrt,drtstrt,drtend-1,drtend-1))
        #polygon(c(min(qc$time)-(5*365),max(qc$time)+(5*365),max(qc$time)+(5*365),min(qc$time)-(5*365)),c(4,4,14,14),col='grey')
        for(i in 1:9){
        polygon(c(qc$time[drtstrt[i]],qc$time[drtend[i]-1],qc$time[drtend[i]-1],qc$time[drtstrt[i]]),
        c(5,5,14,14), col='grey')
        }
        with(qc, lines(time, avcount))
        with(qc, points(time, avcount, pch = 16, cex=.5))
        #points(qc$time[drtstrt],qc$avcount[drtstrt], col = 'red')
        legend('topleft',legend=c('droughtIndex','droughtDeclared'),fill=c(NA,'grey'),border=c(NA,'black'),lty=c(1,NA))
        dev.off()

        # check against http://www.dpi.nsw.gov.au/agriculture/emergency/drought/planning/climate/advance-retreat


        # THIS NEXT ONE CALCULATES THE NUMBER PER DROUGHT YEAR AND COMES UP WITH 17
        # INTERESTING ATTEMPT THAT I MIGHT COME BACK TO
        # BUT FOR NOW WE ARE NOT HAPPY TO INCORPORATE THE ARBITRARY DROUGHT THRESHOLDS IN OUR PREDICTION


        droughtyearsRural <- droughtyears[!droughtyears$sd_group %in% c('Sydney','Hunter','Illawarra'),]
        #                 sd_group droughtyears
        # 1           Central West            3
        # 4        Mid-North Coast            3
        # 5                 Murray            2
        # 6           Murrumbidgee            3
        # 7  North and Far Western            2
        # 8               Northern            2
        # 9         Richmond-Tweed            5
        # 10         South Eastern            4
        mean(droughtyearsRural$droughtyears)
        # 3
        # so 3 out of 38
        (3/38)*100 # 7.9%

        table(attributable$sd_group)
        # set drought index to 0 if <5
        attributable$logDroughtCountDeclared <- ifelse(attributable$logDroughtCount >= log1p(5), attributable$logDroughtCount, 0)
        attach(attributable)
        # TODO this is clobbering the previous calculation, it would be best to keep that and make new names?
        attributable$deathsAttributable <-
        (avgMonthlyDeaths/avgPop) * (exp(betai * logDroughtCountDeclared) - 1) * pop
        # SE
        #LCI
        attributable$deathsAttributableLower <-
        (avgMonthlyDeaths/avgPop) * (exp((betai - sei * 1.96) *  logDroughtCountDeclared) - 1) * pop
        #UCI
        attributable$deathsAttributableUpper <-
        (avgMonthlyDeaths/avgPop) * (exp((betai + sei * 1.96) * logDroughtCountDeclared) - 1) * pop

        detach(attributable)
        head(subset(attributable, logDroughtCountDeclared != 0))


        # now summarise by year
        summaryAttributable <- sqldf(
        'select dthyy, sum(deathsAttributable) as deathsAttributable,
     sum(deaths) as deaths,
        sum(pop) as pop,
        round(avg(logDroughtCountDeclared),1) as logDroughtCountDeclared

        from attributable
        group by dthyy
        order by dthyy
        ')
  summaryAttributable
        # plot the estimated deaths
        with(summaryAttributable,
        plot(dthyy, deathsAttributable, type = 'b', pch = 16)
        )
        par(new=T)
        with(summaryAttributable,
        plot(dthyy, logDroughtCountDeclared, type = 'l',col = 'blue')
        )
        #   par(new=T)
        #   with(summaryAttributable,
        #    plot(dthyy, deaths, type = 'b',col = 'darkblue', pch=16)
        #    )
        # calcualte estimate

        estOut <- sqldf(
        'select sum(deaths) as deaths,
    sum(deathsAttributable) as deathsAttributable,
        sum(deathsAttributableLower) as deathsAttributableLower,
        sum(deathsAttributableUpper) as deathsAttributableUpper
        from attributable
        ')

        # The predicted number of rural male suicides aged 30-49 per drought year over our study period was 17.73 (95%CI 9.26 to 27.29)
        estOut$deathsAttributable
        # [1] 53.19648

        estOut$deathsAttributable / 3
        # 17.73216
        estOut$deathsAttributableLower / 3
        # 9.260883
        estOut$deathsAttributableUpper / 3
        # 27.28826




###################################################
### code chunk number 35: SuiDrtNSW_SupportingInfo.Rnw:2329-2448
###################################################
######################
#do,  Attributable Number of Deaths, rural males 10-29
######################




        # newnode get estimate as attributable deaths
        # need to calculate
        # y(attributableToX) = sum((y0 x (exp(beta * X) - 1) x Pop))
        # where y0 is the baseline incidence rate for the health endpoint being quantified;
        # Pop is the population affected and
        # beta is the effect coefficient drawn from the model.


        # get a test dataset

        data2<- data[,c('sd_group','rural','sex','agegp','agegp2','dthyy','dthmm','deaths','pop','logDroughtCount')]
        head(data2)
        # use the average rates deaths/person/month
        # newnode get descriptive deaths by age/sex/month/zone groups
        # calculate baseline incidence

        names(data)
        desc <- sqldf('
    select sd_group, sex, agegp,avg(deaths) as avgMonthlyDeaths, avg(pop) as avgPop,
        avg(deaths)/avg(pop) as avgRate
        from data
        group by sd_group, sex, agegp
        order by sd_group, sex, agegp
        ')
  head(desc)
        desc[1:40,]
        sqldf(
        'select sd_group, sum(avgMonthlyDeaths), sum(avgPop)
   from desc
        group by sd_group
        order by sd_group
        ')
  subset(desc, sd_group == 'Sydney')
        with(subset(data, sd_group == 'Sydney' & sex == 1), plot(agegp,deaths/pop))
        with(subset(data, sd_group == 'Sydney' & sex == 1 & agegp == '70plus'),
        plot(as.Date(paste(dthyy, dthmm, 1, sep='-')), deaths, type = 'l', col = 'grey')
        )
        abline(2.3392070,0)
        # ok merge with the test dataset
        data2 <- merge(data2, desc, by =  c('sd_group', 'sex', 'agegp'))
        subset(desc, sd_group == 'Central West')
        head(data2)

        # now use the coefficient in
        # y(attributable) = baselineIncidence x (exp(beta * X) - 1) x Pop
        # recall I used
        glmest<-summary(interactionDrtAgeSexRuralModel3)$coefficients
        betai <- glmest[which(row.names(glmest)=='DrtMales10_29rural'),1]
        sei <- glmest[which(row.names(glmest)=='DrtMales10_29rural'),2]
        # estimate only for  DrtMales10_29rural
        attributable <- subset(data2, rural == 1 & sex ==1 & agegp2 == '10_29')
        table(attributable$sd_group)
        attach(attributable)

        attributable$deathsAttributable <-
        (avgMonthlyDeaths/avgPop) * (exp(betai * logDroughtCount) - 1) * pop
        # SE
        #LCI
        attributable$deathsAttributableLower <-
        (avgMonthlyDeaths/avgPop) * (exp((betai - sei * 1.96) *  logDroughtCount) - 1) * pop
        #UCI
        attributable$deathsAttributableUpper <-
        (avgMonthlyDeaths/avgPop) * (exp((betai + sei * 1.96) * logDroughtCount) - 1) * pop

        detach(attributable)
        head(attributable)


        # now summarise by year
        summaryAttributable <- sqldf(
        'select dthyy, sum(deathsAttributable) as deathsAttributable,
     sum(deaths) as deaths,
        sum(pop) as pop,
        round(avg(logDroughtCount),0) as logDroughtCount
        from attributable
        group by dthyy
        order by dthyy
        ')
  summaryAttributable
        # plot the estimated deaths
        with(summaryAttributable,
        plot(dthyy, deathsAttributable/deaths, type = 'l')
        )
        par(new=T)
        with(summaryAttributable,
        plot(dthyy, logDroughtCount, type = 'l',col = 'blue')
        )
        par(new=T)
        with(summaryAttributable,
        plot(dthyy, deaths, type = 'b',col = 'darkblue', pch=16)
        )
        # calcualte estimate

        estOut <- sqldf(
        'select sum(deaths) as deaths,
    sum(deathsAttributable) as deathsAttributable,
        sum(deathsAttributableLower) as deathsAttributableLower,
        sum(deathsAttributableUpper) as deathsAttributableUpper
        from attributable
        ')

        # The predicted number of rural male suicides aged 10-29 per annum associated with droughts over our study period was 2.10 (95%CI 0.56 to 3.79)
        estOut$deathsAttributable
        # [1] 79.64719
        estOut$deathsAttributable / 38
        # [1] 2.095979
        estOut$deathsAttributableLower / 38
        # [1] 0.5618928
        estOut$deathsAttributableUpper / 38
        # [1] 3.791449




###################################################
### code chunk number 36: SuiDrtNSW_SupportingInfo.Rnw:2453-2586
###################################################
######################
#do,  Attributable Number of Female Deaths
######################



        # newnode get estimate as attributable deaths
        # need to calculate
        # y(attributableToX) = sum((y0 x (exp(beta * X) - 1) x Pop))
        # where y0 is the baseline incidence rate for the health endpoint being quantified;
        # Pop is the population affected and
        # beta is the effect coefficient drawn from the model.


        # get a test dataset
        paste(names(data)[c(2:9,17)],sep='', collapse="','")
  data2<- data[,c('sd_group','rural','sex','agegp','agegp2','dthyy','dthmm','deaths','pop','logDroughtCount')]
        head(data2)
        # use the average rates deaths/person/month
        # newnode get descriptive deaths by age/sex/month/zone groups
        # calculate baseline incidence

        names(data)
        desc <- sqldf('
    select sd_group, sex, agegp,avg(deaths) as avgMonthlyDeaths, avg(pop) as avgPop,
        avg(deaths)/avg(pop) as avgRate
        from data
        group by sd_group, sex, agegp
        order by sd_group, sex, agegp
        ')
  head(desc)
        desc[1:40,]
        sqldf(
        'select sd_group, sum(avgMonthlyDeaths), sum(avgPop)
   from desc
        group by sd_group
        order by sd_group
        ')
  subset(desc, sd_group == 'Sydney')
        with(subset(data, sd_group == 'Sydney' & sex == 1), plot(agegp,deaths/pop))
        with(subset(data, sd_group == 'Sydney' & sex == 1 & agegp == '70plus'),
        plot(as.Date(paste(dthyy, dthmm, 1, sep='-')), deaths, type = 'l', col = 'grey')
        )
        abline(2.3392070,0)
        # ok merge with the test dataset
        data2 <- merge(data2, desc, by =  c('sd_group', 'sex', 'agegp'))
        subset(desc, sd_group == 'Central West')
        head(data2)

        # now use the coefficient in
        # y(attributable) = baselineIncidence x (exp(beta * X) - 1) x Pop

        # TASK WRITE AS A FUNCTION
        # attributable <- function(glmest = summary(interactionDrtAgeSexRuralModel3)$coefficients,
        #                           stratum= 'DrtFemales30_49rural'){
        glmest<-summary(interactionDrtAgeSexRuralModel3)$coefficients
        betai <- glmest[which(row.names(glmest)=='DrtFemales30_49rural'),1]
        sei <- glmest[which(row.names(glmest)=='DrtFemales30_49rural'),2]
        xvar.q1q3 <- IQR(data$logDroughtCount)
        xvar.q1q3
        # [1] 1.122753
        exp(betai*xvar.q1q3)
        #[1] 0.8771938
        exp((betai - sei * 1.96) * xvar.q1q3)
        #[1] 0.7707765
        exp((betai + sei * 1.96) * xvar.q1q3)
        #[1] 0.9983036
        # estimate only for  DrtFemales30_49rural
        attributable <- subset(data2, rural == 1 & sex ==2 & agegp2 == '30_49')
        table(attributable$sd_group)
        attach(attributable)

        attributable$deathsAttributable <-
        (avgMonthlyDeaths/avgPop) * (exp(betai * logDroughtCount) - 1) * pop
        # SE
        #LCI
        attributable$deathsAttributableLower <-
        (avgMonthlyDeaths/avgPop) * (exp((betai - sei * 1.96) *  logDroughtCount) - 1) * pop
        #UCI
        attributable$deathsAttributableUpper <-
        (avgMonthlyDeaths/avgPop) * (exp((betai + sei * 1.96) * logDroughtCount) - 1) * pop

        detach(attributable)
        head(attributable)


        # now summarise by year
        summaryAttributable <- sqldf(
        'select dthyy, sum(deathsAttributable) as deathsAttributable,
     sum(deaths) as deaths,
        sum(pop) as pop,
        round(avg(logDroughtCount),0) as logDroughtCount
        from attributable
        group by dthyy
        order by dthyy
        ')
  summaryAttributable
        # plot the estimated deaths
        with(summaryAttributable,
        plot(dthyy, deathsAttributable/deaths, type = 'l')
        )
        par(new=T)
        with(summaryAttributable,
        plot(dthyy, logDroughtCount, type = 'l',col = 'blue')
        )
        par(new=T)
        with(summaryAttributable,
        plot(dthyy, deaths, type = 'b',col = 'darkblue', pch=16)
        )
        # calcualte estimate

        estOut <- sqldf(
        'select sum(deaths) as deaths,
    sum(deathsAttributable) as deathsAttributable,
        sum(deathsAttributableLower) as deathsAttributableLower,
        sum(deathsAttributableUpper) as deathsAttributableUpper
        from attributable
        ')

        # The predicted DECREASED number of rural female suicides aged 30-49 per annum associated with droughts over our study period was -0.72 (95%CI -1.32 to -0.01)
        estOut$deathsAttributable
        # [1] -27.29876
        length(2007:1970)
        estOut$deathsAttributable / 38
        # [1] -0.7183885
        estOut$deathsAttributableLower / 38
        # [1] -1.318613
        estOut$deathsAttributableUpper / 38
        # [1] -0.01011795






###################################################
### code chunk number 37: SuiDrtNSW_SupportingInfo.Rnw:2591-2678
###################################################
######################
#do,  Test the Sex Stratification
######################



        # CONSTRUCT NEW INTERACTION VARIABLE, STRATIFY BY AGE ONLY NOT SEX
        data$Drt10_29urban <- ifelse(data$agegp2 == '10_29' & data$rural == 0, data$logDroughtCount, 0)
        data$Drt30_49urban <- ifelse(data$agegp2 == '30_49' & data$rural == 0, data$logDroughtCount, 0)
        data$Drt50plusurban <- ifelse(data$agegp2 == '50plus' & data$rural == 0, data$logDroughtCount, 0)

        data$Drt10_29rural <- ifelse(data$agegp2 == '10_29' & data$rural == 1, data$logDroughtCount, 0)
        data$Drt30_49rural <- ifelse(data$agegp2 == '30_49' & data$rural == 1, data$logDroughtCount, 0)
        data$Drt50plusrural <- ifelse(data$agegp2 == '50plus' & data$rural == 1, data$logDroughtCount, 0)

        # FIT WITHOUT DROUGHT 30-49
        require(splines)
        strt=Sys.time()
        interactionDrtAgeSexRuralModelNot3049bySex <- glm(deaths ~
        Drt30_49rural
        + sin(timevar*2*pi) + cos(timevar*2*pi)
        + DrtMales10_29rural

        + DrtMales50plusrural
        + DrtFemales10_29rural

        + ns(DrtFemales50plusrural, df = 5)
        + ns(DrtMales10_29urban, df = 6)
        + DrtMales30_49urban
        + ns(DrtMales50plusurban, df = 4)
        + DrtFemales10_29urban
        + ns(DrtFemales30_49urban, df = 3)
        + DrtFemales50plusurban
        + tmax_anomaly
        + agegp2
        + rural
        + sd_group
        + sex
        + agegp
        + agegp*sex*ns(time,3)
        + offset(log(pop)), data=data,family=poisson)
        save.image()
        endd=Sys.time()
        print(endd-strt)

        ###################################################################
        # likelihood ratio tests
        require(lmtest)
        lrtest(interactionDrtAgeSexRuralModel3, interactionDrtAgeSexRuralModelNot3049bySex)
        # Likelihood ratio test
        #
        # Model 1: deaths ~ sin(timevar * 2 * pi) + cos(timevar * 2 * pi) + tmax_anomaly +
        #     DrtMales10_29rural + DrtMales30_49rural + DrtMales50plusrural +
        #     DrtFemales10_29rural + DrtFemales30_49rural + ns(DrtFemales50plusrural,
        #     df = 5) + ns(DrtMales10_29urban, df = 6) + DrtMales30_49urban +
        #     ns(DrtMales50plusurban, df = 4) + DrtFemales10_29urban +
        #     ns(DrtFemales30_49urban, df = 3) + DrtFemales50plusurban +
        #     agegp2 + rural + sd_group + sex + agegp + agegp * sex * ns(time,
        #     3) + offset(log(pop))
        # Model 2: deaths ~ Drt30_49rural + sin(timevar * 2 * pi) + cos(timevar *
        #     2 * pi) + DrtMales10_29rural + DrtMales50plusrural + DrtFemales10_29rural +
        #     ns(DrtFemales50plusrural, df = 5) + ns(DrtMales10_29urban,
        #     df = 6) + DrtMales30_49urban + ns(DrtMales50plusurban, df = 4) +
        #     DrtFemales10_29urban + ns(DrtFemales30_49urban, df = 3) +
        #     DrtFemales50plusurban + tmax_anomaly + agegp2 + rural + sd_group +
        #     sex + agegp + agegp * sex * ns(time, 3) + offset(log(pop))
        #   #Df LogLik Df Chisq Pr(>Chisq)
        # 1  81 -34483
        # 2  80 -34490 -1 15.64   7.66e-05 ***
        # ---



        # BICs
        matrix(estat(interactionDrtAgeSexRuralModel3, 'With Rural 30-49 Sex Strata'))
        # [1,] "With Rural 30-49 Sex Strata"
        # [2,] "98"
        # [aic,] "69127.3020918036"
        # [bic,] "69868.8611256404"
        # [5,] "24.5408746859892"
        matrix(estat(interactionDrtAgeSexRuralModelNot3049bySex, 'Without Rural 30-49 Sex Strata'))
        # [1,] "Without Rural 30-49 Sex Strata"
        # [2,] "97"
        # [aic,] "69140.9425169068"
        # [bic,] "69873.3465009432"
        # [5,] "24.5102710907194"



###################################################
### code chunk number 38: SuiDrtNSW_SupportingInfo.Rnw:2708-2798
###################################################
######################
#do,  Enhanced Drought Index
######################



        data$logDroughtCount2 = log1p(data$avcount2)

        data$Drt2Males10_29urban <- ifelse(data$agegp2 == '10_29' & data$sex == 1 & data$rural == 0, data$logDroughtCount2, 0)
        data$Drt2Males30_49urban <- ifelse(data$agegp2 == '30_49' & data$sex == 1 & data$rural == 0, data$logDroughtCount2, 0)
        data$Drt2Males50plusurban <- ifelse(data$agegp2 == '50plus' & data$sex == 1 & data$rural == 0, data$logDroughtCount2, 0)

        data$Drt2Males10_29rural <- ifelse(data$agegp2 == '10_29' & data$sex == 1 & data$rural == 1, data$logDroughtCount2, 0)
        data$Drt2Males30_49rural <- ifelse(data$agegp2 == '30_49' & data$sex == 1 & data$rural == 1, data$logDroughtCount2, 0)
        data$Drt2Males50plusrural <- ifelse(data$agegp2 == '50plus' & data$sex == 1 & data$rural == 1, data$logDroughtCount2, 0)

        data$Drt2Females10_29urban <- ifelse(data$agegp2 == '10_29' & data$sex == 2 & data$rural == 0, data$logDroughtCount2, 0)
        data$Drt2Females30_49urban <- ifelse(data$agegp2 == '30_49' & data$sex == 2 & data$rural == 0, data$logDroughtCount2, 0)
        data$Drt2Females50plusurban <- ifelse(data$agegp2 == '50plus' & data$sex == 2 & data$rural == 0, data$logDroughtCount2, 0)

        data$Drt2Females10_29rural <- ifelse(data$agegp2 == '10_29' & data$sex == 2 & data$rural == 1, data$logDroughtCount2, 0)
        data$Drt2Females30_49rural <- ifelse(data$agegp2 == '30_49' & data$sex == 2 & data$rural == 1, data$logDroughtCount2, 0)
        data$Drt2Females50plusrural <- ifelse(data$agegp2 == '50plus' & data$sex == 2 & data$rural == 1, data$logDroughtCount2, 0)


        strt=Sys.time()





        interactionDrtAgeSexRuralModel2enhanced <- gam(deaths ~  s(mm, k=3, fx=T, bs = 'cp')
        + s(Drt2Males10_29rural)
        + s(Drt2Males30_49rural)
        + s(Drt2Males50plusrural)
        + s(Drt2Females10_29rural)
        + s(Drt2Females30_49rural)
        + s(Drt2Females50plusrural)
        + s(Drt2Males10_29urban)
        + s(Drt2Males30_49urban)
        + s(Drt2Males50plusurban)
        + s(Drt2Females10_29urban)
        + s(Drt2Females30_49urban)
        + s(Drt2Females50plusurban)
        + s(tmax_anomaly)
        + agegp2
        + rural
        + sd_group
        + sex
        + agegp
        + agegp*sex*ns(time,df = 3)
        + offset(log(pop)), data=data,family=poisson)
        save.image()

        endd=Sys.time()
        print(endd-strt)

        summary(interactionDrtAgeSexRuralModel2enhanced)
        Rsquared.glm.gsm(interactionDrtAgeSexRuralModel2enhanced)

        # make a plot of each group effect
        ylimits <- 0.5

        png('interactionDrtAgeSexRuralModel2enhanced.png',res=200,width = 1000, height = 800)#, pointsize =30)
        par(mfcol=c(3,2),mar=c(4,5,2,1), cex = .5)
        # rural males
        plot(interactionDrtAgeSexRuralModel2enhanced,select=2,se=T, ylim = c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        abline(0,0)
        title('DrtMales10_29rural')
        plot(interactionDrtAgeSexRuralModel2enhanced,select=3,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        abline(0,0)
        title('DrtMales30_49rural')
        plot(interactionDrtAgeSexRuralModel2enhanced,select=4,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        abline(0,0)
        title('DrtMales50plusrural')

        # rural females
        plot(interactionDrtAgeSexRuralModel2enhanced,select=5,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        title('DrtFemales10_29rural')
        abline(0,0)
        plot(interactionDrtAgeSexRuralModel2enhanced,select=6,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        abline(0,0)
        title('DrtFemales30_49rural')
        plot(interactionDrtAgeSexRuralModel2enhanced,select=7,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        abline(0,0)
        title('DrtFemales50plusrural')

        dev.off()




###################################################
### code chunk number 39: SuiDrtNSW_SupportingInfo.Rnw:2819-2899
###################################################
######################
#do,  Self-harm Coded as Undetermined
######################



        # plot of coding changes
        d2 <- dbGetQuery(ch,
        '
  select dthyy, sum(deaths) as sui, sum(sui_und) as sui_und
        from ivan_hanigan.suicidedroughtnsw19702007_rates_drought
        group by dthyy
        order by dthyy
        ')
        png('undetermined.png')
        with(d2,plot(dthyy,sui_und,type='l',ylim=c(0,950)))
        with(d2,lines(dthyy,sui,col='red'))
        dev.off()
        # sensitivity model
        strt=Sys.time()
        interactionDrtAgeSexRuralModel2SuicidePlusUndetermined <- gam(sui_und ~  s(mm, k=3, fx=T, bs = 'cp')
        + s(DrtMales10_29rural)
        + s(DrtMales30_49rural)
        + s(DrtMales50plusrural)
        + s(DrtFemales10_29rural)
        + s(DrtFemales30_49rural)
        + s(DrtFemales50plusrural)
        + s(DrtMales10_29urban)
        + s(DrtMales30_49urban)
        + s(DrtMales50plusurban)
        + s(DrtFemales10_29urban)
        + s(DrtFemales30_49urban)
        + s(DrtFemales50plusurban)
        + tmax_anomaly
        + agegp2
        + rural
        + sd_group
        + sex
        + agegp
        + agegp*sex*ns(time,df = 3)
        + offset(log(pop)), data=data,family=poisson)
        save.image()

        endd=Sys.time()
        print(endd-strt)

        summary(interactionDrtAgeSexRuralModel2SuicidePlusUndetermined)
        Rsquared.glm.gsm(interactionDrtAgeSexRuralModel2SuicidePlusUndetermined)

        # make a plot of each group effect
        ylimits <- 0.5

        png('interactionDrtAgeSexRuralModel2SuicidePlusUndetermined.png',res=200,width = 1000, height = 800)#, pointsize =30)
        par(mfcol=c(3,2),mar=c(4,5,2,1), cex = .5)
        # rural males
        plot(interactionDrtAgeSexRuralModel2SuicidePlusUndetermined,select=2,se=T, ylim = c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        abline(0,0)
        title('DrtMales10_29rural')
        plot(interactionDrtAgeSexRuralModel2SuicidePlusUndetermined,select=3,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        abline(0,0)
        title('DrtMales30_49rural')
        plot(interactionDrtAgeSexRuralModel2SuicidePlusUndetermined,select=4,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        abline(0,0)
        title('DrtMales50plusrural')

        # rural females
        plot(interactionDrtAgeSexRuralModel2SuicidePlusUndetermined,select=5,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        title('DrtFemales10_29rural')
        abline(0,0)
        plot(interactionDrtAgeSexRuralModel2SuicidePlusUndetermined,select=6,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        abline(0,0)
        title('DrtFemales30_49rural')
        plot(interactionDrtAgeSexRuralModel2SuicidePlusUndetermined,select=7,se=T, ylim =  c(-ylimits,ylimits), shade=TRUE,shade.col='grey')
        abline(0,0)
        title('DrtFemales50plusrural')

        dev.off()





###################################################
### code chunk number 40: SuiDrtNSW_SupportingInfo.Rnw:2910-2980
###################################################
######################
#do,  Drop High Leverage Points
######################



        # data2 <- na.omit(df[,c('cvd','tmax','dptp','time')])
        data2 <- data
        data2$hatvalue <- hatvalues(interactionDrtAgeSexRuralModel3)
        data2$res <- residuals(interactionDrtAgeSexRuralModel3, 'pearson')
        summary(data2$hatvalue)
        summary(data2$res)

        png('interactionDrtAgeSexRuralModel3checkLeverage.png',res=200,width = 1000, height = 1000)
        with(data2, plot(hatvalue, res))
        with(subset(data2, hatvalue > 0.023), points(hatvalue, res, col = 'red', pch = 16))
        with(subset(data2, res > 15), points(hatvalue, res, col = 'red', pch = 16))
        abline(0,0)
        segments(0.023,-2,0.023,15)
        dev.off()

        data3 <- subset(data2, hatvalue < 0.023)

        interactionDrtAgeSexRuralModel3noLeverage <- glm(deaths ~ sin(timevar * 2 * pi) + cos(timevar *
        2 * pi) + tmax_anomaly + DrtMales10_29rural + DrtMales30_49rural +
        DrtMales50plusrural + DrtFemales10_29rural + DrtFemales30_49rural +
        ns(DrtFemales50plusrural, df = 5) + ns(DrtMales10_29urban,
        df = 6) + DrtMales30_49urban + ns(DrtMales50plusurban, df = 4) +
        DrtFemales10_29urban + ns(DrtFemales30_49urban, df = 3) +
        DrtFemales50plusurban + agegp2 + rural + sd_group + sex +
        agegp + agegp * sex * ns(time, 3) + offset(log(pop)), family = poisson,
        data = data3
        )

        # # only worked on R 2.10?  NEEDED TO CHANGE THE TERMS BIZZO
        png('interactionDrtAgeSexRuralModel3noLeverage.png',res=200,width = 1000, height = 1000)
        par(mfcol=c(3,2),mar=c(4,5,1,1))
        termplot(interactionDrtAgeSexRuralModel3noLeverage,se=T,ask=F,terms = attr(terms(interactionDrtAgeSexRuralModel3noLeverage),'term.labels') [4],col.term='black',col.se='black')
        abline(0,0)
        termplot(interactionDrtAgeSexRuralModel3noLeverage,se=T,ask=F,terms=attr(terms(interactionDrtAgeSexRuralModel3noLeverage),'term.labels') [5],col.term='black',col.se='black')
        abline(0,0)
        termplot(interactionDrtAgeSexRuralModel3noLeverage,se=T,ask=F,terms=attr(terms(interactionDrtAgeSexRuralModel3noLeverage),'term.labels') [6],col.term='black',col.se='black')
        abline(0,0)
        termplot(interactionDrtAgeSexRuralModel3noLeverage,se=T,ask=F,terms=attr(terms(interactionDrtAgeSexRuralModel3noLeverage),'term.labels') [7],col.term='black',col.se='black')
        abline(0,0)
        termplot(interactionDrtAgeSexRuralModel3noLeverage,se=T,ask=F,terms=attr(terms(interactionDrtAgeSexRuralModel3noLeverage),'term.labels') [8],col.term='black',col.se='black')
        abline(0,0)
        termplot(interactionDrtAgeSexRuralModel3noLeverage,se=T,ask=F,terms=attr(terms(interactionDrtAgeSexRuralModel3noLeverage),'term.labels') [9],col.term='black',col.se='black')
        abline(0,0)
        dev.off()

        data3$hatvalue2 <- hatvalues(interactionDrtAgeSexRuralModel3noLeverage)
        data3$res2 <- residuals(interactionDrtAgeSexRuralModel3noLeverage, 'pearson')
        with(data3, plot(hatvalue2, res2))
        summary(data3$hatvalue2)


        glmest<-summary(interactionDrtAgeSexRuralModel3noLeverage)$coefficients
        betai <- glmest[which(row.names(glmest)=='DrtMales30_49rural'),1]
        sei <- glmest[which(row.names(glmest)=='DrtMales30_49rural'),2]
        xvar.q1q3 <- IQR(data$logDroughtCount)
        exp(betai*xvar.q1q3)
        exp((betai - sei * 1.96) * xvar.q1q3)
        exp((betai + sei * 1.96) * xvar.q1q3)

        drop1(interactionDrtAgeSexRuralModel3noLeverage, test = 'Chisq')
        par(mfrow=c(2,2))
        plot(interactionDrtAgeSexRuralModel3noLeverage)




###################################################
### code chunk number 41: SuiDrtNSW_SupportingInfo.Rnw:3051-3079
###################################################
######################
#do,  show model fig1 and 2
######################


# first fit the model
        interactionDrtAgeSexRuralModel2 <- gam(deaths ~  s(mm, k=3, fx=T, bs = 'cp')
        + s(DrtMales10_29rural)
        + s(DrtMales30_49rural)
        + s(DrtMales50plusrural)
        + s(DrtFemales10_29rural)
        + s(DrtFemales30_49rural)
        + s(DrtFemales50plusrural)
        + s(DrtMales10_29urban)
        + s(DrtMales30_49urban)
        + s(DrtMales50plusurban)
        + s(DrtFemales10_29urban)
        + s(DrtFemales30_49urban)
        + s(DrtFemales50plusurban)
        + s(tmax_anomaly)
        + agegp2
        + rural
        + sd_group
        + sex
        + agegp
        + agegp*sex*ns(time,df = 3)
        + offset(log(pop)), data=data,family=poisson)



###################################################
### code chunk number 42: SuiDrtNSW_SupportingInfo.Rnw:3090-3134
###################################################
######################
#do,  show plot fig 1 and 2
######################



        # now make a plot of each group effects

        png('RuralMales20.png',res=200,width = 600, height = 1000)
        layout(matrix(c(1:4),ncol=1),heights=c(1,1,1,0.2))
        par(mfrow=c(4,1), mar=c(0.1,4,1.5,0.5), cex=.7)
        plot(interactionDrtAgeSexRuralModel2,select=2,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = '',xlab = '', xaxt='n')
        abline(0,0)
        title('Rural Males aged 10-29', cex=.5, font.main = 1)
        plot(interactionDrtAgeSexRuralModel2,select=3,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = 'log(Relative Risk)',xlab = '', xaxt='n')
        abline(0,0)
        title('Rural Males aged 30-49', cex=.5, font.main = 1)
        plot(interactionDrtAgeSexRuralModel2,select=4,rug=F,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = '',xlab = '')
        abline(0,0)
        title('Rural Males aged 50 plus', cex=.5, font.main = 1)
        par(mar=c(1,4,6,0.5))
        plot(1,1,type = 'n', xaxt = 'n', yaxt='n',ylab='',xlab='', axes = F)
        title(main = 'Drought Index: log(1 + count months)',  font.main = 1,cex.main=.9)
        dev.off()


        png('RuralFemales20.png',res=200,width = 600, height = 1000)
        layout(matrix(c(1:4),ncol=1),heights=c(1,1,1,0.2))
        par(mfrow=c(4,1), mar=c(0.1,4,1.5,0.5), cex=.7)
        plot(interactionDrtAgeSexRuralModel2,select=5,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = '',xlab = '', xaxt='n')
        abline(0,0)
        title('Rural Females aged 10-29', cex=.5, font.main = 1)
        plot(interactionDrtAgeSexRuralModel2,select=6,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = 'log(Relative Risk)',xlab = '', xaxt='n')
        abline(0,0)
        title('Rural Females aged 30-49', cex=.5, font.main = 1)
        plot(interactionDrtAgeSexRuralModel2,select=7,rug=F,se=T, ylim = c(-0.8,0.8), shade=TRUE,shade.col='grey', ylab = '',xlab = '')
        abline(0,0)
        title('Rural Females aged 50 plus', cex=.5, font.main = 1)
        par(mar=c(1,4,6,0.5))
        plot(1,1,type = 'n', xaxt = 'n', yaxt='n',ylab='',xlab='', axes = F)
        title(main = 'Drought Index: log(1 + count months)',  font.main = 1,cex.main=.9)
        dev.off()




