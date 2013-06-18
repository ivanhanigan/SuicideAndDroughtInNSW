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
data <- dbGetQuery(ch,
"
select cast(dthyy || '-' || dthmm || '-' || 1 as date) as time, *
from ivan_hanigan.suicidedroughtnsw19702007_rates_drought
")
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
### code chunk number 30: SuiDrtNSW_SupportingInfo.Rnw:1586-1680
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

pdf('tmaxanom2.pdf')
par(mar=c(4,4.2,.5,1))
plot(interactionDrtAgeSexRuralModel2,select=14,rug = T, all.terms=T,se=T,ylab = 'log Relative Risk',xlab='Maximum Temperature Anomaly', ylim=c (-.15,0.15), shade=T)
abline(0,0,lwd=1.5)
dev.off()
