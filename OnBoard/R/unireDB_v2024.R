library(magrittr)
library(dplyr)
library(purrr)
library(readxl)
library(ggplot2)
library(RODBC)
library(gridExtra)
library(stringr)
library(grid)
rm(list = ls())
'%ni%'=Negate('%in%')
wd_acces="C:/Users/a.palermino/OneDrive - CNR/Assegno Scarcella/Solemon/Solemon 2024/OnBoard/access"
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

source_file <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_template.accdb")
file.copy(source_file,  paste0(wd_acces,"/bio_data_v2024_SOLEMON_complete.accdb"), overwrite = TRUE)
file.copy(source_file,  paste0(wd_acces,"/bio_data_v2024_SOLEMON_BENTHOS_complete.accdb"), overwrite = TRUE)



store.hauls=list()

# get info from db 1
MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_2024_ENA.accdb")
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
channel <- odbcDriverConnect(PATH)
acctables=sqlTables(channel) # look for tables
close(channel)

# tables to be checked
tables_check=acctables[grep('ala' ,acctables$TABLE_NAME),]$TABLE_NAME
tables_check=tables_check[-grep('template', tables_check)]
tables_check=tables_check[-grep('test', tables_check)]
tables_check=data.frame(original_name=tables_check)
tables_check$format_name=tables_check$original_name
#tables_check$original_name=str_remove(tables_check$original_name, 'bis')
tables_check$original_name=tolower(tables_check$original_name)
tables_check$original_name=str_remove(tables_check$original_name, 'cala')
tables_check$original_name=str_remove(tables_check$original_name, '_')
tables_check$original_name=str_remove(tables_check$original_name, ' ')
tables_check$original_name=str_remove(tables_check$original_name, ' ')

# get info from db 2
MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_2024_FRA.accdb")
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
channel <- odbcDriverConnect(PATH)
acctables=sqlTables(channel) # look for tables
close(channel)

# tables to be checked
tables_check2=acctables[grep('ala' ,acctables$TABLE_NAME),]$TABLE_NAME
tables_check2=tables_check2[-grep('template', tables_check2)]
tables_check2=tables_check2[-grep('test', tables_check2)]
tables_check2=data.frame(original_name=tables_check2)
tables_check2$format_name=tables_check2$original_name
#tables_check2$original_name=str_remove(tables_check2$original_name, 'bis')
tables_check2$original_name=tolower(tables_check2$original_name)
tables_check2$original_name=str_remove(tables_check2$original_name, 'cala')
tables_check2$original_name=str_remove(tables_check2$original_name, '_')
tables_check2$original_name=str_remove(tables_check2$original_name, ' ')
tables_check2$original_name=str_remove(tables_check2$original_name, ' ')
tables_check2$original_name=str_remove(tables_check2$original_name, '_')
tables_check2$original_name=str_remove(tables_check2$original_name, 'd')
tables_check2$original_name=str_remove(tables_check2$original_name, 'a')


# find hauls in databases
common_hauls=unique(c(tables_check[tables_check$original_name %in% tables_check2$original_name,]$original_name,
tables_check2[tables_check2$original_name %in% tables_check$original_name,]$original_name))

hauls_db_ENA=tables_check[tables_check$original_name %ni% common_hauls,]
hauls_db_ENA$id=seq(1:nrow(hauls_db_ENA))

hauls_db_FRA=tables_check2[tables_check2$original_name %ni% common_hauls,]
hauls_db_FRA$id=seq(1:nrow(hauls_db_FRA))

# find hauls with data taken onboard and to be added
dir.target.ob="C:/Users/a.palermino/OneDrive - CNR/Assegno Scarcella/Solemon/Solemon 2024/OnBoard/data/onboard_measures"
x.files.ob=data.frame(target.file=list.files(path=dir.target.ob))
x.files.ob$hauls.ob=str_remove(str_remove(x.files.ob$target.file,'_onboard_meas.csv'),'haul_')

# upload on final db ####
# hauls only in db 0
for(icheck in 1:nrow(hauls_db_ENA)){
  
  haul=hauls_db_ENA[icheck, ]$format_name
  haul.name=paste('cala',hauls_db_ENA[icheck, ]$original_name,sep='_')
  cat(haul.name)
  # import table 1
  MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_2024_ENA.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  xdat <- sqlQuery(channel,
                   paste0("SELECT * FROM [", haul, "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  
  
  close(channel)
  
  # check if more data needs to be added #
  if(str_remove(haul,'cala_')%in%x.files.ob$hauls.ob){
    tgt.ob=x.files.ob[x.files.ob$hauls.ob==str_remove(haul,'cala_'),]
    xdat.ob=read.csv(file.path(dir.target.ob, tgt.ob$target.file))
    
    # correct errors
    xdat.ob$Species=ifelse(xdat.ob$Species=='PTERBOV','PTEOBOV',xdat.ob$Species)
    xdat.ob$Species=ifelse(xdat.ob$Species=='Ostrea edulis','OSTREDU',xdat.ob$Species)
    xdat.ob$Species=ifelse(xdat.ob$Species=='SCIOCAN','SCYOCAN',xdat.ob$Species)
    
    # 
    xdat.template=xdat[1,]
    xdat.template[1,]=NA
    xdat.template=rbind(xdat.template, xdat.template[rep(1, nrow(xdat.ob)-1), ])
    xdat.template$gear=toupper(xdat.ob$Gear)
    xdat.template$species_name=toupper(xdat.ob$Species)
    xdat.template$length_mm=xdat.ob$Lenght
    xdat.ob$Sex=ifelse(xdat.ob$Sex=='FALSE','F',xdat.ob$Sex)
    xdat.template$Sex=toupper(xdat.ob$Sex)
    xdat.template$Mat=xdat.ob$Mat
    xdat.template$kg_field1=xdat.ob$Total.weight
    xdat.template$weight_g=xdat.ob$Total.weight*1000
    if(haul!='cala_37'){
     xdat.template$weight_g=ifelse(xdat.template$species_name%in%c('HEXATRU','OSTREDU','MUREBRA'),NA,xdat.template$weight_g) 
    }
    xdat.template$total_number=xdat.ob$Number
    xdat.template$ID=seq(1:nrow(xdat.template))+max(xdat$ID)
    if(tgt.ob$hauls.ob=='4'){
      xdat.template$length_mm=930
      xdat.template$length_field2=570
      xdat.template$length_field3=350
    }
    xdat=rbind(xdat, xdat.template)
  }
  
  # save on final db
  
  #MDBPATH <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_complete.accdb")
  #PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  #channel <- odbcDriverConnect(PATH)
  #sqlSave(channel, dat=data.frame(xdat), tablename = haul.name)
  #close(channel)
  xdat$ID=1:nrow(xdat)
  store.hauls[[icheck]]=xdat
  names(store.hauls)[icheck]=haul
  
}


# hauls only in db 1
for(icheck in 1:nrow(hauls_db_FRA)){
  
  haul=hauls_db_FRA[icheck, ]$format_name
  haul.name=paste('cala',hauls_db_FRA[icheck, ]$original_name,sep='_')
  
  # import table 1
  MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_2024_FRA.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  xdat <- sqlQuery(channel,
                   paste0("SELECT * FROM [", haul, "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  
  
  close(channel)
  
  # check if more data needs to be added #
  if(str_remove(haul,'cala_')%in%x.files.ob$hauls.ob){
    tgt.ob=x.files.ob[x.files.ob$hauls.ob==str_remove(haul,'cala_'),]
    xdat.ob=read.csv(file.path(dir.target.ob, tgt.ob$target.file))
    xdat.ob$Species=ifelse(xdat.ob$Species=='Cassidaria','GALEECH',xdat.ob$Species)
    xdat.ob$Species=ifelse(xdat.ob$Species=='Bolinus brandaris','MUREBRA',xdat.ob$Species)
    
    if(tgt.ob$hauls.ob=='48'){
      xdat.ob$Lenght=as.numeric(substr(xdat.ob$Lenght,1,3))
    }
    if(tgt.ob$hauls.ob=='55'){
      xdat.ob$Total.weight=xdat.ob$Total.weight..kg.
    }
    if(tgt.ob$hauls.ob=='68'){
      xdat.ob$Total.weight=xdat.ob$Total.weight.kg
    }
    if(tgt.ob$hauls.ob=='75'){
      xdat.ob$Total.weight=xdat.ob$Total.weight=5000
      xdat.ob$Lenght=20
      xdat.ob$Species='OCTOVUL'
    }
    xdat.template=xdat[1,]
    xdat.template[1,]=NA
    xdat.template=rbind(xdat.template, xdat.template[rep(1, nrow(xdat.ob)-1), ])
    xdat.template$gear=toupper(xdat.ob$Gear)
    xdat.template$species_name=toupper(xdat.ob$Species)
    xdat.template$length_mm=xdat.ob$Lenght
    xdat.ob$Sex=ifelse(xdat.ob$Sex=='FALSE','F',xdat.ob$Sex)
    xdat.template$Sex=toupper(xdat.ob$Sex)
    xdat.template$Mat=xdat.ob$Mat
    xdat.template$kg_field1=xdat.ob$Total.weight
    xdat.template$weight_g=xdat.ob$Total.weight*1000
    xdat.template$weight_g=ifelse(xdat.template$species_name%in%c('HEXATRU','OSTREDU','MUREBRA'),NA,xdat.template$weight_g)
    if(tgt.ob$hauls.ob%in%c('12','75')){
      xdat.template$weight_g=xdat.ob$Total.weight
    }
    if(tgt.ob$hauls.ob=='55'){
      xdat.template$weight_g=NA
    }
    xdat.template$total_number=xdat.ob$Number
    xdat.template$ID=seq(1:nrow(xdat.template))+max(xdat$ID)
    xdat=rbind(xdat, xdat.template)
  }
  
  xdat$ID=1:nrow(xdat)
  # save on final db
  store.hauls=append(store.hauls, list(xdat))
  names(store.hauls)[length(store.hauls)]=haul
  
  
  #MDBPATH <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_complete.accdb")
  #PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  #channel <- odbcDriverConnect(PATH)
  #sqlSave(channel, dat=data.frame(xdat), tablename = haul.name)
  #close(channel)
  
}


# hauls in common
for(icheck in 1:length(common_hauls)){
  
  haul=tables_check[tables_check$original_name==common_hauls[icheck], ]$format_name
  haulbis=tables_check2[tables_check2$original_name==common_hauls[icheck], ]$format_name
  haul.name=paste('cala',tables_check[tables_check$original_name==common_hauls[icheck], ]$original_name,sep='_')
  
  cat(haul.name)
  # Table DB BIS
  MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_2024_FRA.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  xdat <- sqlQuery(channel,
                   paste0("SELECT * FROM [", haul, "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  
  
  close(channel)
  
  # Table DB principale
  MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_2024_ENA.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  xdatbis <- sqlQuery(channel,
                      paste0("SELECT * FROM [", paste0(haulbis), "] ORDER BY [ID]"),
                      stringsAsFactors = FALSE) # Load data into R dataframe
  close(channel)
  print(paste(haul, identical(xdat, xdatbis)))
  
  # cala 29 corretto il SOLEMON_2023_1
  # cala 31 corretto il SOLEMON_2023_1
  # cala 32 corretto il SOLEMON_2023_1
  
  if(str_remove(haul,'cala_')%in%x.files.ob$hauls.ob){
    tgt.ob=x.files.ob[x.files.ob$hauls.ob==str_remove(haul,'cala_'),]
    xdat.ob=read.csv(file.path(dir.target.ob, tgt.ob$target.file))
    
    # correct errors
    xdat.ob$Species=ifelse(xdat.ob$Species=='Ostrea edulis','OSTREDU',xdat.ob$Species)
    xdat.ob$Species=ifelse(xdat.ob$Species=='SCIOCAN','SCYOCAN',xdat.ob$Species)
    if(tgt.ob$hauls.ob=='4'){
      xdat.ob$Lenght=930
    }
    # 
    xdat.template=xdat[1,]
    xdat.template[1,]=NA
    xdat.template=rbind(xdat.template, xdat.template[rep(1, nrow(xdat.ob)-1), ])
    xdat.template$gear=toupper(xdat.ob$Gear)
    xdat.template$species_name=toupper(xdat.ob$Species)
    xdat.template$length_mm=xdat.ob$Lenght
    xdat.ob$Sex=ifelse(xdat.ob$Sex=='FALSE','F',xdat.ob$Sex)
    xdat.template$Sex=toupper(xdat.ob$Sex)
    xdat.template$Mat=xdat.ob$Mat
    xdat.template$kg_field1=xdat.ob$Total.weight
    xdat.template$weight_g=xdat.ob$Total.weight*1000
    xdat.template$weight_g=ifelse(xdat.template$species_name%in%c('HEXATRU','OSTREDU','MUREBRA'),NA,xdat.template$weight_g)
    xdat.template$total_number=xdat.ob$Number
    xdat.template$ID=seq(1:nrow(xdat.template))+max(xdat$ID)
    xdat=rbind(xdat, xdat.template)
  }
  
  xdatbis$ID=seq(max(xdat$ID)+1,(max(xdat$ID)+nrow(xdatbis)),1)
  xdat.combined=rbind(xdat, xdatbis)
  
  # save on final db
  #MDBPATH <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_complete.accdb")
  #PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  #channel <- odbcDriverConnect(PATH)
  #sqlSave(channel, dat=data.frame(xdat.combined), tablename = haul.name)
  #close(channel)
  
  store.hauls=append(store.hauls, list(xdat.combined))
  names(store.hauls)[length(store.hauls)]=haul
  
}


#### check if there is something to add from the otolith db and then save
get_tables=function(db,wd_acces="C:/Users/a.palermino/OneDrive - CNR/Assegno Scarcella/Solemon/Solemon 2024/OnBoard/access"){
  MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_",db,".accdb") 
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  acctables=sqlTables(channel) # look for tables
  close(channel)
  tables_check=acctables[grep('ala' ,acctables$TABLE_NAME),]$TABLE_NAME
  #tables_check=tables_check[-grep('template', tables_check)]
  #tables_check=tables_check[-grep('test', tables_check)]
  return(tables_check)
}
tables.oto=get_tables('2024_OTO')

for(icheck in 1:length(store.hauls)){
  i.haul=store.hauls[[icheck]]
  i.nm=names(store.hauls)[icheck]
  cat(i.nm)
  if(i.nm %in% tables.oto){
    MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_2024_OTO.accdb")
    PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
    channel <- odbcDriverConnect(PATH)
    xdat <- sqlQuery(channel,
                     paste0("SELECT * FROM [", i.nm, "] ORDER BY [ID]"),
                     stringsAsFactors = FALSE) # Load data into R dataframe
    close(channel)
    xdat$Sex=ifelse(xdat$Sex=='FALSE','F',xdat$Sex)
    xdat$ID=seq(max(i.haul$ID)+1,(max(i.haul$ID)+nrow(xdat)),1)
    i.haul=rbind(i.haul, xdat)
  }
  
  #### fix errors
  if(i.nm=='cala_25'){
    i.haul=i.haul[-which(i.haul$ID %in% 157:160),]
    i.haul$ID=seq(1:nrow(i.haul))
  }
  
  if(i.nm=='cala_45'){
    i.haul[i.haul$species_name%in% c('HIPPHIP', 'HIPGUT'),]$length_mm=i.haul[i.haul$species_name%in% c('HIPPHIP', 'HIPGUT'),]$length_mm*10
    i.haul[i.haul$species_name%in% c('HIPPHIP', 'HIPGUT'),]$weight_g=i.haul[i.haul$species_name%in% c('HIPPHIP', 'HIPGUT'),]$kg_field1*1000
    i.haul[i.haul$species_name%in% c('HIPPHIP', 'HIPGUT'),]$kg_field1=0
    i.haul[i.haul$species_name%in% c('HIPPHIP', 'HIPGUT'),]$species_name=c('HIPPHIC','HIPPGUT')
  }
  
  
  ### upload data
  
  MDBPATH <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_complete.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  sqlSave(channel, dat=data.frame(i.haul), tablename = i.nm, rownames = TRUE)
  close(channel)
}



##### merge benthos
benthos.dat=list()

tab.1=get_tables('2024_BENTHOS_FRA')
tab.1=tab.1[-grep('template', tab.1)]
tab.1=tab.1[-grep('test', tab.1)]

for(icheck in 1:length(tab.1)){
    i.nm=tab.1[icheck]
    cat(i.nm)
    MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_2024_BENTHOS_FRA.accdb")
    PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
    channel <- odbcDriverConnect(PATH)
    xdat <- sqlQuery(channel,
                     paste0("SELECT * FROM [", i.nm, "] ORDER BY [ID]"),
                     stringsAsFactors = FALSE) # Load data into R dataframe
    close(channel)
    benthos.dat[[icheck]]=xdat
    names(benthos.dat)[icheck]=i.nm
}


tab.2=get_tables('2024_ENA_BENTHOS')
tab.2=tab.2[-grep('template', tab.2)]
tab.2=tab.2[-grep('test', tab.2)]

for(icheck in 1:length(tab.2)){
  i.nm=tab.2[icheck]
  cat(i.nm)
  MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_2024_ENA_BENTHOS.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  xdat <- sqlQuery(channel,
                   paste0("SELECT * FROM [", i.nm, "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  close(channel)
  benthos.dat=append(benthos.dat, list(xdat))
  names(benthos.dat)[length(benthos.dat)]=i.nm
  
}


for(icheck in 1:length(benthos.dat)){
  i.haul=benthos.dat[[icheck]]
  i.nm=names(benthos.dat)[icheck]
  cat(i.nm)

  MDBPATH <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_BENTHOS_complete.accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  sqlSave(channel, dat=data.frame(i.haul), tablename = i.nm, rownames = TRUE)
  close(channel)
}

























