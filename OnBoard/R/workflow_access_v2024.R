
#
#
# This script controls the functions needed to preocess solemon data collected onboard.
# It is hosted at https://github.com/CNRFisheries/SoleMon_project
# If you consider to do any change, please contact the github page administrator.
#
# it runs on R 4.0.5 32 bit version due to compatibility issue with RODBC package needed to read from .accdb
# Last update: 18.11.2022
#
# functions are written in the 'functions_access_v2_0.R' file and are accompained by explanatory text.
# for usage information refer to the handbook 'handbook.HTML'.
#
#
rm(list = ls())
#main_wd=ifelse(Sys.info()[['user']]=="solemon_pc", 'C:/Users/solemon_pc/Desktop/solemon/2022##/raccolta_dati',
#               ifelse(Sys.info()[['user']]=="e.armelloni", "C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro#/Solemon/github/SoleMon_project/OnBoard", 
#                      ifelse(Sys.info()[['user']]=="Franc", "C:/Users/Franc/OneDrive/Desktop/solemon/2022#/raccolta_dati", NA))) 
#
main_wd="C:/Users/a.palermino/OneDrive - CNR/github/SoleMon_project/OnBoard"
setwd(main_wd)
source('R/functions_access_v2024.R')

# inspect lists of target species and shells
unique(target_species$species_name) # these are the species for which you collect individual length AND individual weight
shells  # these are the species for which you collect total weight and total number 
unique(haul_order$haul)
# set parameters
haul='all' # single haul OR 'all'
db='2024_ENA' # to be specified only for single hauls
updateID='N' # if Y it updates the amount of fish which has an ID assigned per category
area_sepia='D'
year=2024
area='ITA17' 
changes=read_excel("data/logbook_changes.xlsx", 
           sheet = "Sheet1")

## data for RF ####
dat.haul=read_excel("data/NotesCala_checked.xlsx")
dat.lit=read_excel("data/Foglio Inserimento Litter_2024.xlsx", 
           sheet = "inserimento")
names(dat.lit)[7]='w_g'
dat.lit=dat.lit%>%
  dplyr::group_by(Station=as.character(Station))%>%
  dplyr::summarise(kg_litter=sum(as.numeric(w_g))/1000)

# extra data is used to compute the raising factor
extra.data=readr::read_delim("data/additions_benthos.csv", 
           delim = ";", escape_double = FALSE, trim_ws = TRUE)

haul.info=full_join(dat.haul[,c("Station" ,"WRapA (kg)","Tara A (kg)","WRapD(kg)" ,"Tara D (kg)" ,"WBenthos(kg)" )],
          dat.lit,
          by='Station')
haul.info[,2:6]=apply(haul.info[,2:6],2,as.numeric)
names(haul.info)=c('Station','rapiA_kg', 'rapiA_tara','rapiD_kg', 'rapiD_tara', 'benthos_kg','litter_kg')
haul.info=haul.info%>%replace(is.na(.),0)

lw.mcmc=read.csv("data/post_dist_good_species.csv")

# multi-haul applications ####
hauls.need=get_tables(complete = T)
#xhaul=49

for(xhaul in 1:length(hauls.need)){
  cat(xhaul)
  haul=str_remove( hauls.need[xhaul] ,'cala_')
  
  hauldata=function1(haul=str_remove( hauls.need[xhaul] ,'cala_'), 
                     year=year,
                     complete = T)
  
  #function2(xdat=hauldata, 
  #          haul=str_remove( hauls.need[xhaul] ,'cala_'))
  
  if(str_remove( hauls.need[xhaul] ,'cala_')=='45BIS'){
    # hauldata[[3]]$w=c(0.361,1.082)
    # hauldata[[3]]$n=c(72,155)
    trustdat=function3(xdat=hauldata[[1]], 
                       haul=haul, 
                       year = year, 
                       weight_not_target = hauldata[[2]],  
                       subsamples_target=hauldata[[3]])
    
  }else{
    benthosdata=function_benthos(haul=str_remove( hauls.need[xhaul] ,'cala_'),
                                 complete = T,
                                 year = year)
    
    if(hauls.need[xhaul]=='cala_12'){
      benthosdata[[2]]=benthosdata[[2]][benthosdata[[2]]$species_name!='MUREBRA',]
      benthosdata[[2]]=benthosdata[[2]][benthosdata[[2]]$species_name!='HEXATRU',]
    }
    trustdat=function3_benthos(xdat=hauldata[[1]], 
                               xdat_benthos=benthosdata[[1]],
                               haul=str_remove( hauls.need[xhaul] ,'cala_'),
                               year = year,
                               weight_not_target = hauldata[[2]], 
                               weight_benthos=benthosdata[[2]],
                               info.haul=haul.info[haul.info$Station==str_remove( hauls.need[xhaul] ,'cala_'),],
                               subsamples_target=hauldata[[3]]) 
    
  }
}


# single haul application ####

haul='all' # single haul OR 'all'
# function1 extract data from access db and format them
hauldata=function1(haul=haul, 
                   year=year,
                   complete = T)# extract and format data

hauldata[[3]]$w=c(0.361,1.082)
hauldata[[3]]$n=c(72,155)


benthosdata=function_benthos(haul=haul,
                             complete = T,
                             year = year)


# function 2: perform checks
function2(xdat=hauldata, 
          haul=haul)

trustdat=function3_benthos(xdat=hauldata[[1]], 
                           xdat_benthos=benthosdata[[1]],
                           haul=haul,
                           year = year,
                           weight_not_target = hauldata[[2]], 
                           weight_benthos=benthosdata[[2]],
                           info.haul=haul.info[haul.info$Station==haul,],
                          subsamples_target=hauldata[[3]],
                           catch_sample_disattivati = catch_sample_disattivati)

info.haul=info.haul[info.haul$Station==haul,]



# function 2: perform checks
function2(xdat=hauldata, 
          haul=haul)


# function 3: format data to trust format
trustdat=function3(xdat=hauldata[[1]], 
                  haul=haul, 
                  year = year, 
                  weight_not_target = hauldata[[2]],  
                  subsamples_target=hauldata[[3]],
                  catch_sample_disattivati = catch_sample_disattivati) # function 2

# function4: save PDF
function4(trustdat = trustdat, 
          year=year,
          area = area,
          haul=haul)


## check subsamples ####
xdat_store=NULL
for(xhaul in 1:nrow(haul_summary)){
  
  
  # loop parameters
  haul=haul_summary[xhaul,]$haul
  db=haul_summary[xhaul,]$DB
  area=haul_summary[xhaul,]$country
  
  cat('processing haul no.', haul, '(', xhaul,'/', nrow(haul_summary),')' )
  
  # function1 extract data from access db and format them
  hauldata=function1(haul=haul, 
                     db=db,
                     year=year)# extract and format data
  
  
  xdat=hauldata[[2]]
  xdat=xdat[xdat$species_name%in%shells,]
  xdat$haul=haul
  xdat_store=rbind(xdat_store, xdat)
 
  
}

## full workflow entire dataset ####
updateID='N'
year=2024
activate_funs=data.frame(xfunction=c(1:4),
                         activate=c(1,0,1,0))

haul_summary=read_excel("data/haul_order.xlsx")
#haul_summary[haul_summary$haul=='44',]$valid=-1
haul_summary=haul_summary[haul_summary$valid>=0,]
#haul_summary=haul_summary[haul_summary$DB=='ENA',]
#haul_summary$DB=paste0('2024_', haul_summary$DB)
xhaul=19
for(xhaul in 1:nrow(haul_summary)){
  
  
  # loop parameters
  haul=haul_summary[xhaul,]$haul
  db=haul_summary[xhaul,]$DB
  area=haul_summary[xhaul,]$country
  
  cat('processing haul no.', haul, '(', xhaul,'/', nrow(haul_summary),')' )
  
  # function1 extract data from access db and format them
  hauldata=function1(haul=haul, 
                     db=db,
                     year=year,complete = T)# extract and format data
  
  
  soleLFD=hauldata[[1]]
  soleLFD=soleLFD[soleLFD$species_name=='SOLEVUL' &
            !is.na(soleLFD$fish_ID),]
  
  if(activate_funs[2,]$activate==1){
    # function 2: perform checks
    function2(xdat=hauldata, 
              haul=haul)
    
  }

  
  if(activate_funs[3,]$activate==1){
    # function 3: format data to trust format
    trustdat=function3(xdat=hauldata[[1]], 
                       haul=haul, 
                       year = year, 
                       weight_not_target = hauldata[[2]],  
                       subsamples_target=hauldata[[3]],
                       catch_sample_disattivati = catch_sample_disattivati)
    
  }
  
  if(activate_funs[4,]$activate==1){
    # function4: save PDF
    function4(trustdat = trustdat, 
              year=year,
              area=area,
              haul=haul)
    
  }
}


