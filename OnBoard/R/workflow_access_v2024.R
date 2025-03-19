
# LAURAS
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
main_wd=ifelse(Sys.info()[['user']]=="solemon_pc", 'C:/Users/solemon_pc/Desktop/solemon/2022/raccolta_dati',
               ifelse(Sys.info()[['user']]=="e.armelloni", "C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Solemon/github/SoleMon_project/OnBoard", 
                      ifelse(Sys.info()[['user']]=="Franc", "C:/Users/Franc/OneDrive/Desktop/solemon/2022/raccolta_dati", NA))) 

#main_wd="C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Solemon/AtSeaData/2023"
setwd(main_wd)
source('R/functions_access_v2024.R')

# inspect lists of target species and shells
unique(target_species$species_name) # these are the species for which you collect individual length AND individual weight
shells  # these are the species for which you collect total weight and total number 
unique(haul_order$haul)
# set parameters
haul='test' # single haul OR 'all'
db='test' # to be specified only for single hauls
updateID='N'
area_sepia='D'
year=2024
area='ITA17' 



# single haul application ####
# function1 extract data from access db and format them
hauldata=function1(haul=haul, 
                   db=db,
                   year=year)# extract and format data

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

# multi-haul applications: need extra file ####

haul_summary=read_excel("data/haul_order.xlsx")
haul_summary=haul_summary[haul_summary$valid>=0,]
#haul_summary$DB='2022_1'


## get oto LFD ####
oto_store=NULL
meli_store=NULL
oto_hist=read.csv('output/otolith_LFD.csv')
#oto_summary=haul_summary[as.character(haul_summary$day)%in%c('2022-12-03'),]
oto_summary=haul_summary
xhaul=1

for(xhaul in 1:nrow(oto_summary)){
  
  
  # loop parameters
  haul=oto_summary[xhaul,]$haul
  db=oto_summary[xhaul,]$DB
  area=oto_summary[xhaul,]$country
  
  cat('processing haul no.', haul, '(', xhaul,'/', nrow(oto_summary),')' )
  
  # function1 extract data from access db and format them
  hauldata=function1(haul=haul, 
                     db=db,
                     year=year)# extract and format data
  
  
  LFD=hauldata[[1]]
  
  soleLFD=LFD[LFD$species_name=='SOLEVUL' &
                    !is.na(LFD$fish_ID),]
  
  names(soleLFD)
  soleLFD=soleLFD[,c("gear", "species_name","lenght_mm" , "weight_g","Sex" ,"Mat")]
  soleLFD$haul=haul
  meliLFD=LFD[LFD$species_name=='MELIKER' &
                !is.na(LFD$fish_ID),]
  
  oto_store=rbind(oto_store, soleLFD)
  meli_store=rbind(meli_store, meliLFD)
  
}

oto_store=oto_store%>%
  dplyr::mutate(len_bin=floor(lenght_mm/10))%>%
  dplyr::group_by(len_bin)%>%
  tally()

oto_store=rbind(oto_hist, oto_store)
#write.csv(oto_store, 'output/otolith_LFD.csv', row.names = F)
pLFD=ggplot(data=oto_store)+
  geom_col(aes(x=len_bin, y=n))+
  scale_x_continuous(breaks = seq(min(oto_store$len_bin), max(oto_store$len_bin),1))+
  scale_y_continuous(breaks = seq(1,20,1))+
  geom_hline(yintercept = 10, color='red')+
  geom_hline(yintercept = 15, color='green');pLFD
ggsave(plot=pLFD, 'output/soleLFD.png', height = 10, width = 12, units='cm')


meli_store=meli_store%>%
  dplyr::mutate(len_bin=floor(lenght_mm))%>%
  dplyr::group_by(len_bin)%>%
  tally()

## get sepia ID ####
sepi_store=NULL
oto_summary=haul_summary

xhaul=1

for(xhaul in 1:nrow(oto_summary)){
  
  
  # loop parameters
  haul=oto_summary[xhaul,]$haul
  db=oto_summary[xhaul,]$DB
  area=oto_summary[xhaul,]$country
  
  cat('processing haul no.', haul, '(', xhaul,'/', nrow(oto_summary),')' )
  
  # function1 extract data from access db and format them
  hauldata=function1(haul=haul, 
                     db=db,
                     year=year)# extract and format data
  
  
  LFD=hauldata[[1]]
  
  sepidat=LFD[LFD$species_name=='SEPIOFF' &
                !is.na(LFD$fish_ID),]
  sepidat$haul=haul
  
  sepi_store=rbind(sepi_store, sepidat)
  print(paste(nrow(sepidat), 'sepia found in haul', haul))
 
  
}

write.csv(sepi_store, 'output/sepiasample.csv', row.names = F)



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
year=2023
activate_funs=data.frame(xfunction=c(1:4),
                         activate=c(1,1,1,0))

haul_summary=read_excel("data/haul_order.xlsx")
#haul_summary[haul_summary$haul=='44',]$valid=-1
haul_summary=haul_summary[haul_summary$valid>=0,]


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


