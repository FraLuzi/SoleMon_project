# Data Handling for Solemon data: from Access to trust format
#
# Last update: 24.11.2022 added line 57, to be uploaded on main code
#
# Notes: this code run on R 4.0.5 32 bit version due to compatibility issue with RODBC package needed to read from .accdb

# load libraries ####
options(tidyverse.quiet = TRUE)
library(magrittr)
library(dplyr)
library(purrr)
library(readxl)
library(ggplot2)
library(RODBC)
library(gridExtra)
library(stringr)
library(grid)
options(dplyr.summarise.inform = FALSE)
'%ni%'=Negate('%in%')
#main_wd=ifelse(Sys.info()[['user']]=="solemon_pc", 'C:/Users/solemon_pc/Desktop/solemon/2022/raccolta_dati',
#               ifelse(Sys.info()[['user']]=="e.armelloni", "C:/Users/e.armelloni/OneDrive/Lavoro/Solemon/github/SoleMon_project/OnBoard", 
#                      ifelse(Sys.info()[['user']]=="Franc", "C:/Users/Franc/OneDrive/Desktop/solemon/2022/raccolta_dati", NA)))

# set parameters ####
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
wd_acces=file.path(main_wd, "access")
setwd(main_wd)
#haul='33'

# load data ####
species_list=read_excel("data/species_list.xlsx")
mat_list=read_excel("data/maturity_stages.xlsx")
lw_pars=read_excel("data/lw_pars.xlsx")
haul_order <- read_excel("data/haul_order.xlsx")
catch_sample_disattivati <- read_excel("data/catch_sample_disattivati.xlsx")
solemonTB=read_excel('data/solemon_TB.xlsx')
solemonTB$species_name=paste0(solemonTB$GENUS, solemonTB$SPECIES)
target_species=read_excel('data/target_species.xlsx')

# format species lists ####
non_target_list=setdiff(species_list$Medits,target_species$species_name)
non_target=species_list[species_list$Medits %in% non_target_list,]
crustaceans=non_target[non_target$Sp_Subcat=="B",]$Medits
shells=target_species[target_species$target==2,]$species_name
target_species=target_species[target_species$target==1,]
species_list=species_list[,c('Species','Medits')]
names(species_list)[2]='species_name'
species_list=left_join(species_list, target_species,by='species_name')%>%
  replace(is.na(.),0)

# Create folders if do not exsist to save outputs ##   ######## modified_AP ######
if(!dir.exists("output")) dir.create(path="output")
if(!dir.exists("output/checks")) dir.create(path="output/checks")
if(!dir.exists("output/pdf")) dir.create(path="output/pdf")
if(!dir.exists("output/raw_excel")) dir.create(path="output/raw_excel")
if(!dir.exists("output/trust")) dir.create(path="output/trust")
if(!dir.exists("output/trust/bio")) dir.create(path="output/trust/bio")
if(!dir.exists("output/trust/catch")) dir.create(path="output/trust/catch")
if(!dir.exists("output/trust/catch_sample")) dir.create(path="output/trust/catch_sample")

# Run
# extract and format data 
function1=function(haul, db, year){
 
  print(paste('Processing haul', haul))
  
  # connect to access db ####
  MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_",db,".accdb")
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  #acctables=sqlTables(channel) # look for tables
  xdat <- sqlQuery(channel,
                   paste0("SELECT * FROM [", paste0('cala_', haul), "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  close(channel)
  rm(channel)
  # format data: empty cells, NAs, paste species #### 
  write.csv(xdat, paste0('output/raw_excel/',haul,'.csv'),row.names = F) # new line
  
  xdat$idfk=seq(1:nrow(xdat))
  deleterow=xdat[xdat$lenght_mm %in% c(NA, 0) & 
       xdat$weight_g %in% c(NA, 0) &
       xdat$total_number %in% c(NA, 0),]$idfk
  
  xdat=xdat[xdat$idfk%ni%deleterow,]
  print(paste('no records =', nrow(xdat), ', deleted', length(deleterow), 'rows'))
  
  xdat=as_tibble(xdat)
  xdat[is.na(xdat$weight_g),'weight_g']=0
  xdat[is.na(xdat$Mat),'Mat']=0
  xdat$total_number=ifelse(is.na(xdat$total_number), 0, xdat$total_number)
  if(length(grep("TAG",names(xdat)))==0){xdat$TAG=0}
  if(length(grep("kg_subsample",names(xdat)))==0){xdat$kg_subsample=0}
  if(length(grep("type_subsample",names(xdat)))==0){xdat$type_subsample=NA}
  # fill fields according to the row above
  for(i in 2:nrow(xdat)){
    # Fill gaps in gear
    if(is.na(xdat[i,]$gear)){xdat[i,]$gear=xdat[i-1,]$gear}
    # Fill gaps in species name
    if(is.na(xdat[i,]$species_name)){xdat[i,]$species_name=xdat[i-1,]$species_name}
    # Fill gaps in Sex
    if(xdat[i,]$species_name=='MELIKER' & is.na(xdat[i,]$Sex)){xdat[i,]$Sex=xdat[i-1,]$Sex}
    if(xdat[i,]$species_name=='SQUIMAN' & is.na(xdat[i,]$Sex)){xdat[i,]$Sex=xdat[i-1,]$Sex}
  }
  xdat=left_join(xdat, species_list, by='species_name') # attach info on target species
  if(nrow(xdat[is.na(xdat$target),])>0){xdat[is.na(xdat$target),]$target=0}# (name and target [Y/N], then check that no target species is NA)
  
  # Genetic and otolits IDs ####
  FishIDs=read_excel("data/fishID.xlsx") # load files where otolit and gen ids are stored
  FishIDs_support=strsplit(as.character(str_replace_all(FishIDs$species, ':', ',')), ',')
  
  # format columns
  xdat$Oth=xdat$oto_id
  xdat$Gen.Samp=xdat$genetic_id
  xdat$fish_ID=as.character(NA)
  
  if(updateID=='Y'){
  # assign FishID
  for(i in 1:nrow(xdat)){
       if(is.na(xdat[i,]$oto_id)==FALSE|is.na(xdat[i,]$genetic_id)==FALSE){
         # look into the FishID file for the correct species
          for(j in 1:length(FishIDs_support)){
            if(xdat[i,]$species_name%in%FishIDs_support[[j]]){
            FishIDs_row=j
            break
            }
           }
        # assign FISHID
        xdat[i,]$fish_ID=paste0(FishIDs[FishIDs_row,]$code, FishIDs[FishIDs_row,]$fishID+1)
        # update FishID file
        FishIDs[FishIDs_row,]$fishID=FishIDs[FishIDs_row,]$fishID+1
        }
      }
    
  # save updated id sheet
  writexl::write_xlsx(FishIDs, "data/fishID.xlsx")}else{
    
    xdat$fish_ID=xdat$oto_id
  }
  
  # sex and maturity ####
  # format data
  xdat$Sex=as.character(xdat$Sex)
  xdat[xdat$target!=1,]$Sex='I'
  xdat[xdat$target!=1,]$Mat=NA
  xdat[xdat$target==1 & xdat$Sex %ni% c('M', 'F', 'I'),]$Sex='N'
  
  # fill crustaceans empty cells for cases when onboard we specify just if females have spermatopores
  xdat$Mat=ifelse(xdat$species_name%in% c('MELIKER', 'PAPELON') &
                    xdat$Sex=='F' &
                    xdat$Mat==0, 1, xdat$Mat) 
  print(paste('Maturity 0 assigned to',unique(xdat[xdat$target & xdat$Mat%ni%c(1,2,3,4,5,6),]$species_name)))
  xdat[xdat$Mat%ni%c(1,2,3,4,5,6),]$Mat=NA
  xdat$Mat=as.character(xdat$Mat)
  
  # format maturity scale
  for(i in 1:nrow(xdat)){
    if(xdat[i,]$species_name %in% mat_list$SPECIES){
      xmat=mat_list[mat_list$SPECIES==xdat[i,]$species_name,]
      xmat=xmat[xmat$SEX==xdat[i,]$Sex,]
      if(nrow(xmat)==1 & xdat[i,]$Mat %in% c(1,2,3,4,5,6)){
        xdat[i,]$Mat=paste(xmat$SCALE, xdat[i,]$Mat, sep='-')
      }
    }
  }
 
  # Fill gaps non target species L-W relationship ####
  ### new section (oct 2024): fill gaps non target species L-W relationship if freq=1 (means single individuals data)
  
  if(nrow(xdat)>0){
    xdat<-xdat%>% 
      dplyr::group_by(gear, species_name) %>%
      dplyr::mutate(Freq = n())

    for(i in 1:nrow(xdat_a)){
      # format length weight data
      if(xdat[i,]$species_name %in% lw_pars$species_name & 
         xdat[i,]$species_name %in% non_target_list){
        # adjust weight
        if(xdat[i,]$weight_g==0|is.na(xdat[i,]$weight_g) & xdat[i,]$Freq==1){
            a=lw_pars[lw_pars$species_name==xdat[i,]$species_name,]$a
            b=lw_pars[lw_pars$species_name==xdat[i,]$species_name,]$b
          }
          if(xdat[i,]$species_name  %in% crustaceans & xdat[i,]$Freq==1){
            xdat[i,]$weight_g=round((a*xdat[i,]$lenght_mm^b))
          }
        if(xdat[i,]$Freq==1){
            xdat[i,]$weight_g=round((a*xdat[i,]$lenght_mm^b)/1000)
          }
        # adjust length
        if(xdat[i,]$lenght_mm==0|is.na(xdat[i,]$lenght_mm)& xdat[i,]$Freq==1){
            a=lw_pars[lw_pars$species_name==xdat[i,]$species_name,]$a
            b=lw_pars[lw_pars$species_name==xdat[i,]$species_name,]$b
          }
          if(xdat[i,]$species_name  %in% crustaceans & xdat[i,]$Freq==1){
            xdat[i,]$lenght_mm=round((xdat[i,]$weight_g/(a))^(1/b))
          }
        if(xdat[i,]$Freq==1){
            xdat[i,]$lenght_mm=round((xdat[i,]$weight_g/(a/1000))^(1/b))
                }
              }
            }
          }
  xdat<-subset(xdat, select=-Freq)
  # end new section ####
  
  # weight data for non target & raising shells ####
  shells_w=xdat[xdat$species_name%in% shells,]
  
  weight_not_target=xdat[xdat$target!=1 & xdat$species_name%ni% shells,]%>%
    dplyr::group_by(gear, species_name)%>%
    dplyr::summarise(w=sum(weight_g)/1000, n=n()) # format weight by species for non target
  
  if(year>=2022){
    if(nrow(shells_w)>0){
      
      shells_raising=shells_w[!is.na(shells_w$type_subsample),c("gear", "species_name",  "kg_subsample", "type_subsample", "kg_haul",'weight_g','total_number')]
      shells_non_raising=shells_w[is.na(shells_w$type_subsample),c("gear", "species_name",  "weight_g",'total_number')]%>%
        dplyr::group_by(gear, species_name)%>%
        dplyr::summarise(w=sum(weight_g)/1000, n=sum(total_number))
      if(nrow(shells_non_raising)>0){
        weight_not_target=rbind(weight_not_target,shells_non_raising)
      }
      
      
      if(nrow(shells_w)>0){print(shells_raising)}
      
      shells_raising$kg_subsample=ifelse(nchar(shells_raising$kg_subsample)<=3, 
             shells_raising$kg_subsample ,
             shells_raising$kg_subsample/1000)
      
      shells_raising$kg_haul=ifelse(nchar(shells_raising$kg_haul)<=3, 
             shells_raising$kg_haul ,
             shells_raising$kg_haul/1000)
      
      #shells_w=shells_w%>%
      #  dplyr::group_by(gear, species_name)%>%
      #  dplyr::summarise(w=sum(weight_g)/1000, n=sum(total_number))
      #
      #shells_raising=left_join(shells_raising, shells_w, 
      #                         by = c("gear", "species_name"))
      shells_raising$w=as.double(shells_raising$weight_g)
      shells_raising$n=as.double(shells_raising$total_number)
      
      if(nrow(shells_raising)>0){
        for(j in 1:nrow(shells_raising)){
          if(shells_raising[j,]$type_subsample=='species'){
            # case 1, presi tutti animali da cala e poi fatto subcampione per ottenere il numero: n va calcolato, poi kg_subsample diventa w_g
            w_tot=shells_raising[j,]$kg_subsample
            n_tot=shells_raising[j,]$kg_subsample*(shells_raising[j,]$n/shells_raising[j,]$w)
            
          }else if(shells_raising[j,]$type_subsample=='haul'){
            # case 2, fatto subcampione da cala e poi processati tutti individui del subcampione: n_tot va calcolato, w_tot va calcolato
            w_tot=shells_raising[j,]$w*(shells_raising[j,]$kg_haul/shells_raising[j,]$kg_subsample) 
            n_tot=w_tot*(shells_raising[j,]$n/shells_raising[j,]$w) # number in subsample
          }else if(shells_raising[j,]$type_subsample=='multispecies'){
            # case 3 multispecies: ALL individuals of multiple species collected from the haul, subsample of multiple species together, splitting of the species, subsample only for estimating total number (proportion done onboard, so we have the total number of the subsample
            w_tot=shells_raising[j,]$kg_haul*(shells_raising[j,]$w/shells_raising[j,]$kg_subsample) 
            n_tot=w_tot*(shells_raising[j,]$n/shells_raising[j,]$w)
            w_tot=w_tot/1000
          }
          shells_raising[j,]$w=round(w_tot, digits=3)
          shells_raising[j,]$n=round(n_tot)
        }
        weight_not_target=rbind(weight_not_target, shells_raising%>%dplyr::select(names(weight_not_target)))
        
      }
    }
  }else{
    shells_w=shells_w%>%
      dplyr::group_by(gear, species_name)%>%
      dplyr::summarise(w=sum(weight_g)/1000, n=sum(total_number))
    weight_not_target=rbind(weight_not_target, shells_w)
  }
  # clean conflict between raising and non raising
  weight_not_target=weight_not_target%>%dplyr::group_by(gear, species_name)%>%
    dplyr::summarise(w=sum(w), n=sum(n))
  
  xdat[xdat$target!=1,]$weight_g=-1
  # remove shells
  xdat=xdat[xdat$species_name%ni% shells, ]
  
  # weight data for sub-samples of target species (AEQUOPE, mullets etc..) ####
  subsamples_target=xdat[xdat$total_number >0|!is.na(xdat$type_subsample),]
  
  if(nrow(subsamples_target)>0){
    
    for(k in 1:nrow(subsamples_target)){
      if(subsamples_target[k,]$total_number %in% c(NA,0)){
        if(subsamples_target[k,]$type_subsample=='species'){
          
          # reconstruct number
          print(paste('subsample done for', subsamples_target[k,]$species_name))
          measured_animals=xdat[xdat$species_name==subsamples_target[k,]$species_name &
                 is.na(xdat$type_subsample),]
          meas_w=sum(measured_animals$weight_g)
          
          if(subsamples_target[k,]$weight_g==-999){subsamples_target[k,]$weight_g=meas_w}
          meas_n=nrow(measured_animals)
          subsamples_target[k,]$total_number=round((subsamples_target[k,]$kg_subsample*meas_n)/meas_w)
          
          if(subsamples_target[k,]$weight_g < subsamples_target[k,]$kg_subsample){
            subsamples_target[k,]$weight_g = subsamples_target[k,]$kg_subsample
          }
          
        }else if(shells_raising[j,]$type_subsample=='haul'){
          
          print('targetspecies subsample type haul: case not yet considered')
          
        }
         
      }
    }
    
    subsamples_target=subsamples_target%>%
      dplyr::group_by(gear, species_name)%>%
      dplyr::summarise(w=sum(weight_g)/1000, n=sum(total_number))
  }
  
  # remove subsamples
  xdat=xdat[xdat$total_number ==0 & is.na(xdat$type_subsample),]
  
  if(nrow(xdat)>0){
    # Fill gaps for target: lw ####
    for(i in 1:nrow(xdat)){
      # assign length to rajas
      if(xdat[i,]$species_name %in% c('RAJAAST', 'RAJACLA', 'RAJAMIR', 'TORPMAR')){xdat[i,]$lenght_mm=as.numeric(strsplit(xdat[i,]$Notes, '/' )[[1]][1])}
      # format length weight data 
      if(xdat[i,]$species_name %in% lw_pars$species_name){
        # adjust weight
        if(xdat[i,]$weight_g==0|is.na(xdat[i,]$weight_g)){
          if(xdat[i,]$species_name%in% c('MELIKER', 'MERLMER')){
            a=lw_pars[lw_pars$species_name==xdat[i,]$species_name & lw_pars$sex==xdat[i,]$Sex,]$a
            b=lw_pars[lw_pars$species_name==xdat[i,]$species_name& lw_pars$sex==xdat[i,]$Sex,]$b
          }else{
            a=lw_pars[lw_pars$species_name==xdat[i,]$species_name,]$a
            b=lw_pars[lw_pars$species_name==xdat[i,]$species_name,]$b
          }
          if(xdat[i,]$species_name=='SQUIMAN'|xdat[i,]$species_name=='MELIKER'|xdat[i,]$species_name=='PAPELON'){
            xdat[i,]$weight_g=round((a*xdat[i,]$lenght_mm^b))
          }
          else{
            xdat[i,]$weight_g=round((a*xdat[i,]$lenght_mm^b)/1000)
          }
        }
        # adjust length
        if(xdat[i,]$lenght_mm==0|is.na(xdat[i,]$lenght_mm)){
          if(xdat[i,]$species_name%in% c('MELIKER', 'MERLMER')){
            a=lw_pars[lw_pars$species_name==xdat[i,]$species_name & lw_pars$sex==xdat[i,]$Sex,]$a
            b=lw_pars[lw_pars$species_name==xdat[i,]$species_name& lw_pars$sex==xdat[i,]$Sex,]$b
          }else{
            a=lw_pars[lw_pars$species_name==xdat[i,]$species_name,]$a
            b=lw_pars[lw_pars$species_name==xdat[i,]$species_name,]$b
          }
          if(xdat[i,]$species_name=='SQUIMAN'|xdat[i,]$species_name=='MELIKER'|xdat[i,]$species_name=='PAPELON'){
            xdat[i,]$lenght_mm=round((xdat[i,]$weight_g/(a))^(1/b))
          }
          else{
            xdat[i,]$lenght_mm=round((xdat[i,]$weight_g/(a/1000))^(1/b))
          }
        }
      }
    }
  }

  
  return(list(xdat, weight_not_target, subsamples_target))
  
} 

# performs checks 
function2=function(xdat, haul){
  xdat=hauldata
  haul=haul
  xxdat=xdat[[1]]
  # lw plot target species with the L-W theorethical function########### ### Modified_AP ###
  p_lw=xxdat[xxdat$target==1,]%>%
    ggplot(aes(x=lenght_mm, y=weight_g, color=gear))+
    geom_point()+stat_smooth(aes(x=lenght_mm, y=weight_g),lty="dashed", geom="line", color="black", se=F, method="nls", method.args = list(formula= y~(a*x^b),start=list(a=0.0001,b=2.5)))+
    facet_wrap(~species_name, scales='free')+
    ggtitle(paste('Target species haul', haul)); p_lw
  
  ggsave(plot=p_lw, filename = file.path(main_wd, 'output', 'checks', paste0(haul, '_targetlw.png') ), 
         width = 30, height = 20, units='cm')
  
  # count non target 
  p_other=xxdat[xxdat$target!=1,]%>%
    ggplot(aes(x=lenght_mm, y=gear, color=gear))+
    geom_point()+
    facet_wrap(~species_name, scales='free')+
    ggtitle(paste('Other species haul', haul))
  
  ggsave(plot=p_other, filename = file.path(main_wd, 'output', 'checks', paste0(haul, '_other.png') ), 
         width = 30, height = 20, units='cm')
  
  # ind weight all species
  soleomn_dat=solemonTB%>%dplyr::group_by(YEAR, species_name, HAUL_NUMBER)%>%
    dplyr::summarise(mean_w=(TOTAL_WEIGHT_IN_THE_HAUL/TOTAL_NUMBER_IN_THE_HAUL)/1000)%>%
    dplyr::mutate(gear='C')
  
  ######## mean weight trend showing rapido A,D and cobined (C) ###### ### Modified_AP ###
  new_dat_2=xdat[[2]]%>%
    dplyr::group_by(species_name, gear)%>%
    dplyr::summarise(mean_w=(w/n))%>%   
    dplyr::summarise(mean_w=mean(mean_w), gear="C")%>%
    dplyr::mutate(YEAR=2023, HAUL_NUMBER=as.character(haul))%>%
    dplyr::select(names(soleomn_dat))
  new_dat_1=xdat[[2]]%>%
    dplyr::group_by(species_name, gear)%>%
    dplyr::summarise(mean_w=(w/n))%>%   
    dplyr::mutate(YEAR=2023, HAUL_NUMBER=as.character(haul))%>%
    dplyr::select(names(soleomn_dat))
  new_dat<-rbind(new_dat_2,new_dat_1)
  
  plot_dat=soleomn_dat[soleomn_dat$HAUL_NUMBER==new_dat$HAUL_NUMBER & soleomn_dat$species_name %in% new_dat$species_name,]%>%
    rbind(new_dat)
  
  pmw=ggplot(data=plot_dat,aes(x=YEAR, y=mean_w,color=gear))+
    geom_point()+
    facet_wrap(~species_name, scales='free')+
    geom_smooth(col=3)+
    ggtitle(paste('Mean weight other species haul', haul))
  
  ggsave(plot=pmw, filename = file.path(main_wd, 'output', 'checks', paste0(haul, '_other_mw.png') ), 
         width = 30, height = 20, units='cm')
  
  # trend number of samples with rapido A,D and cobined (C)#### ### Modified_AP ###
  catch_target=xxdat[xxdat$target==1,]%>%
    dplyr::group_by(gear, species_name)%>%
    dplyr::summarise(n=n())
  
  allspeciesstats_2=rbind(xdat[[2]][,names(catch_target)],catch_target)%>%
    dplyr::group_by(species_name)%>%
    dplyr::summarise(n=sum(n), gear="C")%>%
    dplyr::mutate(YEAR=2023)
  allspeciesstats_1=rbind(xdat[[2]][,names(catch_target)],catch_target)%>%
    dplyr::group_by(species_name)%>%
    dplyr::mutate(YEAR=2023)
  allspeciesstats<-rbind(allspeciesstats_2,allspeciesstats_1)
  
  p_trend=solemonTB%>%
    dplyr::filter(HAUL_NUMBER==haul,
                  species_name%in%allspeciesstats$species_name)%>%
    dplyr::group_by(YEAR, species_name, HAUL_NUMBER)%>%
    dplyr::summarise(n=sum(TOTAL_NUMBER_IN_THE_HAUL))%>%
    dplyr::mutate(gear='C')%>%dplyr::select(names(allspeciesstats))%>%
    rbind(allspeciesstats)%>%
    ggplot(aes(x=YEAR, y=n, color=gear))+
    geom_point()+
    facet_wrap(~species_name, scales='free')+
    scale_x_continuous(breaks = seq(2005,2023,1))+
    theme(axis.text.x=element_text(angle=90))
  
  ggsave(plot=p_trend, filename = file.path(main_wd, 'output', 'checks', paste0(haul, '_trend.png') ), 
         width = 30, height = 20, units='cm')

}
  

# format trust  
function3=function(xdat, haul, year, weight_not_target, subsamples_target, catch_sample_disattivati){
  survey=paste0('SOLEMON', year)
  # Bio data ####
  bio_data_trust=xdat
  bio_data_trust$Survey=survey
  bio_data_trust$Area=area
  bio_data_trust$Station=haul
  bio_data_trust$Gear=ifelse(bio_data_trust$gear=='A', '1-RAP', '2-RAP')
  bio_data_trust$SampN=1
  bio_data_trust$SpecN=1
  bio_data_trust$Age=NA
  bio_data_trust=bio_data_trust[, c('Survey', 'Area', 'Station', 'Gear', 'species_name', 'SampN', 'SpecN', "lenght_mm", "weight_g", "Sex", 'Mat', 'Oth','Age', 'fish_ID', 'Gen.Samp','Notes', 'TAG')]
  names(bio_data_trust)=c('Survey',	'Area',	'Station'	,'Gear',	'SpecCode',	'SampN'	,'SpecN',	'L(mm)'	,'W(g)',	'Sex'	,'MatStage',	'Oth',	'Age'	,'FishID'	,'Gen.Samp.',	'Notes', 'TAG')
  
  ######## added if 0 change with -1 in L and W ######
  bio_data_trust=bio_data_trust%>%
    dplyr::mutate(MatStage=ifelse(nchar(MatStage)<3,NA,MatStage))
  bio_data_trust$'L(mm)'[bio_data_trust$'L(mm)'==0]<--1
  bio_data_trust$'W(g)'[bio_data_trust$'W(g)'==0]<--1
  bio_data_trust$Oth=ifelse(is.na(bio_data_trust$Oth), 0,1)
  if(haul==18 & survey=='SOLEMON2021'){
    bio_data_trust[bio_data_trust$SpecCode=='SOLEVUL'&!is.na(bio_data_trust$FishID),]$FishID=NA
  }
  
  writexl::write_xlsx(bio_data_trust, file.path(main_wd, 'output', 'trust', 'bio', paste0("Bio_Trust_", haul, ".xlsx") ))
  
  # catch sample data ####
  catch_samples=bio_data_trust%>%
    dplyr::distinct(Survey, Area, Station, Gear, SpecCode)%>%
    dplyr::mutate(SampN=1,
                  'W(kg)'=-1,
                  SampMeth='SIMRANDO',
                  InUse='Y',
                  Picts=NA,
                  Notes=NA)
  if(haul %in% catch_sample_disattivati$Station){
    catch_sample_disattivati$Gear=ifelse(catch_sample_disattivati$Gear=='A', '1-RAP', '2-RAP')
    catch_samples[ catch_samples$SpecCode ==catch_sample_disattivati[catch_sample_disattivati$Station==haul,]$SpecCode &
                     catch_samples$Gear== catch_sample_disattivati[catch_sample_disattivati$Station==haul,]$Gear, ]$InUse='N'
  }
  
  writexl::write_xlsx(catch_samples, file.path(main_wd, 'output', 'trust', 'catch_sample', paste0("Catch_sample_Trust_", haul, ".xlsx") ))
  
  # catch data ####
  if(nrow(subsamples_target)>0){
    xdat=xdat[paste0(xdat$species_name, xdat$gear) %ni% paste0(subsamples_target$species_name, subsamples_target$gear), ]
  }
  
  catch_target=xdat[xdat$target==1,]%>%
    dplyr::group_by(gear, species_name)%>%
    dplyr::summarise(w=sum(weight_g)/1000,
                     n=n())
  
  catch_file_trust=rbind(weight_not_target,catch_target)
  
  # cases when target species were subsampled
  if(nrow(subsamples_target)>0){
    catch_file_trust=rbind(catch_file_trust, subsamples_target)
    catch_file_trust=catch_file_trust%>%
      dplyr::group_by(gear, species_name)%>%
      dplyr::summarise(w=sum(w), n=sum(n))
  }
  
  
  catch_file_trust=catch_file_trust%>%
    left_join(species_list[,c('Species', "species_name")],by = "species_name")
  
  catch_file_trust=catch_file_trust[,c("gear", "Species", "species_name", 'w','n' )]
  catch_file_trust$Survey=survey
  catch_file_trust$Area=area
  catch_file_trust$Station=haul
  catch_file_trust=catch_file_trust%>%dplyr::select(Survey, Area, Station, gear, Species,
                                                    species_name,w,n)
  names(catch_file_trust)=c('Survey' , 'Area', 'Station', 'Gear', 'SpeciesSN', 'Code', 'W(kg)', 'Numb')
  catch_file_trust$RaisF=1
  catch_file_trust$Notes=NA
  catch_file_trust$UserIns=NA
  catch_file_trust$Gear=ifelse(catch_file_trust$Gear=='A', '1-RAP', '2-RAP')
  
  writexl::write_xlsx(catch_file_trust, file.path(main_wd, 'output', 'trust', 'catch', paste0("Catch_Trust_", haul, ".xlsx") ))
  
  return(list(bio_data_trust, catch_samples, catch_file_trust))
  
}


# save pdf 
function4=function(trustdat, year, area, haul){
  survey=paste0('SOLEMON', year)
  
  # format data
  options(scipen = 999)
  df <- trustdat[[1]][,c(4,5,8,9,10,11,12,14)]
  df=df[df$SpecCode%ni%shells,]
  df$Numb=NA
  df_catch=trustdat[[3]]
  df_catch$`W(kg)`=df_catch$`W(kg)`*1000
  names(df_catch)[7]="W(g)"
  title=trustdat[[1]][1,3,drop=T]
  
  df=df%>%rbind(df_catch[df_catch$Code%in% shells,]%>%
                  dplyr::mutate(Survey=survey,
                                Area=area,
                                Station=haul,
                                'L(mm)'=NA,
                                Sex=NA,
                                MatStage=NA,
                                Oth=NA,
                                FishID=NA, 
                                TAG=NA)%>%
                  dplyr::rename('SpecCode'='Code')%>%
                  dplyr::select(names(df)))
  
  ###### Split and merge dataframe to fill all the blank PDF page ##### ## Modified_AP  ##
  n<-nrow(df)/2
  n<-round(n,digits=0)
  df_split<-split(df, seq(nrow(df)) %/% n)
  df_1<-df_split[[1]]
  df_1[""]<-""
  df_merged<-merge(data.frame(df_1, row.names = NULL), data.frame(df_split[[2]], row.names = NULL), by = 0, all = TRUE)[-1]
colnames(df_merged)<-c("Gear","SpecCode","L(mm)","W(g)","Sex","MatStage","Oth","FishID","Numb","","Gear","SpecCode","L(mm)","W(g)","Sex","MatStage","Oth","FishID","Numb")
  
  # pdf file characteristics
  mytheme <- gridExtra::ttheme_default(base_size = 4)
  
  tg <- tableGrob(df_merged, rows = seq_len(nrow(df_merged)), theme=mytheme ) 
  fullheight <- convertHeight(sum(tg$heights), "cm", valueOnly = TRUE)
  margin <- unit(0.51,"in")
  margin_cm <- convertHeight(margin, "cm", valueOnly = TRUE)
  a4height <- 29.7 - margin_cm
  nrows <- nrow(tg)
  npages <- ceiling(fullheight / a4height)
  heights <- convertHeight(tg$heights, "cm", valueOnly = TRUE) 
  rows <- cut(cumsum(heights), include.lowest = FALSE,
              breaks = c(0, cumsum(rep(a4height, npages))))
  groups <- split(seq_len(nrows), rows)
  gl <- lapply(groups, function(id) tg[id,])
  main=paste0("Station ",title)
  
  # make pdf ######## Modified_AP ######
  pdf(file.path(main_wd, 'output', 'pdf', paste0(haul, ".pdf") ), paper = "a4", 
      width = 0, height = 0)
  for(page in seq_len(npages)){
    grid.newpage()
    grid.rect(width=unit(21,"cm") - margin,
              height=unit(29.7,"cm")- margin)
    grid.draw(gl[[page]])
    grid.text(main, x=0.5,y=0.99)
  }
  ## alternative to explicit loop:
  ## print(marrangeGrob(grobs=gl, ncol=1, nrow=1, top=NULL))
  dev.off()
  
}
  


## function to load and format minilog data

load_minilog=function(main_wd, minilog, dates){
  
  xdat <- read_excel(file.path(main_wd, 'data', 'minilog', paste0(paste(minilog, dates, sep='_'), '.xlsx')),
                     sheet='DAT')
  names(xdat)=c('date', 'temp', 'depth', 'sal', 'conductivity', 'sound_vel')
  xdat$date=as.POSIXct(xdat$date*86400, 
                       format='%d.%m.%Y %H:%M:%S', 
                       tz="UTC", 
                       origin = '30.12.1899 00:00:01')
  xdat$depth=as.numeric(str_replace(xdat$depth, ',', '.'))
  xdat$temp=as.numeric(str_replace(xdat$temp, ',', '.'))
  xdat$sal=as.numeric(str_replace(xdat$sal, ',', '.'))
  xdat$day=as.POSIXct(substr(xdat$date, 1,10), tz='UTC')
  return(xdat)
  
  
}
  





  