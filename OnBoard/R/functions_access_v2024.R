# Data Handling for Solemon data: from Access to trust format
#
# Last update: 25.10.2024
#
# This code only works with the access file version 2024. Refer to older versions for the surveys prior to 2024

# pending: implement raising factor for benthos. Now it treat benthos case as case haul (no raising factor)


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
lw_solemon=read_excel("data/lw_SoleMon.xlsx")
haul_order <- read_excel("data/haul_order.xlsx")
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

# Create folders if do not exsist to save outputs ##  
if(!dir.exists("output")) dir.create(path="output")
if(!dir.exists("output/checks")) dir.create(path="output/checks")
if(!dir.exists("output/pdf")) dir.create(path="output/pdf")
if(!dir.exists("output/raw_excel")) dir.create(path="output/raw_excel")
if(!dir.exists("output/trust")) dir.create(path="output/trust")
if(!dir.exists("output/trust/bio")) dir.create(path="output/trust/bio")
if(!dir.exists("output/trust/catch")) dir.create(path="output/trust/catch")
if(!dir.exists("output/trust/catch_sample")) dir.create(path="output/trust/catch_sample")

# Run

#**********************************
# extract and format data
#**********************************
function1=function(haul, db=NA, year, complete=F){
 
  print(paste('Processing haul', haul))
  
  # load from access ####
  if(complete==T){
    MDBPATH <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_complete.accdb")
  }else{
    MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_",db,".accdb") 
  }
  
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  #acctables=sqlTables(channel) # look for tables
  xdat <- sqlQuery(channel,
                   paste0("SELECT * FROM [", paste0('cala_', haul), "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  close(channel)
  rm(channel)
  
  # format data part 1: empty cells, NAs, zeros #### 
  # make a copy of xdat in excel to be investigated if needed
  write.csv(xdat, paste0('output/raw_excel/',haul,'.csv'),row.names = F)
  
  xdat$idfk=seq(1:nrow(xdat))
  deleterow=xdat[xdat$length_mm %in% c(NA, 0) & 
       xdat$weight_g %in% c(NA, 0) &
       is.na(xdat$type_subsample)  &
       xdat$total_number %in% c(NA, 0),]$idfk
  
  xdat=xdat[xdat$idfk%ni%deleterow,]
  print(paste('no records =', nrow(xdat), ', deleted', length(deleterow), 'rows'))
  
  xdat=as_tibble(xdat)
  xdat[is.na(xdat$weight_g),'weight_g']=0
  xdat[is.na(xdat$Mat),'Mat']=0
  xdat$total_number=ifelse(is.na(xdat$total_number), 0, xdat$total_number)
  
  # format data part 2: fill fields according to the row above ####
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
  
  # # Genetic and otolits IDs ####
  # FishIDs=read_excel("data/fishID.xlsx") # load files where otolit and gen ids are stored
  # FishIDs_support=strsplit(as.character(str_replace_all(FishIDs$species, ':', ',')), ',')
  # #xdat$Oth=xdat$id_specimen
  # #xdat$fish_ID=as.character(NA)
  # 
  # if(updateID=='Y'){
  # # assign FishID (deprecated in 2024)
  # for(i in 1:nrow(xdat)){
  #      if(is.na(xdat[i,]$id_specimen)==FALSE){
  #        # look into the FishID file for the correct species
  #         for(j in 1:length(FishIDs_support)){
  #           if(xdat[i,]$species_name%in%FishIDs_support[[j]]){
  #           FishIDs_row=j
  #           break
  #           }
  #          }
  #       # assign FISHID
  #       xdat[i,]$fish_ID=paste0(FishIDs[FishIDs_row,]$code, FishIDs[FishIDs_row,]$fishID+1)
  #       # update FishID file
  #       FishIDs[FishIDs_row,]$fishID=FishIDs[FishIDs_row,]$fishID+1
  #       }
  #     }
  #   
  # # save updated id sheet
  # writexl::write_xlsx(FishIDs, "data/fishID.xlsx")}else{
  #   
  #  # xdat$fish_ID=xdat$id_specimen
  # }
  
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
  
  ## summarise weight data for other commercial species ####
  weight_not_target=xdat[xdat$target!=1 & xdat$species_name%ni% shells,]%>%
    dplyr::group_by(gear, species_name)%>%
    dplyr::summarise(w=sum(weight_g)/1000, n=n()) # format weight by species for non target
  
  ## summarise weight data for shellfishes ####
  shells_w=xdat[xdat$species_name%in% shells,]
    if(nrow(shells_w)>0){
      # identify which needs raising (subsamples)
      shells_raising=shells_w[!is.na(shells_w$type_subsample),]
      shells_non_raising=shells_w[is.na(shells_w$type_subsample),]
      if(nrow(shells_raising)>0){print(shells_raising)}
      
      # add weight of the shells with complete sample to weight of non target
      if(nrow(shells_non_raising)>0){
        shells_non_raising=shells_non_raising %>%
          dplyr::group_by(gear, species_name)%>%
          dplyr::summarise(w=sum(weight_g)/1000, n=sum(total_number))
        weight_not_target=rbind(weight_not_target,shells_non_raising)
      }
      
      ### subsamples of non target ####
      shells_raising$weight_g=as.double(shells_raising$weight_g)
      
      if(nrow(shells_raising)>0){
        for(j in 1:nrow(shells_raising)){
          if(shells_raising[j,]$type_subsample=='C1_species'){
            # case 1, presi tutti animali da cala e poi fatto subcampione per ottenere il numero: 
            # n va calcolato, poi kg_subsample diventa w_g
            w_tot=shells_raising[j,]$kg_field1
            n_tot=shells_raising[j,]$kg_field1*(shells_raising[j,]$subsample_number/shells_raising[j,]$kg_field2)
            
          }else if(shells_raising[j,]$type_subsample%in%c('C2_haul','C3_benthos')){
            # case 2, fatto subcampione da cala e poi processati tutti individui del subcampione: 
            # n_tot va calcolato, w_tot va calcolato
            w_tot=shells_raising[j,]$kg_field2*(shells_raising[j,]$haul_weight/shells_raising[j,]$kg_field1) 
            n_tot=w_tot*(shells_raising[j,]$subsample_number/shells_raising[j,]$kg_field2) # number in subsample
            
          }else if(shells_raising[j,]$type_subsample=='C4_multiple'){
            # case 3 multispecies: ALL individuals of multiple species collected from the haul, 
            # subsample of multiple species together, splitting of the species, 
            # subsample only for estimating total number (proportion done onboard, 
            # so we have the total number of the subsample
            w_tot=shells_raising[j,]$kg_field1*(shells_raising[j,]$kg_field3/shells_raising[j,]$kg_field2) 
            n_tot=w_tot*(shells_raising[j,]$subsample_number/shells_raising[j,]$kg_field3)
            
          }
          shells_raising[j,]$weight_g=round(w_tot, digits=3)
          shells_raising[j,]$total_number=round(n_tot)
        }
        shells_raised=shells_raising%>%
          dplyr::group_by(gear, species_name)%>%
          dplyr::summarise(w=sum(weight_g), n=sum(total_number))
        weight_not_target=rbind(weight_not_target, shells_raised)
        
      }
    }

  # clean conflict between raising and non raising
  weight_not_target=weight_not_target%>%
    dplyr::group_by(gear, species_name)%>%
    dplyr::summarise(w=sum(w), n=sum(n))
  xdat[xdat$target!=1,]$weight_g=-1
  
  # remove shells
  xdat=xdat[xdat$species_name%ni% shells, ]
  
  # Target species ####
  
  ## rajas data ####
  #if(xdat[i,]$species_name %in% c('RAJAAST', 'RAJACLA', 'RAJAMIR', 'TORPMAR')){xdat[i,]$length_mm=as.numeric(strsplit(xdat[i,]$Notes, '/' )[[1]][1])}
  
  ## sub-samples of target species (AEQUOPE, mullets etc..) ####
  subsamples_target=xdat[xdat$total_number >0|!is.na(xdat$type_subsample),]
  if(nrow(subsamples_target)>0){
    
    for(k in 1:nrow(subsamples_target)){
      if(subsamples_target[k,]$total_number %in% c(NA,0)){
        # reconstruct number and weight of the subsample
        measured_animals=xdat[xdat$species_name==subsamples_target[k,]$species_name&
                                xdat$gear==subsamples_target[k,]$gear,]
        meas_grams=sum(measured_animals$weight_g)
        meas_n=nrow(measured_animals)
        
        if(subsamples_target[k,]$type_subsample=='C1_species'){
          # case 1, presi tutti animali da cala e poi fatto subcampione per ottenere il numero: 
          # n va calcolato, poi kg_subsample diventa w_g
          print(paste('subsample case1 done for', subsamples_target[k,]$species_name))
          if(!is.na(subsamples_target[k,]$kg_field2)){
            print(paste('ratio between recorded and measured subsample is:', 
                        (subsamples_target[k,]$kg_field2*1000)/meas_grams))
            meas_grams=subsamples_target[k,]$kg_field2*1000
          }
          # assign number and weight
          subsamples_target[k,]$total_number=round((subsamples_target[k,]$kg_field1*1000)*(meas_n/meas_grams))
          subsamples_target[k,]$weight_g=round(subsamples_target[k,]$kg_field1*1000)
          
        }else if(subsamples_target[k,]$type_subsample%in%c('C2_haul','C3_benthos')){
          # case 2, fatto subcampione da cala e poi processati tutti individui del subcampione: 
          # n_tot va calcolato, w_tot va calcolato
          print(paste('subsample case2 done for', subsamples_target[k,]$species_name))
          if(!is.na(subsamples_target[k,]$kg_field2)&subsamples_target[k,]$kg_field2>0){
            print(paste('ratio between recorded and measured subsample is:', 
                        (subsamples_target[k,]$kg_field2*1000)/meas_grams))
            meas_grams=subsamples_target[k,]$kg_field2*1000
          }
          # assign number and weight
          subsamples_target[k,]$weight_g=round(meas_grams*(subsamples_target[k,]$haul_weight/subsamples_target[k,]$kg_field1))
          subsamples_target[k,]$total_number=round(subsamples_target[k,]$weight_g*(meas_n/meas_grams))
        }
         
      }
    }
    subsamples_target=subsamples_target%>%
      dplyr::group_by(gear, species_name)%>%
      dplyr::summarise(w=sum(weight_g)/1000, n=sum(total_number))
  }
  
  # remove subsamples of target species from the main dataset
  if(haul!='45BIS'){
    xdat=xdat[is.na(xdat$type_subsample),]  
  }
  # remove aggregated data
  aggregated.values=xdat[which(xdat$total_number>0 & is.na(xdat$type_subsample)),]
  if(nrow(aggregated.values)>0){
    xdat=xdat[-which(xdat$total_number>0 & is.na(xdat$type_subsample)),]  
  }
  
  ## Fill gaps for target: lw ####
  # start of bayesian lw
  if(nrow(xdat)>0){
    for(i in 1:nrow(xdat)){
      # format length weight data 
      if(xdat[i,]$species_name %in% unique(lw.mcmc$specie) &
         !xdat[i,]$species_name %in% non_target_list){
        i.lwpars=lw.mcmc[lw.mcmc$specie==xdat[i,]$species_name,]
        # adjust weight
        if(xdat[i,]$weight_g==0|is.na(xdat[i,]$weight_g)){
          
          # this produce 1 draw for each row of posteriors. Our parameters are cm/g
          #lw.preds=exp(as.numeric(mapply(function(mu, sigma) rnorm(n = 1, mean = mu, sd = sigma), 
          #                      (i.lwpars$alpha + i.lwpars$beta * log(xdat[i,]$length_mm/10)), # mu
          #                      i.lwpars$sigma))) # sigma
          xdat[i,]$weight_g=round(as.numeric(quantile(exp(i.lwpars$alpha + i.lwpars$beta * log(xdat[i,]$length_mm/10)), 0.5)))
          xdat[i,]$Notes=paste(xdat[i,]$Notes, 'w inferred')
        }
        # adjust length
        if(xdat[i,]$length_mm==0|is.na(xdat[i,]$length_mm)){
          xdat[i,]$length_mm=round(as.numeric(quantile(exp((log(xdat[i,]$weight_g)-i.lwpars$alpha)/i.lwpars$beta)*10,0.5)))
          xdat[i,]$Notes=paste(xdat[i,]$Notes, 'l inferred')
        }  # end of bayesian lw
      }else{ 
        
        # Start of standard a an b method for species without good lw model results ####
        
        # adjust weight
        if(xdat[i,]$weight_g==0|is.na(xdat[i,]$weight_g)){
          a=lw_solemon[lw_solemon$species_name==xdat[i,]$species_name,]$a
          b=lw_solemon[lw_solemon$species_name==xdat[i,]$species_name,]$b
        
        if(xdat[i,]$species_name  %in% crustaceans){
          xdat[i,]$weight_g=round((a*xdat[i,]$length_mm^b))
        }else{
          xdat[i,]$weight_g=round((a*xdat[i,]$length_mm^b)/1000)
          }
        } 
        # adjust length
        if(xdat[i,]$length_mm==0|is.na(xdat[i,]$length_mm)){
          a=lw_solemon[lw_solemon$species_name==xdat[i,]$species_name,]$a
          b=lw_solemon[lw_solemon$species_name==xdat[i,]$species_name,]$b
        
        if(xdat[i,]$species_name  %in% crustaceans){
          xdat[i,]$length_mm=round((xdat[i,]$weight_g/(a))^(1/b))
        }
        if(!xdat[i,]$species_name  %in% crustaceans){
          xdat[i,]$length_mm=round((xdat[i,]$weight_g/(a/1000))^(1/b))
          } 
        }
      }
    }
  }
  
  return(list(xdat, weight_not_target, subsamples_target))
  
} 

#**********************************
# extract and format data benthos
#**********************************
function_benthos=function(haul, db=NA, year, complete=F){
  # to be done
  print(paste('Processing haul', haul))
  
  # load from access ####
  # load from access ####
  if(complete==T){
    MDBPATH <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_BENTHOS_complete.accdb")
  }else{
    MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_",db,".accdb") 
  }
  
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  #acctables=sqlTables(channel) # look for tables
  xdat <- sqlQuery(channel,
                   paste0("SELECT * FROM [", paste0('cala_', haul), "] ORDER BY [ID]"),
                   stringsAsFactors = FALSE) # Load data into R dataframe
  close(channel)
  rm(channel)
  
  # format data art 1: empty cells, NAs, zeros #### 
  # if(length(unique(xdat$kg_field2))>1){
  #   xdat$weight_g=xdat$kg_field2*1000
  #   xdat$kg_field2=0
  #   print(paste('haul',haul, 'had data in kg field 2. Check'))
  # }
  # if(sum(xdat$total_number)==0){
  # xdat$total_number=xdat$subsample_number  
  # }
  
  #write.csv(xdat, paste0('output/raw_excel/',haul,'.csv'),row.names = F) # new line
  xdat$idfk=seq(1:nrow(xdat))
  deleterow=xdat[xdat$length_mm %in% c(NA, 0) & 
                   xdat$weight_g %in% c(NA, 0) &
                   is.na(xdat$type_subsample)  &
                   xdat$total_number %in% c(NA, 0)&
                   xdat$subsample_number %in% c(NA, 0),]$idfk
  
  xdat=xdat[xdat$idfk%ni%deleterow,]
  print(paste('no records =', nrow(xdat), ', deleted', length(deleterow), 'rows'))
  
  xdat=as_tibble(xdat)
  xdat[is.na(xdat$weight_g),'weight_g']=0
  xdat[is.na(xdat$Mat),'Mat']=0
  xdat$total_number=ifelse(is.na(xdat$total_number), 0, xdat$total_number)
  
  # format data part 2: fill fields according to the row above ####
  for(i in 2:nrow(xdat)){
    # Fill gaps in gear
    if(is.na(xdat[i,]$gear)){xdat[i,]$gear=xdat[i-1,]$gear}
    # Fill gaps in species name
    if(is.na(xdat[i,]$species_name)){xdat[i,]$species_name=xdat[i-1,]$species_name}
  }
  xdat=left_join(xdat, species_list, by='species_name') # attach info on target species
  if(nrow(xdat[is.na(xdat$target),])>0){xdat[is.na(xdat$target),]$target=0}# (name and target [Y/N], then check that no target species is NA)
  
  # sex and maturity ####
  # format data
  xdat$Sex=as.character(xdat$Sex)
  xdat[xdat$target==1 & xdat$Sex %ni% c('M', 'F', 'I'),]$Sex='N'
  
  # fill crustaceans empty cells for cases when onboard we specify just if females have spermatopores
  xdat$Mat=ifelse(xdat$species_name%in% c('LIOCDEP', 'PAPELON') &
                    xdat$Sex=='F' &
                    xdat$Mat==0, 1, xdat$Mat) 
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
  
  # look for subsamples of non target species (case of ANADARA, OPHIURE ecc...)
  subs.nt=xdat[which(xdat$target==0 & xdat$kg_field3>0 & xdat$kg_field1==0),]
  if(nrow(subs.nt)>0){
    for(j in 1:nrow(subs.nt)){
      w_tot=subs.nt[j,]$weight_g
      subs.nt[j,]$total_number=round(subs.nt[j,]$weight_g*(subs.nt[j,]$subsample_number/(subs.nt[j,]$kg_field3*1000)))
    }
    subs.nt=subs.nt%>%
      dplyr::group_by(gear, species_name)%>%
      dplyr::summarise(w=sum(weight_g)/1000, n=sum(total_number))
  }
  
  ## look for subsamples of non target species (Case of TURRCOM e APORPES)
  subs.tur=xdat[which(xdat$target==0 & xdat$kg_field3>0 & xdat$kg_field1>0),]
  if(nrow(subs.tur)>0){
    for(j in 1:nrow(subs.tur)){
      w_tot=subs.tur[j,]$weight_g
      subs.tur[j,]$weight_g=round(subs.tur[j,]$kg_field3*(subs.tur[j,]$kg_field1/subs.tur[j,]$kg_field2)*1000)
      subs.tur[j,]$total_number=round(subs.tur[j,]$weight_g*(subs.tur[j,]$subsample_number/(subs.tur[j,]$kg_field3*1000)))
    }
    subs.tur=subs.tur%>%
      dplyr::group_by(gear, species_name)%>%
      dplyr::summarise(w=sum(weight_g)/1000, n=sum(total_number))
    subs.tur$n=ifelse(subs.tur$species_name=="SHELLS",0,subs.tur$n)
    subs.nt=rbind(subs.nt,subs.tur)
    subs.nt<-subs.nt[subs.nt$species_name!="SHELLS",]
  }
  
  ## summarise weight data for other commercial species ####
  weight_not_target=xdat[xdat$target!=1 & !xdat$species_name%in% subs.nt$species_name,]%>%
    dplyr::group_by(gear, species_name)%>%
    dplyr::summarise(w=sum(weight_g)/1000, n=sum(total_number)) # format weight by species for non target
  
  if(nrow(subs.nt)>0){
    weight_not_target=rbind(weight_not_target, subs.nt)
  }
  
  ## summarise weight data for liocdep ####
  lioc_data=xdat[xdat$species_name=='LIOCDEP',]
  if(nrow(lioc_data)==1){
    lioc_w=lioc_data%>%dplyr::group_by(gear, species_name)%>%
      dplyr::summarise(w=sum(weight_g)/1000, n=1)
    weight_not_target=rbind(weight_not_target, lioc_w)
    
  }else if(nrow(lioc_data)>1){
    lioc_w=lioc_data[lioc_data$weight_g>0,]
    lioc_w=lioc_w%>%dplyr::group_by(gear, species_name)%>%
      dplyr::summarise(w=sum(weight_g)/1000, n=sum(total_number))
    lioc_bio=lioc_data[lioc_data$weight_g==0,]
    weight_not_target=rbind(weight_not_target, lioc_w)
    xdat=xdat[-which(xdat$species_name=='LIOCDEP'&xdat$weight_g>0),]
  }
  
  # clean conflict between raising and non raising
  weight_not_target=weight_not_target%>%
    dplyr::group_by(gear, species_name)%>%
    dplyr::summarise(w=sum(w), n=sum(n))
  xdat[xdat$target!=1,]$weight_g=-1
  
  # target species ####
  ## sub-samples of target species (AEQUOPE, mullets etc..) ####
  data_target=xdat[xdat$target==1&xdat$species_name!='LIOCDEP',]
  if(nrow(data_target)>0){
    weight_target=data_target%>%
      dplyr::group_by(gear, species_name)%>%
      dplyr::summarise(w=sum(weight_g)/1000, n=n())
    weight_not_target=rbind(weight_not_target, weight_target)
  }
  
  # remove subsamples of target species from the main dataset
  xdat=xdat[is.na(xdat$type_subsample),]
  
  subsamples_target=NA
  #xdat=xdat[xdat$target==1,]
  return(list(xdat, weight_not_target, subsamples_target))
} 


get_tables=function(db=NA, complete=F){
  if(complete==T){
    MDBPATH <- paste0(wd_acces,"/bio_data_v2024_SOLEMON_complete.accdb")
  }else{
    MDBPATH <- paste0(wd_acces,"/Maschera inserimento SOLEMON_",db,".accdb") 
  }
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
  channel <- odbcDriverConnect(PATH)
  acctables=sqlTables(channel) # look for tables
  close(channel)
  tables_check=acctables[grep('ala' ,acctables$TABLE_NAME),]$TABLE_NAME
  tables_check=tables_check[-grep('template', tables_check)]
  tables_check=tables_check[-grep('test', tables_check)]
  return(tables_check)
}


#**********************************
# performs checks ####
#**********************************
function2=function(xdat, haul, year=2024){
  
  xxdat=xdat[[1]]
  
  # compute lower and higher ci (0.001,0.999) of theoretical weight based on bayesian lw
  xdat_target<-xxdat[xxdat$target==1,]
  xdat_target$lo.ci<-NA
  xdat_target$hi.ci<-NA
  species_t<-unique(xdat_target$species_name)
  xdat_target_2<-NULL
  for(i in 1:length(species_t)){
    xxdat_target<-xdat_target%>%filter(species_name==species_t[i])
    posterior_dist<-lw.mcmc%>%filter(specie==species_t[i])
    for(j in 1:nrow(xxdat_target)){
      store=exp(as.numeric(mapply(function(mu, sigma) rnorm(n = 1, mean = mu, sd = sigma), 
                                  (posterior_dist$alpha + posterior_dist$beta * 
                                     log(xxdat_target[j,]$length_mm/10)),posterior_dist$sigma)))
      ints=as.numeric(quantile((store), probs = c(0.001,0.999)))
      xxdat_target[j,]$lo.ci=ints[1]
      xxdat_target[j,]$hi.ci=ints[2]
    }
    
    xdat_target_2<-rbind(xdat_target_2,xxdat_target)
  }

  # lw plot target species with the L-W theorethical function########### 
  p_lw<-ggplot(data=xdat_target_2)+
    geom_point(aes(x=length_mm, y=weight_g, color=gear))+
    geom_ribbon(aes(x=length_mm,ymin=lo.ci, ymax=hi.ci), alpha=0.2)+
    facet_wrap(~species_name, scales='free')+ggtitle(paste('Target species haul', haul))
  p_lw
 
 ggsave(plot=p_lw, filename = file.path(main_wd, 'output', 'checks', paste0(haul, '_targetlw.png') ), 
        width = 30, height = 20, units='cm')
  
  # count non target 
  p_other=xxdat[xxdat$target!=1,]%>%
    ggplot(aes(x=length_mm, y=gear, color=gear))+
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
    dplyr::mutate(YEAR=year, HAUL_NUMBER=as.character(haul))%>%
    dplyr::select(names(soleomn_dat))
  new_dat_1=xdat[[2]]%>%
    dplyr::group_by(species_name, gear)%>%
    dplyr::summarise(mean_w=(w/n))%>%   
    dplyr::mutate(YEAR=year, HAUL_NUMBER=as.character(haul))%>%
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
    dplyr::mutate(YEAR=year)
  allspeciesstats_1=rbind(xdat[[2]][,names(catch_target)],catch_target)%>%
    dplyr::group_by(species_name)%>%
    dplyr::mutate(YEAR=year)
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
    scale_x_continuous(breaks = seq(2005,year,1))+
    theme(axis.text.x=element_text(angle=90))
  
  ggsave(plot=p_trend, filename = file.path(main_wd, 'output', 'checks', paste0(haul, '_trend.png') ), 
         width = 30, height = 20, units='cm')

}
  
#**********************************
# format trust
#**********************************
function3=function(xdat, haul, year, weight_not_target, subsamples_target){
  survey=paste0('SOLEMON', year)
  # Bio data ####
  bio_data_trust=xdat
  bio_data_trust$Survey=survey
  bio_data_trust$Station=haul
  bio_data_trust$Station=ifelse(bio_data_trust$Station=="45BIS",as.character("45bis"),bio_data_trust$Station)
  bio_data_trust$Area=ifelse(bio_data_trust$Station %in% c("45","45bis"),"SLO",area)
  bio_data_trust$Gear=ifelse(bio_data_trust$gear=='A', '1-RAP', '2-RAP')
  bio_data_trust$SampN=1
  bio_data_trust$SpecN=1
  bio_data_trust$Age=NA
  bio_data_trust$TAG=NA
  bio_data_trust$Gen.Samp=NA
  bio_data_trust$Oth=NA
  bio_data_trust=bio_data_trust[, c('Survey', 'Area', 'Station', 'Gear', 'species_name', 'SampN', 'SpecN', "length_mm", "weight_g", "Sex", 'Mat', 'Oth','Age', 'id_specimen', 'Gen.Samp','Notes', 'TAG')]
  names(bio_data_trust)=c('Survey',	'Area',	'Station'	,'Gear',	'SpecCode',	'SampN'	,'SpecN',	'L(mm)'	,'W(g)',	'Sex'	,'MatStage',	'Oth',	'Age'	,'FishID'	,'Gen.Samp.',	'Notes', 'TAG')
  
  ######## added if 0 change with -1 in L and W ######
  bio_data_trust=bio_data_trust%>%
    dplyr::mutate(MatStage=ifelse(nchar(MatStage)<3,NA,MatStage))
  bio_data_trust$'L(mm)'[bio_data_trust$'L(mm)'==0]<--1
  bio_data_trust$'W(g)'[bio_data_trust$'W(g)'==0]<--1
  bio_data_trust$Oth=ifelse(is.na(bio_data_trust$FishID), 0,2)
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
  catch_file_trust$Station=haul
  catch_file_trust$Station=ifelse(catch_file_trust$Station=="45BIS",as.character("45bis"),
                                  catch_file_trust$Station)
  catch_file_trust$Area=ifelse(catch_file_trust$Station%in% c("45","45bis"),"SLO",area)
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

#**********************************
# format trust benthos
#**********************************
function3_benthos=function(xdat, xdat_benthos,
                           haul, year, 
                           weight_not_target, weight_benthos,info.haul,
                           subsamples_target,
                           sampling_type='haul'){
  
  survey=paste0('SOLEMON', year)
  # Bio data ####
  xdat_benthos$id_specimen=as.character(xdat_benthos$id_specimen)
  xdat_benthos$type_subsample=as.character(xdat_benthos$type_subsample)
  xdat_benthos$Notes=as.character(xdat_benthos$Notes)
  
  bio_data_trust=rbind(xdat, xdat_benthos[xdat_benthos$length_mm>0,]) # add individual data included in the benthos data
  bio_data_trust$Survey=survey
  bio_data_trust$Station=haul
  bio_data_trust$Station=ifelse(bio_data_trust$Station=="45BIS",as.character("45bis"),bio_data_trust$Station)
  bio_data_trust$Area=ifelse(bio_data_trust$Station %in% c("45","45bis"),"SLO",area)
  bio_data_trust$Gear=ifelse(bio_data_trust$gear=='A', '1-RAP', '2-RAP')
  bio_data_trust$SampN=1
  bio_data_trust$SpecN=1
  bio_data_trust$Age=NA
  bio_data_trust$TAG=NA
  bio_data_trust$Gen.Samp=NA
  bio_data_trust$Oth=NA
  bio_data_trust=bio_data_trust[, c('Survey', 'Area', 'Station', 'Gear', 'species_name', 'SampN', 'SpecN', "length_mm", "weight_g", "Sex", 'Mat', 'Oth','Age', 'id_specimen', 'Gen.Samp','Notes', 'TAG')]
  names(bio_data_trust)=c('Survey',	'Area',	'Station'	,'Gear',	'SpecCode',	'SampN'	,'SpecN',	'L(mm)'	,'W(g)',	'Sex'	,'MatStage',	'Oth',	'Age'	,'FishID'	,'Gen.Samp.',	'Notes', 'TAG')
  
  ######## added if 0 change with -1 in L and W ######
  bio_data_trust=bio_data_trust%>%
    dplyr::mutate(MatStage=ifelse(nchar(MatStage)<3,NA,MatStage))
  bio_data_trust$'L(mm)'[bio_data_trust$'L(mm)'==0]<--1
  bio_data_trust$'W(g)'[bio_data_trust$'W(g)'==0]<--1
  bio_data_trust$'W(g)'=ifelse(is.na(bio_data_trust$'W(g)'), -1,bio_data_trust$'W(g)')
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
  
  ## handle raising factor
  if(paste0('cala_',haul)%in%extra.data$haul){
    extra.items=extra.data[extra.data$haul==paste0('cala_',haul),]
    extra.items=extra.items[!is.na(extra.items$weight_g),]
    extra.items$weight_g=ifelse(extra.items$weight_g==0 & extra.items$kg_field2>0,extra.items$kg_field2*1000,extra.items$weight_g)
    extra_w=sum(extra.items$weight_g)/1000 #### this ain't no good now. Does not count for MUREBRA
  }else{
    extra_w=0
  }
  benthos_kg=sum(weight_benthos$w)
  if(sampling_type=='haul'){
    # case benthos: the sample is not representative of the haul (fish and litter were already cleaned). This was the method used since 2005
  RF=((info.haul$rapiA_kg - info.haul$rapiA_tara) + (info.haul$rapiD_kg - info.haul$rapiD_tara) - (sum(catch_file_trust$w)-extra_w) -(info.haul$litter_kg))/info.haul$benthos_kg # standard
  RF.d=(info.haul$rapiD_kg - info.haul$rapiD_tara - (sum(catch_file_trust[catch_file_trust$gear=='D',]$w)-extra_w) -info.haul$litter_kg)/info.haul$benthos_kg   
  
  }else{
    # case haul: the sample is representative of the haul
  RF=((info.haul$rapiA_kg - info.haul$rapiA_tara) + (info.haul$rapiD_kg - info.haul$rapiD_tara))/info.haul$benthos_kg # standard
  RF.d=(info.haul$rapiD_kg - info.haul$rapiD_tara)/info.haul$benthos_kg 
  }
  
  if(any(weight_benthos$species_name %in% c(target_species$species_name, shells))){
    
    if(any(catch_file_trust$species_name%in% weight_benthos$species_name)){
      cat('found case benthos to be handled')
      #break
      }
  }
  
  catch_file_trust=rbind(catch_file_trust, weight_benthos)
  ###
  
  catch_file_trust=catch_file_trust%>%
    left_join(species_list[,c('Species', "species_name")],by = "species_name")
  
  catch_file_trust=catch_file_trust[,c("gear", "Species", "species_name", 'w','n' )]
  catch_file_trust$Survey=survey
  catch_file_trust$Station=haul
  catch_file_trust$n=ifelse(catch_file_trust$n==0,-1,catch_file_trust$n)
  catch_file_trust$Station=ifelse(catch_file_trust$Station=="45BIS",as.character("45bis"),
                                  catch_file_trust$Station)
  catch_file_trust$Area=ifelse(catch_file_trust$Station%in% c("45","45bis"),"SLO",area)
  catch_file_trust=catch_file_trust%>%dplyr::select(Survey, Area, Station, gear, Species,
                                                    species_name,w,n)
  catch_file_trust$RaisF=1
  catch_file_trust[(nrow(catch_file_trust)-nrow(weight_benthos)+1):nrow(catch_file_trust),]$RaisF=RF
  
      # handle benthos
  if(haul %in% changes$haul){
    x.change.id=changes[changes$haul==haul,]$case_id
    
    #if(x.change.id==0){
    #  catch_file_trust[catch_file_trust$gear=='D' & catch_file_trust$species_name%in% weight_not_target$species_name & catch_file_trust$RaisF>1 ,]$RaisF=1
    #  catch_file_trust=catch_file_trust%>%
    #    dplyr::group_by(Survey, Area, Station,gear,Species,species_name)%>%
    #    dplyr::summarise(w=sum(w),n=sum(n),RaisF=mean(RaisF))
    #}
    
    if(x.change.id==1){
      # case 1: some individuals are in benthos, but their weight shall not be raised because they were also sampled in commercial
      catch_file_trust[catch_file_trust$gear=='D' & catch_file_trust$species_name%in% weight_not_target$species_name & catch_file_trust$RaisF>1 ,]$RaisF=1
      catch_file_trust=catch_file_trust%>%
        dplyr::group_by(Survey, Area, Station,gear,Species,species_name)%>%
        dplyr::summarise(w=sum(w),n=sum(n),RaisF=mean(RaisF))
    }
    
    if(x.change.id==2){
      # case 2: for some species (usually MUREBRA) the gear A was normally sampled and the gear B is to be estimated from benthos 
      catch_file_trust[catch_file_trust$gear=='D' & 
                            catch_file_trust$species_name %in% catch_file_trust[catch_file_trust$gear=='A',]$species_name&
                            catch_file_trust$RaisF>1,]$RaisF=RF.d
    }
  }
  
  names(catch_file_trust)=c('Survey' , 'Area', 'Station', 'Gear', 'SpeciesSN', 'Code', 'W(kg)', 'Numb', 'RF')
  catch_file_trust$Notes=NA
  catch_file_trust$UserIns=NA
  catch_file_trust$Gear=ifelse(catch_file_trust$Gear=='A', '1-RAP', '2-RAP')
  catch_file_trust=catch_file_trust%>%arrange(Gear, RF, SpeciesSN)
  
  # save
  
  writexl::write_xlsx(catch_file_trust, file.path(main_wd, 'output', 'trust', 'catch', paste0("Catch_Trust_", haul, ".xlsx") ))
  
  return(list(bio_data_trust, catch_samples, catch_file_trust))
  
}

#**********************************
# save pdf 
#**********************************
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
  
  # make pdf ######## Modified_AP #####
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
  