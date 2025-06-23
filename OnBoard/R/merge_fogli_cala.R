setwd("C:/Users/a.palermino/OneDrive - CNR/Assegno Scarcella/Solemon/Solemon 2024/OnBoard/data/fogli_cala")
library(dplyr)
library(purrr)
library(readxl)
library(stringr)

station.files=list.files()

i=5
list.hauls=NULL
for(i in 1:length(station.files)){
  haul_id=str_remove(str_remove(station.files[i],'StationTablet_'),'.xlsx')
  xdat=read_excel(station.files[i])
  xdat=xdat[!is.na(xdat$Station),]
  xdat.more=read_excel(station.files[i], 
                       sheet = "NotesCala")
  xdat.more=xdat.more[1,]
  xdat.more$`Tara A (kg)`=ifelse(xdat.more$`Tara A (kg)`=='no', 0,xdat.more$`Tara A (kg)`)
  xdat.more$`Tara D (kg)`=ifelse(xdat.more$`Tara D (kg)`=='no', 0,xdat.more$`Tara D (kg)`)
  xdat.more$`WRapA (kg)`=as.numeric(xdat.more$`WRapA (kg)`)
  xdat.more$`Tara A (kg)`=ifelse(is.na(xdat.more$`Tara A (kg)`), 0,xdat.more$`Tara A (kg)`)
  xdat.more$`Tara D (kg)`=ifelse(is.na(xdat.more$`Tara D (kg)`), 0,xdat.more$`Tara D (kg)`)
  xdat.more$`WRapA (kg)`=as.numeric(xdat.more$`WRapA (kg)`)
  # day, haul, id,note, inizio, fine, valid, DB, country, peso_rapido_A, peso_rapido_D, peso_subcampione_a, peso_subcampione_b
  xdat$id=i
  xdat$valid=1
  xdat=xdat[,c(2,1,23,16,3,17,24)]
  names(xdat)=c('day', 'haul', 'id','note', 'inizio', 'fine', 'valid')
  xdat$DB=NA
  xdat$country='ITA'
  xdat$peso_rapido_A=xdat.more$`WRapA (kg)`-xdat.more$`Tara A (kg)`
  xdat$peso_rapido_D=xdat.more$`WRapD(kg)`-xdat.more$`Tara D (kg)`
  xdat$peso_subcampione_a=xdat.more$`WBenthos(kg)`
  xdat$peso_subcampione_b=NA
  if(!is.na(xdat$fine)){
    xdat$duration=as.numeric(xdat$fine-xdat$inizio)  
  }else{
    xdat$duration=15
  }
  list.hauls=rbind(list.hauls, xdat)
  
  # check data taken onboard
  sample.dat=read_excel(station.files[i], 
             sheet = "Samples onboard")
  if(nrow(sample.dat)>0){
  
   write.csv(sample.dat, file=paste0('C:/Users/a.palermino/OneDrive - CNR/Assegno Scarcella/Solemon/Solemon 2024/OnBoard/data/onboard_measures/haul_',haul_id ,'_onboard_meas.csv'),row.names = F) 
  }
  

}
writexl::write_xlsx(list.hauls, 'C:/Users/a.palermino/OneDrive - CNR/Assegno Scarcella/Solemon/Solemon 2024/OnBoard/data/haul_order.xlsx')






