rm(list = ls())
library(tidyverse)
library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)

target.dir="C:/Users/a.palermino/OneDrive - CNR/github/SoleMon_project/OnBoard"

### catch sample data ####
'%ni%'=Negate('%in%')
setwd(file.path(target.dir, "output/trust/catch_sample"))
target.list=read_excel("C:/Users/a.palermino/OneDrive - CNR/github/SoleMon_project/OnBoard/data/target_species.xlsx")
xfiles=list.files()
dat_store=NA
for(k in 1:length(xfiles)){
  dat_store=rbind(dat_store,read_excel(xfiles[k]) )
}
dat_store=dat_store[!is.na(dat_store$Survey),]%>%filter(Station!="test|45BIS")
writexl::write_xlsx(dat_store, "CatchSample_data.xlsx")


# Catch file ####
setwd(file.path(target.dir, "output/trust/catch"))

xfiles=list.files()
dat_store=NA
for(k in 1:length(xfiles)){
  k.dat=read_excel(xfiles[k])
  #names(dat_store)
  names(k.dat)[9]='RaisF'
  dat_store=rbind(dat_store,k.dat )
  #names(dat_store)[9]='RaisF'
}

dat_store=dat_store[!is.na(dat_store$Survey),]%>%filter(Station!="test")
writexl::write_xlsx(dat_store, "Catch_data.xlsx")

names(dat_store)[7]='w_kg'

# check gear
unique(dat_store$Gear)


# missing data
dat_store[is.na(dat_store$RaisF),] ## is RF complete?

miss.n=dat_store[dat_store$Numb==0,]%>%
  arrange(SpeciesSN)## this should only contain those records that does not need a number (biological discard, shells, etc..)
writexl::write_xlsx(miss.n, "../../edits/miss_n.xlsx")

miss.weight=dat_store[dat_store$w_kg==0,] # missing weights: there should be no record. If you find any, these are all mistakes
writexl::write_xlsx(miss.weight, "../../edits/miss_w_r2.xlsx")

wrong.names=dat_store[is.na(dat_store$SpeciesSN),] # this identifies wrong species code
writexl::write_xlsx(wrong.names, "../../edits/wrong_names.xlsx")


# check mean weight
mean_w=dat_store%>%
  dplyr::group_by(Station, Gear, SpeciesSN)%>%
  dplyr::summarise(mean_w=w_kg/Numb)


mean_w%>%
dplyr::group_by(Station, SpeciesSN)%>%
  dplyr::filter(SpeciesSN %in% c('Hexaplex trunculus', "Bolinus brandaris"), !is.na(var))%>%
  arrange(desc(mean_w))

suspicious=mean_w%>%
  dplyr::group_by(SpeciesSN, Station)%>%
  dplyr::summarise(var=max(mean_w)/min(mean_w))%>%
  arrange(desc(var))%>%
  dplyr::filter(var>1.5)


suspicious_all=left_join(suspicious, mean_w, by=c('Station', 'SpeciesSN'))

error.0=suspicious_all[suspicious_all$mean_w==0,]

ggplot(data=suspicious_all)+
  geom_point(aes(x=Station, y=mean_w, color= Gear))+
  facet_wrap(~SpeciesSN, scales='free')

mean_w%>%
  dplyr::filter(SpeciesSN %in% c('Hexaplex trunculus', "Bolinus brandaris"))%>%
  ggplot()+
  geom_point(aes(x=Station, y=mean_w, color= Gear), size=3)+
  facet_wrap(~SpeciesSN, scales='free')

#ggsave("../../checks/ind_w_murici_sospetto.png", width = 50, height = 40, units='cm')

# check number
mean_n=dat_store%>%
  dplyr::group_by(Station, Gear, Code)%>%
  dplyr::summarise(number=sum(Numb))

target.list=target.list[-which(target.list$species_name %in% c('MYTGALL','MUREBRA','HEXATRU','NATISTE','LIOCDEP')),]

n.check=mean_n[mean_n$Code %in% target.list$species_name,]%>%
  pivot_wider(names_from = Gear, values_from = number)%>%
  replace(is.na(.),0)%>%
  dplyr::mutate(test= `1-RAP`/`2-RAP`, tot=`1-RAP`+`2-RAP`)%>%
  arrange(test)%>%
  dplyr::filter(tot>5)%>%
  arrange(test)
n.check=n.check[n.check$test>1.5|n.check$test<0.5,]

writexl::write_xlsx(n.check, "../../edits/suspicious_n.xlsx")

n.check%>%
  pivot_longer(-c(Station, Code, test,tot))%>%
  ggplot()+
  geom_point(aes(x=Station, y=value, color= name))+
  facet_wrap(~Code, scales='free')


ggplot(data=mean_n[mean_n$Code %in% target.list$species_name,])+
  geom_point(aes(x=Station, y=number, color= Gear))+
  facet_wrap(~Code, scales='free')




### Bio data ####
rm(list = ls())
'%ni%'=Negate('%in%')
setwd("C:/Users/a.palermino/OneDrive - CNR/github/SoleMon_project/OnBoard/output/trust/bio")
target.dir="C:/Users/a.palermino/OneDrive - CNR/github/SoleMon_project/OnBoard"

xfiles=list.files()
dat_store=NA
for(k in 1:length(xfiles)){
  dat_store=rbind(dat_store,read_excel(xfiles[k]))
}
dat_store=dat_store[!is.na(dat_store$Survey),]%>%filter(Station!="test")
writexl::write_xlsx(dat_store, "Bio_data.xlsx")

names(dat_store)[9]='w_g'
names(dat_store)[8]='l_mm'
dat_store$w_g=as.numeric(dat_store$w_g)
dat_store$l_mm=as.numeric(dat_store$l_mm)

species_priority=data.frame(table(dat_store$SpecCode, dat_store$w_g))%>%
  dplyr::filter(as.numeric(Var2)>1)%>%
  dplyr::filter( Freq>1)


# LFD non priority
lfd.np=dat_store%>%
  dplyr::filter(SpecCode %ni% species_priority$Var1)%>%
  dplyr::mutate(l_st=floor(l_mm)/10)%>%
  dplyr::group_by(SpecCode)%>%
  add_tally(name='nrecord')%>%
  dplyr::filter(nrecord>1)%>%
  dplyr::group_by(l_st, SpecCode)%>%
  dplyr::tally()

lfd.np%>%ggplot()+
  geom_col(aes(x=l_st, y=n))+
  facet_wrap(~SpecCode, scales='free')

# check suspicious ### adjust better the code
dat_ARNOLAT<-dat_store[dat_store$SpecCode=='ARNOLAT'&dat_store$l_mm<70,] # this needs to be adapted to check species that are suspicious in the previous plot
dat_GOBINIG<-dat_store[dat_store$SpecCode=='GOBINIG'&dat_store$l_mm<50,] # this needs to be adapted to check species that are suspicious in the previous plot
dat_BUGGLUT<-dat_store[dat_store$SpecCode=='BUGLLUT'&dat_store$l_mm>140,] # this needs to be adapted to check species that are suspicious in the previous plot
dat_SERAHEP<-dat_store[dat_store$SpecCode=='SERAHEP'&dat_store$l_mm<60,] # this needs to be adapted to check species that are suspicious in the previous plot
dat_DIPLANN<-dat_store[dat_store$SpecCode=='DIPLANN'&dat_store$l_mm>180,] # this needs to be adapted to check species that are suspicious in the previous plot
dat_SEPIELE<-dat_store[dat_store$SpecCode=='SEPIELE'&dat_store$l_mm<20,] # this needs to be adapted to check species that are suspicious in the previous plot
dat_MELIKER<-dat_store[dat_store$SpecCode=='MELIKER',] # this needs to be adapted to check species that are suspicious in the previous plot
dat_n_non_target<-rbind(dat_ARNOLAT,dat_GOBINIG,dat_BUGGLUT,dat_SERAHEP,dat_DIPLANN,dat_SEPIELE)%>%
  filter(Survey!=is.na(Survey))

writexl::write_xlsx(dat_n_non_target, "../../edits/suspicious_n_nontarget.xlsx")

# lw priority
ggplot(data=dat_store%>%
         dplyr::filter(SpecCode %in% species_priority$Var1))+
  geom_point(aes(x=l_mm, y=w_g, color=Notes))+
  facet_wrap(~SpecCode, scales='free')

# bayesian LW
lw.mcmc=read_csv("C:/Users/a.palermino/OneDrive - CNR/Assegno Scarcella/Solemon/Solemon 2024/OnBoard/data/post_dist_all_species.csv")


target.species='CALLSAP' # select species

posterior_dist=lw.mcmc[lw.mcmc$specie==target.species,]
dat.species=dat_store[dat_store$SpecCode==target.species,]%>%filter(Survey!=is.na(Survey))
dat.species$lo.ci=dat.species$hi.ci=NA
summary(dat.species)

ch=dat.species[(dat.species$w_g)>100,]

pb <- txtProgressBar(min = 1, max = nrow(dat.species), 
                     style = 3)
for(j in 1:nrow(dat.species)){
  #cat(j)
  store=exp(as.numeric(mapply(function(mu, sigma) rnorm(n = 1, mean = mu, sd = sigma), 
                              (posterior_dist$alpha + posterior_dist$beta * log(dat.species[j,]$l_mm/10)), # mu
                              posterior_dist$sigma))) # sigma
  ints=as.numeric(quantile((store), probs = c(0.001,0.999)))
  dat.species[j,]$lo.ci=ints[1]
  dat.species[j,]$hi.ci=ints[2]
  setTxtProgressBar(pb, j)
}

ci.dat=dat.species%>%
  dplyr::group_by(l_mm)%>%
  dplyr::summarise(lo.ci=mean(lo.ci), hi.ci=mean(hi.ci))

dat.species$suspicious=ifelse(dat.species$w_g>dat.species$hi.ci|dat.species$w_g<dat.species$lo.ci,1,0)
dat.species$id=seq(1:nrow(dat.species))

ggplot(data=dat.species)+
  geom_point(aes(x=l_mm, y=w_g, color=factor(suspicious)))+
  geom_ribbon(data=ci.dat,aes(x=l_mm,ymin=lo.ci, ymax=hi.ci), alpha=0.2)

#dat.species[dat.species$l_mm<75&dat.species$w_g>250,]

# isolate suspicious data
suspicious.data=dat.species[dat.species$suspicious==1,]
write.csv(suspicious.data, paste0(target.dir,'/output/edits/', target.species, '_suspicious_lw.csv'), row.names = F)


dat.sole$hlt=ifelse(dat.sole$id%in%c(259),1,0)
ggplot(data=dat.sole)+
  geom_point(aes(x=l_mm, y=w_g, color=factor(hlt), size=factor(hlt)))

