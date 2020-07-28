setwd("~/gitProgetti/microti")
library(tidyverse)
library(sjPlot)
library(brms)
library(glmmTMB)
library(readxl)
library(tidybayes)
library(ggridges)

dt <- read_excel("MICROTI definitive - modified.xlsx")
dt$id<-1:nrow(dt)

ngr<-dt %>% 
  select( "Idlinf", "totalArea",  "Grarea" , "MNC" , "Grgrade", 
          "NAF", "Micro" , "Grcompl", "id") %>% 
  group_by(Idlinf) %>%
  summarise(ngr=n())

####dataset per model con measurement error#####
dt %>% 
  mutate(Grgrade=ifelse(Grgrade==1, "G1",
                        ifelse(Grgrade==2, "G2", 
                               ifelse(Grgrade==3, "G3", "G4")))) %>% 
  mutate(Grgrade=factor(Grgrade, levels=c("G1", "G2", "G3","G4"))) %>% 
  mutate(D=2*sqrt(totalArea/3.14)) %>% 
  mutate(d=2*sqrt(Grarea/3.14)) %>% 
  
  group_by(Idlinf, Micro, D,d, Grcompl, Grgrade, MNC, NAF) %>% 
  summarise(ngr=n())%>% 
  pivot_wider( names_from = "Grgrade", values_from = ngr, values_fill = list(ngr=0)) %>% 
  group_by(Idlinf) %>% 
  summarise(mNaf=mean(log(NAF+1)), 
            sdNaf=sd(log(NAF+1)), 
            mMNC=mean(log(MNC+1)),
            sdMNC=sd(log(MNC+1)), 
            sG1=sum(G1), 
            sG2=sum(G2),
            sG3=sum(G3),
            sG4=sum(G4), 
            mdg=mean(d),
            sdg=sd(d))  


lnf<- dt %>% 
  select(Idlinf, Micro,totalArea)%>% 
  unique()



###dataset per Ordinal Logistic Regression######
dt2<-dt %>% 
  full_join(ngr, by="Idlinf") %>% 
  mutate(grade=as.integer(as.numeric(factor(Grgrade)))) %>% 
  mutate(d=2*sqrt(totalArea/3.14)) %>% 
  mutate (diametro= scale(d)) %>% 
  mutate(NAFc=factor(ifelse(NAF==0, 0, 1))) %>% 
  mutate(MNCc=factor(ifelse(MNC==0, 0, 1))) %>% 
  mutate(Idlinf=factor(Idlinf)) %>% 
  mutate(lngr=log(ngr)) %>% 
  mutate(naf=scale(NAF)) %>% 
  mutate(mnc=scale(MNC)) %>% 
  mutate(dg=2*sqrt(Grarea/3.14)) %>% 
  mutate(diametroGr=scale(dg))  

