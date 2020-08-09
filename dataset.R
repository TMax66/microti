setwd("~/Library/Mobile Documents/com~apple~CloudDocs/gitProject/microti")
library(tidyverse)
library(sjPlot)
library(brms)
library(glmmTMB)
library(readxl)
library(tidybayes)
library(ggridges)
library(see)
library(bayestestR)
library(bayesplot)

dt <- read_excel("MICROTI definitive - modified.xlsx")
dt$id<-1:nrow(dt)

ngr<-dt %>% 
  select( "Idlinf", "totalArea",  "Grarea" , "MNC" , "Grgrade", 
          "NAF", "Micro" , "Grcompl", "id") %>% 
  group_by(Idlinf) %>%
  summarise(ngr=n())

####dataset per model con measurement error#####
dtw<-dt %>% 
  mutate(Grgrade=ifelse(Grgrade==1, "G1",
                        ifelse(Grgrade==2, "G2", 
                               ifelse(Grgrade==3, "G3", "G4")))) %>% 
  mutate(Grgrade=factor(Grgrade, levels=c("G1", "G2", "G3","G4"))) %>% 
  
  mutate(d=2*sqrt(Grarea/3.14)) %>% 
  group_by(Idlinf, Micro,d, Grcompl, Grgrade, MNC, NAF) %>% 
  summarise(ngr=n())%>% 
  pivot_wider( names_from = "Grgrade", values_from = ngr, values_fill = list(ngr=0)) %>% 
  group_by(Idlinf) %>% 
  summarise( mNaf=mean(log(NAF+1)), 
             mMNC=mean(log(MNC+1)),
            sG1=sum(G1), 
            sG2=sum(G2),
            sG3=sum(G3),
            sG4=sum(G4), 
            Sdg=sum(d)
            )  
lnf<- dt %>% 
  select(Idlinf, Micro,totalArea)%>% 
  mutate(D=2*sqrt(totalArea/3.14)) %>% 
  unique() 

dtM <- lnf %>% 
   left_join(dtw, by="Idlinf") %>% 
   left_join(ngr, by="Idlinf") %>% 
   mutate("Ds" = scale(D),
          Lngr = log(ngr), 
          sG1 =  sG1/ngr,
          sG2 =  sG2/ngr,
          sG3 =  sG3/ngr,
          sG4 =  sG4/ngr, 
          Naf = ifelse(mNaf == 0, "No", "Si"),
          MNC = ifelse(mMNC == 0, "No", "Si"), 
          Micro = ifelse(Micro == "P", 1, 0)) 




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

