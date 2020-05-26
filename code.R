#setwd
setwd("~/gitProgetti/microti")
source("~/gitProgetti/basicpkg.R")
dt <- read_excel("MICROTI definitive - modified.xlsx")

dt$IDGr<-1:nrow(dt)


GGally::ggcorr(dt[,4:7], geom="text")

lnf<- dt %>% 
    select(Idlinf, Micro,totalArea) %>% 
  group_by(Idlinf, Micro,totalArea) %>% 
  mutate(ngr=n()) %>% 
  unique()
  
 

lnf %>% 
  ggplot()+
  geom_density(aes(x=totalArea), adjust=2)+
  geom_rug(aes(x = totalArea, y = 0), position = position_jitter(height = 0))+
  theme_light()

lnf %>% 
  ggplot()+
  geom_density(aes(x=totalArea,colour = Micro), adjust=2)+
  geom_rug(aes(x = totalArea, y = 0), position = position_jitter(height = 0))+
  theme_light()


p3<-lnf %>% 
  filter(Micro=="P") %>% 
  ggplot()+
  geom_density(aes(x=ngr), adjust=0.5)+
  geom_rug(aes(x = ngr,y = 0), position = position_jitter(height = 0))+
  theme_light()

p4<-lnf %>% 
  ggplot()+
  geom_density(aes(x=ngr, colour=Micro), adjust=0.5)+
  geom_rug(aes(x = ngr,y = 0), position = position_jitter(height = 0))+
  theme_light()

(p3+p4)+plot_annotation(tag_levels = 'a')


p5<-lnf %>% 
  ggplot(aes(x=Micro, y=log(ngr), color=Micro))+
  geom_boxplot(width=0.3)+geom_jitter(alpha=0.6, width = 0.15)+
theme_light()


p6<-lnf %>% 
  ggplot(aes(x=Micro, y=log(ngr/totalArea), color=Micro))+
  geom_boxplot(width=0.3)+geom_jitter(alpha=0.6,width = 0.15) +
  theme_light()

(p5+p6)+plot_annotation(tag_levels= 'a')




dt %>% 
  ggplot(aes(x=as.factor(Grgrade), y=MNC))+geom_boxplot()+geom_jitter()
  

dt %>% 
  ggplot(aes(x=as.factor(Grgrade), y=log(NAF+1)))+geom_boxplot() +
  facet_wrap(~Idlinf)



linfo<-unique(dt$Idlinf)
s<-sample(linfo, 20)

dt %>% 
  filter(Idlinf %in% s) %>% 
  ggplot(aes(x=as.factor(Grgrade), y=log(MNC+1), color=Micro))+geom_boxplot() +
  facet_wrap(~Idlinf)


dt %>% 
  filter(Idlinf %in% s) %>% 
  ggplot(aes(x=as.factor(Grgrade), y=log(Grarea), color=Micro))+geom_boxplot() +
  facet_wrap(~Idlinf)


######multilevel model####

