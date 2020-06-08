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
  ggplot(aes(x=as.factor(Grgrade), col=Micro, fill=Micro))+
geom_bar()+labs(x="Grado Istologico", y="N.granulomi")

p7<-dt%>% 
  group_by(Grgrade) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  mutate(reg=factor(Grgrade, unique(Grgrade))) %>% 
  ggplot(aes(x=reg, y=n))+
  geom_bar(stat="identity", position=position_dodge(), width = 0.6,fill="steelblue")+
  labs(x="grado istologico", y="N.granulomi")+
  theme(axis.text=element_text(size=12))+theme_light()+
  geom_text(aes(label=n), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")
  


p8<-dt%>% 
  group_by(Grgrade, Micro) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  mutate(reg=factor(Grgrade, unique(Grgrade))) %>% 
  ggplot(aes(x=reg, y=n, fill=Micro))+
  geom_bar(stat="identity", position=position_dodge(), width = 0.6)+
  labs(x="grado istologico", y="N.granulomi")+
  theme(axis.text=element_text(size=12))+theme_light()+
    geom_text(aes(label=n), vjust=1.6, color="white",
              position = position_dodge(0.9), size=3.5)+
    scale_fill_brewer(palette="Paired")
  

(p7+p8)+plot_annotation(tag_levels= 'a') 

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


######ml####

#p4 modificato

  dt %>% 
  select(Idlinf, Micro,totalArea, Grgrade) %>% 
  group_by(Idlinf, Micro,totalArea,Grgrade) %>% 
  mutate(ngr=n()) %>% 
  unique() %>% 
  ggplot()+
  geom_density(aes(x=log(ngr), colour=Micro), adjust=1.8)+
  geom_rug(aes(x = log(ngr),y = 0), position = position_jitter(height = 0))+
  theme_light()+facet_grid(~Grgrade)

#p5 modificato
  dt %>% 
  select(Idlinf, Micro,totalArea, Grgrade) %>% 
  group_by(Idlinf, Micro,totalArea,Grgrade) %>% 
  mutate(ngr=n()) %>% 
  unique() %>% 
  ggplot(aes(x=Micro, y=log(ngr), color=Micro))+
  geom_boxplot(width=0.3)+geom_jitter(alpha=0.6, width = 0.15)+
  theme_light()+facet_grid(~Grgrade)
  
  
#p6 modificato
  dt %>% 
    select(Idlinf, Micro,totalArea, Grgrade) %>% 
    group_by(Idlinf, Micro,totalArea,Grgrade) %>% 
    mutate(ngr=n()) %>% 
    unique() %>% 
    ggplot(aes(x=Micro, y=log(ngr/totalArea), color=Micro))+
    geom_boxplot(width=0.3)+geom_jitter(alpha=0.6,width = 0.15) +
    theme_light()+facet_grid(~Grgrade)

#somma delle aree dei singoli granulomi by grade e microbiologia
  dt %>% 
    select(Idlinf, Micro,totalArea, Grgrade, Grarea) %>% 
    group_by(Idlinf, Micro,Grgrade) %>% 
    summarise(totAgrGrade=sum(Grarea)) %>% 
    ggplot(aes(x=Micro, y=log(totAgrGrade), color=Micro))+
    geom_boxplot(width=0.3)+geom_jitter(alpha=0.6,width = 0.15) +
    theme_light()+facet_grid(~Grgrade)
  
  
  
 x<- dt %>% 
    select(Idlinf, Micro,totalArea, Grgrade, Grarea) %>% 
    group_by(Idlinf, Micro,Grgrade) %>% 
    summarise(totAgrGrade=sum(Grarea))
 
  y<-dt %>% 
    select(Idlinf, Micro,totalArea) %>% 
    group_by(Idlinf, Micro,totalArea) %>% 
    unique()
  
  
  z<-x %>% 
    left_join(y, by="Idlinf") 
  
  z %>% 
    ggplot(aes(x=Micro.x, y=log(totAgrGrade/totalArea), color=Micro.x))+
    geom_boxplot(width=0.3)+geom_jitter(alpha=0.6,width = 0.15) +
    theme_light()+facet_grid(~Grgrade)
  
  
  
  ##diametro=2*(sqrt(Area/pi))
  
  dati<-dt %>% 
    group_by(Idlinf) %>% 
    mutate(diametro=2*sqrt(totalArea/3.14))
    
  

  