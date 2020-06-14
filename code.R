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
  
    ggplot(aes(x=Micro, y=log(totAgrGrade), color=Micro))+
    geom_boxplot(width=0.3)+geom_jitter(alpha=0.6,width = 0.15) +
    theme_light()+facet_grid(~Grgrade)+labs(title="Distribuzione della somma delle aree dei granulomi in base a status microbiologico e grado istologico")
  
  
  
  dt %>% 
    select(Idlinf, Micro,totalArea, Grgrade, Grarea) %>% 
    group_by(Idlinf, Micro,Grgrade) %>% 
    mutate(nArea=Grarea/totalArea) %>% 
    summarise(SumnArea=sum(nArea)) %>% 
    ggplot(aes(x=Micro, y=log(SumnArea), color=Micro))+
    geom_boxplot(width=0.3)+geom_jitter(alpha=0.6,width = 0.15) +
    theme_light()+facet_grid(~Grgrade)+labs(title="Distribuzione della somma normalizzata delle singole aree dei granulomi in base a status microbiologico e grado istologico")
  
  
  
  
  dt %>% 
    select(Grgrade, Grarea) %>% 
    group_by(Grgrade) %>% 
    summarise(a=mean(Grarea),
              min=min(Grarea),
              max=max(Grarea)) %>% 
    ggplot(aes(x=Grgrade, y=Grarea))+geom_boxplot(width=0.3)
  
  
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
    
################################################################################
  
dt %>% 
    select(Idlinf,Grgrade, Micro, MNC, NAF) %>% 
    group_by(Idlinf, Grgrade, Micro) %>% 
    summarise(mnc=mean(log(MNC+1)),
              zhil=mean(log(NAF+1))) %>% 
    ggplot(aes(x=as.factor(Grgrade), y=mnc))+
    geom_boxplot(width=0.3, )+geom_jitter(alpha=0.6,width = 0.15) +
    theme_light()+facet_grid(~Micro)
      

  dt %>% 
    select(Idlinf,Grgrade, Micro, MNC, NAF) %>% 
    group_by(Idlinf, Grgrade, Micro) %>% 
    summarise(mnc=mean(log(MNC+1)),
              naf=mean(log(NAF+1))) %>% 
    ggplot(aes(x=as.factor(Grgrade), y=naf))+
    geom_boxplot(width=0.3, )+geom_jitter(alpha=0.6,width = 0.15) +
    theme_light()+facet_grid(~Micro)
  
  
  
  
  dt %>% 
    select(Idlinf, Micro,totalArea, Grgrade) %>% 
    group_by(Idlinf, Micro,totalArea,Grgrade) %>% 
    mutate(ngr=n()) %>% 
    mutate(grade=factor(Grgrade)) %>% 
    unique() %>% 
    ggplot()+
    geom_density(aes(x=log(ngr), colour=grade), adjust=1.8)+
    geom_rug(aes(x = log(ngr),y = 0), position = position_jitter(height = 0))+
    theme_light()+facet_grid(~Micro)
  
  
  
  p5m<- dt %>%
    select(Idlinf, Micro,totalArea, Grgrade) %>%
    group_by(Idlinf, Micro,totalArea,Grgrade) %>%
    mutate(ngr=n()) %>%
    mutate(grade=factor(Grgrade)) %>% 
    unique() %>%
    ggplot(aes(x=Micro, y=log(ngr), color=Micro))+
    geom_boxplot(width=0.3)+geom_jitter(alpha=0.6, width = 0.15)+
    theme_light()+facet_grid(~Grgrade)
  
  
  dt %>% 
    select(Idlinf, Micro,totalArea, Grgrade, Grarea) %>% 
    group_by(Idlinf, Micro,Grgrade) %>% 
    mutate(nArea=Grarea/totalArea) %>% 
    summarise(SumnArea=sum(nArea)) %>% 
    ggplot(aes(x=as.factor(Grgrade), y=log(SumnArea)))+
    geom_boxplot(width=0.3)+geom_jitter(alpha=0.6,width = 0.15) + 
    theme_light()+facet_grid(~Micro)+labs(x="grado istologico",caption="Distribuzione della somma normalizzata delle singole aree dei granulomi in base a status microbiologico e grado istologico") 
  
  
  
  
  
  
  
  
  
  
  
#################
  
  
dt %>% 
    group_by(Micro,Grgrade) %>% 
    summarise(ngr=n()) %>% 
    ggplot(aes(x=Grgrade, y=ngr)) + 
    geom_bar(stat="identity", position=position_dodge(), width = 0.6, fill="steelblue")+
    facet_wrap(~Micro)
    
  
dt %>% 
  mutate(MNCq=ifelse(MNC==0, 0, 1)) %>% 
  group_by(Idlinf, Grgrade, Micro) %>% 
  summarise(mncz=1-mean(MNCq)) %>% 
  ggplot(aes(x=Grgrade, y=mncz)) +
  geom_bar(stat="identity") +
  facet_wrap(~Micro)
  


  
  
  
library(brms)
  
 
options(mc.cores=parallel::detectCores())
  

  
nullmdl <- brm(Grgrade ~ (1|Idlinf), data=dt,
               family=cumulative("logit"), cores = 4, iter = 4000)
  
  
  
modM <- brm(Grgrade ~ Micro+(1|Idlinf), data=dt, family=cumulative("logit"), threshold="flexible")


  
  
  
  
  
  
  
  

# library(rethinking)
# simplehist(dt$Grgrade, xlim=c(1,4), xlab="Grado")  
# 
# pr_k<-table(dt$Grgrade)/nrow(dt)
# cum_pr_k<-cumsum(pr_k)
# plot(1:4, cum_pr_k, type="b",xlab="Grado")
# 
# logit<-function(x) log(x/(1-x))
# lco<-logit(cum_pr_k)
# 
# plot(1:4, lco, type="b",xlab="Grado")
# 
# 
# m1.0<-ulam(
#   alist(
#     Gr~dordlogit(0, cutpoints),
#     cutpoints~dnorm(0,1.5)
#   ), data=list(Gr=dt$Grgrade), chains=4,cores=4)
# 
# 
# dat<-list(Gr=dt$Grgrade,
#           M=as.integer(as.factor(dt$Micro))-1)
# 
# 
# m1.1<-ulam(
#   alist(
#     Gr~dordlogit(phi, cutpoints),
#     phi<-bM*M,
#     bM~dnorm(0,0.5),
#     cutpoints~dnorm(0,1.5)
#   ), data=dat, chains=4,cores=4)
# 
# 
# 
# 
# cutpoints <- m1.1 %>%
#   recover_types(dat) %>%
#   spread_draws(cutpoints[Gr])
# 
# # define the last cutpoint
# last_cutpoint <- tibble(
#   .draw <- 1:max(cutpoints$.draw),
#   Gr <- "4",
#   cutpoints <- Inf
# )
# 
# 
# cutpoints = bind_rows(cutpoints, last_cutpoint) %>%
#   # define the previous cutpoint (cutpoint_{j-1})
#   group_by(.draw) %>%
#   arrange(Gr) %>%
#   mutate(prev_cutpoint = lag(cutpoints, default = -Inf))
# 
# cutpoints %>%
#   group_by(Gr) %>%
#   median_qi(cutpoints, prev_cutpoint)
# 
# 
# fitted_cGr_probs = dat %>%
#   data_grid(mpg = seq_range(mpg, n = 101)) %>%
#   add_fitted_draws(m_cyl) %>%
#   inner_join(cutpoints, by = ".draw") %>%
#   mutate(`P(cyl | mpg)` =
#            # this part is logit^-1(cutpoint_j - beta*x) - logit^-1(cutpoint_{j-1} - beta*x)
#            plogis(cutpoint - .value) - plogis(prev_cutpoint - .value)
#   )
# 
# fitted_cyl_probs %>%
#   head(10)