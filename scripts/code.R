#setwd
# setwd("~/gitProgetti/microti")
# source("~/gitProgetti/basicpkg.R")
library(tidyverse)
library(readxl)
library(here)
dt <- read_excel(here("data/raw", "MICROTI definitive - modified.xlsx"))

dt$IDGr<-1:nrow(dt)


library(ggraph)
library(igraph)


dt %>% 
  mutate(Grgrade=ifelse(Grgrade==1, "G1",
                        ifelse(Grgrade==2, "G2", 
                               ifelse(Grgrade==3, "G3", "G4")))) %>% 
  mutate(Grgrade=factor(Grgrade, levels=c("G1", "G2", "G3","G4"))) %>% 
  mutate(D=2*sqrt(totalArea/3.14)) %>% 
  mutate(d=2*sqrt(Grarea/3.14)) %>% 
  
  group_by(Idlinf, Micro, D,d, Grcompl,IDGr, Grgrade, MNC, NAF) %>% 
  summarise(ngr=n())%>% 
  pivot_wider( names_from = "Grgrade", values_from = ngr, values_fill = list(ngr=0)) %>% 
  View()











linfo<-unique(dt$Idlinf)
set.seed(999)
s<-sample(linfo, 20)

dt2<-dt %>% 
  filter(Idlinf %in% s) 


gdt<-dt2 %>% 
  mutate(level1=factor(rep("X", dim(.)[1])), 
         level2=factor(Idlinf), 
         level3=factor(IDGr)) %>% 
  select(level1, level2, level3)

edges_level1_2 <- gdt %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 <- gdt %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edge_list=rbind(edges_level1_2, edges_level2_3)




mygraph <- graph_from_data_frame( edge_list )

ggraph(mygraph, layout = 'dendrogram', circular = FALSE)+ 
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()



library(DiagrammeR)
x<-grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fontsize=6,]
      edge [arrowhead = none, arrowtail = none]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
         
  



      # edge definitions with the node IDs
      tab1 -> tab2;
      tab1 -> tab3;
      tab1 -> tab4;
      tab1  ->  tab5;
      
      
      }

      [1]:  paste0('Linfonodo (IdLinf):\\n ', 'dimensione \\n', 'stato microbiologico \\n', 'n.di granulomi')
      [2]:  paste0('granuloma (idgr):\\n ', 'dimensione \\n', 'grado istologico \\n', 'MNC \\n', 'NAF \\n')
      [3]:  paste0('granuloma (idgr):\\n ', 'dimensione \\n', 'grado istologico \\n', 'MNC \\n', 'NAF \\n')
      [4]:  paste0('granuloma (idgr):\\n ', 'dimensione \\n', 'grado istologico \\n', 'MNC \\n', 'NAF \\n')
      [5]: 'livello 1'
      [6]: 'livello 2'
    


      ")









































GGally::ggcorr(dt[,4:7], geom="text")

lnf<- dt %>% 
    select(Idlinf, Micro,totalArea) %>% 
  group_by(Idlinf, Micro,totalArea) %>% 
  mutate(ngr=n()) %>% 
  unique()
  
 
lnf<- dt %>% 
  select(Idlinf, Micro,totalArea, Grgrade) %>% 
  group_by(Idlinf, Micro,totalArea) %>% 
  mutate(ngr=n()) %>% 
  unique()


dt%>% 
  group_by(Idlinf, Grgrade ) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  mutate(reg=Grgrade) %>% 
  ggplot(aes(x=reg, y=n))+
  geom_bar(stat="identity", position=position_dodge(), width = 0.9)+
  labs(x="grado istologico", y="N.granulomi")+
  theme(axis.text=element_text(size=12))+theme_light()+
  geom_text(aes(label=n), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")









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
        group_by(Idlinf,Grgrade, Micro) %>% 
        summarise(ngr=sum(n())) %>% 
        ggplot(aes(x=as.factor(Grgrade),y=log(ngr)))+
        geom_bar(aes(fill=Micro),stat="identity", position=position_dodge(), width = 0.6) +
        facet_wrap(~Idlinf) 
    
  

      
      
        dt %>% 
        filter(Idlinf %in% s) %>% 
        group_by(Idlinf,Grgrade, Micro) %>% 
        summarise(ngr=sum(n())) %>% 
        ggplot(aes(x=as.factor(Grgrade),y=log(ngr), fill=Micro))+
        geom_bar(stat="identity", position=position_dodge(), width = 0.6) +
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
    View()
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
  mutate(NAFq=ifelse(NAF==0, 0, 1)) %>% 
  group_by(Micro) %>% 
  summarise(mean(MNCq), mean(NAFq))
  group_by(Idlinf, Grgrade, Micro) %>% 
  summarise(mncz=1-mean(MNCq)) %>% 
  ggplot(aes(x=Grgrade, y=mncz)) +
  geom_point() +
  facet_wrap(~Micro)
  


linfo<-unique(dt$Idlinf)
set.seed(999)
s<-sample(linfo, 20)

dt %>% 
 # filter(Idlinf %in% s) %>% 
  group_by(Idlinf,Micro, Grgrade) %>%
  summarise(area=mean(Grarea)) %>%
  arrange(area) %>% 
  mutate(linf=factor(Idlinf, unique(Idlinf))) %>% 
  ggplot(aes(x=linf,y=log(area), color=factor(Grgrade)))+
  geom_point(size=2.3)+coord_flip()+theme_light()+facet_wrap(~Micro)





dt %>% 
  group_by(Grgrade, Grarea) %>% 
  ggplot(aes(x=as.factor(Grgrade),y=log(Grarea)))+
  geom_boxplot(width=0.3)+geom_jitter(alpha=0.4, width = 0.15, size=0.8, color="blue", shape=19)+
  theme_light()




  
#################MIXED MODEL###############


# library(lme4)
# 
# 
# mod<-lmer(log(MNC+1)~Micro+factor(Grgrade)+log(NAF+1)+(1|Idlinf), data =dt )
# 
# library(merTools)
# REsim(mod)             # mean, median and sd of the random effect estimates
# plotREsim(REsim(mod))
# 
# 
# library(sjPlot)
# 
# p<-plot_model(mod)
# 
# plot_grid(p)
# 
# p$Micro
# p$Grgrade
# 



####MODELLI ####

ngr<-dt %>% 
  select( "Idlinf", "totalArea",  "Grarea" , "MNC" , "Grgrade", 
          "NAF", "Micro",  "IDGr" ) %>% 
  group_by(Idlinf) %>%
  summarise(ngr=n())

dt2<-dt %>% 
  full_join(ngr, by="Idlinf")



# dt %>% 
#   ggplot(aes(x=log(MNC+1)))+
#   geom_histogram()
# 
# dt %>% 
#   ggplot(aes(x=NAF))+
#   geom_histogram()+xlim(-1,50)
# 
# 
# 
# dt %>% 
#   summarise(m=mean(MNC), 
#             s2=var(MNC), 
#             r=s2/m)



library(glmmTMB)

m1 <- glmmTMB(MNC ~ as.factor(Grgrade)+(1|Idlinf),zi=~Micro,
               family=nbinom2, data=dt2)


m2 <- glmmTMB(MNC ~ as.factor(Grgrade)+scale(totalArea)+log(NAF+1)+log(ngr)+scale(Grarea)+Micro+(1|Idlinf),zi=~Micro,
              family=nbinom2, data=dt2)










library(sjPlot)

p<-plot_model(m2, show.intercept = FALSE, transform = NULL)



count<-p[["data"]] %>% 
  filter(wrap.facet=="conditional")



Grade_2<-rnorm(1000000, -0.128191245,  0.06123946 )
Grade_3<-rnorm(1000000, -0.267964545, 0.09482551  )
Grade_4<-rnorm(100000, 0.837276044, 0.11846970)
AreaLinf<-rnorm(100000, -0.574638176, 0.18887923)
logNAF <- rnorm(100000, 0.059668159, 0.04481653)
logNgr <- rnorm(100000, 0.148200418, 0.11113954)
AreaGran <- rnorm(100000,  0.197207487, 0.02694107 )
MicroP <- rnorm(100000,  -0.008527956,  0.36117827 )


dx<-data.frame( Grade_2, Grade_3, Grade_4, AreaLinf, logNAF, 
               logNgr, AreaGran, MicroP)
library(sjPlot)
library(ggeffects)
library(sjmisc)
library(bayesplot)
library(tidybayes)

dx %>% 
  pivot_longer(1:8,names_to = "group", values_to = "estimate") %>% 
 # mutate(group=factor(group, levels=c("Grade_4","Grade_3","Grade_2", "AreaLinf", "logNAF",
                                     # "logNgr","AreaGran","MicroP"))) %>% 
  ggplot(aes(x=estimate, y=group, fill=group))+
  geom_halfeyeh(alpha=0.8, .width = c(0.95))+
  scale_fill_manual(values=c("firebrick4","firebrick4", "firebrick4","firebrick4",
                             "firebrick4", "firebrick4", "firebrick4", "firebrick4"))+
  vline_0(color="red",linetype = 2)+labs(title="Coefficients plot", x="Count model estimated coefficients (log-mean)", y="")+
  theme_ggeffects()+theme(legend.position = 'none')+labs(title="MNC- coefficient plot")

















library(brms)
  
 
options(mc.cores=parallel::detectCores())
  

  
nullmdl <- brm(Grgrade ~ (1|Idlinf), data=dt,
               family=cumulative("logit"), cores = 4, iter = 5000)
  
  
  
modM <- brm(Grgrade ~ Micro+log(MNC+1)+(1|Idlinf), data=dt, family=cumulative("logit"), cores = 4, iter = 5000)




  
  
  
  
  
  

#  library(rethinking)
#  simplehist(dt$Grgrade, xlim=c(1,4), xlab="Grado")  
# # 
#  pr_k<-table(dt$Grgrade)/nrow(dt)
#  cum_pr_k<-cumsum(pr_k)
#  plot(1:4, cum_pr_k, type="b",xlab="Grado")
# # 
#  logit<-function(x) log(x/(1-x))
#  lco<-logit(cum_pr_k)
# # 
#  plot(1:4, lco, type="b",xlab="Grado")
# # 
# # 
# # m1.0<-ulam(
# #   alist(
# #     Gr~dordlogit(0, cutpoints),
# #     cutpoints~dnorm(0,1.5)
# #   ), data=list(Gr=dt$Grgrade), chains=4,cores=4)
# # 
# # 
# # dat<-list(Gr=dt$Grgrade,
# #           M=as.integer(as.factor(dt$Micro))-1)
# # 
# # 
# # m1.1<-ulam(
# #   alist(
# #     Gr~dordlogit(phi, cutpoints),
# #     phi<-bM*M,
# #     bM~dnorm(0,0.5),
# #     cutpoints~dnorm(0,1.5)
# #   ), data=dat, chains=4,cores=4)
# # 
# # 
# # 
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