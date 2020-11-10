library("here")

source(here("scripts", "dataset.R"))




#Modello stato microbiologico (Pos/Neg)####

# uso il dataset dtM
#<- definisco le prior del modello e uso una prior per i coefficenti b winfo

#variabili da inserire nel modello...
# diametro totale dei linfonodi D ( variabile scalata)
# sG1,sG2,sG3,sG4 <- proporzione per linfonodo di granulomi dei diversi gradi istologici
# Naf <- presenza di batteri acido resistenti nel linfonodo 
# MNC <- presenza di cellule giganti nel linfonodo
# Lngr <- numero di granulomi per linfonodo (log)

formula <- Micro ~  sG1 + sG2 + sG3 + sG4 + Naf + MNC
family <- bernoulli("logit")
dati <- dtM

myprior = c(set_prior("normal(0,10)", class = "Intercept"), 
            set_prior("normal(0,1)", class = "b"))


GGally::ggcorr(dtM[,c(7:10, 12)], geom="text")

# PRIOR PREDICTIVE CHECK full model####

mprior <- brm(Micro ~ 1 + sG4 + sG3 + sG2 + sG1 + Ds + Lngr + Naf + MNC,
             family = bernoulli("logit"), 
             prior = myprior,
             data = dtM, iter = 4000, cores = 4, 
             sample_prior = "only",
             seed = 1999,
             control = list(adapt_delta = 0.90,
                            max_treedepth = 20
             ))

pp_check(mprior, nsamples = 100)


#generare fake data ......
newY=predict(fullmod, newdata = dtM, summary=FALSE, nsamples=1)[1,]
dtM$Yfake <- newY
#fit model with fake data
fakeMod <- brm(Yfake ~ sG4 + sG3 + sG2 + sG1  + Lngr + Naf + MNC + Ds,
             family = bernoulli("logit"), 
             prior = myprior,
             data = dtM, iter = 4000, cores = 4, 
             control = list(adapt_delta = 0.90,
                            max_treedepth = 20
             ))

#check predictive 

pp_check(fakeMod, type = "error_hist")

####MODEL#####
mod1 <- brm(formula = formula,
             family = bernoulli("logit"), 
             prior = myprior,
             data = dtM, iter = 4000, cores = 4, 
             control = list(adapt_delta = 0.90,
                            max_treedepth = 20
             ))
mod2 <- update(mod1, . ~ . - Ds, newdata = dtM)
mod3 <- update(mod2, . ~ . - Lngr,newdata = dtM)


# color_scheme_set("red")
# ppc_dens_overlay(y = dtM$Micro,
#                  yrep = posterior_predict(mod3, draws = 50))
# 
# 
# loo(mod1,mod2,mod3,mod4,mod5)
# 
# result <- estimate_density(mod1)
# 
# plot(result, priors = TRUE)
# 
# result <- p_direction(mod1)
# plot(result, priors = TRUE)
# 
# result <- p_significance(mod1)

ceff <- conditional_effects(mod3)



dtceff %>% 
  ggplot(aes(x=ceff$, y=Micro))+geom_line()+
  











#ORDINAL MODEL####
#dataset <-dt2
#prior predictive check
#outcome: grade
#predittori : diametro, Micro, ngr, NAFc, MNCc, naf, mnc, diametroGr

# formula <- grade ~ (1|Idlinf) + Micro + Grcompl + lngr + lnaf + lmnc + lGrArea
# formula2 <- grade ~ (1|Idlinf) + MNCc  + mnc + cs(diametroGr)+ ngr


# fullmod <-brm(formula, 
#               data=dt2, family=sratio(), 
#               prior = myprior,
#               cores = 4, iter = 2000, chains = 4,
#               sample_prior = "only")
# 
# newY=predict(fullmod, newdata = dt2, summary=FALSE, nsamples=1)[1,]
# dt2$Yfake <- newY
# #fit model with fake data
# fakeMod <- brm(Yfake ~ (1 | Idlinf) + Micro + diametro + ngr + NAFc + MNCc + 
#                  naf + mnc + diametroGr,
#                data=dt2, family=sratio(), 
#                prior = myprior,
#                cores = 4, iter = 2000, chains = 4,
#                )
# 
# pp_check(fakeMod, nsamples = 100)
# 
# 
# 
# dt2 %>% 
#   ggplot(aes(x = grade, y = lGrArea))+
#   geom_jitter()
# 
# dt2 %>% 
#   ggplot(aes(x = lngr, y = lGrArea))+
#   geom_jitter()+geom_smooth()

#Model

formula <- grade ~ (1|Idlinf) 
family <- sratio("logit")
dati <- dt2

myprior = c(set_prior("normal(0,5)", class = "Intercept"), 
            set_prior("normal(0,5)", class = "b"))

mod <-brm(formula, 
              data=dt2, family=family, 
              prior = set_prior("normal(0,5)", class = "Intercept"),
              cores = 4, iter = 4000, chains = 4)




mod1 <- update(mod, .~. +sArea,newdata = dt2, prior = myprior)
#mod1a <- update(mod1, .~. , family = cumulative("logit"))
mod2 <- update(mod1, .~. + Grcompl + snaf + smnc + Micro + sngr, newdata = dt2, cores = 4, iter = 4000, chains = 4)



GGally::ggcorr(dt2[,c(19:22)], geom="text")


kfold3 <-kfold(mod3, K=10)
kfold4 <- kfold(mod4, K=10)

save(mod2, file = "ordinalM.RData")

load(here("data", "processed","ordinalM.RData"))

make_stancode(grade ~ (1 | Idlinf) + sArea + Grcompl + snaf + smnc + Micro + sngr, data = dt2 )



plot(p_direction(mod2, parameters = c("MicroP", "sngr",  "GrcomplI", "snaf", "smnc",  "sArea")))

plot(p_direction(mod2, effects = "random"))


conditional_effects(mod2, categorical = TRUE, effects = "sArea")


pdr <- p_direction(mod4, effects = "random")

pdr %>% 
  mutate(pv = pd_to_p(pd)) %>% 
  filter(pv <= 0.1)

plot(pd[4:8,])





####g1 grafico distribuzione n.granulomi per grado#####
p<-dt2 %>% 
  group_by(grade) %>% 
  summarise(freq=n()) %>% 
  ggplot(aes(x=grade, y=freq))+
  geom_point() + 
  geom_text(aes(label = freq), nudge_y = 100) +
  geom_segment(aes(xend = grade, yend = 0)) +
  xlab("grade")+ ylab("frequency")+
  theme_light()


pr_k<-table(dt$Grgrade)/nrow(dt)
cum_pr_k<-cumsum(pr_k)
  
plot.df <- data.frame(grado = 1:4, cum_pr_k)

p1<-plot.df %>% 
  ggplot(aes(x = grado, y = cum_pr_k)) +
  geom_point(color = "gray70") +
  geom_line(color = "gray70") +
  geom_point(aes(y = cum_pr_k)) +
  geom_line(aes(y = cum_pr_k)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(minor_breaks = 0:5) +
  xlab("grade") +
  ylab("Cumulative Proportion") +
  theme_minimal()

lco <- rethinking::logit(cum_pr_k)
plot.df$lco <- lco

p2 <- ggplot(plot.df[1:3, ], aes(x = grado, y = lco)) +
  geom_point(color = "gray70") +
  geom_line(color = "gray70") +
  geom_point(aes(y = lco)) +
  geom_line(aes(y = lco)) +
  scale_y_continuous() +
  scale_x_continuous(minor_breaks = 0:4) +
  xlab("grade") +
  ylab("Log-Cumulative-Odds") +
  theme_minimal()



plot.df$pr_k<-pr_k

plot.df$ys <-  plot.df$cum_pr_k - plot.df$pr_k

p3<-ggplot(plot.df, aes(x = grado, y = cum_pr_k)) +
  geom_segment(aes(x = grado, xend = grado, y = ys, yend = cum_pr_k), size = 1.3, color = "gray70") +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("grade") +
  ylab("Cumulative Proportion") +
  theme_minimal()

library(patchwork)
p|p1/p2|p3

# family <- brms::cumulative(logit)
# formula <- grade ~ 1
# 
# get_prior(formula, data = dt2, family = family)
# 
# prior <- brm(formula = formula,
#              data = dt2,
#              family = family,
#             # prior = set_prior("student_t(3, 0, )", class = "Intercept"),
#              sample_prior = "only",
#              seed = 9121,
#              cores = 4, iter = 5000)
# prior_out <- predict(prior, probs = c(0, 1))
# head(prior_out)
# 
# fit0 <- brm(formula = formula,
#             data = dt2,
#             family = family,
#            # prior = set_prior("student_t(3, 0, 10)", class = "Intercept"),
#             seed = 9121)
# fixef(fit0)
# 
# fit0_out <- predict(fit0, probs = c(0, 1))
# head(fit0_out)
# 


fullmod <-brm(grade ~ Micro + lngr + naf + mnc + NAFc + MNCc + 
                diametro + diametroGr + (1|Idlinf), 
              data=dt2, family=cumulative("logit"), cores = 4, iter = 5000)

fullmod2 <-brm(grade ~ Micro + lngr + naf + mnc  + MNCc + 
                diametro + diametroGr*NAFc + (1|Idlinf), 
              data=dt2, family=cumulative("logit"), cores = 4, iter = 5000)



fullmod <-brm(grade ~ grade + (1|Idlinf), 
              data=dt2, family=cumulative("logit"), cores = 4, iter = 5000)



fullmod<-add_criterion(fullmod, "waic")
fullmod2<-add_criterion(fullmod2, "waic")


conditions <- make_conditions(fullmod2, "NAFc")

conditional_effects(fullmod2, conditions = conditions, effects = "diametroGr:NAFc")


color_scheme_set("red")
a<-mcmc_plot(fullmod)
b<- mcmc_areas_ridges(fullmod2)


loo_compare(fullmod, fullmod2)



#to plot interaction##


mod1 <- brm(grade ~ Micro + lngr + naf + mnc + NAFc + MNCc + 
               (1|Idlinf), 
            data=dt2, family=cumulative("logit"), cores = 4, iter = 5000)


mod0 <- brm(grade ~ diametroGr + (1|Idlinf), 
            data=dt2, family=cumulative("logit"), cores = 4, iter = 5000)


mod0.1 <- brm(grade ~ naf + NAFc + (1|Idlinf), 
            data=dt2, family=cumulative("logit"), cores = 4, iter = 5000)


mod0.2 <- brm(NAF~diametroGr + (1|Idlinf), 
              data=dt2, 
              family=zero_inflated_negbinomial(), 
              cores = 4, iter = 5000)
 


save(fullmod2, file="modelli2.Rdata")


# fullpred <- predict(fullmod, probs = c(0, 1))
# fullfit <- posterior_samples(fullmod)

###pp-check per confronto dati-modello rispetto al grade (variabile outcome)
pp_check(fullmod, type = "bars", nsamples = 1000) +
  scale_x_continuous("grade", breaks = 1:7) +
  ggtitle("Data with posterior predictions") +
  theme(legend.background = element_blank(),
        legend.position = c(.9, .8),
        panel.grid = element_blank())




get_variables(fullmod) %>% 
  View()

#### Population-level effects plot ####
fullmod %>%
    gather_draws(b_MicroP, b_lngr, b_naf, b_mnc, b_NAFc1, b_MNCc1, b_diametro ) %>%
    
    median_hdi(.width = c(.95, .66)) %>%
    mutate(predictor = reorder(.variable, .value)) %>% 
    ggplot(aes(y = predictor, x = .value, xmin = .lower, xmax = .upper)) +
    geom_pointintervalh()+
    geom_vline(xintercept = 0, col = "gray70") +
    scale_y_discrete(labels = c("N.granulomi(log)", "MNCpresence", "MicroP", 
                                "diametro", "MNC", "NAFpresence", "NAF")) +
   # scale_x_continuous(limits = c(-.2, NA), breaks = c(-.2, 0, .2)) +
    ylab("") + xlab("Coefficient Value") +
   # ggtitle("A") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 

#alternative plot
fullmod %>%
  gather_draws(b_MicroP, b_lngr, b_naf, b_mnc, b_NAFc1, b_MNCc1, b_diametro ) %>%
  mutate(predictor = reorder(.variable, .value)) %>% 
  ggplot(aes(y = predictor, x = .value)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  geom_vline(xintercept = 0, col = "gray70") +
  scale_y_discrete(labels = c("N.granulomi(log)", "MNCpresence", "MicroP", 
                              "diametro", "MNC", "NAFpresence", "NAF")) +
  ylab("") + xlab("Coefficient Value") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())






######group - level effects plot ######
d<-fullmod %>%
  spread_draws(r_Idlinf[Idlinf,Intercept]) %>%
  median_qi() 

d$linf<-seq(1:98)
  

ggplot(d, aes(y = linf, x = r_Idlinf, xmin = .lower, xmax = .upper)) +
  geom_pointinterval(size=0.3)+
  xlab("Coefficient Value") + ylab("") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_vline(xintercept = 0, col = "blue")


#alternative plot

d<-fullmod %>%
  spread_draws(r_Idlinf[Idlinf,Intercept]) %>% 
  mutate(Lymph=factor(Idlinf)) %>% 
  ggplot(aes(y = Lymph, x = r_Idlinf)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  theme_ridges()+
  xlab("Coefficient Value")  +
  geom_vline(xintercept = 0, col = "blue")+
  theme(
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



#conditional effects plot
###diametro
gg <- conditional_effects(fullmod, categorical = TRUE)
ggdiam <- gg[[7]] 
diams <- scale(dt2$d)
scale <- attr(diams, "scaled:scale")
center <- attr(diams, "scaled:center")

ggdiam <- ggdiam %>%
  mutate(diams = diametro* scale + center)

  ggplot(ggdiam, aes(x = diams, y = estimate__, col = cats__)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), 
                alpha = .5, col = NA) +
    scale_color_brewer(name = "Histologic Grade", label = c("1", "2", "3", "4"), palette = "Reds") +
    scale_fill_brewer(name = "Histologic Grade", label = c("1", "2", "3", "4"), palette = "Reds") +
    scale_y_continuous(limits = c(0, 1)) +
    ylab("Probability") +
    xlab("Lymphonode dimension (diameter)") +
    theme_minimal() 
  
  ##alternative plot
  # ggplot(ggdiam, aes(x = diams, y = estimate__)) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin = lower__, ymax = upper__), 
  #               alpha = .5, col = NA) +
  #   # scale_color_brewer(name = "Histologic Grade", label = c("1", "2", "3", "4"), palette = "Reds") +
  #   # scale_fill_brewer(name = "Histologic Grade", label = c("1", "2", "3", "4"), palette = "Reds") +
  #   scale_y_continuous(limits = c(0, 1)) +
  #   ylab("Probability") +
  #   xlab("Lymphonode dimension (diameter)") +
  #   theme_minimal()+
  #   facet_grid(~cats__)

  gg <- conditional_effects(fullmod, categorical = TRUE)
  ggngr <- gg[[2]] 
  
  ggngr<- ggngr %>% 
    mutate(n.granul=exp(lngr))
  
  ggplot(ggngr, aes(x = n.granul, y = estimate__, col = cats__)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), 
                alpha = .5, col = NA) +
    scale_color_brewer(name = "Histologic Grade", label = c("1", "2", "3", "4"), palette = "Reds") +
    scale_fill_brewer(name = "Histologic Grade", label = c("1", "2", "3", "4"), palette = "Reds") +
    scale_y_continuous(limits = c(0, 1)) +
    ylab("Probability") +
    xlab("n.granulom per lymphonodes") +
    theme_minimal() 

  ####conditional effect NAF
  ggnaf <- gg[[3]]
  naf <- scale(dt2$NAF)
  scale <- attr(naf, "scaled:scale")
  center <- attr(naf, "scaled:center")
  
  ggnaf <- ggnaf %>%
    mutate(Naf = naf*scale + center)
  
  ggplot(ggnaf, aes(x = Naf, y = estimate__, col = cats__)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), 
                alpha = .5, col = NA) +
    scale_color_brewer(name = "Histologic Grade", label = c("1", "2", "3", "4"), palette = "Reds") +
    scale_fill_brewer(name = "Histologic Grade", label = c("1", "2", "3", "4"), palette = "Reds") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits=c(0, 100))+
    ylab("Probability") +
    xlab("n.acid resistent bacteria") +
    theme_minimal() 
  
###conditional plot MNC####
  ggmnc <- gg[[4]]
  mnc <- scale(dt2$MNC)
  scale <- attr(mnc, "scaled:scale")
  center <- attr(mnc, "scaled:center")
  
  ggmnc <- ggmnc %>%
    mutate(Mnc = mnc*scale + center)
  
  ggplot(ggmnc, aes(x = Mnc, y = estimate__, col = cats__)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), 
                alpha = .5, col = NA) +
    scale_color_brewer(name = "Histologic Grade", label = c("1", "2", "3", "4"), palette = "Reds") +
    scale_fill_brewer(name = "Histologic Grade", label = c("1", "2", "3", "4"), palette = "Reds") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits=c(0, 100))+
    ylab("Probability") +
    xlab("N. MNC ") +
    theme_minimal() 

  ###conditional plot diametroGR####
  ggmnc <- gg[[8]]
  
  
  
  
# library(sjPlot)
# tab_model(fullmod)






























# predicted <- dt2 %>%
#   data_grid(Micro = NA, lngr = 0, naf = 0, mnc = 0,  NAFc= 0, 
#             MNCc = 0 , diametro = 0, 
#             Idlinf = NA) %>%
# add_predicted_draws(fullmod) 
# 
# library(tidybayes)
# predicted <- dt2 %>%
#   dplyr::select(Micro , lngr, naf, mnc, NAFc, MNCc, diametro, Idlinf) %>%
#   add_predicted_draws(fullmod)
# 
# table(dt2$grade)/nrow(dt2)
# table(predicted$.prediction)/nrow(predicted)






# library(xtable)
# dt2 %>%  select(totalArea,diametro,ngr, NAF, MNC, Grgrade, Micro) %>% summary()



# # 
#  logit<-function(x) log(x/(1-x))
#  lco<-logit(cum_pr_k)
# # 
#  plot(1:4, lco, type="b",xlab="Grado")
# # 



############### Bayesian Ordinal Logistic Regression #####

library(brms)


options(mc.cores=parallel::detectCores())


nmod<- brm(grade ~ 1, data=dt2,
           family=cumulative("logit"), cores = 4, iter = 5000, 
           file = "nomd")


nullmod <- brm(grade ~ 1+ (1|Idlinf), data=dt2,
               family=cumulative("logit"), cores = 4, iter = 5000)


fullmod <-brm(grade ~ Micro + lngr + naf + mnc + NAFc + MNCc + diametro + (1|Idlinf), 
              data=dt2, family=cumulative("logit"), cores = 4, iter = 5000)


n_iter <- 50


nd <- tibble(mnc = seq(from = 0, to = 20, by = 0.5))


f <-
  fitted(fullmod,
         newdata = nd,
         summary = F,
         nsamples = n_iter)



forest(fullmod)



plot_model(fullmod, type = "re")





post <- posterior_samples(nullmod)
glimpse(post)


post %>% 
  mutate_all(.funs = ~pnorm(. ,0, 1)) %>% 
  transmute(`p[Y==1]` = `b_Intercept[1]`,
            `p[Y==2]` = `b_Intercept[2]` - `b_Intercept[1]`,
            `p[Y==3]` = `b_Intercept[3]` - `b_Intercept[2]`,
            `p[Y==4]` = 1 - `b_Intercept[3]`) %>% 
  set_names(1:4) %>% 
  pivot_longer(everything(), names_to = "Y") %>% 
  
  ggplot(aes(x = value, y = Y)) +
  geom_halfeyeh(point_interval = mode_hdi, .width = .95, size = 1/2) +
  xlab(expression(italic(p)*"["*Y==italic(i)*"]")) +
  coord_cartesian(xlim = c(0, 1)) +
  theme(panel.grid = element_blank())











set.seed(23)

pp_check(nmod, type = "bars", nsamples = 1000) +
  scale_x_continuous("grade", breaks = 1:7) +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("Data with posterior predictions",
          subtitle = "N = 100") +
  theme(legend.background = element_blank(),
        legend.position = c(.9, .8),
        panel.grid = element_blank())




save(list=c("nullmod", "fullmod"), file="modelli.Rdata")


pp_check(fullmod, type = "bars", nsamples = 1000) +
  scale_x_continuous("grade", breaks = 1:7) +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("Data with posterior predictions",
          subtitle = "N = 100") +
  theme(legend.background = element_blank(),
        legend.position = c(.9, .8),
        panel.grid = element_blank())


# dt2 %>% 
#   ggplot(aes(y=log(MNC+1), x=as.factor(Grgrade)))+
#   geom_boxplot()+facet_grid(~Micro)
#  
 

#################MIXED MODEL###############

##ZINB model
m0<- glmmTMB(NAF ~ as.factor(Grgrade)  + Grcompl + MNC+ ngr+
                  scale(diametro) + as.factor(Micro) + (1|Idlinf),
              zi= ~ as.factor(Grgrade)+ MNC+ ngr+
               scale(diametro) + as.factor(Micro) ,family=nbinom2, data=dt2)



m0<- glmmTMB(MNC ~ 1, family=nbinom2, data=dt2)

 











# 
# modM <- brm(Grgrade ~ Micro + (1|Idlinf), 
#             data=dt2, family=cumulative("logit"), cores = 4, iter = 5000)
# 
# 
# modM2<- brm(Grgrade ~ Micro + log(ngr) + (1|Idlinf), 
#             data=dt2, family=cumulative("logit"), cores = 4, iter = 5000)
# 
# 
# modM3 <-brm(Grgrade ~ Micro + log(ngr) + mnc + (1|Idlinf), 
#                     data=dt2, family=cumulative("logit"), cores = 4, iter = 5000)
# 
# modM4 <-brm(Grgrade ~ Micro + log(ngr) + mnc + naf + (1|Idlinf), 
#             data=dt2, family=cumulative("logit"), cores = 4, iter = 5000)
# 
# 
# 
# 


# library(sjPlot)
# 
# p<-plot_model(m1, show.intercept = FALSE, transform = NULL)
# 
# 
# 
# count<-p[["data"]] %>% 
#   filter(wrap.facet=="conditional")
# 
# 
# 
# Grade_2<-rnorm(1000000, -0.128191245,  0.06123946 )
# Grade_3<-rnorm(1000000, -0.267964545, 0.09482551  )
# Grade_4<-rnorm(100000, 0.837276044, 0.11846970)
# AreaLinf<-rnorm(100000, -0.574638176, 0.18887923)
# logNAF <- rnorm(100000, 0.059668159, 0.04481653)
# logNgr <- rnorm(100000, 0.148200418, 0.11113954)
# AreaGran <- rnorm(100000,  0.197207487, 0.02694107 )
# MicroP <- rnorm(100000,  -0.008527956,  0.36117827 )
# 
# 
# dx<-data.frame( Grade_2, Grade_3, Grade_4, AreaLinf, logNAF, 
#                 logNgr, AreaGran, MicroP)
# library(sjPlot)
# library(ggeffects)
# library(sjmisc)
# library(bayesplot)
# library(tidybayes)
# 
# dx %>% 
#   pivot_longer(1:8,names_to = "group", values_to = "estimate") %>% 
#   # mutate(group=factor(group, levels=c("Grade_4","Grade_3","Grade_2", "AreaLinf", "logNAF",
#   # "logNgr","AreaGran","MicroP"))) %>% 
#   ggplot(aes(x=estimate, y=group, fill=group))+
#   geom_halfeyeh(alpha=0.8, .width = c(0.95))+
#   scale_fill_manual(values=c("firebrick4","firebrick4", "firebrick4","firebrick4",
#                              "firebrick4", "firebrick4", "firebrick4", "firebrick4"))+
#   vline_0(color="red",linetype = 2)+labs(title="Coefficients plot", x="Count model estimated coefficients (log-mean)", y="")+
#   theme_ggeffects()+theme(legend.position = 'none')+labs(title="MNC- coefficient plot")