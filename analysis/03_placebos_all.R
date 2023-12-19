<<<<<<< HEAD
library(tidyverse)
library(here)
library(sandwich)
library(lmtest)
require(broom)


# load data
load(file=paste0(here(),"/data/VoterSupportData.RData"))
load(file=paste0(here(),"/data/ClimatePolicies.RData"))
load(file=paste0(here(),"/data/Elections.RData"))

# Analysis ----

## a) full period time series 

### data

placebos <- bind_cols( placebo = as_factor(c("election2002", "election2006", "election2010", "terrorNO", "riotsSTH", "election2014", "terrorSTH", "election2018", "corona")), date = lubridate::as_date(c(elections$date[2], elections$date[3], elections$date[4], "2011-07-22", "2013-05-19", elections$date[5], "2017-04-07", elections$date[6], "2020-03-10")))



data_placebo <- VoterSupportData %>% 
  dplyr::select(
    Publicerad,
    GovernmentSupport,
    Unemployment,
    ConsumerPriceIndex,
    HouseholdConsumption,
    MiljöpartietMP,
    ConsumerConfidence,
    GovtSearch,
    above_published,
    PolicyIndicator,
    Date_numeric,
    GovernmentSupport_monthlyMean,
    GovernmentSupport_monthlyMean_deviation,
    IncumbentParty,
    PollingInstitute,
    StartCollection,
    EndCollection
  ) %>% mutate(above_published_placebo = case_when(`Publicerad` < as.Date(placebos$date[1])~0,
                                                   `Publicerad` > as.Date(placebos$date[1]) & `Publicerad` < as.Date(placebos$date[2]) ~ 1,
                                                   `Publicerad` > as.Date(placebos$date[2]) & `Publicerad` < as.Date(placebos$date[3]) ~ 2,
                                                   `Publicerad` > as.Date(placebos$date[3]) & `Publicerad` < as.Date(placebos$date[4]) ~ 3,
                                                   `Publicerad` > as.Date(placebos$date[4]) & `Publicerad` < as.Date(placebos$date[5]) ~ 4,
                                                   `Publicerad` > as.Date(placebos$date[5]) & `Publicerad` < as.Date(placebos$date[6]) ~ 5,
                                                   `Publicerad` > as.Date(placebos$date[6]) & `Publicerad` < as.Date(placebos$date[7]) ~ 6,
                                                   `Publicerad` > as.Date(placebos$date[7]) & `Publicerad` < as.Date(placebos$date[8]) ~ 7,
                                                   `Publicerad` > as.Date(placebos$date[8]) & `Publicerad` < as.Date(placebos$date[9]) ~ 8,
                                                   `Publicerad` > as.Date(placebos$date[9]) ~ 9) %>% as.factor(),
               PolicyIndicator_placebo = case_when(`EndDate` < as.Date(placebos$date[1])~0,
                                                   `StartCollection` > as.Date(placebos$date[1]) & `EndDate` < as.Date(placebos$date[2]) ~ 1,
                                                   `StartCollection` > as.Date(placebos$date[2]) & `EndDate` < as.Date(placebos$date[3]) ~ 2,
                                                   `StartCollection` > as.Date(placebos$date[3]) & `EndDate` < as.Date(placebos$date[4]) ~ 3,
                                                   `StartCollection` > as.Date(placebos$date[4]) & `EndDate` < as.Date(placebos$date[5]) ~ 4,
                                                   `StartCollection` > as.Date(placebos$date[5]) & `EndDate` < as.Date(placebos$date[6]) ~ 5,
                                                   `StartCollection` > as.Date(placebos$date[6]) & `EndDate` < as.Date(placebos$date[7]) ~ 6,
                                                   `StartCollection` > as.Date(placebos$date[7]) & `EndDate` < as.Date(placebos$date[8]) ~ 7,
                                                   `StartCollection` > as.Date(placebos$date[8]) & `EndDate` < as.Date(placebos$date[9]) ~ 8,
                                                   `StartCollection` > as.Date(placebos$date[9]) ~ 9) %>% as.factor(),
               StartCollection = StartCollection %>% as.Date(),
               EndDate = EndDate %>% as.Date()
               )


# remove period outlier
data_placebo <- data_placebo %>% filter(Date!="2002-10-01")


### model 
lm_placebo_controls <- lm( GovernmentSupport ~ -1 + #PolicyIndicator_placebo*Date_numeric +
                          lag(GovernmentSupport,1) +
                          lag(GovernmentSupport,2) +
                          lag(GovernmentSupport,3) +
                          Unemployment + 
                          ConsumerPriceIndex  +
                          HouseholdConsumption +
                          ConsumerConfidence +
                          as.factor(PollingInstitute) 
                        ,
                        data = data_placebo %>% as.data.frame(), na.action=na.exclude); summary(lm_placebo_controls)

step.placebomodel.didit <- MASS::stepAIC(lm_placebo_controls, direction = "forward", 
                                         trace = FALSE)
summary(step.placebomodel.didit)
coeftest(step.placebomodel.didit, vcov = vcovPL(step.placebomodel.didit))
preds_placebo <- bind_cols(data_placebo,predict(step.placebomodel.didit, interval = 'confidence')%>% as.data.frame(), residuals = resid(step.placebomodel.didit, na.action=na.exclude))


# plotting

timeseriesplot_placebo <- preds_placebo %>% ggplot(aes(y=GovernmentSupport, x=EndDate)) +
  geom_point(col="darkgray") +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==0), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==0), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==1), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==1), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==2), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==2),aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==3), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==3), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==4), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==4), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==5), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==5), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==6), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==6), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==7), aes(y=fit, x=EndDate), col="darkred") +
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==7), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==8), aes(y=fit, x=EndDate), col="darkred") +
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==8), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==9), aes(y=fit, x=EndDate), col="darkred") +
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==9), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  xlab("time") +
  geom_segment(data=placebos[c(1,3,8),], aes(x = date, xend= date, y = min(preds_placebo$GovernmentSupport, na.rm = T)-1, yend = max(preds_placebo$GovernmentSupport, na.rm=T)+1), col = "red", size=1) +
  geom_segment(data=placebos[c(2,6),], aes(x = date, xend= date, y = min(preds_placebo$GovernmentSupport, na.rm = T)-1, yend = max(preds_placebo$GovernmentSupport, na.rm=T)+1), col = "purple", size=1) +
  geom_segment(data=placebos[c(4:5,7,9),], aes(x = date, xend= date, y = min(preds_placebo$GovernmentSupport, na.rm = T)-1, yend = max(preds_placebo$GovernmentSupport, na.rm=T)+1), col = "orange", size=1) +
  scale_x_date(expand = c(0, 0)) +
  theme_minimal()

timeseriesplot_placebo



# looping ----

plot_lst <- list()
lin_regs_lst <- list()
poly_regs_2_lst <- list()
poly_regs_3_lst <- list()

plot_lst_controls <- list()
lin_regs_lst_controls <- list()
poly_regs_2_lst_controls <- list()
poly_regs_3_lst_controls <- list()

plot_lst_resids <- list()
lin_regs_lst_resids <- list()
poly_regs_2_lst_resids <- list()
poly_regs_3_lst_resids <- list()

coef_lst <- list()
coef_plot_lst <- list() 

seg_data <- data.frame()
seg_data_controls <- data.frame()
seg_data_resids <- data.frame()


for (i in (placebos %>% rownames() %>% as.numeric() %>% min):(placebos %>% rownames() %>% as.numeric() %>% max)){
  
  #real obs simple model
  
  df <- data_placebo %>% filter(EndDate >= lubridate::ymd(placebos$date[i])-90& 
                           EndDate <= lubridate::ymd(placebos$date[i])+90 ) %>% 
    mutate(timetotreat = (EndDate - as.Date(placebos$date[i])) %>% as.numeric)
  lm_reg <- lm( GovernmentSupport ~ PolicyIndicator_placebo*timetotreat,
                #poly(Dat_num, 2, raw=T),
                data = df, na.action=na.exclude)
  lin_regs_lst[[i]] <- lm_reg
  
  poly_reg_2 <- lm( GovernmentSupport ~ PolicyIndicator_placebo*poly(timetotreat, 2, raw=T),
                  data = df, na.action=na.exclude)
  poly_regs_2_lst[[i]] <- poly_reg_2
  
  poly_reg_3 <- lm( GovernmentSupport ~ PolicyIndicator_placebo*poly(timetotreat, 3, raw=T),
                    data = df, na.action=na.exclude)
  poly_regs_3_lst[[i]] <- poly_reg_3
  
  df <- bind_cols(df,predict(lm_reg, interval = 'confidence')%>% as.data.frame() %>% rename(fit_lin=fit,lwr_lin=lwr,upr_lin=upr),
                  predict(poly_reg_2, interval= 'confidence') %>% as.data.frame() %>% rename(fit_poly_2=fit,lwr_poly_2=lwr,upr_pol_2=upr),
                  predict(poly_reg_3, interval= 'confidence') %>% as.data.frame() %>% rename(fit_poly_3=fit,lwr_poly_3=lwr,upr_pol_3=upr))

  seg_data <- bind_rows(seg_data,bind_cols(placebos[i,], ymin=min(df$GovernmentSupport,na.rm = T)-1, ymax= max(df$GovernmentSupport,na.rm = T)+1))
  
  lin_cols <- case_when(
    i %in% c(1,3,8) ~ "red",
    i %in% c(2,6) ~ "purple",
    i %in% c(4:5,7,9) ~ "orange"
  )
  
  plot_lst[[i]] <- df %>% ggplot(aes(y=GovernmentSupport, x=EndDate)) + 
    geom_point(col="darkgray") + xlab("time") +
    geom_segment(data=seg_data[i,], aes(x = date, xend= date, y=ymin, yend= ymax), col = lin_cols, size=1) +
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_lin,ymax=upr_lin), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_lin), col="darkred", size=.75) + 
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_lin,ymax=upr_lin), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_lin), col="darkred", size=.75) +
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_poly_2,ymax=upr_pol_2), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_poly_2), col="blue", size=.75) + 
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_poly_2,ymax=upr_pol_2), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_poly_2), col="blue", size=.75) +
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_poly_3,ymax=upr_pol_3), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_poly_3), col="lightblue", size=.75) + 
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_poly_3,ymax=upr_pol_3), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_poly_3), col="lightblue", size=.75) + ylab("without controls") +
    theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())

  #residuals
  
  df_resids <- preds_placebo %>% filter(EndDate >= lubridate::ymd(placebos$date[i])-90& 
                                   EndDate <= lubridate::ymd(placebos$date[i])+90 ) %>% 
    mutate(timetotreat = (EndDate - as.Date(placebos$date[i])) %>% as.numeric)
  
  lm_reg_resids <- lm( residuals ~ PolicyIndicator_placebo*timetotreat,
                       #poly(Dat_num, 2, raw=T),
                       data = df_resids, na.action=na.exclude)
  lin_regs_lst_resids[[i]] <- lm_reg_resids
  
  poly_reg_2_resids <- lm( residuals ~ PolicyIndicator_placebo*poly(timetotreat, 2, raw=T),
                         data = df_resids, na.action=na.exclude)
  poly_regs_2_lst_resids[[i]] <- poly_reg_2_resids
  
  poly_reg_3_resids <- lm( residuals ~ PolicyIndicator_placebo*poly(timetotreat, 3, raw=T),
                         data = df_resids, na.action=na.exclude)
  poly_regs_3_lst_resids[[i]] <- poly_reg_3_resids
  
  df_resids <- bind_cols(
    df_resids,predict(lm_reg_resids, interval = 'confidence')%>% as.data.frame() %>% rename(fit_lin=fit,lwr_lin=lwr,upr_lin=upr),
    predict(poly_reg_2_resids, interval= 'confidence') %>% as.data.frame() %>% rename(fit_poly_2=fit,lwr_poly_2=lwr,upr_pol_2=upr),
    predict(poly_reg_3_resids, interval= 'confidence') %>% as.data.frame() %>% rename(fit_poly_3=fit,lwr_poly_3=lwr,upr_pol_3=upr))
  
  seg_data_resids<- bind_rows(seg_data_resids, bind_cols(placebos[i,], ymin=min(df_resids$residuals,na.rm = T)-1, ymax= max(df_resids$residuals,na.rm = T)+1))
  
  plot_lst_resids[[i]] <- df_resids %>% ggplot(aes(y=residuals, x=EndDate)) + 
    geom_point(col="darkgray") + xlab("time") +
    geom_segment(data=seg_data_resids[i,], aes(x = date, xend= date, y=ymin, yend=ymax), col = lin_cols, size=1) +
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_lin,ymax=upr_lin), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_lin), col="darkred", size=.75) + 
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_lin,ymax=upr_lin), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_lin), col="darkred", size=.75) +
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_poly_2,ymax=upr_pol_2), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_poly_2), col="blue", size=.75) + 
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_poly_2,ymax=upr_pol_2), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_poly_2), col="blue", size=.75) +
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_poly_3,ymax=upr_pol_3), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_poly_3), col="lightblue", size=.75) + 
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_poly_3,ymax=upr_pol_3), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_poly_3), col="lightblue", size=.75) + ylab("residualized") + theme_minimal()
    
  
  coef_lst[[i]] <- 
    bind_rows(
      bind_cols(
        level_change=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[4], sc_se=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[4],
        sc_conf_low= tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[4], sc_conf_high=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[4],
        
        linear = T, polynomial_2 =F,  polynomial_3 =F, residualized =F
      ),
      bind_cols(
        level_change=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[5], sc_se=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[5],
        sc_conf_low= tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[5], sc_conf_high=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[5],
        
        linear = F, polynomial_2 =T,  polynomial_3 =F, residualized =F
      ),
      bind_cols(
        level_change=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[6], sc_se=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[6],
        sc_conf_low= tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[6], sc_conf_high=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[6],
        
        linear = F, polynomial_2 =F, polynomial_3 =T, residualized =F
      ),
      bind_cols(
        level_change=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[4], sc_se=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[4],
        sc_conf_low= tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[4], sc_conf_high=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[4],
        
        linear = T, polynomial_2 =F,  polynomial_3 =F, residualized =T
      ),
      bind_cols(
        level_change=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[5], sc_se=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[5],
        sc_conf_low= tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[5], sc_conf_high=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[5],
        
        linear = F, polynomial_2 =T,  polynomial_3 =F, residualized =T
      ),
      bind_cols(
        level_change=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[6], sc_se=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[6],
        sc_conf_low= tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[6], sc_conf_high=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[6],
        
        linear = F, polynomial_2 =F,  polynomial_3 =T, residualized =T
      )
    
      ) %>% mutate(model=c(1:6))
  
  
  coef_plot_lst[[i]] <- cowplot::plot_grid(
    cowplot::ggdraw() + cowplot::draw_label(
      "coefficients",
      x = 0,
      hjust = 0
    ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      ),  
    ggplot(coef_lst[[i]], aes(y=level_change, x=model))+
      geom_point() +
      geom_pointrange(aes(ymin = lc_conf_low, ymax = lc_conf_high)) +
      # labs(title = "Coefficients of a linear regression model") + 
      geom_hline(yintercept=0, linetype="dashed", color = "darkgray", size=1.5) +
      theme_bw() + theme(axis.text.x=element_blank(),axis.title.x=element_blank(), 
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         axis.ticks = element_blank()),
    
    ggplot(coef_lst[[i]], aes(y=slope_change, x=model))+
      geom_point() +
      geom_pointrange(aes(ymin = sc_conf_low, ymax = sc_conf_high)) +
      # labs(title = "Coefficients of a linear regression model") + 
      geom_hline(yintercept=0, linetype="dashed", color = "darkgray", size=1.5) +
      theme_bw() + theme(axis.text.x=element_blank(),axis.title.x=element_blank(),  
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         axis.ticks = element_blank()),
    
    cowplot::ggdraw() + cowplot::draw_label(
      "model specifications",
      x = 0,
      hjust = 0
    ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      ),
    
    ggplot(coef_lst[[i]]) +
      geom_point(aes( y="residualized", x= model, color = residualized, size=.2)) +
      geom_point(aes( y="polynomial_2", x= model, color = polynomial_2, size=.2)) +
      geom_point(aes( y="polynomial_3", x= model, color = polynomial_3, size=.2)) +
      geom_point(aes( y="linear", x= model, color = linear, size=.2)) +  
      theme_minimal() + scale_color_manual(values = c("TRUE" = "darkgray", "FALSE" = "white"))+
      theme(legend.position="none",axis.text.x=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),
            # panel.grid.major.x = element_blank(),  panel.grid.major.y = element_blank(), 
            panel.grid.minor.x = element_blank()),
    ncol = 1, align = "v", axis = "lr", rel_heights = c(.2,2,2,.2,1)#, 
    #labels = c("coefficients","specifications"), label_size = 10
  )
  
}

plot_placebo <- cowplot::plot_grid(timeseriesplot_placebo, 
                            # cowplot::plot_grid(plotlist =plot_lst_controls,nrow = 1),
                            cowplot::plot_grid(
                              cowplot::plot_grid(plotlist =plot_lst,nrow = 1), 
                              cowplot::plot_grid(plotlist =plot_lst_resids,nrow = 1),
                              nrow=2, align = "v", axis = "lr"
                            ),
                            cowplot::plot_grid(plotlist = coef_plot_lst, nrow=1),
                            labels = c('a)', 'b)','c)'),
                            nrow=3 )
plot_placebo
ggsave(filename="period_placebo.pdf", path=here("analysis/plots/"), width = 57, height = 35, units="cm")

# lapply(lin_regs_lst, summary)
# lapply(lin_regs_lst_controls, summary)
# lapply(lin_regs_lst_resids, summary)

=======
library(tidyverse)
library(here)
library(sandwich)
library(lmtest)
require(broom)


# load data
load(file=paste0(here(),"/data/VoterSupportData.RData"))
load(file=paste0(here(),"/data/ClimatePolicies.RData"))
load(file=paste0(here(),"/data/Elections.RData"))

# Analysis ----

## a) full period time series 

### data

placebos <- bind_cols( placebo = as_factor(c("election2002", "election2006", "election2010", "terrorNO", "riotsSTH", "election2014", "terrorSTH", "election2018", "corona")), date = lubridate::as_date(c(elections$date[2], elections$date[3], elections$date[4], "2011-07-22", "2013-05-19", elections$date[5], "2017-04-07", elections$date[6], "2020-03-10")))



data_placebo <- VoterSupportData %>% 
  dplyr::select(
    Publicerad,
    GovernmentSupport,
    Unemployment,
    ConsumerPriceIndex,
    HouseholdConsumption,
    MiljöpartietMP,
    ConsumerConfidence,
    GovtSearch,
    above_published,
    PolicyIndicator,
    Date_numeric,
    GovernmentSupport_monthlyMean,
    GovernmentSupport_monthlyMean_deviation,
    IncumbentParty,
    PollingInstitute,
    StartCollection,
    EndCollection
  ) %>% mutate(above_published_placebo = case_when(`Publicerad` < as.Date(placebos$date[1])~0,
                                                   `Publicerad` > as.Date(placebos$date[1]) & `Publicerad` < as.Date(placebos$date[2]) ~ 1,
                                                   `Publicerad` > as.Date(placebos$date[2]) & `Publicerad` < as.Date(placebos$date[3]) ~ 2,
                                                   `Publicerad` > as.Date(placebos$date[3]) & `Publicerad` < as.Date(placebos$date[4]) ~ 3,
                                                   `Publicerad` > as.Date(placebos$date[4]) & `Publicerad` < as.Date(placebos$date[5]) ~ 4,
                                                   `Publicerad` > as.Date(placebos$date[5]) & `Publicerad` < as.Date(placebos$date[6]) ~ 5,
                                                   `Publicerad` > as.Date(placebos$date[6]) & `Publicerad` < as.Date(placebos$date[7]) ~ 6,
                                                   `Publicerad` > as.Date(placebos$date[7]) & `Publicerad` < as.Date(placebos$date[8]) ~ 7,
                                                   `Publicerad` > as.Date(placebos$date[8]) & `Publicerad` < as.Date(placebos$date[9]) ~ 8,
                                                   `Publicerad` > as.Date(placebos$date[9]) ~ 9) %>% as.factor(),
               PolicyIndicator_placebo = case_when(`EndDate` < as.Date(placebos$date[1])~0,
                                                   `StartCollection` > as.Date(placebos$date[1]) & `EndDate` < as.Date(placebos$date[2]) ~ 1,
                                                   `StartCollection` > as.Date(placebos$date[2]) & `EndDate` < as.Date(placebos$date[3]) ~ 2,
                                                   `StartCollection` > as.Date(placebos$date[3]) & `EndDate` < as.Date(placebos$date[4]) ~ 3,
                                                   `StartCollection` > as.Date(placebos$date[4]) & `EndDate` < as.Date(placebos$date[5]) ~ 4,
                                                   `StartCollection` > as.Date(placebos$date[5]) & `EndDate` < as.Date(placebos$date[6]) ~ 5,
                                                   `StartCollection` > as.Date(placebos$date[6]) & `EndDate` < as.Date(placebos$date[7]) ~ 6,
                                                   `StartCollection` > as.Date(placebos$date[7]) & `EndDate` < as.Date(placebos$date[8]) ~ 7,
                                                   `StartCollection` > as.Date(placebos$date[8]) & `EndDate` < as.Date(placebos$date[9]) ~ 8,
                                                   `StartCollection` > as.Date(placebos$date[9]) ~ 9) %>% as.factor(),
               StartCollection = StartCollection %>% as.Date(),
               EndDate = EndDate %>% as.Date()
               )


# remove period outlier
data_placebo <- data_placebo %>% filter(Date!="2002-10-01")


### model 
lm_placebo_controls <- lm( GovernmentSupport ~ -1 + #PolicyIndicator_placebo*Date_numeric +
                          lag(GovernmentSupport,1) +
                          lag(GovernmentSupport,2) +
                          lag(GovernmentSupport,3) +
                          Unemployment + 
                          ConsumerPriceIndex  +
                          HouseholdConsumption +
                          ConsumerConfidence +
                          as.factor(PollingInstitute) 
                        ,
                        data = data_placebo %>% as.data.frame(), na.action=na.exclude); summary(lm_placebo_controls)

step.placebomodel.didit <- MASS::stepAIC(lm_placebo_controls, direction = "forward", 
                                         trace = FALSE)
summary(step.placebomodel.didit)
coeftest(step.placebomodel.didit, vcov = vcovPL(step.placebomodel.didit))
preds_placebo <- bind_cols(data_placebo,predict(step.placebomodel.didit, interval = 'confidence')%>% as.data.frame(), residuals = resid(step.placebomodel.didit, na.action=na.exclude))


# plotting

timeseriesplot_placebo <- preds_placebo %>% ggplot(aes(y=GovernmentSupport, x=EndDate)) +
  geom_point(col="darkgray") +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==0), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==0), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==1), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==1), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==2), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==2),aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==3), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==3), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==4), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==4), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==5), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==5), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==6), aes(y=fit, x=EndDate), col="darkred") + 
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==6), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==7), aes(y=fit, x=EndDate), col="darkred") +
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==7), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==8), aes(y=fit, x=EndDate), col="darkred") +
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==8), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  geom_line(data = preds_placebo %>% filter(PolicyIndicator_placebo==9), aes(y=fit, x=EndDate), col="darkred") +
  geom_ribbon(data = preds_placebo %>% filter(PolicyIndicator_placebo==9), aes(ymin=lwr,ymax=upr), alpha=0.3) +
  xlab("time") +
  geom_segment(data=placebos[c(1,3,8),], aes(x = date, xend= date, y = min(preds_placebo$GovernmentSupport, na.rm = T)-1, yend = max(preds_placebo$GovernmentSupport, na.rm=T)+1), col = "red", size=1) +
  geom_segment(data=placebos[c(2,6),], aes(x = date, xend= date, y = min(preds_placebo$GovernmentSupport, na.rm = T)-1, yend = max(preds_placebo$GovernmentSupport, na.rm=T)+1), col = "purple", size=1) +
  geom_segment(data=placebos[c(4:5,7,9),], aes(x = date, xend= date, y = min(preds_placebo$GovernmentSupport, na.rm = T)-1, yend = max(preds_placebo$GovernmentSupport, na.rm=T)+1), col = "orange", size=1) +
  scale_x_date(expand = c(0, 0)) +
  theme_minimal()

timeseriesplot_placebo



# looping ----

plot_lst <- list()
lin_regs_lst <- list()
poly_regs_2_lst <- list()
poly_regs_3_lst <- list()

plot_lst_controls <- list()
lin_regs_lst_controls <- list()
poly_regs_2_lst_controls <- list()
poly_regs_3_lst_controls <- list()

plot_lst_resids <- list()
lin_regs_lst_resids <- list()
poly_regs_2_lst_resids <- list()
poly_regs_3_lst_resids <- list()

coef_lst <- list()
coef_plot_lst <- list() 

seg_data <- data.frame()
seg_data_controls <- data.frame()
seg_data_resids <- data.frame()


for (i in (placebos %>% rownames() %>% as.numeric() %>% min):(placebos %>% rownames() %>% as.numeric() %>% max)){
  
  #real obs simple model
  
  df <- data_placebo %>% filter(EndDate >= lubridate::ymd(placebos$date[i])-90& 
                           EndDate <= lubridate::ymd(placebos$date[i])+90 ) %>% 
    mutate(timetotreat = (EndDate - as.Date(placebos$date[i])) %>% as.numeric)
  lm_reg <- lm( GovernmentSupport ~ PolicyIndicator_placebo*timetotreat,
                #poly(Dat_num, 2, raw=T),
                data = df, na.action=na.exclude)
  lin_regs_lst[[i]] <- lm_reg
  
  poly_reg_2 <- lm( GovernmentSupport ~ PolicyIndicator_placebo*poly(timetotreat, 2, raw=T),
                  data = df, na.action=na.exclude)
  poly_regs_2_lst[[i]] <- poly_reg_2
  
  poly_reg_3 <- lm( GovernmentSupport ~ PolicyIndicator_placebo*poly(timetotreat, 3, raw=T),
                    data = df, na.action=na.exclude)
  poly_regs_3_lst[[i]] <- poly_reg_3
  
  df <- bind_cols(df,predict(lm_reg, interval = 'confidence')%>% as.data.frame() %>% rename(fit_lin=fit,lwr_lin=lwr,upr_lin=upr),
                  predict(poly_reg_2, interval= 'confidence') %>% as.data.frame() %>% rename(fit_poly_2=fit,lwr_poly_2=lwr,upr_pol_2=upr),
                  predict(poly_reg_3, interval= 'confidence') %>% as.data.frame() %>% rename(fit_poly_3=fit,lwr_poly_3=lwr,upr_pol_3=upr))

  seg_data <- bind_rows(seg_data,bind_cols(placebos[i,], ymin=min(df$GovernmentSupport,na.rm = T)-1, ymax= max(df$GovernmentSupport,na.rm = T)+1))
  
  lin_cols <- case_when(
    i %in% c(1,3,8) ~ "red",
    i %in% c(2,6) ~ "purple",
    i %in% c(4:5,7,9) ~ "orange"
  )
  
  plot_lst[[i]] <- df %>% ggplot(aes(y=GovernmentSupport, x=EndDate)) + 
    geom_point(col="darkgray") + xlab("time") +
    geom_segment(data=seg_data[i,], aes(x = date, xend= date, y=ymin, yend= ymax), col = lin_cols, size=1) +
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_lin,ymax=upr_lin), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_lin), col="darkred", size=.75) + 
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_lin,ymax=upr_lin), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_lin), col="darkred", size=.75) +
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_poly_2,ymax=upr_pol_2), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_poly_2), col="blue", size=.75) + 
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_poly_2,ymax=upr_pol_2), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_poly_2), col="blue", size=.75) +
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_poly_3,ymax=upr_pol_3), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_poly_3), col="lightblue", size=.75) + 
    geom_ribbon(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_poly_3,ymax=upr_pol_3), alpha=0.2) +
    geom_line(data=df %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_poly_3), col="lightblue", size=.75) + ylab("without controls") +
    theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())

  #residuals
  
  df_resids <- preds_placebo %>% filter(EndDate >= lubridate::ymd(placebos$date[i])-90& 
                                   EndDate <= lubridate::ymd(placebos$date[i])+90 ) %>% 
    mutate(timetotreat = (EndDate - as.Date(placebos$date[i])) %>% as.numeric)
  
  lm_reg_resids <- lm( residuals ~ PolicyIndicator_placebo*timetotreat,
                       #poly(Dat_num, 2, raw=T),
                       data = df_resids, na.action=na.exclude)
  lin_regs_lst_resids[[i]] <- lm_reg_resids
  
  poly_reg_2_resids <- lm( residuals ~ PolicyIndicator_placebo*poly(timetotreat, 2, raw=T),
                         data = df_resids, na.action=na.exclude)
  poly_regs_2_lst_resids[[i]] <- poly_reg_2_resids
  
  poly_reg_3_resids <- lm( residuals ~ PolicyIndicator_placebo*poly(timetotreat, 3, raw=T),
                         data = df_resids, na.action=na.exclude)
  poly_regs_3_lst_resids[[i]] <- poly_reg_3_resids
  
  df_resids <- bind_cols(
    df_resids,predict(lm_reg_resids, interval = 'confidence')%>% as.data.frame() %>% rename(fit_lin=fit,lwr_lin=lwr,upr_lin=upr),
    predict(poly_reg_2_resids, interval= 'confidence') %>% as.data.frame() %>% rename(fit_poly_2=fit,lwr_poly_2=lwr,upr_pol_2=upr),
    predict(poly_reg_3_resids, interval= 'confidence') %>% as.data.frame() %>% rename(fit_poly_3=fit,lwr_poly_3=lwr,upr_pol_3=upr))
  
  seg_data_resids<- bind_rows(seg_data_resids, bind_cols(placebos[i,], ymin=min(df_resids$residuals,na.rm = T)-1, ymax= max(df_resids$residuals,na.rm = T)+1))
  
  plot_lst_resids[[i]] <- df_resids %>% ggplot(aes(y=residuals, x=EndDate)) + 
    geom_point(col="darkgray") + xlab("time") +
    geom_segment(data=seg_data_resids[i,], aes(x = date, xend= date, y=ymin, yend=ymax), col = lin_cols, size=1) +
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_lin,ymax=upr_lin), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_lin), col="darkred", size=.75) + 
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_lin,ymax=upr_lin), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_lin), col="darkred", size=.75) +
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_poly_2,ymax=upr_pol_2), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_poly_2), col="blue", size=.75) + 
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_poly_2,ymax=upr_pol_2), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_poly_2), col="blue", size=.75) +
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(ymin=lwr_poly_3,ymax=upr_pol_3), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric < i), aes(EndDate, fit_poly_3), col="lightblue", size=.75) + 
    geom_ribbon(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(ymin=lwr_poly_3,ymax=upr_pol_3), alpha=0.2) +
    geom_line(data=df_resids %>% filter(PolicyIndicator_placebo %>% as.character() %>% as.numeric == i), aes(EndDate, fit_poly_3), col="lightblue", size=.75) + ylab("residualized") + theme_minimal()
    
  
  coef_lst[[i]] <- 
    bind_rows(
      bind_cols(
        level_change=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[4], sc_se=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[4],
        sc_conf_low= tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[4], sc_conf_high=tidy(coeftest(lin_regs_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[4],
        
        linear = T, polynomial_2 =F,  polynomial_3 =F, residualized =F
      ),
      bind_cols(
        level_change=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[5], sc_se=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[5],
        sc_conf_low= tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[5], sc_conf_high=tidy(coeftest(poly_regs_2_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[5],
        
        linear = F, polynomial_2 =T,  polynomial_3 =F, residualized =F
      ),
      bind_cols(
        level_change=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[6], sc_se=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[6],
        sc_conf_low= tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[6], sc_conf_high=tidy(coeftest(poly_regs_3_lst[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[6],
        
        linear = F, polynomial_2 =F, polynomial_3 =T, residualized =F
      ),
      bind_cols(
        level_change=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[4], sc_se=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[4],
        sc_conf_low= tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[4], sc_conf_high=tidy(coeftest(lin_regs_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[4],
        
        linear = T, polynomial_2 =F,  polynomial_3 =F, residualized =T
      ),
      bind_cols(
        level_change=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[5], sc_se=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[5],
        sc_conf_low= tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[5], sc_conf_high=tidy(coeftest(poly_regs_2_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[5],
        
        linear = F, polynomial_2 =T,  polynomial_3 =F, residualized =T
      ),
      bind_cols(
        level_change=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[2], lc_se=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[2], 
        lc_conf_low= tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[2], lc_conf_high=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[2],
        
        slope_change=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$estimate[6], sc_se=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$`std.error`[6],
        sc_conf_low= tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL),conf.int = TRUE)$conf.low[6], sc_conf_high=tidy(coeftest(poly_regs_3_lst_resids[[i]], vcov = vcovPL, level = 0.90),conf.int = TRUE)$conf.high[6],
        
        linear = F, polynomial_2 =F,  polynomial_3 =T, residualized =T
      )
    
      ) %>% mutate(model=c(1:6))
  
  
  coef_plot_lst[[i]] <- cowplot::plot_grid(
    cowplot::ggdraw() + cowplot::draw_label(
      "coefficients",
      x = 0,
      hjust = 0
    ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      ),  
    ggplot(coef_lst[[i]], aes(y=level_change, x=model))+
      geom_point() +
      geom_pointrange(aes(ymin = lc_conf_low, ymax = lc_conf_high)) +
      # labs(title = "Coefficients of a linear regression model") + 
      geom_hline(yintercept=0, linetype="dashed", color = "darkgray", size=1.5) +
      theme_bw() + theme(axis.text.x=element_blank(),axis.title.x=element_blank(), 
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         axis.ticks = element_blank()),
    
    ggplot(coef_lst[[i]], aes(y=slope_change, x=model))+
      geom_point() +
      geom_pointrange(aes(ymin = sc_conf_low, ymax = sc_conf_high)) +
      # labs(title = "Coefficients of a linear regression model") + 
      geom_hline(yintercept=0, linetype="dashed", color = "darkgray", size=1.5) +
      theme_bw() + theme(axis.text.x=element_blank(),axis.title.x=element_blank(),  
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         axis.ticks = element_blank()),
    
    cowplot::ggdraw() + cowplot::draw_label(
      "model specifications",
      x = 0,
      hjust = 0
    ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      ),
    
    ggplot(coef_lst[[i]]) +
      geom_point(aes( y="residualized", x= model, color = residualized, size=.2)) +
      geom_point(aes( y="polynomial_2", x= model, color = polynomial_2, size=.2)) +
      geom_point(aes( y="polynomial_3", x= model, color = polynomial_3, size=.2)) +
      geom_point(aes( y="linear", x= model, color = linear, size=.2)) +  
      theme_minimal() + scale_color_manual(values = c("TRUE" = "darkgray", "FALSE" = "white"))+
      theme(legend.position="none",axis.text.x=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),
            # panel.grid.major.x = element_blank(),  panel.grid.major.y = element_blank(), 
            panel.grid.minor.x = element_blank()),
    ncol = 1, align = "v", axis = "lr", rel_heights = c(.2,2,2,.2,1)#, 
    #labels = c("coefficients","specifications"), label_size = 10
  )
  
}

plot_placebo <- cowplot::plot_grid(timeseriesplot_placebo, 
                            # cowplot::plot_grid(plotlist =plot_lst_controls,nrow = 1),
                            cowplot::plot_grid(
                              cowplot::plot_grid(plotlist =plot_lst,nrow = 1), 
                              cowplot::plot_grid(plotlist =plot_lst_resids,nrow = 1),
                              nrow=2, align = "v", axis = "lr"
                            ),
                            cowplot::plot_grid(plotlist = coef_plot_lst, nrow=1),
                            labels = c('a)', 'b)','c)'),
                            nrow=3 )
plot_placebo
ggsave(filename="period_placebo.pdf", path=here("analysis/plots/"), width = 57, height = 35, units="cm")

# lapply(lin_regs_lst, summary)
# lapply(lin_regs_lst_controls, summary)
# lapply(lin_regs_lst_resids, summary)

>>>>>>> 972e08ac31bc84d037c526d404f79af5d910a244
