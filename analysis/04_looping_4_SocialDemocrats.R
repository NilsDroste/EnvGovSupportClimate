<<<<<<< HEAD
library(tidyverse)
library(here)
library(sandwich)
library(lmtest)
require(broom)

# load data
load(file=paste0(here(),"/data/VoterSupportData.RData"))
load(file=paste0(here(),"/data/ClimatePolicies.RData"))


# Analysis ----

## full period time series model without interventions for later residualization
### data
data4_socialDemocrats <- VoterSupportData %>%
  filter(Published >= elections$date[5] &
           Published <= elections$date[6]) %>%
  dplyr::select(
    Published,
    GovernmentSupport,
    SocialdemokraternaS,
    Unemployment,
    ConsumerPriceIndex,
    HouseholdConsumption,
    SocialdemokraternaS,
    ConsumerConfidence,
    GovtSearch,
    PolicyIndicator,
    Date_numeric,
    PollingInstitute,
    StartCollection,
    EndCollection
  ) %>% mutate(StartCollection = StartCollection %>% as.Date(),
               EndCollection = EndCollection %>% as.Date())

# fixing that there are only very few observations for periods of env policy 10 and 14
data4_socialDemocrats  <-
  data4_socialDemocrats  %>% mutate(
    PolicyIndicator = na_if(PolicyIndicator, 10),
    PolicyIndicator = na_if(PolicyIndicator, 14),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                11, 10),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                12, 11),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                13, 12),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                15, 13),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                16, 14),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                17, 15)
  )



### model
lm4_didit_socialDemocrats <- lm(
  SocialdemokraternaS ~ #PolicyIndicator*Date_numeric +
    lag(GovernmentSupport, 1) +
    lag(GovernmentSupport, 2) +
    lag(GovernmentSupport, 3) +
    lag(GovernmentSupport, 4) +
    lag(GovernmentSupport, 5) +
    lag(GovernmentSupport, 6) +
    Unemployment +
    ConsumerPriceIndex  +
    HouseholdConsumption +
    GovtSearch +
    # SocialdemokraternaS +
    ConsumerConfidence +
    as.factor(PollingInstitute)
  ,
  data = data4_socialDemocrats  %>% as.data.frame(),
  na.action = na.exclude
)
summary(lm4_didit_socialDemocrats)
step.model4.didit_socialDemocrats <-
  MASS::stepAIC(lm4_didit_socialDemocrats, direction = "forward",
                trace = FALSE)
summary(step.model4.didit_socialDemocrats)
coeftest(step.model4.didit_socialDemocrats, vcov = vcovPL(step.model4.didit))
preds4_socialDemocrats <-
  bind_cols(
    data4_socialDemocrats ,
    predict(step.model4.didit_socialDemocrats, interval = 'confidence') %>% as.data.frame(),
    residuals = resid(step.model4.didit_socialDemocrats, na.action = na.exclude)
  )

# plotting

timeseriesplot4_socialDemocrats <- preds4_socialDemocrats  %>% #filter(!is.na(PolicyIndicator)) %>%
  ggplot(aes(y = SocialdemokraternaS, x = EndCollection)) +
  geom_point(col = "darkgray") +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 8),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 8),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 9),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 9),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 10),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 10),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 11),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 11),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 12),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 12),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 13),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 13),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 14),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 14),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 15),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 15),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 16),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 16),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 17),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 17),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  xlab("time") +
  geom_segment(
    data = envpols[c(9:17), ],
    aes(
      x = date,
      xend = date,
      y = 20,
      yend = 35
    ),
    col = "green4",
    size = 1
  ) +
  annotate(
    "rect",
    xmin = envpols[10, ]$date,
    xmax = envpols[11, ]$date,
    ymin = 20,
    ymax = 35,
    fill = "green4",
    alpha = 0.25
  ) +
  annotate(
    "rect",
    xmin = envpols[14, ]$date,
    xmax = envpols[15, ]$date,
    ymin = 20,
    ymax = 35,
    fill = "green4",
    alpha = 0.25
  ) +
  scale_x_date(limits = c(elections$date[5], elections$date[6]),
               expand = c(0, 0)) + theme_minimal()

timeseriesplot4_socialDemocrats


# looping over single interventions ----

plot_lst4_socialDemocrats <- list()
lin_regs_lst4_socialDemocrats <- list()
poly_regs_lst4_socialDemocrats <- list()

plot_lst4_socialDemocrats_resids <- list()
lin_regs_lst4_socialDemocrats_resids <- list()
poly_regs_lst4_socialDemocrats_resids <- list()

coef_lst4_socialDemocrats <- list()
coef_plot_lst4_socialDemocrats <- list()

#

j = data4_socialDemocrats  %>% pull(PolicyIndicator) %>% as.numeric() %>% min(na.rm = T)


for (i in (envpols[c(9, 11:13, 15:17), ] %>% rownames() %>% as.numeric() %>% min):(envpols[c(9, 11:13, 15:17), ] %>% rownames() %>% as.numeric() %>% max)) {
  #real obs simple model
  
  df_socialDemocrats <-
    data4_socialDemocrats  %>% filter(
      StartCollection >= lubridate::ymd(envpols[c(9, 11:13, 15:17), ]$date[i]) - 90 &
        EndCollection <= lubridate::ymd(envpols[c(9, 11:13, 15:17), ]$date[i]) +
        90
    ) %>%
    mutate(timetotreat = (EndCollection - as.Date(envpols[c(9, 11:13, 15:17), ]$date[i])) %>% as.numeric)
  lm_reg <- lm(
    SocialdemokraternaS ~ PolicyIndicator * timetotreat,
    #poly(Dat_num, 2, raw=T),
    data = df_socialDemocrats,
    na.action = na.exclude
  )
  lin_regs_lst4_socialDemocrats[[i]] <- lm_reg
  
  poly_reg <-
    lm(
      SocialdemokraternaS ~ PolicyIndicator * poly(timetotreat, 2, raw = T),
      data = df_socialDemocrats,
      na.action = na.exclude
    )
  poly_regs_lst4_socialDemocrats[[i]] <- poly_reg
  
  df_socialDemocrats <-
    bind_cols(
      df_socialDemocrats,
      predict(lm_reg, interval = 'confidence') %>% as.data.frame() %>% rename(
        fit_lin = fit,
        lwr_lin = lwr,
        upr_lin = upr
      ),
      predict(poly_reg, interval = 'confidence') %>% as.data.frame() %>% rename(
        fit_poly = fit,
        lwr_poly = lwr,
        upr_pol = upr
      )
    )
  
  
  plot_lst4_socialDemocrats[[i]] <- df_socialDemocrats %>% filter(!is.na(PolicyIndicator))  %>%
    ggplot(aes(y = SocialdemokraternaS, x = EndCollection)) +
    geom_point(col = "darkgray") + xlab("time") +
    geom_segment(
      data = envpols[c(9, 11:13, 15:17), ][i, ],
      aes(
        x = date,
        xend = date,
        y = 20,
        yend = 35
      ),
      col = "green4",
      size = 1
    ) +
    geom_ribbon(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) +
    geom_ribbon(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) + ylab("without controls") +
    theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  
  if (i == 2) {
    plot_lst4_socialDemocrats[[i]] <- df_socialDemocrats %>% # filter(!is.na(PolicyIndicator)) %>%
      ggplot(aes(y = SocialdemokraternaS, x = EndCollection)) +
      geom_point(col = "darkgray") + xlab("time") +
      geom_segment(
        data = envpols[10, ],
        aes(
          x = date,
          xend = date,
          y = 20,
          yend = 35
        ),
        col = "green4",
        size = 1
      ) +
      geom_segment(
        data = envpols[c(9, 11:13, 15:17), ][i, ],
        aes(
          x = date,
          xend = date,
          y = 20,
          yend = 35
        ),
        col = "green4",
        size = 1
      ) +
      annotate(
        "rect",
        xmin = envpols[10, ]$date,
        xmax = envpols[11, ]$date,
        ymin = 20,
        ymax = 35,
        fill = "green4",
        alpha = 0.25
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) + ylab("without controls") +
      theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  if (i == 5) {
    plot_lst4_socialDemocrats[[i]] <- df_socialDemocrats %>% # filter(!is.na(PolicyIndicator)) %>%
      ggplot(aes(y = SocialdemokraternaS, x = EndCollection)) +
      geom_point(col = "darkgray") + xlab("time") +
      geom_segment(
        data = envpols[14, ],
        aes(
          x = date,
          xend = date,
          y = 20,
          yend = 35
        ),
        col = "green4",
        size = 1
      ) +
      geom_segment(
        data = envpols[c(9, 11:13, 15:17), ][i, ],
        aes(
          x = date,
          xend = date,
          y = 20,
          yend = 35
        ),
        col = "green4",
        size = 1
      ) +
      annotate(
        "rect",
        xmin = envpols[14, ]$date,
        xmax = envpols[15, ]$date,
        ymin = 20,
        ymax = 35,
        fill = "green4",
        alpha = 0.25
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) + ylab("without controls") +
      theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  # residuals
  
  df_resids_socialDemocrats <-
    preds4_socialDemocrats %>% filter(
      StartCollection >= lubridate::ymd(envpols[c(9, 11:13, 15:17), ]$date[i]) - 90 &
        EndCollection <= lubridate::ymd(envpols[c(9, 11:13, 15:17), ]$date[i]) +
        90
    ) %>%
    mutate(timetotreat = (EndCollection - as.Date(envpols[c(9, 11:13, 15:17), ]$date[i])) %>% as.numeric)
  
  lm_reg_resids <- lm(residuals ~ PolicyIndicator * timetotreat,
                      #poly(Dat_num, 2, raw=T),
                      data = df_resids_socialDemocrats,
                      na.action = na.exclude)
  lin_regs_lst4_socialDemocrats_resids[[i]] <- lm_reg_resids
  
  poly_reg_resids <-
    lm(
      residuals ~ PolicyIndicator * poly(timetotreat, 2, raw = T),
      data = df_resids_socialDemocrats,
      na.action = na.exclude
    )
  poly_regs_lst4_socialDemocrats_resids[[i]] <- poly_reg_resids
  
  df_resids_socialDemocrats <- bind_cols(
    df_resids_socialDemocrats,
    predict(lm_reg_resids, interval = 'confidence') %>% as.data.frame() %>% rename(
      fit_lin = fit,
      lwr_lin = lwr,
      upr_lin = upr
    ),
    predict(poly_reg_resids, interval = 'confidence') %>% as.data.frame() %>% rename(
      fit_poly = fit,
      lwr_poly = lwr,
      upr_pol = upr
    )
  )
  
  
  plot_lst4_socialDemocrats_resids[[i]] <-
    df_resids_socialDemocrats  %>% filter(!is.na(PolicyIndicator)) %>% ggplot(aes(y = residuals, x =
                                                                           EndCollection)) +
    geom_point(col = "darkgray") + xlab("time") +
    geom_segment(
      data = envpols[c(9, 11:13, 15:17), ][i, ],
      aes(
        x = date,
        xend = date,
        y = -6,
        yend = 5
      ),
      col = "green4",
      size = 1
    ) +
    geom_ribbon(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) +
    geom_ribbon(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) + ylab("residualized") + theme_minimal()
  
  if (i == 2) {
    plot_lst4_socialDemocrats_resids[[i]] <-
      df_resids_socialDemocrats %>% # filter(!is.na(PolicyIndicator)) %>%
      ggplot(aes(y = residuals, x = EndCollection)) +
      geom_point(col = "darkgray") + xlab("time") +
      geom_segment(
        data = envpols[10, ],
        aes(
          x = date,
          xend = date,
          y = -6,
          yend = 5
        ),
        col = "green4",
        size = 1
      ) +
      geom_segment(
        data = envpols[c(9, 11:13, 15:17), ][i, ],
        aes(
          x = date,
          xend = date,
          y = -6,
          yend = 5
        ),
        col = "green4",
        size = 1
      ) +
      annotate(
        "rect",
        xmin = envpols[10, ]$date,
        xmax = envpols[11, ]$date,
        ymin = -6,
        ymax = 5,
        fill = "green4",
        alpha = 0.25
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) + ylab("without controls") +
      theme_minimal()
  }
  
  if (i == 5) {
    plot_lst4_socialDemocrats_resids[[i]] <-
      df_resids_socialDemocrats %>% # filter(!is.na(PolicyIndicator)) %>%
      ggplot(aes(y = residuals, x = EndCollection)) +
      geom_point(col = "darkgray") + xlab("time") +
      geom_segment(
        data = envpols[14, ],
        aes(
          x = date,
          xend = date,
          y = -6,
          yend = 5
        ),
        col = "green4",
        size = 1
      ) +
      geom_segment(
        data = envpols[c(9, 11:13, 15:17), ][i, ],
        aes(
          x = date,
          xend = date,
          y = -6,
          yend = 5
        ),
        col = "green4",
        size = 1
      ) +
      annotate(
        "rect",
        xmin = envpols[14, ]$date,
        xmax = envpols[15, ]$date,
        ymin = -6,
        ymax = 5,
        fill = "green4",
        alpha = 0.25
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) + ylab("without controls") +
      theme_minimal() 
  }
  
  coef_lst4_socialDemocrats[[i]] <-
    bind_rows(
      bind_cols(
        level_change = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[4],
        sc_se = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[4],
        sc_conf_low = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[4],
        sc_conf_high = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[4],
        
        linear = T,
        polynomial = F,
        residualized = F
      ),
      bind_cols(
        level_change = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[5],
        sc_se = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[5],
        sc_conf_low = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[5],
        sc_conf_high = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[5],
        
        linear = F,
        polynomial = T,
        residualized = F
      ),
      bind_cols(
        level_change = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[4],
        sc_se = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[4],
        sc_conf_low = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[4],
        sc_conf_high = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[4],
        
        linear = T,
        polynomial = F,
        residualized = T
      ),
      bind_cols(
        level_change = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[5],
        sc_se = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[5],
        sc_conf_low = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[5],
        sc_conf_high = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[5],
        
        linear = F,
        polynomial = T,
        residualized = T
      )
    ) %>% mutate(model = c(1:4))
  
  
  coef_plot_lst4_socialDemocrats[[i]] <- cowplot::plot_grid(
    cowplot::ggdraw() + cowplot::draw_label("coefficients",
                                            x = 0,
                                            hjust = 0) +
      theme(# add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)),
    ggplot(coef_lst4_socialDemocrats[[i]], aes(y = level_change, x = model)) +
      geom_point() +
      geom_pointrange(aes(ymin = lc_conf_low, ymax = lc_conf_high)) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "darkgray",
        size = 1.5
      ) +
      theme_bw() + theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank()
      ),
    
    ggplot(coef_lst4_socialDemocrats[[i]], aes(y = slope_change, x = model)) +
      geom_point() +
      geom_pointrange(aes(ymin = sc_conf_low, ymax = sc_conf_high)) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "darkgray",
        size = 1.5
      ) +
      theme_bw() + theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank()
      ),
    
    cowplot::ggdraw() + cowplot::draw_label("model specifications",
                                            x = 0,
                                            hjust = 0) +
      theme(# add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)),
    
    ggplot(coef_lst4_socialDemocrats[[i]]) +
      geom_point(
        aes(
          y = "residualized",
          x = model,
          color = residualized,
          size = .25
        )
      ) +
      geom_point(aes(
        y = "polynomial",
        x = model,
        color = polynomial,
        size = .25
      )) +
      geom_point(aes(
        y = "linear",
        x = model,
        color = linear,
        size = .25
      )) +
      theme_minimal() + scale_color_manual(values = c(
        "TRUE" = "darkgray", "FALSE" = "white"
      )) +
      theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank()
      ),
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(.2, 2, 2, .2, 1)#,
  )
  j = j + 1
}


plot4_socialDemocrats <- cowplot::plot_grid(
  timeseriesplot4_socialDemocrats,
  # cowplot::plot_grid(plotlist =plot_lst4_socialDemocrats_controls,nrow = 1),
  cowplot::plot_grid(
    cowplot::plot_grid(plotlist = plot_lst4_socialDemocrats, nrow = 1),
    cowplot::plot_grid(plotlist = plot_lst4_socialDemocrats_resids, nrow = 1),
    nrow = 2,
    align = "v",
    axis = "lr"
  ),
  cowplot::plot_grid(plotlist = coef_plot_lst4_socialDemocrats, nrow =
                       1),
  labels = c('a)', 'b)', 'c)'),
  nrow = 3
)

plot4_socialDemocrats
ggsave(
  filename = "period4_socialDemocrats.pdf",
  path = here("analysis/plots/"),
  width = 57,
  height = 35,
  units = "cm"
)


# lapply(lin_regs_lst4_socialDemocrats, summary)
# lapply(lin_regs_lst4_socialDemocrats_controls, summary)
# lapply(lin_regs_lst4_socialDemocrats_resids, summary)
=======
library(tidyverse)
library(here)
library(sandwich)
library(lmtest)
require(broom)

# load data
load(file=paste0(here(),"/data/VoterSupportData.RData"))
load(file=paste0(here(),"/data/ClimatePolicies.RData"))


# Analysis ----

## full period time series model without interventions for later residualization
### data
data4_socialDemocrats <- VoterSupportData %>%
  filter(Published >= elections$date[5] &
           Published <= elections$date[6]) %>%
  dplyr::select(
    Published,
    GovernmentSupport,
    SocialdemokraternaS,
    Unemployment,
    ConsumerPriceIndex,
    HouseholdConsumption,
    SocialdemokraternaS,
    ConsumerConfidence,
    GovtSearch,
    PolicyIndicator,
    Date_numeric,
    PollingInstitute,
    StartCollection,
    EndCollection
  ) %>% mutate(StartCollection = StartCollection %>% as.Date(),
               EndCollection = EndCollection %>% as.Date())

# fixing that there are only very few observations for periods of env policy 10 and 14
data4_socialDemocrats  <-
  data4_socialDemocrats  %>% mutate(
    PolicyIndicator = na_if(PolicyIndicator, 10),
    PolicyIndicator = na_if(PolicyIndicator, 14),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                11, 10),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                12, 11),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                13, 12),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                15, 13),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                16, 14),
    PolicyIndicator = replace(PolicyIndicator, PolicyIndicator ==
                                17, 15)
  )



### model
lm4_didit_socialDemocrats <- lm(
  SocialdemokraternaS ~ #PolicyIndicator*Date_numeric +
    lag(GovernmentSupport, 1) +
    lag(GovernmentSupport, 2) +
    lag(GovernmentSupport, 3) +
    lag(GovernmentSupport, 4) +
    lag(GovernmentSupport, 5) +
    lag(GovernmentSupport, 6) +
    Unemployment +
    ConsumerPriceIndex  +
    HouseholdConsumption +
    GovtSearch +
    # SocialdemokraternaS +
    ConsumerConfidence +
    as.factor(PollingInstitute)
  ,
  data = data4_socialDemocrats  %>% as.data.frame(),
  na.action = na.exclude
)
summary(lm4_didit_socialDemocrats)
step.model4.didit_socialDemocrats <-
  MASS::stepAIC(lm4_didit_socialDemocrats, direction = "forward",
                trace = FALSE)
summary(step.model4.didit_socialDemocrats)
coeftest(step.model4.didit_socialDemocrats, vcov = vcovPL(step.model4.didit))
preds4_socialDemocrats <-
  bind_cols(
    data4_socialDemocrats ,
    predict(step.model4.didit_socialDemocrats, interval = 'confidence') %>% as.data.frame(),
    residuals = resid(step.model4.didit_socialDemocrats, na.action = na.exclude)
  )

# plotting

timeseriesplot4_socialDemocrats <- preds4_socialDemocrats  %>% #filter(!is.na(PolicyIndicator)) %>%
  ggplot(aes(y = SocialdemokraternaS, x = EndCollection)) +
  geom_point(col = "darkgray") +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 8),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 8),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 9),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 9),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 10),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 10),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 11),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 11),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 12),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 12),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 13),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 13),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 14),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 14),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 15),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 15),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 16),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 16),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 17),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds4_socialDemocrats %>% filter(PolicyIndicator == 17),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  xlab("time") +
  geom_segment(
    data = envpols[c(9:17), ],
    aes(
      x = date,
      xend = date,
      y = 20,
      yend = 35
    ),
    col = "green4",
    size = 1
  ) +
  annotate(
    "rect",
    xmin = envpols[10, ]$date,
    xmax = envpols[11, ]$date,
    ymin = 20,
    ymax = 35,
    fill = "green4",
    alpha = 0.25
  ) +
  annotate(
    "rect",
    xmin = envpols[14, ]$date,
    xmax = envpols[15, ]$date,
    ymin = 20,
    ymax = 35,
    fill = "green4",
    alpha = 0.25
  ) +
  scale_x_date(limits = c(elections$date[5], elections$date[6]),
               expand = c(0, 0)) + theme_minimal()

timeseriesplot4_socialDemocrats


# looping over single interventions ----

plot_lst4_socialDemocrats <- list()
lin_regs_lst4_socialDemocrats <- list()
poly_regs_lst4_socialDemocrats <- list()

plot_lst4_socialDemocrats_resids <- list()
lin_regs_lst4_socialDemocrats_resids <- list()
poly_regs_lst4_socialDemocrats_resids <- list()

coef_lst4_socialDemocrats <- list()
coef_plot_lst4_socialDemocrats <- list()

#

j = data4_socialDemocrats  %>% pull(PolicyIndicator) %>% as.numeric() %>% min(na.rm = T)


for (i in (envpols[c(9, 11:13, 15:17), ] %>% rownames() %>% as.numeric() %>% min):(envpols[c(9, 11:13, 15:17), ] %>% rownames() %>% as.numeric() %>% max)) {
  #real obs simple model
  
  df_socialDemocrats <-
    data4_socialDemocrats  %>% filter(
      StartCollection >= lubridate::ymd(envpols[c(9, 11:13, 15:17), ]$date[i]) - 90 &
        EndCollection <= lubridate::ymd(envpols[c(9, 11:13, 15:17), ]$date[i]) +
        90
    ) %>%
    mutate(timetotreat = (EndCollection - as.Date(envpols[c(9, 11:13, 15:17), ]$date[i])) %>% as.numeric)
  lm_reg <- lm(
    SocialdemokraternaS ~ PolicyIndicator * timetotreat,
    #poly(Dat_num, 2, raw=T),
    data = df_socialDemocrats,
    na.action = na.exclude
  )
  lin_regs_lst4_socialDemocrats[[i]] <- lm_reg
  
  poly_reg <-
    lm(
      SocialdemokraternaS ~ PolicyIndicator * poly(timetotreat, 2, raw = T),
      data = df_socialDemocrats,
      na.action = na.exclude
    )
  poly_regs_lst4_socialDemocrats[[i]] <- poly_reg
  
  df_socialDemocrats <-
    bind_cols(
      df_socialDemocrats,
      predict(lm_reg, interval = 'confidence') %>% as.data.frame() %>% rename(
        fit_lin = fit,
        lwr_lin = lwr,
        upr_lin = upr
      ),
      predict(poly_reg, interval = 'confidence') %>% as.data.frame() %>% rename(
        fit_poly = fit,
        lwr_poly = lwr,
        upr_pol = upr
      )
    )
  
  
  plot_lst4_socialDemocrats[[i]] <- df_socialDemocrats %>% filter(!is.na(PolicyIndicator))  %>%
    ggplot(aes(y = SocialdemokraternaS, x = EndCollection)) +
    geom_point(col = "darkgray") + xlab("time") +
    geom_segment(
      data = envpols[c(9, 11:13, 15:17), ][i, ],
      aes(
        x = date,
        xend = date,
        y = 20,
        yend = 35
      ),
      col = "green4",
      size = 1
    ) +
    geom_ribbon(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) +
    geom_ribbon(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) + ylab("without controls") +
    theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  
  if (i == 2) {
    plot_lst4_socialDemocrats[[i]] <- df_socialDemocrats %>% # filter(!is.na(PolicyIndicator)) %>%
      ggplot(aes(y = SocialdemokraternaS, x = EndCollection)) +
      geom_point(col = "darkgray") + xlab("time") +
      geom_segment(
        data = envpols[10, ],
        aes(
          x = date,
          xend = date,
          y = 20,
          yend = 35
        ),
        col = "green4",
        size = 1
      ) +
      geom_segment(
        data = envpols[c(9, 11:13, 15:17), ][i, ],
        aes(
          x = date,
          xend = date,
          y = 20,
          yend = 35
        ),
        col = "green4",
        size = 1
      ) +
      annotate(
        "rect",
        xmin = envpols[10, ]$date,
        xmax = envpols[11, ]$date,
        ymin = 20,
        ymax = 35,
        fill = "green4",
        alpha = 0.25
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) + ylab("without controls") +
      theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  if (i == 5) {
    plot_lst4_socialDemocrats[[i]] <- df_socialDemocrats %>% # filter(!is.na(PolicyIndicator)) %>%
      ggplot(aes(y = SocialdemokraternaS, x = EndCollection)) +
      geom_point(col = "darkgray") + xlab("time") +
      geom_segment(
        data = envpols[14, ],
        aes(
          x = date,
          xend = date,
          y = 20,
          yend = 35
        ),
        col = "green4",
        size = 1
      ) +
      geom_segment(
        data = envpols[c(9, 11:13, 15:17), ][i, ],
        aes(
          x = date,
          xend = date,
          y = 20,
          yend = 35
        ),
        col = "green4",
        size = 1
      ) +
      annotate(
        "rect",
        xmin = envpols[14, ]$date,
        xmax = envpols[15, ]$date,
        ymin = 20,
        ymax = 35,
        fill = "green4",
        alpha = 0.25
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) +
      geom_ribbon(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) + ylab("without controls") +
      theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  
  # residuals
  
  df_resids_socialDemocrats <-
    preds4_socialDemocrats %>% filter(
      StartCollection >= lubridate::ymd(envpols[c(9, 11:13, 15:17), ]$date[i]) - 90 &
        EndCollection <= lubridate::ymd(envpols[c(9, 11:13, 15:17), ]$date[i]) +
        90
    ) %>%
    mutate(timetotreat = (EndCollection - as.Date(envpols[c(9, 11:13, 15:17), ]$date[i])) %>% as.numeric)
  
  lm_reg_resids <- lm(residuals ~ PolicyIndicator * timetotreat,
                      #poly(Dat_num, 2, raw=T),
                      data = df_resids_socialDemocrats,
                      na.action = na.exclude)
  lin_regs_lst4_socialDemocrats_resids[[i]] <- lm_reg_resids
  
  poly_reg_resids <-
    lm(
      residuals ~ PolicyIndicator * poly(timetotreat, 2, raw = T),
      data = df_resids_socialDemocrats,
      na.action = na.exclude
    )
  poly_regs_lst4_socialDemocrats_resids[[i]] <- poly_reg_resids
  
  df_resids_socialDemocrats <- bind_cols(
    df_resids_socialDemocrats,
    predict(lm_reg_resids, interval = 'confidence') %>% as.data.frame() %>% rename(
      fit_lin = fit,
      lwr_lin = lwr,
      upr_lin = upr
    ),
    predict(poly_reg_resids, interval = 'confidence') %>% as.data.frame() %>% rename(
      fit_poly = fit,
      lwr_poly = lwr,
      upr_pol = upr
    )
  )
  
  
  plot_lst4_socialDemocrats_resids[[i]] <-
    df_resids_socialDemocrats  %>% filter(!is.na(PolicyIndicator)) %>% ggplot(aes(y = residuals, x =
                                                                           EndCollection)) +
    geom_point(col = "darkgray") + xlab("time") +
    geom_segment(
      data = envpols[c(9, 11:13, 15:17), ][i, ],
      aes(
        x = date,
        xend = date,
        y = -6,
        yend = 5
      ),
      col = "green4",
      size = 1
    ) +
    geom_ribbon(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) +
    geom_ribbon(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) + ylab("residualized") + theme_minimal()
  
  if (i == 2) {
    plot_lst4_socialDemocrats_resids[[i]] <-
      df_resids_socialDemocrats %>% # filter(!is.na(PolicyIndicator)) %>%
      ggplot(aes(y = residuals, x = EndCollection)) +
      geom_point(col = "darkgray") + xlab("time") +
      geom_segment(
        data = envpols[10, ],
        aes(
          x = date,
          xend = date,
          y = -6,
          yend = 5
        ),
        col = "green4",
        size = 1
      ) +
      geom_segment(
        data = envpols[c(9, 11:13, 15:17), ][i, ],
        aes(
          x = date,
          xend = date,
          y = -6,
          yend = 5
        ),
        col = "green4",
        size = 1
      ) +
      annotate(
        "rect",
        xmin = envpols[10, ]$date,
        xmax = envpols[11, ]$date,
        ymin = -6,
        ymax = 5,
        fill = "green4",
        alpha = 0.25
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) + ylab("without controls") +
      theme_minimal()
  }
  
  if (i == 5) {
    plot_lst4_socialDemocrats_resids[[i]] <-
      df_resids_socialDemocrats %>% # filter(!is.na(PolicyIndicator)) %>%
      ggplot(aes(y = residuals, x = EndCollection)) +
      geom_point(col = "darkgray") + xlab("time") +
      geom_segment(
        data = envpols[14, ],
        aes(
          x = date,
          xend = date,
          y = -6,
          yend = 5
        ),
        col = "green4",
        size = 1
      ) +
      geom_segment(
        data = envpols[c(9, 11:13, 15:17), ][i, ],
        aes(
          x = date,
          xend = date,
          y = -6,
          yend = 5
        ),
        col = "green4",
        size = 1
      ) +
      annotate(
        "rect",
        xmin = envpols[14, ]$date,
        xmax = envpols[15, ]$date,
        ymin = -6,
        ymax = 5,
        fill = "green4",
        alpha = 0.25
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_lin, ymax = upr_lin),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_lin),
        col = "darkred",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) +
      geom_ribbon(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(ymin = lwr_poly, ymax = upr_pol),
        alpha = 0.3
      ) +
      geom_line(
        data = df_resids_socialDemocrats %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
        aes(EndCollection, fit_poly),
        col = "blue",
        size = .75
      ) + ylab("without controls") +
      theme_minimal() 
  }
  
  coef_lst4_socialDemocrats[[i]] <-
    bind_rows(
      bind_cols(
        level_change = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[4],
        sc_se = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[4],
        sc_conf_low = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[4],
        sc_conf_high = tidy(coeftest(lin_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[4],
        
        linear = T,
        polynomial = F,
        residualized = F
      ),
      bind_cols(
        level_change = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[5],
        sc_se = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[5],
        sc_conf_low = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[5],
        sc_conf_high = tidy(coeftest(poly_regs_lst4_socialDemocrats[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[5],
        
        linear = F,
        polynomial = T,
        residualized = F
      ),
      bind_cols(
        level_change = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[4],
        sc_se = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[4],
        sc_conf_low = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[4],
        sc_conf_high = tidy(coeftest(lin_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[4],
        
        linear = T,
        polynomial = F,
        residualized = T
      ),
      bind_cols(
        level_change = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[5],
        sc_se = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[5],
        sc_conf_low = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[5],
        sc_conf_high = tidy(coeftest(poly_regs_lst4_socialDemocrats_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[5],
        
        linear = F,
        polynomial = T,
        residualized = T
      )
    ) %>% mutate(model = c(1:4))
  
  
  coef_plot_lst4_socialDemocrats[[i]] <- cowplot::plot_grid(
    cowplot::ggdraw() + cowplot::draw_label("coefficients",
                                            x = 0,
                                            hjust = 0) +
      theme(# add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)),
    ggplot(coef_lst4_socialDemocrats[[i]], aes(y = level_change, x = model)) +
      geom_point() +
      geom_pointrange(aes(ymin = lc_conf_low, ymax = lc_conf_high)) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "darkgray",
        size = 1.5
      ) +
      theme_bw() + theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank()
      ),
    
    ggplot(coef_lst4_socialDemocrats[[i]], aes(y = slope_change, x = model)) +
      geom_point() +
      geom_pointrange(aes(ymin = sc_conf_low, ymax = sc_conf_high)) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "darkgray",
        size = 1.5
      ) +
      theme_bw() + theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank()
      ),
    
    cowplot::ggdraw() + cowplot::draw_label("model specifications",
                                            x = 0,
                                            hjust = 0) +
      theme(# add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)),
    
    ggplot(coef_lst4_socialDemocrats[[i]]) +
      geom_point(
        aes(
          y = "residualized",
          x = model,
          color = residualized,
          size = .25
        )
      ) +
      geom_point(aes(
        y = "polynomial",
        x = model,
        color = polynomial,
        size = .25
      )) +
      geom_point(aes(
        y = "linear",
        x = model,
        color = linear,
        size = .25
      )) +
      theme_minimal() + scale_color_manual(values = c(
        "TRUE" = "darkgray", "FALSE" = "white"
      )) +
      theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank()
      ),
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(.2, 2, 2, .2, 1)#,
  )
  j = j + 1
}


plot4_socialDemocrats <- cowplot::plot_grid(
  timeseriesplot4_socialDemocrats,
  # cowplot::plot_grid(plotlist =plot_lst4_socialDemocrats_controls,nrow = 1),
  cowplot::plot_grid(
    cowplot::plot_grid(plotlist = plot_lst4_socialDemocrats, nrow = 1),
    cowplot::plot_grid(plotlist = plot_lst4_socialDemocrats_resids, nrow = 1),
    nrow = 2,
    align = "v",
    axis = "lr"
  ),
  cowplot::plot_grid(plotlist = coef_plot_lst4_socialDemocrats, nrow =
                       1),
  labels = c('a)', 'b)', 'c)'),
  nrow = 3
)

plot4_socialDemocrats
ggsave(
  filename = "period4_socialDemocrats.pdf",
  path = here("analysis/plots/"),
  width = 57,
  height = 35,
  units = "cm"
)


# lapply(lin_regs_lst4_socialDemocrats, summary)
# lapply(lin_regs_lst4_socialDemocrats_controls, summary)
# lapply(lin_regs_lst4_socialDemocrats_resids, summary)
>>>>>>> 972e08ac31bc84d037c526d404f79af5d910a244
