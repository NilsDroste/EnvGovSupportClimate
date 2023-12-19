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
data2 <- VoterSupportData %>%
  filter(Published >= elections$date[3] &
           Published <= elections$date[4]) %>%
  dplyr::select(
    Published,
    GovernmentSupport,
    Unemployment,
    ConsumerPriceIndex,
    HouseholdConsumption,
    MiljöpartietMP,
    ConsumerConfidence,
    GovtSearch,
    Date_numeric,
    PolicyIndicator,
    PollingInstitute,
    StartCollection,
    EndCollection
  ) %>% mutate(StartCollection = StartCollection %>% as.Date(),
               EndCollection = EndCollection %>% as.Date())


### model
lm2_didit <- lm(
  GovernmentSupport ~ 
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
    # MiljöpartietMP +
    ConsumerConfidence +
    as.factor(PollingInstitute)
  ,
  data = data2 %>% as.data.frame(),
  na.action = na.exclude
)
summary(lm2_didit)
step.model2.didit <-
  MASS::stepAIC(lm2_didit, direction = "forward",
                trace = FALSE)
summary(step.model2.didit)
coeftest(step.model2.didit, vcov = vcovPL(step.model2.didit))
preds2 <-
  bind_cols(
    data2,
    predict(step.model2.didit, interval = 'confidence') %>% as.data.frame(),
    residuals = resid(step.model2.didit, na.action = na.exclude)
  )

# plotting
timeseriesplot2 <-
  preds2 %>% filter(!is.na(PolicyIndicator)) %>% ggplot(aes(y = GovernmentSupport, x =
                                                               EndCollection)) +
  geom_point(col = "darkgray") +
  geom_line(
    data = preds2 %>% filter(PolicyIndicator == 3),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds2 %>% filter(PolicyIndicator == 3),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds2 %>% filter(PolicyIndicator == 4),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds2 %>% filter(PolicyIndicator == 4),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  geom_line(
    data = preds2 %>% filter(PolicyIndicator == 5),
    aes(y = fit, x = EndCollection),
    col = "darkred"
  ) +
  geom_ribbon(
    data = preds2 %>% filter(PolicyIndicator == 5),
    aes(ymin = lwr, ymax = upr),
    alpha = 0.3
  ) +
  xlab("time") +
  geom_segment(
    data = envpols[4:5, ],
    aes(
      x = date,
      xend = date,
      y = 33,
      yend = 53
    ),
    col = "green4",
    size = 1
  ) +
  scale_x_date(limits = c(elections$date[3], elections$date[4]),
               expand = c(0, 0)) + theme_minimal()

timeseriesplot2


# looping over single interventions ----

plot_lst2 <- list()
lin_regs_lst2 <- list()
poly_regs_lst2 <- list()

plot_lst2_resids <- list()
lin_regs_lst2_resids <- list()
poly_regs_lst2_resids <- list()

coef_lst2 <- list()
coef_plot_lst2 <- list()

j = data2 %>% pull(PolicyIndicator) %>% as.numeric() %>% min(na.rm = T)


for (i in (envpols[4:5, ] %>% rownames() %>% as.numeric() %>% min):(envpols[4:5, ] %>% rownames() %>% as.numeric() %>% max)) {
  
  #real obs simple model
  df <-
    data2 %>% filter(
      Published >= lubridate::ymd(envpols[4:5, ]$date[i]) - 90 &
        Published <= lubridate::ymd(envpols[4:5, ]$date[i]) +
        90
    ) %>%
    mutate(timetotreat = (EndCollection - as.Date(envpols[4:5, ]$date[i])) %>% as.numeric)
  lm_reg <- lm(
    GovernmentSupport ~ PolicyIndicator * timetotreat,
    #poly(Dat_num, 2, raw=T),
    data = df,
    na.action = na.exclude
  )
  lin_regs_lst2[[i]] <- lm_reg
  
  poly_reg <-
    lm(
      GovernmentSupport ~ PolicyIndicator * poly(timetotreat, 2, raw = T),
      data = df,
      na.action = na.exclude
    )
  poly_regs_lst2[[i]] <- poly_reg
  
  df <-
    bind_cols(
      df,
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
  
  
  plot_lst2[[i]] <-
    df %>% filter(!is.na(PolicyIndicator)) %>% ggplot(aes(y = GovernmentSupport, x =
                                                             EndCollection)) +
    geom_point(col = "darkgray") + xlab("time") +
    geom_segment(
      data = envpols[4:5, ][i, ],
      aes(
        x = date,
        xend = date,
        y = 33,
        yend = 53
      ),
      col = "green4",
      size = 1
    ) +
    geom_ribbon(
      data = df %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) +
    geom_ribbon(
      data = df %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) + ylab("without controls") +
    theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  
  #residuals
  
  df_resids <-
    preds2 %>% filter(
      StartCollection >= lubridate::ymd(envpols[4:5, ]$date[i]) - 90 &
        EndCollection <= lubridate::ymd(envpols[4:5, ]$date[i]) +
        90
    ) %>%
    mutate(timetotreat = (EndCollection - as.Date(envpols[4:5, ]$date[i])) %>% as.numeric)
  
  lm_reg_resids <- lm(residuals ~ PolicyIndicator * timetotreat,
                      #poly(Dat_num, 2, raw=T),
                      data = df_resids,
                      na.action = na.exclude)
  lin_regs_lst2_resids[[i]] <- lm_reg_resids
  
  poly_reg_resids <-
    lm(
      residuals ~ PolicyIndicator * poly(timetotreat, 2, raw = T),
      data = df_resids,
      na.action = na.exclude
    )
  poly_regs_lst2_resids[[i]] <- poly_reg_resids
  
  df_resids <- bind_cols(
    df_resids,
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
  
  
  plot_lst2_resids[[i]] <-
    df_resids %>% filter(!is.na(PolicyIndicator)) %>% ggplot(aes(y = residuals, x =
                                                                    EndCollection)) +
    geom_point(col = "darkgray") + xlab("time") +
    geom_segment(
      data = envpols[4:5, ][i, ],
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
      data = df_resids %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df_resids %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_lin, ymax = upr_lin),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_lin),
      col = "darkred",
      size = .75
    ) +
    geom_ribbon(
      data = df_resids %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids %>% filter(PolicyIndicator %>% as.character() %>% as.numeric < j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) +
    geom_ribbon(
      data = df_resids %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(ymin = lwr_poly, ymax = upr_pol),
      alpha = 0.3
    ) +
    geom_line(
      data = df_resids %>% filter(PolicyIndicator %>% as.character() %>% as.numeric == j),
      aes(EndCollection, fit_poly),
      col = "blue",
      size = .75
    ) + ylab("residualized") + theme_minimal()
  
  
  coef_lst2[[i]] <-
    bind_rows(
      bind_cols(
        level_change = tidy(coeftest(lin_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(lin_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(lin_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(lin_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(lin_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[4],
        sc_se = tidy(coeftest(lin_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[4],
        sc_conf_low = tidy(coeftest(lin_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[4],
        sc_conf_high = tidy(coeftest(lin_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[4],
        
        linear = T,
        polynomial = F,
        residualized = F
      ),
      bind_cols(
        level_change = tidy(coeftest(poly_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(poly_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(poly_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(poly_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(poly_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[5],
        sc_se = tidy(coeftest(poly_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[5],
        sc_conf_low = tidy(coeftest(poly_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[5],
        sc_conf_high = tidy(coeftest(poly_regs_lst2[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[5],
        
        linear = F,
        polynomial = T,
        residualized = F
      ),
      bind_cols(
        level_change = tidy(coeftest(lin_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(lin_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(lin_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(lin_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(lin_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[4],
        sc_se = tidy(coeftest(lin_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[4],
        sc_conf_low = tidy(coeftest(lin_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[4],
        sc_conf_high = tidy(coeftest(lin_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[4],
        
        linear = T,
        polynomial = F,
        residualized = T
      ),
      bind_cols(
        level_change = tidy(coeftest(poly_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[2],
        lc_se = tidy(coeftest(poly_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[2],
        lc_conf_low = tidy(coeftest(poly_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[2],
        lc_conf_high = tidy(coeftest(poly_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[2],
        
        slope_change = tidy(coeftest(poly_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$estimate[5],
        sc_se = tidy(coeftest(poly_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$`std.error`[5],
        sc_conf_low = tidy(coeftest(poly_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.low[5],
        sc_conf_high = tidy(coeftest(poly_regs_lst2_resids[[i]], vcov = vcovPL), conf.int = TRUE)$conf.high[5],
        
        linear = F,
        polynomial = T,
        residualized = T
      )
    ) %>% mutate(model = c(1:4))
  
  
  coef_plot_lst2[[i]] <- cowplot::plot_grid(
    cowplot::ggdraw() + cowplot::draw_label("coefficients",
                                            x = 0,
                                            hjust = 0) +
      theme(# add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)),
    ggplot(coef_lst2[[i]], aes(y = level_change, x = model)) +
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
    
    ggplot(coef_lst2[[i]], aes(y = slope_change, x = model)) +
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
    
    ggplot(coef_lst2[[i]]) +
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

plot2 <- cowplot::plot_grid(
  timeseriesplot2,
  cowplot::plot_grid(
    cowplot::plot_grid(plotlist = plot_lst2, nrow = 1),
    cowplot::plot_grid(plotlist = plot_lst2_resids, nrow = 1),
    nrow = 2,
    align = "v",
    axis = "lr"
  ),
  cowplot::plot_grid(plotlist = coef_plot_lst2, nrow =
                       1),
  labels = c('a)', 'b)', 'c)'),
  nrow = 3
)

plot2
ggsave(
  filename = "period2.pdf",
  path = here("analysis/plots/"),
  width = 57,
  height = 35,
  units = "cm"
)