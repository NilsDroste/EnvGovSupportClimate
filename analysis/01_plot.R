library(tidyverse)
library(here)

# load data
load(file=paste0(here(),"/data/VoterSupportData.RData"))
load(file=paste0(here(),"/data/ClimatePolicies.RData"))


GovernmentDates <- VoterSupportData %>% group_by(IncumbentParty) %>%
  dplyr::mutate(
    first = dplyr::first(Published),
    last = dplyr::last(Published),
    firstMonth = dplyr::first(PollMonth),
    lastMonth = dplyr::last(PollMonth),
  ) %>% dplyr::select(IncumbentParty, first, last, firstMonth, lastMonth) %>% filter(!is.na(IncumbentParty), !IncumbentParty=="M FP C KD") %>% unique

timeseriesplot <- VoterSupportData  %>% mutate(
  StartCollection = StartCollection %>% as.Date(),
  EndCollection = EndCollection %>% as.Date()) %>% filter(!is.na(GovernmentSupport)) %>% ggplot(aes(y=GovernmentSupport, x=EndCollection)) +
  geom_rect(data=GovernmentDates, aes(xmin = first %>% as.Date(), xmax = last %>% as.Date(), ymin = 20, ymax = 60),
            inherit.aes=FALSE, alpha = 0.4, fill = c("gray")) +
  xlab("time") +
  geom_point(col="darkgray") +
  geom_segment(data=envpols, aes(x = date, xend= date, y=20, yend=60), col = "green4", size=1) +
  geom_segment(data=elections[-1,], aes(x = date, xend=date, y=20, yend=60), col = "darkred", size=1.5, linetype='dotted') +
  annotate("text", label = "Gov't:", x = lubridate::as_date("2002-02-01") , y = 62, size = 8, colour = "grey60")+
  # annotate("text", label = "S", x = lubridate::as_date("2002-06-01"), y = 62, size = 8, colour = "grey60")+
  annotate("text", label = "S", x = lubridate::as_date("2005-01-01"), y = 62, size = 8, colour = "grey60")+
  annotate("text", label = "M L C KD", x = lubridate::as_date("2009-01-01"), y = 62, size = 8, colour = "grey60")+
  annotate("text", label = "M L C KD", x = lubridate::as_date("2013-03-01"), y = 62, size = 8, colour = "grey60")+
  annotate("text", label = "S MP", x = lubridate::as_date("2016-06-01"), y = 62, size = 8, colour = "grey60")+
  annotate("text", label = "S MP C L", x = lubridate::as_date("2019-10-01"), y = 62, size = 8, colour = "grey60") + theme_minimal(base_size = 22) +
  ylab("Government Support") 


  timeseriesplot
  ggsave(plot = timeseriesplot,
    filename = "Figure1.pdf",
    path = here("analysis/plots/"),
    width = 38,
    height = 18,
    units = "cm"
  )