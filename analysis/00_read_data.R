###########################################
# Henriks et al. (2023) Voter do not punish
# their government for climate policies ###
# script author: nils.droste@svet.lu.se ###

# load packages ----
library(tidyverse)
library(here)
library(readxl)
library(stringr)
library(lubridate)
library(zoo)

# load data ----
# get the content from the excel sheets, into one object (PartySupport) w/o the källa sheet
PartySupport_location <-
  paste0(here(), "/data/raw_data/Polls_00-20_FiveInstitutes.xlsx")
PartySupport <- PartySupport_location %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = PartySupport_location) %>% within(rm("källa"))

# PartySupport in now a list object we iterate over to make them equal
for (i in 1:length(PartySupport)) {
  {
    PartySupport[[i]] <-
      PartySupport[[i]] %>% dplyr::select(c(1:12)) %>% rename(Publicerad = contains("Publicerad"))
  }
}

# now we combine them into one single tibble dataframe
PartySupport <- PartySupport %>% reduce(full_join)

# create a Year month for the poll from "period" (which we will use to match the other other data for an easy data "glueing")
PartySupport <-
  PartySupport %>% mutate(
    PollMonth = Publicerad %>% as.yearmon(),
    PollYear = PartySupport$Publicerad %>% lubridate::year(),
    PollWeek = strftime(Publicerad, format = "%Y-%V")
  )

# umemployment data
unemployment <-
  read_excel(paste0(here(), "/data/raw_data/Arbetsloshet.xlsx"), range = "A3:C260") %>%
  rename(Date = `...1`,
         Unemployment  = `Procent`,
         UnemploymentTrend = `Procent, trend`) %>% mutate(
           Date = paste(
             `Date` %>%  str_sub(1, 3) %>% str_replace_all(pattern = "maj", "may") %>% str_replace_all(pattern =
                                                                                                         "okt", "oct"),
             `Date` %>%  str_sub(-2, -1),
             sep = " "
           ) %>% as.yearmon('%b%y')
         )

# consumption data
householdconsumption <-
  read_excel(paste0(here(), "/data/raw_data/Hushallens_konsumtion.xlsx"),
             range = "A2:C259") %>% rename(
               Date = `...1`,
               HouseholdConsumption  = `Procent`,
               HouseholdConsumptionDetrended = `Procent, trendjusterat`
             ) %>% mutate(
               `Date` = paste(
                 unemployment$Date %>%  str_sub(1, 3) %>% 
                   str_replace_all(pattern = "maj", "may") %>% str_replace_all(pattern = "okt", "oct"),
                 `Date` %>%  str_sub(-2, -1),
                 sep = " "
               ) %>% as.yearmon('%b%y')
             )

# price indices
consumerprices <-
  read_excel(paste0(here(), "/data/raw_data/Ekonomifakta-15497-2020-04-06.xlsx")) %>% rename(
    Date = "YearMonth Inflationstakten",
    ConsumerPriceIndexFixedInterest = "KPIF (%)",
    ConsumerPriceIndex = "KPI (%)"
  ) %>% mutate(
    Date = paste(
      `Date` %>%  str_sub(1, 3) %>% str_replace_all(pattern = "maj", "may") %>% str_replace_all(pattern = "okt", "oct"),
      `Date` %>%  str_sub(-2, -1),
      sep = " "
    ) %>% as.yearmon('%b%y')
  )

# Governments
IncumbentParty =  paste(rep(2000:2020, each = 12), c(1:12), sep = "-") %>% as.yearmon('%Y-%m') %>% as_tibble() %>% 
  rename(Date = value) %>%
  mutate(IncumbentParty = ifelse(
    `Date` >= as.yearmon("2000-01") &
      `Date` <= as.yearmon("2006-09"),
    "S",
    ifelse(
      `Date` >= as.yearmon("2006-10") &
        `Date` <= as.yearmon("2014-09"),
      "M FP C KD",
      ifelse(`Date` >= as.yearmon("2014-10"), "S MP", NA)
    )
  ))


# consumer confidence index (https://data.oecd.org/leadind/consumer-confidence-index-cci.htm)
consumerconfindence <-
  read_csv(paste0(here(), "/data/raw_data/DP_LIVE_14032021145647968.csv")) %>% 
  filter(LOCATION == "SWE") %>% dplyr::mutate(Date = TIME %>% as.yearmon(),  ConsumerConfidence = Value) %>% dplyr::select(Date, ConsumerConfidence)

# google search trends "regeringen" (https://trends.google.com/trends/explore?date=all&geo=SE&q=regeringen)
regeringen <-
  read.csv(paste0(here(), "/data/raw_data/multiTimeline.csv"), skip = 1) %>% 
  dplyr::mutate(Date =  Month %>% as.yearmon(), GovtSearch = `regeringen...Sweden.`) %>% dplyr::select(Date, GovtSearch)

# Stock Index (https://stooq.com/q/d/?s=^omxs)
stockholm30 <-
  read.csv(paste0(here(), "/data/raw_data/^omxs_d.csv"))


# creating first dataset
VoterSupportData <-
  PartySupport %>%
  left_join(unemployment, by = c("PollMonth" = "Date")) %>%
  left_join(householdconsumption,  by = c("PollMonth" = "Date")) %>%
  left_join(consumerprices,  by = c("PollMonth" = "Date")) %>%
  left_join(IncumbentParty, by = c("PollMonth" = "Date")) %>%
  left_join(consumerconfindence, by = c("PollMonth" = "Date")) %>%
  left_join(regeringen, by = c("PollMonth" = "Date")) %>%
  arrange(PollMonth) %>% mutate(GovernmentSupport = ifelse(
    IncumbentParty == "S",
    `SocialdemokraternaS`,
    ifelse(
      IncumbentParty == "M FP C KD",
      `ModeraternaM` + `LiberalernaL` + `CenterpartietC` + `KristdemokraternaKD`,
      ifelse(
        IncumbentParty == "S MP",
        `SocialdemokraternaS` + `MiljöpartietMP`,
        NA
      )
    )
  ))

# add in election dates
elections <- bind_cols(
  date = lubridate::as_date(
    c(
      "1998-08-20",
      "2002-10-15",
      "2006-10-17",
      "2010-10-19",
      "2014-10-14",
      "2018-10-09"
    )
  ),
  government = c("S", "S2", "M_FP_C_KD", "M_FP_C_KD2", "S_MP", "S_MP_L_C")
)


# add in environmental policies ----
envpols <- bind_cols(
  Policy = c(
    "ClimateInvest_2003_Jul",
    "EnergyEfficency_2004_Dec",
    "HeatingSubsidy_2006_Jan",
    "EnvCar_2007_Apr",
    "SustainableCity_2009_Jan",
    "TaxEnvCar",
    "FuelTax_2011_Jan",
    "SuperEnvCar_2012_Jan",
    "BiogasSupport_2015_Jan",
    "ClimateStep_2015_Jun",
    "LocalClimateInvest_2015_Jul",
    "UrbanEnv_2015_Oct",
    "ElectricBus",
    "IndustryStep_Jan_2018",
    "Emobility_Feb_2018",
    "FlyTax_2018_Apr",
    "BonusMalus_2018_Jul",
    "Co2CHPplants_2019_Jan",
    "EcoBonus_2019_Dec",
    "EnvCarTax_2020_Jan"
  ),
  date = lubridate::as_date(
    c(
      "2003-07-01",
      "2004-12-01",
      "2006-01-15",
      "2007-04-01",
      "2009-01-15",
      "2010-10-01",
      "2011-01-01",
      "2012-01-16",
      "2015-01-01",
      "2015-06-25",
      "2015-07-15",
      "2015-10-01",
      "2016-07-26",
      "2018-01-03",
      "2018-02-01",
      "2018-04-01",
      "2018-07-01",
      "2019-01-08",
      "2019-12-05",
      "2020-01-01"
    )
  )
)

# fix periods ----
# for polling data get collection dates right

#extract the data in a dataframe based on pattern
dat <-
  as.data.frame(
    stringr::str_match(
      VoterSupportData$Period,
      '(\\d+)\\s?([a-z]+)?\\s(\\-)\\s(\\d+)\\s([a-z]+)'
    )
  )[, -1]

# add year
dat$year <- VoterSupportData$PollYear

#Change the columns to respective type
dat <- type.convert(dat, as.is = TRUE)

#fix names
names(dat) <- c("day1", "month1", "to", "day2", "month2", "year1")

#Copy the year column
dat$year2 <- dat$year1

#Copy the month column if it is the same
dat$month1[is.na(dat$month1)] <- dat$month2[is.na(dat$month1)]

#creat month string
mthx <-
  c("jan",
    "feb",
    "mar",
    "apr",
    "maj",
    "jun",
    "jul",
    "aug",
    "sep",
    "okt",
    "nov",
    "dec")

#Subtract 1 from the year only if the End month is earlier than Start month
dat <-
  transform(dat, year1 = year1 - as.integer(match(month1, mthx) > match(month2, mthx)))
dat <-
  dat %>% mutate(
    month1 = replace(month1, month1 == "maj",  "may"),
    month1 = replace(month1, month1 == "okt", "oct"),
    month2 = replace(month2, month2 == "maj",  "may"),
    month2 = replace(month2, month2 == "okt", "oct")
  )

#Create the final result dataframe pasting the values
result <-
  data.frame(
    StartCollection = with(dat, paste(day1, month1, year1) %>% lubridate::dmy()),
    EndCollection   = with(dat, paste(day2, month2, year2) %>% lubridate::dmy())
  )

# fixing one case where this did not work
result <-
  result %>% mutate(
    StartCollection = replace(StartCollection, StartCollection == "2010-12-09", "2009-12-09"),
    EndCollection = replace(EndCollection, EndCollection ==
                              "2010-12-30", "2010-12-30")
  )

# add in data
VoterSupportData <- VoterSupportData %>% bind_cols(result)

#drop unknown periods
VoterSupportData <- VoterSupportData %>% filter(Period != "okänt")

# create policy indicator if policy happened after publishing date
VoterSupportData <-
  VoterSupportData %>% mutate(
    above_published = case_when(
      `Publicerad` < as.Date(envpols$date[1]) ~ 0,
      `Publicerad` > as.Date(envpols$date[1]) &
        `Publicerad` < as.Date(envpols$date[2]) ~ 1,
      `Publicerad` > as.Date(envpols$date[2]) &
        `Publicerad` < as.Date(envpols$date[3]) ~ 2,
      `Publicerad` > as.Date(envpols$date[3]) &
        `Publicerad` < as.Date(envpols$date[4]) ~ 3,
      `Publicerad` > as.Date(envpols$date[4]) &
        `Publicerad` < as.Date(envpols$date[5]) ~ 4,
      `Publicerad` > as.Date(envpols$date[5]) &
        `Publicerad` < as.Date(envpols$date[6]) ~ 5,
      `Publicerad` > as.Date(envpols$date[6]) &
        `Publicerad` < as.Date(envpols$date[7]) ~ 6,
      `Publicerad` > as.Date(envpols$date[7]) &
        `Publicerad` < as.Date(envpols$date[8]) ~ 7,
      `Publicerad` > as.Date(envpols$date[8]) &
        `Publicerad` < as.Date(envpols$date[9]) ~ 8,
      `Publicerad` > as.Date(envpols$date[9]) &
        `Publicerad` < as.Date(envpols$date[10]) ~ 9,
      `Publicerad` > as.Date(envpols$date[10]) &
        `Publicerad` < as.Date(envpols$date[11]) ~ 10,
      `Publicerad` > as.Date(envpols$date[11]) &
        `Publicerad` < as.Date(envpols$date[12]) ~ 11,
      `Publicerad` > as.Date(envpols$date[12]) &
        `Publicerad` < as.Date(envpols$date[13]) ~ 12,
      `Publicerad` > as.Date(envpols$date[13]) &
        `Publicerad` < as.Date(envpols$date[14]) ~ 13,
      `Publicerad` > as.Date(envpols$date[14]) &
        `Publicerad` < as.Date(envpols$date[15]) ~ 14,
      `Publicerad` > as.Date(envpols$date[15]) &
        `Publicerad` < as.Date(envpols$date[16]) ~ 15,
      `Publicerad` > as.Date(envpols$date[16]) &
        `Publicerad` < as.Date(envpols$date[17]) ~ 16,
      `Publicerad` > as.Date(envpols$date[17]) &
        `Publicerad` < as.Date(envpols$date[18]) ~ 17,
      `Publicerad` > as.Date(envpols$date[18]) &
        `Publicerad` < as.Date(envpols$date[19]) ~ 18,
      `Publicerad` > as.Date(envpols$date[19]) &
        `Publicerad` < as.Date(envpols$date[20]) ~ 19,
      `Publicerad` > as.Date(envpols$date[20]) ~ 20
    ) %>% as.factor()
  )

# create policy indicator if policy happened after collection date
VoterSupportData <-
  VoterSupportData %>% mutate(
    above_collection = case_when(
      `EndCollection` < as.Date(envpols$date[1]) ~ 0,
      `StartCollection` > as.Date(envpols$date[1]) &
        `EndCollection` < as.Date(envpols$date[2]) ~ 1,
      `StartCollection` > as.Date(envpols$date[2]) &
        `EndCollection` < as.Date(envpols$date[3]) ~ 2,
      `StartCollection` > as.Date(envpols$date[3]) &
        `EndCollection` < as.Date(envpols$date[4]) ~ 3,
      `StartCollection` > as.Date(envpols$date[4]) &
        `EndCollection` < as.Date(envpols$date[5]) ~ 4,
      `StartCollection` > as.Date(envpols$date[5]) &
        `EndCollection` < as.Date(envpols$date[6]) ~ 5,
      `StartCollection` > as.Date(envpols$date[6]) &
        `EndCollection` < as.Date(envpols$date[7]) ~ 6,
      `StartCollection` > as.Date(envpols$date[7]) &
        `EndCollection` < as.Date(envpols$date[8]) ~ 7,
      `StartCollection` > as.Date(envpols$date[8]) &
        `EndCollection` < as.Date(envpols$date[9]) ~ 8,
      `StartCollection` > as.Date(envpols$date[9]) &
        `EndCollection` < as.Date(envpols$date[10]) ~ 9,
      `StartCollection` > as.Date(envpols$date[10]) &
        `EndCollection` < as.Date(envpols$date[11]) ~ 10,
      `StartCollection` > as.Date(envpols$date[11]) &
        `EndCollection` < as.Date(envpols$date[12]) ~ 11,
      `StartCollection` > as.Date(envpols$date[12]) &
        `EndCollection` < as.Date(envpols$date[13]) ~ 12,
      `StartCollection` > as.Date(envpols$date[13]) &
        `EndCollection` < as.Date(envpols$date[14]) ~ 13,
      `StartCollection` > as.Date(envpols$date[14]) &
        `EndCollection` < as.Date(envpols$date[15]) ~ 14,
      `StartCollection` > as.Date(envpols$date[15]) &
        `EndCollection` < as.Date(envpols$date[16]) ~ 15,
      `StartCollection` > as.Date(envpols$date[16]) &
        `EndCollection` < as.Date(envpols$date[17]) ~ 16,
      `StartCollection` > as.Date(envpols$date[17]) &
        `EndCollection` < as.Date(envpols$date[18]) ~ 17,
      `StartCollection` > as.Date(envpols$date[18]) &
        `EndCollection` < as.Date(envpols$date[19]) ~ 18,
      `StartCollection` > as.Date(envpols$date[19]) &
        `EndCollection` < as.Date(envpols$date[20]) ~ 19,
      `StartCollection` > as.Date(envpols$date[20]) ~ 20
    ) %>% as.factor()
  )

# create running numeric date indicator, and government periods
VoterSupportData <-
  VoterSupportData %>% mutate(
    Date_numeric = Publicerad %>% as.Date %>% as.numeric,
    Government = case_when(
      `Publicerad` < elections$date[2] ~ "S",
      `Publicerad` > elections$date[2] &
        `Publicerad` < elections$date[3] ~ "S2",
      `Publicerad` > elections$date[3] &
        `Publicerad` < elections$date[4] ~ "M_FP_C_KD",
      `Publicerad` > elections$date[4] &
        `Publicerad` < elections$date[5] ~ "M_FP_C_KD2",
      `Publicerad` > elections$date[5] &
        `Publicerad` < elections$date[6] ~ "S_MP",
      `Publicerad` > elections$date[6] ~ "S_MP_L_C"
    ) %>% as.factor()
  )


# fix names and clean out unnecessary columns
VoterSupportData <-
  VoterSupportData %>% rename(
    Published = Publicerad,
    PollingInstitute = Institut,
    PolicyIndicator = above_collection # go with policy indicator above collection date
  ) %>%
  select(
    PollingInstitute,
    Published,
    StartCollection,
    EndCollection,
    PollYear,
    PollMonth,
    PollWeek,
    Date_numeric,
    PolicyIndicator,
    IncumbentParty,
    Government,
    GovernmentSupport,
    SocialdemokraternaS,
    VänsterpartietV,
    MiljöpartietMP,
    ModeraternaM,
    LiberalernaL,
    CenterpartietC,
    KristdemokraternaKD,
    SverigedemokraternaSD,
    `Övriga partierÖvriga`,
    Unemployment,
    UnemploymentTrend,
    HouseholdConsumption,
    HouseholdConsumptionDetrended,
    ConsumerPriceIndexFixedInterest,
    ConsumerPriceIndex,
    ConsumerConfidence,
    GovtSearch
  )

# write out data in two formats for reuse and sharing
write_csv2(VoterSupportData,
           paste0(here(), "/data/VoterSupportData.csv"))
save(VoterSupportData,
     file = paste0(here(), "/data/VoterSupportData.RData"))

write_csv2(envpols,
           paste0(here(), "/data/ClimatePolicies.csv"))
save(envpols,
     file = paste0(here(), "/data/ClimatePolicies.RData"))

write_csv2(elections,
           paste0(here(), "/data/Elections.csv"))
save(elections,
     file = paste0(here(), "/data/Elections.RData"))
