#### Preamble ####
# Purpose: Process the survey data downloaded from https://www.openicpsr.org/openicpsr/project/117583/version/V1/view
# Author: Wenxuan Li
# Data: 20 Feburary 2022
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Need to install packages `haven` and `tidyverse`


# important libraries
library(haven)
library(tidyverse)
library(here)
# loading data, using r projects
## If we source this function from rmarkdown and simply using rproj, 
## the path might have some problem. Thus, we add the `here` function
here <- here
path1 <- here("inputs/data/oecd_panel.dta")
path2 <- here("inputs/data/wbg_panel_wdi_gender.dta")
path3 <- here("inputs/data/ilo_panel_sel.dta")
raw_oecd <- read_dta(path1)
raw_wbg <- read_dta(path2)
raw_ilo <- read_dta(path3)
# oecd data set selection for education proportion
oecd_edu <- raw_oecd %>%
  filter(year==c(2017)) %>%
  filter(ctry_iso!="ZAF"&ctry_iso!="CHN"&ctry_iso!="BRA"&ctry_iso!="ARG"&ctry_iso!="RUS"&
           ctry_iso!="COL"&ctry_iso!="CRI"&ctry_iso!="IND"&ctry_iso!="IDN"&ctry_iso!="MLT"&
           ctry_iso!="ROU"&ctry_iso!="BGR"&ctry_iso!="HRV"&ctry_iso!="CYP") %>%
  mutate(female_bach_2534 = EDUS_ATT_F_A2534_L6_T,
         female_bach_3544 = EDUS_ATT_F_A3544_L6_T,
         female_bach_4554 = EDUS_ATT_F_A4554_L6_T,
         female_bach_5564 = EDUS_ATT_F_A5564_L6_T,
         male_bach_2534 = EDUS_ATT_M_A2534_L6_T,
         male_bach_3544 = EDUS_ATT_M_A3544_L6_T,
         male_bach_4554 = EDUS_ATT_M_A4554_L6_T,
         male_bach_5564 = EDUS_ATT_M_A5564_L6_T) %>%
  dplyr::select(year,ctry_iso,female_bach_2534,female_bach_3544,female_bach_4554,female_bach_5564,
                male_bach_2534,male_bach_3544,male_bach_4554,male_bach_5564)
# oecd data set selection for stem proportion
oecd_stem <- raw_oecd %>%
  filter(year==c(2016)) %>%
  filter(ctry_iso!="ZAF"&ctry_iso!="CHN"&ctry_iso!="BRA"&ctry_iso!="ARG"&ctry_iso!="RUS"&
           ctry_iso!="COL"&ctry_iso!="CRI"&ctry_iso!="IND"&ctry_iso!="IDN"&ctry_iso!="MLT"&
           ctry_iso!="ROU"&ctry_iso!="BGR"&ctry_iso!="HRV"&ctry_iso!="CYP") %>%
  mutate(female_l5t8 = EDUS_FIELD_F_L5T8_F05T07,
         male_l5t8 = EDUS_FIELD_M_L5T8_F05T07,
         female_l5 = EDUS_FIELD_F_L5_F05T07,
         male_l5 = EDUS_FIELD_M_L5_F05T07,
         female_l6 = EDUS_FIELD_F_L6_F05T07,
         male_l6 = EDUS_FIELD_M_L6_F05T07,
         female_l7 = EDUS_FIELD_F_L7_F05T07,
         male_l7 = EDUS_FIELD_M_L7_F05T07,
         female_l8 = EDUS_FIELD_F_L8_F05T07,
         male_l8 = EDUS_FIELD_M_L8_F05T07) %>%
  dplyr::select(year,ctry_iso,female_l5t8,male_l5t8,female_l5,male_l5,female_l6,male_l6,female_l7,male_l7,female_l8,male_l8)
edu_steam <- full_join(oecd_edu,oecd_stem,by=c("ctry_iso","year"))
# world bank data set processing
wbg_labour <- raw_wbg %>%
  filter(year == c(1990,2017)) %>%
  filter(ctry_iso!="ZAF"&ctry_iso!="CHN"&ctry_iso!="BRA"&ctry_iso!="ARG"&ctry_iso!="RUS"&
           ctry_iso!="COL"&ctry_iso!="CRI"&ctry_iso!="IND"&ctry_iso!="IDN"&ctry_iso!="MLT"&
           ctry_iso!="ROU"&ctry_iso!="BGR"&ctry_iso!="HRV"&ctry_iso!="CYP") %>%
  mutate(log_gdp = log(NY_GDP_PCAP_PP_KD))%>%
  select(ctry_iso,year,log_gdp)
ilo_labour <- raw_ilo %>%
  filter(year == c(1990,2017)) %>%
  filter(ctry_iso!="ZAF"&ctry_iso!="CHN"&ctry_iso!="BRA"&ctry_iso!="ARG"&ctry_iso!="RUS"&
           ctry_iso!="COL"&ctry_iso!="CRI"&ctry_iso!="IND"&ctry_iso!="IDN"&ctry_iso!="MLT"&
           ctry_iso!="ROU"&ctry_iso!="BGR"&ctry_iso!="HRV"&ctry_iso!="CYP") %>%
  mutate(female_labour = EAP_DWAP_R_F_AA2554_GECT_ms,
         male_labour = EAP_DWAP_R_M_AA2554_GECT_ms) %>%
  select(ctry_iso,year,female_labour,male_labour)
gdp_labour <- full_join(wbg_labour,ilo_labour,by=c("ctry_iso","year"))
# combine together
data <- full_join(edu_steam,gdp_labour,by=c("ctry_iso","year"))


