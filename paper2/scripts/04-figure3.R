#### Preamble ####
# Purpose: Plot fig3, Gender Differences in STEM Education (Tertiary Degrees and above)
# Author: Wenxuan Li
# Data: 20 Feburary 2022
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have run the script `01-data_cleaning.R`, which loads the data
# - Need to have run the script `02-figure1.R`, which loads necessary libraries

# filter for proper subset
data_2016 <- data %>%
  filter(year == 2016)
# plot
steaml5tl8 <- ggplot(data_2016, aes(x=male_l5t8, y=female_l5t8)) +
  geom_point() + geom_text_repel(label=data_2016$ctry_iso)+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(name="STEM, male",limits=c(0,50))+
  scale_y_continuous(name="STEM,female",limits=c(0,50))

