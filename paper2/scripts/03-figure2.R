#### Preamble ####
# Purpose: Plot fig2, Female and male labor force participation around the world
# Author: Wenxuan Li
# Data: 20 Feburary 2022
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have run the script `01-data_cleaning.R`, which loads the data
# - Need to have run the script `02-figure1.R`, which loads necessary libraries

# change data format
data_labour <- data %>%
  filter(year == c(1990,2017)) %>%
  mutate(year = as.factor(year))
# plot 
female_labour <- ggplot(data_labour, aes(x=log_gdp, y=female_labour,shape=year,color=year)) +
  geom_point() + geom_smooth()+
  scale_x_continuous(name="ln(GDP per capita)")+
  scale_y_continuous(name="Female labor force participation (25,54)")
male_labour <- ggplot(data_labour, aes(x=log_gdp, y=male_labour,shape=year,color=year)) +
  geom_point() + geom_smooth()+
  scale_x_continuous(name="ln(GDP per capita)")+
  scale_y_continuous(name="Male labor force participation (25,54)")
ggarrange(female_labour,male_labour,label=c("Female","Male"),nrow=1,ncol=2)
