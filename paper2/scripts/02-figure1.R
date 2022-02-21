#### Preamble ####
# Purpose: Plot fig1, Shares of Men and Women with at Least a Bachelor’s Degree in different age ranges
# Author: Wenxuan Li
# Data: 20 Feburary 2022
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have run the script `01-data_cleaning.R`, which loads the data
# - Need to install packages `ggplot2`, `ggrepel`, `ggpubr`

# import libraries
library(ggplot2)
library(ggrepel)
library(ggpubr)
# filter for proper subset
data_2017 <- data %>%
  filter(year == 2017)
# plot
edu2534 <- ggplot(data_2017, aes(x=male_bach_2534, y=female_bach_2534)) +
  geom_point() + geom_text_repel(label=data_2017$ctry_iso)+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(name="Share with bachelor’s, male(25,34)",limits=c(0,50))+
  scale_y_continuous(name="Share with bachelor’s, female(25,34)",limits=c(0,50))
edu3544 <- ggplot(data_2017, aes(x=male_bach_3544, y=female_bach_3544)) +
  geom_point() + geom_text_repel(label=data_2017$ctry_iso)+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(name="Share with bachelor’s, male(35,44)",limits=c(0,50))+
  scale_y_continuous(name="Share with bachelor’s, female(35,44)",limits=c(0,50))
edu4554 <- ggplot(data_2017, aes(x=male_bach_4554, y=female_bach_4554)) +
  geom_point() + geom_text_repel(label=data_2017$ctry_iso)+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(name="Share with bachelor’s, male(45,54)",limits=c(0,50))+
  scale_y_continuous(name="Share with bachelor’s, female(45,54)",limits=c(0,50))
edu5564 <- ggplot(data_2017, aes(x=male_bach_5564, y=female_bach_5564)) +
  geom_point() + geom_text_repel(label=data_2017$ctry_iso)+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(name="Share with bachelor’s, male(55,64)",limits=c(0,50))+
  scale_y_continuous(name="Share with bachelor’s, female(55,64)",limits=c(0,50))

