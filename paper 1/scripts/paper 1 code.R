## import libraries
library(tidyverse)
library(opendatatoronto)
library(dplyr)
library(knitr)

## import the data
# get package
package <- show_package("058236d2-d26e-4622-9665-941b9e7a5229")
# get all resources for this package
resources <- list_package_resources("058236d2-d26e-4622-9665-941b9e7a5229")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()

## data process
data_proc <- data %>%
  mutate(speed_median = as.numeric(pct_50),
         speed_over40 = (as.numeric(spd_40)+as.numeric(spd_45)+as.numeric(spd_50)+as.numeric(spd_55)+as.numeric(spd_60)+as.numeric(spd_65)+as.numeric(spd_70)+as.numeric(spd_75)+as.numeric(spd_80)+as.numeric(spd_85)+as.numeric(spd_90)+as.numeric(spd_95)+as.numeric(spd_100_and_above))/as.numeric(volume),
         speed_over100 = as.numeric(spd_100_and_above)/as.numeric(volume),
         volume = as.factor(case_when(as.numeric(volume)<=10000 ~1,
                                      as.numeric(volume)<=30000 ~2,
                                      as.numeric(volume)<=60000 ~3,
                                      as.numeric(volume)<=80000 ~4,
                                      as.numeric(volume)>80000 ~5))) %>%
  select(speed_median,speed_over40,speed_over100,volume)

## summary statistics for numerical variables
speed_median_sum <- summary(data_proc$speed_median)
speed_over40_sum <- summary(data_proc$speed_over40)
speed_over100_sum <- summary(data_proc$speed_over100)
data_sum <- data.frame(cbind(speed_median_sum,speed_over40_sum,speed_over100_sum))
colnames(data_sum) <- c("Median","Over 40 km/h","Over 100 km/h")
rownames(data_sum) <- c("Minimum","1st quantile","Median","Mean","3rd quantile","Maximum")
kable(data_sum,caption="Summary statistics for variable of interest")

## figures
# figure 1: histograms for median speeds
ggplot(data_proc, aes(x=speed_median)) + geom_histogram() + geom_histogram(binwidth=1)

# figure 2: histograms for the proportion of vehicles larger than 40 km/h of different volume level
ggplot(data_proc, aes(x=speed_over40, color=volume)) +
  geom_histogram(fill="white")

# figure 3: histograms for the proportion of vehicles larger than 100 km/h of different volume level
ggplot(data_proc, aes(x=speed_over100, color=volume)) +
  geom_histogram(fill="white")




