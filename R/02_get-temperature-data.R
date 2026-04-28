## this script downloads sea surface temperature data for Wizard island and other sites around Barkley sound 
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(rerddap) # For easily downloading subsets of data
setwd(here::here())


#### TO LOOK INTO:
# Air temperature data were taken from Cape Beale Lighthouse at the southern opening of Barkley Sound, and SST data were taken from Amphitrite Lighthouse at the northern opening of Barkley Sound.


# ----------------- prepare coordinates ---------------------------
## read in the Barkley Sound site summary data and create a unique list of latitude and longitude coordinates of sites 
bs_sites <- read.csv("data-raw/barkley-sound/SiteDataSummary_pone.0213191.s001.csv")
colnames(bs_sites)

bs_sites = select(bs_sites, Location, Latitude, Longitude) %>%
  distinct()

## now get the coordinates of the Wizard Island site
## from Starko et al: 48.857983N, 125.160793W
wi_site = data.frame(Location = "Wizard Island", Latitude = 48.857983, Longitude = -125.160793)

## combine the data 
site = rbind(bs_sites, wi_site)


# ---------- get sea surface temperatures ------------------------
## begin by finding out the Dataset ID of the data you want to access:
## https://coastwatch.pfeg.noaa.gov/erddap/index.html

## load the info for your chosen NOAA OISST dataset using the Dataset ID
info <- info(datasetid = "erdHadISST")
## let's try the HadISST Average Sea Surface Temperature dataset
## this data is sampled on a 1° global grid monthly from 1870 - present

## download the data in a specific geographic location for a specific time period
## since our first site was sampled in 1993 and our last site was sampled in 2017, let's get data for between these years
time_series <- griddap(info,
                       time = c("1993-01-01", "2018-12-31"), ## choose the times you want data between
                       latitude = c(min(site$Latitude), max(site$Latitude)), ## choose the maximum and minimum lat and lon you want data for
                       longitude = c(min(site$Longitude), max(site$Longitude)),
                       url = "https://upwell.pfeg.noaa.gov/erddap")

## get rid of times with no data 
data <- time_series$data[time_series$data$sst > -999,]

## reformat the time column into date, year and month columns 
data$date <- lubridate::as_date(data$time)
data$year <- lubridate::year(data$date)
data$month <- lubridate::month(data$date)
data <- data %>% select(-time)

range(data$date)

## plot the time series of data 
data %>%
  ggplot(aes(x = date, y = sst)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "Temp (°C)") +
  theme_bw()

## save as csv
write.csv(data, "data-processed/env-data/BarkleySound_monthly-sst_1deg-resolution.csv", row.names = F)

# ----------------- challenge: summarize the data into yearly sst estimates  ---------------------------

# ----------------- solution  ---------------------------
data_annual = data %>%
  group_by(latitude, longitude, year) %>%
  summarise(mean_annual_sst = mean(sst))



