# Plot some data
# goal: let's plot species percent cover as a function of time

# here, we plot some data

# libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(janitor)
library(lubridate)

#read in aglage data
full_alg<-readr::read_csv("data-processed/wizard-island/all_algae_cover_1997-2017.csv")

#have a look
full_alg

#hmm, there are a lot of spaces and capitals in the in the column titles, those are going to be annoying.
#let's clean it up with janitor::clean_names
full_alg<-readr::read_csv("data-processed/wizard-island/all_algae_cover_1997-2017.csv") %>%
  clean_names()
#What did clean_names() do?

#let's read in the invertebrate data too
full_invts<-readr::read_csv("data-processed/wizard-island/all_invertebrate_cover_1997-2017.csv")%>%
  clean_names()

#wide vs long format
#reshaping with pivoting data
#Data frames are often described as wide or long. 
#*Wide* when a row has more than one observation, and the units of observation are on one row each
#*Long* when a row has only one observation, but the units of observation are repeated down the column
#more here: (https://datacarpentry.org/R-ecology-lesson/fig/tidyr-pivot_wider_longer.gif)

#let's change change to long format so we can easily plot species patterns across time and space
full_alg_long<-full_alg  %>%
  pivot_longer (cols=c(-date, -site, -exposure, -transect_number, -tidal_height), names_to = "species",
                values_to = "cover")

#let's plot abundance of a species across time - how about "alaria_marginata"
#plot
full_alg_long %>%
  filter(species=="alaria_marginata") %>%
  ggplot(aes(x=date, y=cover)) +
  geom_point() 


#what's going on with those dates?
glimpse(unique(full_alg_long$date))

#the dates are characters - how do we make them numerical?
#let's use the "parse_date_time" function from lubridate package
full_alg_long_time<-full_alg_long %>%
  mutate(datetime_str=parse_date_time(date, orders = "dmy", quiet = TRUE))

#now dates are linear

#let's replot the changes in the same species
full_alg_long_time %>%
  filter(species=="alaria_marginata") %>%
  ggplot(aes(x=datetime_str, y=cover)) +
  geom_point() 

#changes with time!

#what are the years?
unique(full_alg_long_time$datetime_str) #gets unique vales from a vector
#hmm be handy to have a column that is just year


#let's use lubridate to make a column that just pulls out the years
full_alg_long_dy <- full_alg_long_time %>%
  mutate(year=year(ymd(datetime_str))) #uses lubridate "ymd" function to get just year

#here are the unique years
unique(full_alg_long_dy$year)

#now maybe we can plot changes for all species
full_alg_long_dy %>%
  ggplot(aes(x=year, y=(cover))) +
  geom_point() +
  facet_wrap(~species) 



#let's repeat all of this for the inverts

#repeat for animals
#shape data
full_invts_long_dy<-full_invts  %>%
  pivot_longer (cols=c(-date, -site, -exposure, -transect_number, -tidal_height), names_to = "species",
                values_to = "cover")  %>% #makes data long-format
  mutate(datetime_str=parse_date_time(date, orders = "dmy", quiet = TRUE))  %>% #gets date string into a linear vector
  mutate(year=year(ymd(datetime_str))) #gets just year

#plot for each species
full_invts_long_dy %>%
  ggplot(aes(x=year, y=(cover))) +
  geom_point() +
  facet_wrap(~species)

#we reached our goal
#let's talk about these plots
