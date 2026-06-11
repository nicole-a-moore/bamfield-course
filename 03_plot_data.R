# Plot some data

# here, we plot some data

# libraries ---------------------------------------------------------------
library(tidyverse)
library(janitor)

full_alg<-readr::read_csv("data-processed/wizard-island/all_algae_cover_1997-2017.csv") %>%
  clean_names()
full_invts<-readr::read_csv("data-processed/wizard-island/all_invertebrate_cover_1997-2017.csv")%>%
  clean_names()

full_alg_long<-full_alg  %>%
  pivot_longer (cols=c(-date, -site, -exposure, -transect_number, -tidal_height), names_to = "species",
                values_to = "cover")

full_alg_long %>%
  ggplot(aes(x=tidal_height, y=cover, col=species)) +
  geom_point() +
  facet_wrap(~date)
