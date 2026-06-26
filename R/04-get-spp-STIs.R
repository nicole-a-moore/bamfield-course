# calculate thermal indexes for species



# libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
# install if needed:
#remotes::install_github("ropensci/taxize")
library(taxize)
library(worrms)
library(robis)
library(stringr)
library(sf)

# upload species list -----------------------------------------------------
# algae --------
full_alg<-readr::read_csv("data-processed/wizard-island/all_algae_cover_1997-2017.csv") %>%
  clean_names()
# pivot so species are in one column
full_alg <-  full_alg %>%
  pivot_longer (cols=c(-date, -site, -exposure, -transect_number, -tidal_height), names_to = "species",
                values_to = "cover")
# pull species
alg_spp <- full_alg %>% 
  distinct(species) %>% 
  pull() %>%
  # capitalize first letter (genus)
  str_to_sentence() %>%
  # change underscore to space
  str_replace_all(.,"_"," ")

# invertebrates -----------
full_invts<-readr::read_csv("data-processed/wizard-island/all_invertebrate_cover_1997-2017.csv")%>%
  clean_names()
# piot
full_invts <- full_invts %>%
  pivot_longer (cols=c(-date, -site, -exposure, -transect_number, -tidal_height), names_to = "species",
                values_to = "cover")
# pull species
invts_spp <- full_invts %>% 
  distinct(species) %>% 
  pull() %>%
  # capitalize first letter (genus)
  str_to_sentence() %>%
  # change underscore to space
  str_replace_all(.,"_"," ")

# merge ------- 
spp <- c(alg_spp, invts_spp) %>% sort()
rm(full_alg, full_invts, alg_spp, invts_spp)

# filter out one-word names
spp <- spp[spp %>% str_detect(., " ")]
# filter out anything with "spp"
spp <- spp[!spp %>% str_detect(., "spp")]



# validate spp names ------------------------------------------------------
# use taxize package to confirm all names are correct
spp_clean <- taxize::gna_verifier(spp) %>%  
  # keep exact or fuzzy matches
  filter(matchType %in% c("Exact","Fuzzy")) %>% 
  # get cleaned names
  pull(matchedCanonicalSimple)

# view the differences
spp[!spp %in% spp_clean] 
spp_clean[!spp_clean %in% spp]
# we lost 3 that aren't exact species, and one was spelled wrong.
rm(spp)



# get APHIA IDs -----------------------------------------------------------
aphiaids <- vector(mode = "list", length = length(spp_clean))
names(aphiaids) <- spp_clean

for(i in spp_clean){
  
  id <- wm_records_name(i) %>%
    select(scientificname, valid_name, valid_AphiaID ) 
  
  # if more than one row (usually because of variants),
  # keep only the matching one
  if(nrow(id) > 1){
    id <- id %>%
      filter(scientificname == i)
  }

  aphiaids[[i]] <- id
  
  print(paste(i, id$valid_AphiaID))
  
}

# bind 
aphiaids_bind <- aphiaids %>%
  bind_rows()


# get obis records --------------------------------------------------------
# go through every species and pull all occurrences for the valid aphiaID
# this will be a bit slow
obis_recs_raw <- purrr::map(
  .x = aphiaids_bind$valid_AphiaID,
  .f = ~.x %>% occurrence(taxonid = ., 
                          enddate = as.Date("2026-06-01")) %>%
    as_tibble(),
  .progress = T
)
names(obis_recs_raw) <- aphiaids_bind$valid_AphiaID

lapply(obis_recs_raw, nrow) %>% unlist() %>% unname()
obis_recs_raw[[1]] %>% glimpse()



# create a cleaned occurrence dataset
obis_recs_filtered <- purrr::map(
  .x = obis_recs_raw,
  .f = ~.x %>%
    select(species,speciesid,  date_year, decimalLatitude, decimalLongitude, depth, sst) %>%
    filter(!is.na(date_year)) %>%
    filter(!is.na(sst))
)
rm(obis_recs_raw)


# plot a few --------------------------------------------------------------
sf_use_s2(F)
world <- rnaturalearth::ne_countries(returnclass = "sf") %>% st_union()
i <- 11
ggplot() +
  geom_sf(data = world,
          color = "transparent") +
  geom_point(data = obis_recs_filtered[[i]],
             aes(x = decimalLongitude,
                 y = decimalLatitude),
             alpha = .1,
             color = "blue",
             shape = 16
             ) +
  labs(title = unique(obis_recs_filtered[[i]]$species),
       subtitle = paste0("n records: ", scales::comma(nrow(obis_recs_filtered[[i]]))))



# summarize STI -----------------------------------------------------------
# OBIS records come with an sst value that is derived from BioOracle,
# which features gridded long-term climatologies. While we could improve upon
# this, it is fine enough to derive thermal indices for species for this purpose.
# see here: https://obis.org/data/access/?utm_source

spp_sti <- purrr::map(
  .x = obis_recs_filtered,
  .f = ~.x %>% 
    summarize(valid_name = unique(species),
              valid_AphiaID = unique(speciesid),
              mean_sst = mean(sst),
              max_sst = max(sst),
              min_sst = min(sst),
              q95_sst = quantile(sst, .95),
              q05_sst = quantile(sst, .05),
              q50_sst = quantile(sst, .5))
) %>%
  bind_rows()


# bind back to original species names -------------------------------------
# remember that some of the species' names changed when finding the aphiaIDs.
# in order to make them interoperable to our dataset, let's merge back to the 
# old names that are present in our surveys.

spp_stis_join <- aphiaids_bind %>% 
  rename(survey_name = scientificname) %>%
  left_join(spp_sti)

spp_stis_join$survey_name %in% spp_clean %>% table()
spp_clean %in% spp_stis_join$survey_name %>% table()


# plot
spp_stis_join %>%
  mutate(survey_name = forcats::fct_reorder(survey_name, mean_sst)) %>%
  ggplot(aes(x = survey_name, 
             y = mean_sst)) +
  geom_segment(aes(xend = survey_name,
                   y = q05_sst,
                   yend = q95_sst)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1, 
                                   vjust = 1)) +
  labs(x = "Species",
       y = "Thermal Index"
       ) +
  scale_y_continuous(labels = ~paste0(., "°"))


# save csv
readr::write_csv(spp_stis_join,
                 file = "data-processed/species-thermal-indices/spp-stis.csv")



