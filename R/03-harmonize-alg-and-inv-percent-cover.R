# Compare columns in wizard datasets

# here, I manually reformatted 2017 data to reflect the format of
# 1997-2009 data, but some of the columns likely differ. This script
# compares and merges old and new invertebrate and algae data.

# Note that in earlier sampling years, percent cover was recorded
# in addition to density counts in large and small quadrats, but
# in 2017, only percent cover and presence/absence were recorded. 

# libraries ---------------------------------------------------------------
library(dplyr)


# data --------------------------------------------------------------------
inv_2009 <- readr::read_csv("data-raw/wizard-island/doi-10.5683-sp2-vbpbfn/Data/BMSC_Wizard_InvertebrateCover_1997-2009.csv")
inv_2017 <- readxl::read_xlsx("data-processed/wizard-island/wizard2017_manual_reformat.xlsx", sheet = "invertebrate_cover_manual")
inv_2017$Date <- "19-Aug-17"

colnames(inv_2009)
colnames(inv_2017)

colnames(inv_2009)[!colnames(inv_2009) %in% colnames(inv_2017)]
colnames(inv_2017)[!colnames(inv_2017) %in% colnames(inv_2009)]


# remove dead mussel and dead barnacle
inv_2017$`Dead barnacle` <- NULL
inv_2017$`Dead mussel` <- NULL


# harmonize column names --------------------------------------------------
# Chthamalus dalli is spelled wrong in 1997-2009 data ("Cthamalus")
inv_2009 <- inv_2009 %>%
  rename(`Chthamalus dalli` = `Cthamalus dalli`)

# Membranipora membranacea is spelled wrong in 1998-2009 ("Membraniporta")
inv_2009 <- inv_2009 %>%
  rename(`Membranipora membranacea` = `Membraniporta membranacea`)

# Ophlitaspongia pennata is spelled wrong in 2017 ("Ophilitaspongia")
inv_2017 <- inv_2017 %>%
  rename(`Ophlitaspongia pennata` = `Ophilitaspongia pennata`)

# Schizoporella unicornis is spelled wrong in 2017 ("uncornis")
inv_2017 <- inv_2017 %>%
  rename(`Schizoporella unicornis` = `Schizoporella uncornis`)

# Mitella polymerus is outdated in 2017 (should be Pollicipes polymerus)
inv_2017 <- inv_2017 %>%
  rename(`Pollicipes polymerus` = `Mitella polymerus`)

# Notes on Bryozoans: --------------
# 2017 includes Bryozoans:
# - Sertularella spp. (white moss)
# - Abietinaria spp. (course sea fir hydroids)
# - Dendrobeania lichenoides (leaf crust)
# which were not included in earlier sampling, 
# but earlier sampling includes 
# - Bryozoa spp      
# both datasets share:
# - Schizoporella uncornis (red/orange encrusting)
# - Membranipora membranacea (kelp lace)

# possible fixes: 
# 1. sum Sertularella, Abietinaria, and Dendrobeania into "Bryozoa spp"
# or 2. exclude the above 3 and Bryozoan spp
# let's do the former, whatever:
inv_2017 <- inv_2017 %>%
  mutate(`Bryozoa spp` = `Sertularella spp` + `Abietinaria spp` + `Dendrobeania lichenoides`) %>% 
  select(-c(`Sertularella spp`, `Abietinaria spp`, `Dendrobeania lichenoides`)) 


# Notes on Sponges: --------------
# 2017 includes Sponges:
# - "Hymenamphiastra cyanocrypta" 
# - "Undetermined epiphidic white sponge"
# whereas earlier data only includes:
# - "Sponges"
# sum the two species to "sponges" in 2017

inv_2017 <- inv_2017 %>%
  mutate(Sponges = `Hymenamphiastra cyanocrypta` + `Undetermined epiphidic white sponge`) %>%
  select(-c(`Hymenamphiastra cyanocrypta`, `Undetermined epiphidic white sponge`))



# notes on counted species ------------------------------------------------
# in early data, Serpula vermicularis were counted
# but in 2017, they were presence/absence. Remove from data
inv_2009$`Serpula vermicularis` <- NULL


# in early data, Cnemidocarpa finmarkiensis were counted
# but in 2017, they were presence/absence. Remove from data. 
# (also note that they're in the "cover" dataset, but the values are only 0, 1, and 2)
inv_2009$`Cnemidocarpa finmarkiensis` <- NULL

# "Hydrozoa spp" are in early data, but not late. Also low values, unclear of count or percent cover
inv_2009$`Hydrozoa spp` <- NULL

# "Spirorbis spp"  is in early data as percent cover (?) but as pres/abs
# in 2017 (also spelled differently: Spirobis). Remove from percent cover
inv_2009$`Spirorbis spp` <- NULL

# "Tunicates" are in 2009 percent cover data, but tunicates are Pres/Abs in 2017, 
# and separated by individual species. Remove from cover:
inv_2009$`Tunicates` <- NULL


# "Mytilus californianus" is in early data, in addition to "Mytilus spp", but
# only Mytilus spp is in 2017 data. Not sure what we should do about this yet. 
# maybe just add blank column for califnorianus in 2017.
inv_2017$`Mytilus californianus` <- NA

# check colnames again
setequal(colnames(inv_2009), colnames(inv_2017))

# ok, merge datasets ------------------------------------------------------
# now columns should match and be bindable
full <- inv_2009 %>% rbind(inv_2017)
full %>% glimpse()
full %>% distinct(Date)

readr::write_csv(full, 
                 "data-processed/wizard-island/all_invertebrate_cover_1997-2017.csv")

rm(list = ls())


# repeat with algae -------------------------------------------------------

alg_2009 <- readr::read_csv("data-raw/wizard-island/doi-10.5683-sp2-vbpbfn/Data/BMSC_Wizard_AlgalCover_1997-2009.csv")
alg_2017 <- readxl::read_xlsx("data-processed/wizard-island/wizard2017_manual_reformat.xlsx", sheet = "algae_cover_manual")
alg_2017$Date <- "19-Aug-17"

colnames(alg_2009)
colnames(alg_2017)

colnames(alg_2009)[!colnames(alg_2009) %in% colnames(alg_2017)]
colnames(alg_2017)[!colnames(alg_2017) %in% colnames(alg_2009)]

# 2017 separates Fucus d and Fucus s, whereas 2009 has only "Fucus spp"
alg_2017 <- alg_2017 %>%
  mutate(`Fucus spp` = `Fucus distichus` + `Fucus spiralis`) %>% 
  select(-c( `Fucus distichus` , `Fucus spiralis`))

# 2009 calls ulva "ulva spp"
alg_2009 <- alg_2009 %>%
  rename(`Ulva lactuca` = `Ulva spp`) 

# 2017 has "Nemalion vermiculare" whereas 2009 has "Nemalion spp"
alg_2009 <- alg_2009 %>%
  rename(`Nemalion vermiculare` = `Nemalion spp`) 

# 2017 spells Egregia menziesii wrong ("menziessii")
alg_2017 <- alg_2017 %>% 
  rename(`Egregia menziesii` = `Egregia menziessii`)

# 2017 spells "Enteromorpha intestinalis" wrong ("Eteromorpha instestinalis"),
# but actually both should be Ulva intestinalis
alg_2017 <- alg_2017 %>%
  rename(`Ulva intestinalis` = `Eteromorpha instestinalis`)
alg_2009 <- alg_2009 %>%
  rename(`Ulva intestinalis` = `Enteromorpha intestinalis`)

# 2009 includes "Ahnfeltia pacifica", which doesn't seem to exist based on google. 
# I will assume this is "Ahnfeltia fastigiata" as is present in 2017
alg_2009 <- alg_2009 %>%
  rename(`Ahnfeltia fastigiata` = `Ahnfeltia pacifica`)

# 2009 has "Leathesia difformis", which is outdated. 
# should be "Leathesia marina"
alg_2009 <- alg_2009 %>%
  rename(`Leathesia marina` = `Leathesia difformis`)

# 2017 has "Neorhodomela lariz", but should be "Neorhodomela larix"
alg_2017 <- alg_2017 %>%
  rename(`Neorhodomela larix` = `Neorhodomela lariz`)

# 2017 has "Saccharina sessilis" but the valid name should be Hedophyllum sessile
alg_2017 <- alg_2017 %>%
  rename(`Hedophyllum sessile` = `Saccharina sessilis`)

# 2017 calls "Encrusting coralline algae" "Pseudolithophyllum / Spongites (Encrusting coralline algae)"
# so I'll edit here to make it consistent
alg_2017 <- alg_2017 %>%
  rename(`Encrusting coralline algae` = `Pseudolithophyllum / Spongites`)


# the rest I can't really resolve at the moment. 
# lets just get rid of them for now. (6 species in 2009 and 7 species in 2017)
alg_2017[colnames(alg_2017)[!colnames(alg_2017) %in% colnames(alg_2009)]] <- NULL
alg_2009[colnames(alg_2009)[!colnames(alg_2009) %in% colnames(alg_2017)]] <- NULL

full_alg <- alg_2009 %>% rbind(alg_2017)
full_alg %>% glimpse()
full_alg %>% distinct(Exposure)

# save 
readr::write_csv(full_alg,
                 "data-processed/wizard-island/all_algae_cover_1997-2017.csv")


rm(list = ls())
