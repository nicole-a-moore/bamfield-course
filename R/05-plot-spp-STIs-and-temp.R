# Plot species SSTs and Barkley Sound temperature



# libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# data --------------------------------------------------------------------
spp_stis <- readr::read_csv(file = "data-processed/species-thermal-indices/spp-stis.csv")



# plot
p_sti <- spp_stis %>%
  mutate(survey_name = forcats::fct_reorder(survey_name, mean_sst)) %>%
  ggplot(aes(x = survey_name, 
             y = mean_sst)) +
  geom_segment(aes(xend = survey_name,
                   y = q05_sst,
                   yend = q95_sst)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1, 
                                   vjust = 1),
        plot.margin = margin(l = 40,10,10,10)) +
  labs(x = "Species",
       y = "Thermal Index",
       title = "Wizard Island Thermal Indices") +
  scale_y_continuous(labels = ~paste0(., "°"))



# get wizard temp ---------------------------------------------------------
temp <- readr::read_csv("data-processed/env-data/BarkleySound_monthly-sst_1deg-resolution.csv")

yearly_temp <- temp %>%
  group_by(year) %>%
  summarize(mean_sst = mean(sst),
            max_sst = max(sst),
            min_sst = min(sst))


p_temp <- yearly_temp %>% 
  tidyr::pivot_longer(cols = c(mean_sst, max_sst, min_sst),
                      names_to = "var",
                      values_to = "sst") %>%
  ggplot(aes(x = year, y = sst, color = var)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", linetype = "dashed", show.legend = F, alpha = .25) +
  scale_color_manual(values = c("tomato2", "grey40","cornflowerblue"),
                     labels = c("Hottest Month",
                                "Mean of Months",
                                "Coldest Month")) + 
  labs(title = "Barkley Sound SST",
       x = "Year",
       y = "SST",
       color = "Temp. Var") +
  scale_y_continuous(labels = ~paste0(., "°C")) 



# find mean of min, mean, max temp ----------------------------------------
temp_means <- yearly_temp %>%
  tidyr::pivot_longer(cols = c(mean_sst, max_sst, min_sst),
                      names_to = "var",
                      values_to = "sst") %>%
  group_by(var) %>%
  summarize(mean_temp = mean(sst))

p_sti + 
  geom_hline(data = temp_means,
             aes(yintercept = mean_temp,
                 color = var),
             linetype = "dashed") + 
  scale_color_manual(values = c("tomato2", "grey40","cornflowerblue"),
                     labels = c("Hottest Month",
                                "Mean of Months",
                                "Coldest Month")) +
  labs(color = "Timeseries Avg.\nTemps (1993-2025)")
