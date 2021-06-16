# code to replicate Supplementary Figure 1 of Oliver et al. 2021
# created by Ruth Oliver
# 2021.06.16

# load libraries
require(dplyr)
library(tidyverse)
library(data.table)
library(foreign)
library(broom)
library(scales)

rm(list=ls())

# set directory where associated data is stored
data_dir <- "~/Desktop/data/"

# read in annual national values
taxa_data <- fread(paste0(data_dir,"supporting-data-national-values.csv"))

# set time period for summarizing recent period
year_start <- 2010
year_end <- 2019

# function for simplifying linear regression results
clean_results <- function(results) {
  results = tidy(results, model)
  
  results <- results %>%
    filter(term == "year") %>%
    mutate("slope_sig" = ifelse(p.value > 0.01, NA,estimate)) %>%
    rename("slope" = estimate) %>%
    select(country, slope, slope_sig)
  
  return(results)
}

# find recent change rates in Steward's SSII 
ssii_change <- taxa_data %>% 
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>% 
  do(model = lm(stewards_ssii ~ year, data = .)) %>%
  clean_results(.)


# find recent change rates in National SSEI 
ssei_change <- taxa_data %>% 
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>% 
  filter(sum(!is.na(national_ssei)) > 5) %>%
  do(model = lm(national_ssei ~ year, data = .)) %>%
  clean_results(.)


taxa_summary <- taxa_data %>%
  # find mean national values over recent period
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country, ISO3) %>%
  summarise(national_ssii = mean(national_ssii, na.rm = TRUE),
            stewards_ssii = mean(stewards_ssii, na.rm = TRUE),
            national_ssei = mean(national_ssei, na.rm = TRUE),
            n_records_total = mean(n_records, na.rm = TRUE),
            prop_species_total = mean(prop_species_total, na.rm = TRUE)) %>%
  # join with information on change rates over recent period
  left_join(.,ssii_change, by = "country") %>%
  select(-slope_sig) %>%
  rename("stewards_ssii_change" = slope) %>%
  left_join(.,ssei_change, by = "country") %>%
  select(-slope_sig) %>%
  rename("national_ssei_change" = slope)

# join summary data to national boundary spatial object for plotting
#w <- left_join(gadm,taxa_summary, by = c("NAME_0"="country")) 

ssii_label <- "Steward's SSII"
ssei_label <- "National SSEI"
records_label <- "National species records (n)"
species_label <- "National species recorded (%)"


# function for axis tick labels
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) {
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x} else {
        x[1:nth != 1]}} else {
          if(empty) {
            x[1:nth != 1] <- ""
            x} else {
              x[1:nth == 1]}}}



# plot Supplementary Figure 1a: Steward's SSII and National SSEI (2010-2019)
sfig1a <- ggplot(data = taxa_summary,aes(x = stewards_ssii_change, y = national_ssei_change, label = ISO3)) +
  geom_text(size = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(x = paste0(ssii_label," change rate (",year_start,"-",year_end,")"),
       y = paste0(ssei_label," change rate (",year_start,"-",year_end,")"),
       tag = expression(bold("a"))) 

# plot Supplementary Figure 1b: Number of species recorded and Steward's SSII (2010-2019)
sfig1b <- ggplot(data = taxa_summary,aes(x = n_records_total, y = stewards_ssii, label = ISO3)) +
  geom_text(size = 2) +
  scale_x_continuous(trans = "log10", labels = comma) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)),
                     breaks = seq(0,1,0.01),
                     labels = every_nth(seq(0,1,0.01),5,inverse = TRUE)) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(x = paste0(records_label," (",year_start,"-",year_end,")"),
       y = paste0(ssii_label," (",year_start,"-",year_end,")"),
       tag = expression(bold("b"))) 

# plot Supplementary Figure 1c: Percent of species recorded and Steward's SSII (2010-2019)
sfig1c <- ggplot(data = taxa_summary,aes(x = prop_species_total*100, y = stewards_ssii, label = ISO3)) +
  geom_text(size = 2) +
  scale_x_continuous(breaks = seq(0,100,5),
                     labels = every_nth(seq(0,100,5),2,inverse = TRUE)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)),
                     breaks = seq(0,1,0.01),
                     labels = every_nth(seq(0,1,0.01),5,inverse = TRUE)) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(x = paste0(species_label," (",year_start,"-",year_end,")"),
       y = paste0(ssii_label," (",year_start,"-",year_end,")"),
       tag = expression(bold("c"))) 

# plot Supplementary Figure 1d: Percent of species recorded and National SSEI (2010-2019)
sfig1d <- ggplot(data = taxa_summary, aes(x = prop_species_total*100, y = national_ssei, label = ISO3)) +
  geom_text(size = 2) +
  scale_x_continuous(breaks = seq(0,100,5),
                     labels = every_nth(seq(0,100,5),2,inverse = TRUE)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     breaks = seq(0,1,0.01),
                     labels = every_nth(seq(0,1,0.01),5,inverse = TRUE)) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(x = paste0(species_label," (",year_start,"-",year_end,")"),
       y = paste0(ssei_label," (",year_start,"-",year_end,")"),
       tag = expression(bold("d"))) 
