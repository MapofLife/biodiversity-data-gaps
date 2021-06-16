# code to replicate Supplementary Figure 2 of Oliver et al. 2021
# created by Ruth Oliver
# 2021.06.16

# load libraries
require(dplyr)
library(tidyverse)
library(data.table)
library(foreign)
library(sf)
library(cowplot)

rm(list=ls())

# set directory where associated data is stored
data_dir <- "~/Desktop/data/"

# read in annual national values
taxa_data <- fread(paste0(data_dir,"supporting-data-national-values.csv"))

# read in national boundaries for plotting purposes
# available at gadm.org
gadm = st_read(dsn = "~/Desktop/")

# set time period for summarizing recent period
year_start <- 2010
year_end <- 2019

# summarize National and Steward's SSII over recent period
taxa_summary <- taxa_data %>%
  # find mean national values over recent period
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>%
  summarise(stewards_ssii = mean(stewards_ssii, na.rm = TRUE),
            national_ssii = mean(national_ssii, na.rm = TRUE)) %>%
  # find percent difference
  mutate("diff" = (stewards_ssii - national_ssii)/national_ssii*100)

# summarize percent difference into categories
taxa_summary$diff_brks <- cut(taxa_summary$diff, breaks = c(-100,-50,-25,0,25,50,100,400))

# join to national boundaries for plotting
w <- left_join(gadm,taxa_summary, by = c("NAME_0"="country"))

# plot Supplementary Figure 2a: comparisons of National and Steward's SSII
sfig2a <- ggplot(data = taxa_summary) +
  geom_point(aes(x = national_ssii, y = stewards_ssii, col = diff_brks), alpha = 0.5) +
  scale_color_manual(values = c("#EC4E20","#F08700","#FFBF00","#55C1FF","#5887FF","#3956A3","#102E4A")) +
  geom_abline(col = "grey30", linetype = "dashed") + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  scale_x_continuous(limits = c(0,max(c(taxa_summary$national_ssii,taxa_summary$stewards_ssii))),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(0,max(c(taxa_summary$national_ssii,taxa_summary$stewards_ssii))),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "National SSII (2010-2019)", y = "Steward's SSII (2010-2019)", tag = expression(bold("a")),color = "Difference (%)")


# plot Supplementary Figure 2b: map of comparisons of National and Steward's SSII
sfig2b <-ggplot(data = w) +
  geom_sf(aes(fill = diff_brks), colour = "transparent",size = 0.001) +
  scale_fill_manual(values = c("#EC4E20","#F08700","#FFBF00","#55C1FF","#5887FF","#3956A3","#102E4A")) +
  
  theme(legend.position = "none", 
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  coord_sf(datum = NA) +
  labs(tag = expression(bold("b")))
