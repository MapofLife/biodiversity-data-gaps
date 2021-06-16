# code to replicate Figure 2 of Oliver et al. 2021
# created by Ruth Oliver
# 2021.06.16

# load libraries
require(dplyr)
library(tidyverse)
library(data.table)

rm(list=ls())

# set directory where associated data is stored
data_dir <- "~/Desktop/data/"

# read in species level data for example species 
# (jaguar and collared peccary)
taxa_data <- fread(paste0(data_dir,"supplementary-table-1.csv")) %>%
  rename(n_records = "Records (n)") %>%
  rename_all(tolower)

# read in national level data for example species in sample countries 
# (Brazil, Colombia, Costa Rica, Mexico)
national_data <- fread(paste0(data_dir,"supplementary-table-2.csv")) %>%
  rename(national_ssii = "National SSII",
         stewards_ssii = "Steward's SSII",
        national_ssei = "National SSEI") %>%
  rename_all(tolower) %>%
  select(country, year, national_ssii, stewards_ssii, national_ssei)


# function for axis tick labels
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) {
  if (!inverse) {
    if(empty) {x[1:nth == 1] <- ""
      x} else {
        x[1:nth != 1]}} else {
          if(empty) {x[1:nth != 1] <- ""
            x} else {x[1:nth == 1]}}}

breaks <- 2000:2019
labels <- as.character(breaks)
labels[!(breaks%%5==0)] <- ''

# plot Fig. 2c: Number of records collected for example species (2000-2019)
fig2c <- ggplot(data = taxa_data) +
  geom_line(aes(x = year, y = n_records, group = species, color = species)) +
  geom_point(aes(x = year, y = n_records, group = species, color = species)) +
  scale_color_manual(values = c("#BD93BD","#D7AF70")) +
  
  scale_y_continuous( expand = expansion(mult = c(0.01, 0.05))) +
  scale_x_continuous(limits = c(2000, 2019),
                     breaks = seq(2000, 2019,by = 1),
                     labels = every_nth(seq(2000, 2019,by = 1),5, inverse = TRUE),
                     expand = expansion(mult = c(0, 0.01))) +
  theme_cowplot() +
  theme(legend.position = c(0.01, 0.9),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(x = " ", y = "Records (n)",tag = expression(bold("c"))) 

# plot Fig. 2d: SSII for example species (2000-2019)
fig2d <- ggplot(data = taxa_data) +
  geom_line(aes(x = year, y = ssii, group = species, color = species)) +
  geom_point(aes(x = year, y = ssii, group = species, color = species)) +
  scale_color_manual(values = c("#BD93BD","#D7AF70")) +
  
  scale_y_continuous( expand = expansion(mult = c(0.01, 0.05))) +
  scale_x_continuous(limits = c(2000, 2019),
                     breaks = seq(2000, 2019,by = 1),
                     labels = every_nth(seq(2000, 2019,by = 1),5, inverse = TRUE),
                     expand = expansion(mult = c(0, 0.01))) +
  theme_cowplot() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(x = " ", y = "SSII",tag = expression(bold("d"))) 

# plot Fig. 2e: SSEI for example species (2000-2019)
fig2e <- ggplot(data = taxa_data) +
  geom_line(aes(x = year, y = ssei, group = species, color = species)) +
  geom_point(aes(x = year, y = ssei, group = species, color = species)) +
  scale_color_manual(values = c("#BD93BD","#D7AF70")) +
  
  scale_y_continuous( expand = expansion(mult = c(0.01, 0.05))) +
  scale_x_continuous(limits = c(2000, 2019),
                     breaks = seq(2000, 2019,by = 1),
                     labels = every_nth(seq(2000, 2019,by = 1),5, inverse = TRUE),
                     expand = expansion(mult = c(0, 0.01))) +
  theme_cowplot() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(x = " ", y = "SSEI",tag = expression(bold("e"))) 

# plot Fig. 2f: National SSII for example species in sample countries (2000-2019)
fig2f <- ggplot(data = national_data) +
  geom_line(aes(x = year, y = national_ssii, color = country, group = country)) +
  geom_point(aes(x = year, y = national_ssii, color = country, group = country)) +
  geom_line(aes(x = year, y = stewards_ssii, color = country,group = country),lty = 3) +
  scale_color_manual(values = c("#679436","#246EB9","#A33E57","#62929E")) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  scale_x_continuous(limits = c(2000, 2019),
                     breaks = seq(2000, 2019,by = 1),
                     labels = every_nth(seq(2000, 2019,by = 1),5, inverse = TRUE),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(breaks = seq(0, 1,by = 0.1),
                     expand = expansion(mult = c(0.01, 0.05))) +
  labs(x = " ", y = "SSII",tag = expression(bold("f"))) 

# plot Fig. 2g: National SSII for example species in sample countries (2000-2019)
fig2g <- ggplot(data = national_data) +
  geom_line(aes(x = year, y = national_ssei, color = country, group = country)) +
  geom_point(aes(x = year, y = national_ssei, color = country, group = country)) +
  scale_color_manual(values = c("#679436","#246EB9","#A33E57","#62929E")) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  scale_x_continuous(limits = c(2000, 2019),
                     breaks = seq(2000, 2019,by = 1),
                     labels = every_nth(seq(2000, 2019,by = 1),5, inverse = TRUE),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(breaks = seq(0, 1,by = 0.1),
                     expand = expansion(mult = c(0.01, 0.05))) +
  labs(x = " ", y = "SSEI",tag = expression(bold("g"))) 