# code to replicate Figure 3 of Oliver et al. 
# created by Ruth Oliver
# 2021.06.16

# load libraries
require(dplyr)
library(tidyverse)
library(data.table)
library(cowplot)
library(scales)

rm(list=ls())

# set directory where associated data is stored
data_dir <- "~/Desktop/data/"

# read in annual taxa replicate data
all_data <- fread(paste0(data_dir,"supporting-data-figure3-replicates.csv"))

# read in annual taxa summary data
summary_data <- fread(paste0(data_dir,"supporting-data-figure3.csv"))

# reset taxa factor order
summary_data$taxa <- factor(summary_data$taxa, levels = c("birds", "mammals", "amphibians", "reptiles"))

### plotting details
# axis labels
coverage_label <- "SSII"
records_label <- "Annual global species records (n)"
species_label <- "Annual global species recorded (%)"
entropy_label <- "SSEI"
# set date range
year_end <- 2019
# set colors
col_birds <- "#064789"
col_mammals <- "#FF934F"
col_amphs <- "#8E5572"
col_reps <- "#679436"

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

fig3a <- ggplot(data = summary_data) +
  geom_point(aes(x = year,y = n_records, group = taxa, color = taxa), size = 0.5) +
  geom_line(aes(x = year,y = n_records, group = taxa, color = taxa), show.legend = FALSE) +
  scale_color_manual(values = c(col_birds,col_mammals,col_amphs,col_reps),
                     )+
  scale_y_continuous(trans = "log10", labels = comma) +
  scale_x_continuous(limits = c(1950, year_end),
                     breaks = seq(1950,2020,by = 2),
                     labels = every_nth(seq(1950,2020,by = 2),5, inverse = TRUE),
                     expand = expansion(mult = c(0, 0.01))) +
  guides(color = guide_legend(override.aes = list(size= 5))) +
  theme_cowplot() +
  theme(legend.position = c(0.04, 0.8),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9),
        axis.text.y  = element_text(size = 6),
        axis.text.x = element_text(size = 8)) +
  labs(x = "", y = records_label,tag = expression(bold("a"))) 


fig3b <- ggplot(data = summary_data) +
  geom_point(aes(x = year,y = prop_species*100, group = taxa, color = taxa), size = 0.5) +
  geom_line(aes(x = year,y = prop_species*100, group = taxa, color = taxa)) +
  scale_color_manual(values = c(col_birds,col_mammals,col_amphs,col_reps))+
  scale_y_continuous(breaks = seq(0,100,5),
                     labels = every_nth(seq(0,100,5),2,inverse = TRUE)) +
  scale_x_continuous(limits = c(1950, year_end),
                     breaks = seq(1950,2020,by = 2),
                     labels = every_nth(seq(1950,2020,by = 2),5, inverse = TRUE),
                     expand = expansion(mult = c(0.01, 0.01))) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(x = "", y = species_label,tag = expression(bold("b"))) 


fig3c <- ggplot(data = all_data) +
  stat_summary(aes(x = year, y = ssii, group = taxa, color = taxa),geom = "line", fun = mean) +
  stat_summary(aes(x = year, y = ssii, group = taxa, fill = taxa),geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.2) +
  scale_color_manual(values = c(col_amphs,col_birds,col_mammals,col_reps)) +
  scale_fill_manual(values = c(col_amphs,col_birds,col_mammals,col_reps)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01)),
                     breaks = seq(0,1,0.01),
                     labels = every_nth(seq(0,1,0.01),2,inverse = TRUE)) +
  scale_x_continuous(limits = c(1950, year_end),
                     breaks = seq(1950,2020,by = 2),
                     labels = every_nth(seq(1950,2020,by = 2),5, inverse = TRUE),
                     expand = expansion(mult = c(0.01, 0.01))) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  guides(colour = guide_legend(override.aes = list(alpha = c(0.1)))) +
  labs(x = " ", y = coverage_label, tag = expression(bold("c"))) 


fig3d <- ggplot(data = summary_data) +
  geom_point(aes(x = n_records, y = ssii, group = taxa, color = taxa), size = 0.5) +
  scale_color_manual(values = c(col_birds,col_mammals,col_amphs,col_reps),
                     labels = c(" "," "," "," "," "))+
  scale_x_continuous(trans = "log10", labels = comma) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01)),
                     breaks = seq(0,1,0.01),
                     labels = every_nth(seq(0,1,0.01),2,inverse = TRUE)) +
  theme_cowplot() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text.x  = element_text(size = 6),
        axis.title = element_text(size = 9),
        axis.text.y = element_text(size = 8)) +
  labs(x = records_label, y = coverage_label,tag = expression(bold("d"))) 


fig3e <- ggplot(data = all_data) +
  stat_summary(aes(x = year, y = ssei, group = taxa, color = taxa),geom = "line", fun = mean) +
  stat_summary(aes(x = year, y = ssei, group = taxa, fill = taxa),geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.2) +
  scale_color_manual(values = c(col_amphs,col_birds,col_mammals,col_reps)) +
  scale_fill_manual(values = c(col_amphs,col_birds,col_mammals,col_reps)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01)),
                     breaks = seq(0,1,0.01),
                     labels = every_nth(seq(0,1,0.01),2,inverse = TRUE)) +
  scale_x_continuous(limits = c(1950, year_end),
                     breaks = seq(1950,2020,by = 2),
                     labels = every_nth(seq(1950,2020,by = 2),5, inverse = TRUE),
                     expand = expansion(mult = c(0.01, 0.01))) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  guides(colour = guide_legend(override.aes = list(alpha = c(0.1)))) +
  labs(x = " ", y = entropy_label, tag = expression(bold("e"))) 


fig3f <- ggplot(data = summary_data) +
  geom_point(aes(x = prop_species*100, y = ssei, group = taxa, color = taxa), size = 0.5) +
  scale_color_manual(values = c(col_birds,col_mammals,col_amphs,col_reps))+
  scale_x_continuous(breaks = seq(0,100,5),
                     labels = every_nth(seq(0,100,5),2,inverse = TRUE)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01)),
                     breaks = seq(0,1,0.01),
                     labels = every_nth(seq(0,1,0.01),2,inverse = TRUE)) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(x = species_label, y = entropy_label,tag = expression(bold("f"))) 

