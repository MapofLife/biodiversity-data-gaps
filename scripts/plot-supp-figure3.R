# code to replicate Supplementary Figure 3 of Oliver et al. 2021
# created by Ruth Oliver
# 2021.06.16

# load libraries
library(ggplot2) 
require(dplyr)
library(tidyverse)
library(data.table)
library(cowplot)

rm(list=ls())

# set directory where associated data is stored
data_dir <- "~/Desktop/data/"

# read in annual SSII and SSEI values for 2 example species
coverage <- fread(paste0(data_dir,"supporting-data-supp-figure3c.csv"))

# reformat results for plotting purposes
# separate results for 3 spatial grains: 110, 55, 27.5 km
# 360 = 110 x 100 km
# 720 = 55 x 55 km 
# 1440 = 27.5 x 27.5 km

r_360 <- coverage %>%
  filter(grid == "110km") %>%
  select(scientificname,year,ssii,ssei) %>%
  rename("ssii_360" = "ssii",
         "ssei_360" = "ssei") 
r_720 <- coverage %>%
  filter(grid == "55km") %>%
  select(scientificname,year,ssii,ssei) %>%
  rename("ssii_720" = "ssii",
         "ssei_720" = "ssei") 
r_1440 <- coverage %>%
  filter(grid == "27.5km") %>%
  select(scientificname,year,ssii,ssei) %>%
  rename("ssii_1440" = "ssii",
         "ssei_1440" = "ssei") 

# rejoin data
r <- left_join(r_360,r_720, by = c("scientificname","year")) %>%
  left_join(.,r_1440, by = c("scientificname","year")) 


# function to estimate slopes between indices computed at 3 spatial grains
summarize_results <- function(species_name){
  results <- data.frame("grid" = c("720~360","1440~720","1440~360","720~360","1440~720","1440~360"),
                        "scientificnmae" = rep(species_name,6),
                        "slope" = rep(0,6),
                        "slope2.5" = rep(0,6),
                        "slope97.5" = rep(0,6),
                        "p" = rep(0,6),
                        "index" = c(rep("ssii",3),rep("ssei",3)))
  
  x1 <- summary(lm(data = subset(r, scientificname == species_name),
                   formula = ssii_720 ~ ssii_360))
  results[1,3] <- x1$coefficients[2,1]
  results[1,4] <- x1$coefficients[2,1] - 2*x1$coefficients[2,2]
  results[1,5] <- x1$coefficients[2,1] + 2*x1$coefficients[2,2]
  results[1,6] <- x1$coefficients[2,4]
  
  x1 <- summary(lm(data = subset(r, scientificname == species_name),
                   formula = ssii_1440 ~ ssii_720))
  results[2,3] <- x1$coefficients[2,1]
  results[2,4] <- x1$coefficients[2,1] - 2*x1$coefficients[2,2]
  results[2,5] <- x1$coefficients[2,1] + 2*x1$coefficients[2,2]
  results[2,6] <- x1$coefficients[2,4]
  
  x1 <- summary(lm(data = subset(r, scientificname == species_name),
                   formula = ssii_1440 ~ ssii_360))
  results[3,3] <- x1$coefficients[2,1]
  results[3,4] <- x1$coefficients[2,1] - 2*x1$coefficients[2,2]
  results[3,5] <- x1$coefficients[2,1] + 2*x1$coefficients[2,2]
  results[3,6] <- x1$coefficients[2,4]
  
  x1 <- summary(lm(data = subset(r, scientificname == species_name),
                   formula = ssei_720 ~ ssei_360))
  results[4,3] <- x1$coefficients[2,1]
  results[4,4] <- x1$coefficients[2,1] - 2*x1$coefficients[2,2]
  results[4,5] <- x1$coefficients[2,1] + 2*x1$coefficients[2,2]
  results[4,6] <- x1$coefficients[2,4]
  
  x1 <- summary(lm(data = subset(r, scientificname == species_name),
                   formula = ssei_1440 ~ ssei_720))
  results[5,3] <- x1$coefficients[2,1]
  results[5,4] <- x1$coefficients[2,1] - 2*x1$coefficients[2,2]
  results[5,5] <- x1$coefficients[2,1] + 2*x1$coefficients[2,2]
  results[5,6] <- x1$coefficients[2,4]
  
  x1 <- summary(lm(data = subset(r, scientificname == species_name),
                   formula = ssei_1440 ~ ssei_360))
  results[6,3] <- x1$coefficients[2,1]
  results[6,4] <- x1$coefficients[2,1] - 2*x1$coefficients[2,2]
  results[6,5] <- x1$coefficients[2,1] + 2*x1$coefficients[2,2]
  results[6,6] <- x1$coefficients[2,4]
  
  return(results)
}

# compute regression slopes for 2 example species
# hbird1 = Eriocnemis vestita
# hbird2 = Oreotrochilus leucopleurus

hbird1_results <- summarize_results("Eriocnemis vestita")
hbird2_results <- summarize_results("Oreotrochilus leucopleurus")

# set date range for plotting
year_start <- 2010
year_end <- 2019

sfig3c <- ggplot(data = coverage) +
  facet_grid(cols = vars(grid)) +
  geom_point(aes(x = year, y = ssii, color = scientificname)) +
  geom_point(aes(x = year, y = ssei, color = scientificname)) +
  
  geom_line(aes(x = year, y = ssii, color = scientificname)) +
  geom_line(aes(x = year, y = ssei, color = scientificname),lty=2) +
  
  scale_color_manual(values = c("#054A91","#5FAD56")) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        legend.position = c(0.7,0.5),
        axis.text.x  = element_text(size = 6),
        axis.title = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8)) +
  scale_x_continuous(limits = c(year_start, year_end),
                     breaks = seq(year_start, year_end,by = 2),
                     labels = seq(year_start, year_end,by = 2),
                     expand = expansion(mult = c(0.05, 0.05))) +
  labs(y = "Index", tag = "c") 



sfig3d <- ggplot(data = r) +
  geom_smooth(method = 'lm',aes(x = ssii_360, y = ssii_720, group = scientificname, color = scientificname)) +
  scale_color_manual(values = c("#054A91","#5FAD56")) +
  
  geom_point(aes(x = ssii_360, y = ssii_720, group = scientificname, color = scientificname)) +
  
  geom_text(aes(x = 0, y = 0.3, label = paste0("slope = ",round(hbird1_results[1,3],2),
                                               " [",round(hbird1_results[1,4],2),"-",
                                               round(hbird1_results[1,5],2),"], p < 0.001")), color = "#054A91", hjust = 0) +
  geom_text(aes(x = 0, y = 0.28, label = paste0("slope = ",round(hbird2_results[1,3],2),
                                                " [",round(hbird2_results[1,4],2),"-",
                                                round(hbird2_results[1,5],2),"], p < 0.001")), color = "#5FAD56", hjust = 0) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = "SSII (110km)",y = "SSII (55km)", tag = "d") 

sfig3e <- ggplot(data = r) +
  geom_smooth(method = 'lm',aes(x = ssii_720, y = ssii_1440, group = scientificname, color = scientificname)) +
  scale_color_manual(values = c("#054A91","#5FAD56")) +
  
  geom_point(aes(x = ssii_720, y = ssii_1440, group = scientificname, color = scientificname)) +
  geom_text(aes(x = 0, y = 0.2, label = paste0("slope = ",round(hbird1_results[2,3],2),
                                               " [",round(hbird1_results[2,4],2),"-",
                                               round(hbird1_results[2,5],2),"], p < 0.001")), color = "#054A91", hjust = 0) +
  geom_text(aes(x = 0, y = 0.185, label = paste0("slope = ",round(hbird2_results[2,3],2),
                                                 " [",round(hbird2_results[2,4],2),"-",
                                                 round(hbird2_results[2,5],2),"], p < 0.001")), color = "#5FAD56", hjust = 0) +
  
  theme_cowplot() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = "SSII (55km)",y = "SSII (27.5m)", tag = "e")

sfig3f <- ggplot(data = r) +
  geom_smooth(method = 'lm',aes(x = ssii_360, y = ssii_1440, group = scientificname, color = scientificname)) +
  scale_color_manual(values = c("#054A91","#5FAD56")) +
  
  geom_point(aes(x = ssii_360, y = ssii_1440, group = scientificname, color = scientificname)) +
  
  geom_text(aes(x = 0, y = 0.3, label = paste0("slope = ",round(hbird1_results[3,3],2),
                                               " [",round(hbird1_results[3,4],2),"-",
                                               round(hbird1_results[3,5],2),"], p < 0.001")), color = "#054A91", hjust = 0) +
  geom_text(aes(x = 0, y = 0.28, label = paste0("slope = ",round(hbird2_results[3,3],2),
                                                " [",round(hbird2_results[3,4],2),"-",
                                                round(hbird2_results[3,5],2),"], p < 0.001")), color = "#5FAD56", hjust = 0) +
  
  theme_cowplot() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = "SSII (110km)",y = "SSII (27.5km)", tag = "f")

sfig3g <- ggplot(data = r) +
  geom_smooth(method = 'lm',aes(x = ssei_360, y = ssei_720, group = scientificname, color = scientificname)) +
  scale_color_manual(values = c("#054A91","#5FAD56")) +
  
  geom_point(aes(x = ssei_360, y = ssei_720, group = scientificname, color = scientificname)) +
  
  geom_text(aes(x = 0.5, y = 0.92, label = paste0("slope = ",round(hbird1_results[4,3],2),
                                                 " [",round(hbird1_results[4,4],2),"-",
                                                 round(hbird1_results[4,5],2),"], p < 0.001")), color = "#054A91", hjust = 0) +
  geom_text(aes(x = 0.5, y = 0.9, label = paste0("slope = ",round(hbird2_results[4,3],2),
                                                  " [",round(hbird2_results[4,4],2),"-",
                                                  round(hbird2_results[4,5],2),"], p < 0.001")), color = "#5FAD56", hjust = 0) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = "SSEI (110km)",y = "SSEI (55km)", tag = "g") 

sfig3h <- ggplot(data = r) +
  geom_smooth(method = 'lm',aes(x = ssei_720, y = ssei_1440, group = scientificname, color = scientificname)) +
  scale_color_manual(values = c("#054A91","#5FAD56")) +
  
  geom_point(aes(x = ssei_720, y = ssei_1440, group = scientificname, color = scientificname)) +
  geom_text(aes(x = 0.5, y = 0.98, label = paste0("slope = ",round(hbird1_results[5,3],2),
                                                  " [",round(hbird1_results[5,4],2),"-",
                                                  round(hbird1_results[5,5],2),"], p < 0.001")), color = "#054A91", hjust = 0) +
  geom_text(aes(x = 0.5, y = 0.96, label = paste0("slope = ",round(hbird2_results[5,3],2),
                                                  " [",round(hbird2_results[5,4],2),"-",
                                                  round(hbird2_results[5,5],2),"], p < 0.001")), color = "#5FAD56", hjust = 0) +
  
  theme_cowplot() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = "SSEI (55km)",y = "SSEI (27.5m)", tag = "h")

sfig3i <- ggplot(data = r) +
  geom_smooth(method = 'lm',aes(x = ssei_360, y = ssei_1440, group = scientificname, color = scientificname)) +
  scale_color_manual(values = c("#054A91","#5FAD56")) +
  
  geom_point(aes(x = ssei_360, y = ssei_1440, group = scientificname, color = scientificname)) +
  
  geom_text(aes(x = 0.5, y = 0.98, label = paste0("slope = ",round(hbird1_results[6,3],2),
                                                  " [",round(hbird1_results[6,4],2),"-",
                                                  round(hbird1_results[6,5],2),"], p < 0.001")), color = "#054A91", hjust = 0) +
  geom_text(aes(x = 0.5, y = 0.96, label = paste0("slope = ",round(hbird2_results[6,3],2),
                                                  " [",round(hbird2_results[6,4],2),"-",
                                                  round(hbird2_results[6,5],2),"], p < 0.001")), color = "#5FAD56", hjust = 0) +
  
  theme_cowplot() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = "SSEI (110km)",y = "SSEI (27.5km)", tag = "i")