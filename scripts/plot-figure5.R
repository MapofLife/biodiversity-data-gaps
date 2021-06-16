# code to replicate Figure 5 of Oliver et al. 2021
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


taxa_summary <- taxa_data %>%
  # find mean national values over recent period
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>%
  summarise(stewards_ssii = mean(stewards_ssii, na.rm = TRUE)) %>%
  # join with information on change rates over recent period
  left_join(.,ssii_change, by = "country") %>%
  # group recent trends by no or decreasing vs. increasing trends
  mutate(slope_summary = slope) %>%
  mutate(slope_summary = replace(slope, !is.na(slope_sig),1)) %>%
  mutate(slope_summary = replace(slope_summary, slope < 0,0)) %>%
  mutate(slope_summary = replace(slope_summary, is.na(slope_sig),0))


# join summary data to national boundary spatial object for plotting
w <- left_join(gadm,taxa_summary, by = c("NAME_0"="country")) 

# establish groups based on mean and change rate in Steward's SSII
mean_ssii <- mean(w$stewards_ssii, na.rm=TRUE)

w <- w %>%
  add_column("group" = rep(0,nrow(w))) %>%
  mutate(group = replace(group, stewards_ssii < mean_ssii & slope_summary == 0,1)) %>%
  mutate(group = replace(group, stewards_ssii < mean_ssii & slope_summary == 1,2)) %>%
  mutate(group = replace(group, stewards_ssii >= mean_ssii & slope_summary == 0,3)) %>%
  mutate(group = replace(group, stewards_ssii >= mean_ssii & slope_summary == 1,4)) %>%
  filter(group > 0)


ssii_label <- "Steward's SSII"

# plot Fig 5a: typologies of nations' data coverage and trends

# plot limits
max_mean <- max(taxa_summary$stewards_ssii,na.rm = TRUE)
max_slope <- max(taxa_summary$slope,na.rm = TRUE)
min_slope <- min(taxa_summary$slope,na.rm = TRUE)
min_mean <- min(taxa_summary$stewards_ssii,na.rm = TRUE)

# add dummy variable to separate panels
w <- w %>%
  mutate("dummy" = group) %>%
  mutate(dummy = replace(dummy, group == 1, 1)) %>%
  mutate(dummy = replace(dummy, group == 3, 1)) %>%
  mutate(dummy = replace(dummy, group == 4, 2)) %>%
  mutate(dummy = replace(dummy, group == 2, 2)) 

# group labels to add to plot
dat_text <- data.frame(label = c("1", "3", "2","4"), dummy   = c(1, 1, 2, 2),
  x = c(min_slope + 0.05*min_slope, min_slope + 0.05*min_slope, max_slope - 0.05*max_slope,max_slope - 0.05*max_slope),
  y = c(0, max_mean- 0.1*max_mean, 0, max_mean - 0.1*max_mean))

# rectangle to shade plot backgrounds
dat_rect <- data.frame(x1 = c(min_slope + 0.05*min_slope,min_slope + 0.05*min_slope,0,0),
                       x2 = c(max_slope + 0.05*max_slope,max_slope+ 0.05*max_slope,max_slope+ 0.05*max_slope,max_slope+ 0.05*max_slope),
                       y1 = c(0,mean_ssii,0,mean_ssii),
                       y2 = c(mean_ssii,max_mean + 0.05*max_mean,mean_ssii,max_mean + 0.05*max_mean),
                       dummy   = c(1, 1, 2, 2))


fig5a <- ggplot() +
  geom_point(data = w, aes(x = slope, y = stewards_ssii, color = as.factor(group)), size = 0.8) +
  scale_color_manual(values = c("#33658A","#86BBD8","#F6AE2D","#F26419")) +
  geom_hline(yintercept = mean_ssii, linetype = "dashed", color = "grey30") +
  
  facet_wrap(~dummy, scales = "free_x") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing.x = unit(3, "mm"),
        legend.position = "none",
        axis.text.x = element_text(size = 7.5),
        axis.title = element_text(size = 9)) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01)),
                     breaks = seq(-0.02,0.03,by = 0.01),
                     labels = c("-0.02","-0.01","0","0.01","0.02","0.03")) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  labs(y = paste0(ssii_label," (",year_start,"-",year_end,")"), 
       x = paste0(ssii_label," Change Rate"," (",year_start,"-",year_end,")")) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label),
    color = c("#33658A","#F6AE2D","#86BBD8","#F26419"),
    size = 5,
    fontface = "bold",
    hjust   = -0.1,
    vjust   = -1) +
  geom_rect(data = dat_rect, 
            aes(xmin=x1,xmax = x2, ymin = y1, ymax = y2), 
            fill = c("#33658A","#F6AE2D","#86BBD8","#F26419"), 
            alpha = 0.3) +
  labs(tag = expression(bold("a")))

# plot Fig 5b: examples from national typologies
ymax <- 0.45
lwd <- 0.8

fig5b_1 <-ggplot() +
  geom_line(data = subset(taxa_data,country %in% c("Zimbabwe","Vietnam")),aes(year,stewards_ssii,group=country, linetype = factor(country)), 
            size = lwd, color = "#33658A") +
  geom_hline(yintercept = mean_ssii, linetype = "dashed", color = "grey30") +
  theme_cowplot(12) +
  theme(legend.position = c(0.05,0.9), 
        legend.title = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 8)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),limits = c(0,ymax)) +
  labs(x = "Year", y = ssii_label,color = "Quartile") +
  scale_x_continuous(limits = c(1950, 2018),expand = expansion(mult = c(0, 0.05))) +
  geom_text(aes(x = 1955, y = 0.14, label = "1"),col = "#33658A", size = 6, fontface = "bold")

fig5b_2 <-ggplot() +
  geom_line(data = subset(taxa_data,country %in% c("India","Brazil")),aes(year,stewards_ssii,group=country, linetype = factor(country)), 
            size = lwd, color = "#86BBD8" ) +
  geom_hline(yintercept = mean_ssii, linetype = "dashed", color = "grey30") +
  theme_cowplot(12) +
  theme(legend.position = c(0.05,0.9), 
        legend.title = element_blank(),
        axis.title=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),limits = c(0,ymax)) +
  labs(x = "Year",color = "Quartile") +
  scale_x_continuous(limits = c(1950, 2018),expand = expansion(mult = c(0, 0.05))) +
  geom_text(aes(x = 1955, y = 0.14, label = "2"),col = "#86BBD8", size = 6, fontface = "bold")

fig5b_3 <-ggplot() +
  geom_line(data = subset(taxa_data,country %in% c("Australia","Finland")),aes(year,stewards_ssii,group=country, linetype = factor(country)), 
            size = lwd, color = "#F6AE2D" ) +
  geom_hline(yintercept = mean_ssii, linetype = "dashed", color = "grey30") +
  theme_cowplot(12) +
  theme(legend.position = c(0.05,0.9), 
        legend.title = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),limits = c(0,ymax)) +
  labs(x = " ", y = ssii_label,color = "Quartile", tag = "b") +
  scale_x_continuous(limits = c(1950, 2018),expand = expansion(mult = c(0, 0.05))) +
  geom_text(aes(x = 1955, y = 0.14, label = "3"),col = "#F6AE2D", size = 6, fontface = "bold")

fig5b_4 <- ggplot() +
  geom_line(data = subset(taxa_data,country %in% c("South Africa","Mexico")),aes(year,stewards_ssii,group=country, linetype = factor(country)), 
            size = lwd, color = "#F26419") +
  geom_hline(yintercept = mean_ssii, linetype = "dashed", color = "grey30") +
  theme_cowplot(12) +
  theme(legend.position = c(0.05,0.9), 
        legend.title = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),limits = c(0,ymax)) +
  labs(x = " ", y = " ",color = "Quartile") +
  scale_x_continuous(limits = c(1950, 2018),expand = expansion(mult = c(0, 0.05))) +
  geom_text(aes(x = 1955, y = 0.14, label = "4"),col = "#F26419", size = 6, fontface = "bold")

# plot Fig 5c: map of group assignments
fig5c <- ggplot() +
  geom_sf(data = w,aes(fill = as.factor(group)),colour = "transparent",size = 0.2) +
  scale_fill_manual(values = c("#33658A","#86BBD8","#F6AE2D","#F26419"), na.value = "gray75",name = "Group") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        legend.position = "bottom") +
  theme(legend.position = "none") +
  coord_sf(datum = NA) +
  labs(tag = expression(bold("c")))

# summarize group assignments
group_summary <- w %>%
  filter(!is.na(group)) %>%
  st_set_geometry(NULL) %>%
  group_by(group) %>%
  summarise("pct" = n()/nrow(.)) %>%
  mutate(taxa = rep("all",nrow(.)))

# plot Fig 5c inset: propotion of nations in each group
fig5c_inset <- ggplot(group_summary, aes(y = pct, x = taxa, fill = as.factor(group)))+ 
  geom_bar(stat = "identity", color = "gray80",position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#33658A","#86BBD8","#F6AE2D","#F26419")) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 2,
                             nrow = 1)) +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = margin(0,0,0,0,"cm"),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank()) +
  coord_flip() +
  labs(x = NULL, y = NULL, fill = NULL) 



