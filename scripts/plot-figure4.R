# code to replicate Figure 4 of Oliver et al. 2021
# created by Ruth Oliver
# 2021.06.16

# load libraries
require(dplyr)
library(tidyverse)
library(data.table)
library(foreign)
library(sf)
library(broom)
library(viridis)

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

print(paste0("proportion of nations with significant trends in Steward's SSII: ",
             nrow(subset(ssii_change,!is.na(slope_sig)))/nrow(ssii_change)))

# find recent change rates in National SSEI 
ssei_change <- taxa_data %>% 
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>% 
  filter(sum(!is.na(national_ssei)) > 5) %>%
  do(model = lm(national_ssei ~ year, data = .)) %>%
  clean_results(.)

print(paste0("proportion of nations with significant trends in National SSEI: ",
             nrow(subset(ssei_change,!is.na(slope_sig)))/nrow(ssei_change)))


taxa_summary <- taxa_data %>%
  # find mean national values over recent period
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>%
  summarise(national_ssii = mean(national_ssii, na.rm = TRUE),
            stewards_ssii = mean(stewards_ssii, na.rm = TRUE),
            national_ssei = mean(national_ssei, na.rm = TRUE),
            prop_species_total = mean(prop_species_total, na.rm = TRUE)) %>%
  # join with information on change rates over recent period
  left_join(.,ssii_change, by = "country") %>%
  select(-slope_sig) %>%
  rename("stewards_ssii_change" = slope) %>%
  left_join(.,ssei_change, by = "country") %>%
  select(-slope_sig) %>%
  rename("national_ssei_change" = slope)

# join summary data to national boundary spatial object for plotting
w <- left_join(gadm,taxa_summary, by = c("NAME_0"="country")) 

ssii_label <- "Steward's SSII"
ssei_label <- "National SSEI"


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


# plot Fig. 4a: mean Steward's SSII (2010-2019)
fig4a <- ggplot() +
  geom_sf(data = w,aes(fill = stewards_ssii), colour = "transparent") +
  scale_fill_viridis_c(option = "viridis",trans = "sqrt",  
                       breaks = seq(0,1,0.05),
                       labels = every_nth(seq(0,1,0.05),2,inverse = TRUE)) +
  
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(fill = paste0(ssii_label," (",year_start,"-",year_end,")"),tag = expression(bold("a"))) +
  coord_sf(datum = NA)



# plot Fig. 4b: Steward's SSII change rate (2010-2019)
custom.interval.ssii <- c(-0.21,-0.01,0,0.002,0.004,0.005,0.007,0.009,0.01,0.015,0.03)
w$stewards_ssii_change_custom <- cut(w$stewards_ssii_change,
                               breaks = custom.interval.ssii, 
                               labels = c(-0.01,0,0.002,0.004,0.005,0.007,0.009,0.01,0.015,0.03),
                               include.lowest = TRUE)

fig4b <- ggplot() +
  geom_sf(data = w,aes(fill = stewards_ssii_change_custom),colour = "transparent") +
  scale_fill_manual(values = c("#016FB9","#B5E2FA","#FBE89D","#F9DF74","#F0C62D","#EDAE49","#DF9216","#FB5012","#E63B2E","#A82C23"),na.value = "grey30")+
  coord_sf(datum = NA) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  theme(plot.margin = margin(0,0,0,0,"cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  labs(fill = paste0(ssii_label," change rate (",year_start,"-",year_end,")"),tag = expression(bold("b"))) +
  guides(fill = guide_legend(nrow = 1,
                             keywidth = 1.2,
                             label.position = "bottom", 
                             title.position = "top", 
                             title.vjust = 0.9,
                             label.hjust = 1,
                             label.vjust = 1,
                             label.theme = element_text(size = 7))) 


# plot Fig. 4d: mean National SSII (2010-2019)
fig4d <- ggplot() +
  geom_sf(data = w,aes(fill = national_ssei), colour = "transparent") +
  scale_fill_gradientn(colours=viridis_pal(direction=1, option="magma")(10),
                       values=c(0, 0.4, 0.70, 0.8,0.85,0.9,0.95, 1),
                       breaks = seq(0,1,0.05),
                       labels = every_nth(seq(0,1,0.05),2,inverse = TRUE)) +
  
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(fill = paste0(ssei_label," (",year_start,"-",year_end,")"),tag = expression(bold("d"))) +
  coord_sf(datum = NA)


# plot Fig. 4d inset: mean Steward's SSII and National SSEI (2010-2019)
fig4d_inset <- ggplot() +
  geom_point(data = taxa_summary, aes(x = stewards_ssii, y = national_ssei), size = 0.3, alpha = 0.3) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01)),
                     breaks = seq(0,1,0.1),
                     labels = every_nth(seq(0,1,0.1),2,inverse = TRUE)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01)),
                     breaks = seq(0,1,0.1),
                     labels = every_nth(seq(0,1,0.1),2,inverse = TRUE)) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title = element_text(size = 6.5),
        axis.text = element_text(size = 6)) +
  labs(x = ssii_label, y = ssei_label) 


# plot Fig. 4e: National SSEI change rate (2010-2019)
custom.interval.ssei <- c(-0.05,-0.02,-0.01,-0.005,-0.001,0,0.005,0.01,0.015,0.025)
w$national_ssei_change_custom <- cut(w$national_ssei_change,
                                     breaks = custom.interval.ssei, 
                                     labels = c(-0.02,-0.01,-0.005,-0.001,0,0.005,0.01,0.015,0.025),
                                     include.lowest = TRUE)

fig4e <- ggplot() +
  geom_sf(data = w,aes(fill = national_ssei_change_custom),colour = "transparent") +
  scale_fill_manual(values = c("#01497A","#016FB9","#0193F4","#79CAF6","#B5E2FA","#F9DF74","#EDAE49","#FB5012","#A82C23"),na.value = "grey30")+
  coord_sf(datum = NA) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  theme(plot.margin = margin(0,0,0,0,"cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  labs(fill = paste0(ssei_label," change rate (",year_start,"-",year_end,")"),tag = expression(bold("e"))) +
  guides(fill = guide_legend(nrow = 1,
                             keywidth = 1.2,
                             label.position = "bottom", 
                             title.position = "top", 
                             title.vjust = 0.9,
                             label.hjust = 1,
                             label.vjust = 1,
                             label.theme = element_text(size = 7))) 


# find recent change rates in Steward's SSII by taxa 
ssii_change_amphibians <- taxa_data %>% 
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>% 
  filter(sum(!is.na(stewards_ssii_amphibians)) == 10) %>%
  do(model = lm(stewards_ssii_amphibians ~ year, data = .)) %>%
  clean_results(.) %>%
  ungroup() %>%
  mutate("taxa" = rep("amphibians", nrow(.)))

ssii_change_birds <- taxa_data %>% 
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>% 
  filter(sum(!is.na(stewards_ssii_birds)) == 10) %>%
  do(model = lm(stewards_ssii_birds ~ year, data = .)) %>%
  clean_results(.) %>%
  ungroup() %>%
  mutate("taxa" = rep("birds", nrow(.)))

ssii_change_mammals <- taxa_data %>% 
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>% 
  filter(sum(!is.na(stewards_ssii_mammals)) == 10) %>%
  do(model = lm(stewards_ssii_mammals ~ year, data = .)) %>%
  clean_results(.) %>%
  ungroup() %>%
  mutate("taxa" = rep("mammals", nrow(.)))

ssii_change_reptiles <- taxa_data %>% 
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>% 
  filter(sum(!is.na(stewards_ssii_reptiles)) == 10) %>%
  do(model = lm(stewards_ssii_reptiles ~ year, data = .)) %>%
  clean_results(.) %>%
  ungroup() %>%
  mutate("taxa" = rep("reptiles", nrow(.)))

ssii_change_taxa <- rbind(ssii_change_amphibians,ssii_change_birds,ssii_change_mammals,ssii_change_reptiles)

taxa_info <- ssii_change_taxa %>%
  group_by(taxa) %>%
  summarise("n_total" = n())

# summarize recent trends in Steward's SSII by taxa
ssii_change_taxa <- ssii_change_taxa %>%
  mutate("slope_sign" = slope_sig) %>%
  mutate(slope_sign = replace(slope_sign, slope_sig < 0, "less")) %>%
  mutate(slope_sign = replace(slope_sign, slope_sig == 0, "equal")) %>%
  mutate(slope_sign = replace(slope_sign, slope_sig > 0, "greater")) %>%
  mutate(slope_sign = replace(slope_sign, is.na(slope_sig), "not_sig")) %>%
  group_by(taxa, slope_sign) %>%
  summarise(n = n()) %>%
  left_join(.,taxa_info, by = "taxa") %>%
  mutate(pct = (n/n_total)*100) %>%
  select(taxa, slope_sign, pct)

# reset factor orders for plotting
ssii_change_taxa$taxa <- ordered(ssii_change_taxa$taxa,c("reptiles","amphibians","mammals","birds"))
ssii_change_taxa$slope_sign <- ordered(ssii_change_taxa$slope_sign,c("not_sig","equal","less","greater"))

# plot Fig 4c: Steward's SSII change rate by taxa
fig4c <- ggplot(ssii_change_taxa, aes(y = pct, x= taxa, fill = slope_sign))+ 
  geom_bar(stat = "identity", color = "gray30",position = position_fill(reverse = TRUE), size = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#FDF1C4","#016FB9","#FB5012"), na.value = "grey90") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.margin = margin(0,0,0,0,"cm"),
    axis.text.x = element_text(size = 7),
    axis.line = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    panel.grid.major.y = element_blank()) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       fill = NULL, tag = expression(bold("c")))

# find recent change rates in National SSEI by taxa 
ssei_change_amphibians <- taxa_data %>% 
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>% 
  filter(sum(!is.na(national_ssei_amphibians)) == 10) %>%
  do(model = lm(national_ssei_amphibians ~ year, data = .)) %>%
  clean_results(.) %>%
  ungroup() %>%
  mutate("taxa" = rep("amphibians", nrow(.)))

ssei_change_birds <- taxa_data %>% 
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>% 
  filter(sum(!is.na(national_ssei_birds)) == 10) %>%
  do(model = lm(national_ssei_birds ~ year, data = .)) %>%
  clean_results(.) %>%
  ungroup() %>%
  mutate("taxa" = rep("birds", nrow(.)))

ssei_change_mammals <- taxa_data %>% 
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>% 
  filter(sum(!is.na(national_ssei_mammals)) == 10) %>%
  do(model = lm(national_ssei_mammals ~ year, data = .)) %>%
  clean_results(.) %>%
  ungroup() %>%
  mutate("taxa" = rep("mammals", nrow(.)))

ssei_change_reptiles <- taxa_data %>% 
  filter(year >= year_start) %>%
  filter(year <= year_end) %>%
  group_by(country) %>% 
  filter(sum(!is.na(national_ssei_reptiles)) == 10) %>%
  do(model = lm(national_ssei_reptiles ~ year, data = .)) %>%
  clean_results(.) %>%
  ungroup() %>%
  mutate("taxa" = rep("reptiles", nrow(.)))

ssei_change_taxa <- rbind(ssei_change_amphibians,ssei_change_birds,ssei_change_mammals,ssei_change_reptiles)

taxa_info <- ssei_change_taxa %>%
  group_by(taxa) %>%
  summarise("n_total" = n())

# summarize recent trends in National SSEI by taxa
ssei_change_taxa <- ssei_change_taxa %>%
  mutate("slope_sign" = slope_sig) %>%
  mutate(slope_sign = replace(slope_sign, slope_sig < 0, "less")) %>%
  mutate(slope_sign = replace(slope_sign, slope_sig == 0, "equal")) %>%
  mutate(slope_sign = replace(slope_sign, slope_sig > 0, "greater")) %>%
  mutate(slope_sign = replace(slope_sign, is.na(slope_sig), "not_sig")) %>%
  group_by(taxa, slope_sign) %>%
  summarise(n = n()) %>%
  left_join(.,taxa_info, by = "taxa") %>%
  mutate(pct = (n/n_total)*100) %>%
  select(taxa, slope_sign, pct)

# reset factor orders for plotting
ssei_change_taxa$taxa <- ordered(ssei_change_taxa$taxa,c("reptiles","amphibians","mammals","birds"))
ssei_change_taxa$slope_sign <- ordered(ssei_change_taxa$slope_sign,c("not_sig","equal","less","greater"))

# plot Fig 4f: National SSEI change rate by taxa
fig4e <- ggplot(ssei_change_taxa, aes(y = pct, x= taxa, fill = slope_sign))+ 
  geom_bar(stat = "identity", color = "gray30",position = position_fill(reverse = TRUE), size = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#FDF1C4","#016FB9","#FB5012"), na.value = "grey90") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.margin = margin(0,0,0,0,"cm"),
    axis.text.x = element_text(size = 7),
    axis.line = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    panel.grid.major.y = element_blank()) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       fill = NULL, tag = expression(bold("e")))
