#### WATER POTENTIAL-WATER CONTENT DIFFERENCES BETWEEN PLOTS ###################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")

#### LOAD DATA ----------------------------------------------------------------- ####

# metadata (taxonomy)

metadata <- readxl::read_excel(paste0(data.path, "metadata_cax_radar/cax_radar_metadata_caxiuana_10_2023.xlsx"), sheet = 1) %>%
  dplyr::select(id = ID, genus, dbh = dbh_2023_cm, size_class)

## Wet season campaign 
radar_wp_wc_05_2023.data <- read_csv(file = paste0(data.path, "leaf_water_potential_water_content/radar_wp_wc_05_2023.csv")) %>%
  mutate(campaign = "2023-05") %>%
  dplyr::select(-date)

radar_wp_wc_05_2023.data <- merge(radar_wp_wc_05_2023.data, metadata, 
                          by = "id", 
                          all.x = T)

# Intermediate season campaign 1
radar_wp_wc_07_2023.data <- read_csv(file = paste0(data.path, "leaf_water_potential_water_content/radar_wp_wc_07_2023.csv")) %>%
  mutate(campaign = "2023-07") %>%
  dplyr::select(-date)

radar_wp_wc_07_2023.data <- merge(radar_wp_wc_07_2023.data, metadata, 
                                  by = "id", 
                                  all.x = T)

# Dry season campaign 1
radar_wp_wc_10_2023.data <- read_csv(file = paste0(data.path, "leaf_water_potential_water_content/radar_wp_wc_10_2023.csv")) %>%
  mutate(campaign = "2023-10") %>%
  dplyr::select(-date)

radar_wp_wc_10_2023.data <- merge(radar_wp_wc_10_2023.data, metadata, 
                                  by = "id", 
                                  all.x = T)

# Dry season campaign 2 (december)
radar_wp_wc_12_2023.data <- read_csv(file = paste0(data.path, "leaf_water_potential_water_content/radar_wp_wc_12_2023.csv")) %>%
  mutate(campaign = "2023-12") %>%
  dplyr::select(-date)

radar_wp_wc_12_2023.data <- merge(radar_wp_wc_12_2023.data, metadata, 
                                  by = "id", 
                                  all.x = T)


## Paulo's data (2018) 

wp_dry_2018.data <- read_csv(file = paste0(data.path, "caxuana_traits/treeData.csv")) %>%
  mutate(plot = ifelse(treatment == "tfe", "TFE", "Control"),
         id = paste0(plot, "_", tree),
         campaign = "2018-10",
         wp_md = -abs(psi_md),
         wp_pd = -abs(psi_pd)) %>%
  dplyr::select(id, plot, genus, wp_md, wp_pd, campaign)

wp_dry_2018.data <- aggregate(wp_dry_2018.data, by = list(wp_dry_2018.data$id), FUN = meanOrMode) %>%
  dplyr::select(-Group.1)

wp_dry_2018.data$genus <- paste0(wp_dry_2018.data$genus)

## Same individuals comparison dataset (to cover 2018 trees and compare same individuals)

big_sampling_2023 <- read_csv(paste0(data.path, "leaf_water_potential_water_content/big_campaign_water_potentials_2023.csv")) %>%
  # filter(date_md > "2023-06-01") %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         id = paste0(plot, "_", tree),
         campaign = ifelse(season == "dry", "2023-10", "2023-05")) %>%
  dplyr::select(id, plot, genus, wp_md = psiMD, wp_pd = psiPD, campaign) %>%
  filter(id %in% wp_dry_2018.data$id) # 49 trees sampled both times

wp_dry_2018_comparison.data <- wp_dry_2018.data %>%
  dplyr::select(-genus) %>%
  filter(id %in% big_sampling_2023$id) # 49 trees sampled both times

wp_dry_2018_comparison_withGenus.data <- merge(wp_dry_2018_comparison.data, big_sampling_2023[, c("id", "genus")], 
                                               by = "id")

wp_comparison_2018_2023 <- bind_rows(wp_dry_2018_comparison_withGenus.data, big_sampling_2023)


## Radar plus whole 2018 dataset
# wp_dry_2018.data
radar_wp_wc.data <- bind_rows(radar_wp_wc_05_2023.data, radar_wp_wc_07_2023.data, radar_wp_wc_10_2023.data) #radar_wp_wc_12_2023.data
unique(big_sampling_2023_tomerge$campaign)
big_sampling_2023_tomerge <- big_sampling_2023 %>%
  mutate(dataset = "all trees",
         campaign )

radar_wp_wc.data_tomerge <- radar_wp_wc.data %>%
  mutate(dataset = "monitored trees")

radar_vs_big_campaign <- bind_rows(big_sampling_2023_tomerge, radar_wp_wc.data_tomerge)

radar_vs_big_campaign_a <- radar_vs_big_campaign %>%
  filter(plot == "Control")

radar_vs_big_campaign_a_wet <- radar_vs_big_campaign_a %>%
  filter(campaign == "2023-05")

radar_vs_big_campaign_a_dry <- radar_vs_big_campaign_a %>%
  filter(campaign == "2023-10")

radar_vs_big_campaign_b <- radar_vs_big_campaign %>%
  filter(plot == "TFE")

radar_vs_big_campaign_b_wet <- radar_vs_big_campaign_b %>%
  filter(campaign == "2023-05")

radar_vs_big_campaign_b_dry <- radar_vs_big_campaign_b %>%
  filter(campaign == "2023-10")

#### BIG CAMPAIGN VS MONITORED TREES ------------------------------------------- ####

## Control

# wet
radar_vs_big.lm <- lm(wp_md ~ dataset, 
                    data = radar_vs_big_campaign_a_wet)
summary(radar_vs_big.lm)

radar_vs_big.lmer <- lmer(wp_md ~ dataset + (1|genus/id), 
                        data = radar_vs_big_campaign_a_wet)
summary(radar_vs_big.lmer)

# dry
radar_vs_big.lm <- lm(wp_md ~ dataset, 
                      data = radar_vs_big_campaign_a_dry)
summary(radar_vs_big.lm)

radar_vs_big.lmer <- lmer(wp_md ~ dataset + (1|genus/id), 
                          data = radar_vs_big_campaign_a_dry)
summary(radar_vs_big.lmer)

## TFE

# wet
radar_vs_big.lm <- lm(wp_md ~ dataset, 
                      data = radar_vs_big_campaign_b_wet)
summary(radar_vs_big.lm)

radar_vs_big.lmer <- lmer(wp_md ~ dataset + (1|genus/id), 
                          data = radar_vs_big_campaign_b_wet)
summary(radar_vs_big.lmer)

# dry
radar_vs_big.lm <- lm(wp_md ~ dataset, 
                      data = radar_vs_big_campaign_b_dry)
summary(radar_vs_big.lm)

radar_vs_big.lmer <- lmer(wp_md ~ dataset + (1|genus/id), 
                          data = radar_vs_big_campaign_b_dry)
summary(radar_vs_big.lmer)


#### DISTRIBUTIONS ------------------------------------------------------------- ####

## May data

# ggplot(data = wp_wc_05_2023.data, aes(log(-wp_md))) + geom_density()
ggplot(data = radar_wp_wc_05_2023.data, aes(wp_md)) + geom_density()

# ggplot(data = wp_wc_05_2023.data, aes(log(-wp_pd))) + geom_density()
ggplot(data = radar_wp_wc_05_2023.data, aes(wp_pd)) + geom_density()

# ggplot(data = wp_wc_05_2023.data, aes(relative_wc_md)) + geom_density()
ggplot(data = radar_wp_wc_05_2023.data, aes(relative_wc_md)) + geom_density()

# ggplot(data = wp_wc_05_2023.data, aes(relative_wc_pd)) + geom_density()
ggplot(data = radar_wp_wc_05_2023.data, aes(relative_wc_pd)) + geom_density()

## July data

ggplot(data = radar_wp_wc_07_2023.data, aes(wp_md)) + geom_density()

ggplot(data = radar_wp_wc_07_2023.data, aes(wp_pd)) + geom_density()

ggplot(data = radar_wp_wc_07_2023.data, aes(relative_wc_md)) + geom_density()

ggplot(data = radar_wp_wc_07_2023.data, aes(relative_wc_pd)) + geom_density()


## October data

ggplot(data = radar_wp_wc_10_2023.data, aes(wp_md)) + geom_density()

ggplot(data = radar_wp_wc_10_2023.data, aes(wp_pd)) + geom_density()

ggplot(data = radar_wp_wc_10_2023.data, aes(relative_wc_md)) + geom_density()

ggplot(data = radar_wp_wc_10_2023.data, aes(relative_wc_pd)) + geom_density()


## Transform

radar_wp_wc_05_2023.data <- radar_wp_wc_05_2023.data %>%
  mutate(log_abs_wp_md = log(-wp_md),
         log_abs_wp_pd = log(-wp_pd))

radar_wp_wc_07_2023.data <- radar_wp_wc_07_2023.data %>%
  mutate(log_abs_wp_md = log(-wp_md),
         log_abs_wp_pd = log(-wp_pd))

radar_wp_wc_10_2023.data <- radar_wp_wc_10_2023.data %>%
  mutate(log_abs_wp_md = log(-wp_md),
         log_abs_wp_pd = log(-wp_pd))


#### MODELS AND PLOTS 2018 ----------------------------------------------------- ####

### Plot effect for 2018 (dry season?) ####

## Paulo's data

## WP pd

pd_10_2023.lm <- lm(wp_pd ~ plot, 
                    data = wp_dry_2018.data)
summary(pd_10_2023.lm)

pd_10_2023.lm <- lmer(wp_pd ~ plot + (1|genus), 
                    data = wp_dry_2018.data)
summary(pd_10_2023.lm)


## WP md

md_2018.lm <- lmer(wp_md ~ plot + (1|genus), 
                   data = wp_dry_2018.data)
summary(md_2018.lm)


#### MODELS WET SEASON --------------------------------------------------------- ####

### Plot effect for may ####

## WP pd

pd_05_2023.lm <- lm(wp_pd ~ plot, 
                    data = radar_wp_wc_05_2023.data)
summary(pd_05_2023.lm)

pd_05_2023.lmer <- lmer(wp_pd ~ plot + (1|genus/id), 
                    data = radar_wp_wc_05_2023.data)
summary(pd_05_2023.lmer)

pd_05_2023.lmer <- lmer(wp_pd ~ plot * dbh + (1|genus/id), 
                        data = radar_wp_wc_05_2023.data)
summary(pd_05_2023.lmer)


## WP md

md_05_2023.lm <- lm(wp_md ~ plot, 
                    data = radar_wp_wc_05_2023.data)
summary(md_05_2023.lm)

md_05_2023.lmer <- lmer(wp_md ~ plot + (1|genus/id), 
                        data = radar_wp_wc_05_2023.data)
summary(md_05_2023.lmer)

md_05_2023.lmer <- lmer(wp_md ~ plot * dbh + (1|genus/id), 
                        data = radar_wp_wc_05_2023.data)
summary(md_05_2023.lmer)


## WC pd

pd_wc_05_2023.lmer <- lmer(relative_wc_pd ~ plot + (1|genus/id), 
                        data = radar_wp_wc_05_2023.data)
summary(pd_wc_05_2023.lmer)

pd_wc_05_2023.lmer <- lmer(relative_wc_pd ~ plot * dbh + (1|genus/id), 
                           data = radar_wp_wc_05_2023.data)
summary(pd_wc_05_2023.lmer)


## WC md

md_wc_05_2023.lm <- lm(relative_wc_md ~ plot, 
                       data = radar_wp_wc_05_2023.data)
summary(md_wc_05_2023.lm)

md_wc_05_2023.lmer <- lmer(relative_wc_md ~ plot + (1|genus/id), 
                           data = radar_wp_wc_05_2023.data)
summary(md_wc_05_2023.lmer)

md_wc_05_2023.lmer <- lmer(relative_wc_md ~ plot * dbh + (1|genus/id), 
                           data = radar_wp_wc_05_2023.data)
summary(md_wc_05_2023.lmer)


#### MODELS INTERMEDIATE SEASON ------------------------------------------------ ####

### Plot effect for july ####

## WP pd

pd_07_2023.lmer <- lmer(wp_pd ~ plot + (1|genus/id), 
                           data = radar_wp_wc_07_2023.data)
summary(pd_07_2023.lmer)

pd_07_2023.lmer <- lmer(wp_pd ~ plot * dbh + (1|genus/id), 
                        data = radar_wp_wc_07_2023.data)
summary(pd_07_2023.lmer)


## WP md

md_07_2023.lmer <- lmer(wp_md ~ plot + (1|genus/id), 
                        data = radar_wp_wc_07_2023.data)
summary(md_07_2023.lmer)

md_07_2023.lmer <- lmer(wp_md ~ plot * dbh + (1|genus/id), 
                        data = radar_wp_wc_07_2023.data)
summary(md_07_2023.lmer)


## WC pd

pd_wc_07_2023.lmer <- lmer(relative_wc_pd ~ plot + (1|genus/id), 
                        data = radar_wp_wc_07_2023.data)
summary(pd_wc_07_2023.lmer)

pd_wc_07_2023.lmer <- lmer(relative_wc_pd ~ plot * dbh + (1|genus/id), 
                           data = radar_wp_wc_07_2023.data)
summary(pd_wc_07_2023.lmer)


## WC md

md_wc_07_2023.lmer <- lmer(relative_wc_md ~ plot + (1|genus/id), 
                           data = radar_wp_wc_07_2023.data)
summary(md_wc_07_2023.lmer)

md_wc_07_2023.lmer <- lmer(relative_wc_md ~ plot * dbh + (1|genus/id), 
                           data = radar_wp_wc_07_2023.data)
summary(md_wc_07_2023.lmer)


#### MODELS DRY SEASON --------------------------------------------------------- ####

### Plot effect for October ####

## WP pd

pd_10_2023.lm <- lm(wp_pd ~ plot, 
                    data = radar_wp_wc_10_2023.data)
summary(pd_10_2023.lm)

pd_10_2023.lmer <- lmer(wp_pd ~ plot + (1|genus/id), 
                           data = radar_wp_wc_10_2023.data)
summary(pd_10_2023.lmer)

pd_10_2023.lmer <- lmer(wp_pd ~ plot * dbh + (1|genus/id), 
                        data = radar_wp_wc_10_2023.data)
summary(pd_10_2023.lmer)


## WP md

md_10_2023.lm <- lm(wp_md ~ plot,
                    data = radar_wp_wc_10_2023.data)
summary(md_10_2023.lm)

md_10_2023.lmer <- lmer(wp_md ~ plot + (1|genus/id), 
                        data = radar_wp_wc_10_2023.data)
summary(md_10_2023.lmer)

md_10_2023.lmer <- lmer(wp_md ~ plot * dbh + (1|genus/id), 
                        data = radar_wp_wc_10_2023.data)
summary(md_10_2023.lmer)


## WC pd

pd_wc_10_2023.lmer <- lmer(relative_wc_pd ~ plot + (1|genus/id), 
                        data = radar_wp_wc_10_2023.data)
summary(pd_wc_10_2023.lmer)

pd_wc_10_2023.lmer <- lmer(relative_wc_pd ~ plot * dbh + (1|genus/id), 
                           data = radar_wp_wc_10_2023.data)
summary(pd_wc_10_2023.lmer)


## WC md

md_wc_10_2023.lmer <- lmer(relative_wc_md ~ plot + (1|genus/id), 
                           data = radar_wp_wc_10_2023.data)
summary(md_wc_10_2023.lmer)

md_wc_10_2023.lmer <- lmer(relative_wc_md ~ plot * dbh + (1|genus/id), 
                           data = radar_wp_wc_10_2023.data)
summary(md_wc_10_2023.lmer)


#### MODELS DRY SEASON 2 ------------------------------------------------------- ####

### Plot effect for December ####

## Radar data

## WP pd

pd_12_2023.lmer <- lmer(wp_pd ~ plot + (1|genus/id), 
                        data = radar_wp_wc_12_2023.data)
summary(pd_12_2023.lmer)

pd_12_2023.lmer <- lmer(wp_pd ~ plot * dbh + (1|genus/id), 
                        data = radar_wp_wc_12_2023.data)
summary(pd_12_2023.lmer)

## WP md

md_12_2023.lmer <- lmer(wp_md ~ plot + (1|genus/id), 
                        data = radar_wp_wc_12_2023.data)
summary(md_12_2023.lmer)

md_12_2023.lmer <- lmer(wp_md ~ plot * dbh + (1|genus/id), 
                        data = radar_wp_wc_12_2023.data)
summary(md_12_2023.lmer)


## WC pd

pd_wc_12_2023.lmer <- lmer(relative_wc_pd ~ plot + (1|genus/id), 
                           data = radar_wp_wc_12_2023.data)
summary(pd_wc_12_2023.lm)

pd_wc_12_2023.lmer <- lmer(relative_wc_pd ~ plot * dbh + (1|genus/id), 
                           data = radar_wp_wc_12_2023.data)
summary(pd_wc_12_2023.lmer)


## WC md

md_wc_12_2023.lmer <- lmer(relative_wc_md ~ plot + (1|genus/id), 
                           data = radar_wp_wc_12_2023.data)
summary(md_wc_12_2023.lmer)

md_wc_12_2023.lmer <- lmer(relative_wc_md ~ plot * dbh + (1|genus/id), 
                           data = radar_wp_wc_12_2023.data)
summary(md_wc_12_2023.lmer)


#### PLOTS --------------------------------------------------------------------- ####

### Midday WP ####

pdf("results/manuscript/midday_leaf_water_potential.pdf", 
    height = h, 
    width = w)
leaf_wp_md.plot <- ggplot(data = radar_wp_wc.data, 
                          aes(x = campaign,
                              y = wp_md,
                              color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Leaf water potential at midday (MPa)") + 
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(-5.00, 0.00))
leaf_wp_md.plot
dev.off()

radar_wp_wc.data %>%
  filter(campaign == "2023-05") %>%
  filter(plot == "Control") %>%
  pull(wp_md) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-05") %>%
  filter(plot == "TFE") %>%
  pull(wp_md) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-07") %>%
  filter(plot == "Control") %>%
  pull(wp_md) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-07") %>%
  filter(plot == "TFE") %>%
  pull(wp_md) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-10") %>%
  filter(plot == "Control") %>%
  pull(wp_md) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-10") %>%
  filter(plot == "TFE") %>%
  pull(wp_md) %>%
  mean(na.rm = T)

# by size group

size_radar_wp_wc.data <- radar_wp_wc.data %>%
  filter(!is.na(size_class)) %>%
  filter(campaign != "2018-10")

pdf("results/midday_leaf_water_potential_size_class.pdf", 
    height = h, 
    width = w)
leaf_wp_md.plot <- ggplot(data = size_radar_wp_wc.data, 
                          aes(x = campaign,
                              y = wp_md,
                              color = plot,
                              fill = size_class)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Leaf water potential at midday (MPa)") + 
  theme_minimal()  +
  # theme(legend.title = element_blank(), legend.position = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(-5.00, 0.00))
leaf_wp_md.plot
dev.off()

### Midday WC ####

pdf("results/manuscript/midday_leaf_water_content.pdf", 
    height = h, 
    width = w)
leaf_wc_md.plot <- ggplot(data = radar_wp_wc.data, 
                          aes(x = campaign,
                              y = relative_wc_md,
                              color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Leaf relative water content at midday") + 
  theme_minimal()  +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(0, 1))
leaf_wc_md.plot
dev.off()

radar_wp_wc.data %>%
  filter(campaign == "2023-05") %>%
  filter(plot == "Control") %>%
  pull(relative_wc_md) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-05") %>%
  filter(plot == "TFE") %>%
  pull(relative_wc_md) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-07") %>%
  filter(plot == "Control") %>%
  pull(relative_wc_md) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-07") %>%
  filter(plot == "TFE") %>%
  pull(relative_wc_md) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-10") %>%
  filter(plot == "Control") %>%
  pull(relative_wc_md) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-10") %>%
  filter(plot == "TFE") %>%
  pull(relative_wc_md) %>%
  mean(na.rm = T)

### Predawn WP ####

pdf("results/manuscript/predawn_leaf_water_potential.pdf", 
    height = h, 
    width = w)
leaf_wp_md.plot <- ggplot(data = radar_wp_wc.data, 
                          aes(x = campaign,
                              y = wp_pd,
                              color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Leaf water potential at predawn (MPa)") + 
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(-5, 0))
leaf_wp_md.plot
dev.off()

radar_wp_wc.data %>%
  filter(campaign == "2023-05") %>%
  filter(plot == "Control") %>%
  pull(wp_pd) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-05") %>%
  filter(plot == "TFE") %>%
  pull(wp_pd) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-07") %>%
  filter(plot == "Control") %>%
  pull(wp_pd) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-07") %>%
  filter(plot == "TFE") %>%
  pull(wp_pd) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-10") %>%
  filter(plot == "Control") %>%
  pull(wp_pd) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-10") %>%
  filter(plot == "TFE") %>%
  pull(wp_pd) %>%
  mean(na.rm = T)

### Predawn WC ####

pdf("results/manuscript/predawn_leaf_water_content.pdf", 
    height = h, 
    width = w)
leaf_wc_md.plot <- ggplot(data = radar_wp_wc.data, 
                          aes(x = campaign,
                              y = relative_wc_pd,
                              color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Leaf relative water content at predawn") + 
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),  limits = c(0, 1))
leaf_wc_md.plot
dev.off()

radar_wp_wc.data %>%
  filter(campaign == "2023-05") %>%
  filter(plot == "Control") %>%
  pull(relative_wc_pd) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-05") %>%
  filter(plot == "TFE") %>%
  pull(relative_wc_pd) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-07") %>%
  filter(plot == "Control") %>%
  pull(relative_wc_pd) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-07") %>%
  filter(plot == "TFE") %>%
  pull(relative_wc_pd) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-10") %>%
  filter(plot == "Control") %>%
  pull(relative_wc_pd) %>%
  mean(na.rm = T)

radar_wp_wc.data %>%
  filter(campaign == "2023-10") %>%
  filter(plot == "TFE") %>%
  pull(relative_wc_pd) %>%
  mean(na.rm = T)

#### COMPARISON USING SAME INDIVIDUALS ----------------------------------------- ####

### 2018 ####

sameInd_2018 <- wp_comparison_2018_2023 %>%
  filter(campaign == "2018-10")

sameInd_pd.lmer <- lmer(wp_pd ~ plot + (1|genus), 
                                   data = sameInd_2018)
summary(sameInd_pd.lmer)

sameInd_md.lmer <- lmer(wp_md ~ plot + (1|genus), 
                        data = sameInd_2018)
summary(sameInd_md.lmer)


### 2023 ####

sameInd_2023 <- wp_comparison_2018_2023 %>%
  filter(campaign == "2023-10")

sameInd_pd_2023.lmer <- lmer(wp_pd ~ plot + (1|genus), 
                        data = sameInd_2023)
summary(sameInd_pd_2023.lmer)

sameInd_md_2023.lmer <- lmer(wp_md ~ plot + (1|genus), 
                        data = sameInd_2023)
summary(sameInd_md_2023.lmer)


pdf("results/radar_wp_pd_comparison_2018_2023_plot.pdf", 
    height = h, 
    width = w)
radar_pd_comparison.boxplot <- ggplot(data = wp_comparison_2018_2023, 
                                   aes(x = campaign, 
                                       y = wp_pd,
                                       color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("WP predawn (MPa)") + 
  theme_minimal() +
  theme(legend.position = "none")
radar_pd_comparison.boxplot
dev.off()

# wp_comparison_2018 <- wp_comparison_2018_2023 %>%
#   filter(campaign == "2018-10") %>%
#   dplyr::select(-campaign, wp_md_2018_10 = wp_md, wp_pd_2018_10 = wp_pd)
# 
# wp_comparison_2023 <- wp_comparison_2018_2023 %>%
#   filter(campaign == "2023-10") %>%
#   dplyr::select(-campaign, -plot, - genus, wp_md_2023_10 = wp_md, wp_pd_2023_10 = wp_pd)
# 
# difference <- merge(wp_comparison_2018, wp_comparison_2023, by = "id") %>%
#   mutate(difference_pd_wp = abs(wp_pd_2018_10) - abs(wp_pd_2023_10))
# 
# difference

