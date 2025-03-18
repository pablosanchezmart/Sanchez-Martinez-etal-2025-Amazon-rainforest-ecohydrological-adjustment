#### LEAF WATER POTENTIAL ######################################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")

#### DATA ---------------------------------------------------------------------- ####

all_leaf_wp_2023 <- read_csv(paste0(root.dir, "data_processed/leaf_water_potential_water_content/big_campaign_water_potentials_2023.csv")) %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         ID = paste0(plot, "_", id)) %>%
  dplyr::select(ID, plot, dbh2023, campaign = season, genus, species, leaf_wp_md_MPa = psiMD, leaf_wp_pd_MPa = psiPD)

all_leaf_wp_2023 %>%
  filter(plot == "TFE") %>%
  pull(ID) %>%
  unique() %>%
  length()

wet_leaf_wp_2023 <- all_leaf_wp_2023 %>%
  filter(campaign == "wet") %>%
  mutate(campaign = "May-2023")

dry_leaf_wp_2023 <- all_leaf_wp_2023 %>%
  filter(campaign == "dry") %>%
  mutate(campaign = "Oct.-2023")

all_leaf_wp_2023 <- all_leaf_wp_2023 %>%
  mutate(campaign = "all")

all_wet_dry_leaf_wp_2023 <- bind_rows(all_leaf_wp_2023, wet_leaf_wp_2023, dry_leaf_wp_2023)

## Paulo previous data

# wp_dry_2018.data <- read_csv(file = paste0(data.path, "caxuana_traits/treeData.csv")) %>%
#   mutate(plot = ifelse(treatment == "tfe", "TFE", "Control"),
#          id = paste0(plot, "_", tree),
#          campaign = "Oct.-2018",
#          leaf_wp_md_MPa = -abs(psi_md),
#          leaf_wp_pd_MPa = -abs(psi_pd)) %>%
#   dplyr::select(id, plot, genus, leaf_wp_md_MPa, leaf_wp_pd_MPa, campaign)
# 
# wp_dry_2018.data <- aggregate(wp_dry_2018.data, by = list(wp_dry_2018.data$id), FUN = meanOrMode) %>%
#   dplyr::select(-Group.1)
# 
# wp_dry_2018.data$genus <- paste0(wp_dry_2018.data$genus)


all_leaf_wp <- bind_rows(wet_leaf_wp_2023, dry_leaf_wp_2023)


#### WATER POTENTIAL AT PREDAWN ------------------------------------------------####

pdf("results/manuscript/all_dry_wet_max_leaf_wp_pd.pdf", height = h, width = w)
leaf_wp_md_plot.plot <- ggplot(all_wet_dry_leaf_wp_2023, 
                               aes(x = fct_inorder(campaign), 
                                   y = leaf_wp_pd_MPa, 
                                   color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Leaf water potential at predawn (MPa)") + 
  theme_minimal()  +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(-5, 0))
leaf_wp_md_plot.plot
dev.off()

all_wet_dry_leaf_wp_2023 %>%
  filter(campaign == "May-2023") %>%
  filter(plot == "Control") %>%
  pull(leaf_wp_pd_MPa) %>%
  mean(na.rm = T)

all_wet_dry_leaf_wp_2023 %>%
  filter(campaign == "May-2023") %>%
  filter(plot == "TFE") %>%
  pull(leaf_wp_pd_MPa) %>%
  mean(na.rm = T)

all_wet_dry_leaf_wp_2023 %>%
  filter(campaign == "Oct.-2023") %>%
  filter(plot == "Control") %>%
  pull(leaf_wp_pd_MPa) %>%
  mean(na.rm = T)

all_wet_dry_leaf_wp_2023 %>%
  filter(campaign == "Oct.-2023") %>%
  filter(plot == "TFE") %>%
  pull(leaf_wp_pd_MPa) %>%
  mean(na.rm = T)


### WP pd all ####

## wp pd vs. dbh

pd_dbh.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ dbh2023, data = all_leaf_wp)
pd_dbh.lm_sum <- summary(pd_dbh.lm)
pd_dbh.lm_sum

pd_all_dbh <- data.frame("response" = "WP pd (whole year)", 
                         "predictor" = "dbh",
                         "variance_explained" = round(pd_dbh.lm_sum$adj.r.squared, 2))


## wp pd vs. genus

# pd_gen.lmer <- lmer(log(abs(leaf_wp_pd_MPa)) ~ (1|genus), 
#                     data = all_leaf_wp)
# summary(pd_gen.lmer)
# gen_var <- 0.1535   / (1.3445    + 0.1535   )

pd_gen.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ genus, data = all_leaf_wp)
pd_gen.lm_sum <- summary(pd_gen.lm)
pd_gen.lm_sum

pd_all_gen <- data.frame("response" = "WP pd (whole year)", 
                         "predictor" = "genus",
                         "variance_explained" = round(pd_gen.lm_sum$adj.r.squared, 2))

## wp pd vs. plot

pd_all.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ plot, 
                data = all_leaf_wp)
summary(pd_all.lm)

## wp pd vs. plot + dbh

pd_all_dbh.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ plot + dbh2023, 
                    data = all_leaf_wp)
summary(pd_all_dbh.lm)

## wp pd vs. plot + dbh | genus

pd_all_dbh_gen.lmer <- lmer(log(abs(leaf_wp_pd_MPa)) ~ plot + dbh2023 + (1|genus), 
                            data = all_leaf_wp)
summary(pd_all_dbh_gen.lmer)

## variances

pd_all <- rbind(pd_all_dbh, pd_all_gen)
pd_all


### WP pd dry ####

## wp pd vs. dbh

pd_dbh.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ dbh2023, data = dry_leaf_wp_2023)
pd_dbh.lm_sum <- summary(pd_dbh.lm)

pd_dry_dbh <- data.frame("response" = "WP pd (dry season)", 
                         "predictor" = "dbh",
                         "variance_explained" = round(pd_dbh.lm_sum$adj.r.squared, 2))


## wp pd vs. genus
# 
# pd_gen.lmer <- lmer(log(abs(leaf_wp_pd_MPa)) ~ (1|genus), 
#                     data = dry_leaf_wp_2023)
# summary(pd_gen.lmer)
# gen_var <- 0.3298/ (0.4376 + 0.3298)

pd_gen.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ genus, 
                    data = dry_leaf_wp_2023)
pd_gen.lm_sum <- summary(pd_gen.lm)

pd_dry_gen <- data.frame("response" = "WP pd (dry season)", "predictor" = "genus", "variance_explained" = round(pd_gen.lm_sum$adj.r.squared, 2))


## wp pd vs. plot

pd_all.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ plot, 
                data = dry_leaf_wp_2023)
summary(pd_all.lm)

## wp pd vs. plot + dbh

pd_all_dbh.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ plot + dbh2023, 
                    data = dry_leaf_wp_2023)
summary(pd_all_dbh.lm)

## wp pd vs. plot + dbh | genus

pd_all_dbh_gen.lmer <- lmer(log(abs(leaf_wp_pd_MPa)) ~ plot + dbh2023 + (1|genus), 
                            data = dry_leaf_wp_2023)
summary(pd_all_dbh_gen.lmer)

## variances

pd_dry <- rbind(pd_dry_dbh, pd_dry_gen)
pd_dry


### WP pd wet ####

## wp pd vs. dbh

pd_dbh.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ dbh2023, data = wet_leaf_wp_2023)
pd_dbh.lm_sum <- summary(pd_dbh.lm)
pd_dbh.lm_sum

pd_wet_dbh <- data.frame("response" = "WP pd (wet season)",
                         "predictor" = "dbh",
                         "variance_explained" = round(pd_dbh.lm_sum$adj.r.squared, 2))


## wp pd vs. genus

# pd_gen.lmer <- lmer(log(abs(leaf_wp_pd_MPa)) ~ (1|genus), 
#                     data = wet_leaf_wp_2023)
# summary(pd_gen.lmer)
# gen_var <- 0.02344  / (0.45673   + 0.02344)

pd_gen.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ genus,
                    data = wet_leaf_wp_2023)
pd_gen.lm_sum <- summary(pd_gen.lm)

pd_wet_gen <- data.frame("response" = "WP pd (wet season)",
                         "predictor" = "genus",
                         "variance_explained" = round(pd_gen.lm_sum$adj.r.squared, 2))


## wp pd vs. plot

pd_all.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ plot, 
                data = wet_leaf_wp_2023)
summary(pd_all.lm)

## wp pd vs. plot + dbh

pd_all_dbh.lm <- lm(log(abs(leaf_wp_pd_MPa)) ~ plot + dbh2023, 
                    data = wet_leaf_wp_2023)
summary(pd_all_dbh.lm)

## wp pd vs. plot + dbh | genus

pd_all_dbh_gen.lmer <- lmer(log(abs(leaf_wp_pd_MPa)) ~ plot + dbh2023 + (1|genus), 
                            data = wet_leaf_wp_2023)
summary(pd_all_dbh_gen.lmer)

## variances

pd_wet <- rbind(pd_wet_dbh, pd_wet_gen)
pd_wet


#### WATER POTENTIAL AT MIDDAY ------------------------------------------------ ####

### WP md ####

pdf("results/manuscript/all_dry_wet_max_leaf_wp_md.pdf", height = h, width = w)
leaf_wp_md_plot.plot <- ggplot(all_wet_dry_leaf_wp_2023, 
                               aes(x = fct_inorder(as.factor(campaign)), 
                                   y = leaf_wp_md_MPa, 
                                   color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Leaf water potential at midday (MPa)") + 
  theme_minimal()  +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(-5, 0))
leaf_wp_md_plot.plot
dev.off()

all_wet_dry_leaf_wp_2023 %>%
  filter(campaign == "May-2023") %>%
  filter(plot == "Control") %>%
  pull(leaf_wp_md_MPa) %>%
  mean(na.rm = T)

all_wet_dry_leaf_wp_2023 %>%
  filter(campaign == "May-2023") %>%
  filter(plot == "TFE") %>%
  pull(leaf_wp_md_MPa) %>%
  mean(na.rm = T)

all_wet_dry_leaf_wp_2023 %>%
  filter(campaign == "Oct.-2023") %>%
  filter(plot == "Control") %>%
  pull(leaf_wp_md_MPa) %>%
  mean(na.rm = T)

all_wet_dry_leaf_wp_2023 %>%
  filter(campaign == "Oct.-2023") %>%
  filter(plot == "TFE") %>%
  pull(leaf_wp_md_MPa) %>%
  mean(na.rm = T)



### WP md all ####

## wp md vs. dbh

md_dbh.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ dbh2023, data = all_leaf_wp)
md_dbh.lm_sum <- summary(md_dbh.lm)
md_dbh.lm_sum

md_all_dbh <- data.frame("response" = "WP md (whole year)", 
                         "predictor" = "dbh",
                         "variance_explained" = round(md_dbh.lm_sum$adj.r.squared, 2))


## wp md vs. genus

# md_gen.lmer <- lmer(log(abs(leaf_wp_md_MPa)) ~ (1|genus), 
#                     data = all_leaf_wp)
# summary(md_gen.lmer)
# gen_var <- 0.06975   / (0.51692    + 0.06975)

md_gen.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ genus, 
                    data = all_leaf_wp)
md_gen.lm_sum <- summary(md_gen.lm)

md_all_gen <- data.frame("response" = "WP md (whole year)", 
                         "predictor" = "genus",
                         "variance_explained" = round(md_gen.lm_sum$adj.r.squared, 2))


## wp md vs. plot

md_all.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ plot, 
                data = all_leaf_wp)
summary(md_all.lm)

## wp md vs. plot + dbh

md_all_dbh.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ plot + dbh2023, 
                    data = all_leaf_wp)
summary(md_all_dbh.lm)

## wp md vs. plot + dbh | genus

md_all_dbh_gen.lmer <- lmer(log(abs(leaf_wp_md_MPa)) ~ plot + dbh2023 + (1|genus), 
                            data = all_leaf_wp)
summary(md_all_dbh_gen.lmer)

## variances

md_all <- rbind(md_all_dbh, md_all_gen)
md_all


### WP md dry ####

## wp md vs. dbh

md_dbh.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ dbh2023, data = dry_leaf_wp_2023)
md_dbh.lm_sum <- summary(md_dbh.lm)

md_dry_dbh <- data.frame("response" = "WP md (dry season)", 
                         "predictor" = "dbh",
                         "variance_explained" = round(md_dbh.lm_sum$adj.r.squared, 2))


## wp md vs. genus

# md_gen.lmer <- lmer(log(abs(leaf_wp_md_MPa)) ~ (1|genus), 
#                     data = dry_leaf_wp_2023)
# summary(md_gen.lmer)
# gen_var <- 0.08172  / (0.18593   + 0.08172  )

md_gen.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ genus, 
                    data = dry_leaf_wp_2023)
md_gen.lm_sum <- summary(md_gen.lm)

md_dry_gen <- data.frame("response" = "WP md (dry season)",
                         "predictor" = "genus",
                         "variance_explained" = round(md_gen.lm_sum$adj.r.squared, 2))


## wp md vs. plot

md_all.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ plot, 
                data = dry_leaf_wp_2023)
summary(md_all.lm)

## wp md vs. plot + dbh

md_all_dbh.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ plot + dbh2023, 
                    data = dry_leaf_wp_2023)
summary(md_all_dbh.lm)

## wp md vs. plot + dbh | genus

md_all_dbh_gen.lmer <- lmer(log(abs(leaf_wp_md_MPa)) ~ plot + dbh2023 + (1|genus), 
                            data = dry_leaf_wp_2023)
summary(md_all_dbh_gen.lmer)

## variances

md_dry <- rbind(md_dry_dbh, md_dry_gen)
md_dry


### WP md wet ####

## wp md vs. dbh

md_dbh.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ dbh2023, data = wet_leaf_wp_2023)
md_dbh.lm_sum <- summary(md_dbh.lm)
md_dbh.lm_sum

md_wet_dbh <- data.frame("response" = "WP md (wet season)",
                         "predictor" = "dbh",
                         "variance_explained" = round(md_dbh.lm_sum$adj.r.squared, 2))


## wp md vs. genus

md_gen.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ genus, 
                    data = wet_leaf_wp_2023)
md_gen.lm_sum <- summary(md_gen.lm)

md_wet_gen <- data.frame("response" = "WP md (wet season)",
                         "predictor" = "genus",
                         "variance_explained" = round(md_gen.lm_sum$adj.r.squared, 2))


## wp md vs. plot

md_all.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ plot, 
                data = wet_leaf_wp_2023)
summary(md_all.lm)

## wp md vs. plot + dbh

md_all_dbh.lm <- lm(log(abs(leaf_wp_md_MPa)) ~ plot + dbh2023, 
                    data = wet_leaf_wp_2023)
summary(md_all_dbh.lm)

## wp md vs. plot + dbh | genus

md_all_dbh_gen.lmer <- lmer(log(abs(leaf_wp_md_MPa)) ~ plot + dbh2023 + (1|genus), 
                            data = wet_leaf_wp_2023)
summary(md_all_dbh_gen.lmer)

## variances

md_wet <- rbind(md_wet_dbh, md_wet_gen)
md_wet


#### VARIANCE EXPLAINED -------------------------------------------------------- ####

leaf_wp_variances <- bind_rows(md_all, md_wet, md_dry,
                               pd_all, pd_wet, pd_dry)

write_csv(leaf_wp_variances, "results/leaf_wp_variance_explained.csv")

