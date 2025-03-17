#### RELATIONSHIP UPSCALING STEAM WATER CONTENT DAILY DATA ################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")
forcerun <- T

#### DATA ---------------------------------------------------------------------- ####

# stem_wc_2023 <- read_csv(paste0(root.dir, "data_processed/caxiuana_stem_water_content/individual_15min_stem_water_content.csv"))

stem_wc_2023 <- read_csv(paste0(root.dir, "data_processed/caxiuana_stem_water_content/processed_stem_water_content_2023-05-02-2024-05-31.csv")) %>%
  mutate(date = as_date(timestamp),
         date_id = paste0(date, "_", ID),
         hour = hour(timestamp)) %>%
  dplyr::select(date_id, date, everything(), -timestamp_ID, -teros12_sensor, -bulk_EC_mS.cm, -size_class)
names(stem_wc_2023)

# metadata (taxonomy)

metadata <- readxl::read_excel(paste0(data.path, "metadata_cax_radar/cax_radar_metadata_caxiuana_10_2023.xlsx"), sheet = 1) %>%
  mutate(size_class = ifelse(dbh_2023_cm > 30, "emergent", "non-emergent"))  %>%  # more than 30m height
  dplyr::select(ID, genus, dbh_2023_cm, size_class)

toDaily_stem_wc_2023 <- merge(stem_wc_2023, metadata, by = "ID", all.x = T)


## Daily stem water content

if(!file.exists("data_processed/daily_cleaned_processed_stem_vwc.csv") | forcerun){

daily_stem_wc_2023 <- aggregate(toDaily_stem_wc_2023,
                                    by = list(toDaily_stem_wc_2023$date_id),
                                    FUN = meanOrMode) %>%
  dplyr::select(-Group.1)

### maximum daily sapflow

daily_max_sapflow_2023 <- aggregate(toDaily_stem_wc_2023[, c("clean_calibrated_water_content_m3.m3", "tempCor_gf_clean_calibrated_water_content_m3.m3")],
                                    by = list(toDaily_stem_wc_2023$date_id),
                                    FUN = quantile, probs = 0.95, na.rm = T)  %>%
  rename(max_clean_calibrated_water_content_m3.m3 = clean_calibrated_water_content_m3.m3,
         max_tempCor_gf_clean_calibrated_water_content_m3.m3 = tempCor_gf_clean_calibrated_water_content_m3.m3,
         date_id = Group.1)

daily_max_sapflow_2023[sapply(daily_max_sapflow_2023, is.infinite)] <- NA

# Merge mean with maximum sapflow

daily_stem_wc_2023 <- merge(daily_stem_wc_2023, daily_max_sapflow_2023, by = "date_id") %>%
  # rename(date_id = Group.1) %>%
  mutate(date = ymd(date)) %>%
  dplyr::select(date_id, date, plot, ID, species, everything())


### stem wc reduction ####

daily_stem_wc_2023$stem_vwc_reduction_m3_m3 <- NA
daily_stem_wc_2023$stem_vwc_reduction_perc <- NA

for(ind in unique(daily_stem_wc_2023$ID)){
  
  # whole tree
  
  maximum_annual_stem_wc <- daily_stem_wc_2023 %>%
    filter(ID == ind) %>%
    pull(max_tempCor_gf_clean_calibrated_water_content_m3.m3) %>%
    # quantile(., probs = 0.95, na.rm = T)
    max(., na.rm = T)
  
  # absolute
  daily_stem_wc_2023[daily_stem_wc_2023$ID == ind, "stem_vwc_reduction_m3_m3"] <- maximum_annual_stem_wc - daily_stem_wc_2023[daily_stem_wc_2023$ID == ind, "max_tempCor_gf_clean_calibrated_water_content_m3.m3"]
  
  # relative
  daily_stem_wc_2023[daily_stem_wc_2023$ID == ind, "stem_vwc_reduction_perc"] <- (daily_stem_wc_2023[daily_stem_wc_2023$ID == ind, "stem_vwc_reduction_m3_m3"] / maximum_annual_stem_wc) * 100
  
  daily_stem_wc_2023[sapply(daily_stem_wc_2023, is.infinite)] <- NA
}

  write_csv(daily_stem_wc_2023, "data_processed/daily_cleaned_processed_stem_vwc.csv")

} else {
  daily_stem_wc_2023 <- read_csv("data_processed/daily_cleaned_processed_stem_vwc.csv")
}


## whole year and examples plot

all_year_daily_stem_wc_2023 <- daily_stem_wc_2023 %>%
  mutate(month = "Whole period")

wet_daily_stem_wc_2023 <- daily_stem_wc_2023 %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2023-06-01") %>%
  mutate(month = "2023-05")

dry_daily_stem_wc_2023 <- daily_stem_wc_2023 %>%
  filter(date >= "2023-10-01") %>%
  filter(date < "2023-11-01") %>%
  mutate(month = "2023-10")

wet_dry_daily_stem_wc_2023.data <- bind_rows(all_year_daily_stem_wc_2023, wet_daily_stem_wc_2023, dry_daily_stem_wc_2023)


#### Distributions ####

ggplot(data = stem_wc_2023, aes (x = tempCor_gf_clean_calibrated_water_content_m3.m3)) +
  geom_density()


### DAILY MAXIMUM STEM WATER CONTENT ------------------------------------------- ####

pdf("results/manuscript/all_dry_wet_max_stem_wc.pdf", height = h, width = w)
stem_wc_plot.plot <- ggplot(wet_dry_daily_stem_wc_2023.data, 
                            aes(x = fct_inorder(as.factor(month)), 
                                y = max_tempCor_gf_clean_calibrated_water_content_m3.m3, 
                                color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Stem water content (m3/m3)") + 
  theme_minimal()  +
  theme(legend.position = "none")
stem_wc_plot.plot
dev.off()

stem_wc_plot_all.mmod <- lmer(formula = max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + dbh_2023_cm + (1|genus/ID),
                                         data = all_year_daily_stem_wc_2023)
summary(stem_wc_plot_all.mmod)

stem_wc_plot_wet.mmod <- lmer(formula = max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + dbh_2023_cm + (1|genus/ID),
                                         data = wet_daily_stem_wc_2023)
summary(stem_wc_plot_wet.mmod)

stem_wc_plot_dry.mmod <- lmer(formula = max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + dbh_2023_cm + (1|genus/ID),
                                         data = dry_daily_stem_wc_2023)
summary(stem_wc_plot_dry.mmod)


### Differences month by month ###

daily_stem_wc_2023$month_year <- ymd(paste0(year(daily_stem_wc_2023$date), "-", month(daily_stem_wc_2023$date), "-01"))

coefs.df <- data.frame()
for(m in as_date(sort(unique(daily_stem_wc_2023$month_year)))){
  
  # Filter by month
  
  daily_stem_wc_2023_month <- daily_stem_wc_2023 %>%
    filter(month_year == m)
  
  sapflow_plot_month.mmod <- lmer(formula = max_tempCor_gf_clean_calibrated_water_content_m3.m3  ~ plot + (1|ID),
                                  data = daily_stem_wc_2023_month)
  
  coef_month.df <- as.data.frame(coef(summary(sapflow_plot_month.mmod)))[2, ]
  coef_month.df$month <- m
  
  coefs.df <- rbind(coefs.df, coef_month.df)
}
coefs.df$month <- as_date(coefs.df$month)
coefs.df

# Save the plot

pdf("results/manuscript/daily_stem_wc_2023_month_by_month.pdf", height = h, width = w*2)
stem_wc_plot_month_2023.plot <- ggplot(data = daily_stem_wc_2023, 
                                       aes(x = fct_inorder(as.factor(month_year)), 
                                           y = max_tempCor_gf_clean_calibrated_water_content_m3.m3,
                                           color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Stem water content (m3/m3)") + 
  theme_minimal() +
  theme(legend.position = "none")
# ylim(-1, 6)
stem_wc_plot_month_2023.plot
dev.off()


#### genus and size effects all ####

## wc vs. dbh

wc_dbh.lm <- lm(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ dbh_2023_cm, data = all_year_daily_stem_wc_2023)
wc_dbh.lm_sum <- summary(wc_dbh.lm)
wc_dbh.lm_sum

wc_all_dbh <- data.frame("response" = "Max. stem wc (all year)",
                         "predictor" = "dbh",
                         "variance_explained" = round(wc_dbh.lm_sum$adj.r.squared, 2))

## wc vs. genus

wc_all_dbh_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ (1|ID) + (1|genus), 
                            data = all_year_daily_stem_wc_2023)
summary(wc_all_dbh_gen.lmer)

gen_var <- 0.001433 / (0.001433  + 0.001433 + 0.001268)

wc_all_gen <- data.frame("response" = "Max. stem wc (all year)",
                         "predictor" = "genus",
                         "variance_explained" = round(gen_var, 2))

## wp md vs. plot + dbh | genus

wc_all_dbh_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + dbh_2023_cm + + (1|ID) + (1|genus), 
                            data = all_year_daily_stem_wc_2023)
summary(wc_all_dbh_gen.lmer)

wc_all_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + dbh_2023_cm + (1|ID), 
                        data = all_year_daily_stem_wc_2023)
summary(wc_all_gen.lmer)

wc_all_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + (1|ID) + (1|genus), 
                        data = all_year_daily_stem_wc_2023)
summary(wc_all_gen.lmer)

## variances

wc_all <- rbind(wc_all_dbh, wc_all_gen)
wc_all


#### genus and size effects wet ####

## wc vs. dbh

wc_dbh.lm <- lm(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ dbh_2023_cm, data = wet_daily_stem_wc_2023)
wc_dbh.lm_sum <- summary(wc_dbh.lm)
wc_dbh.lm_sum

wc_wet_dbh <- data.frame("response" = "Max. stem wc (wet season)",
                         "predictor" = "dbh",
                         "variance_explained" = round(wc_dbh.lm_sum$adj.r.squared, 2))

## wc vs. genus

wc_wet_dbh_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ (1|ID) + (1|genus), 
                            data = wet_daily_stem_wc_2023)
summary(wc_wet_dbh_gen.lmer)

gen_var <- 0.0011856  / (0.0011856   + 0.0023459  + 0.0002787 )

wc_wet_gen <- data.frame("response" = "Max. stem wc (wet season)",
                         "predictor" = "genus",
                         "variance_explained" = round(gen_var, 2))

## wp md vs. plot + dbh | genus

wc_wet_dbh_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + dbh_2023_cm + + (1|ID) + (1|genus), 
                            data = wet_daily_stem_wc_2023)
summary(wc_wet_dbh_gen.lmer)

wc_wet_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + dbh_2023_cm + (1|ID), 
                        data = wet_daily_stem_wc_2023)
summary(wc_wet_gen.lmer)

wc_wet_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + (1|ID) + (1|genus), 
                        data = wet_daily_stem_wc_2023)
summary(wc_wet_gen.lmer)

## variances

wc_wet <- rbind(wc_wet_dbh, wc_wet_gen)
wc_wet


#### genus and size effects dry ####

## wc vs. dbh

wc_dbh.lm <- lm(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ dbh_2023_cm, data = dry_daily_stem_wc_2023)
wc_dbh.lm_sum <- summary(wc_dbh.lm)
wc_dbh.lm_sum

wc_dry_dbh <- data.frame("response" = "Max. stem wc (dry season)",
                         "predictor" = "dbh",
                         "variance_explained" = round(wc_dbh.lm_sum$adj.r.squared, 2))

## wc vs. genus

wc_dry_dbh_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ (1|ID) + (1|genus), 
                            data = dry_daily_stem_wc_2023)
summary(wc_dry_dbh_gen.lmer)

gen_var <- 1.627e-03  / (1.627e-03   + 3.024e-03  + 3.395e-05 )

wc_dry_gen <- data.frame("response" = "Max. stem wc (wet season)",
                         "predictor" = "genus",
                         "variance_explained" = round(gen_var, 2))

## wp md vs. plot + dbh | genus

wc_wet_dbh_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + dbh_2023_cm + + (1|ID) + (1|genus), 
                            data = dry_daily_stem_wc_2023)
summary(wc_wet_dbh_gen.lmer)

wc_wet_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + dbh_2023_cm + (1|ID), 
                        data = dry_daily_stem_wc_2023)
summary(wc_wet_gen.lmer)

wc_wet_gen.lmer <- lmer(max_tempCor_gf_clean_calibrated_water_content_m3.m3 ~ plot + (1|ID) + (1|genus), 
                        data = dry_daily_stem_wc_2023)
summary(wc_wet_gen.lmer)

## variances

wc_dry <- rbind(wc_dry_dbh, wc_dry_gen)
wc_dry


#### VARIANCE EXPLAINED -------------------------------------------------------- ####

wc_variances <- bind_rows(wc_all, wc_wet, wc_dry)

write_csv(wc_variances, "results/stem_wc_variance_explained.csv")


### DAILY REDUCTION IN STEM WATER CONTENT -------------------------------------- ####

pdf("results/manuscript/all_dry_wet_reduction_stem_wc.pdf", height = h, width = w)
red_stem_wc_plot.plot <- ggplot(wet_dry_daily_stem_wc_2023.data, 
                            aes(x = fct_inorder(as.factor(month)), 
                                y = stem_vwc_reduction_perc, 
                                color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Reduction in stem water content (%)") + 
  theme_minimal()  +
  theme(legend.position = "none")
red_stem_wc_plot.plot
dev.off()

stem_wc_red_plot_all.mmod <- lmer(formula = sqrt(stem_vwc_reduction_perc) ~ plot + dbh_2023_cm + (1|genus/ID),
                              data = all_year_daily_stem_wc_2023)
summary(stem_wc_red_plot_all.mmod)

stem_wc_red_plot_wet.mmod <- lmer(formula = sqrt(stem_vwc_reduction_perc) ~ plot + dbh_2023_cm + (1|genus/ID),
                              data = wet_daily_stem_wc_2023)
summary(stem_wc_red_plot_wet.mmod)

stem_wc_red_plot_dry.mmod <- lmer(formula = sqrt(stem_vwc_reduction_perc) ~ plot + dbh_2023_cm + (1|genus/ID),
                              data = dry_daily_stem_wc_2023)
summary(stem_wc_red_plot_dry.mmod)


### Differences month by month ###

daily_stem_wc_2023$month_year <- ymd(paste0(year(daily_stem_wc_2023$date), "-", month(daily_stem_wc_2023$date), "-01"))

coefs.df <- data.frame()
for(m in as_date(sort(unique(daily_stem_wc_2023$month_year)))){
  
  # Filter by month
  
  daily_stem_wc_2023_month <- daily_stem_wc_2023 %>%
    filter(month_year == m)
  
  sapflow_plot_month.mmod <- lmer(formula = sqrt(stem_vwc_reduction_perc)  ~ plot + (1|ID),
                                  data = daily_stem_wc_2023_month)
  
  coef_month.df <- as.data.frame(coef(summary(sapflow_plot_month.mmod)))[2, ]
  coef_month.df$month <- m
  
  coefs.df <- rbind(coefs.df, coef_month.df)
}
coefs.df$month <- as_date(coefs.df$month)
coefs.df

# Save the plot

pdf("results/manuscript/daily_stem_wc_reduction_2023_month_by_month.pdf", height = h, width = w*2)
stem_wc_plot_month_2023.plot <- ggplot(data = daily_stem_wc_2023, 
                                       aes(x = fct_inorder(as.factor(month_year)), 
                                           y = stem_vwc_reduction_perc,
                                           color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Stem water content reduction (%)") + 
  theme_minimal() +
  theme(legend.position = "none")
# ylim(-1, 6)
stem_wc_plot_month_2023.plot
dev.off()


