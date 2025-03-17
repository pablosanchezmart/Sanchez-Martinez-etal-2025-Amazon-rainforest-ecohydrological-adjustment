#### RELATIONSHIP SAP FLOW - VPD #####################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")
forcerun <- T

#### DATA ---------------------------------------------------------------------- ####

## Meteorological data

# met_2023 <- read_csv(paste0(data.path, "met/2022-09-18-2023-12-16_met_control_processed.csv"))

met_2023 <- read_csv(paste0(data.path, "met/1/2023-01-01-2024-12-31_met_control_processed.csv")) %>%
  filter(!is.na(date)) %>%
  mutate(month = as_date(dmy(paste0("01-", month(date, label = F), "-", year(date)))),
         ID = "Control") %>% # for gap filling pursposes
  dplyr::select(timestamp, date, ID, month, vpd16m_kPa, vpd42m_kPa, precip_mm, t16m_C) %>%
  filter(date > "2023-04-01")  %>%
  filter(date < "2024-05-01")

met_2023 <- gapFillTimeSeries(met_2023, variable = "vpd16m_kPa")
met_2023 <- gapFillTimeSeries(met_2023, variable = "vpd42m_kPa")
met_2023 <- gapFillTimeSeries(met_2023, variable = "t16m_C")
tail(met_2023)

met_2023 <- met_2023 %>%
  dplyr::select(-ID) %>%
  arrange(timestamp)

met.variables <- c("vpd28m_kPa", "rh28m_perc", "t28m_C")

# metadata (taxonomy)

metadata <- readxl::read_excel(paste0(data.path, "metadata_cax_radar/cax_radar_metadata_caxiuana_10_2023.xlsx"), sheet = 1) %>%
  mutate(size_class = ifelse(dbh_2023_cm > 30, "emergent", "non-emergent"))  %>%  # more than 30m height
  dplyr::select(ID, genus, dbh_2023_cm, size_class)


## Sap flow (sub-daily)

sapflow.variables <- c("cleaned_bl_sap_flux_Kg_h", "gf_cleaned_bl_sap_flux_Kg_h", "sap_flux_kg_h_cm", "gf_sap_flux_kg_h_cm")

id_variables <- c("date", "ID", "genus", "species", "plot", "dbh_2023_cm", "size_class")

mean_variables <- c(sapflow.variables, 
                    # "stem_area_cm", 
                    # "t2m_C", "rh2m_perc","tdc2m_C", "twc2m_C",
                    # "vp2m_mbar", "t16m_C", "rh16m_perc", "tdc16m_C", "twc16m_C", "vp16m_mbar",
                    # "t28m_C", "rh28m_perc", "tdc28m_C", "twc28m_C", "vp28m_mbar", "t42m_C", "rh42m_perc", 
                    # "tdc42m_C", "twc_42_C", "vp42m_bar", "rad_global_W_m2", "rad_glob_tot_MJ_m2", 
                    # "rad_net_W_m2", "t_canopy_C", "t_canopy_2_C", "wind_speed_m_s",
                    # "wind_speed_max_m_s", "wind_direction", 
                    # "vpd2m_kPa", "vpd28m_kPa"
                    "vpd16m_kPa", "gf_vpd16m_kPa", "vpd42m_kPa", "gf_vpd42m_kPa")

sum_variables <- c(sapflow.variables, "precip_mm")

if(!file.exists("data_processed/cleaned_processed_sapflow_2022-11-17-2024-06_01.csv") | forcerun){
  
  sap_flow_2023 <- read_csv(paste0(data.path, "caxuana_sapflow/cleaned_processed_sapflow_2022-11-17-2024-06-01.csv")) %>%
    filter(!is.na(ID))

  sapflow_met_2023 <- merge(sap_flow_2023,
                            met_2023,
                            by = "timestamp",
                            all.x = T) %>%
    filter(timestamp > "2023-04-01") %>%
    mutate(log_bl_sap_flux_Kg_h = log(bl_sap_flux_Kg_h + 10))
  # hour = hour(timestamp),
  # date = as_date(timestamp)
  # date_hour = paste0(date, "_", hour),
  # date_hour_id = paste0(date_hour, "_", ID)
  # )
  
  sapflow_met_2023 <- merge(sapflow_met_2023, metadata, 
                            by = "ID", 
                            all.x = T) %>%
    arrange(timestamp, ID) %>%
    dplyr::select(ID,  plot, timestamp, genus, species, dbh_2023_cm, size_class, everything())
  
  write_csv(sapflow_met_2023, "data_processed/cleaned_processed_sapflow_2022-2024-06-01.csv")
  print("data_processed/cleaned_processed_sapflow_2022-2024-06-01.csv")
} else {
  sapflow_met_2023 <- read_csv("data_processed/cleaned_processed_sapflow_2022-11-17-2024-06-01.csv")
}


## Daily sapflow (only using daylight values (excluding night))

if(!file.exists("data_processed/daily_cleaned_processed_sapflow_2022-11-17-2024-06-01.csv") | forcerun){
  
  toDaily_sapflow_met_2023 <- sapflow_met_2023 %>%
    mutate(date = as_date(timestamp),
           date_id = paste0(date, "_", ID),
           hour = hour(timestamp)) %>%
    filter(hour > 6, hour < 20) %>%
    dplyr::select(date_id, date, everything(), -timestamp, -hour)
  
  daily_sapflow_met_2023 <- aggregate(toDaily_sapflow_met_2023[, c(id_variables, mean_variables)],
                                      by = list(toDaily_sapflow_met_2023$date_id),
                                      FUN = meanOrMode) 
  
  ### maximum daily sapflow
  
  daily_max_sapflow_2023 <- aggregate(toDaily_sapflow_met_2023[, c(sapflow.variables)],
                                      by = list(toDaily_sapflow_met_2023$date_id),
                                      FUN = quantile, probs = 0.95, na.rm = T)  %>%
    rename(max_daily_sap_flux_Kg_h = cleaned_bl_sap_flux_Kg_h,
           max_sap_flux_kg_h_cm = sap_flux_kg_h_cm,
           max_daily_gf_sap_flux_Kg_h = gf_cleaned_bl_sap_flux_Kg_h,
           max_gf_sap_flux_kg_h_cm = gf_sap_flux_kg_h_cm)
  daily_max_sapflow_2023[sapply(daily_max_sapflow_2023, is.infinite)] <- NA
  
  ### total daily sapflow
  
  daily_sum_sapflow_2023 <- aggregate(toDaily_sapflow_met_2023[, sum_variables],
                                      by = list(toDaily_sapflow_met_2023$date_id),
                                      FUN = sum, na.rm = T)  %>%
    rename(daily_total_cleaned_bl_sap_flux_Kg_day = cleaned_bl_sap_flux_Kg_h,
           daily_total_cleaned_bl_sap_flux_Kg_day_cm = sap_flux_kg_h_cm,
           daily_total_gf_cleaned_bl_sap_flux_Kg_day = gf_cleaned_bl_sap_flux_Kg_h,
           daily_total_gf_cleaned_bl_sap_flux_Kg_day_cm = gf_sap_flux_kg_h_cm)
  
  # Merge mean with maximum sapflow
  
  daily_sapflow_met_2023 <- merge(daily_sapflow_met_2023, daily_max_sapflow_2023, by = "Group.1") %>%
    # rename(date_id = Group.1) %>%
    mutate(date = ymd(date)) %>%
    dplyr::select(Group.1, date, plot, ID, species, everything())
  
  
  # add total sapflow
  
  daily_sapflow_met_2023 <- merge(daily_sapflow_met_2023, daily_sum_sapflow_2023, by = "Group.1") %>%
    rename(date_id = Group.1) %>%
    mutate(date = ymd(date)) %>%
    dplyr::select(date, plot, ID, species, everything(), -date_id)
  
  ### Reduction of sapflow compared to wet season
  
  daily_sapflow_met_2023$sap_flux_reduction_kg_h <- NA
  daily_sapflow_met_2023$sap_flux_reduction_kg_h_cm <- NA
  
  daily_sapflow_met_2023$gf_sap_flux_reduction_kg_h <- NA
  daily_sapflow_met_2023$gf_sap_flux_reduction_kg_h_cm <- NA
  
  for(ind in unique(daily_sapflow_met_2023$ID)){
    
    # whole tree
    
    maximum_annual_sapflow <- daily_sapflow_met_2023 %>%
      filter(ID == ind) %>%
      pull(max_daily_sap_flux_Kg_h) %>%
      # quantile(., probs = 0.95, na.rm = T)
      max(., na.rm = T)
    
    maximum_daily_sapflow <- daily_sapflow_met_2023 %>%
      filter(ID == ind) %>%
      pull(daily_total_cleaned_bl_sap_flux_Kg_day) %>%
      # quantile(., probs = 0.95, na.rm = T)
      max(., na.rm = T)
    
    # absolute
    
    daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "sap_flux_reduction_kg_h"] <- maximum_annual_sapflow - daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "max_daily_sap_flux_Kg_h"]
    daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "gf_sap_flux_reduction_kg_h"] <- maximum_annual_sapflow - daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "max_daily_gf_sap_flux_Kg_h"]
    
    # relative
    
    daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "sap_flux_reduction_h_perc"] <- (daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "sap_flux_reduction_kg_h"] / maximum_annual_sapflow) * 100
    daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "gf_sap_flux_reduction_h_perc"] <- (daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "gf_sap_flux_reduction_kg_h"] / maximum_annual_sapflow) * 100
    
    # daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "sap_flux_reduction_day_perc"] <- (daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "sap_flux_reduction_kg_day"] / maximum_daily_sapflow) * 100
    
    daily_sapflow_met_2023[sapply(daily_sapflow_met_2023, is.infinite)] <- NA
    
    # per cm
    
    maximum_annual_sapflow_cm <- daily_sapflow_met_2023 %>%
      filter(ID == ind) %>%
      pull(max_sap_flux_kg_h_cm) %>%
      # quantile(., probs = 0.95, na.rm = T)
      max(., na.rm = T)
    
    
    maximum_daily_sapflow_cm <- daily_sapflow_met_2023 %>%
      filter(ID == ind) %>%
      pull(daily_total_cleaned_bl_sap_flux_Kg_day_cm) %>%
      # quantile(., probs = 0.95, na.rm = T)
      max(., na.rm = T)
    
    # absolute
    
    daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "sap_flux_reduction_kg_h_cm"] <- maximum_annual_sapflow_cm - daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "max_sap_flux_kg_h_cm"]
    daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "gf_sap_flux_reduction_kg_h_cm"] <- maximum_annual_sapflow_cm - daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "max_gf_sap_flux_kg_h_cm"]
    
    # relative
    
    daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "sap_flux_reduction_cm_h_perc"] <- (daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "sap_flux_reduction_kg_h_cm"] / maximum_annual_sapflow_cm) * 100
    daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "gf_sap_flux_reduction_cm_h_perc"] <- (daily_sapflow_met_2023[daily_sapflow_met_2023$ID == ind, "gf_sap_flux_reduction_kg_h_cm"] / maximum_annual_sapflow_cm) * 100
    
    daily_sapflow_met_2023[sapply(daily_sapflow_met_2023, is.infinite)] <- NA

  }
  
  write_csv(daily_sapflow_met_2023, "data_processed/daily_cleaned_processed_sapflow_2022-11-17-2024-06-01.csv")
  print("data_processed/daily_cleaned_processed_sapflow_2022-11-17-2024-06-01.csv")
} else {
  daily_sapflow_met_2023 <- read_csv("data_processed/daily_cleaned_processed_sapflow_2022-11-17-2024-06-01.csv")
}


#### FILTERING OF THE DATA ----------------------------------------------------- ####

### Id with data problems ####

id_with_problems <- c("Control_204", "Control_252", "Control_357", "Control_359", "Control_363", "TFE_211", "TFE_80")

id_excluded_to_balance <- c("Control_316", "Control_308", "Control_322")

daily_sapflow_met_2023 <- daily_sapflow_met_2023 %>% filter(!ID %in% id_with_problems)
daily_sapflow_met_2023 <- daily_sapflow_met_2023 %>% filter(!ID %in% id_excluded_to_balance)

daily_sapflow_met_2023 %>% pull(ID) %>% unique()

### study period with all trees being monitored ####

daily_sapflow_met_2023 <- daily_sapflow_met_2023 %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2024-05-01")

head(daily_sapflow_met_2023)
tail(daily_sapflow_met_2023)

a <- daily_sapflow_met_2023 %>% filter(plot == "Control") 
length(unique(a$ID)) # 16 ind 

b <- daily_sapflow_met_2023 %>% filter(plot == "TFE")  # 15 ind
length(unique(b$ID)) # 18 ind 

a %>% group_by(size_class) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

b %>% group_by(size_class) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

daily_sapflow_met_2023$yday <- yday(daily_sapflow_met_2023$date)


### plots for main text (wet season, dry season and whole year)

## whole year and examples plot

all_yeardaily_sapflow_met_2023 <- daily_sapflow_met_2023 %>%
  mutate(month = "Whole period")

wet_daily_sapflow_met_2023 <- daily_sapflow_met_2023 %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2023-06-01") %>%
  mutate(month = "2023-05")

dry_daily_sapflow_met_2023 <- daily_sapflow_met_2023 %>%
  filter(date >= "2023-10-01") %>%
  filter(date < "2023-11-01") %>%
  mutate(month = "2023-10")

wet_dry_daily_sapflow_met_2023.data <- bind_rows(all_yeardaily_sapflow_met_2023, wet_daily_sapflow_met_2023, dry_daily_sapflow_met_2023)


#### SAPFLOW DIFFERENCES BETWEEN PLOTS ----------------------------------------- ####

#### Distributions ####

ggplot(data = daily_sapflow_met_2023, aes (x = cleaned_bl_sap_flux_Kg_h)) +
  geom_density()

ggplot(data = daily_sapflow_met_2023, aes (x = log(cleaned_bl_sap_flux_Kg_h))) +
  geom_density()

ggplot(data = daily_sapflow_met_2023, aes (x = sap_flux_kg_h_cm)) +
  geom_density()

ggplot(data = daily_sapflow_met_2023, aes (x = log(sap_flux_kg_h_cm))) +
  geom_density()

ggplot(data = daily_sapflow_met_2023, aes (x = max_daily_sap_flux_Kg_h)) +
  geom_density()

ggplot(data = daily_sapflow_met_2023, aes (x = log(max_daily_sap_flux_Kg_h))) +
  geom_density()

ggplot(data = daily_sapflow_met_2023, aes (x = daily_total_cleaned_bl_sap_flux_Kg_day)) +
  geom_density()

ggplot(data = daily_sapflow_met_2023, aes (x = log(daily_total_cleaned_bl_sap_flux_Kg_day))) +
  geom_density()

# ggplot(data = daily_sapflow_met_2023, aes (x = sap_flux_reduction_kg_day)) +
#   geom_density()
# 
# ggplot(data = daily_sapflow_met_2023, aes (x = log(sap_flux_reduction_kg_day))) +
#   geom_density()
# 
# ggplot(data = daily_sapflow_met_2023, aes (x = sap_flux_reduction_day_perc)) +
  # geom_density()
# 
# ggplot(data = daily_sapflow_met_2023, aes (x = log(sap_flux_reduction_day_perc))) +
  # geom_density()

ggplot(data = daily_sapflow_met_2023, aes (x = sap_flux_reduction_kg_h)) +
  geom_density()

ggplot(data = daily_sapflow_met_2023, aes (x = sap_flux_reduction_h_perc)) +
  geom_density()

ggplot(data = daily_sapflow_met_2023, aes (x = sap_flux_reduction_kg_h_cm)) +
  geom_density()

ggplot(data = daily_sapflow_met_2023, aes (x = sqrt(sap_flux_reduction_kg_h_cm))) +
  geom_density()

### maximum hourly sapflow per cm vs. plot ####

# using only observations

sapflow_cm_plot.mmod <- lmer(formula = log(max_sap_flux_kg_h_cm) ~ dbh_2023_cm + plot + (1|genus/ID), 
                             data = daily_sapflow_met_2023)
summary(sapflow_cm_plot.mmod)

### month by month ###

daily_sapflow_met_2023$month <- month(daily_sapflow_met_2023$date)

coefs.df <- data.frame()
for(m in unique(daily_sapflow_met_2023$month)){
  
  # Filter by month
  
  daily_sapflow_met_2023_month <- daily_sapflow_met_2023 %>%
    filter(month == m)
  
  sapflow_plot_month.mmod <- lmer(formula = log(max_sap_flux_kg_h_cm + 10) ~ plot + (1|ID),
                                  data = daily_sapflow_met_2023_month)
  
  coef_month.df <- as.data.frame(coef(summary(sapflow_plot_month.mmod)))[2, ]
  coef_month.df$month <- m
  
  coefs.df <- rbind(coefs.df, coef_month.df)
}
round(coefs.df, 3)

pdf("results/manuscript/max_saplfow_kg_h_cm_vs_plot_2023_month_by_month.pdf", height = h, width = w*2)
max_sapflow_cm_plot_month_2023.plot <- ggplot(data = daily_sapflow_met_2023, 
                                              aes(x = fct_inorder(as.factor(month_year)), 
                                                  y = max_sap_flux_kg_h_cm,
                                                  color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  ylim(0, 0.2) +
  xlab("") + ylab("Maximum hourly sap flow per day (kg/h cm)") + 
  theme_minimal() +
  theme(legend.position = "none")
max_sapflow_cm_plot_month_2023.plot
dev.off()

# Wet and dry season

pdf("results/manuscript/all_dry_wet_sapflow_max_saplfow_kg_h_cm_vs_plot.pdf", height = h, width = w)
sapflow_plot.plot <- ggplot(wet_dry_daily_sapflow_met_2023.data, 
                            aes(x = fct_inorder(as.factor(month)), 
                                y = max_sap_flux_kg_h_cm, 
                                color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  ylim(0, 0.2) +
  xlab("") + ylab("Maximum hourly sap flow per day (kg/h cm)") + 
  theme_minimal() +
  theme(legend.position = "none")
sapflow_plot.plot
dev.off()


#### genus and size effects all ####

## max sf vs. dbh

sf_dbh.lm <- lm(log(max_sap_flux_kg_h_cm + 10) ~ dbh_2023_cm, data = all_yeardaily_sapflow_met_2023)
sf_dbh.lm_sum <- summary(sf_dbh.lm)
sf_dbh.lm_sum

sf_all_dbh <- data.frame("response" = "Max. sapflow (all year)",
                         "predictor" = "dbh",
                         "variance_explained" = round(sf_dbh.lm_sum$adj.r.squared, 2))

## max sf vs. genus

md_all_dbh_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ (1|ID) + (1|genus), 
                            data = all_yeardaily_sapflow_met_2023)
summary(md_all_dbh_gen.lmer)

gen_var <- 6.575e-06 / (1.104e-05 + 6.575e-06 + 5.924e-06)

sf_all_gen <- data.frame("response" = "Max. sapflow (all year)",
                         "predictor" = "genus",
                         "variance_explained" = round(gen_var, 2))


## max sf vs. plot + dbh | genus

md_all_dbh_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ plot + dbh_2023_cm + + (1|ID) + (1|genus), 
                            data = all_yeardaily_sapflow_met_2023)
summary(md_all_dbh_gen.lmer)

md_all_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ plot + dbh_2023_cm + + (1|ID), 
                            data = all_yeardaily_sapflow_met_2023)
summary(md_all_dbh_gen.lmer)

md_all_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ plot + (1|ID) + (1|genus), 
                        data = all_yeardaily_sapflow_met_2023)
summary(md_all_dbh_gen.lmer)

## variances

sf_all <- rbind(sf_all_dbh, sf_all_gen, sf_all_sp)
sf_all


#### genus and size effects wet ####

## max sf vs. dbh

sf_dbh.lm <- lm(log(max_sap_flux_kg_h_cm + 10) ~ dbh_2023_cm, data = wet_daily_sapflow_met_2023)
sf_dbh.lm_sum <- summary(sf_dbh.lm)
sf_dbh.lm_sum

sf_wet_dbh <- data.frame("response" = "Max. sapflow (wet season)",
                         "predictor" = "dbh",
                         "variance_explained" = round(sf_dbh.lm_sum$adj.r.squared, 2))

## max sf vs. genus

md_wet_dbh_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ (1|ID) + (1|genus), 
                            data = wet_daily_sapflow_met_2023)
summary(md_wet_dbh_gen.lmer)

gen_var <- 1.019e-05 / (3.085e-05 + 1.019e-05 + 2.238e-06)

sf_wet_gen <- data.frame("response" = "Max. sapflow (wet season)",
                         "predictor" = "genus",
                         "variance_explained" = round(gen_var, 2))


## max sf vs. plot + dbh | genus

md_wet_dbh_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ plot + dbh_2023_cm + + (1|ID) + (1|genus), 
                            data = wet_daily_sapflow_met_2023)
summary(md_wet_dbh_gen.lmer)

md_wet_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ plot + dbh_2023_cm + + (1|ID), 
                        data = wet_daily_sapflow_met_2023)
summary(md_wet_dbh_gen.lmer)

md_wet_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ plot + (1|ID) + (1|genus), 
                        data = wet_daily_sapflow_met_2023)
summary(md_wet_dbh_gen.lmer)

## variances

sf_wet <- rbind(sf_wet_dbh, sf_wet_gen)
sf_wet


#### genus and size effects dry ####

## wp md vs. dbh

sf_dbh.lm <- lm(log(max_sap_flux_kg_h_cm + 10) ~ dbh_2023_cm, data = dry_daily_sapflow_met_2023)
sf_dbh.lm_sum <- summary(sf_dbh.lm)
sf_dbh.lm_sum

sf_dry_dbh <- data.frame("response" = "Max. sapflow (dry season)",
                         "predictor" = "dbh",
                         "variance_explained" = round(sf_dbh.lm_sum$adj.r.squared, 2))

## wp md vs. genus

sf_gen.lm <- lm(log(max_sap_flux_kg_h_cm + 10) ~ genus, 
                data = dry_daily_sapflow_met_2023)
sf_gen.lm_sum <- summary(sf_gen.lm)

md_wet_dbh_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ (1|ID) + (1|genus), 
                            data = dry_daily_sapflow_met_2023)
summary(md_wet_dbh_gen.lmer)

gen_var <- 5.943e-06 / (4.113e-06 + 5.943e-06 + 3.523e-07)

sf_dry_gen <- data.frame("response" = "Max. sapflow (dry season)",
                         "predictor" = "genus",
                         "variance_explained" = round(gen_var, 2))

## wp md vs. plot + dbh | genus

md_wet_dbh_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ plot + dbh_2023_cm + (1|ID) + (1|genus), 
                            data = dry_daily_sapflow_met_2023)
summary(md_wet_dbh_gen.lmer)

md_wet_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ plot + dbh_2023_cm + + (1|ID), 
                        data = dry_daily_sapflow_met_2023)
summary(md_wet_dbh_gen.lmer)

md_wet_gen.lmer <- lmer(log(max_sap_flux_kg_h_cm + 10) ~ plot + (1|ID) + (1|genus), 
                        data = dry_daily_sapflow_met_2023)
summary(md_wet_dbh_gen.lmer)

## variances

sf_dry <- rbind(sf_dry_dbh, sf_dry_gen)
sf_dry


#### VARIANCE EXPLAINED -------------------------------------------------------- ####

sf_variances <- bind_rows(sf_all, sf_wet, sf_dry)

write_csv(sf_variances, "results/sapflow_variance_explained.csv")


### reduction from maximum sapflow per cm vs. plot ####

# using only observations

reduction_sapflow_kg_h_plot.mmod <- lmer(formula = sqrt(sap_flux_reduction_cm_h_perc) ~ plot + dbh_2023_cm + (1|genus/ID),
                                         data = daily_sapflow_met_2023)
summary(reduction_sapflow_kg_h_plot.mmod)

# using gap filled data

gf_reduction_sapflow_kg_h_plot.mmod <- lmer(formula = sqrt(gf_sap_flux_reduction_cm_h_perc) ~ plot + dbh_2023_cm + (1|genus/ID),
                                         data = daily_sapflow_met_2023)
summary(gf_reduction_sapflow_kg_h_plot.mmod)

# using gap filled data looking at size class effects

reduction_sapflow_kg_h_plot_size_class.mmod <- lmer(formula = sqrt(gf_sap_flux_reduction_cm_h_perc) ~ dbh_2023_cm + (1|genus/ID),
                                         data = daily_sapflow_met_2023)
summary(reduction_sapflow_kg_h_plot_size_class.mmod)






# Save the plot

pdf("results/manuscript/all_dry_wet_sapflow_cm_proportion_vs_plot.pdf", height = h, width = w)
sapflow_plot.plot <- ggplot(wet_dry_daily_sapflow_met_2023.data, 
                            aes(x = fct_inorder(as.factor(month)), 
                                y = gf_sap_flux_reduction_cm_h_perc, 
                                color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Percentage of reduction in sapflow relative to individual annual max (%)") + 
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))
sapflow_plot.plot
dev.off()

pdf("results/all_dry_wet_sapflow_reduction_vs_plot_per_size_class.pdf", height = h, width = w)
sapflow_plot.plot <- ggplot(wet_dry_daily_sapflow_met_2023.data, 
                            aes(x = fct_inorder(as.factor(month)), 
                                y = gf_sap_flux_reduction_cm_h_perc, 
                                color = plot,
                                fill = size_class)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Sapflow reduction over maximum capacity (%)") + 
  theme_minimal() 
sapflow_plot.plot
dev.off()


### Differences month by month ###

daily_sapflow_met_2023$month_year <- ymd(paste0(year(daily_sapflow_met_2023$date), "-", month(daily_sapflow_met_2023$date), "-01"))

coefs.df <- data.frame()
for(m in as_date(sort(unique(daily_sapflow_met_2023$month_year)))){
  
  # Filter by month
  
  daily_sapflow_met_2023_month <- daily_sapflow_met_2023 %>%
    filter(month_year == m)
  
  sapflow_plot_month.mmod <- lmer(formula = sqrt(gf_sap_flux_reduction_cm_h_perc) ~ plot + dbh_2023_cm + (1|genus/ID),
                                  data = daily_sapflow_met_2023_month)
  
  coef_month.df <- as.data.frame(coef(summary(sapflow_plot_month.mmod)))[2, ]
  coef_month.df$month <- m
  
  coefs.df <- rbind(coefs.df, coef_month.df)
}
coefs.df$month <- as_date(coefs.df$month)
coefs.df

# Save the plot

pdf("results/manuscript/sapflow_reduction_kg_h_cm_vs_plot_2023_month_by_month.pdf", height = h, width = w*2)
sapflow_plot_month_2023.plot <- ggplot(data = daily_sapflow_met_2023, 
                                       aes(x = fct_inorder(as.factor(month_year)), 
                                           y = gf_sap_flux_reduction_cm_h_perc,
                                           color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Sapflow reduction over maximum capacity (kg/h)") + 
  theme_minimal() +
  theme(legend.position = "none") #  axis.text.x = element_text(angle = 90)
# ylim(-1, 6)
sapflow_plot_month_2023.plot
dev.off()


### total daily sap flow vs. plot ####

# using only observations

total_sapflow_plot.mmod <- lmer(formula = log(daily_total_cleaned_bl_sap_flux_Kg_day + 10) ~ dbh_2023_cm + plot + (1|genus/ID), 
                             data = daily_sapflow_met_2023)
summary(sapflow_cm_plot.mmod)

# using gap filled data

total_sapflow_plot.mmod <- lmer(formula = log(daily_total_gf_cleaned_bl_sap_flux_Kg_day + 10) ~ dbh_2023_cm + plot + (1|genus/ID), 
                                data = daily_sapflow_met_2023)
summary(sapflow_cm_plot.mmod)

### month by month ###

daily_sapflow_met_2023$month <- month(daily_sapflow_met_2023$date)

coefs.df <- data.frame()
for(m in unique(daily_sapflow_met_2023$month)){
  
  # Filter by month
  
  daily_sapflow_met_2023_month <- daily_sapflow_met_2023 %>%
    filter(month == m)
  
  sapflow_plot_month.mmod <- lmer(formula = log(daily_total_gf_cleaned_bl_sap_flux_Kg_day + 10) ~ plot + (1|ID),
                                  data = daily_sapflow_met_2023_month)
  
  coef_month.df <- as.data.frame(coef(summary(sapflow_plot_month.mmod)))[2, ]
  coef_month.df$month <- m
  
  coefs.df <- rbind(coefs.df, coef_month.df)
}
round(coefs.df, 3)

pdf("results/manuscript/total_sapflow_kg_vs_plot_2023_month_by_month.pdf", height = h, width = w*2)
total_sapflow_plot_month_2023.plot <- ggplot(data = daily_sapflow_met_2023, 
                                              aes(x = fct_inorder(as.factor(month_year)), 
                                                  y = daily_total_gf_cleaned_bl_sap_flux_Kg_day,
                                                  color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  ylim(0, 400) +
  xlab("") + ylab("Individual sapflow (kg/day)") + 
  theme_minimal() +
  theme(legend.position = "none")
total_sapflow_plot_month_2023.plot
dev.off()

# Wet and dry season

pdf("results/manuscript/all_dry_wet_total_sapflow_kg_vs_plot.pdf", height = h, width = w)
sapflow_plot.plot <- ggplot(wet_dry_daily_sapflow_met_2023.data, 
                            aes(x = fct_inorder(as.factor(month)), 
                                y = daily_total_gf_cleaned_bl_sap_flux_Kg_day, 
                                color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  ylim(0, 400) +
  xlab("") + ylab("Individual sapflow (kg/day)") + 
  theme_minimal() +
  theme(legend.position = "none")
sapflow_plot.plot
dev.off()


### Reduction in sapflow vs. plot ####

reduction_sapflow_kg_h_plot.mmod <- lmer(formula = sap_flux_reduction_kg_h ~ dbh_2023_cm + plot + (1|genus/ID), 
                                         data = daily_sapflow_met_2023)
summary(reduction_sapflow_kg_h_plot.mmod)

reduction_sapflow_perc_plot.mmod <- lmer(formula = sap_flux_reduction_h_perc ~ plot + (1|genus/ID), 
                                         data = daily_sapflow_met_2023)
summary(reduction_sapflow_perc_plot.mmod)

# Save the plot

# pdf("results/manuscript/all_dry_wet_sapflow_reduction_vs_plot.pdf", height = h, width = w)
# sapflow_plot.plot <- ggplot(wet_dry_daily_sapflow_met_2023.data,
#                             aes(x = fct_inorder(as.factor(month)),
#                                 y = sap_flux_reduction_kg_h,
#                                 color = plot)) +
#   geom_boxplot() +
#   scale_color_manual(values = c(color_control, color_tfe)) +
#   xlab("") + ylab("Sapflow reduction over maximum capacity (kg/h)") +
#   theme_minimal() +
#   theme(legend.position = "none")
# sapflow_plot.plot
# dev.off()

# pdf("results/all_dry_wet_sapflow_reduction_vs_plot_per_size_class.pdf", height = h, width = w)
# sapflow_plot.plot <- ggplot(wet_dry_daily_sapflow_met_2023.data,
#                             aes(x = fct_inorder(as.factor(month)),
#                                 y = sap_flux_reduction_kg_h,
#                                 color = plot,
#                                 fill = size_class)) +
#   geom_boxplot() +
#   scale_color_manual(values = c(color_control, color_tfe)) +
#   xlab("") + ylab("Sapflow reduction over maximum capacity (kg/h)") +
#   theme_minimal()
# sapflow_plot.plot
# dev.off()


### Differences month by month ###

daily_sapflow_met_2023$month_year <- ymd(paste0(year(daily_sapflow_met_2023$date), "-", month(daily_sapflow_met_2023$date), "-01"))

coefs.df <- data.frame()
for(m in as_date(sort(unique(daily_sapflow_met_2023$month_year)))){
  
  # Filter by month
  
  daily_sapflow_met_2023_month <- daily_sapflow_met_2023 %>%
    filter(month_year == m)
  
  sapflow_plot_month.mmod <- lmer(formula = sap_flux_reduction_kg_h ~ plot + (1|genus/ID),
                                  data = daily_sapflow_met_2023_month)
  
  coef_month.df <- as.data.frame(coef(summary(sapflow_plot_month.mmod)))[2, ]
  coef_month.df$month <- m
  
  coefs.df <- rbind(coefs.df, coef_month.df)
}
coefs.df$month <- as_date(coefs.df$month)
coefs.df

# Save the plot
# 
# pdf("results/sapflow_reduction_kg_h_vs_plot_2023_month_by_month.pdf", height = h*2, width = w*2)
# sapflow_plot_month_2023.plot <- ggplot(data = daily_sapflow_met_2023, 
#                                        aes(x = fct_inorder(as.factor(month_year)), 
#                                            y = sap_flux_reduction_kg_h,
#                                            color = plot)) + 
#   geom_boxplot() + 
#   scale_color_manual(values = c(color_control, color_tfe)) + 
#   xlab("") + ylab("Sapflow reduction over maximum capacity (kg/h)") + 
#   theme_minimal() +
#   theme(legend.position = "none")
# # ylim(-1, 6)
# sapflow_plot_month_2023.plot
# dev.off()
# 
# pdf("results/predicted_plot_daily_sapflow_differences.pdf", height = h, width = w)
# sapflow_plot.plot <- ggplot(wet_dry_predicted_sapflow.data, 
#                             aes(x = fct_inorder(as.factor(month)), 
#                                 y = predicted_sap_flux_l_m2_day, 
#                                 color = plot)) + 
#   geom_boxplot() + 
#   scale_color_manual(values = c(color_control, color_tfe)) + 
#   xlab("") + ylab("Predicted plot-level sap flow (mm/day)") + 
#   theme_minimal() +
#   theme(legend.position = "none")
# sapflow_plot.plot
# dev.off()


### Maximum hourly sapflow vs. plot ####

max_sapflow_kg_h_plot.mmod <- lmer(formula = log(max_daily_sap_flux_Kg_h) ~ dbh_2023_cm + plot + (1|genus/ID), 
                                   data = daily_sapflow_met_2023)
summary(max_sapflow_kg_h_plot.mmod)
r.squaredGLMM(max_sapflow_kg_h_plot.mmod)

## Change through time

max_sapflow_kg_h_plot_time.mmod <- lmer(formula = log(max_daily_sap_flux_Kg_h) ~ plot * yday + (1|genus/ID), 
                                        data = daily_sapflow_met_2023)
summary(max_sapflow_kg_h_plot_time.mmod)

# 
# pdf("results/manuscript/all_dry_wet_maximum_sapflow_vs_plot.pdf", height = h, width = w)
# sapflow_plot.plot <- ggplot(wet_dry_daily_sapflow_met_2023.data, 
#                             aes(x = fct_inorder(as.factor(month)), 
#                                 y = max_daily_sap_flux_Kg_h, 
#                                 color = plot)) + 
#   geom_boxplot() + 
#   scale_color_manual(values = c(color_control, color_tfe)) + 
#   xlab("") + ylab("Maximum hourly sap flow per day (kg/h)") + 
#   theme_minimal() +
#   theme(legend.position = "none")
# sapflow_plot.plot
# dev.off()

### Month by month ###

daily_sapflow_met_2023$month_year <- ymd(paste0(year(daily_sapflow_met_2023$date), "-", month(daily_sapflow_met_2023$date), "-01"))

coefs.df <- data.frame()
for(m in as_date(sort(unique(daily_sapflow_met_2023$month_year)))){
  
  # Filter by month
  
  daily_sapflow_met_2023_month <- daily_sapflow_met_2023 %>%
    filter(month_year == m)
  
  sapflow_plot_month.mmod <- lmer(formula = log(max_daily_sap_flux_Kg_h) ~ plot + (1|genus/ID),
                                  data = daily_sapflow_met_2023_month)
  
  coef_month.df <- as.data.frame(coef(summary(sapflow_plot_month.mmod)))[2, ]
  coef_month.df$month <- m
  
  coefs.df <- rbind(coefs.df, coef_month.df)
}
coefs.df$month <- as_date(coefs.df$month)
coefs.df

# Save the plot

pdf("results/maximum_sapflow_vs_plot_2023_month_by_month.pdf", height = h*2, width = w*2)
sapflow_plot_month_2023.plot <- ggplot(data = daily_sapflow_met_2023, 
                                       aes(x = fct_inorder(as.factor(month_year)), 
                                           y = max_daily_sap_flux_Kg_h,
                                           color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Maximum sap flow (kg/h)") + 
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 25)
sapflow_plot_month_2023.plot
dev.off()


#### DAILY PATTERNS (not included for now) ------------------------------------------------------------ ####
# 
# month_time_plot_sapflow_met_2023 <- sapflow_met_2023 %>%
#   mutate(time = as_datetime(hour(sapflow_met_2023$timestamp)),
#          hour = hour(sapflow_met_2023$timestamp),
#          month = month(timestamp),
#          # time = paste0(hour(sapflow_met_2023$timestamp), ":", minute(sapflow_met_2023$timestamp), ":", second(sapflow_met_2023$timestamp)),
#          plot_month_time = paste0(plot, "_", month(timestamp), "_", time)) %>%
#   dplyr::select(plot_month_time, ID, month, time, hour, plot, bl_sap_flux_Kg_h, cleaned_bl_sap_flux_Kg_h, sap_flux_kg_h_cm)
# 
# head(month_time_plot_sapflow_met_2023)
# 
# plot.list <- list()
# coefs.df <- data.frame()
# for(m in sort(unique(month_time_plot_sapflow_met_2023$month))){
#   
#   month_sapflow_met_2023 <- month_time_plot_sapflow_met_2023 %>%
#     filter(month == m)
#   
#   subdaily_sapflow.mmod <- lmer(formula = sap_flux_kg_h_cm ~ plot + (1|ID),
#                                   data = month_sapflow_met_2023)
#   
#   coef_month.df <- as.data.frame(coef(summary(subdaily_sapflow.mmod)))[2, ]
#   coef_month.df$month <- m
#   
#   coefs.df <- rbind(coefs.df, coef_month.df)
#   
#   
#   
#   plot.list[[m]] <- ggplot(month_sapflow_met_2023, aes(x = time, 
#                                                        y = sap_flux_kg_h_cm , 
#                                                        color = plot, 
#                                                        group = plot)) +
#     geom_violin() +
#     geom_smooth(method = "gam") + 
#     ylim(0, 0.08) +
#     scale_color_manual(values = c(color_control, color_tfe)) + 
#     xlab(paste0("Hour of the day month ", m)) + ylab("Mean individual sap flow (kg/h cm)") +
#     theme_minimal() + 
#     theme(legend.title = element_blank(), legend.position = "bottom")
#   plot(plot.list[[m]])
# }
# 
# coefs.df
# 
# pdf("results/manuscript/mean_hourly_sapflow_cm_patterns.pdf", height = h*2, width = w*2)
# ggarrange(plot.list[[5]], plot.list[[6]], plot.list[[7]], plot.list[[8]], plot.list[[9]], plot.list[[10]], plot.list[[11]], plot.list[[12]],
#           nrow = 2, ncol = 4, common.legend = T, legend = "bottom")
# dev.off()
# 
# 
# pdf("results/manuscript/mean_hourly_sapflow_cm_may.pdf", height = h, width = w)
# plot.list[[5]] + theme(legend.position = "none")
# dev.off()
# 
# pdf("results/manuscript/mean_hourly_sapflow_cm_october.pdf", height = h, width = w)
# plot.list[[10]] + theme(legend.position = "none")
# dev.off()
# 

