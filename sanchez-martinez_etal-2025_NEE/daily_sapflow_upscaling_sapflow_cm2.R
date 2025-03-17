#### RELATIONSHIP UPSCALING SAP FLOW DAILY DATA ################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")


#### DATA ---------------------------------------------------------------------- ####

# from sapflow script

sapflow_2023 <- read_csv("data_processed/daily_cleaned_processed_sapflow_2022_11_17-2023_12_14.csv") %>%
  rename(precip_mm.x = precip_mm_lba, precip_mm.y = precip_mm_infra) %>%
  select(date, plot, ID, species, genus, dbh_2023_cm, size_class, contains("sap_flux")) %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2024-05-01")

# sapflow_met_2023 <- combineData(data = sapflow_met_2023, variablesToCombine = "precip_mm")

met_2023 <- read_csv(paste0(data.path, "met/2022-09-18-2023-12-16_met_control_processed.csv")) %>%
  mutate(date = as_date(timestamp)) %>%
  filter(date %in% as_date(unique(sapflow_2023$date))) %>%
  rename(precip_mm.x = precip_mm_lba, precip_mm.y = precip_mm_infra) %>%
  select(date, everything(), -timestamp, -year) %>%
  filter(date >= "2022-12-01") %>%
  filter(date < "2023-12-01")

## Unify precipitation

met_2023 <- combineData(data = met_2023, variablesToCombine = "precip_mm")

## Annual precipitation of 2175.51 mm

met_2023 %>%
  pull(precip_mm) %>%
  sum(na.rm = T)

## add soil wc

# hourly gap filled soil wc
control_soilwc.df <- read.csv(paste0(data.path, "soil_moisture/pablo/control_hourly_soil_water_content_2019-03-08_2023-12-16.csv"), header = T) %>%
  mutate(plot = "Control",
         date = as_date(date))

tfe_soilwc.df <- read.csv(paste0(data.path, "soil_moisture/pablo/tfe_hourly_soil_water_content_2022-06-17_2023-12-16.csv"), header = T) %>%
  mutate(plot = "TFE",
         date = as_date(date))

soilwc.df <- bind_rows(control_soilwc.df, tfe_soilwc.df) %>%
  filter(date %in% as_date(unique(sapflow_2023$date))) %>%
  mutate(date_plot = paste0(date, "/", plot)) %>%
  select(date, plot, date_plot, contains("vwc")) %>%
  filter(date >= "2022-12-01") %>%
  filter(date < "2023-12-01")

### Aggregate daily soil data

daily_soilwc.df <- aggregate(soilwc.df[, c(-1, -2, -3)], by = list(soilwc.df$date_plot), FUN = mean, na.rm = T) %>%
  rename(date_plot = Group.1) %>%
  mutate(date = as_date(str_split_fixed(date_plot, "/", n = 2)[, 1]),
         plot = str_split_fixed(date_plot, "/", n = 2)[, 2]) %>%
  select(date, plot, everything())

### Aggregate daily met data

mean.variables <- c("t2m_C", "rh2m_perc", "tdc2m_C", "twc2m_C", "vp2m_mbar", "t16m_C", "rh16m_perc", "tdc16m_C",
                    "twc16m_C", "vp16m_mbar", "t28m_C", "rh28m_perc", "tdc28m_C", "twc28m_C", "vp28m_mbar", 
                    "t42m_C", "rh42m_perc", "tdc42m_C", "twc_42_C", "vp42m_bar", "rad_global_W_m2", 
                    "rad_glob_tot_MJ_m2", "rad_net_W_m2", "t_canopy_C", "t_canopy_2_C", "wind_speed_m_s",
                    "wind_speed_max_m_s", "wind_direction",
                    "vpd2m_kPa", "vpd16m_kPa", "vpd28m_kPa", "vpd42m_kPa")

sum.variables <- c("precip_mm")

if(!file.exists(paste0(data.path, "met/2022-09-18-2023-12-16_daily_met_control_processed.csv"))){
  
  mean_daily_met_2023 <- aggregate(met_2023[, mean.variables], 
                                   by = list(met_2023$date), 
                                   FUN = meanOrMode)
  
  sum_daily_met_2023 <- aggregate(met_2023[, sum.variables], 
                                  by = list(met_2023$date), 
                                  FUN = sum, na.rm = T)
  
  daily_met_2023 <- merge(mean_daily_met_2023, sum_daily_met_2023, by = "Group.1") %>%
    rename(date = Group.1, precip_mm = x) 
  
  write_csv(daily_met_2023, paste0(data.path, "met/2022-09-18-2023-12-16_daily_met_control_processed.csv")) 
  
} else{
  daily_met_2023 <- read_csv(paste0(data.path, "met/2022-09-18-2023-12-16_daily_met_control_processed.csv")) 
}

## merge soil and met daily data (soil is plot specific, met is not)

daily_met_soil_2023 <- merge(daily_soilwc.df, daily_met_2023, by = "date", all.x = T)

## add day of the year

daily_met_soil_2023$yday <- yday(daily_met_soil_2023$date)

# mean dbh per individual for 2022-2023

dbh_2023 <- read_csv(paste0(root.dir, "data_processed/biomass/dbh_2005_2023.csv")) %>%
  filter(date > "2022-05-01") %>% 
  mutate(ID = paste0(plot, "_", tree_number),
         genus = str_split_fixed(species, pattern = " ", n = 2)[, 1]) %>%
  select(ID, genus, plot, dbh_cm, clean_dbh_cm , gf_clean_dbh_cm , gf2_clean_dbh_cm)

mean_dbh_2022_2023 <- aggregate(dbh_2023[, -1], by = list(dbh_2023$ID), FUN = meanOrMode) %>%
  rename(ID = Group.1)

# add dbh to sapflow data

sapflow_dbh_2023 <- merge(sapflow_2023, mean_dbh_2022_2023[, c(-2, -3)], by = "ID", all.x = T) %>%
  arrange(ID, date) %>%
  mutate(date_plot = paste0(date, "/", plot))

# add soil and met data to sapflow data

sapflow_met_dbh_2023 <- merge(sapflow_dbh_2023, daily_met_soil_2023[, c(-1, -2)], by = "date_plot", all.x = T) %>%
  arrange(ID, date) %>%
  select(-date_plot)

## Divide TFE precipitation by 2
sapflow_met_dbh_2023[sapflow_met_dbh_2023$plot == "TFE", "precip_mm"] <- sapflow_met_dbh_2023[sapflow_met_dbh_2023$plot == "TFE", "precip_mm"]/2

## size class

sapflow_met_dbh_2023 <- sapflow_met_dbh_2023 %>%
  mutate(size_class = ifelse(clean_dbh_cm < 20, "small", "medium"))

sapflow_met_dbh_2023$size_class[sapflow_met_dbh_2023$clean_dbh_cm > 40] <- "large"


### prepare newdata to be predicted ####

daily_met_soil_2023$date_plot <- NULL

if(!file.exists("data_processed/daily_individual_data_to_predict_sapflow.csv")){
  
  date <- unique(sapflow_2023$date)
  newData <- data.frame()
  for(ind in unique(mean_dbh_2022_2023$ID)){
    
    plot_daily_met_soil_2023 <- daily_met_soil_2023 %>%
      filter(plot == unlist(str_split(ind, "_"))[1])
    
    newDataInd <- cbind(ID = ind, plot_daily_met_soil_2023)
    
    newData <- rbind(newData, newDataInd)
  }
  
  # add dbh and genus
  
  newData_dbh_genus <- merge(newData, mean_dbh_2022_2023[, -3], by = "ID", all.x = T) %>%
    mutate(stem_area_cm2 = pi*(gf2_clean_dbh_cm/2)^2) %>%
    select(ID, plot, date, genus, everything())
  
  # size class
  
  newData_dbh_genus <- newData_dbh_genus %>%
    mutate(size_class = ifelse(clean_dbh_cm < 20, "small", "medium"))
  
  newData_dbh_genus$size_class[newData_dbh_genus$clean_dbh_cm > 40] <- "large"
  
  write_csv(newData_dbh_genus, "data_processed/daily_individual_data_to_predict_sapflow.csv")
} else {
  newData_dbh_genus <- read_csv("data_processed/daily_individual_data_to_predict_sapflow.csv")
}

length(unique(newData_dbh_genus$ID)) # 1330 individuals to predict

## Divide TFE precipitation by 2
newData_dbh_genus[newData_dbh_genus$plot == "TFE", "precip_mm"] <- newData_dbh_genus[newData_dbh_genus$plot == "TFE", "precip_mm"]/2


#### SELECT PREDICTION MODEL --------------------------------------------------- ####

{
  # Only including plot and genus
  
  sapflow_hourly_plot_genus.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + (1|genus/ID), 
                                         data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_genus.mmod)
  r.squaredGLMM(sapflow_hourly_plot_genus.mmod) # r2m = 0.19, r2c = 0.62
  
  # Adding vpd
  
  sapflow_hourly_plot_genus_vpd.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + vpd42m_kPa + (1|genus/ID), 
                                             data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_genus_vpd.mmod)
  r.squaredGLMM(sapflow_hourly_plot_genus_vpd.mmod) # r2m = 0.03, r2c = 0.63
  
  # adding dbh
  
  sapflow_hourly_plot_genus_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + gf2_clean_dbh_cm + (1|ID), 
                                             data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_genus_dbh.mmod)
  r.squaredGLMM(sapflow_hourly_plot_genus_dbh.mmod) # r2m = 0.08, r2c = 0.60
  
  # adding prec
  
  sapflow_hourly_plot_genus_prec.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + precip_mm + (1|genus/ID), 
                                              data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_genus_prec.mmod)
  r.squaredGLMM(sapflow_hourly_plot_genus_prec.mmod) # r2m = 0.01, r2c = 0.62
  
  
  # adding prec, vpd and dbh and timestamp
  
  # cor(sapflow_met_dbh_2023$vpd42m_kPa, sapflow_met_dbh_2023$precip_mm_infra, use = "pairwise.complete.obs")
  
  sapflow_hourly_plot_genus_prec_vpd_dbh.mmod <- lmer(formula = bl_sap_flux_Kg_h ~ plot + precip_mm + vpd42m_kPa + date + (1|genus/ID), 
                                                      data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_genus_prec_vpd_dbh.mmod)
  r.squaredGLMM(sapflow_hourly_plot_genus_prec_vpd_dbh.mmod) # r2m = 0.05, r2c = 0.68
  
  # not using genus nor timestamp
  
  sapflow_hourly_plot_prec_vpd_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + precip_mm + vpd42m_kPa + gf2_clean_dbh_cm + (1|ID), 
                                                data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_prec_vpd_dbh.mmod)
  r.squaredGLMM(sapflow_hourly_plot_prec_vpd_dbh.mmod) # r2m = 0.13, r2c = 0.62
}

# soil water content (best model)

# cor(sapflow_met_dbh_2023[, c("daily_total_cleaned_bl_sap_flux_Kg_day", "vwc_100cm_m3_m3", "vwc_250cm_m3_m3", "vwc_400cm_m3_m3", "vwc_50cm_m3_m3")], use = "pairwise.complete.obs")

summary(sapflow_met_dbh_2023$daily_total_cleaned_bl_sap_flux_Kg_day)

sapflow_daily_plot_vwc_vpd_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day_cm2 ~ plot + vwc_250cm_m3_m3 + vpd16m_kPa + gf2_clean_dbh_cm  + (1|genus/ID), 
                                            data = sapflow_met_dbh_2023)
summary(sapflow_daily_plot_vwc_vpd_dbh.mmod)
r.squaredGLMM(sapflow_daily_plot_vwc_vpd_dbh.mmod) # r2m = 0.10, r2c = 0.68

# sapflow_daily_plot_vwc_vpd_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day_cm2 ~ plot + vwc_250cm_m3_m3 + vpd16m_kPa + gf2_clean_dbh_cm + (1|genus/ID), 
#                                              data = sapflow_met_dbh_2023)
# summary(sapflow_daily_plot_vwc_vpd_dbh.mmod)
# r.squaredGLMM(sapflow_daily_plot_vwc_vpd_dbh.mmod) # r2m = 0.03, r2c = 0.76


#### PREDICT INDIVIDUAL DATA --------------------------------------------------- ####
# 
# newData_dbh_genus <- newData_dbh_genus
#   filter(date >= "2023-05-01") %>%
#   filter(date < "2024-05-01")

predicted_sapflow.data <- newData_dbh_genus %>%
  select(date, ID, plot, genus, stem_area_cm2, vwc_250cm_m3_m3, vpd16m_kPa, gf2_clean_dbh_cm, precip_mm)

predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_cm2_day.x <- predict(sapflow_daily_plot_vwc_vpd_dbh.mmod, newData_dbh_genus, 
                                                                          allow.new.levels = T)

predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_cm2_day.x[predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_cm2_day.x < 0] <- NA


### Imputation using monthly mean per individual ####

predicted_sapflow.data$id_month_year <- paste0(predicted_sapflow.data$ID, "/", month(predicted_sapflow.data$date), "/", year(predicted_sapflow.data$date))

monthly_avg_id <- predicted_sapflow.data %>% group_by(id_month_year) %>% 
  summarise(predicted_daily_total_sap_flux_Kg_cm2_day.y = mean(predicted_daily_total_sap_flux_Kg_cm2_day.x, na.rm = T))

predicted_sapflow.data <- merge(predicted_sapflow.data, monthly_avg_id, by = "id_month_year", all.x = T)

summary(predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_cm2_day.x)

imp_predicted_sapflow.data <- combineData(predicted_sapflow.data, variablesToCombine = "predicted_daily_total_sap_flux_Kg_cm2_day")

imp_predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_day <- imp_predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_cm2_day * imp_predicted_sapflow.data$stem_area_cm2

summary(imp_predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_day)

imp_predicted_sapflow.data <- imp_predicted_sapflow.data %>%
  filter(date >= "2022-12-01") %>%
  filter(date < "2023-12-01") %>%
  select(-id_month_year) %>%
  arrange(ID, date)

write_csv(imp_predicted_sapflow.data, "data_processed/predicted_daily_individual_data_sapflow.csv")

# it seems to be more trees in TFE than control, check!

predicted_sapflow.data %>% filter(plot == "Control") %>% pull(ID) %>% unique() %>% length()
predicted_sapflow.data %>% filter(plot == "TFE") %>% pull(ID) %>% unique() %>% length()


#### INDIVIDUAL DAILY DATA PLOTS ----------------------------------------------- ####

predicted_sapflow.data <- read_csv("data_processed/predicted_daily_individual_data_sapflow.csv")

### Plot examples ####

plotExamples.data <- predicted_sapflow.data %>%
  filter(ID %in% c("Control_108", "TFE_120"))

ggplot(plotExamples.data, aes(x = date, y = predicted_daily_total_sap_flux_Kg_day, group = ID, color = plot)) +
  geom_line()

ggplot(plotExamples.data, aes(x = date, y = vwc_250cm_m3_m3, group = ID, color = plot)) +
  geom_line()

ggplot(plotExamples.data, aes(x = date, y = vpd16m_kPa, group = ID, color = plot)) +
  geom_line()

ggplot(plotExamples.data, aes(x = date, y = precip_mm, group = ID, color = plot)) +
  geom_line()


#### SCALING AT PLOT LEVEL ----------------------------------------------------- ####

predicted_sapflow.data <-read_csv("data_processed/predicted_daily_individual_data_sapflow.csv") %>%
  select(ID, plot, date, predicted_daily_total_sap_flux_Kg_day, precip_mm)

### Aggregate plot daily data ####

predicted_sapflow.data$plot_date <- paste0(predicted_sapflow.data$plot, "/", predicted_sapflow.data$date)
# head(toDaily_plot_predicted_sapfow)
toDaily_plot_predicted_sapfow <- aggregate(as_tibble(predicted_sapflow.data[, c("predicted_daily_total_sap_flux_Kg_day", "precip_mm")]), 
                                           by = list(predicted_sapflow.data$plot_date), 
                                           FUN = sum, na.rm = T)

daily_plot_predicted_sapfow <- toDaily_plot_predicted_sapfow %>%
  mutate(plot = str_split_fixed(Group.1, pattern = "/", n = 2)[, 1],
         date = as_date(str_split_fixed(Group.1, pattern = "/", n = 2)[, 2]),
         year_month = paste0(year(date), "-", month(date)),
         plot_area_m2 = ifelse(plot == "Control", control_m2, tfe_m2),
         predicted_sap_flux_Kg_day = predicted_daily_total_sap_flux_Kg_day,
         predicted_sap_flux_l_m2_day = predicted_sap_flux_Kg_day/plot_area_m2) %>%
  select(plot, date, year_month, plot_area_m2, predicted_sap_flux_Kg_day, predicted_sap_flux_l_m2_day, precip_mm)

write_csv(daily_plot_predicted_sapfow, "data_processed/predicted_daily_total_plot_data_sapflow.csv")

## Plot examples

ggplot(daily_plot_predicted_sapfow, aes(x = date, y = predicted_sap_flux_Kg_day, group = plot, color = plot)) +
  geom_line()

ggplot(daily_plot_predicted_sapfow, aes(x = date, y = predicted_sap_flux_l_m2_day, group = plot, color = plot))  + 
  geom_line()

ggplot(daily_plot_predicted_sapfow, aes(x = date, y = precip_mm, group = plot, color = plot)) +
  geom_line()


### Aggregate plot monthly data ####

daily_plot_predicted_sapfow$plot_year_month <- paste0(daily_plot_predicted_sapfow$plot, "/", daily_plot_predicted_sapfow$year_month)

toMonthly_plot_predicted_sapfow <- aggregate(as_tibble(daily_plot_predicted_sapfow[, c("predicted_sap_flux_Kg_day", "predicted_sap_flux_l_m2_day", "precip_mm")]), 
                                             by = list(daily_plot_predicted_sapfow$plot_year_month), 
                                             FUN = sum, na.rm = T)

monthly_plot_predicted_sapfow <- toMonthly_plot_predicted_sapfow %>%
  mutate(plot = str_split_fixed(Group.1, pattern = "/", n = 2)[, 1],
         date = str_split_fixed(Group.1, pattern = "/", n = 2)[, 2],
         date = as_date(paste0(date, "-01"))) %>%
  select(plot, date, predicted_sap_flux_Kg_month = predicted_sap_flux_Kg_day, predicted_sap_flux_l_m2_month = predicted_sap_flux_l_m2_day, precip_mm)

write_csv(monthly_plot_predicted_sapfow, "data_processed/predicted_monthly_total_plot_data_sapflow.csv")


### PLOTTING #####

ggplot(monthly_plot_predicted_sapfow, aes(x = date, y = predicted_sap_flux_l_m2_month, group = plot, color = plot)) +
  geom_point() + geom_line()

all_year_predicted_sapflow.data <- daily_plot_predicted_sapfow %>%
  mutate(month = "Whole year")

wet_predicted_sapflow.data <- daily_plot_predicted_sapfow %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2023-06-01") %>%
  mutate(month = "05-2023")

dry_predicted_sapflow.data <- daily_plot_predicted_sapfow %>%
  filter(date >= "2023-11-01") %>%
  filter(date < "2023-12-01") %>%
  mutate(month = "11-2023")


lm1 <- lm(predicted_sap_flux_l_m2_day ~ plot, data = all_year_predicted_sapflow.data)
summary(lm1)

lm2 <- lm(predicted_sap_flux_l_m2_day ~ plot, data = wet_predicted_sapflow.data)
summary(lm2)

lm3 <- lm(predicted_sap_flux_l_m2_day ~ plot, data = dry_predicted_sapflow.data)
summary(lm3)

wet_dry_predicted_sapflow.data <- bind_rows(all_year_predicted_sapflow.data, wet_predicted_sapflow.data, dry_predicted_sapflow.data)

pdf("results/manuscript/predicted_plot_daily_sapflow_differences.pdf", height = h, width = w)
sapflow_plot.plot <- ggplot(wet_dry_predicted_sapflow.data, 
                            aes(x = fct_inorder(as.factor(month)), 
                                y = predicted_sap_flux_l_m2_day, 
                                color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Predicted plot-level sap flow (mm/day)") + 
  theme_minimal() +
  theme(legend.position = "none")
sapflow_plot.plot
dev.off()

## Total yearly precipitation = 2175.51 mm

met_2023 %>% 
  pull(precip_mm) %>%
  sum(na.rm = T)

## Total yearly sapflow control = 2664.188 mm

all_year_predicted_sapflow.data %>%
  filter(plot == "Control") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  sum(na.rm = T)

## Total yearly sapflow TFE = 2088.401 mm

all_year_predicted_sapflow.data %>%
  filter(plot == "TFE") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  sum(na.rm = T)

## Total may precipitation = 147.58 mm

met_2023 %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2023-06-01") %>%
  pull(precip_mm) %>%
  sum(na.rm = T)

## Total may sapflow control = 224.850 mm

wet_predicted_sapflow.data %>%
  filter(plot == "Control") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  sum(na.rm = T)

## Total may sapflow TFE = 169.24 mm

wet_predicted_sapflow.data %>%
  filter(plot == "TFE") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  sum(na.rm = T)

## Total nov precipitation = 26.20 mm

met_2023 %>%
  filter(date >= "2023-11-01") %>%
  filter(date < "2023-12-01") %>%
  pull(precip_mm) %>%
  sum(na.rm = T)

## Total nov sapflow control = 213.164 mm

dry_predicted_sapflow.data %>%
  filter(plot == "Control") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  sum(na.rm = T)

## Total nov sapflow TFE = 228.68 mm

dry_predicted_sapflow.data %>%
  filter(plot == "TFE") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  sum(na.rm = T)

