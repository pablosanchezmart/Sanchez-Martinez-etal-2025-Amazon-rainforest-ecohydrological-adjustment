#### RELATIONSHIP UPSCALING SAP FLOW DAILY DATA ################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")
forcerun <- T
#### DATA ---------------------------------------------------------------------- ####

sapflow.variables <- c("daily_total_cleaned_bl_sap_flux_Kg_day", "daily_total_gf_cleaned_bl_sap_flux_Kg_day")

id_variables <- c("date", "ID", "species", "plot", "dbh_2023_cm", "size_class")

met_vars <- c("vpd16m_kPa", "gf_vpd16m_kPa", "vpd42m_kPa", "gf_vpd42m_kPa", "precip_mm")

soil_vars <- c("gf_vwc_250cm_m3_m3", "vwc_250cm_m3_m3")

# from sapflow script

sapflow_2023 <- read_csv("data_processed/daily_cleaned_processed_sapflow_2022-11-17-2024-06-01.csv") %>%
  select(all_of(c(sapflow.variables, id_variables, met_vars))) %>%
  # filter(date >= "2022-12-01") %>%
  # filter(date < "2023-12-01")
  filter(date >= "2023-05-01") %>%
  filter(date < "2024-01-01")
names(sapflow_2023)

mean_sapflow <- mean(sapflow_2023$daily_total_cleaned_bl_sap_flux_Kg_day, na.rm = T)
sd_sapflow <- sd(sapflow_2023$daily_total_cleaned_bl_sap_flux_Kg_day, na.rm = T)

upper_threshold <- mean_sapflow + (sd_sapflow * 3)
lower_threshold <- mean_sapflow - (sd_sapflow * 3)

sapflow_2023$daily_total_cleaned_bl_sap_flux_Kg_day[sapflow_2023$daily_total_cleaned_bl_sap_flux_Kg_day > upper_threshold] <- NA
sapflow_2023$daily_total_cleaned_bl_sap_flux_Kg_day[sapflow_2023$daily_total_cleaned_bl_sap_flux_Kg_day < lower_threshold] <- NA

### soil daily data

soil_a <- read_csv(paste0(root.dir, "data_processed/soil_moisture/pablo/wc_time_series/control_daily_soil_water_content_2000-01-01_2024-05-27.csv")) %>%
  mutate(plot = "Control")
soil_b <- read_csv(paste0(root.dir, "data_processed/soil_moisture/pablo/wc_time_series/tfe_daily_soil_water_content_2000-01-01_2024-05-27.csv")) %>%
  mutate(plot = "TFE")

soil <- bind_rows(soil_a, soil_b)

sapflow_2023 <- merge(sapflow_2023, soil, by = c("plot", "date"), all.x = T)

ID_genus_a <- readxl::read_excel(paste0(root.dir, "data_processed/caxuana_census/ID_taxonomy_2023.xlsx"), 
                                 sheet = "plot_a") %>%
  mutate(ID = paste0("Control_", tree_number),
         genus = str_split_fixed(species, " ", 2)[, 1],
         genus = ifelse(genus == "NA", NA, genus)) %>%
  select(ID, genus)

ID_genus_b <- readxl::read_excel(paste0(root.dir, "data_processed/caxuana_census/ID_taxonomy_2023.xlsx"), 
                                 sheet = "plot_a") %>%
  mutate(ID = paste0("TFE_", tree_number),
         genus = str_split_fixed(species, " ", 2)[, 1],
         genus = ifelse(genus == "NA", NA, genus)) %>%
  select(ID, genus)

ID_genus <- bind_rows(ID_genus_a, ID_genus_b)

wd_gbm.data_6 <- read_csv("data_processed/individual_annual_biomass_wc_2000_2023.csv") %>%
  filter(year == 2023) %>%
  select(ID, dbh_cm, gf_wd_g_cm3)


# add genus to dbh data

mean_dbh_2023 <- merge(wd_gbm.data_6, ID_genus, by = "ID", all.x = T)

# add dbh to sapflow data

sapflow_met_dbh_2023 <- merge(sapflow_2023, mean_dbh_2023, by = "ID", all.x = T) %>%
  arrange(ID, date) %>%
  mutate(date_plot = paste0(date, "/", plot))

names(sapflow_met_dbh_2023)

## Divide TFE precipitation by 2
sapflow_met_dbh_2023[sapflow_met_dbh_2023$plot == "TFE", "precip_mm"] <- sapflow_met_dbh_2023[sapflow_met_dbh_2023$plot == "TFE", "precip_mm"]/2

### prepare newdata to be predicted ####

daily_met_soil_2023 <- sapflow_met_dbh_2023 %>%
  select(date, plot, all_of(met_vars), all_of(soil_vars))

daily_met_soil_2023$date_plot <- NULL

if(!file.exists("data_processed/daily_individual_data_to_predict_sapflow.csv")| forcerun){
  
  date <- unique(sapflow_2023$date)
  newData <- data.frame()
  for(ind in unique(mean_dbh_2023$ID)){
    
    plot_daily_met_soil_2023 <- daily_met_soil_2023 %>%
      filter(plot == unlist(str_split(ind, "_"))[1])
    
    newDataInd <- cbind(ID = ind, plot_daily_met_soil_2023)
    
    newData <- rbind(newData, newDataInd)
  }
  
  # add dbh and genus
  
  newData_dbh_genus <- merge(newData, mean_dbh_2023, by = "ID", all.x = T) %>%
    select(ID, plot, date, genus, everything())
  
  # size class
  
  newData_dbh_genus <- newData_dbh_genus %>%
    mutate(size_class = ifelse(mean(dbh_cm, na.rm = T) > 30, "emergent", "non-emergent"))  # more than 30m height
  
  write_csv(newData_dbh_genus, "data_processed/daily_individual_data_to_predict_sapflow.csv")
} else {
  newData_dbh_genus <- read_csv("data_processed/daily_individual_data_to_predict_sapflow.csv")
}

length(unique(newData_dbh_genus$ID)) # 1366 individuals to predict

## Divide TFE precipitation by 2
newData_dbh_genus[newData_dbh_genus$plot == "TFE", "precip_mm"] <- newData_dbh_genus[newData_dbh_genus$plot == "TFE", "precip_mm"]/2

summary(newData_dbh_genus)



#### DIRECT SCALING ------------------------------------------------------------ ####

sapflow_daily_plot_vwc_vpd_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ dbh_cm + (1|ID), 
                                            data = sapflow_met_dbh_2023)
summary(sapflow_daily_plot_vwc_vpd_dbh.mmod)
r.squaredGLMM(sapflow_daily_plot_vwc_vpd_dbh.mmod) # r2m = 0.15, r2c = 0.71



#### SELECT PREDICTION MODEL --------------------------------------------------- ####

{
  # Only including plot and genus
  
  sapflow_hourly_plot_genus.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + (1|genus/ID), 
                                         data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_genus.mmod)
  r.squaredGLMM(sapflow_hourly_plot_genus.mmod) # r2m = 0.19, r2c = 0.62
  
  # Adding vpd
  
  sapflow_hourly_plot_genus_vpd.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + gf_vpd42m_kPa + (1|genus/ID), 
                                             data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_genus_vpd.mmod)
  r.squaredGLMM(sapflow_hourly_plot_genus_vpd.mmod) # r2m = 0.03, r2c = 0.63
  
  # adding dbh
  
  sapflow_hourly_plot_genus_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + dbh_cm + (1|ID), 
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
  
  sapflow_hourly_plot_genus_prec_vpd_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + precip_mm + gf_vpd42m_kPa + date + (1|genus/ID), 
                                                      data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_genus_prec_vpd_dbh.mmod)
  r.squaredGLMM(sapflow_hourly_plot_genus_prec_vpd_dbh.mmod) # r2m = 0.05, r2c = 0.68
  
  # not using genus nor timestamp
  
  sapflow_hourly_plot_prec_vpd_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + precip_mm + gf_vpd42m_kPa + dbh_cm + (1|ID), 
                                                data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_prec_vpd_dbh.mmod)
  r.squaredGLMM(sapflow_hourly_plot_prec_vpd_dbh.mmod) # r2m = 0.13, r2c = 0.62
}

# soil water content (best model)

# cor(sapflow_met_dbh_2023[, c("rew_250cm_m3_m3", "vwc_100cm_m3_m3", "vwc_250cm_m3_m3", "vwc_400cm_m3_m3", "vwc_50cm_m3_m3")], use = "pairwise.complete.obs")
# head(sapflow_met_dbh_2023)
# summary(sapflow_met_dbh_2023$daily_total_cleaned_bl_sap_flux_Kg_day)


sapflow_met_dbh_2023$yday <- yday(sapflow_met_dbh_2023$date)


sapflow_daily_plot_vwc_vpd_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + gf_vpd16m_kPa + gf_vwc_250cm_m3_m3 + dbh_cm + (1|ID), 
                                            data = sapflow_met_dbh_2023)
summary(sapflow_daily_plot_vwc_vpd_dbh.mmod)
r.squaredGLMM(sapflow_daily_plot_vwc_vpd_dbh.mmod) # r2m = 0.15, r2c = 0.71


#### PREDICT INDIVIDUAL DATA --------------------------------------------------- ####

predicted_sapflow.data <- newData_dbh_genus %>%
  select(date, ID, plot, genus, gf_vpd16m_kPa, gf_vwc_250cm_m3_m3, dbh_cm, precip_mm, size_class) %>%
  filter(!is.na(dbh_cm))

predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_day.x <- predict(sapflow_daily_plot_vwc_vpd_dbh.mmod, predicted_sapflow.data, 
                                                                          allow.new.levels = T)

predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_day.x[predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_day.x < 0] <- NA


### Imputation using monthly mean per individual ####

predicted_sapflow.data$id_month_year <- paste0(predicted_sapflow.data$ID, "/", month(predicted_sapflow.data$date), "/", year(predicted_sapflow.data$date))

monthly_avg_id <- predicted_sapflow.data %>% group_by(id_month_year) %>% 
  summarise(predicted_daily_total_sap_flux_Kg_day.y = mean(predicted_daily_total_sap_flux_Kg_day.x, na.rm = T))

predicted_sapflow.data <- merge(predicted_sapflow.data, monthly_avg_id, by = "id_month_year", all.x = T)

summary(predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_day.x)

imp_predicted_sapflow.data <- combineData(predicted_sapflow.data, variablesToCombine = "predicted_daily_total_sap_flux_Kg_day")

summary(imp_predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_day)

imp_predicted_sapflow.data <- imp_predicted_sapflow.data %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2024-05-01") %>%
  select(-id_month_year) %>%
  arrange(ID, date)

write_csv(imp_predicted_sapflow.data, "data_processed/predicted_daily_individual_data_sapflow.csv")


predicted_sapflow.data %>% filter(plot == "Control") %>% pull(ID) %>% unique() %>% length()
predicted_sapflow.data %>% filter(plot == "TFE") %>% pull(ID) %>% unique() %>% length()


#### INDIVIDUAL DAILY DATA PLOTS ----------------------------------------------- ####

predicted_sapflow.data <- read_csv("data_processed/predicted_daily_individual_data_sapflow.csv")

### Plot examples ####

plotExamples.data <- predicted_sapflow.data %>%
  filter(ID %in% c("Control_1.2", "TFE_99.2"))

# ggplot(plotExamples.data, aes(x = date, y = predicted_daily_total_sap_flux_Kg_day, group = ID, color = plot)) +
  # geom_line()

ggplot(plotExamples.data, aes(x = date, y = gf_vwc_250cm_m3_m3, group = ID, color = plot)) +
  geom_line()

ggplot(plotExamples.data, aes(x = date, y = gf_vpd16m_kPa, group = ID, color = plot)) +
  geom_line()

ggplot(plotExamples.data, aes(x = date, y = precip_mm, group = ID, color = plot)) +
  geom_line()

ggplot(predicted_sapflow.data, aes(x = plot, y = predicted_daily_total_sap_flux_Kg_day, color = plot, fill = size_class)) +
  geom_boxplot()


#### SCALING AT PLOT LEVEL ----------------------------------------------------- ####

predicted_sapflow.data <-read_csv("data_processed/predicted_daily_individual_data_sapflow.csv") %>%
  select(ID, plot, date,everything())

### Aggregate plot daily data ####

predicted_sapflow.data$plot_date <- paste0(predicted_sapflow.data$plot, "/", predicted_sapflow.data$date)

toDaily_plot_predicted_sapfow <- aggregate(as_tibble(predicted_sapflow.data[, c("predicted_daily_total_sap_flux_Kg_day", "precip_mm")]), 
                                           by = list(predicted_sapflow.data$plot_date), 
                                           FUN = sum, na.rm = T)

toDaily_abiotic <-  aggregate(as_tibble(predicted_sapflow.data[, c("gf_vpd16m_kPa", "gf_vwc_250cm_m3_m3")]), 
                            by = list(predicted_sapflow.data$plot_date), 
                            FUN = mean, na.rm = T)

toDaily_plot_predicted_sapfow <- merge(toDaily_plot_predicted_sapfow, toDaily_abiotic, by = "Group.1", all.x = T)

daily_plot_predicted_sapfow <- toDaily_plot_predicted_sapfow %>%
  mutate(plot = str_split_fixed(Group.1, pattern = "/", n = 2)[, 1],
         date = as_date(str_split_fixed(Group.1, pattern = "/", n = 2)[, 2]),
         year_month = paste0(year(date), "-", month(date)),
         plot_area_m2 = ifelse(plot == "Control", control_m2, tfe_m2),
         predicted_sap_flux_Kg_day = predicted_daily_total_sap_flux_Kg_day,
         predicted_sap_flux_l_m2_day = predicted_sap_flux_Kg_day/plot_area_m2) %>%
  select(plot, date, year_month, plot_area_m2, predicted_sap_flux_Kg_day, predicted_sap_flux_l_m2_day, 
         everything())

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

monthly_plot_predicted_sapfow <- read_csv("data_processed/predicted_monthly_total_plot_data_sapflow.csv")
daily_plot_predicted_sapfow <- read_csv("data_processed/predicted_daily_total_plot_data_sapflow.csv")

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

wet_dry_predicted_sapflow.data <- bind_rows(all_year_predicted_sapflow.data, wet_predicted_sapflow.data, dry_predicted_sapflow.data)


lm1 <- lm(predicted_sap_flux_l_m2_day ~ plot, data = all_year_predicted_sapflow.data)
summary(lm1)

lm2 <- lm(predicted_sap_flux_l_m2_day ~ plot, data = wet_predicted_sapflow.data)
summary(lm2)

lm3 <- lm(predicted_sap_flux_l_m2_day ~ plot, data = dry_predicted_sapflow.data)
summary(lm3)


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


## Total yearly precipitation = 2085 mm
tail(daily_met_soil_2023)
daily_met_soil_2023 %>% 
  filter(plot == "Control") %>%
  pull(precip_mm) %>%
  sum(na.rm = T)

## mean yearly sapflow control = 4.51 mm

all_year_predicted_sapflow.data %>%
  filter(plot == "Control") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  mean(na.rm = T)

## mean yearly sapflow TFE = 1015 mm

all_year_predicted_sapflow.data %>%
  filter(plot == "TFE") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  mean(na.rm = T)



## Total yearly sapflow control = 1541 mm

all_year_predicted_sapflow.data %>%
  filter(plot == "Control") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  sum(na.rm = T)

## Total yearly sapflow TFE = 1015 mm

all_year_predicted_sapflow.data %>%
  filter(plot == "TFE") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  sum(na.rm = T)

## Total may precipitation = 288.16 mm

daily_met_soil_2023 %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2023-06-01") %>%
  pull(precip_mm) %>%
  sum(na.rm = T)

## mean may vpd = 288.16 mm

daily_met_soil_2023 %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2023-06-01") %>%
  pull(gf_vpd16m_kPa) %>%
  mean(na.rm = T)

## Mean may temp = 28.51

daily_met_soil_2023 %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2023-06-01") %>%
  pull(t16m_C) %>%
  mean(na.rm = T)

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

## Total nov precipitation = 52.40 mm

daily_met_soil_2023 %>%
  filter(date >= "2023-11-01") %>%
  filter(date < "2023-12-01") %>%
  pull(precip_mm) %>%
  sum(na.rm = T)

## Mean nov vpd = 0.87

daily_met_soil_2023%>%
  filter(date >= "2023-11-01") %>%
  filter(date < "2023-12-01") %>%
  pull(gf_vpd16m_kPa) %>%
  mean(na.rm = T)

## Mean nov temp = 28.51

daily_met_soil_2023 %>%
  filter(date >= "2023-11-01") %>%
  filter(date < "2023-12-01") %>%
  pull(t16m_C) %>%
  mean(na.rm = T)


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

