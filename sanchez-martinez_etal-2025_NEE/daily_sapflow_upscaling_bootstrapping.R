#### RELATIONSHIP UPSCALING SAP FLOW DAILY DATA ################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")
# forcerun <- T
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
  filter(date < "2024-05-01")
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
  
  sapflow_hourly_plot_genus_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ dbh_cm + (1|ID), 
                                             data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_genus_dbh.mmod)
  r.squaredGLMM(sapflow_hourly_plot_genus_dbh.mmod) # r2m = 0.08, r2c = 0.60
  
  # adding prec
  
  sapflow_hourly_plot_genus_prec.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ precip_mm + (1|genus/ID), 
                                              data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_genus_prec.mmod)
  r.squaredGLMM(sapflow_hourly_plot_genus_prec.mmod) # r2m = 0.01, r2c = 0.62
  
  
  # adding prec, vpd and dbh and timestamp
  
  # cor(sapflow_met_dbh_2023$vpd42m_kPa, sapflow_met_dbh_2023$precip_mm_infra, use = "pairwise.complete.obs")
  
  sapflow_hourly_plot_genus_prec_vpd_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + precip_mm + vpd42m_kPa + date + (1|genus/ID), 
                                                      data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_genus_prec_vpd_dbh.mmod)
  r.squaredGLMM(sapflow_hourly_plot_genus_prec_vpd_dbh.mmod) # r2m = 0.05, r2c = 0.68
  
  # not using genus nor timestamp
  
  sapflow_hourly_plot_prec_vpd_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + precip_mm + vpd42m_kPa + dbh_cm + (1|ID), 
                                                data = sapflow_met_dbh_2023)
  summary(sapflow_hourly_plot_prec_vpd_dbh.mmod)
  r.squaredGLMM(sapflow_hourly_plot_prec_vpd_dbh.mmod) # r2m = 0.13, r2c = 0.62
}

# soil water content (best model)

# cor(sapflow_met_dbh_2023[, c("rew_250cm_m3_m3", "vwc_100cm_m3_m3", "vwc_250cm_m3_m3", "vwc_400cm_m3_m3", "vwc_50cm_m3_m3")], use = "pairwise.complete.obs")
# head(sapflow_met_dbh_2023)
# summary(sapflow_met_dbh_2023$daily_total_cleaned_bl_sap_flux_Kg_day)

sapflow_daily_plot_vwc_vpd_dbh.mmod <- lmer(formula = daily_total_cleaned_bl_sap_flux_Kg_day ~ plot + gf_vpd16m_kPa + gf_vwc_250cm_m3_m3 + dbh_cm + (1|ID), 
                                            data = sapflow_met_dbh_2023)
summary(sapflow_daily_plot_vwc_vpd_dbh.mmod)
r.squaredGLMM(sapflow_daily_plot_vwc_vpd_dbh.mmod) # r2m = 0.15, r2c = 0.71


#### PREDICT INDIVIDUAL DATA --------------------------------------------------- ####

predicted_sapflow.data <- newData_dbh_genus %>%
  select(date, ID, plot, genus, gf_vpd16m_kPa, gf_vwc_250cm_m3_m3, dbh_cm, precip_mm, size_class)

# predicted_sapflow.data$predicted_daily_total_sap_flux_Kg_day.x <- predict(sapflow_daily_plot_vwc_vpd_dbh.mmod, predicted_sapflow.data, 
                                                                          # allow.new.levels = T)
# sapflow_hourly_plot_genus_dbh.mmod
# sapflow_daily_plot_vwc_vpd_dbh.mmod

bootfit2 <- bootMer(sapflow_daily_plot_vwc_vpd_dbh.mmod, 
                    FUN=function(x)predict(x, predicted_sapflow.data, allow.new.levels = T),
                    nsim=number_simulations_bt)

dim(bootfit2$t)

bootfit2$t[bootfit2$t < 0] <- NA

boot <- as.data.frame(t(bootfit2$t))
tail(boot)

names(boot) <- paste0("daily_total_cleaned_bl_sap_flux_Kg_day_", 1:number_simulations_bt)

boot$mean_daily_total_cleaned_bl_sap_flux_Kg_day <- rowMeans(boot, na.rm = T)

predicted_sapflow.data <- cbind(predicted_sapflow.data, boot)

head(predicted_sapflow.data)
tail(predicted_sapflow.data)

# write_csv(predicted_sapflow.data, "data_processed/bt_predicted_daily_individual_data_sapflow.csv")
write_feather(predicted_sapflow.data, "data_processed/bt_predicted_daily_individual_data_sapflow.feather")

#### SCALING AT PLOT LEVEL ----------------------------------------------------- ####

predicted_sapflow.data <- read_feather("data_processed/bt_predicted_daily_individual_data_sapflow.feather")


### Aggregate plot daily data ####

toPlot_predicted_sapflow.data <- predicted_sapflow.data %>%
  mutate(plot_date = paste0(predicted_sapflow.data$plot, "/", predicted_sapflow.data$date)) %>%
  select(plot_date, contains(("daily_total_cleaned_")))

toDaily_plot_predicted_sapfow <- aggregate(toPlot_predicted_sapflow.data[, -1], 
                                           by = list(toPlot_predicted_sapflow.data$plot_date), 
                                           FUN = sum, na.rm = T)

daily_plot_predicted_sapfow <- toDaily_plot_predicted_sapfow %>%
  mutate(plot = str_split_fixed(Group.1, pattern = "/", n = 2)[, 1],
         date = as_date(str_split_fixed(Group.1, pattern = "/", n = 2)[, 2]),
         year_month = paste0(year(date), "-", month(date))) %>%
         # plot_area_m2 = ifelse(plot == "Control", control_m2, tfe_m2)) %>%
         # predicted_sap_flux_Kg_day = predicted_daily_total_sap_flux_Kg_day,
         # predicted_sap_flux_l_m2_day = predicted_sap_flux_Kg_day/plot_area_m2) %>%
  # select(plot, date, year_month, plot_area_m2, predicted_sap_flux_Kg_day, predicted_sap_flux_l_m2_day, precip_mm, rew_250cm_m3_m3, vwc_250cm_m3_m3)
dplyr::select(-Group.1)

ggplot(daily_plot_predicted_sapfow, 
                      aes(y = mean_daily_total_cleaned_bl_sap_flux_Kg_day, 
                          x = date, 
                          group = plot, 
                          color = plot)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "gam", se = F) +
  theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  ylab("Soil water content (mm)") + xlab("")

write_csv(daily_plot_predicted_sapfow, "data_processed/bt_predicted_daily_total_plot_data_sapflow.csv")


### Aggregate plot monthly data ####

daily_plot_predicted_sapfow$plot_year_month <- paste0(daily_plot_predicted_sapfow$plot, "/", 
                                                      daily_plot_predicted_sapfow$year_month)

toMonthly_plot_predicted_sapfow <- aggregate(as_tibble(daily_plot_predicted_sapfow[, !names(daily_plot_predicted_sapfow) %in% c("plot", "date", "year_month", "plot_year_month")]), 
                                             by = list(daily_plot_predicted_sapfow$plot_year_month), 
                                             FUN = sum, na.rm = T)

# kg per day per ha
toMonthly_plot_predicted_sapfow[, -1] <- toMonthly_plot_predicted_sapfow[, -1]/10000

monthly_plot_predicted_sapfow <- toMonthly_plot_predicted_sapfow %>%
  mutate(plot = str_split_fixed(Group.1, pattern = "/", n = 2)[, 1],
         date = str_split_fixed(Group.1, pattern = "/", n = 2)[, 2],
         date = as_date(paste0(date, "-01"))) %>%
  # select(plot, date, predicted_sap_flux_Kg_month = predicted_sap_flux_Kg_day, predicted_sap_flux_l_m2_month = predicted_sap_flux_l_m2_day, precip_mm)
  dplyr::select(plot, date, everything(), -Group.1)

write_csv(monthly_plot_predicted_sapfow, "data_processed/bt_predicted_monthly_total_plot_data_sapflow.csv")


### PLOTTING #####

monthly_plot_predicted_sapfow <- read_csv("data_processed/bt_predicted_monthly_total_plot_data_sapflow.csv")
daily_plot_predicted_sapfow <- read_csv("data_processed/bt_predicted_daily_total_plot_data_sapflow.csv")

# ggplot(monthly_plot_predicted_sapfow, aes(x = date, y = predicted_sap_flux_l_m2_month, group = plot, color = plot)) +
#   geom_point() + geom_line()

all_year_predicted_sapflow.data <- daily_plot_predicted_sapfow %>%
  mutate(month = "1_Whole year",
         period = paste0(plot, "/", month))

wet_predicted_sapflow.data <- daily_plot_predicted_sapfow %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2023-06-01") %>%
  mutate(month = "2_2023-05",
         period = paste0(plot, "/", month))

dry_predicted_sapflow.data <- daily_plot_predicted_sapfow %>%
  filter(date >= "2023-10-01") %>%
  filter(date < "2023-11-01") %>%
  mutate(month = "3_2023-10",
         period = paste0(plot, "/", month))

wet_dry_predicted_sapflow.data <- bind_rows(all_year_predicted_sapflow.data, wet_predicted_sapflow.data, dry_predicted_sapflow.data)


year_season_predictetd_sapflow.data <- aggregate(as_tibble(wet_dry_predicted_sapflow.data[, !names(wet_dry_predicted_sapflow.data) %in% c("plot", "year_month", "date", "plot_year_month", "month", "period")]), 
                                                 by = list(wet_dry_predicted_sapflow.data$period), 
                                                 FUN = sum, na.rm = T) %>%
  rename("period" = Group.1)

head(year_season_predictetd_sapflow.data)

t_year_season_predictetd_sapflow.data <- as.data.frame(t(year_season_predictetd_sapflow.data)[-1, ]) %>%
  mutate_if(is.character, as.numeric)

colnames(t_year_season_predictetd_sapflow.data) <- t(year_season_predictetd_sapflow.data)[1, ]

head(t_year_season_predictetd_sapflow.data)

col_year_season_predictetd_sapflow.data <- data.frame()
for(col in colnames(t_year_season_predictetd_sapflow.data)){
  
  ind_col_year_season_predictetd_sapflow.data <- data.frame(group = col,
                                                        sap_flow_Kg_day = t_year_season_predictetd_sapflow.data[, col])
 
  col_year_season_predictetd_sapflow.data <- rbind(col_year_season_predictetd_sapflow.data, ind_col_year_season_predictetd_sapflow.data)
}

col_year_season_predictetd_sapflow.data <- col_year_season_predictetd_sapflow.data %>%
  mutate(sap_flow_mm_day = sap_flow_Kg_day/10000,
         plot = str_split_fixed(group, pattern = "/", n = 2)[, 1],
         period = str_split_fixed(group, pattern = "/", n = 2)[, 2],
         )

# factor(col_year_season_predictetd_sapflow.data$period, levels = c("Whole year", "2023-05", "2023-10"))

wy <- col_year_season_predictetd_sapflow.data %>%
  filter(period == "1_Whole year")

lm1 <- lm(sap_flow_mm_day ~ plot, data = wy)
summary(lm1)

wy <- col_year_season_predictetd_sapflow.data %>%
  filter(period == "2_2023-05")

lm1 <- lm(sap_flow_mm_day ~ plot, data = wy)
summary(lm1)

wy <- col_year_season_predictetd_sapflow.data %>%
  filter(period == "3_2023-10")

lm1 <- lm(sap_flow_mm_day ~ plot, data = wy)
summary(lm1)


pdf("results/manuscript/predicted_plot_annual_sapflow_differences.pdf", height = h, width = w)
sapflow_plot.plot <- ggplot(col_year_season_predictetd_sapflow.data, 
                            aes(x = fct_inorder(as.factor(period)), 
                                y = sap_flow_mm_day, 
                                color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  xlab("") + ylab("Predicted plot-level sap flow (mm/day)") + 
  theme_minimal() +
  theme(legend.position = "none")
sapflow_plot.plot
dev.off()



## Total yearly sapflow control = 1475.05 mm

col_year_season_predictetd_sapflow.data %>% 
  filter(group == "Control/1_Whole year") %>%
  pull(sap_flow_mm_day) %>%
  median(na.rm = T)

col_year_season_predictetd_sapflow.data %>% 
  filter(group == "Control/1_Whole year") %>%
  pull(sap_flow_mm_day) %>%
  sd(na.rm = T)

## Total yearly sapflow TFE = 1057.95 mm

col_year_season_predictetd_sapflow.data %>% 
  filter(group == "TFE/1_Whole year") %>%
  pull(sap_flow_mm_day) %>%
  median(na.rm = T)

col_year_season_predictetd_sapflow.data %>% 
  filter(group == "TFE/1_Whole year") %>%
  pull(sap_flow_mm_day) %>%
  sd(na.rm = T)

## 05-2023  sapflow control = 113.05 mm

col_year_season_predictetd_sapflow.data %>% 
  filter(group == "Control/2_2023-05") %>%
  pull(sap_flow_mm_day) %>%
  mean(na.rm = T)

## 05-2023 yearly sapflow TFE = 76.91 mm

col_year_season_predictetd_sapflow.data %>% 
  filter(group == "TFE/2_2023-05") %>%
  pull(sap_flow_mm_day) %>%
  mean(na.rm = T)

## Total yearly sapflow control = 89.11 mm

col_year_season_predictetd_sapflow.data %>% 
  filter(group == "Control/3_2023-10") %>%
  pull(sap_flow_mm_day) %>%
  mean(na.rm = T)

## Total yearly sapflow TFE = 70.30 mm

col_year_season_predictetd_sapflow.data %>% 
  filter(group == "TFE/3_2023-10") %>%
  pull(sap_flow_mm_day) %>%
  mean(na.rm = T)

