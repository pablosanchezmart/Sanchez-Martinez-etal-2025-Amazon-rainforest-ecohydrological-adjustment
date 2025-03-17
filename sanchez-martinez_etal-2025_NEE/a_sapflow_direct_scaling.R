#### RELATIONSHIP UPSCALING SAP FLOW DAILY DATA ################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")
# forcerun <- T

#### DATA ---------------------------------------------------------------------- ####

sapflow.variables <- c("daily_total_cleaned_bl_sap_flux_Kg_day", "daily_total_gf_cleaned_bl_sap_flux_Kg_day", "daily_total_gf_cleaned_bl_sap_flux_Kg_day_cm")
id_variables <- c("date", "ID", "species", "plot", "dbh_2023_cm", "size_class")
met_vars <- c("vpd16m_kPa", "gf_vpd16m_kPa", "vpd42m_kPa", "gf_vpd42m_kPa")
soil_vars <- c("gf_vwc_250cm_m3_m3", "vwc_250cm_m3_m3")

# from sapflow script

sapflow_2023 <- read_csv("data_processed/daily_cleaned_processed_sapflow_2022-11-17-2024-06-01.csv") %>%
  dplyr::select(all_of(c(sapflow.variables, id_variables, met_vars))) %>%
  filter(date >= "2023-05-01") %>%
  filter(date < "2024-05-01")
names(sapflow_2023)

# meteo

met_2023 <- read_csv(paste0(data.path, "met/2023-01-01-2024-12-31_met_control_processed.csv")) %>%
  filter(!is.na(date)) %>%
  mutate(month = as_date(dmy(paste0("01-", month(date, label = F), "-", year(date)))),
         ID = "Control") %>% # for gap filling pursposes
  dplyr::select(timestamp, date, ID, month, vpd16m_kPa, vpd42m_kPa) %>%
  filter(date > "2023-04-01")  %>%
  filter(date < "2024-05-01")

names(met_2023)
met_2023_1 <- gapFillTimeSeries(met_2023, variable = "vpd16m_kPa")
met_2023 <- gapFillTimeSeries(met_2023_1, variable = "vpd42m_kPa")

met_2023 <- met_2023 %>%
  select(-ID) %>%
  dplyr::select("date", all_of(met_vars)) %>%
  arrange(date) %>%
  select("date", all_of(met_vars))

met <- aggregate(met_2023, by = list(met_2023$date), FUN = mean, na.rm = T) %>%
  dplyr::select(-Group.1)


### soil daily data

soil_a <- read_csv(paste0(root.dir, "data_processed/soil_moisture/pablo/wc_time_series/control_daily_soil_water_content_2000-01-01_2024-05-27.csv")) %>%
  mutate(plot = "Control")
soil_b <- read_csv(paste0(root.dir, "data_processed/soil_moisture/pablo/wc_time_series/tfe_daily_soil_water_content_2000-01-01_2024-05-27.csv")) %>%
  mutate(plot = "TFE")
soil <- bind_rows(soil_a, soil_b) 

sapflow_2023 <- merge(sapflow_2023, soil, by = c("plot", "date"), all.x = T)

### dbh

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
  select(ID, dbh_cm, gf_wd_g_cm3) %>%
  filter(!is.na(dbh_cm))

# add genus to dbh data

mean_dbh_2023 <- merge(wd_gbm.data_6, ID_genus, by = "ID", all.x = T)

# add dbh to sapflow data

sapflow_met_dbh_2023 <- merge(sapflow_2023, mean_dbh_2023, by = "ID", all.x = T) %>%
  arrange(ID, date) %>%
  mutate(date_plot = paste0(date, "/", plot))

names(sapflow_met_dbh_2023)

## Divide TFE precipitation by 2

# sapflow_met_dbh_2023[sapflow_met_dbh_2023$plot == "TFE", "precip_mm"] <- sapflow_met_dbh_2023[sapflow_met_dbh_2023$plot == "TFE", "precip_mm"]/2


#### previous plots ####

ggplot(data = sapflow_met_dbh_2023, aes(x = as_date(date), y = daily_total_cleaned_bl_sap_flux_Kg_day, group = plot, colour = plot)) + 
  geom_smooth(method = "loess")

ggplot(data = sapflow_met_dbh_2023, aes(x = as_date(date), y = gf_vpd16m_kPa, group = plot, colour = plot)) + 
  geom_smooth(method = "loess")

ggplot(data = sapflow_met_dbh_2023, aes(x = as_date(date), y = gf_vwc_250cm_m3_m3, group = plot, colour = plot)) + 
  geom_smooth(method = "loess")

#### BOOTSTRAP LINEAR MODEL PREDICTION ----------------------------------------- ####

# Fit a linear model to the sample data
lm_model <- lm(daily_total_gf_cleaned_bl_sap_flux_Kg_day ~ plot + dbh_cm + gf_vwc_250cm_m3_m3 + gf_vpd16m_kPa, data = sapflow_met_dbh_2023)
summary(lm_model)

sapflow_met_dbh_2023 <- sapflow_met_dbh_2023 %>%
  filter(!is.na(date)) %>%
  filter(!is.na(ID)) %>%
  filter(!is.na(dbh_cm))
summary(sapflow_met_dbh_2023)

# lm_model <- lme(
#   daily_total_gf_cleaned_bl_sap_flux_Kg_day ~ dbh_cm + gf_vwc_250cm_m3_m3 + gf_vpd16m_kPa,
#   random = ~ 1 | ID,
#   correlation = corAR1(form = ~ as.numeric(date) | ID),
#   data = sapflow_met_dbh_2023
# )
# summary(lm_model)
# r.squaredGLMM(lm_model) # r2m = 0.15, r2c = 0.71
# 
# # Use predictInterval() to get prediction intervals
# predictions <- predict(lm_model, newdata = df_pred, allow.new.levels = T)

### prepare newdata to be predicted ####

all_dates <- unique(sapflow_met_dbh_2023$date)
df_pred <- expand.grid(ID = wd_gbm.data_6$ID, date = all_dates)
df_pred$plot <- ifelse(str_detect(df_pred$ID, "Control"), "Control", "TFE")

# Merge the forest data (dbh) with the prediction data frame
df_pred <- df_pred %>%
  left_join(mean_dbh_2023, by = 'ID') %>%
  left_join(soil, by = c("plot", "date")) %>%
  left_join(met, by = c("date")) %>%
  filter(!is.na(date)) %>%
  filter(!is.na(ID)) %>%
  filter(!is.na(dbh_cm))
summary(df_pred)

# Function to fit model and predict sap flow
fit_predict <- function(data, indices) {
  d <- data[indices, ] %>%
    distinct(ID, date, .keep_all = TRUE)
  
  # model <- lme(daily_total_gf_cleaned_bl_sap_flux_Kg_day ~ dbh_cm + gf_vwc_250cm_m3_m3 + gf_vpd16m_kPa,
  #              random = ~ 1 | ID,
  #              correlation = corAR1(form = ~ as.numeric(date) | ID), data = d)
  model <- lmer(
    daily_total_gf_cleaned_bl_sap_flux_Kg_day ~ dbh_cm + gf_vwc_250cm_m3_m3 + gf_vpd16m_kPa + (1 | ID), #(1 | as.numeric(date)),
    data = d
    )
    # model <- lm(
      # daily_total_gf_cleaned_bl_sap_flux_Kg_day ~ plot + dbh_cm + gf_vwc_250cm_m3_m3 + gf_vpd16m_kPa, #(1 | as.numeric(date)),
      # data = d
  # )
  
  predictions <- predict(model, newdata = df_pred, allow.new.levels = T)
  return(predictions)
}

# Bootstrapping to calculate 95% confidence intervals for predictions
set.seed(123)  # For reproducibility
boot_results <- boot(data = sapflow_met_dbh_2023, 
                     statistic = fit_predict, 
                     R = 3)

# save(boot_results, "outputs/boot_object.RData")
# load(file = "outputs/boot_object.RData")

# Extract the mean prediction and 95% confidence intervals
df_pred$mean_prediction <- colMeans(boot_results$t)
df_pred$lower_bound <- apply(boot_results$t, 2, quantile, probs = 0.025, na.rm = T)
df_pred$upper_bound <- apply(boot_results$t, 2, quantile, probs = 0.975, na.rm = T)

df_pred$mean_prediction[df_pred$mean_prediction < 0] <- NA
df_pred$lower_bound[df_pred$lower_bound < 0] <- NA
df_pred$upper_bound[df_pred$upper_bound < 0] <- NA
summary(df_pred)

a <- df_pred %>%
  filter(ID == "Control_1.2")
summary(a)

# gap fill with mean per individual

id_means <- sapflow_met_dbh_2023 %>%
  group_by(ID) %>%
  summarise(id_mean_sap_flow = mean(daily_total_gf_cleaned_bl_sap_flux_Kg_day, na.rm = TRUE))

# Merge monthly means with df_pred
df_pred <- df_pred %>%
  left_join(id_means, by = c("ID"))

# Fill missing sap_flow predictions with monthly mean sap flow
df_pred <- df_pred %>%
  mutate(
    gf_mean_prediction = ifelse(is.na(mean_prediction), id_mean_sap_flow, mean_prediction)
  )

# gap fill with mean per plot 

plot_means <- sapflow_met_dbh_2023 %>%
  group_by(plot) %>%
  summarise(plot_mean_sap_flow = mean(daily_total_gf_cleaned_bl_sap_flux_Kg_day, na.rm = TRUE))

# Merge monthly means with df_pred
df_pred <- df_pred %>%
  left_join(plot_means, by = c("plot"))

# Fill missing sap_flow predictions with monthly mean sap flow
df_pred <- df_pred %>%
  mutate(
    gf2_mean_prediction = ifelse(is.na(gf_mean_prediction), plot_mean_sap_flow, gf_mean_prediction)
  )

summary(df_pred)

# Sum the sap flow predictions to get the total forest sap flow per day

total_forest_sap_flow <- df_pred %>%
  mutate(plot_date = paste0(plot, "/", date)) %>%
  dplyr::group_by(plot_date) %>%
  summarise(
    total_sap_flow_kg = sum(gf2_mean_prediction, na.rm = TRUE),
    lower95CI_total_sap_flow_kg = sum(lower_bound, na.rm = TRUE),
    upper95CI_total_sap_flow_kg = sum(upper_bound, na.rm = TRUE)
  ) %>%
  mutate(total_sap_flow_mm = total_sap_flow_kg / 10000,
         lower95CI_total_sap_flow_mm = lower95CI_total_sap_flow_kg / 10000,
         upper95CI_total_sap_flow_mm = upper95CI_total_sap_flow_kg / 10000
         ) %>%
  mutate(plot = str_split_fixed(plot_date, "/", 2)[, 1],
         date = as_date(str_split_fixed(plot_date, "/", 2)[, 2])) %>%
  dplyr::select(plot, date, 
                total_sap_flow_kg, 
                lower95CI_total_sap_flow_kg, upper95CI_total_sap_flow_kg,
                total_sap_flow_mm, 
                lower95CI_total_sap_flow_mm, upper95CI_total_sap_flow_mm
                )

# Print or save the result
dim(total_forest_sap_flow)
summary(total_forest_sap_flow)

total_forest_sap_flow %>%
  filter(plot == "Control") %>%
  pull(total_sap_flow_mm) %>%
  sum(na.rm = T)

total_forest_sap_flow %>%
  filter(plot == "Control") %>%
  pull(lower95CI_total_sap_flow_mm) %>%
  sum(na.rm = T)

total_forest_sap_flow %>%
  filter(plot == "Control") %>%
  pull(upper95CI_total_sap_flow_mm) %>%
  sum(na.rm = T)


total_forest_sap_flow %>%
  filter(plot == "TFE") %>%
  pull(total_sap_flow_mm) %>%
  sum(na.rm = T)

total_forest_sap_flow %>%
  filter(plot == "TFE") %>%
  pull(lower95CI_total_sap_flow_mm) %>%
  sum(na.rm = T)

total_forest_sap_flow %>%
  filter(plot == "TFE") %>%
  pull(upper95CI_total_sap_flow_mm) %>%
  sum(na.rm = T)

n_ind_control <- df_pred %>%
  filter(plot == "Control") %>%
  pull(ID) %>%
  unique() %>%
  length()
n_ind_control

n_ind_tfe <- df_pred %>%
  filter(plot == "TFE") %>%
  pull(ID) %>%
  unique() %>%
  length()
n_ind_tfe

control_mean_sapflow_ind_kg_day <- sapflow_met_dbh_2023 %>%
  filter(plot == "Control") %>%
  pull(daily_total_gf_cleaned_bl_sap_flux_Kg_day) %>%
  mean(na.rm = T)

tfe_mean_sapflow_ind_kg_day <- sapflow_met_dbh_2023 %>%
  filter(plot == "TFE") %>%
  pull(daily_total_gf_cleaned_bl_sap_flux_Kg_day) %>%
  mean(na.rm = T)

(control_mean_sapflow_ind_kg_day * n_ind_control/ 10000) * 365

(tfe_mean_sapflow_ind_kg_day * n_ind_tfe/ 10000) * 365

ggplot(data = total_forest_sap_flow, aes(x = as_date(date), y = total_sap_flow_mm, group = plot, colour = plot)) + geom_line()


write.csv(total_forest_sap_flow, 'total_forest_sap_flow.csv', row.names = FALSE)


# all_year_predicted_sapflow.data <- daily_plot_predicted_sapfow %>%
#   mutate(month = "1_Whole year",
#          period = paste0(plot, "/", month))
# 
# wet_predicted_sapflow.data <- daily_plot_predicted_sapfow %>%
#   filter(date >= "2023-05-01") %>%
#   filter(date < "2023-06-01") %>%
#   mutate(month = "2_2023-05",
#          period = paste0(plot, "/", month))
# 
# dry_predicted_sapflow.data <- daily_plot_predicted_sapfow %>%
#   filter(date >= "2023-10-01") %>%
#   filter(date < "2023-11-01") %>%
#   mutate(month = "3_2023-10",
#          period = paste0(plot, "/", month))
# 
# wet_dry_predicted_sapflow.data <- bind_rows(all_year_predicted_sapflow.data, wet_predicted_sapflow.data, dry_predicted_sapflow.data)
# 

#### DIRECT SCALING PLOT LEVEL ------------------------------------------------- ####

plot_mean_sap_flow_kg_day_cm <- aggregate(sapflow_met_dbh_2023[, c("daily_total_gf_cleaned_bl_sap_flux_Kg_day_cm", "daily_total_gf_cleaned_bl_sap_flux_Kg_day")],
                                          FUN = meanOrMode,
                                          by = list("date_plot" = sapflow_met_dbh_2023$date_plot))


df_pred$date_plot <- paste0(df_pred$date, "/", df_pred$plot)

direct_df_pred <- merge(df_pred, plot_mean_sap_flow_kg_day_cm, by = "date_plot", all.x = T) %>%
  mutate(total_id_sapflow_kg = daily_total_gf_cleaned_bl_sap_flux_Kg_day_cm  * dbh_cm)
head(direct_df_pred)

summary(direct_df_pred)


# Sum the sap flow predictions to get the total forest sap flow per day
total_forest_sap_flow <- direct_df_pred %>%
  mutate(plot_date = paste0(plot, "/", date)) %>%
  dplyr::group_by(plot_date) %>%
  summarise(
    total_sap_flow_kg = sum(total_id_sapflow_kg, na.rm = TRUE)
    # lower95CI_total_sap_flow_kg = sum(lower_bound, na.rm = TRUE),
    # upper95CI_total_sap_flow_kg = sum(upper_bound, na.rm = TRUE)
  ) %>%
  mutate(total_sap_flow_mm = total_sap_flow_kg / 10000
         # lower95CI_total_sap_flow_mm = lower95CI_total_sap_flow_kg / 10000,
         # upper95CI_total_sap_flow_mm = upper95CI_total_sap_flow_kg / 10000
  ) %>%
  mutate(plot = str_split_fixed(plot_date, "/", 2)[, 1],
         date = as_date(str_split_fixed(plot_date, "/", 2)[, 2])) %>%
  dplyr::select(plot, date, 
                total_sap_flow_kg, 
                # lower95CI_total_sap_flow_kg, upper95CI_total_sap_flow_kg,
                total_sap_flow_mm, 
                # lower95CI_total_sap_flow_mm, upper95CI_total_sap_flow_mm
  ) %>%
  ungroup()

# Print or save the result
tail(total_forest_sap_flow)

total_forest_sap_flow %>%
  filter(plot == "Control") %>%
  pull(total_sap_flow_mm) %>%
  sum(na.rm = T)

total_forest_sap_flow %>%
  filter(plot == "TFE") %>%
  pull(total_sap_flow_mm) %>%
  sum(na.rm = T)

ggplot(data = total_forest_sap_flow, aes(x = as_date(date), y = total_sap_flow_mm, group = plot, colour = plot)) + geom_line()

head(pred.df)


