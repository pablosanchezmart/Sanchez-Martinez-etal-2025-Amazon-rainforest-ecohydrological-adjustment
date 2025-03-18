#### SYNTHESIS #################################################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")


#### DATA ####

## meteo

daily_met_soil_2023 <- read_csv(paste0(data.path, "met/2022-09-18-2023-12-16_daily_met_control_processed.csv")) 

sapflow_2023 <- read_csv("data_processed/daily_cleaned_processed_sapflow_2022_11_17-2023_12_14.csv") %>%
  rename(precip_mm.x = precip_mm_lba, precip_mm.y = precip_mm_infra) %>%
  dplyr::select(date, plot, ID, species, genus, dbh_2023_cm, size_class, contains("sap_flux")) %>%
  # filter(date >= "2022-12-01") %>%
  # filter(date < "2023-12-01")
  filter(date >= "2023-05-01") %>%
  filter(date < "2023-12-01")

## sapflow 

daily_plot_predicted_sapfow <- read_csv("data_processed/predicted_daily_total_plot_data_sapflow.csv")

## stem wc

plot_stem_wc <- read_csv(paste0(data.path, "stem_wc_data_processed_v1_2023_04_29/plot_scaled_15min_stem_water_content.csv"))

## ind. stem wc

ind_stem_wc <- read_csv(paste0(data.path, "stem_wc_data_processed_v1_2023_04_29/individual_15min_stem_water_content.csv"))

## water potentails

big_sampling_2023 <- read_csv(paste0(data.path, "leaf_water_potential_water_content/big_campaign_water_potentials_2023.csv")) %>%
  filter(date_md > "2023-06-01") %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         id = paste0(plot, "_", tree),
         campaign = "2023-10") %>%
  dplyr::select(id, plot, genus, wp_md = psiMD, wp_pd = psiPD, campaign)


## biomass

yearly_biomass_soilwc.df <- read_csv("data_processed/annual_biomass_wc_2000_2023.csv")

## bt predicted sapflow

predicted_sapflow.data <- read_csv("data_processed/bt_predicted_daily_individual_data_sapflow.csv")

## daily sapflow

daily_sapflow_met_2023 <- read_csv("data_processed/daily_cleaned_processed_sapflow_2022-11-17-2024-06-01.csv")


### CONTROL -------------------------------------------------------------------- ####

### biomass ####

a_biomass <- yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(carbon_biomass_MgC_ha) %>%
  mean(na.rm = T)
a_biomass

yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(carbon_biomass_MgC_ha) %>%
  std.error(na.rm = T)


### productivity ####

yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year < 2017) %>%
  pull(productivity_MgC_yr_ha) %>%
  mean(na.rm = T)

yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year < 2017) %>%
  pull(productivity_MgC_yr_ha) %>%
  std.error(na.rm = T)


a_productivity <- yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(productivity_MgC_yr_ha) %>%
  mean(na.rm = T)
a_productivity

yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(productivity_MgC_yr_ha) %>%
  std.error(na.rm = T)


### growth ####

yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(mean_growth_dbh_cm_year) %>%
  mean(na.rm = T)

yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(mean_growth_dbh_cm_year) %>%
  std.error(na.rm = T)


### soil wc per biomass ####

a_prev_wc_bio <- yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year < 2017) %>%
  pull(wc_per_biomass_mm_MgC) %>%
  mean(na.rm = T)
a_prev_wc_bio

yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year < 2017) %>%
  pull(wc_per_biomass_mm_MgC) %>%
  std.error(na.rm = T)

a_wc_bio <- yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(wc_per_biomass_mm_MgC) %>%
  mean(na.rm = T)
a_wc_bio

yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(wc_per_biomass_mm_MgC) %>%
  std.error(na.rm = T)


## Total yearly precipitation = 2085 mm

daily_met_soil_2023 %>% 
  filter(plot == "Control") %>%
  pull(precip_mm) %>%
  sum(na.rm = T)


### soil water content ####

control_soil_wc <- yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(max_absolute_wc_0_400cm_l_m2) %>%
  mean(na.rm = T)
control_soil_wc

yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(max_absolute_wc_0_400cm_l_m2) %>%
  std.error(na.rm = T)

## water soil / prec
(control_soil_wc/control_prec) * 100


### stem water content ####

ind_stem_wc %>%
  filter(plot == "Control") %>%
  pull(water_content_m3.m3) %>%
  mean(na.rm = T)

plot_stem_wc %>%
  filter(plot == "Control") %>%
  pull(absolute_sapwood_water_content_mm) %>%
  std.error(na.rm = T)




### daily transpiration ####

daily_sapflow_met_2023 %>%
  filter(plot == "Control") %>%
  pull(daily_total_gf_cleaned_bl_sap_flux_Kg_day) %>%
  mean(na.rm = T)

daily_sapflow_met_2023 %>%
  filter(plot == "Control") %>%
  pull(daily_total_gf_cleaned_bl_sap_flux_Kg_day) %>%
  sd(na.rm = T)

### plot stem water content ####

control_plot_stem_wc.df <- plot_stem_wc %>%
  filter(plot == "Control") %>%
  pull(absolute_sapwood_water_content_mm) %>%
  mean(na.rm = T)
control_plot_stem_wc.df

se_control_plot_stem_wc.df <- plot_stem_wc %>%
  filter(plot == "Control") %>%
  pull(absolute_sapwood_water_content_mm) %>%
  std.error(na.rm = T)
se_control_plot_stem_wc.df

## water stem / soil
(control_plot_stem_wc.df/control_soil_wc) * 100

## mean yearly sapflow control = 4.51 mm

control_plot_transpiration <- daily_plot_predicted_sapfow %>%
  filter(plot == "Control") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  mean(na.rm = T)
control_plot_transpiration

se_control_plot_transpiration <- daily_plot_predicted_sapfow %>%
  filter(plot == "Control") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  std.error(na.rm = T)
se_control_plot_transpiration

## total yearly sapflow control = 4.51 mm

control_plot_total_transpiration <- daily_plot_predicted_sapfow %>%
  filter(plot == "Control") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  sum(na.rm = T)
control_plot_total_transpiration

## transpiration / water stem
(control_plot_transpiration/control_plot_stem_wc.df) * 100

## transpiration / water stem
(control_plot_total_transpiration/control_prec) * 100

## soil water / water stem
(control_soil_wc/control_prec) * 100

## water potentials

mean_md_wp_oct  <- big_sampling_2023 %>%
  filter(plot == "Control") %>%
  pull(wp_md) %>%
  mean(na.rm = T)
mean_md_wp_oct

se_md_wp_oct  <- big_sampling_2023 %>%
  filter(plot == "Control") %>%
  pull(wp_md) %>%
  std.error(na.rm = T)
se_md_wp_oct


### TFE -------------------------------------------------------------------- ####

### biomass ####

b_biomass <- yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year > 2017) %>%
  pull(carbon_biomass_MgC_ha) %>%
  mean(na.rm = T)
b_biomass

yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year > 2017) %>%
  pull(carbon_biomass_MgC_ha) %>%
  std.error(na.rm = T)


prev_bio <- yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year == 2002) %>%
  pull(carbon_biomass_MgC_ha)

(prev_bio - b_biomass) / prev_bio


## proportion of redution

(a_biomass - b_biomass) / a_biomass

# reduction whole rainforest in Amazon

123 * 0.3408

### productivity ####

yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year < 2017) %>%
  pull(productivity_MgC_yr_ha) %>%
  mean(na.rm = T)

yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year < 2017) %>%
  pull(productivity_MgC_yr_ha) %>%
  std.error(na.rm = T)

b_productivity <- yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year > 2017) %>%
  pull(productivity_MgC_yr_ha) %>%
  mean(na.rm = T)
b_productivity
yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year > 2017) %>%
  pull(productivity_MgC_yr_ha) %>%
  std.error(na.rm = T)

## proportion of redution

(a_productivity - b_productivity) / a_productivity


### growth ####

yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year == 2023) %>%
  pull(mean_growth_dbh_cm_year) %>%
  mean(na.rm = T)

yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year > 2017) %>%
  pull(mean_growth_dbh_cm_year) %>%
  std.error(na.rm = T)


### soil wc per biomass ####

b_prev_wc_bio <- yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year < 2017) %>%
  pull(wc_per_biomass_mm_MgC) %>%
  mean(na.rm = T)

yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year < 2017) %>%
  pull(wc_per_biomass_mm_MgC) %>%
  std.error(na.rm = T)

b_wc_bio <- yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year > 2017) %>%
  pull(wc_per_biomass_mm_MgC) %>%
  mean(na.rm = T)

yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year > 2017) %>%
  pull(wc_per_biomass_mm_MgC) %>%
  std.error(na.rm = T)

(a_wc_bio - b_wc_bio) / b_wc_bio

(a_prev_wc_bio - b_prev_wc_bio) / a_prev_wc_bio


## Total yearly precipitation = 2085 mm

TFE_prec <- daily_met_soil_2023 %>% 
  filter(plot == "TFE") %>%
  pull(precip_mm) %>%
  sum(na.rm = T)
TFE_prec <- TFE_prec/2

### soil water content ####

TFE_soil_wc <- yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year == 2023) %>%
  pull(max_absolute_wc_0_400cm_l_m2)
TFE_soil_wc

## water soil / prec
(TFE_soil_wc/TFE_prec) * 100

### stem water content ####

ind_stem_wc %>%
  filter(plot == "TFE") %>%
  pull(water_content_m3.m3) %>%
  mean(na.rm = T)

plot_stem_wc %>%
  filter(plot == "TFE") %>%
  pull(absolute_sapwood_water_content_mm) %>%
  std.error(na.rm = T)

### plot stem water content ####

TFE_plot_stem_wc.df <- plot_stem_wc %>%
  filter(plot == "TFE") %>%
  pull(absolute_sapwood_water_content_mm) %>%
  mean(na.rm = T)
TFE_plot_stem_wc.df

se_TFE_plot_stem_wc.df <- plot_stem_wc %>%
  filter(plot == "TFE") %>%
  pull(absolute_sapwood_water_content_mm) %>%
  std.error(na.rm = T)
se_TFE_plot_stem_wc.df

## water stem / soil
(TFE_plot_stem_wc.df/TFE_soil_wc) * 100

### sapflow ####

## mean yearly sapflow TFE = 4.51 mm

TFE_plot_transpiration <- daily_plot_predicted_sapfow %>%
  filter(plot == "TFE") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  mean(na.rm = T)
TFE_plot_transpiration

se_tfe_plot_transpiration <- daily_plot_predicted_sapfow %>%
  filter(plot == "TFE") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  std.error(na.rm = T)
se_tfe_plot_transpiration

## total yearly sapflow TFE = 4.51 mm

tfe_plot_total_transpiration <- daily_plot_predicted_sapfow %>%
  filter(plot == "TFE") %>%
  pull(predicted_sap_flux_l_m2_day) %>%
  sum(na.rm = T)
tfe_plot_total_transpiration

a_predicted_sapflow.data <- predicted_sapflow.data %>%
  filter(plot == "Control") %>%
  dplyr::select(contains("daily_total_cleaned_bl_sap_flux_Kg_day"))



## transpiration / water stem
(TFE_plot_transpiration/TFE_plot_stem_wc.df) * 100

## transpiration / water stem
(tfe_plot_total_transpiration/TFE_prec) * 100

## soil water / water stem
(TFE_soil_wc/TFE_prec) * 100

## water potentials

mean_md_wp_oct  <- big_sampling_2023 %>%
  filter(plot == "TFE") %>%
  pull(wp_md) %>%
  mean(na.rm = T)
mean_md_wp_oct

se_md_wp_oct  <- big_sampling_2023 %>%
  filter(plot == "TFE") %>%
  pull(wp_md) %>%
  std.error(na.rm = T)
se_md_wp_oct



### daily transpiration ####

daily_sapflow_met_2023 %>%
  filter(plot == "TFE") %>%
  pull(daily_total_gf_cleaned_bl_sap_flux_Kg_day) %>%
  mean(na.rm = T)

daily_sapflow_met_2023 %>%
  filter(plot == "TFE") %>%
  pull(daily_total_gf_cleaned_bl_sap_flux_Kg_day) %>%
  sd(na.rm = T)
