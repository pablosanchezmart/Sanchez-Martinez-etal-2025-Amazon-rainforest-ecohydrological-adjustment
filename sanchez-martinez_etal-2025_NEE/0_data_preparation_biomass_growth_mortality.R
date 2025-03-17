#### GROWTH, MORTAILITY AND BIOMASS DATA PREPARATION ###########################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")

tfe_m2 <- 6423
control_m2 <- 6253

tfe_m2 <- 10000
control_m2 <- 10000


#### BIOMASS FUNCTIONS --------------------------------------------------------- ####

# Where wd is wood density (g/cm3), D is DBH (cm), H is height (m).

biomass.control.2018 = function (wd, D, H) {
  
  0.087735*((wd*(D^2)*H)^.953513)}

tfe.bio <-function(wd,D,H){
  
  0.01162*((wd*(D^2)*H)^1.119834)
}

caxiuana_height.tfe = function(D){
  43.979701*(1-exp(-0.060399*D^0.800062))
}

caxiuana_height.control = function(D){
  51.35899*(1-exp(-0.06958*D^0.70448))
}


#### DATA ---------------------------------------------------------------------- ####

## dbh data (from growth)
dbh_pa <- readxl::read_excel(paste0(data.path, "biomass/PA_DBH.xlsx")) %>%
  mutate(plot = "Control",
         ID = paste0(plot, "_", ID)) %>%
  dplyr::select(ID, plot, contains("dbh"))

dbh_pb <- readxl::read_excel(paste0(data.path, "biomass/PB_DBH.xlsx")) %>%
  mutate(plot = "TFE",
         ID = paste0(plot, "_", ID)) %>%
  dplyr::select(ID, plot, contains("dbh"))

dbh.data <- bind_rows(dbh_pa, dbh_pb)

## dbh (from dbh)

dbh_2005_2023 <- read_csv(paste0(data.path, "biomass/dbh_2005_2023.csv")) %>%
  mutate(ID = paste0(plot, "_", tree_number),
         ID_year = paste0(ID, "/", year)) %>%
  dplyr::select(ID_year, dbh_ingrid_cm = clean_dbh_cm)
  

# exclude 2021, only data for TFE
dbh.data <- dbh.data %>%
  dplyr::select(-"dbh2021")

## WD measured (2018)

measured_wd <- read_csv(paste0(data.path, "caxuana_traits/treeData.csv")) %>%
  mutate(plot = ifelse(treatment == "control", "Control", "TFE"),
         ID = paste0(plot, "_", tree)) %>%
  dplyr::select(ID, wd_g_cm3 = wood_density)

## species info

a_id_species <- readxl::read_excel(paste0(data.path, "caxuana_census/ID_taxonomy_2023.xlsx"), 
                                 sheet = "plot_a") %>%
  mutate(genus = str_split_fixed(species, " ", 2)[, 1],
         plot = "Control",
         ID = paste0(plot, "_", tree_number)) %>%
  dplyr::select(ID, plot, species, genus)

b_id_species <- readxl::read_excel(paste0(data.path, "caxuana_census/ID_taxonomy_2023.xlsx"), 
                                 sheet = "plot_b") %>%
  mutate(genus = str_split_fixed(species, " ", 2)[, 1],
         plot = "TFE",
         ID = paste0(plot, "_", tree_number)) %>%
  dplyr::select(ID, plot, species, genus)

id_species <- bind_rows(a_id_species, b_id_species)

## wood density data

 # 180 individuals with measured wd
unique_measured_wd <- aggregate(measured_wd[, "wd_g_cm3"], 
                                  by = list(measured_wd$ID), 
                                  FUN = mean, na.rm = T) %>%
  rename(ID = Group.1) 

# literature wood density data
# literature_wd <- read_csv(paste0(data.path,"biomass/carbon_growth_cax_2005_2019.csv")) %>%
  literature_wd <- read_csv(paste0(data.path,"biomass/growth_2005_2019_cax.csv")) %>%
  mutate(ID = paste0(plot, "_", Tree.ID)) %>%
  dplyr::select(ID, genus, wd_g_cm3 = wd.mean, se_wd_g_cm3 = wd.se)
  
  
  # literature_wd <- getWoodDensity(genus = dbh.data$genus, species = dbh.data$species) %>%
    # mutate(se_wd_g_cm3 = sdWD/nInd)

# 752 individuals with measured wd
unique_literature_wd <- aggregate(literature_wd[, c("wd_g_cm3", "se_wd_g_cm3")], 
                            by = list(literature_wd$ID), 
                            FUN = mean, na.rm = T) %>%
  rename(ID = Group.1)

## literature wood density data

genus_literature_wd_2 <- aggregate(literature_wd[, c("wd_g_cm3", "se_wd_g_cm3")],
                             by = list(literature_wd$genus),
                             FUN = mean, na.rm = T) %>%
  rename(genus = Group.1)


unique_genus_literature_wd_2 <- merge(literature_wd[, c("ID", "genus")],
                         genus_literature_wd_2, by = "genus", all.x = T) %>%
  dplyr::select(-genus)

unique_genus_literature_wd_2 <- aggregate(unique_genus_literature_wd_2[, c("wd_g_cm3", "se_wd_g_cm3")],
                                   by = list(unique_genus_literature_wd_2$ID),
                                   FUN = mean, na.rm = T) %>%
  rename(ID = Group.1)

## soil data

control_soilwc.df <-read_csv(paste0(data.path, "soil_moisture/pablo/wc_time_series/control_daily_soil_water_content_2000-01-01_2024-05-27.csv")) %>%
  mutate(plot = "Control")

tfe_soilwc.df <-read_csv(paste0(data.path, "soil_moisture/pablo/wc_time_series/tfe_daily_soil_water_content_2000-01-01_2024-05-27.csv")) %>%
  mutate(plot = "TFE") %>%
  filter(date >= min(control_soilwc.df$date))

soilwc.df <- bind_rows(control_soilwc.df, tfe_soilwc.df) %>%
  arrange(plot, date)

head(soilwc.df)


#### LOOP TO CALCULATE GROWTH AND MORTALITY ------------------------------------ ####

gbm.data <- data.frame()
for(id in unique(dbh.data$ID)){
  
  ind_dbh.data <- dbh.data %>%
    filter(ID == id)
  
  
  
  ind_gbm.data <- data.frame("ID" = id,
                             "plot" = ind_dbh.data$plot,
                             "year" = as.numeric(str_remove_all(names(ind_dbh.data[, 3]), "dbh")),
                             "dbh_cm" = as.numeric(ind_dbh.data[, 3]),
                             "growth_dbh_cm_year" = NA,
                             "mortality" = ifelse(is.na(as.numeric(ind_dbh.data[, 3])), NA, 0))
  
  for(i in 4:length(dbh.data)){
    
    prev_year <- as.numeric(str_remove_all(names(ind_dbh.data[, i-1]), "dbh"))
    curr_year <- as.numeric(str_remove_all(names(ind_dbh.data[, i]), "dbh"))
    
    prev_dbh <- as.numeric(ind_dbh.data[, i-1])
    curr_dbh <- as.numeric(ind_dbh.data[, i])
    
    ## annual growth calculation
    
    growth_cm_year <- (curr_dbh - prev_dbh) / (curr_year - prev_year)
    
    ## if dbh is measured, it's alive
    
    if(!is.na(curr_dbh)){
      mortality <- 0
      
    } else{
      
      ## otherwise, it previously measured and now missing, it's dead
      
      if(!is.na(prev_dbh) & is.na(curr_dbh)){
        mortality <- 1
        
      } else{
        
        ## if both previous and current data missing (not present yet) or if already dead:  NA
        
        if(is.na(prev_dbh) & is.na(curr_dbh) | is.na(any(ind_gbm.data$mortality == 1)) | any(ind_gbm.data$mortality == 1)){
          
          mortality <- NA
        }
    }
    }
    
    ## individual annual data
    
    year_ind_gbm.data <- data.frame("ID" = id,
                               "plot" = ind_dbh.data$plot,
                               "year" = curr_year,
                               "dbh_cm" = curr_dbh,
                               "growth_dbh_cm_year" = growth_cm_year,
                               "mortality" = mortality)
    
    ## individual all years data
    
    ind_gbm.data <- bind_rows(ind_gbm.data, year_ind_gbm.data)
    
  }
  
  ## all individuals all years data
  
  gbm.data <- bind_rows(gbm.data, ind_gbm.data)
}
gbm.data$survival <- ifelse(gbm.data$mortality == 0, 1, 0)

summary(gbm.data$growth_dbh_cm_year)

gbm.data %>%
  filter(growth_dbh_cm_year < -3)


#### COMPLETE DATASET ---------------------------------------------------------- ####

## add wood density

wd_gbm.data_1 <- merge(gbm.data, 
                     unique_measured_wd, 
                     by = "ID", 
                     all.x = T)

## complete with literature wood density data

wd_gbm.data_2 <- merge(wd_gbm.data_1, unique_literature_wd,
                       by = "ID", all.x = T)
head(wd_gbm.data_2)

wd_gbm.data_3 <- combineData(wd_gbm.data_2, variablesToCombine = c("wd_g_cm3"))
summary(wd_gbm.data_3)

wd_gbm.data_3 %>%
  filter(is.na(wd_g_cm3)) %>%
  pull(ID) %>%
  unique() %>%
  length()

wd_gbm.data_4 <- merge(wd_gbm.data_3, unique_genus_literature_wd_2,
                       by = "ID", all.x = T)
head(wd_gbm.data_4)

wd_gbm.data_5 <- combineData(wd_gbm.data_4, variablesToCombine = c("wd_g_cm3", "se_wd_g_cm3"))
summary(wd_gbm.data_5)


## gap fill with mean wd

wd_gbm.data_5$gf_wd_g_cm3.x <- wd_gbm.data_5$wd_g_cm3
wd_gbm.data_5$gf_wd_g_cm3.y <- mean(wd_gbm.data_5$wd_g_cm3, na.rm = T)

wd_gbm.data_5$gf_se_wd_g_cm3.x <- wd_gbm.data_5$se_wd_g_cm3
wd_gbm.data_5$gf_se_wd_g_cm3.y <- mean(wd_gbm.data_5$se_wd_g_cm3, na.rm = T)

wd_gbm.data_6 <- combineData(wd_gbm.data_5, variablesToCombine = c("gf_wd_g_cm3", "gf_se_wd_g_cm3"))

summary(wd_gbm.data_6)

## upper and lower CI wd

wd_gbm.data_6 <- wd_gbm.data_6 %>%
  mutate(upper_95ci_gf_wd_g_cm3 = gf_wd_g_cm3 + (1.96 * gf_se_wd_g_cm3),
         lower_95ci_gf_wd_g_cm3 = gf_wd_g_cm3 - (1.96 * gf_se_wd_g_cm3))


#### CALCULATE INDIVIDUAL BIOMASS ---------------------------------------------- ####

## add dbh (from census)

# wd_gbm.data_6$ID_year <- paste0(wd_gbm.data_6$ID, "/", wd_gbm.data_6$year)
# 
# wd_gbm.data_6 <- merge(wd_gbm.data_6,
#                        dbh_2005_2023,
#                        by = "ID_year",
#                        all.x = T) %>%
#   dplyr::select(-ID_year)
# summary(wd_gbm.data_6)
# wd_gbm.data_6$dbh_cm <- wd_gbm.data_6$dbh_ingrid_cm

wd_gbm.data_6$height_m <- caxiuana_height.control(wd_gbm.data_6$dbh_cm)

wd_gbm.data_6$biomass_kg <- biomass.control.2018(wd = wd_gbm.data_6$gf_wd_g_cm3, 
                                                                           D = wd_gbm.data_6$dbh_cm,
                                                                           H = wd_gbm.data_6$height_m)


wd_gbm.data_6$upper_95ci_biomass_kg <- biomass.control.2018(wd = wd_gbm.data_6$upper_95ci_gf_wd_g_cm3, 
                                                         D = wd_gbm.data_6$dbh_cm,
                                                         H = wd_gbm.data_6$height_m)

wd_gbm.data_6$lower_95ci_biomass_kg <- biomass.control.2018(wd = wd_gbm.data_6$lower_95ci_gf_wd_g_cm3, 
                                                                    D = wd_gbm.data_6$dbh_cm,
                                                                    H = wd_gbm.data_6$height_m)

wd_gbm.data_6 <- wd_gbm.data_6 %>%
  group_by(ID) %>%
  mutate(size_class = ifelse(mean(dbh_cm, na.rm = T) > 30, "emergent", "non-emergent"))  %>%  # more than 30m height
ungroup()

### save individual data ####

write_csv(wd_gbm.data_6, "data_processed/individual_annual_biomass_wc_2000_2023.csv")
# write_csv(wd_gbm.data_6, "data_processed/individual_annual_biomass_wc_2000_2023_census.csv")
write_csv(wd_gbm.data_6, paste0(data.path, "biomass/individual_annual_biomass_wc_2000_2023.csv"))

emergent_list <- wd_gbm.data_6 %>%
  filter(size_class == "emergent") %>%
  pull(ID) %>%
  unique() %>%
  as.data.frame()

# write_csv(emergent_list, "data_processed/emergent_tree_list.csv")

#### PLOT LEVEL DATA ----------------------------------------------------------- ####

wd_gbm.data_6 <- read_csv("data_processed/individual_annual_biomass_wc_2000_2023.csv")
# wd_gbm.data_6 <- read_csv("data_processed/individual_annual_biomass_wc_2000_2023_census.csv")

sum_variables <- c("biomass_kg", "lower_95ci_biomass_kg", "upper_95ci_biomass_kg", "mortality", "survival")
mean_variables <- c("biomass_kg", "dbh_cm", "growth_dbh_cm_year", "wd_g_cm3", "height_m")

wd_gbm.data_6$plot_year <- paste0(wd_gbm.data_6$plot, "/", wd_gbm.data_6$year)

sum_plot_wd_gbm.data <- aggregate(wd_gbm.data_6[, sum_variables], 
                            by = list(wd_gbm.data_6$plot_year), 
                            FUN = sum, na.rm = T) %>%
  rename(plot_year = Group.1)
head(sum_plot_wd_gbm.data)

mean_plot_wd_gbm.data <- aggregate(wd_gbm.data_6[, mean_variables], 
                          by = list(wd_gbm.data_6$plot_year), 
                          FUN = mean, na.rm = T) %>%
  rename(plot_year = Group.1)

names(mean_plot_wd_gbm.data)[c(-1)] <- paste0("mean_", names(mean_plot_wd_gbm.data)[-1])

head(mean_plot_wd_gbm.data)

se_plot_wd_gbm.data <- aggregate(wd_gbm.data_6[, mean_variables], 
                                   by = list(wd_gbm.data_6$plot_year), 
                                   FUN = std.error) %>%
  rename(plot_year = Group.1)

names(se_plot_wd_gbm.data)[c(-1)] <- paste0("se_", names(se_plot_wd_gbm.data)[-1])
head(se_plot_wd_gbm.data)

## emergent size class

emergent_wd_gbm.data_6 <- wd_gbm.data_6 %>%
  filter(size_class == "emergent")

sum_emergent_plot_wd_gbm.data <- aggregate(emergent_wd_gbm.data_6[, sum_variables], 
                                  by = list(emergent_wd_gbm.data_6$plot_year), 
                                  FUN = sum, na.rm = T) %>%
  rename(plot_year = Group.1)

names(sum_emergent_plot_wd_gbm.data)[c(-1)] <- paste0("emergent_", names(sum_emergent_plot_wd_gbm.data)[-1])
head(sum_emergent_plot_wd_gbm.data)

mean_emergent_plot_wd_gbm.data <- aggregate(emergent_wd_gbm.data_6[, mean_variables], 
                                   by = list(emergent_wd_gbm.data_6$plot_year), 
                                   FUN = mean, na.rm = T) %>%
  rename(plot_year = Group.1)

names(mean_emergent_plot_wd_gbm.data)[c(-1)] <- paste0("emergent_mean_", names(mean_emergent_plot_wd_gbm.data)[-1])
head(mean_emergent_plot_wd_gbm.data)

se_emergent_plot_wd_gbm.data <- aggregate(emergent_wd_gbm.data_6[, mean_variables], 
                                 by = list(emergent_wd_gbm.data_6$plot_year), 
                                 FUN = std.error) %>%
  rename(plot_year = Group.1)

names(se_emergent_plot_wd_gbm.data)[c(-1)] <- paste0("emergent_se_", names(se_emergent_plot_wd_gbm.data)[-1])
head(se_emergent_plot_wd_gbm.data)

## non-emergent size class

non_emergent_wd_gbm.data_6 <- wd_gbm.data_6 %>%
  filter(size_class == "non-emergent")

sum_non_emergent_plot_wd_gbm.data <- aggregate(non_emergent_wd_gbm.data_6[, sum_variables], 
                                           by = list(non_emergent_wd_gbm.data_6$plot_year), 
                                           FUN = sum, na.rm = T) %>%
  rename(plot_year = Group.1)

names(sum_non_emergent_plot_wd_gbm.data)[c(-1)] <- paste0("non_emergent_", names(sum_non_emergent_plot_wd_gbm.data)[-1])
head(sum_non_emergent_plot_wd_gbm.data)

mean_non_emergent_plot_wd_gbm.data <- aggregate(non_emergent_wd_gbm.data_6[, mean_variables], 
                                            by = list(non_emergent_wd_gbm.data_6$plot_year), 
                                            FUN = mean, na.rm = T) %>%
  rename(plot_year = Group.1)

names(mean_non_emergent_plot_wd_gbm.data)[c(-1)] <- paste0("non_emergent_mean_", names(mean_non_emergent_plot_wd_gbm.data)[-1])
head(mean_non_emergent_plot_wd_gbm.data)

se_non_emergent_plot_wd_gbm.data <- aggregate(non_emergent_wd_gbm.data_6[, mean_variables], 
                                          by = list(non_emergent_wd_gbm.data_6$plot_year), 
                                          FUN = std.error) %>%
  rename(plot_year = Group.1)
names(se_non_emergent_plot_wd_gbm.data)[c(-1)] <- paste0("non_emergent_se_", names(se_non_emergent_plot_wd_gbm.data)[-1])

head(se_non_emergent_plot_wd_gbm.data)

### merge all data ####

plot_wd_gbm_1.data <- bind_cols(sum_plot_wd_gbm.data, mean_plot_wd_gbm.data[, -1], se_plot_wd_gbm.data[, -1],
                              sum_emergent_plot_wd_gbm.data[, -1], mean_emergent_plot_wd_gbm.data[, -1], se_emergent_plot_wd_gbm.data[, -1],
                              sum_non_emergent_plot_wd_gbm.data[, -1], mean_non_emergent_plot_wd_gbm.data[, -1], se_non_emergent_plot_wd_gbm.data[, -1]) 
head(plot_wd_gbm_1.data)

# Divide per 1000 to calculate biomass in Mg and divide by 2 to report results in carobn (substrancting the water component)
# calculate mortality rate

plot_wd_gbm.data <- plot_wd_gbm_1.data %>%
  mutate(
    plot = str_split_fixed(plot_year, "/", 2)[, 1],
    year = as.numeric(str_split_fixed(plot_year, "/", 2)[, 2]),
    
    # plot area
    plot_area_ha = ifelse(plot == "Control", control_m2/10000, tfe_m2/10000),
    
    # biomass per area, divide by 2 to get carbon
    carbon_biomass_MgC_ha = ((biomass_kg/1000)/plot_area_ha)/2,
    # CI
    lower_95ci_carbon_biomass_MgC_ha = ((lower_95ci_biomass_kg/1000)/plot_area_ha)/2,
    upper_95ci_carbon_biomass_MgC_ha = ((upper_95ci_biomass_kg/1000)/plot_area_ha)/2,
    
    # emergent trees biomass per area, divide by 2 to get carbon
    emergent_carbon_biomass_MgC_ha = ((emergent_biomass_kg/1000)/plot_area_ha)/2,
    # CI
    emergent_lower_95ci_carbon_biomass_MgC_ha = ((emergent_lower_95ci_biomass_kg/1000)/plot_area_ha)/2,
    emergent_upper_95ci_carbon_biomass_MgC_ha = ((emergent_upper_95ci_biomass_kg/1000)/plot_area_ha)/2,
    
    # emergent trees biomass per area, divide by 2 to get carbon
    non_emergent_carbon_biomass_MgC_ha = ((non_emergent_biomass_kg/1000)/plot_area_ha)/2,
    # CI
    non_emergent_lower_95ci_carbon_biomass_MgC_ha = ((non_emergent_lower_95ci_biomass_kg/1000)/plot_area_ha)/2,
    non_emergent_upper_95ci_carbon_biomass_MgC_ha = ((non_emergent_upper_95ci_biomass_kg/1000)/plot_area_ha)/2,
    
    # mortality rate: mortality/survivors per year
    
    n_years = c(NA, diff(year)),
    
    # mortality_rate = (mortality/(survival + mortality)) / n_years
    mortality_rate = (mortality/(survival)) / n_years,
    
    emergent_mortality_rate = (emergent_mortality/(emergent_survival)) / n_years,
    non_emergent_mortality_rate = (non_emergent_mortality/(non_emergent_survival)) / n_years
    
  ) %>%
  dplyr::select(plot, year, everything(), -plot_year, -n_years)

head(plot_wd_gbm.data)


### save data ####

# plot_wd_gbm.data <- plot_wd_gbm.data %>%
#   dplyr::select(plot, year, mortality, survival, mortality_rate, 
#          carbon_biomass_MgC_ha, lower_95ci_carbon_biomass_MgC_ha, upper_95ci_carbon_biomass_MgC_ha)

write_csv(plot_wd_gbm.data, paste0(data.path, "biomass/plot_annual_biomass_pablo_2002_2023.csv"))
# write_csv(plot_wd_gbm.data, paste0(data.path, "biomass/plot_annual_biomass_pablo_2002_2023_census.csv"))

  
#### ADD SOIL DATA ------------------------------------------------------------- ####
  
## aggregate per year
  
  daily_soilwc.df <- soilwc.df %>%
    mutate(
      date_plot = paste0(date, "_", plot),
      year_plot = paste0(year(date), "_", plot),
      month_plot = paste0(year(date), "_", month(date), "_", plot),
      
      # Absolute water content calculation vwc * (depth x surafce) (*1000 to reprsent in L and divided by 10000 to express in l/m2))
      absolute_wc_0_50cm_l_m2 = ((gf_vwc_50cm_m3_m3 * (0.50 * 10000)) * 1000) / 10000,
      absolute_wc_50_100cm_l_m2 = ((gf_vwc_100cm_m3_m3 * (0.50 * 10000)) * 1000) / 10000,
      absolute_wc_100_250cm_l_m2 = ((gf_vwc_250cm_m3_m3 * (1.50 * 10000)) * 1000) / 10000,
      absolute_wc_250_400cm_l_m2 = ((gf_vwc_400cm_m3_m3 * (1.50 * 10000)) * 1000)/ 10000,
      absolute_wc_0_400cm_l_m2 = absolute_wc_0_50cm_l_m2 + absolute_wc_50_100cm_l_m2 + absolute_wc_100_250cm_l_m2 + absolute_wc_250_400cm_l_m2
    ) %>%
    dplyr::select(date, plot, date_plot, year_plot, month_plot, contains("absolute"))
  
daily_soilwc.df$ew_0_400_cm <- (daily_soilwc.df$absolute_wc_0_400cm_l_m2 - min(daily_soilwc.df$absolute_wc_0_400cm_l_m2, na.rm = T))/ 
  (max(daily_soilwc.df$absolute_wc_0_400cm_l_m2, na.rm = T) - min(daily_soilwc.df$absolute_wc_0_400cm_l_m2, na.rm = T))

summary(daily_soilwc.df)
names(daily_soilwc.df)

write_csv(daily_soilwc.df, "data_processed/daily_soilwc.csv")

 max_annual_soilwc.df <- aggregate(daily_soilwc.df[, c(-1, -2, -3, -4, -5)], 
                                by = list(daily_soilwc.df$year_plot), 
                                FUN = getQuantile, 
                                prob = 0.90) %>%
    rename(year_plot = Group.1) %>%
    mutate(year = str_split_fixed(year_plot, "_", n = 2)[, 1],
           plot = str_split_fixed(year_plot, "_", n = 2)[, 2]) %>%
    dplyr::select(year, plot, everything())
  names(max_annual_soilwc.df)[c(-1, -2, -3)] <- paste0("max_", names(max_annual_soilwc.df)[c(-1, -2, -3)])
  head(max_annual_soilwc.df)
  
 min_annual_soilwc.df <- aggregate(daily_soilwc.df[, c(-1, -2, -3, -4, -5)], 
                                by = list(daily_soilwc.df$year_plot), 
                                FUN = getQuantile, 
                                prob = 0.10) %>%
    rename(year_plot = Group.1)
  names(min_annual_soilwc.df)[c(-1)] <- paste0("min_", names(min_annual_soilwc.df)[c(-1)])
  head(min_annual_soilwc.df)
  
  annual_soilwc.df <- merge(max_annual_soilwc.df, min_annual_soilwc.df, by = "year_plot")
    # mutate(extractable_water_0_400cm_l_m2 = max_absolute_wc_0_400cm_l_m2 - min_absolute_wc_0_400cm_l_m2)
  head(annual_soilwc.df)
  
  # set same origin point (assuming wc was similar in both plots before experiment, do we have data proving that?)

  annual_soilwc.df$max_absolute_wc_0_400cm_l_m2[annual_soilwc.df$plot == "TFE" & annual_soilwc.df$year == 2002] <- annual_soilwc.df$max_absolute_wc_0_400cm_l_m2[annual_soilwc.df$plot == "Control" & annual_soilwc.df$year == 2002]
  
  annual_soilwc.df$max_ew_0_400_cm[annual_soilwc.df$plot == "TFE" & annual_soilwc.df$year == 2002] <- annual_soilwc.df$max_ew_0_400_cm[annual_soilwc.df$plot == "Control" & annual_soilwc.df$year == 2002]
  
  
  mean_annual_soilwc.df <- aggregate(daily_soilwc.df[, c(-1, -2, -3, -4, -5)], 
                                    by = list(daily_soilwc.df$year_plot), 
                                    FUN = mean, 
                                    na.rm = T) %>%
    rename(year_plot = Group.1)
  names(mean_annual_soilwc.df)[c(-1)] <- paste0("mean_", names(mean_annual_soilwc.df)[c(-1)])
  head(mean_annual_soilwc.df)
  
  toSave_annual_soilwc.df <- merge(annual_soilwc.df, mean_annual_soilwc.df, by = "year_plot") 
  head(toSave_annual_soilwc.df)
  
  write_csv(toSave_annual_soilwc.df, "data_processed/annual_soil_wc_2000_2023.csv")
  
#### PRODUCTIVITY AND WC PER UNIT BIOMASS CALCULATION --------------------------  ####
  
  # ingrid's and Pablo's data
  # yearly_biomass.df <- read_csv(paste0(data.path, "biomass/yearly_biomass_cax_tls_allo.csv"))
  # yearly_biomass.df <- read_csv(paste0(data.path, "biomass/plot_annual_biomass_ingrid_pablo_2005_2023.csv")) %>%
  #pablo and vanessa's data
  yearly_biomass.df <- read_csv(paste0(data.path, "biomass/plot_annual_biomass_pablo_2002_2023.csv")) %>%
    mutate(
      # date = dmy(date),
      # year = year(date),
      year_plot = paste0(year, "_", plot),
      productivity_MgC_yr_ha = NA,
      lower_95ci_productivity_MgC_yr_ha = NA,
      upper_95ci_productivity_MgC_yr_ha = NA) 
    # rename(carbon_biomass_MgC_ha = carbon_biomcarbon_biomass_control_MgC_haass_control_MgC_ha)
  head(yearly_biomass.df)
  
### productivity ####
  
yearly_biomass_productivity.df <- data.frame()
years <- unique(yearly_biomass.df$year)
for(p in unique(yearly_biomass.df$plot)){
    
    plot_yearly_biomass.df <- yearly_biomass.df %>%
      filter(plot == p) %>%
      arrange(year)
    
    for(i in 2:length(years)){
      
      prev_year <- years[i-1]
      curr_year <- years[i]
      
      plot_yearly_biomass.df$productivity_MgC_yr_ha[plot_yearly_biomass.df$year == curr_year] <- (plot_yearly_biomass.df$carbon_biomass_MgC_ha[plot_yearly_biomass.df$year == curr_year]-
                                                                                                    plot_yearly_biomass.df$carbon_biomass_MgC_ha[plot_yearly_biomass.df$year == prev_year]) / (curr_year - prev_year)
      
      plot_yearly_biomass.df$upper_95ci_productivity_MgC_yr_ha[plot_yearly_biomass.df$year == curr_year] <- (plot_yearly_biomass.df$upper_95ci_carbon_biomass_MgC_ha[plot_yearly_biomass.df$year == curr_year]-
                                                                                                    plot_yearly_biomass.df$upper_95ci_carbon_biomass_MgC_ha[plot_yearly_biomass.df$year == prev_year]) / (curr_year - prev_year)
      
      plot_yearly_biomass.df$lower_95ci_productivity_MgC_yr_ha[plot_yearly_biomass.df$year == curr_year] <- (plot_yearly_biomass.df$lower_95ci_carbon_biomass_MgC_ha[plot_yearly_biomass.df$year == curr_year]-
                                                                                                               plot_yearly_biomass.df$lower_95ci_carbon_biomass_MgC_ha[plot_yearly_biomass.df$year == prev_year]) / (curr_year - prev_year)
    }
    
    yearly_biomass_productivity.df <- bind_rows(yearly_biomass_productivity.df, plot_yearly_biomass.df)
    
}
  
yearly_biomass.df <- yearly_biomass_productivity.df  %>%
    dplyr::select(year, everything(), 
           # -dat, -date, 
           -plot, -year)
  
### Merge soil and biomass
  
toMerge_annual_soilwc.df <- annual_soilwc.df %>%
    dplyr::select(-year, -plot)
  
yearly_biomass_soilwc.df <- merge(yearly_biomass.df, toMerge_annual_soilwc.df, 
                                    by = "year_plot", all.x = T) %>%
    mutate(
      year = str_split_fixed(year_plot, "_", 2)[, 1],
      plot = str_split_fixed(year_plot, "_", 2)[, 2],
      
      # biomass_per_wc_MgC_l_ha = carbon_biomass_MgC_ha / (absolute_wc_0_400cm_l_m2 * 10000),
      # wc_per_biomass_l_kgC = (absolute_wc_0_400cm_l_m2 * 10000) / (carbon_biomass_MgC_ha * 1000),
      wc_per_biomass_mm_MgC = (max_absolute_wc_0_400cm_l_m2) / (carbon_biomass_MgC_ha),
      
      upper_95ci_wc_per_biomass_mm_MgC = (max_absolute_wc_0_400cm_l_m2) / (lower_95ci_carbon_biomass_MgC_ha),
      lower_95ci_wc_per_biomass_mm_MgC = (max_absolute_wc_0_400cm_l_m2) / (upper_95ci_carbon_biomass_MgC_ha),
      
      ew_per_biomass_mm_MgC = (max_ew_0_400_cm) / (carbon_biomass_MgC_ha),
      
      upper_95ci_ew_per_biomass_mm_MgC = (max_ew_0_400_cm) / (lower_95ci_carbon_biomass_MgC_ha),
      lower_95ci_ew_per_biomass_mm_MgC = (max_ew_0_400_cm) / (upper_95ci_carbon_biomass_MgC_ha)
      
    ) %>%
    dplyr::select(year, plot, everything(), -year_plot) %>%
    # filter(date > "2000-01-01") %>%
    # filter(date < "2023-01-01") %>%
    arrange(plot, year)
  
summary(yearly_biomass_soilwc.df)
  
#### save final dataset ####
  
yearly_biomass_soilwc.df$mean_growth_dbh_cm_year[yearly_biomass_soilwc.df$mean_growth_dbh_cm_year < 0] <- NA
  
write_csv(yearly_biomass_soilwc.df, "data_processed/annual_biomass_wc_2000_2023.csv")
  
names(yearly_biomass_soilwc.df)
  
toGeneral_yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
  dplyr::select(-contains("emergent"), -contains("non_emergent"))

write_csv(toGeneral_yearly_biomass_soilwc.df, paste0(data.path, "biomass/annual_biomass_wc_2000_2023.csv"))

  
### plot ####
  
biomass.plot <- ggplot(yearly_biomass_soilwc.df, 
                         aes(y = carbon_biomass_MgC_ha, 
                             x = year, group = plot,
                             color = plot)) +
    geom_line(alpha = 0.3) +
    # geom_errorbar(aes(ymin=lower_95ci_carbon_biomass_MgC_ha, ymax=upper_95ci_carbon_biomass_MgC_ha), alpha = 0.3) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "gam", se = F) +
    theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
    scale_color_manual(values = c(color_control, color_tfe)) +
    ylab("Biomass (Mg C/ha)") + xlab("")
biomass.plot

# pdf("census_biomass.pdf")
# biomass.plot
# dev.off()
