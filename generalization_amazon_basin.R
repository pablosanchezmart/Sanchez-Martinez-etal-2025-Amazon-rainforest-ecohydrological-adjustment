#### RELATIONSHIP UPSCALING SAP FLOW DAILY DATA ################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")

# Install and load required packages
# install.packages(c("raster", "sf", "dplyr", "ggplot2", "httr"))
library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(httr)


#### DATA ------------------------------------------------------------- ####

#### amazon precipitation ####

### precipitation and potential evapotranspiration

precip <- raster(paste0(root.dir, "data/global_products/worldclim/wc2.1_2.5m_bio_12.tif"))
precip_ssp126 <- stack(paste0(root.dir, "data/global_products/worldclim/wc2.1_2.5m_bioc_ACCESS-CM2_ssp126_2081-2100.tif"))[[12]]
precip_ssp585 <- stack(paste0(root.dir, "data/global_products/worldclim/wc2.1_2.5m_bioc_ACCESS-CM2_ssp585_2081-2100.tif"))[[12]]

pet <- raster(paste0(root.dir, "data/global_products/pet/Global-AI_ET0_annual_v3/et0_v3_yr.tif"))

### amazon shapefile ####

# Load the Amazon Basin shapefile
amazon <- st_read(paste0(root.dir, "data/amazon_basin_shapefile/amazon_sensulatissimo_gmm_v1.shp"))

# ggplot() +
#   geom_sf(data = amazon, color = "red") +
#   geom_sf(data = amaz.dry, color = "black")

# Transform shapefile to match the CRS of the raster data
amazon <- st_transform(amazon, crs(precip))


### LULC Amazon ####

# Data retrived from MapBiomas, cite as:
# MapBiomas Project, 2023, "Collection 8 of the Annual Land Cover and Land Use Maps of Brazil (1985-2022)"
# https://doi.org/10.58053/MapBiomas/VJIJCL, MapBiomas Data, V1

# Reference scientific publication: 
# Souza at. al. (2020) - Reconstructing Three Decades of Land Use and Land Cover Changes in Brazilian Biomes 
# with Landsat Archive and Earth Engine - Remote Sensing, Volume 12, Issue 17, 10.3390/rs12172735.

## Read matadata
amaz.lulc.meta = read.csv(paste0(root.dir, "data/amazon_covers/mapbiomas-amazon-collection-50-area.csv"))

## Amazon LULC map
amaz.lulc = raster(paste0(root.dir, "data/amazon_covers/mapbiomas-amazon-collection-50-2022-0000000000-0000000000.tif"))

## Amazon forests
amaz.fore = raster(paste0(root.dir, "data/amazon_covers/mapbiomas-amazon-collection-50-area-forest.tif"))

## Amazon savanna
amaz.sava = raster(paste0(root.dir, "data/amazon_covers/mapbiomas-amazon-collection-50-area-savanna.tif"))

## amazon dry forest
amaz.dry <- st_read(paste0(root.dir, "data/amazon_covers/dryflor_rec_gr_shp/seasonally_dryfo.shp"))

# Visualize it
# plot(amaz.lulc)
# plot(amaz.fore)
# plot(amaz.sava)

### amazon biomass ####

## citation: 

# Load the biomass data
# biomass <- raster(paste0(root.dir, "data/global_products/global_biomass_ESA/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2017-fv1.0.nc"), varname = "agb")
# # Mask and crop the biomass and precipitation data to the Amazon Basin
# biomass_amazon <- mask(crop(biomass, amazon), amazon)
# biomass_amazon[biomass_amazon == 0] <- NA
# writeRaster(biomass_amazon, "outputs/biomass_amazon_raster.tif", overwrite = T)
biomass_amazon <- raster("outputs/biomass_amazon_raster.tif")

#### RASTER PREPARATION -------------------------------------------------------- #####

# beginCluster(n = 4)
# amaz.fore_proj <- projectRaster(amaz.fore, crs = crs(biomass_amazon))
# amaz.lulc.res <- resample(amaz.lulc, biomass_amazon, method = "ngb")
# endCluster()

# writeRaster(amaz.lulc.res, "outputs/mapbiomas-amazon-collection-1ha.tif")

# keep biomass for rainforest raster only
# biomass_amazon_rainforest <- biomass_amazon * amaz.fore_proj_res
# save(biomass_amazon_rainforest, file = "outputs/biomass_amazon_rainforest.RData")
# load(file = "outputs/biomass_amazon_rainforest.RData")

precip_amazon <- mask(crop(precip, amazon), amazon)
# precip_ssp126_amazon <- mask(crop(precip_ssp126, amazon), amazon)
# precip_ssp585_amazon <- mask(crop(precip_ssp585, amazon), amazon)
pet_amazon <- mask(crop(pet, amazon), amazon)

### dry forest rasterization ###

# Transform shapefile to match the CRS of the raster data
amaz.dry <- st_transform(amaz.dry, crs(precip_amazon))

amaz.dry <- st_intersection(st_make_valid(amaz.dry), st_make_valid(amazon))
plot(amaz.dry)

amaz.dry_rast <- fasterize(amaz.dry, precip_amazon, field = "Id")

# beginCluster(n = 4)
# raster_resampled <- resample(amaz.dry_rast, 
#                              y = amaz.lulc,
#                              method = "ngb", 
#                              filename = paste0(root.dir, "data/amazon_covers/dryflor_rec_gr_shp/dryflor_rec_gr.tif"), 
#                              overwrite = T)
# endCluster()

amaz.dryfore <- raster(paste0(root.dir, "data/amazon_covers/dryflor_rec_gr_shp/dryflor_rec_gr.tif"))

## delete forest and savanna pixels within amazon dry forest

# Invert the mask to keep values outside the polygon (i.e., where shapefile_raster is 0)

# amaz.fore_ndf <- mask(amaz.fore, amaz.dryfore, maskvalue = 0, inverse = TRUE)
# amaz.sava_ndf <- mask(amaz.sava, amaz.dryfore, maskvalue = 0, inverse = TRUE)

#### AMAZON WATER CONTENT ------------------------------------------------------ ####

soil_vwc_amazon <- list()
forcerun <- F
for(year in c(2017:2023)){
  
  if(!file.exists(paste0("outputs/mean_soil_vwc_", year, "_amazon.tif")) | forcerun){
    
    soil_vwc_year <- brick(paste0(root.dir, "data/global_products/esa_volumetric_soil_water_content/amazon_basin/soil_vwc_", year, ".grib"))
    
    beginCluster(n = 4)
    soil_vwc_year <- projectRaster(soil_vwc_year, crs = crs(biomass_amazon))
    soil_vwc_year_amazon <- mask(crop(soil_vwc_year, amazon), amazon)
    endCluster()
    
    soil_vwc_0_7_year_amazon <- raster::subset(soil_vwc_year_amazon, grep('layer.1', names(soil_vwc_year_amazon), value = T))
    soil_vwc_7_28_year_amazon <- raster::subset(soil_vwc_year_amazon, grep('layer.2', names(soil_vwc_year_amazon), value = T))
    soil_vwc_28_100_year_amazon <- raster::subset(soil_vwc_year_amazon, grep('layer.3', names(soil_vwc_year_amazon), value = T))
    soil_vwc_100_255_year_amazon <- raster::subset(soil_vwc_year_amazon, grep('layer.4', names(soil_vwc_year_amazon), value = T))

    # Calculate the mean across all layers
    mean_soil_vwc_0_7_year_amazon <- calc(soil_vwc_0_7_year_amazon, fun = mean, na.rm = T)
    names(mean_soil_vwc_0_7_year_amazon) <- paste0("vwc_m3_m3_", year, "2023_0_7_cm")
    
    mean_soil_vwc_7_28_year_amazon <- calc(soil_vwc_7_28_year_amazon, fun = mean, na.rm = T)
    names(mean_soil_vwc_7_28_year_amazon) <- paste0("vwc_m3_m3_", year, "2023_7_28_cm")
    
    mean_soil_vwc_28_100_year_amazon <- calc(soil_vwc_28_100_year_amazon, fun = mean, na.rm = T)
    names(mean_soil_vwc_28_100_year_amazon) <- paste0("vwc_m3_m3_", year, "2023_28_100_cm")
    
    mean_soil_vwc_100_255_year_amazon <- calc(soil_vwc_100_255_year_amazon, fun = mean, na.rm = T)
    names(mean_soil_vwc_100_255_year_amazon) <- paste0("vwc_m3_m3_", year, "2023_100_255_cm")
    
    mean_soil_vwc_year_amazon <- stack(mean_soil_vwc_0_7_year_amazon, 
                                       mean_soil_vwc_7_28_year_amazon, 
                                       mean_soil_vwc_28_100_year_amazon, 
                                       mean_soil_vwc_100_255_year_amazon)
    plot(mean_soil_vwc_year_amazon)
    
    #' change the layer name of the raster
    
    terra::writeRaster(mean_soil_vwc_year_amazon, 
                       paste0("outputs/mean_soil_vwc_", year, "_amazon.tif"), 
                       overwrite = T)
    
    soil_vwc_amazon[[paste0("soil_vwc_", year)]] <-  mean_soil_vwc_year_amazon
    
  } else{
    
    soil_vwc_amazon[[paste0("soil_vwc_", year)]] <-  brick(paste0("outputs/mean_soil_vwc_", year, "_amazon.tif"))
  }
}

mean_soil_vwc_amazon <- list()
for(i in 1:4){
  
  mean_soil_vwc_amazon[[i]] <- calc(stack(soil_vwc_amazon$soil_vwc_2017[[i]],
                                          soil_vwc_amazon$soil_vwc_2018[[i]],
                                          soil_vwc_amazon$soil_vwc_2019[[i]],
                                          soil_vwc_amazon$soil_vwc_2020[[i]],
                                          soil_vwc_amazon$soil_vwc_2021[[i]],
                                          soil_vwc_amazon$soil_vwc_2022[[i]],
                                          soil_vwc_amazon$soil_vwc_2023[[i]]),
                                    fun = mean, na.rm = T)
  
}


#### AMAZON EVAPORATION -------------------------------------------------------- ####

eva_amazon <- list()
forcerun <- F
year <- 2023
for(year in c(2017:2023)){
  
  if(!file.exists(paste0("outputs/evaporation_", year, "_amazon.tif")) | forcerun){
    
    eva_year <- brick(paste0(root.dir, "data/global_products/esa_evaporation/amazon_basin/evaporation_", year, ".grib"))
    
    beginCluster(n = 4)
    eva_year <- projectRaster(eva_year, crs = crs(biomass_amazon))
    eva_year_amazon <- mask(crop(eva_year, amazon), amazon)
    endCluster()
    
    # Calculate the sum of evaporation
    sum_eva_year_amazon <- calc(eva_year_amazon, fun = sum, na.rm = T)
    
    sum_eva_year_amazon <- sum_eva_year_amazon * 1000
    
    terra::writeRaster(sum_eva_year_amazon, 
                       paste0("outputs/evaporation_", year, "_amazon.tif"), 
                       overwrite = T)
    
    eva_amazon[[paste0("evapration_", year)]] <-  sum_eva_year_amazon
    
  } else{
    
    eva_amazon[[paste0("evapration_", year)]] <-  brick(paste0("outputs/evaporation_", year, "_amazon.tif"))
  }
}


eva_amazon <- calc(stack(eva_amazon$evapration_2017,
                         eva_amazon$evapration_2018,
                         eva_amazon$evapration_2019,
                         eva_amazon$evapration_2020,
                         eva_amazon$evapration_2021,
                         eva_amazon$evapration_2022,
                         eva_amazon$evapration_2023),
                                  fun = mean, na.rm = T)


#### DATASET SAMPLING ---------------------------------------------------------- ####

# Set the sample size
sample_size <- 1000000
set.seed(123)

# forest pixels

lc_values_fore <- sampleRandom(amaz.fore,
                               size = sample_size,
                               asRaster = FALSE,
                               xy = T) %>%
  as.data.frame() %>%
  filter(mapbiomas.amazon.collection.50.area.forest == 1) %>%
  # sample_n(size = 10000) %>%
  mutate(cover = "forest") %>%
  dplyr::select(x, y, cover)
summary(lc_values_fore)


lc_values_sava <- sampleRandom(amaz.sava,
                               size = sample_size,
                               asRaster = FALSE,
                               xy = T) %>%
  as.data.frame() %>%
  filter(mapbiomas.amazon.collection.50.area.savanna == 1) %>%
  # sample_n(size = 10000) %>%
  mutate(cover = "savanna") %>%
  dplyr::select(x, y, cover)
summary(lc_values_sava)

# sample_size <- 10000
lc_values_dryfore <- sampleRandom(amaz.dryfore,
                               size = sample_size,
                               asRaster = FALSE,
                               xy = T) %>%
  as.data.frame() %>%
  mutate(cover = "dry forest") %>%
  dplyr::select(x, y, cover)
summary(lc_values_dryfore)

lc_values <- bind_rows(lc_values_fore,lc_values_sava, lc_values_dryfore)

# Extract the biomass values at the sampled coordinates
biomass_values <- cbind(lc_values, 
                       raster::extract(biomass_amazon, lc_values[, c("x", "y")], 
                                       df = T)) %>%
  dplyr::select(-ID) %>%
  rename("biomass" = "layer")
summary(biomass_values)

# Extract the precipitation values at the sampled coordinates
precip_values <- cbind(biomass_values, 
                        raster::extract(precip_amazon, biomass_values[, c("x", "y")], 
                                        df = T)) %>%
  dplyr::select(-ID) %>%
  rename("AP_current" = "wc2.1_2.5m_bio_12")
summary(precip_values)

# extract evaporation values at the sampled coordinates
eva_values <- cbind(precip_values, 
                    raster::extract(eva_amazon$layer, biomass_values[, c("x", "y")], 
                                    df = T)) %>%
  dplyr::select(-ID) %>%
  rename("evaporation_mm" = "layer")
summary(eva_values)

# Extract the pet values at the sampled coordinates
pet_values <- cbind(eva_values, 
                        raster::extract(pet_amazon, biomass_values[, c("x", "y")], 
                                        df = T)) %>%
  dplyr::select(-ID)
summary(pet_values)

# Extract the land cover values at the sampled coordinates
lc_values <- cbind(pet_values, raster::extract(amaz.lulc, biomass_values[, c("x", "y")], df = T)) %>%
  dplyr::select(-ID) %>%
  rename("land_cover" = "classification_2022")
summary(lc_values)

# extract soil vwc values

soil_vwc_0_7_cm_values <- cbind(lc_values, raster::extract(mean_soil_vwc_amazon[[1]], biomass_values[, c("x", "y")], df = T)) %>%
  dplyr::select(-ID) %>%
  rename("soil_vwc_0_7_cm_m3_m3" = "layer") 
summary(soil_vwc_0_7_cm_values)

soil_vwc_7_28_cm_values <- cbind(soil_vwc_0_7_cm_values,  raster::extract(mean_soil_vwc_amazon[[2]], biomass_values[, c("x", "y")], df = T)) %>%
  dplyr::select(-ID) %>%
  rename("soil_vwc_7_28_cm_m3_m3" = "layer") 
summary(soil_vwc_7_28_cm_values)

soil_vwc_28_100_cm_values <- cbind(soil_vwc_7_28_cm_values, raster::extract(mean_soil_vwc_amazon[[3]], biomass_values[, c("x", "y")], df = T)) %>%
  dplyr::select(-ID) %>%
  rename("soil_vwc_28_100_cm_m3_m3" = "layer") 
summary(soil_vwc_28_100_cm_values)

soil_vwc_100_255_cm_values <- cbind(soil_vwc_28_100_cm_values, raster::extract(mean_soil_vwc_amazon[[4]], biomass_values[, c("x", "y")], df = T)) %>%
  dplyr::select(-ID) %>%
  rename("soil_vwc_100_255_cm_m3_m3" = "layer") 
summary(soil_vwc_100_255_cm_values)

data <- soil_vwc_100_255_cm_values %>%
  mutate(group = "whole_amazon",
    # CWD_pet = pet_pm_sr_yr - abs(evaporation_mm),
    CWD = abs(evaporation_mm) - AP_current,
    CWA =  AP_current - abs(evaporation_mm),
    wc_biomass_mm_MgC = CWA / biomass,
    # Absolute water content calculation (depth x surafce / 1000 (to express in L/m2))
    soil_wc_0_7_cm_mm = ((soil_vwc_0_7_cm_m3_m3 * (0.07 * 10000)) * 1000) / 10000,
    soil_wc_7_28_cm_mm =((soil_vwc_7_28_cm_m3_m3 * (0.21 * 10000)) * 1000) / 10000,
    soil_wc_28_100_cm_mm = ((soil_vwc_28_100_cm_m3_m3 * (0.72 * 10000)) * 1000) / 10000,
    soil_wc_100_255_cm_mm = ((soil_vwc_100_255_cm_m3_m3 * (1.55 * 10000)) * 1000) / 10000,
    
    mean_vwc = (soil_vwc_0_7_cm_m3_m3 + soil_vwc_7_28_cm_m3_m3 + soil_vwc_28_100_cm_m3_m3 + soil_vwc_100_255_cm_m3_m3) / 4,
    
    absolute_wc_0_400cm_mm = ((mean_vwc * (4 * 10000)) * 1000) / 10000,
    
    absolute_wc_0_100cm_mm = (soil_wc_0_7_cm_mm + soil_wc_7_28_cm_mm + soil_wc_28_100_cm_mm),
    absolute_wc_0_255cm_mm = (soil_wc_0_7_cm_mm + soil_wc_7_28_cm_mm + soil_wc_28_100_cm_mm + soil_wc_100_255_cm_mm),
    )
  # filter(land_cover %in% c("3", "4"))

write_csv(data, "outputs/biomass_precipitation_amazon_basin.csv")

data <- read_csv("outputs/biomass_precipitation_amazon_basin.csv")

forest_data <- data %>%
  filter(cover %in% "forest")

mean(forest_data$biomass, na.rm = T)
sd(forest_data$biomass, na.rm = T)

dry_forest_data <- data %>%
  filter(cover %in% "dry forest")

mean(dry_forest_data$biomass, na.rm = T)
sd(dry_forest_data$biomass, na.rm = T)

sav_data <- data %>%
  filter(cover %in% "savanna")

mean(sav_data$biomass, na.rm = T)
sd(sav_data$biomass, na.rm = T)
dim(data)

# Sample the data frame to keep 10000 observations per group
set.seed(123)
data_sampled <- data %>%
  mutate(land_cover = as.character(cover)) %>%
  filter(!is.na(land_cover)) %>%
  group_by(land_cover) %>%
  sample_n(10000, replace = TRUE) %>%
  ungroup()

dim(data_sampled)
unique(data_sampled$land_cover)

max_wc <-  read_csv("data_processed/annual_soil_wc_2000_2023.csv") %>%
  pull(max_absolute_wc_0_400cm_l_m2) %>%
  max()


# CWD
CWD <- read_csv("data_processed/annual_soil_wc_2000_2023.csv") %>%
  mutate(CWD = max_wc - mean_absolute_wc_0_400cm_l_m2,
         group = ifelse(plot == "Control", "CAX_Control", "CAX_TFE")) %>%
  filter(year > 2016) %>%
  group_by(group) %>%
  # summarize(absolute_wc_0_400cm_mm = max(max_absolute_wc_0_400cm_l_m2, na.rm = TRUE))
  summarize(CWD = max(CWD, na.rm = TRUE))
CWD

# CWA
CWA <- read_csv("data_processed/annual_soil_wc_2000_2023.csv") %>%
  mutate(CWA = max_absolute_wc_0_400cm_l_m2,
         group = ifelse(plot == "Control", "CAX_Control", "CAX_TFE")) %>%
  filter(year > 2016) %>%
  group_by(group) %>%
  summarize(CWA = max(CWA, na.rm = TRUE))
CWA

# SOIL
# soil <- read_csv("data_processed/annual_soil_wc_2000_2023.csv") %>%
#   mutate(absolute_wc_0_255cm_mm = mean_absolute_wc_0_50cm_l_m2 + mean_absolute_wc_50_100cm_l_m2 + max_absolute_wc_100_250cm_l_m2,
#          absolute_wc_0_100cm_mm = mean_absolute_wc_0_50cm_l_m2 + mean_absolute_wc_50_100cm_l_m2,
#          mean_absolute_wc_0_50cm_l_m2,
#          group = ifelse(plot == "Control", "CAX_Control", "CAX_TFE")) %>%
#   filter(year > 2016) %>%
#   group_by(group) %>%
#   summarize(absolute_wc_0_400cm_mm = max(max_absolute_wc_0_400cm_l_m2, na.rm = TRUE))
#   summarize(CWD = max(CWD, na.rm = TRUE))
# CWD


yearly_biomass_soilwc.df <- read_csv("data_processed/annual_biomass_wc_2000_2023.csv")

soil_wc_control <- yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(max_absolute_wc_0_400cm_l_m2) %>%
  mean(na.rm = T)
soil_wc_control

soil_wc_tfe <- yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year > 2017) %>%
  pull(max_absolute_wc_0_400cm_l_m2) %>%
  mean(na.rm = T)
soil_wc_tfe

soil_wc_biomass_control <- yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year > 2017) %>%
  pull(wc_per_biomass_mm_MgC) %>%
  mean(na.rm = T)
soil_wc_biomass_control

soil_wc_biomass_tfe <- yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year > 2017) %>%
  pull(wc_per_biomass_mm_MgC) %>%
  mean(na.rm = T)
soil_wc_biomass_tfe

head(data)

cax_data <- data.frame("biomass" = c(248.28, 163.65), 
                       "AP_current" = c(2085.38, 1087.76),
                       "CWA" = c(soil_wc_control, soil_wc_tfe),
                       "group" = c("CAX_Control", "CAX_TFE"),
                       "land_cover" = c("CAX", "CAX"))

# cax_data <- merge(cax_data, CWD, by = "group")
# cax_data <- merge(cax_data, CWA, by = "group")
  # mutate(wc_biomass_mm_MgC = CWA / biomass)
# cax_data

slope <- (cax_data$biomass[1] - cax_data$biomass[2]) / (cax_data$CWA[1] - cax_data$CWA[2])
slope
# cax_data$wc_biomass <- cax_data$absolute_wc_0_255cm_mm / cax_data$biomass
data <- bind_rows(data_sampled, cax_data)
  # select only forest and savanna 
  # filter(land_cover %in% c("3", "4", "CAX"))

set.seed(123)
plot_data <- data %>%
  mutate(land_cover = as.character(cover)) %>%
  filter(!is.na(land_cover)) %>%
  group_by(land_cover) %>%
  sample_n(1000, replace = TRUE) %>%
  ungroup()

plot_data <- bind_rows(plot_data, cax_data)

forest_data <- data %>%
filter(land_cover %in% c("forest"))

dry_forest_data <- data %>%
  filter(land_cover %in% c("dry forest"))

savanna_data <- data %>%
  filter(land_cover %in% c("savanna"))


### BIOMASS VS CWA ------------------------------------------------------------- ####

# Plot the data
pdf("results/manuscript/amazon_basin_biomass_vs_CWA.pdf", width = w * 2, height = h * 2)
p <- ggplot(plot_data, aes(x = CWA, 
                      y = biomass, 
                      color = land_cover)) +
  geom_point(alpha = 0.8) +
  # stat_density_2d(aes(color = land_cover, fill = ..level..), geom = "polygon") +
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept = 163.65, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  geom_hline(yintercept = 248.28, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +

  # amazon region mean biomass  
  geom_hline(yintercept = 48.15735, linetype = "dashed", colour = "#fde725", linewidth = 0.8) +
  geom_hline(yintercept = 124.4955, linetype = "dashed", colour = "#440154", linewidth = 0.8) +
  geom_hline(yintercept = 267.0465, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  
  geom_vline(xintercept = 655.2174, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  geom_vline(xintercept = 1100.3055, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  # stat_ellipse() + 
  geom_smooth(method = "lm", se = F, color = "#bc3754") + 
  scale_color_viridis_d() +
  labs(x = "CWA (mm)", y = "Biomass (Mg/ha)") +
  theme_minimal() +  
  scale_color_manual(values = c("red", "#440154", "#21918c", "#fde725")) +
  theme(legend.position = "none")
# p
p_with_marginals <- ggMarginal(p, 
                               type = "density", groupFill = TRUE, 
                               groupColour = T)
p_with_marginals
dev.off()

mod1 <- lm(biomass ~ CWA, data = data)
summary(mod1)

mod_fore <- lm(biomass ~ CWA, data = forest_data)
summary(mod_fore)

mod_sava <- lm(biomass ~ CWA, data = savanna_data)
summary(mod_sava)


### t-tests ####

# Calculate means of the groups
mean_dry_forest <- mean(dry_forest_data$biomass, na.rm = T)
mean_forest <- mean(forest_data$biomass, na.rm = T)
mean_savanna <- mean(savanna_data$biomass, na.rm = T)

# Calculate distances of the points to group means
data_points <- cax_data %>%
  mutate(
    Distance_to_forest = abs(biomass - mean_forest),
    Distance_to_dry_forest = abs(biomass - mean_dry_forest),
    Distance_to_savanna = abs(biomass - mean_savanna)
  )
print(data_points)

# Perform t-tests to determine if the points are significantly closer to one group than the other

t_test_forest_a <- t.test(log(forest_data$biomass), mu = log(data_points$biomass[1]))
t_test_forest_a

t_test_dry_forest_a <- t.test(log(dry_forest_data$biomass), mu = log(data_points$biomass[1]))
t_test_dry_forest_a

t_test_savanna_a <- t.test(log(savanna_data$biomass), mu = log(data_points$biomass[1]))
t_test_savanna_a

t_test_forest_b <- t.test(log(forest_data$biomass), mu = log(data_points$biomass[2]))
t_test_forest_b

t_test_dry_forest_b <- t.test(log(dry_forest_data$biomass), mu = log(data_points$biomass[2]))
t_test_dry_forest_b

t_test_savanna_b <- t.test(log(savanna_data$biomass), mu = log(data_points$biomass[2]))
t_test_savanna_b


### BIOMASS VS CWD ------------------------------------------------------------- ####

# Plot the data
pdf("results/manuscript/amazon_basin_biomass_vs_CWD.pdf", width = w * 2, height = h * 2)
p <- ggplot(data, aes(x = CWD, 
                      y = biomass, 
                      color = land_cover)) +
  geom_point() +
  # stat_density_2d(aes(color = land_cover, fill = ..level..), geom = "polygon") +
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept = 163.65, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  geom_hline(yintercept = 248.28, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  geom_vline(xintercept = -496.3279, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  geom_vline(xintercept = -111.1745, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  # stat_ellipse() + 
  geom_smooth(method = "lm", se = F, color = "#bc3754") + 
  scale_color_viridis_d() +
  labs(x = "CWD (mm)", y = "Biomass (Mg/ha)") +
  theme_minimal() +  
  scale_color_manual(values = c("#440154", "#21918c", "#fde725")) +
  theme(legend.position = "none")
# p
p_with_marginals <- ggMarginal(p, 
                               type = "density", groupFill = TRUE, 
                               groupColour = T)
p_with_marginals
dev.off()

mod1 <- lm(biomass ~ HWA, data = data)
summary(mod1)

mod_fore <- lm(biomass ~ CWD_prec, data = forest_data)
summary(mod_fore)

mod_sava <- lm(biomass ~ CWD_prec, data = savanna_data)
summary(mod_sava)


### BIOMASS VS SOIL WC --------------------------------------------------------- ####

# Plot the data
pdf("results/manuscript/amazon_basin_biomass_vs_soil_wc.pdf", width = w * 2, height = h * 2)
p <- ggplot(data, aes(x = absolute_wc_0_400cm_mm, 
                      y = biomass, 
                      color = land_cover)) +
  geom_point() +
  # stat_density_2d(aes(color = land_cover, fill = ..level..), geom = "polygon") +
  geom_smooth(method = "lm", se = F) +
  # geom_hline(yintercept = 163.65, linetype = "dashed", colour = "#21918c", linewidth = 0.8) + 
  # geom_hline(yintercept = 248.28, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  # geom_vline(xintercept = 1087.76, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  # geom_vline(xintercept = 2085.38, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  # stat_ellipse() + 
  geom_smooth(method = "lm", se = F, color = "#bc3754") + 
  scale_color_viridis_d() +
  labs(x = "Soil wc (mm)", y = "Biomass (Mg/ha)") +
  theme_minimal() +  
  scale_color_manual(values = c("#440154", "#21918c", "#fde725")) +
  theme(legend.position = "none")
p
p_with_marginals <- ggMarginal(p, 
                               type = "density", groupFill = TRUE, 
                               groupColour = T)
p_with_marginals
dev.off()

mod1 <- lm(biomass ~ absolute_wc_0_400cm_mm, data = data)
summary(mod1)

mod_fore <- lm(biomass ~ absolute_wc_0_400cm_mm, data = forest_data)
summary(mod_fore)

mod_sava <- lm(biomass ~ absolute_wc_0_400cm_mm, data = savanna_data)
summary(mod_sava)



### BIOMASS VS PRECIPITATION --------------------------------------------------- ####

# Plot the data
pdf("results/manuscript/amazon_basin_biomass_vs_AP.pdf", width = w * 2, height = h * 2)
p <- ggplot(data, aes(x = AP_current, 
                      y = biomass, 
                      color = land_cover)) +
  geom_point() +
  # stat_density_2d(aes(color = land_cover, fill = ..level..), geom = "polygon") +
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept = 163.65, linetype = "dashed", colour = "#21918c", linewidth = 0.8) + 
  geom_hline(yintercept = 248.28, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  geom_vline(xintercept = 1087.76, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  geom_vline(xintercept = 2085.38, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  # stat_ellipse() + 
  geom_smooth(method = "lm", se = F, color = "#bc3754") + 
  scale_color_viridis_d() +
  labs(x = "Annual Precipitation (mm)", y = "Biomass (Mg/ha)") +
  theme_minimal() +  
  scale_color_manual(values = c("#440154", "#21918c", "#fde725")) +
  theme(legend.position = "none")
# p
p_with_marginals <- ggMarginal(p, 
                               type = "density", groupFill = TRUE, 
                               groupColour = T)
p_with_marginals
dev.off()

mod1 <- lm(biomass ~ AP_current, data = data)
summary(mod1)

mod_fore <- lm(biomass ~ AP_current, data = forest_data)
summary(mod_fore)

mod_sava <- lm(biomass ~ AP_current, data = savanna_data)
summary(mod_sava)


### WATER PER UNIT BIOMASS ----------------------------------------------------- ####
a <-  data %>%
  filter(wc_biomass_mm_MgC < 10) %>%
  filter(wc_biomass_mm_MgC > 0) %>%
  filter(!land_cover %in% "CAX")
  
pdf("results/manuscript/amazon_basin_biomass_vs_CWD.pdf", width = w * 2, height = h * 2)
p <- ggplot(a, aes(x = wc_biomass_mm_MgC,
                      fill = land_cover)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 4.431712, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  geom_vline(xintercept = 4.003773, linetype = "dashed", colour = "#21918c", linewidth = 0.8) +
  scale_color_viridis_d() +
  labs(x = "wc per biomass") +
  theme_minimal() +  
  scale_color_manual(values = c("#440154", "#fde725")) +
  scale_fill_manual(values = c("#440154", "#fde725")) 
  # theme(legend.position = "none")
p
dev.off()



#### GENERALIZE PRECIPITATION EFFECT ON AMAZON --------------------------------- ####

a_prec <- 2085.38
b_prec <- a_prec/2

a_bio <- 248.34
b_bio <- 163.91

diff_bio <- a_bio - b_bio

diff_prec <- a_prec - b_prec

slope <- diff_bio / diff_prec

### precipitation difference multiplied by the slope between prec loss and biomass ####

change_precip_ssp126_amazon <- precip_ssp126_amazon - precip_amazon
change_precip_ssp585_amazon <- precip_ssp585_amazon - precip_amazon

# plot(biomass_amazon)
plot(precip_amazon)
plot(change_precip_ssp126_amazon)
plot(change_precip_ssp585_amazon)

### biomass loss ####

# current estimated total biomass

current_total_biomass <- cellStats(biomass_amazon, stat = sum, na.rm = T) / 1000000000 # 165 Pg in whole amazon

## ssp 126
biomass_change_ssp126_amazon <- change_precip_ssp126_amazon * slope

biomass_loss_ssp126_amazon <- biomass_change_ssp126_amazon * -1
biomass_loss_ssp126_amazon[biomass_loss_ssp126_amazon < 0] <- 0

pixel_area_km2 <- raster::area(biomass_loss_ssp126_amazon) * 100

total_biomass_ha_loss_ssp126_amazon <- biomass_loss_ssp126_amazon * pixel_area_km2

total_biomass_loss_ssp126_amazon <- cellStats(total_biomass_ha_loss_ssp126_amazon, stat = sum, na.rm = T) / 1000000000 # net loss of 673901747 Mg, 0.674 Pg

total_biomass_loss_ssp126_amazon

total_biomass_loss_ssp126_amazon/current_total_biomass


## ssp 585
biomass_change_ssp585_amazon <- change_precip_ssp585_amazon * slope

biomass_loss_ssp585_amazon <- biomass_change_ssp585_amazon * -1
biomass_loss_ssp585_amazon[biomass_loss_ssp585_amazon < 0] <- 0


total_biomass_ha_loss_ssp585_amazon <-  biomass_loss_ssp585_amazon * pixel_area_km2 

total_biomass_loss_ssp585_amazon <- cellStats(total_biomass_ha_loss_ssp585_amazon, stat = sum, na.rm = T) / 1000000000 # net loss of 16938987370 Mg, 16.94 Pg, 

total_biomass_loss_ssp585_amazon

total_biomass_loss_ssp585_amazon/current_total_biomass

#### PLOTS --------------------------------------------------------------------- ####

### precipitation ####

decrease_precip_ssp126_amazon <- change_precip_ssp126_amazon * -1
decrease_precip_ssp126_amazon[decrease_precip_ssp126_amazon < 0] <- 0

decrease_precip_ssp585_amazon <- change_precip_ssp585_amazon * -1
decrease_precip_ssp585_amazon[decrease_precip_ssp585_amazon < 0] <- 0

precip_amazon.df <- as.data.frame(precip_amazon, xy = T, na.rm = T)
precip_ssp126_amazon.df <- as.data.frame(precip_ssp126_amazon, xy = T, na.rm = T)
precip_ssp585_amazon.df <- as.data.frame(precip_ssp585_amazon, xy = T, na.rm = T)

decrease_precip_ssp126_amazon.df <- as.data.frame(decrease_precip_ssp126_amazon, xy = T, na.rm = T)
decrease_precip_ssp585_amazon.df <- as.data.frame(decrease_precip_ssp585_amazon, xy = T, na.rm = T)

change_precip_ssp126_amazon.df <- as.data.frame(change_precip_ssp126_amazon, xy = T, na.rm = T)
change_precip_ssp585_amazon.df <- as.data.frame(change_precip_ssp585_amazon, xy = T, na.rm = T)


precip_amazon.plot <- ggplot(data = precip_amazon.df, 
                                             aes(x = x, 
                                                 y = y, 
                                                 fill = wc2.1_2.5m_bio_12)) +
  geom_raster() +
  scale_fill_viridis_c() +  # Choose a color palette (e.g., Viridis)
  labs(x = "", y = "", fill = "Change in AP ssp126 (mm)",
       title = "Current AP") +
  theme_minimal()
precip_amazon.plot

change_precip_ssp126_amazon.plot <- ggplot(data = change_precip_ssp126_amazon.df, 
                                           aes(x = x, 
                                               y = y, 
                                               fill = layer)) +
  geom_raster() +
  scale_fill_viridis_c() +  # Choose a color palette (e.g., Viridis)
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  +
  labs(x = "", y = "", fill = "Change in AP ssp126 (mm)")
change_precip_ssp126_amazon.plot

change_precip_ssp126_amazon.leg <- get_legend(change_precip_ssp126_amazon.plot)

change_precip_ssp126_amazon.plot <- change_precip_ssp126_amazon.plot +
  theme(legend.position = "none")

change_precip_ssp585_amazon.plot <- ggplot(data = change_precip_ssp585_amazon.df, 
                                           aes(x = x, 
                                               y = y, 
                                               fill = layer)) +
  geom_raster() +
  scale_fill_viridis_c() +  # Choose a color palette (e.g., Viridis)
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  +
  labs(x = "", y = "", fill = "Change in AP ssp126 (mm)")
change_precip_ssp585_amazon.plot

change_precip_ssp585_amazon.leg <- get_legend(change_precip_ssp585_amazon.plot)

change_precip_ssp585_amazon.plot <- change_precip_ssp585_amazon.plot +
  theme(legend.position = "none")

pdf("results/manuscript/amazon_basin_precipitation_change.pdf", width = w * 2, height = h)
ggarrange(
  # precip_amazon.plot, 
          change_precip_ssp126_amazon.plot, 
          change_precip_ssp585_amazon.plot, nrow = 1, ncol = 2)
dev.off()

pdf("results/manuscript/amazon_basin_precipitation_change_legend_ssp126.pdf", width = w, height = h)
plot(change_precip_ssp126_amazon.leg)
dev.off()

pdf("results/manuscript/amazon_basin_precipitation_change_legend_ssp585.pdf", width = w, height = h)
plot(change_precip_ssp585_amazon.leg)
dev.off()

### biomass loss ####

biomass_loss_ssp126_amazon.df <- as.data.frame(biomass_loss_ssp126_amazon, xy = T, na.rm = T)
biomass_loss_ssp585_amazon.df <- as.data.frame(biomass_loss_ssp585_amazon, xy = T, na.rm = T)


biomass_loss_ssp126_amazon.plot <- ggplot(data = biomass_loss_ssp126_amazon.df, 
                                             aes(x = x, 
                                                 y = y, 
                                                 fill = layer)) +
  geom_raster() +
  scale_fill_viridis_c() +  # Choose a color palette (e.g., Viridis)
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  +
  labs(fill = "Biomass loss (Mg/ha)", x = "", y = "")
biomass_loss_ssp126_amazon.plot

biomass_loss_ssp126_amazon.leg <- get_legend(biomass_loss_ssp126_amazon.plot)

biomass_loss_ssp126_amazon.plot <- biomass_loss_ssp126_amazon.plot +
  theme(legend.position = "none")

biomass_loss_ssp585_amazon.plot <- ggplot(data = biomass_loss_ssp585_amazon.df, 
                                             aes(x = x, 
                                                 y = y, 
                                                 fill = layer)) +
  geom_raster(show.legend = T) +
  scale_fill_viridis_c() +  # Choose a color palette (e.g., Viridis)
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  +
  labs(fill = "Biomass loss (Mg/ha)", x = "", y = "")
biomass_loss_ssp585_amazon.plot

biomass_loss_ssp585_amazon.leg <- get_legend(biomass_loss_ssp585_amazon.plot)

biomass_loss_ssp585_amazon.plot <- biomass_loss_ssp585_amazon.plot +
  theme(legend.position = "none")

pdf("results/manuscript/amazon_basin_biomass_loss.pdf", width = w * 2, height = h)
ggarrange(
  # space_plot, 
    biomass_loss_ssp126_amazon.plot, 
    # space_plot,
    biomass_loss_ssp585_amazon.plot, 
    # space_plot, 
    nrow = 1, ncol = 2 
    # widths = c(0.05, 1, 0.05, 1, 0.05)
    )
dev.off()


pdf("results/manuscript/amazon_basin_biomass_loss_legend_ssp585.pdf", width = w, height = h)
plot(biomass_loss_ssp585_amazon.leg)
dev.off()

pdf("results/manuscript/amazon_basin_biomass_loss_legend_ssp126.pdf", width = w, height = h)
plot(biomass_loss_ssp126_amazon.leg)
dev.off()



### amazon region on the world

# Load required packages
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Load world map data (medium scale for simplicity)
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Create the plot
pdf("results/manuscript/amazon_basin_location.pdf", width = w, height = h)
ggplot() +
  geom_sf(data = world_map, fill = "black", color = "black") +  # Plot world map
  geom_sf(data = amazon, fill = "darkgreen", color = "white") +  # Highlight Amazon Basin
  # labs(title = "World Map Highlighting the Amazon Basin") +
  theme_void()
dev.off()

## proportion compared to current biomass (net gain vs net loss?)