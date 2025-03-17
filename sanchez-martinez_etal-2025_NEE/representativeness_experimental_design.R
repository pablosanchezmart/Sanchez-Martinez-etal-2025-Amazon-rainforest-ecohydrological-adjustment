#### GROWTH, MORTAILITY AND BIOMASS DATA PREPARATION ###########################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")

tfe_m2 <- 6423
control_m2 <- 6253

# tfe_m2 <- 10000
# control_m2 <- 10000


### DATA ----------------------------------------------------------------------- ####

metadata <- readxl::read_excel(paste0(data.path, "metadata_cax_radar/cax_radar_metadata_caxiuana_10_2023.xlsx"), sheet = 1) %>%
  mutate(size_class = ifelse(dbh_2023_cm > 30, "emergent", "non-emergent"))  %>%  # more than 30m height
  dplyr::select(ID, plot, species, genus, dbh_2023_cm, size_class) %>%
  mutate(basal_area =  dbh_2023_cm^2 * 0.005454)

ID_genus_a <- readxl::read_excel(paste0(root.dir, "data_processed/caxuana_census/ID_taxonomy_2023.xlsx"), 
                                 sheet = "plot_a") %>%
  mutate(ID = paste0("Control_", tree_number),
         genus = str_split_fixed(species, " ", 2)[, 1],
         genus = ifelse(genus == "NA", NA, genus)) %>%
  dplyr::select(ID, species, genus)
ID_genus_b <- readxl::read_excel(paste0(root.dir, "data_processed/caxuana_census/ID_taxonomy_2023.xlsx"), 
                                 sheet = "plot_a") %>%
  mutate(ID = paste0("TFE_", tree_number),
         genus = str_split_fixed(species, " ", 2)[, 1],
         genus = ifelse(genus == "NA", NA, genus)) %>%
  dplyr::select(ID, species, genus)


ID_genus <- bind_rows(ID_genus_a, ID_genus_b)

wd_gbm.data_6 <- read_csv("data_processed/individual_annual_biomass_wc_2000_2023.csv") %>%
  filter(year == 2023) %>%
  dplyr::select(ID, plot, dbh_cm) %>%
  na.omit() %>%
  mutate(basal_area =  dbh_cm^2 * 0.005454)

mean_dbh_2023 <- merge(wd_gbm.data_6, ID_genus, by = "ID", all.x = T)
head(mean_dbh_2023)

all_leaf_wp_2023 <- read_csv(paste0(root.dir, "data_processed/leaf_water_potential_water_content/big_campaign_water_potentials_2023.csv")) %>%
  mutate(plot = ifelse(plot == "A", "Control", "TFE"),
         ID = paste0(plot, "_", id),
         basal_area =  dbh2023^2 * 0.005454) %>%
  filter(season == "dry") %>%
  dplyr::select(ID, plot, genus, basal_area)


### BASAL AREA OF MONITORED GENUS ---------------------------------------------- ####


### plot Control ####

sampled_genus_a <- metadata %>%
  filter(plot == "Control") %>%
  pull(genus) %>%
  unique() 

genus_basal_area_a <- mean_dbh_2023 %>%
  filter(plot == "Control") %>%
  group_by(genus) %>%
  summarize("total_basal_area" = sum(basal_area))

total_basal_area_a <- sum(genus_basal_area_a$total_basal_area)

genus_basal_area_a$total_basal_area_perc <- genus_basal_area_a$total_basal_area / total_basal_area_a
summary(genus_basal_area_a$total_basal_area_perc)

sampled_total_basal_area_a <- genus_basal_area_a %>%
  filter(genus %in% sampled_genus_a) %>%
  pull(total_basal_area_perc) %>%
  sum()


### plot tfe ####

sampled_genus_b <- metadata %>%
  filter(plot == "TFE") %>%
  pull(genus) %>%
  unique() 

genus_basal_area_b <- mean_dbh_2023 %>%
  filter(plot == "TFE") %>%
  group_by(genus) %>%
  summarize("total_basal_area" = sum(basal_area))

total_basal_area_b <- sum(genus_basal_area_b$total_basal_area)

genus_basal_area_b$total_basal_area_perc <- genus_basal_area_b$total_basal_area / total_basal_area_b
summary(genus_basal_area_b$total_basal_area_perc)

sampled_total_basal_area_b <- genus_basal_area_b %>%
  filter(genus %in% sampled_genus_b) %>%
  pull(total_basal_area_perc) %>%
  sum()
sampled_total_basal_area_b

total_basal_area_b / tfe_m2





### BASAL AREA OF BIG CAMPAIGN GENUS ------------------------------------------- ####


### plot Control ####

sampled_genus_a <- metadata %>%
  filter(plot == "Control") %>%
  pull(genus) %>%
  unique() 

genus_basal_area_a <- all_leaf_wp_2023 %>%
  filter(plot == "Control") %>%
  group_by(genus) %>%
  summarize("total_basal_area" = sum(basal_area))

total_basal_area_a <- sum(genus_basal_area_a$total_basal_area, na.rm = T)

genus_basal_area_a$total_basal_area_perc <- genus_basal_area_a$total_basal_area / total_basal_area_a
summary(genus_basal_area_a$total_basal_area_perc)

sampled_total_basal_area_a <- genus_basal_area_a %>%
  filter(genus %in% sampled_genus_a) %>%
  pull(total_basal_area_perc) %>%
  sum()
sampled_total_basal_area_a

### plot tfe ####

sampled_genus_b <- metadata %>%
  filter(plot == "TFE") %>%
  pull(genus) %>%
  unique() 

genus_basal_area_b <- all_leaf_wp_2023 %>%
  filter(plot == "TFE") %>%
  group_by(genus) %>%
  summarize("total_basal_area" = sum(basal_area))

total_basal_area_b <- sum(genus_basal_area_b$total_basal_area, na.rm = T)

genus_basal_area_b$total_basal_area_perc <- genus_basal_area_b$total_basal_area / total_basal_area_b
summary(genus_basal_area_b$total_basal_area_perc)

sampled_total_basal_area_b <- genus_basal_area_b %>%
  filter(genus %in% sampled_genus_b) %>%
  pull(total_basal_area_perc) %>%
  sum(na.rm = T)
sampled_total_basal_area_b

total_basal_area_b / tfe_m2



