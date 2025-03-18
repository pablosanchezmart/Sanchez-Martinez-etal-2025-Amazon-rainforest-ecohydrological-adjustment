#### INITIALIZATION SENSOR DATA PROCESSING #####################################

## Pablo Sanchez Martinez
## 05/2023

remove(list = ls())


#### CREATE DIRECTIORES -------------------------------------------------------- ####

root.dir <- c("C://Users//psanche2//OneDrive - University of Edinburgh//postdoc_UoE//")

# root.dir <- c("C://Users//pablo//OneDrive - University of Edinburgh//postdoc_UoE//")

dir.create("scripts/", showWarnings = F)

dir.create("outputs/", showWarnings = F)
dir.create("outputs/plots/", showWarnings = F)
dir.create("outputs/plots/leaf_water_potential/", showWarnings = F)
dir.create("outputs/plots/leaf_water_content/", showWarnings = F)
dir.create("outputs/plots/individual_sapflow/", showWarnings = F)

dir.create("results/", showWarnings = F)

data.path <- "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data_processed/"


#### PACKAGES ------------------------------------------------------------------ ####

# install.packages("rnaturalearthdata", dependencies = T)

library(lmeresampler)

library(readr)
library(ggplot2)
library(ggpubr)
library(lubridate)
# library(TrEvol)
library(lme4)
library(lmerTest)
library(MuMIn)
library(interactions)
library(stringr)
library(arrow)
library(tidyverse)
library(plotrix)
library(BIOMASS)
library(lavaan)
library(lavaanPlot)
library(lme4)

library(nlme)
library(boot)
library(terra)
library(ncdf4)
library(raster)
library(ggExtra)

library(dplyr)
library(fasterize)


#### PARAMETERS ---------------------------------------------------------------- ####

space_plot <- ggplot() + theme_void()

number_simulations_bt <- 10

h <- 3
w <- 3
forcerun <- F

tfe_m2 <- 6423
control_m2 <- 6253

tfe_m2 <- 10000
control_m2 <- 10000

plot_area <- 10000


color_control <- "#1b9e77ff"
color_tfe <- "#d95f02ff"

#### FUNCTIONS ----------------------------------------------------------------- ####

### Mean or mode ####

meanOrMode <- function (x){
  require(lubridate)
  
  # all NAs?
  if(all(is.na(x))){
    return(NA)
  } else{
    
    # Mean for numbers
    
    if (is.numeric(x) | is.double(x) | is.integer(x)) {
      meanX <- mean(x, na.rm = T)
      return(meanX)
    } else{
      
      # mode for characters
      if (is.character(x) | is.factor(x)| lubridate::is.Date(x)) {
        tble <- as.data.frame(table(x))
        modeX <- tble %>% dplyr::filter(.data$Freq == max(.data$Freq)) %>% 
          dplyr::pull(.data$x) %>%
          as.character()
        return(modeX)
      }
    }
  }
}


### combine data ####

combineData <- function(data, variablesToCombine){
  
  data <- as.data.frame(data)
  
  for(var in variablesToCombine){
    var.x <- paste(var, ".x", sep = "")
    var.y <- paste(var, ".y", sep = "")
    
    var.class <- class(data[, var.x])
    
    # choose default value from 'x' in the case that both '.x' and '.y' have values
    data[!is.na(data[, var.x]) & !is.na(data[, var.y]), var] <- data[!is.na(data[, var.x]) & !is.na(data[, var.y]), var.x]
    
    # replace NA values in 'x' with values from 'y' and vice-versa
    data[is.na(data[, var.x]), var] <- data[is.na(data[, var.x]), var.y]
    data[is.na(data[, var.y]), var] <- data[is.na(data[, var.y]), var.x]
    
    # Delete ".x" and ".y" columns
    data[, var.x] <- NULL
    data[, var.y] <- NULL
    
    if(var.class %in% c("numeric", "integer", "double")){
      data[, var] <- as.numeric(data[, var])
    } else{
      data[, var] <- as.character(data[, var])
    }
  }
  return(data)
}


### Calculate quantile for a given variable ####

getQuantile <- function(x, prob = 0.05){
  res <- as.numeric(quantile(x, probs = prob, na.rm = T))
  return(res)
}

### gap fill time series using monthly mean for that hour ####

gapFillTimeSeries <- function(data = NULL, variable = NULL){
  
  if(is.null(data)){
    stop("please specify the data argument")
  }
  
  if(is.null(variable)){
    stop("please specify the variable argument")
  }
  
  if(!"timestamp" %in% names(data)){
    stop("please include a date_time column called timestamp")
  }
  
  if(!"ID" %in% names(data)){
    stop("please include a character column called ID")
  }
  
  timestamp_backbone <- data.frame("timestamp" = seq(as_datetime(min(data$timestamp)), 
                                                     as_datetime(max(data$timestamp)), 
                                                     by = "60 min"))
  
  data$month <- paste0(lubridate::month(data$timestamp, label = T, abbr = T), "-", year(data$timestamp))
  
  gf_data <- data.frame()
  for(id in unique(data$ID)){
    
    for(m in unique(data$month)){
      
      id_sf_data <- data %>%
        filter(ID == id) %>%
        filter(month == m)
      
      if(length(id_sf_data$timestamp) < 1){
        next("no data for this month")
      }
      
      ## generate full time series (to gap fill in some cases)
      
      timestamp_backbone_month <- timestamp_backbone %>%
        mutate(month = paste0(lubridate::month(timestamp, label = T, abbr = T), "-", year(timestamp))) %>%
        filter(month == m) %>%
        dplyr::select(-month)
      
      id_sf_data <- merge(timestamp_backbone_month,
                          id_sf_data,
                          by = "timestamp", 
                          all.x = T) %>%
        # mutate(ID = id) %>%
        arrange(ID, timestamp)
      
      ## gap filling (mean of the time for that month and individual)
      
      id_sf_data$hour <- hour(id_sf_data$timestamp)
      
      id_sf_data$gf_variable <- id_sf_data[, variable]
      
      hour_mean <- aggregate(id_sf_data[, variable], 
                             by = list("hour" = id_sf_data$hour), 
                             FUN = mean, 
                             na.rm = T)
      # rename(gf_clean_calibrated_water_content_m3.m3 = x)
      
      names(hour_mean) <- c("hour", "gf_variable")
      
      id_sf_data <- merge(id_sf_data, hour_mean, 
                          by = "hour", 
                          all.x = T)
      
      id_sf_data <- combineData(data = id_sf_data, 
                                variablesToCombine = c("gf_variable"))
      
      names(id_sf_data)[names(id_sf_data) == "gf_variable"] <- paste0("gf_", variable)
      
      gf_data <- rbind(gf_data, id_sf_data)
    }
  }
  
  gf_data <- gf_data %>%
    arrange(ID, timestamp) %>%
    dplyr::select(-month, -hour)
  head(gf_data)
  return(gf_data)
  cat("creating new gap-filled variable: ", paste0("gf_", variable))
}

