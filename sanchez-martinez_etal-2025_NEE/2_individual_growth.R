#### GROWTH ###################################################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")

#### DATA ---------------------------------------------------------------------- ####

## plot level

yearly_biomass_soilwc.df <- read_csv("data_processed/annual_biomass_wc_2000_2023.csv") %>%
  mutate(upper_95ci_growth_dbh_cm_year = mean_growth_dbh_cm_year + (1.96 * se_growth_dbh_cm_year),
         lower_95ci_growth_dbh_cm_year = mean_growth_dbh_cm_year - (1.96 * se_growth_dbh_cm_year)) %>%
  arrange(year)
head(yearly_biomass_soilwc.df)
yearly_biomass_soilwc.df$mortality_rate[c(1, 2)] <- NA
yearly_biomass_soilwc.df$mortality[c(1, 2)] <- NA

yearly_biomass_soilwc.df$wc_per_biomass_mm_MgC[yearly_biomass_soilwc.df$year < 2008] <- NA
yearly_biomass_soilwc.df$upper_95ci_wc_per_biomass_mm_MgC[yearly_biomass_soilwc.df$year < 2008] <- NA
yearly_biomass_soilwc.df$lower_95ci_wc_per_biomass_mm_MgC[yearly_biomass_soilwc.df$year < 2008] <- NA

## individual level

# seasonal

dbh_2005_2023 <- read_csv(paste0(root.dir, "data_processed/biomass/dbh_2005_2023.csv")) %>%
  mutate(growth_cm_day = NA,
         genus = str_split_fixed(species, " ", 2)[, 1],
         ID = paste0(plot, "_", tree_number)) %>%
  select(ID, plot, date, year, month, season, species, genus, clean_dbh_cm, gf2_clean_dbh_cm, growth_cm_day)

# annual

wd_gbm.data_6 <- read_csv("data_processed/individual_annual_biomass_wc_2000_2023.csv")

dbh_2005_2023 <- merge(dbh_2005_2023, wd_gbm.data_6, by = c("ID", "year"), all.x = T) %>%
  arrange(year, ID)


yearly_biomass_soilwc.df$mean_biomass_Mg <- yearly_biomass_soilwc.df$mean_biomass_kg / 1000

yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
  mutate(phase = ifelse(year < 2017, "transition", "steady_state"),
         plot_phase = paste0(plot, "_", phase))

## for segments
yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
  arrange(plot, year)

segmments.data <- data.frame(
  
  ## figure 2
  x_start = yearly_biomass_soilwc.df$survival,
  x_end = c(yearly_biomass_soilwc.df$survival[-1], tail(yearly_biomass_soilwc.df$survival, 1)),
  y_start = yearly_biomass_soilwc.df$carbon_biomass_MgC_ha,
  y_end = c(yearly_biomass_soilwc.df$carbon_biomass_MgC_ha[-1], tail(yearly_biomass_soilwc.df$carbon_biomass_MgC_ha, 1)),
  
  e_x_start = yearly_biomass_soilwc.df$emergent_survival,
  e_x_end = c(yearly_biomass_soilwc.df$emergent_survival[-1], tail(yearly_biomass_soilwc.df$emergent_survival, 1)),
  
  ne_x_start = yearly_biomass_soilwc.df$non_emergent_survival,
  ne_x_end = c(yearly_biomass_soilwc.df$non_emergent_survival[-1], tail(yearly_biomass_soilwc.df$non_emergent_survival, 1)),
  
  mb_y_start = yearly_biomass_soilwc.df$mean_biomass_Mg,
  mb_y_end = c(yearly_biomass_soilwc.df$mean_biomass_Mg[-1], tail(yearly_biomass_soilwc.df$mean_biomass_Mg, 1)),
  
  ## FIGURE 3
  
  wc_gr_x_start = yearly_biomass_soilwc.df$wc_per_biomass_mm_MgC,
  wc_gr_x_end = c(yearly_biomass_soilwc.df$wc_per_biomass_mm_MgC[-1], tail(yearly_biomass_soilwc.df$wc_per_biomass_mm_MgC, 1)),
  wc_gr_y_start = yearly_biomass_soilwc.df$mean_growth_dbh_cm_year,
  wc_gr_y_end = c(yearly_biomass_soilwc.df$mean_growth_dbh_cm_year[-1], tail(yearly_biomass_soilwc.df$mean_growth_dbh_cm_year, 1)),
  
  wc_gr_e_y_start = yearly_biomass_soilwc.df$emergent_mean_growth_dbh_cm_year,
  wc_gr_e_y_end = c(yearly_biomass_soilwc.df$emergent_mean_growth_dbh_cm_year[-1], tail(yearly_biomass_soilwc.df$emergent_mean_growth_dbh_cm_year, 1)),
  
  wc_gr_ne_y_start = yearly_biomass_soilwc.df$non_emergent_mean_growth_dbh_cm_year,
  wc_gr_ne_y_end = c(yearly_biomass_soilwc.df$non_emergent_mean_growth_dbh_cm_year[-1], tail(yearly_biomass_soilwc.df$non_emergent_mean_growth_dbh_cm_year, 1))
  )

yearly_biomass_soilwc.df <- bind_cols(yearly_biomass_soilwc.df, segmments.data)

## plot specific models

a_yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
  filter(plot == "Control")

b_yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
  filter(plot == "TFE")


### ANNUAL GROWTH AT INDIVIDUAL LEVEL YEAR BY YEAR ----------------------------- ####

### all ####

ind_growth_annual_plot.list <- list()
annual_coefs.df <- data.frame()
for(m in sort(unique(wd_gbm.data_6$year))){
  
  # Filter by month
  
  annual_dbh_growth.df_month <- wd_gbm.data_6 %>%
    filter(year == m)
  
  if(all(is.na(annual_dbh_growth.df_month))){
    next()
  }
  
  if(length(unique(annual_dbh_growth.df_month$plot)) < 2){
    next()
  }
  
  if(sum(annual_dbh_growth.df_month$growth_dbh_cm_year, na.rm = T) == 0){
    next()
  }
  
  growth_plot_month.mmod <- lm(growth_dbh_cm_year ~ plot,
                                 data = annual_dbh_growth.df_month)
  
  summary(growth_plot_month.mmod)
  ind_growth_annual_plot.list[[m]] <- ggplot(annual_dbh_growth.df_month, 
                                             aes(y = growth_dbh_cm_year, 
                                                 x = clean_dbh_cm, 
                                                 # group = plot,
                                                 color = plot)) +
    geom_point() +
    geom_smooth(method = "lm", se = T) +
    theme_minimal() + 
    theme(legend.position = "bottom", legend.title = element_blank()) + 
    scale_color_manual(values = c(color_control, color_tfe)) + 
    ylab("Growth (cm/day)") + ylim(-0.05, 0.05)
  
  coef_annualdf <- as.data.frame(coef(summary(growth_plot_month.mmod)))[1:2, ]
  coef_annualdf[3, ] <- coef_annualdf[2, ] 
  coef_annualdf$Estimate[2] <- coef_annualdf$Estimate[1] + coef_annualdf$Estimate[2] 
  coef_annualdf$plot[1] <- "Control"
  coef_annualdf$plot[2] <- "TFE"
  coef_annualdf$plot[3] <- "Difference"
  
  coef_annualdf$year <- unique(annual_dbh_growth.df_month$year)
  annual_coefs.df <- rbind(annual_coefs.df, coef_annualdf)
}
# coefs.df$date <- as_date(coefs.df$date)
rownames(annual_coefs.df) <- NULL
head(annual_coefs.df)

annual_coefs.df <- annual_coefs.df %>%
  rename(pvalue = "Pr(>|t|)") %>%
  mutate(upper_95CI =  Estimate + (1.96 * `Std. Error`),
         lower_95CI =  Estimate - (1.96 * `Std. Error`),
         p = round(pvalue, 3),
         sign = ifelse(pvalue < 0.05, "s", "ns"))

diff_annual_coefs.df <- annual_coefs.df %>%
  filter(plot == "Difference")

first_empty_year <- data.frame("year" = 2002)

diff_annual_coefs.df <- bind_rows(diff_annual_coefs.df, first_empty_year) %>%
  arrange(year)

all_annual_growth_difference <- ggplot(diff_annual_coefs.df, 
                  aes(x = year, y = Estimate, group = plot, shape = sign)) + 
  geom_line(alpha = 0.3) +
  geom_errorbar(aes(ymin = lower_95CI, ymax =upper_95CI), alpha = 0.3) + 
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 'dotted', col = 'red') + 
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  ylim(-0.75, 0.5) +
  # scale_color_manual(values = c(color_control, color_tfe)) + 
  ylab("plot effect on growth (cm/year)") + xlab("")
all_annual_growth_difference

### non-emergent ####

ne_wd_gbm.data_6 <- wd_gbm.data_6 %>%
  filter(size_class == "non-emergent")

ne_ind_growth_annual_plot.list <- list()
ne_annual_coefs.df <- data.frame()
for(m in sort(unique(ne_wd_gbm.data_6$year))){
  
  # Filter by month
  
  annual_dbh_growth.df_month <- ne_wd_gbm.data_6 %>%
    filter(year == m)
  
  if(all(is.na(annual_dbh_growth.df_month))){
    next()
  }
  
  if(length(unique(annual_dbh_growth.df_month$plot)) < 2){
    next()
  }
  
  if(sum(annual_dbh_growth.df_month$growth_dbh_cm_year, na.rm = T) == 0){
    next()
  }
  
  growth_plot_month.mmod <- lm(growth_dbh_cm_year ~ plot,
                               data = annual_dbh_growth.df_month)
  
  summary(growth_plot_month.mmod)
  ne_ind_growth_annual_plot.list[[m]] <- ggplot(annual_dbh_growth.df_month, 
                                             aes(y = growth_dbh_cm_year, 
                                                 x = clean_dbh_cm, 
                                                 # group = plot,
                                                 color = plot)) +
    geom_point() +
    geom_smooth(method = "lm", se = T) +
    theme_minimal() + 
    theme(legend.position = "bottom", legend.title = element_blank()) + 
    scale_color_manual(values = c(color_control, color_tfe)) + 
    ylab("Growth (cm/day)") + ylim(-0.05, 0.05)
  
  coef_annualdf <- as.data.frame(coef(summary(growth_plot_month.mmod)))[1:2, ]
  coef_annualdf[3, ] <- coef_annualdf[2, ] 
  coef_annualdf$Estimate[2] <- coef_annualdf$Estimate[1] + coef_annualdf$Estimate[2] 
  coef_annualdf$plot[1] <- "Control"
  coef_annualdf$plot[2] <- "TFE"
  coef_annualdf$plot[3] <- "Difference"
  
  coef_annualdf$year <- unique(annual_dbh_growth.df_month$year)
  ne_annual_coefs.df <- rbind(ne_annual_coefs.df, coef_annualdf)
}

rownames(ne_annual_coefs.df) <- NULL
head(ne_annual_coefs.df)

ne_annual_coefs.df <- ne_annual_coefs.df %>%
  rename(pvalue = "Pr(>|t|)") %>%
  mutate(upper_95CI =  Estimate + (1.96 * `Std. Error`),
         lower_95CI =  Estimate - (1.96 * `Std. Error`),
    p = round(pvalue, 3),
         sign = ifelse(pvalue < 0.05, "s", "ns"))  %>%
  mutate(phase = ifelse(year < 2017, "transition", "steady_state"),
         plot_phase = paste0(plot, "_", phase))

diff_ne_annual_coefs.df <- ne_annual_coefs.df %>%
  filter(plot == "Difference")

ne_annual_growth_difference <- ggplot(diff_ne_annual_coefs.df, 
       aes(x = year, 
           y = Estimate,
           group = plot, 
           shape = phase, 
           color = sign)) + 
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  geom_line(alpha = 0.3) +
  geom_errorbar(aes(ymin = lower_95CI, ymax =upper_95CI), alpha = 0.3) + 
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 'dotted', col = 'black') + 
  geom_smooth(method = "lm", se = F, color = "black") +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  ylim(-0.75, 0.5) +
  scale_color_manual(values = c("grey", "black")) +
  ylab("Plot effect on growth (cm/year)") + xlab("")
ne_annual_growth_difference

ne_annual_coefs.df <- ne_annual_coefs.df %>%
  filter(plot != "Difference")

ne_annual_growth <- ggplot(ne_annual_coefs.df, 
                                      aes(x = year, 
                                          y = Estimate, 
                                          group = plot, 
                                          color = plot,
                                          shape = phase)) + 
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  geom_line(alpha = 0.3) +
  geom_errorbar(aes(ymin = lower_95CI, ymax =upper_95CI), alpha = 0.3) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  ylim(-0, 0.8) +
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylab("Growth (cm/year)") + xlab("")
ne_annual_growth


### emergent ####

e_wd_gbm.data_6 <- wd_gbm.data_6 %>%
  filter(size_class == "emergent")

e_ind_growth_annual_plot.list <- list()
e_annual_coefs.df <- data.frame()
for(m in sort(unique(e_wd_gbm.data_6$year))){
  
  # Filter by month
  
  annual_dbh_growth.df_month <- e_wd_gbm.data_6 %>%
    filter(year == m)
  
  if(all(is.na(annual_dbh_growth.df_month))){
    next()
  }
  
  if(length(unique(annual_dbh_growth.df_month$plot)) < 2){
    next()
  }
  
  if(sum(annual_dbh_growth.df_month$growth_dbh_cm_year, na.rm = T) == 0){
    next()
  }
  
  growth_plot_month.mmod <- lm(growth_dbh_cm_year ~ plot,
                               data = annual_dbh_growth.df_month)
  
  summary(growth_plot_month.mmod)
  e_ind_growth_annual_plot.list[[m]] <- ggplot(annual_dbh_growth.df_month, 
                                                aes(y = growth_dbh_cm_year, 
                                                    x = clean_dbh_cm, 
                                                    # group = plot,
                                                    color = plot)) +
    geom_point() +
    geom_smooth(method = "lm", se = T) +
    theme_minimal() + 
    theme(legend.position = "bottom", legend.title = element_blank()) + 
    scale_color_manual(values = c(color_control, color_tfe)) + 
    ylab("Growth (cm/day)") + ylim(-0.05, 0.05)
  
  coef_annualdf <- as.data.frame(coef(summary(growth_plot_month.mmod)))[1:2, ]
  coef_annualdf[3, ] <- coef_annualdf[2, ] 
  coef_annualdf$Estimate[2] <- coef_annualdf$Estimate[1] + coef_annualdf$Estimate[2] 
  coef_annualdf$plot[1] <- "Control"
  coef_annualdf$plot[2] <- "TFE"
  coef_annualdf$plot[3] <- "Difference"
  
  coef_annualdf$year <- unique(annual_dbh_growth.df_month$year)
  e_annual_coefs.df <- rbind(e_annual_coefs.df, coef_annualdf)
}

rownames(e_annual_coefs.df) <- NULL
head(e_annual_coefs.df)

e_annual_coefs.df <- e_annual_coefs.df %>%
  rename(pvalue = "Pr(>|t|)") %>%
  mutate(upper_95CI =  Estimate + (1.96 * `Std. Error`),
         lower_95CI =  Estimate - (1.96 * `Std. Error`),
         p = round(pvalue, 3),
         sign = ifelse(pvalue < 0.05, "s", "ns")) %>%
  mutate(phase = ifelse(year < 2017, "transition", "steady_state"),
         plot_phase = paste0(plot, "_", phase))

diff_e_annual_coefs.df <- e_annual_coefs.df %>%
  filter(plot == "Difference")

e_annual_growth_difference <- ggplot(diff_e_annual_coefs.df, 
       aes(x = year, 
           y = Estimate, 
           group = plot, 
           color = sign, 
           shape = phase)) +
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  geom_line(alpha = 0.3) +
  geom_errorbar(aes(ymin = lower_95CI, ymax =upper_95CI), alpha = 0.3) + 
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 'dotted', col = 'black') + 
  geom_smooth(method = "lm", se = F, color = "black") +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  ylim(-0.75, 0.5) +
  scale_color_manual(values = c("grey", "black")) +
  ylab("Plot effect on growth (cm/year)") + xlab("")
e_annual_growth_difference

e_annual_coefs.df <- e_annual_coefs.df %>%
  filter(plot != "Difference")

e_annual_growth <- ggplot(e_annual_coefs.df, 
                           aes(x = year, 
                               y = Estimate, 
                               group = plot, 
                               color = plot, 
                               shape = phase)) + 
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  geom_line(alpha = 0.3) +
  geom_errorbar(aes(ymin = lower_95CI, ymax =upper_95CI), alpha = 0.3) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  ylim(-0, 0.8) +
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylab("Growth (cm/year)") + xlab("")
e_annual_growth


#### LINEAR MODELS (PLOT LEVEL) ------------------------------------------------ ####

### abundance per size class ####

# difference_biomass <- data.frame("year" = a_yearly_biomass_soilwc.df$year,
#                                  "difference_survival" = a_yearly_biomass_soilwc.df$survival - b_yearly_biomass_soilwc.df$survival,
#                                  "difference_emergent_survival" = a_yearly_biomass_soilwc.df$emergent_survival - b_yearly_biomass_soilwc.df$emergent_survival,
#                                  "difference_non_emergent_survival" = a_yearly_biomass_soilwc.df$non_emergent_survival - b_yearly_biomass_soilwc.df$non_emergent_survival
#                                  )

abundance.plot <- ggplot(yearly_biomass_soilwc.df, 
                                 aes(x = year, 
                                     y = survival, 
                                     # group = plot,
                                     color = plot
                                 )) + 
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "gam", se = F) +
  theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  ylab("Tree density (ind/ha)") + xlab("") 
abundance.plot

e_abundance.plot <- ggplot(yearly_biomass_soilwc.df, 
                         aes(x = year, 
                             y = emergent_survival, 
                             group = plot,
                             color = plot,
                             shape = phase
                         )) + 
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  ylim(50, 410) +
  geom_smooth(method = "lm", se = F) +
  theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  ylab("Emergent tree density (ind/ha)") + xlab("") 
e_abundance.plot

ne_abundance.plot <- ggplot(yearly_biomass_soilwc.df, 
                         aes(x = year, 
                             y = non_emergent_survival, 
                             group = plot,
                             color = plot,
                             shape = phase
                         )) + 
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +
  ylim(50, 410) +
  theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  ylab("Non-emergent tree density (ind/ha)") + xlab("") 
ne_abundance.plot


### biomas vs abundance ####

## biomass vs. abundance

abundance_biomass.lm <- lm(log(carbon_biomass_MgC_ha) ~ year + plot * survival, 
                           data = yearly_biomass_soilwc.df)
summary(abundance_biomass.lm)

# control

abundance_biomass.lm <- lm(log(carbon_biomass_MgC_ha) ~ survival, 
                           data = a_yearly_biomass_soilwc.df)
summary(abundance_biomass.lm)

# tfe

abundance_biomass.lm <- lm(carbon_biomass_MgC_ha ~ survival, 
                           data = b_yearly_biomass_soilwc.df)
summary(abundance_biomass.lm)

## plot

abundance_biomass.plot <- ggplot(yearly_biomass_soilwc.df, 
                                 aes(x = survival, 
                                     y = carbon_biomass_MgC_ha, 
                                     group = plot,
                                     color = plot, 
                                     shape = phase
                                     )) + 
  geom_point(alpha = 0.7) +
  geom_text(
    label= yearly_biomass_soilwc.df$year, 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  ) +
  geom_segment(aes(x = x_start, xend = x_end,
                   y = y_start, yend = y_end, 
                   color = plot), alpha = 0.3) +
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylab("Biomass (MgC/ha)") + xlab("Tree density (ind/ha)") 
abundance_biomass.plot


## biomass vs. emergent abundance

e_abundance_biomass.lm <- lm(log(carbon_biomass_MgC_ha) ~ year + plot * emergent_survival, 
                             data = yearly_biomass_soilwc.df)
summary(e_abundance_biomass.lm)

# control
abundance_biomass.lm <- lm(log(carbon_biomass_MgC_ha) ~ emergent_survival, 
                           data = a_yearly_biomass_soilwc.df)
summary(abundance_biomass.lm)

# tfe
abundance_biomass.lm <- lm(log(carbon_biomass_MgC_ha) ~ emergent_survival, 
                           data = b_yearly_biomass_soilwc.df)
summary(abundance_biomass.lm)


e_abundance_biomass.plot <- ggplot(yearly_biomass_soilwc.df, 
                                   aes(x = emergent_survival, 
                                       y = carbon_biomass_MgC_ha, 
                                       group = plot,
                                       color = plot, 
                                       shape = phase
                                   )) + 
  geom_point(alpha = 0.7) +
  geom_text(
    label= yearly_biomass_soilwc.df$year, 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  ) +
  geom_segment(aes(x = e_x_start, xend = e_x_end,
                   y = y_start, yend = y_end, 
                   color = plot), alpha = 0.3, 
               # arrow = arrow(length = unit(0.05, "cm"))
               ) +
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylab("Biomass (MgC/ha)") + xlab("Tree density (ind/ha)")  
e_abundance_biomass.plot


## biomass vs. non-emergent abundance

ne_abundance_biomass.lm <- lm(log(carbon_biomass_MgC_ha) ~ year + plot * non_emergent_survival, 
                              data = yearly_biomass_soilwc.df)
summary(ne_abundance_biomass.lm)

# control
abundance_biomass.lm <- lm(log(carbon_biomass_MgC_ha) ~ non_emergent_survival, 
                           data = a_yearly_biomass_soilwc.df)
summary(abundance_biomass.lm)

# tfe
abundance_biomass.lm <- lm(log(carbon_biomass_MgC_ha) ~ non_emergent_survival, 
                           data = b_yearly_biomass_soilwc.df)
summary(abundance_biomass.lm)

ne_abundance_biomass.plot <- ggplot(yearly_biomass_soilwc.df, 
                                    aes(x = non_emergent_survival, 
                                        y = carbon_biomass_MgC_ha, 
                                        group = plot,
                                        color = plot, 
                                        shape = phase
                                    )) + 
  geom_point(alpha = 0.7) +
  geom_text(
    label= yearly_biomass_soilwc.df$year, 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  ) +
  geom_segment(aes(x = ne_x_start, xend = ne_x_end,
                   y = y_start, yend = y_end, 
                   color = plot), alpha = 0.3) +
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylab("Biomass (MgC/ha)") + xlab("Tree density (ind/ha)")  
ne_abundance_biomass.plot

### mean biomas vs abundance ####

## mean biomass vs. size

abundance_mean_biomass.lm <- lm(log(mean_biomass_Mg) ~ year + plot * survival, 
                           data = yearly_biomass_soilwc.df)
summary(abundance_mean_biomass.lm)

# control
abundance_mean_biomass.lm <- lm(log(mean_biomass_Mg) ~ survival, 
                           data = a_yearly_biomass_soilwc.df)
summary(abundance_mean_biomass.lm)

# tfe
abundance_mean_biomass.lm <- lm(mean_biomass_Mg ~ survival, 
                           data = b_yearly_biomass_soilwc.df)
summary(abundance_mean_biomass.lm)


abundance_mean_biomass.plot <- ggplot(yearly_biomass_soilwc.df, 
                                 aes(x = survival, 
                                     y = mean_biomass_Mg, 
                                     group = plot,
                                     color = plot, 
                                     shape = phase
                                 )) + 
  geom_point(alpha = 0.7) +
  geom_text(
    label= yearly_biomass_soilwc.df$year, 
    # nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  ) +
  geom_segment(aes(x = x_start, xend = x_end,
                   y = mb_y_start, yend = mb_y_end, 
                   color = plot), alpha = 0.3) +
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylab("Biomass (MgC/ha)") + xlab("Tree density (ind/ha)")  
abundance_mean_biomass.plot


## mean_biomass vs. emergent abundance

e_abundance_mean_biomass.lm <- lm(log(mean_biomass_Mg) ~ year + plot * emergent_survival, 
                             data = yearly_biomass_soilwc.df)
summary(e_abundance_mean_biomass.lm)

# control
abundance_mean_biomass.lm <- lm(log(mean_biomass_Mg) ~ emergent_survival, 
                           data = a_yearly_biomass_soilwc.df)
summary(abundance_mean_biomass.lm)

# tfe
abundance_mean_biomass.lm <- lm(log(mean_biomass_Mg) ~ emergent_survival, 
                           data = b_yearly_biomass_soilwc.df)
summary(abundance_mean_biomass.lm)


e_abundance_mean_biomass.plot <- ggplot(yearly_biomass_soilwc.df, 
                                   aes(x = emergent_survival, 
                                       y = mean_biomass_Mg, 
                                       group = plot,
                                       color = plot, 
                                       shape = phase
                                   )) + 
  geom_point(alpha = 0.7) +
  geom_text(
    label= yearly_biomass_soilwc.df$year, 
    # nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  ) +
  geom_segment(aes(x = e_x_start, xend = e_x_end,
                   y = mb_y_start, yend = mb_y_end, 
                   color = plot), alpha = 0.3) +
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylab("Biomass (MgC/ha)") + xlab("Tree density (ind/ha)")  
e_abundance_biomass.plot


## mean_biomass vs. non-emergent abundance

ne_abundance_mean_biomass.lm <- lm(log(mean_biomass_Mg) ~ year + plot * non_emergent_survival, 
                              data = yearly_biomass_soilwc.df)
summary(ne_abundance_mean_biomass.lm)

# control
abundance_mean_biomass.lm <- lm(log(mean_biomass_Mg) ~ non_emergent_survival, 
                           data = a_yearly_biomass_soilwc.df)
summary(abundance_mean_biomass.lm)

# tfe
abundance_mean_biomass.lm <- lm(log(mean_biomass_Mg) ~ non_emergent_survival, 
                           data = b_yearly_biomass_soilwc.df)
summary(abundance_mean_biomass.lm)

ne_abundance_mean_biomass.plot <- ggplot(yearly_biomass_soilwc.df, 
                                    aes(x = non_emergent_survival, 
                                        y = mean_biomass_Mg, 
                                        group = plot,
                                        color = plot, 
                                        shape = phase
                                    )) + 
  geom_point(alpha = 0.7) +
  geom_text(
    label= yearly_biomass_soilwc.df$year, 
    # nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  ) +
  geom_segment(aes(x = ne_x_start, xend = ne_x_end,
                   y = mb_y_start, yend = mb_y_end, 
                   color = plot), alpha = 0.3) +
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylab("Biomass (MgC/ha)") + xlab("Tree density (ind/ha)")  
ne_abundance_mean_biomass.plot


### growth vs wc/biomass ####

## growth vs wc/biomass

growth_waterPerBiomass.lm <- lm(log(mean_growth_dbh_cm_year) ~ year + plot * wc_per_biomass_mm_MgC, 
                                data = yearly_biomass_soilwc.df)
summary(growth_waterPerBiomass.lm)

# control
a_growth_waterPerBiomass.lm <- lm(log(mean_growth_dbh_cm_year) ~ wc_per_biomass_mm_MgC, 
                           data = a_yearly_biomass_soilwc.df)
summary(a_growth_waterPerBiomass.lm)

# tfe
b_growth_waterPerBiomass.lm <- lm(log(mean_growth_dbh_cm_year) ~ wc_per_biomass_mm_MgC, 
                           data = b_yearly_biomass_soilwc.df)
summary(b_growth_waterPerBiomass.lm)

growth_waterPerBiomass.plot <- ggplot(yearly_biomass_soilwc.df, 
                                      aes(x = wc_per_biomass_mm_MgC, 
                                          y = mean_growth_dbh_cm_year, 
                                          color = plot, 
                                          shape = phase)) + 
  geom_point(alpha = 0.7) +
  geom_text(
    label= yearly_biomass_soilwc.df$year, 
    check_overlap = T
  ) +
  geom_segment(aes(x = wc_gr_x_start, xend = wc_gr_x_end,
                   y = wc_gr_y_start, yend = wc_gr_y_end, 
                   color = plot), alpha = 0.3, 
               # arrow = arrow(length = unit(0.05, "cm"))
  ) +
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylim(0, 1) +
  ylab("Mean growth (cm/year)") + xlab("Water content per unit biomass (mm/MgC)") 
growth_waterPerBiomass.plot

## emergent growth vs wc/biomass

e_growth_waterPerBiomass.lm <- lm(emergent_mean_growth_dbh_cm_year ~ year + plot * wc_per_biomass_mm_MgC, 
                                  data = yearly_biomass_soilwc.df)
summary(e_growth_waterPerBiomass.lm)

# control
a_growth_waterPerBiomass.lm <- lm(log(emergent_mean_growth_dbh_cm_year) ~ wc_per_biomass_mm_MgC, 
                                  data = a_yearly_biomass_soilwc.df)
summary(a_growth_waterPerBiomass.lm)

# tfe

b_growth_waterPerBiomass.lm <- lm(log(emergent_mean_growth_dbh_cm_year) ~ wc_per_biomass_mm_MgC, 
                                  data = b_yearly_biomass_soilwc.df)
summary(b_growth_waterPerBiomass.lm)

e_growth_waterPerBiomass.plot <- ggplot(yearly_biomass_soilwc.df, 
                                        aes(x = wc_per_biomass_mm_MgC, 
                                            y = emergent_mean_growth_dbh_cm_year, 
                                            color = plot, 
                                            group = plot, 
                                            shape = phase)) + 
  geom_point(alpha = 0.7) +
  geom_text(
    label= yearly_biomass_soilwc.df$year, 
    check_overlap = T
  ) +
  geom_segment(aes(x = wc_gr_x_start, xend = wc_gr_x_end,
                   y = wc_gr_e_y_start, yend = wc_gr_e_y_end, 
                   color = plot), alpha = 0.3, 
               # arrow = arrow(length = unit(0.05, "cm"))
  ) +
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylim(0, 1.3) +
  ylab("Mean growth (cm/year)") + xlab("Water content per unit biomass (mm/MgC)") 
e_growth_waterPerBiomass.plot

## non emergent growth vs wc/biomass

ne_growth_waterPerBiomass.lm <- lm(log(non_emergent_mean_growth_dbh_cm_year) ~ year + plot * wc_per_biomass_mm_MgC, 
                                   data = yearly_biomass_soilwc.df)
summary(ne_growth_waterPerBiomass.lm)

# control
a_growth_waterPerBiomass.lm <- lm(log(non_emergent_mean_growth_dbh_cm_year) ~ wc_per_biomass_mm_MgC, 
                                  data = a_yearly_biomass_soilwc.df)
summary(a_growth_waterPerBiomass.lm)

# tfe
b_growth_waterPerBiomass.lm <- lm(log(non_emergent_mean_growth_dbh_cm_year) ~ wc_per_biomass_mm_MgC, 
                                  data = b_yearly_biomass_soilwc.df)
summary(b_growth_waterPerBiomass.lm)


ne_growth_waterPerBiomass.plot <- ggplot(yearly_biomass_soilwc.df, 
                                         aes(x = wc_per_biomass_mm_MgC, 
                                             y = non_emergent_mean_growth_dbh_cm_year, 
                                             color = plot, 
                                             group = plot, 
                                             shape = phase)) + 
  geom_point(alpha = 0.7) +
  geom_text(
    label= yearly_biomass_soilwc.df$year, 
    check_overlap = T
  ) +
  geom_segment(aes(x = wc_gr_x_start, xend = wc_gr_x_end,
                   y = wc_gr_ne_y_start, yend = wc_gr_ne_y_end, 
                   color = plot), alpha = 0.3, 
               # arrow = arrow(length = unit(0.05, "cm"))
  ) +
  geom_smooth(method = "lm", se = F) +   
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylim(0, 1.3) +
  ylab("Mean growth (cm/year") + xlab("Water content per unit biomass (mm/MgC)") 
ne_growth_waterPerBiomass.plot


#### FIGURE 2: DENSITY VS BIOMASS ---------------------------------------------- ####

pdf("results/manuscript/biomass_tree_density.pdf", height = h * 3, width = w*2)
ggarrange(e_abundance.plot, ne_abundance.plot,
          space_plot, space_plot,
          e_abundance_biomass.plot, ne_abundance_biomass.plot,
          space_plot, space_plot,
          e_abundance_mean_biomass.plot, ne_abundance_mean_biomass.plot,
          ncol = 2, nrow = 5,
          labels = c("Emergent", "Non-emegent"), 
          common.legend = T, legend = "none",
          heights = c(1, 
                      0.05,
                      1, 
                      0.05,
                      1))
dev.off()


#### FIGURE 3: GROWTH ---------------------------------------------------------- ####

pdf("results/manuscript/individual_growth_trend.pdf", height = h*3, width = w*2)
ggarrange(
  e_annual_growth, ne_annual_growth,
  space_plot, space_plot,
  e_annual_growth_difference, ne_annual_growth_difference,
  space_plot, space_plot,
  e_growth_waterPerBiomass.plot, ne_growth_waterPerBiomass.plot,
  ncol = 2, nrow = 5,
  labels = c("Emergent", "Non-emegent"), 
  common.legend = T, legend = "none",
  heights = c(1, 
              0.05,
              1, 
              0.05,
              1))
dev.off()


# ## plot
# 
# pdf("results/manuscript/growth_water_per_biomass.pdf", height = h*3, width = w*2)
# ggarrange(
#   # biomass_soil_vwc.plot,
#   e_annual_growth, ne_annual_growth,
#   # all_annual_growth_difference, 
#   e_annual_growth_difference, ne_annual_growth_difference,
#   e_growth_waterPerBiomass.plot,
#   ne_growth_waterPerBiomass.plot,
#   ncol = 2, nrow = 3,
#   labels = c("Emergent", "Non-emegent", "Emergent", "Non-emegent"), 
#   common.legend = T, legend = "none")
# dev.off()