#### PLOT-LEVEL TIME SERIES ####################################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")

#### DATA ---------------------------------------------------------------------- ####

yearly_biomass_soilwc.df <- read_csv("data_processed/annual_biomass_wc_2000_2023.csv") %>%
  mutate(upper_95ci_growth_dbh_cm_year = mean_growth_dbh_cm_year + (1.96 * se_growth_dbh_cm_year),
         lower_95ci_growth_dbh_cm_year = mean_growth_dbh_cm_year - (1.96 * se_growth_dbh_cm_year)) %>%
  arrange(year)
head(yearly_biomass_soilwc.df)
yearly_biomass_soilwc.df$mortality_rate[c(1, 2)] <- NA
yearly_biomass_soilwc.df$mortality[c(1, 2)] <- NA

tail(yearly_biomass_soilwc.df)

yearly_biomass_soilwc.df$max_absolute_wc_0_400cm_l_m2[yearly_biomass_soilwc.df$year %in% c(2005:2007)] <- NA
yearly_biomass_soilwc.df$wc_per_biomass_mm_MgC[yearly_biomass_soilwc.df$year %in% c(2005:2007)] <- NA
yearly_biomass_soilwc.df$upper_95ci_wc_per_biomass_mm_MgC[yearly_biomass_soilwc.df$year %in% c(2005:2007)] <- NA
yearly_biomass_soilwc.df$lower_95ci_wc_per_biomass_mm_MgC[yearly_biomass_soilwc.df$year %in% c(2005:2007)] <- NA

initial_density_ind_ha_a <- yearly_biomass_soilwc.df %>%
  filter(plot == "Control") %>%
  filter(year == min(year)) %>%
  pull(survival)

initial_density_ind_ha_b <- yearly_biomass_soilwc.df %>%
  filter(plot == "TFE") %>%
  filter(year == min(year)) %>%
  pull(survival)

yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
  mutate(density_deviation_ind_ha = ifelse(plot == "Control", 
                                           survival - initial_density_ind_ha_a,
                                           survival - initial_density_ind_ha_b))

yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
  mutate(phase = ifelse(year < 2017, "transition", "steady_state"),
         plot_phase = paste0(plot, "_", phase))


## ingrid data

yearly_biomass_ingrid.df <- read_csv(paste0(data.path, "biomass/plot_annual_biomass_ingrid_pablo_2005_2023.csv")) %>%
  mutate(date = as_date(dmy(date)),
         year = year(date))

a <- yearly_biomass_ingrid.df %>%
  filter(year %in% c(2002, 2023)) %>%
  filter(plot == "TFE")

a$carbon_biomass_control_MgC_ha[1] - a$carbon_biomass_control_MgC_ha[2]


#### PLOT ---------------------------------------------------------------------- ####

#### soil water content ####

soilwc.plot <- ggplot(yearly_biomass_soilwc.df, 
                      aes(y = max_absolute_wc_0_400cm_l_m2, 
                          x = year, 
                          group = plot, 
                          color = plot)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  # geom_smooth(method = "gam", se = F) +
  theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  ylab("Soil water content (mm)") + xlab("")
soilwc.plot

pdf("results/manuscript/annual_soil_absolute_wc_time_series.pdf", height = h, width = w*2)
soilwc.plot
dev.off()

### FIGURE 1: plot level time series ####

biomass.plot <- ggplot(yearly_biomass_soilwc.df, 
                       aes(y = carbon_biomass_MgC_ha, 
                           x = year,
                           group = plot,
                           # shape = phase,
                           color = plot)) +
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower_95ci_carbon_biomass_MgC_ha, ymax = upper_95ci_carbon_biomass_MgC_ha, fill = plot, color = NULL), alpha = 0.2) + 
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "gam", se = F) +
  theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  scale_fill_manual(values = c(color_control, color_tfe)) +
  ylim(140, 280) +
  ylab("Biomass (Mg C/ha)") + xlab("")
biomass.plot


ingrid_biomass.plot <- ggplot(yearly_biomass_ingrid.df, 
                       aes(y = carbon_biomass_control_MgC_ha, 
                           x = year,
                           group = plot,
                           # shape = phase,
                           color = plot)) +
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  # geom_ribbon(aes(ymin = lower_95ci_carbon_biomass_MgC_ha, ymax = upper_95ci_carbon_biomass_MgC_ha, fill = plot, color = NULL), alpha = 0.2) + 
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "gam", se = F) +
  theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  scale_fill_manual(values = c(color_control, color_tfe)) +
  # ylim(140, 280) +
  ylab("Biomass (Mg C/ha)") + xlab("")
ingrid_biomass.plot

pdf("results/manuscript/biomass_dbh_time_series.pdf", height = h, width = w*2)
ingrid_biomass.plot
dev.off()


# to avoid NA's in the middle giving problems when plotting
toPlotyearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
  filter(!year %in% 2003:2008)

biomass_soil_vwc.plot <- ggplot(toPlotyearly_biomass_soilwc.df, 
                                aes(y = wc_per_biomass_mm_MgC, 
                                    x = year,
                                    group = plot,
                                    # shape = phase,
                                    color = plot)) +
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower_95ci_wc_per_biomass_mm_MgC, ymax = upper_95ci_wc_per_biomass_mm_MgC, fill = plot, color = NULL), alpha = 0.2) +
  geom_line(alpha = 0.3) +
  # geom_errorbar(aes(ymin=lower_95ci_wc_per_biomass_mm_MgC, ymax=upper_95ci_wc_per_biomass_mm_MgC), alpha = 0.3) +
  geom_point(alpha = 0.7) + 
  geom_smooth(method = "gam", se = F) +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank()) +
  scale_color_manual(values = c(color_control, color_tfe)) +
  scale_fill_manual(values = c(color_control, color_tfe)) +
  # ylim(2, 5) +
  ylab("Soil wc / biomass (mm/MgC)") + xlab("")
biomass_soil_vwc.plot


productivity.plot <- ggplot(yearly_biomass_soilwc.df, 
                            aes(y = productivity_MgC_yr_ha, 
                                x = year, 
                                group = plot,
                                # shape = phase,
                                color = plot)) +
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.1, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower_95ci_productivity_MgC_yr_ha, ymax = upper_95ci_productivity_MgC_yr_ha, fill = plot, color = NULL), alpha = 0.2) +
  geom_line(alpha = 0.3) +
  # geom_errorbar(aes(ymin=lower_95ci_productivity_MgC_yr_ha, ymax=upper_95ci_productivity_MgC_yr_ha), alpha = 0.3) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method = "gam", se = F) +
  # geom_line() + 
  theme_minimal() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank()) + 
  ylim(-35, 20) +
  scale_fill_manual(values = c(color_control, color_tfe)) +
  scale_color_manual(values = c(color_control, color_tfe)) + ylab("Productivity (Mg C/ha year)") + xlab("")
productivity.plot

# growth.plot <- ggplot(yearly_biomass_soilwc.df, 
#                       aes(y = mean_growth_dbh_cm_year, 
#                           x = year, 
#                           group = plot, 
#                           color = plot)) +
#   geom_line(alpha = 0.3) +
#   geom_errorbar(aes(ymin=lower_95ci_growth_dbh_cm_year, ymax=upper_95ci_growth_dbh_cm_year), alpha = 0.3) + 
#   geom_point(alpha = 0.7) +
#   geom_smooth(method = "gam", se = F) +
#   # geom_hline(yintercept = 0, linetype="dashed", color = "red", size = 1) +
#   theme_minimal() + 
#   theme(legend.position = "bottom",
#         legend.title = element_blank()) + 
#   scale_color_manual(values = c(color_control, color_tfe)) + ylab("Mean growth (cm/year)") + xlab("")
# growth.plot

# mortality.plot <- ggplot(yearly_biomass_soilwc.df, 
#                             aes(y = emergent_mortality, 
#                                 x = year, 
#                                 group = plot, 
#                                 color = plot)) +
#   geom_line(alpha = 0.3) +
#   geom_point(alpha = 0.7) +
#   geom_smooth(method = "gam", se = F) +
#   # geom_hline(yintercept = 0, linetype="dashed", color = "red", size = 1) +
#   # geom_line() +
#   theme_minimal() + 
#   theme(legend.position = "bottom",
#         legend.title = element_blank()) + 
#   scale_color_manual(values = c(color_control, color_tfe)) + ylab("Mortality (ind/ha)") + xlab("")
# mortality.plot

abundance.plot <- ggplot(yearly_biomass_soilwc.df, 
                         aes(y = mean_growth_dbh_cm_year, 
                             x = year, 
                             group = plot, 
                             color = plot,
                             shape = phase)) +
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "red", size = 0.1, alpha = 0.7) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "gam", se = F) +
  geom_errorbar(aes(ymin=lower_95ci_growth_dbh_cm_year, ymax=upper_95ci_growth_dbh_cm_year), alpha = 0.3) + 
  # geom_hline(yintercept = 0, linetype="dashed", color = "red", size = 1) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) + ylab("Tree density (ind/ha)") + xlab("")
abundance.plot

### Save figure 1 ####

pdf("results/manuscript/plot_time_series.pdf", height = h*2, width = w*2)
ggarrange(
  biomass.plot, 
  space_plot,
  biomass_soil_vwc.plot,
  space_plot,
  productivity.plot, 
  # space_plot,
  # abundance.plot,
          ncol = 1, nrow = 5, legend = "none", align = "hv", heights = c(1, 
                                                                         0.05,
                                                                         1, 
                                                                         0.05,
                                                                         1, 
                                                                         0.05,
                                                                         1))
dev.off()



pdf("results/manuscript/figure_1a.pdf", height = h, width = w*2)
biomass.plot + theme(legend.position = "none")
dev.off()
 
pdf("results/manuscript/figure_1b.pdf", height = h, width = w*2)
biomass_soil_vwc.plot + theme(legend.position = "none")
dev.off()

pdf("results/manuscript/figure_1c.pdf", height = h, width = w*2)
productivity.plot + theme(legend.position = "none")
dev.off()

  

# rowland_2015_yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
#   filter(year < 2015) %>%
#   filter(!year %in% c(2010:2012))
# 
# rowland_2015_biomass.plot <- ggplot(rowland_2015_yearly_biomass_soilwc.df, 
#                        aes(y = carbon_biomass_MgC_ha, 
#                            x = year,
#                            color = plot)) +
#   geom_line(alpha = 0.3) +
#   geom_errorbar(aes(ymin=lower_95ci_carbon_biomass_MgC_ha, ymax=upper_95ci_carbon_biomass_MgC_ha), alpha = 0.3) + 
#   geom_point(alpha = 0.7) +
#   # geom_smooth(method = "gam", se = F) +
#   theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
#   scale_color_manual(values = c(color_control, color_tfe)) +
#   ylab("Biomass (Mg C/ha)") + xlab("")
# rowland_2015_biomass.plot


#### per size class ####

# without error bars
plot.vars <- c("survival", "mortality")

plot.list <- list()
for(plot.var in plot.vars){
  
  # ylab
  if(plot.var == "survival"){
    ylabel <- "Tree density (ind/ha)"
  } else{
    ylabel <- "Mortality (ind/ha)"
  }
  
  all_plot.var <- names(yearly_biomass_soilwc.df)[str_detect(names(yearly_biomass_soilwc.df), plot.var)]
  
  non_emergent.vars <- all_plot.var[str_detect(all_plot.var, "non_emergent")]
  emergent.vars <- all_plot.var[str_detect(all_plot.var, "emergent")]
  emergent.vars <- emergent.vars[!emergent.vars %in%non_emergent.vars]
  
  all.vars <- all_plot.var[!all_plot.var %in% c(emergent.vars, non_emergent.vars)]
  
  
  all_yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
    mutate(depth = "all") %>%
    select(year, plot, depth, all_of(all.vars))
  
  emergent_yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
    mutate(depth = "emergent") %>%
    select(year, plot, depth, all_of(emergent.vars))
  
  non_emergent_yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
    mutate(depth = "non-emergent") %>%
    select(year, plot, depth, all_of(non_emergent.vars))
  
  names(non_emergent_yearly_biomass_soilwc.df) <- str_remove_all(names(non_emergent_yearly_biomass_soilwc.df), "non_emergent_")
  names(emergent_yearly_biomass_soilwc.df) <- str_remove_all(names(emergent_yearly_biomass_soilwc.df), "emergent_") 
  
  by_size_yearly_biomass_soilwc.df <- bind_rows(all_yearly_biomass_soilwc.df, emergent_yearly_biomass_soilwc.df, non_emergent_yearly_biomass_soilwc.df)
  
  by_size_yearly_biomass_soilwc.df$plot_size_class <- paste0(by_size_yearly_biomass_soilwc.df$plot, "_", by_size_yearly_biomass_soilwc.df$depth)
  by_size_yearly_biomass_soilwc.df[, "values"] <- by_size_yearly_biomass_soilwc.df[, plot.var]
  
  plot.list[[plot.var]] <- ggplot(by_size_yearly_biomass_soilwc.df, 
                                  aes(y = values, 
                                      x = year, 
                                      group = plot_size_class, 
                                      color = plot,
                                      shape = depth)) +
    geom_line(alpha = 0.3) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "gam", se = F) +
    theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
    scale_color_manual(values = c(color_control, color_tfe)) +
    ylab("Tree density (ind/ha)") + xlab("")
}
plot.list$survival
plot.list$mortality


# with error bars
plot.var <- c("mean_growth_dbh_cm_year")
ylabel <- "Mean growth (cm/year)"

all_plot.var <- names(yearly_biomass_soilwc.df)[str_detect(names(yearly_biomass_soilwc.df), plot.var)]

non_emergent.vars <- all_plot.var[str_detect(all_plot.var, "non_emergent")]
emergent.vars <- all_plot.var[str_detect(all_plot.var, "emergent")]
emergent.vars <- emergent.vars[!emergent.vars %in%non_emergent.vars]

all.vars <- all_plot.var[!all_plot.var %in% c(emergent.vars, non_emergent.vars)]


# all_yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
#     mutate(depth = "all") %>%
#     select(year, plot, depth, all_of(all.vars))

emergent_yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
  mutate(depth = "emergent") %>%
  select(year, plot, depth, all_of(emergent.vars))

non_emergent_yearly_biomass_soilwc.df <- yearly_biomass_soilwc.df %>%
  mutate(depth = "non-emergent") %>%
  select(year, plot, depth, all_of(non_emergent.vars))

names(non_emergent_yearly_biomass_soilwc.df) <- str_remove_all(names(non_emergent_yearly_biomass_soilwc.df), "non_emergent_")
names(emergent_yearly_biomass_soilwc.df) <- str_remove_all(names(emergent_yearly_biomass_soilwc.df), "emergent_") 

by_size_yearly_biomass_soilwc.df <- bind_rows(emergent_yearly_biomass_soilwc.df, non_emergent_yearly_biomass_soilwc.df)

by_size_yearly_biomass_soilwc.df$plot_size_class <- paste0(by_size_yearly_biomass_soilwc.df$plot, "_", by_size_yearly_biomass_soilwc.df$depth)
by_size_yearly_biomass_soilwc.df[, "values"] <- by_size_yearly_biomass_soilwc.df[, plot.var]

ggplot(by_size_yearly_biomass_soilwc.df, 
       aes(y = values, 
           x = year, 
           group = plot_size_class, 
           color = plot,
           shape = depth)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "gam", se = F) +
  theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  scale_color_manual(values = c(color_control, color_tfe)) +
  ylab(ylabel) + xlab("")


#### Plot water contents different depths ####
# 
# variables <- names(annual_soilwc.df)[str_detect(names(annual_soilwc.df), "absolute")]
# 
# annual_soilwcToplot_all.df <- data.frame()
# for(variable in variables){
#   
#   depth <- str_remove_all(variable, c("absolute_wc_"))
#   depth <- str_remove_all(depth, c("_l_m2"))
#   
#   annual_soilwcToplot.df <- data.frame(year = annual_soilwc.df[, "year"],
#                                        plot = annual_soilwc.df[, "plot"],
#                                        depth = depth,
#                                        "absolute_l_m2" = annual_soilwc.df[, variable])
#   
#   annual_soilwcToplot_all.df <- rbind(annual_soilwcToplot_all.df, annual_soilwcToplot.df)
#   
# }
# 
# annual_soilwcToplot_all.df <- annual_soilwcToplot_all.df %>%
#   filter(depth %in% c("0_400cm", "0_50cm", "250_400cm"))
# 
# annual_soilwcToplot_all.df$group_depth <- paste0(annual_soilwcToplot_all.df$plot, "_", annual_soilwcToplot_all.df$depth)
# 
# soilwc_absolute.plot <- ggplot(annual_soilwcToplot_all.df, 
#                                aes(y = absolute_l_m2, x = year, group = group_depth, color = depth, shape = plot)) +
#   geom_point() +
#   geom_line() +
#   theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
#   
#   scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")) +
#   # scale_color_manual(values = c(color_control, color_tfe)) + 
#   ylab("Absolute water content (mm)") + xlab("")
# soilwc_absolute.plot
# 
# pdf("results/manuscript/soil_absolute_water_content.pdf", height = 3, width = 2)
# soilwc_absolute.plot
# dev.off()


