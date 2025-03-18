#### METEOROLOGY ###############################################################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")


#### DATA ---------------------------------------------------------------------- ####

## Meteorological and soil data (from daily_sapflow upscaling)

# met_2023 <- read_csv(paste0(data.path, "met/2022-09-18-2023-12-16_daily_met_control_processed.csv"))

met_2023 <- read_csv(paste0(data.path, "met/2023-01-01-2024-12-31_met_control_processed.csv")) %>%
  filter(!is.na(date)) %>%
  mutate(month = as_date(dmy(paste0("01-", month(date, label = F), "-", year(date)))),
         ID = "Control") %>%  # for gap filling purposes
  select(timestamp, ID, date, month, vpd16m_kPa, vpd42m_kPa, precip_mm, t16m_C) %>%
  filter(date > "2023-04-01")  %>%
  filter(date < "2024-05-01")

getQuantile(met_2023$vpd16m_kPa, 0.90)
getQuantile(met_2023$vpd42m_kPa, 0.90)

daily_met_2023 <- aggregate(met_2023[, c(-1, -2)], 
                      by = list("date" = met_2023$date), 
                      FUN = mean, na.rm = T)
head(daily_met_2023)

sum_monthly_met_2023 <- aggregate(met_2023[, c(-1, -2)], 
                              by = list("month" = met_2023$month), 
                              FUN = sum, na.rm = T) %>%
  select(month, precip_mm)

mean_monthly_met_2023 <- aggregate(met_2023[, c(-1, -2)], 
                              by = list("month" = met_2023$month), 
                              FUN = mean, na.rm = T) %>%
  select(month, t16m_C)

monthly_met_2023 <- merge(mean_monthly_met_2023, sum_monthly_met_2023, by = "month")

head(monthly_met_2023)

#### PLOTTING ------------------------------------------------------------------ ####

### met vpd ####

vpd_16m.plot <- ggplot(data = daily_met_2023, 
                       aes(x = date, 
                           y = vpd16m_kPa)) + 
  geom_point(color = "grey", alpha = 0.7) +
  geom_line(color = "grey", alpha = 0.3) +
  geom_smooth(method = "gam", color = "black", se = FALSE) +
  # theme(legend.position = "none") +
  theme_minimal() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylab("Vapor preassure deficit (kPa)") + xlab("")
vpd_16m.plot

temp.plot <- ggplot(data = daily_met_2023, 
                    aes(x = date, 
                        y = t16m_C)) +
  geom_point(color = "grey", alpha = 0.7) +
  geom_line(color = "grey", alpha = 0.3) +
  geom_smooth(method = "gam", color = "black", se = FALSE) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylab("Temperature (ºC)") + xlab("")
temp.plot

precip_mm.plot <- ggplot(data = monthly_met_2023, 
                       aes(x = month, 
                           y = precip_mm)) + 
  geom_col(fill = "grey") +
  # geom_line(color = "black") +
  # geom_smooth(method = "gam", color = "black", se = FALSE) +
  theme(legend.position = "none") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylab("Precipitation (mm)") + xlab("")
precip_mm.plot



climatol.plot <- ggplot(monthly_met_2023, aes(x = month)) +
  geom_bar(aes(y = precip_mm), stat = "identity", fill = "grey", alpha = 0.6) +
  geom_line(aes(y = t16m_C * 10, group = 1), color = "black", size = 1) +  # Scale t16m_C for secondary axis
  geom_point(aes(y = t16m_C * 10), color = "black", size = 2) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(~ . / 10, name = "Temperature (°C)")  # Secondary y-axis
  ) +
  xlab("") +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "black")
  ) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b")
climatol.plot

pdf("results/manuscript/climate.pdf", height = h*2, width = w*2)
ggarrange(climatol.plot, 
          vpd_16m.plot,
          ncol = 1, common.legend = T, legend = "bottom")
dev.off()


### soil WC ####
# 
# rew_50cm.plot <- ggplot(data = met_2023, 
#                        aes(x = date, 
#                            y = rew_50cm_m3_m3,
#                            color = plot)) + 
#   # geom_point() +
#   geom_line() +
#   theme(legend.position = "none") +
#   scale_color_manual(values = c(color_control, color_tfe)) + 
#   theme_minimal() +
#   theme(legend.position = "none") + 
#   ylab("Relative extractable water (REW) at 50cm depth") + xlab("")
# rew_50cm.plot
# 
# rew_250cm.plot <- ggplot(data = met_2023, 
#                         aes(x = date, 
#                             y = rew_250cm_m3_m3,
#                             color = plot)) + 
#   # geom_point() +
#   geom_line() +
#   theme(legend.position = "none") +
#   scale_color_manual(values = c(color_control, color_tfe)) + 
#   theme_minimal() +
#   theme(legend.position = "none") + 
#   ylab("REW at 250cm depth") + xlab("")
# rew_250cm.plot
# 
# rew_400cm.plot <- ggplot(data = met_2023, 
#                          aes(x = date, 
#                              y = rew_400cm_m3_m3,
#                              color = plot)) + 
#   # geom_point() +
#   geom_line() +
#   theme(legend.position = "none") +
#   scale_color_manual(values = c(color_control, color_tfe)) + 
#   theme_minimal() +
#   theme(legend.position = "none") + 
#   ylab("REW at 400cm depth") + xlab("")
# rew_400cm.plot
# 
# pdf("results/manuscript/soil.pdf", height = h*3, width = w)
# ggarrange(rew_50cm.plot, 
#           rew_250cm.plot,
#           rew_400cm.plot,
#           ncol = 1)
# dev.off()
# 
# 
# 
# # 
# # 
# # variables <- names(met_2023)[str_detect(names(met_2023), "absolute")]
# # 
# # met_2023 <- as.data.frame(met_2023)
# # 
# # annual_soilwcToplot_all.df <- data.frame()
# # for(variable in variables){
# #   
# #   depth <- str_remove_all(variable, c("absolute_wc_"))
# #   depth <- str_remove_all(depth, c("_l_m2"))
# #   
# #   annual_soilwcToplot.df <- data.frame(year = met_2023[, "date"],
# #                                        plot = met_2023[, "plot"],
# #                                        depth = depth,
# #                                        "absolute_l_m2" = met_2023[, variable])
# #   
# #   annual_soilwcToplot_all.df <- bind_rows(annual_soilwcToplot_all.df, annual_soilwcToplot.df)
# #   
# # }
# # head(annual_soilwcToplot_all.df)
# # 
# # annual_soilwcToplot_all.df <- annual_soilwcToplot_all.df %>%
# #   filter(depth %in% c("0_400cm", "0_50cm", "250_400cm"))
# # 
# # annual_soilwcToplot_all.df$group_depth <- paste0(annual_soilwcToplot_all.df$plot, "_", annual_soilwcToplot_all.df$depth)
# # 
# # soilwc_absolute.plot <- ggplot(annual_soilwcToplot_all.df, 
# #                                aes(y = absolute_l_m2, x = year, color = plot)) +
# #   geom_point() +
# #   geom_line() +
# #   theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) + 
# #   
# #   scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")) +
# #   # scale_color_manual(values = c(color_control, color_tfe)) + 
# #   ylab("Absolute water content (L/m2)") + xlab("")
# # soilwc_absolute.plot
# # 
# # 
# # 
# # 
# # vpd_16m.plot <- ggplot(data = met_2023, 
# #                        aes(x = date, 
# #                            y = absolute_wc_0_50cm_l_m2,
# #                            color = plot)) + 
# #   # geom_point() +
# #   geom_line() +
# #   # geom_smooth(method = "gam", se = FALSE) +
# #   theme(legend.position = "none") + 
# #   theme_minimal() +
# #   ylab("Vapor preassure deficit (KPa)") + xlab("")
# # vpd_16m.plot
# 
# 
