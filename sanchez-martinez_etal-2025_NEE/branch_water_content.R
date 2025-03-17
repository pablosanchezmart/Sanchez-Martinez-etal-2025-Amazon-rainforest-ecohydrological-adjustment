#### WOOD WATER CONTENT DIFFERENCES BETWEEN PLOTS ##############################

## Pablo Sanchez Martinez
## 09/2023

source("initialization.R")

#### LOAD DATA ----------------------------------------------------------------- ####

## Branch

branch_wc_10_2023.data <- read_csv(file = "C:/Users/psanche2/OneDrive - University of Edinburgh/postdoc_UoE/data_processed/caxuana_branch_water_content/Branch water content_1.csv") %>%
  mutate(plot = ifelse(str_detect(`Tree ID`, "A"), "Control", "TFE"),
         id = paste0(plot, "_", parse_number(`Tree ID`))) %>%
  select(id, plot, DT, `VWC sw`, `sw density`, `PD or MD`)

names(branch_wc_10_2023.data) <- str_replace_all(names(branch_wc_10_2023.data), " ", "_")


## predawn and midday identification

pd_branch_wc_10_2023.data <- branch_wc_10_2023.data %>%
  filter(PD_or_MD == "PD") %>%
  select(id, plot, VWC_sw_pd = VWC_sw)

md_branch_wc_10_2023.data <- branch_wc_10_2023.data %>%
  filter(PD_or_MD == "MD") %>%
  select(-PD_or_MD, DT, sw_density) %>%
  select(id, VWC_sw_md = VWC_sw, sw_density)

branch_wc_10_2023.data <- merge(pd_branch_wc_10_2023.data, 
                                md_branch_wc_10_2023.data,
                                by = "id", 
                                all.x = T)

# metadata (taxonomy)

metadata <- readxl::read_excel(paste0(data.path, "metadata_cax_radar/cax_radar_metadata_caxiuana_10_2023.xlsx"), sheet = 1) %>%
  select(id = ID, genus)


branch_wc_10_2023.data <- merge(branch_wc_10_2023.data, metadata, 
                                  by = "id", 
                                  all.x = T)

#### MODELS AND PLOTS DRY SEASON ----------------------------------------------- ####

### Predawn branch water content ####

pd_branch_wc_10_2023.lm <- lm(VWC_sw_pd ~ plot, 
                    data = branch_wc_10_2023.data)
summary(pd_branch_wc_10_2023.lm)

pd_branch_wc_10_2023.lmer <- lmer(VWC_sw_pd ~ plot + (1|genus/id), 
                              data = branch_wc_10_2023.data)
summary(pd_branch_wc_10_2023.lmer)

pdf("results/pd_branch_water_content.pdf", 
    height = h, 
    width = w)
pd_branch_wc_md.plot <- ggplot(data = branch_wc_10_2023.data, 
                          aes(x = plot,
                              y = VWC_sw_pd,
                              color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Branch volumetric water content at predawn") + 
  theme_minimal() +
  ylim(0, 1)  +
  theme(legend.title = element_blank(), legend.position = "none")
pd_branch_wc_md.plot
dev.off()


### Midday branch water content ####

md_branch_wc_10_2023.lm <- lm(VWC_sw_md ~ plot, 
                              data = branch_wc_10_2023.data)
summary(md_branch_wc_10_2023.lm)

md_branch_wc_10_2023.lmer <- lmer(VWC_sw_md ~ plot + (1|genus/id), 
                                  data = branch_wc_10_2023.data)
summary(md_branch_wc_10_2023.lmer)

pdf("results/md_branch_water_content.pdf", 
    height = h, 
    width = w)
md_branch_wc_md.plot <- ggplot(data = branch_wc_10_2023.data, 
                               aes(x = plot,
                                   y = VWC_sw_md,
                                   color = plot)) + 
  geom_boxplot() + 
  scale_color_manual(values = c(color_control, color_tfe)) + 
  xlab("") + ylab("Branch volumetric water content at midday") + 
  theme_minimal() +
  ylim(0, 1)  +
  theme(legend.title = element_blank(), legend.position = "none")
md_branch_wc_md.plot
dev.off()
