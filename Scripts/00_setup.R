# Purpose: Set up folders and relative paths for ATS analysis
# Author: Tim Essam
# Date: 2019-11-13
# Notes: Run prior to other scripts to ensure path shortcuts are set

pacman::p_load("tidyverse", "tidytext", "RColorBrewer", "readxl", "purrr", "sf", "scales", "ggrepel", "glue", "stringr")

# Create folders for project (if they do not exist)
folder_list <- list("Data", "Images", "Scripts", "Dataout", "GIS", "Documents", "Graphics")
map(folder_list, ~dir.create(.))

datapath <- "Data"
dataout <- "Dataout"
gispath <- "GIS"
graphpath <- "Graphics"
imagepath <- "Images"
rpath <- "Scripts"

# format maps
map_format <- list(  
  scale_fill_viridis_c(option = "A", direction = -1, alpha = 0.85),
  theme_minimal(),
  theme(strip.text = element_text(hjust = 0, size = 12),
        legend.position = "top",
        axis.ticks = element_blank(),
        axis.text = element_blank())
)

plot_save <- list(height = 17,
                  width = 16,
                  dpi = "retina",
                  useDinbats = FALSE,
                  units = c("in")
)
