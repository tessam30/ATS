# Purpose: Set up folders and relative paths for ATS analysis
# Author: Tim Essam
# Date: 2019-11-13
# Notes: Run prior to other scripts to ensure path shortcuts are set

pacman::p_load("tidyverse", "tidytext", "RColorBrewer", "readxl", "purrr")

# Create folders for project (if they do not exist)
folder_list <- list("Data", "Images", "Scripts", "Dataout", "GIS", "Documents", "Graphics")
map(folder_list, ~dir.create(.))

datapath <- "Data"
dataout <- "Dataout"
gispath <- "GIS"
graphpath <- "Graphics"
imagepath <- "Images"
rpath <- "Scripts"

