# Explore spatial patterns with APS data
# Author: 
# Date:
# Notes:


library(OpenStreetMap)
library(ggmap)
library(ggspatial)
# Pulling in GIS data to explore ------------------------------------------

fac_geo <- st_read(file.path(gispath, "Facility_Points", "Facility_Points.shp")) %>% 
  mutate(elem_flag = ifelse(str_detect(SYMBOL, "Elementary"), 1, 0),
         school_name = str_remove(NAME, "Elementary School"))

arl <- get_stamenmap(
  bbox = c(left = -77.17232,
           bottom = 38.827447,
           right = -77.032086,
           top = 38.93428),
  zoom = 12)
ggmap(arl)

head(facs)
facs %>% filter(contains(SYMBOL, "Elementary")) %>% count(NAME) %>% print(n = Inf)

# Merge in the transfer data
transfers <- read_csv(file.path(datapath, "ATS_transfers.csv"))

fac_geo_ats <- 
  fac_geo %>% 
  left_join(., transfers, by = c("OBJECTID" = "FID")) %>% 
  group_by(below_50) %>% 
  mutate(below_50_total = sum(transfers, na.rm = TRUE)) %>% 
    ungroup()



# Plot the schools
fac_geo_ats %>% 
  filter(elem_flag == 1) %>% 
  ggplot() + geom_sf(aes(size = transfers, colour = transfers)) +
  scale_color_viridis_c()

                
ggmap(arl) + geom_sf(data = fac_geo %>% 
                       filter(elem_flag == 1), aes(color = NAME),
                     inherit.aes = FALSE, size = 3) +
  coord_sf(datum = NA) +
  theme_minimal()


map_plot <- function(df, xvar, title = "", subtitle = "") {
ggplot(fac_geo_ats %>% filter(elem_flag == 1)) +
  annotation_map_tile(zoom = 13) +
  geom_sf(data = fac_geo_ats %>% 
            filter(elem_flag == 1), aes(size = {{xvar}}, colour = {{xvar}}),
          inherit.aes = FALSE) +
  geom_sf_label(data = fac_geo_ats %>% filter({{xvar}} > 1),
                aes(label = School), alpha = 0.25, vjust = 2.5) +
  geom_sf_text(aes(label = {{xvar}}), colour = "white") +
  geom_sf(data = fac_geo_ats %>% filter({{xvar}} == 0),
          aes(fill = school_name)) +
  geom_sf_label(data = fac_geo_ats %>% filter({{xvar}} == 0),
                  aes(label = School), alpha = 0.25, vjust = 1.25) +
  coord_sf(datum = NA) +
  theme_minimal() +
  scale_color_viridis_c(option = "A", direction = -1) +
  scale_size(range = c(0, 20)) + theme(legend.position = "none") +
  labs(x = "", y = "",
       title = str_c({{title}}),
       subtitle = str_c({{subtitle}})) 

}  

bus <- map_plot(fac_geo_ats, bus_count, title = "Bus count summary per elementary school",
         subtitle = "ATS and Key have the largest bus counts")

ggsave(file.path(imagepath, "ATS_bus_count_summary.pdf"),
       plot = bus,
       height = 11.5,
       width = 8,
       useDingbats = FALSE,
       scale = 1.33)


transfers %>% group_by(below_50) %>% summarise(sum = sum(transfers))

transfer_map <- map_plot(fac_geo_ats, transfers,
                         title = "Total transfers to ATS",
                         subtitle = "Fourty three percent of transfers are from South Arlington")

ggsave(file.path(imagepath, "ATS_transfer_map.pdf"),
       plot = transfer_map,
       height = 11.5,
       width = 8,
       useDingbats = FALSE,
       scale = 1.33)

