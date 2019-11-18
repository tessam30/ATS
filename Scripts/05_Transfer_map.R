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

ggplot(fac_geo_ats %>% filter(elem_flag == 1)) +
  annotation_map_tile(zoom = 13) +
  geom_sf(data = fac_geo_ats %>% 
            filter(elem_flag == 1), aes(size = transfers, colour = transfers),
          inherit.aes = FALSE) +
  geom_sf_label(data = fac_geo_ats %>% filter(transfers > 1),
                aes(label = School), alpha = 0.25, vjust = 2.5) +
  geom_sf_text(aes(label = transfers), colour = "white") +
  geom_sf(data = fac_geo_ats %>% filter(transfers == 0),
          aes(fill = school_name)) +
  geom_sf_label(data = fac_geo_ats %>% filter(transfers == 0),
                  aes(label = School), alpha = 0.25, vjust = 1.25) +
  coord_sf(datum = NA) +
  theme_minimal() +
  scale_color_viridis_c(option = "A", direction = -1) +
  scale_size(range = c(0, 20)) + theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Arlington Traditional School transfer numbers",
       subtitle = "Fourty-three percent (228/525) of transfers are from South Arlington")

  
  
  ggplot(nc_centers) +
  annotation_map_tile(zoom = 7) +
  geom_sf(aes(color = SID79, size = BIR74),
          show.legend = "point", inherit.aes = FALSE) +
    geom_sf_text(aes(label = NAME), colour = "white") +
  coord_sf(datum = NA) +
  theme_minimal()
