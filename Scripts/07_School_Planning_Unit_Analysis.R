# Explore spatial patterns of planning unites
# Author: Tim Essam
# Date: 2019_11_26
# Notes:




# Load GIS data -----------------------------------------------------------
# Background info on data: https://www.apsva.us/facilities-planning/find-your-planning-unit/

aps_schools <- st_read(file.path(gispath, "Facility_Points", "Facility_Points.shp")) %>% 
  mutate(elem_flag = ifelse(str_detect(SYMBOL, "Elementary"), 1, 0),
         school_name = str_trim(str_remove(NAME, "Elementary School")),
         option_school = ifelse(school_name %in% c("Arlington Traditional School",
                                            "Campbell", "Claremont",
                                            "Key", "Patrick Henry"), 1, 0),
         school_name = ifelse(school_name == "Arlington Traditional School", "ATS", school_name))


spu <- st_read(file.path(gispath, "School_Planning_Units_2017", "School_Planning_Units_2017.shp"))
spu %>% mutate(check = PU_T == PU) %>% count(check) # PU_T and PU contain the same info

aps_boundaries <- st_read(file.path(gispath, "APS_school_boundaries_reconstructed.shp"))



excel_sheets(file.path(datapath, "Planning-Unit-Level-Data_Nov_22_2019-1.xlsx"))
plan_count <- read_excel(file.path(datapath, "Planning-Unit-Level-Data_Nov_22_2019-1.xlsx"), 
                         sheet = "Student Counts by Plan Unit" )


pc_names <- c("PU", "nbhd_school", "students")
names(plan_count) <- set_names(pc_names)


proposal <- read_excel(file.path(datapath, "Planning-Unit-Level-Data_Nov_22_2019-1.xlsx"), 
                       sheet = "Student Counts by PU & Proposal", skip = 1)

proposal_names <- c("PU", "students_now", "current_school", "school_type",
                    "nbhd_school", "walk_school", "bus_now", "nbhd_school_rbs",
                    "bus_rbs", "nbhd_school_p1", "bus_p1", "nbhd_school_p2", "bus_p2")
names(proposal) <- set_names(proposal_names)


# Calculate the total counts for each school under different scenarios
proposal_counts <- 
  proposal %>% 
  select(current_school, contains("nbhd_school"), everything()) %>% 
  pivot_longer(
    cols = current_school:nbhd_school_p2,
    names_to = "proposal",
    values_to = "school"
  ) %>% 
  group_by(proposal, school) %>% 
  summarise(count = sum(students_now)) %>% 
  spread(proposal, count)

# Explore the spatial patterns of student density -------------------------

spu_pc <- spu %>% 
  left_join(., plan_count, by = c("PU" = "PU")) 

spu_proposal <- spu %>% 
  left_join(., proposal)



# Student counts by planning unit - straight up ---------------------------

spu_pc %>% 
  ggplot() +
  geom_sf(aes(fill = students), colour = "white", size = 0.25) +
  map_format +
  scale_fill_viridis_c(na.value = "#f9f9f9", alpha = 0.85, option = "A", direction = -1) +
  geom_sf(data = aps_boundaries, alpha = 0.25) +
  geom_sf(data = (aps_schools %>% filter(option_school == 1)), fill = "") +
  geom_sf_text(data = aps_schools %>% filter(option_school == 1),
               aes(label = school_name), 
               size = 4, alpha = 0.7, vjust = 1.25) +
  labs(x = "", y = "", title = "Number of ATS students for 2019-2020 by planning unit",
       subtitle = "darker colors indicate more students",
       caption = "Source: Planning Unit Level Data 2019-11-22 & APS School Planning Units",
       fill = "Students per planning unit") 
  

ggsave(file.path(imagepath, "ATS Students per student planning.pdf"),
       plot = last_plot(),
       height = 11,
       width = 8.5,
       units = c("in"),
       dpi = "retina",
       useDingbats = FALSE)


map_district <- function(df, x) {
  df %>% 
    ggplot() +
    geom_sf(aes(fill = {{x}}), alpha = 0.8) +
    scale_fill_viridis_c(option = "A", direction = -1, end = .9)  +
    theme(legend.position = "top",
          axis.text = element_blank()) +
    geom_sf_text(aes(label = {{x}}), colour = "#f9f9f9") +
    labs(x = "", y = "")
}

# At a more granular level you can split it out by the school districts
# This will query any of the school districts 
district_maps <- 
  spu_proposal %>% 
  group_by(PU) %>% 
  mutate(students = sum(students_now, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(nbhd_school) %>%
  mutate(total_students = sum(students_now, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(nbhd_school) %>%
  nest() %>% 
  mutate(plots = map2(data, nbhd_school, ~map_district(., students) +
                       geom_sf(data = aps_boundaries %>% filter(nbhd_school %in% data$nbhd_sc), alpha = 0.25) +
                        labs(title = str_c(nbhd_school))))

district_maps$plots[[1]]




ggplot() +
  geom_sf(data = spu, aes(fill = PU), fill = "#f0f0f0", colour = "white", size = 0.25) +
  geom_sf(data = spu_pc, aes(fill = students)) +
  facet_wrap(~nbhd_school) +
  scale_fill_viridis_c(option = "A", direction = -1)


# What is the current scenario in terms of attendance by school mapped by planning unit?
# Functionalize plot, so we can compare across the three scenarios

proposal_map <- function(df, x = students_now, ...) {
  ggplot() +
    geom_sf(data = spu, aes(fill = PU), fill = "#f0f0f0", colour = "white", size = 0.25) +
    geom_sf(data = df, aes(fill = {{x}})) +
    facet_wrap(vars(...)) +
    scale_fill_viridis_c(option = "A", direction = -1)
}

proposal_map(spu_proposal %>% filter(current_school == "ATS"), students_now, current_school)


ggplot() + 
  geom_sf(data = spu, aes(fill = PU), fill = "#f0f0f0", colour = "white", size = 0.25) +
  geom_sf(data = spu_proposal %>% filter(school_type == "Option"), aes(fill = students_now),
          colour = "white") +
  geom_sf(data = (aps_schools %>% filter(option_school == 1)), fill = "") +
  geom_sf_text(data = aps_schools %>% filter(option_school == 1),
                        aes(label = school_name), 
               size = 8, alpha = 0.25, vjust = 1.25 ) +
  facet_wrap(school_type ~ current_school, nrow = 2) +
  labs(x = "", y = "", title = "Number of ATS students for 2019-2020 by planning unit",
       subtitle = "darker colors indicate more students",
       caption = "Source: Planning Unit Level Data 2019-11-22 & APS School Planning Units",
       fill = "Students per planning unit") +
  map_format





ggsave(file.path(imagepath, "ATS Students per student planning unit by current school.pdf"),
       plot = last_plot(),
       height = 17,
       width = 16,
       units = c("in"),
       dpi = "retina",
       useDingbats = FALSE)


# APS Boundaries filled by student population -----------------------------

aps_boundaries %>% 
  mutate(SUM_stdnts = ifelse(SUM_stdnts == 0, NA, SUM_stdnts),
         lon = map_dbl(geometry, ~ st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~ st_centroid(.x)[[2]])
         ) %>% 
  ggplot() +
  geom_sf(aes(fill = SUM_stdnts)) +
  geom_text(aes(x = lon, y = lat, label = str_c(nbhd_sc, "\n", SUM_stdnts)), colour = "white") +
  map_format +
  scale_fill_viridis_c(option = "A", direction = -1, na.value="#f9f9f9") +
  labs(x = "", y = "", caption = "Reconstructed Arlington Public School boundaries from planning data",
       title = "Arlington Public School Boundaries",
       subtitle = "Student counts based on figures associated with planning units in boundary",
       fill = "Number of students within boundaries")

ggsave(file.path(imagepath, "APS_school_boundaries_with_student_counts.pdf"),
       plot = last_plot(),
       height = 11,
       width = 8.5,
       useDingbats = FALSE,
       dpi = "retina")



# Write the proposal data out to reconstruct the 
st_write(spu_proposal, file.path(gispath, "APS_proposal.shp"), delete_layer = TRUE)

# Dissolve planning units into current school boundaries ------------------
# Better solution is to use on ArcMap
aps_boundaries <- spu_proposal %>% group_by(nbhd_school) %>% summarise(students = sum(students_now))
ggplot(aps_boundaries) + geom_sf()
