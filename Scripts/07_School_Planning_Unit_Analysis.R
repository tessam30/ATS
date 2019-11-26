# Explore spatial patterns of planning unites
# Author: Tim Essam
# Date: 2019_11_26
# Notes:


theme_set(theme_minimal())
map_theme <- list(theme(strip.text = element_text(hjust = 0)))

# Load GIS data -----------------------------------------------------------
# Background info on data: https://www.apsva.us/facilities-planning/find-your-planning-unit/

spu <- st_read(file.path(gispath, "School_Planning_Units_2017", "School_Planning_Units_2017.shp"))
spu %>% mutate(check = PU_T == PU) %>% count(check) # PU_T and PU contain the same info

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
    map_theme + 
    scale_fill_viridis_c(option = "A", direction = -1)
}

proposal_map(spu_proposal %>% filter(current_school == "ATS"), students_now, current_school)


ggplot() + 
  geom_sf(data = spu, aes(fill = PU), fill = "#f0f0f0", colour = "white", size = 0.25) +
  geom_sf(data = spu_proposal %>% filter(school_type == "Option"), aes(fill = students_now),
          colour = "white") +
  facet_wrap(school_type ~ current_school, nrow = 2) +
  map_theme +
  scale_fill_viridis_c(option = "A", direction = -1) +
  labs(x = "", y = "", title = "Number of ATS students for 2019-2020 by planning unit",
       subtitle = "darker colors indicate more students",
       caption = "Source: Planning Unit Level Data 2019-11-22 & APS School Planning Units",
       fill = "Students per planning unit") +
  theme(legend.position = "top")



ggsave(file.path(imagepath, "ATS Students per student planning unit by current school.pdf"),
       plot = last_plot(),
       height = 17,
       width = 16,
       units = c("in"),
       dpi = "retina",
       useDingbats = FALSE)


# Write the proposal data out to reconstruct the 
st_write(spu_proposal, file.path(gispath, "APS_proposal.shp"), delete_layer = TRUE)

# Dissolve planning units into current school boundaries ------------------
# Better solution is to use on ArcMap
aps_boundaries <- spu_proposal %>% group_by(nbhd_school) %>% summarise(students = sum(students_now))
ggplot(aps_boundaries) + geom_sf()
