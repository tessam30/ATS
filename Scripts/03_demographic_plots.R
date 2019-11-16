# Purpose: Load and explore civil rights demographic data for APS
# Author: Tim Essam
# Date: 2019-11-13
# Notes: 


# Loading data ------------------------------------------------------------

ats_color <- "#2171b5"
non_ats <-  "#bdbdbd"

# Civil Rights and Free and Reduce Lunch ----------------------------------
excel_sheets(file.path(datapath, "Civil Rights_Free Reduced Meal Stats 2018.xlsx"))

cr <- read_excel(file.path(datapath, "Civil Rights_Free Reduced Meal Stats 2018.xlsx"), sheet = "Civil Rights", skip = 2) %>% 
  select(-contains("%")) %>% 
  rename(school = `...1`,
         `American Indian/Alaskan Native` = `No....2`,
         `Asian` = `No....4`,
         `Black/African American` = `No....6`,
         Hispanic = `No....8`,
         `Native Hawaiian/Other` = `No....10`,
         `White` = `No....12`,
         `Multiple` = `No....14`,
         `Total` = `...16`) %>% 
  filter(school != "APS Average") %>% 
  gather(demographic, count, `American Indian/Alaskan Native`:Multiple) %>% 
  mutate(share = (count / Total)) %>% 
  group_by(demographic) %>% 
  mutate(ATS_total = sum(count)) %>% 
  ungroup() %>% 
  mutate(ats_flag = ifelse(school == "Arlington Traditional", "#2171b5", "#bdbdbd"),
         school = ifelse(school == "Gleve", "Glebe", school))

# Plot the totals across demographics - create a function to select between counts and totals
demog_plot <- function(df, xvar = count) {
  df %>% 
    mutate(dem_order = fct_reorder(demographic, {{xvar}}, .desc = TRUE),
           school_order = reorder_within(school, {{xvar}}, demographic)) %>% 
    ggplot(aes(x = school_order, y =  {{xvar}}, fill = ats_flag)) + geom_col() +
    #geom_text(aes(label = round( {{xvar}}, 2), hjust = 1.24, colour = "#ffffff")) +
    facet_wrap(~dem_order, scales = "free_y", nrow = 2) + 
    coord_flip() +
    scale_x_reordered() +
    scale_fill_identity() +
    theme_minimal() +
    labs(x = "", y = "") +
    theme(strip.text = element_text(hjust = 0))
}

demog_plot(cr, xvar = share) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

demog_plot(cr) +
  labs(title = "Civil Rights Statistics by School as of October 31, 2018")

ggsave(file.path(imagepath, "Elementary Civil Rights Summary 2018-10-31.pdf"),
       plot = last_plot(),
       height = 8.5,
       width = 11,
       useDingbats = FALSE,
       scale = 1.33)

demog_plot(cr %>% filter(demographic == "White"), xvar = Total) +
  theme(strip.text = element_blank()) + 
  geom_text(aes(label = Total), hjust = 1.24, colour = "#ffffff") +
  labs(title = "McKinely is the largest school at nearly 800 students. ATS ranks 16th overall.",
       caption = "Civil Rights Statistics by School as of October 31, 2018")


# Free and reduced meals --------------------------------------------------

frm <- read_excel(file.path(datapath, "Civil Rights_Free Reduced Meal Stats 2018.xlsx"), 
                  sheet = "Free-Reduced Meal", skip = 3) %>% 
  select(school = `...1`,
         frm_tally = `No.`,
         tot_students = `Total Students*`) %>% 
  mutate(frm_share = frm_tally / tot_students,
         ats_flag = ifelse(school == "Arlington Traditional", ats_color, non_ats))

frm %>% 
  mutate(school_order = fct_reorder(school, frm_share)) %>% 
  ggplot(aes(x = school_order, y = frm_share, fill = ats_flag)) + geom_col() +
  coord_flip() +
  scale_fill_identity() +
  scale_color_identity() +
  theme_minimal() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  #geom_text(aes(label = round(frm_share, 2), hjust = 1.25, colour = "#ffffff")) +
  labs(x = "", y = "", 
       title = "Percent of students receiving free or reduced meals",
       caption = "Source: Civil Rights_Free Reduced Meal Stats 2018")
