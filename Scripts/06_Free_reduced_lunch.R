# Explore free and reduced school lunch numbers
# Author: Tim Essam
# Date: 2019_11_21
# Notes: 


# Call in school lunch data and explore -----------------------------------
ats_color <- "#2171b5"
non_ats <-  "#bdbdbd"


lunch <- read_csv(file.path(datapath, "SNPMonthlyEligibilityReport.csv"), skip = 1) %>% 
  select(school_id = `007-Arlington County Public Schools`,
         school_name = X2,
         school_type = X3,
         low_grade = X4,
         high_grade = X5,
         snp_members = X6,
         free_elig = X7,
         free_pct_str = X8,
         reduced_elig = X9,
         reduced_pct_str = X10,
         tot_free_reduced = X11,
         tot_free_reduced_pct_str = X12) %>% 
  mutate(school_name = str_to_title(school_name) %>% str_remove_all(., "Elem.*"))



# Reshape for faceting wrapping plots -------------------------------------


lunch_long <- lunch %>% 
  select(-contains("_str")) %>% 
  gather(type, count, free_elig:tot_free_reduced) %>% 
  mutate(percent = count / snp_members,
         ats_flag = ifelse(school_name == "Arlington Traditional", ats_color, non_ats),
         type = case_when(
           type == "free_elig" ~ "1.free",
           type == "reduced_elig" ~ "2.reduced",
           type == "tot_free_reduced" ~ "3.free and reduced"
         ))

lunch_long %>% 
  filter(school_type == "Elementary") %>% 
  mutate(school_order = reorder_within(school_name, percent, type)) %>% 
  ggplot(aes(x = school_order, y = percent, fill = ats_flag)) + 
  geom_col() +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~type, scales = "free_y") +
  scale_fill_identity() +
  scale_color_identity() +
  theme_minimal() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  #geom_text(aes(label = round(frm_share, 2), hjust = 1.25, colour = "#ffffff")) +
  labs(x = "", y = "", 
       title = "Percent of students receiving free or reduced meals",
       caption = "Source: SNP Monthly Eligibility Report 2019-11") +
  theme(strip.text = element_text(hjust = 0))

ggsave(file.path(imagepath, "Free and reduced meals summary.pdf"),
       plot = last_plot(),
       height = 8.5,
       width = 11,
       scale = 1.2,
       useDingbats = FALSE)


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