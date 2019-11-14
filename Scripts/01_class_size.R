# Purpose: Load and explore data provided so far
# Author: Tim Essam
# Date: 2019-11-13
# Notes: 


# Loading data ------------------------------------------------------------
dir(datapath)

class_size <- read_excel(file.path(datapath, "APS Class Size Report 2018-2019_tidy_names.xlsx"), skip = 3)

cs_long <- 
  class_size %>% 
  gather(stat, value, k_ave:five_max) %>% 
  separate(stat, into = c("Grade", "Stat")) %>% 
  spread(Stat, value) %>% 
  mutate(Grade = case_when(
    Grade == "k" ~ "K",
    Grade == "one" ~ "1st",
    Grade == "two" ~ "2nd",
    Grade == "three" ~ "3rd",
    Grade == "four" ~ "4th",
    Grade == "five" ~ "5th"
  )) %>% 
  mutate(school_sort = reorder_within(School, ave, Grade, .desc = TRUE),
         grade_order = factor(Grade), 
         grade_order = fct_relevel(grade_order, "K", after = 0),
         ATS_flag = case_when(
           School == "Arlington Traditional" ~ "#377eb8",
           School == "APS Average" ~ "#000000",
           TRUE ~ "#bdbdbd"
         ),
         ats_ave = ifelse(School == "APS Average", ave, NA),
         School = ifelse(School == "Gleve", "Glebe", School)) %>% 
  group_by(Grade) %>% 
  fill(ats_ave, .direction = c("updown")) %>% 
  ungroup()
  

class_size_plot <- 
  cs_long %>% 
  ggplot() + 
  geom_vline(aes(xintercept = ats_ave), colour = "#525252", linetype = "dotted") +
  geom_segment(aes(x = min, xend = max, y = school_sort, yend = school_sort),
               colour = "#d9d9d9", size = 1) +
  geom_point(aes(x = ave, y = school_sort, fill = ATS_flag), size = 3, shape = 21, colour = "white") +
  facet_wrap(~grade_order, scale = "free_y") +
  scale_y_reordered() +
  theme_minimal() +
  scale_fill_identity() +
  labs(y = "", x = "Average class size",
       title = "ATS class size is above the APS average for all grades K - 5th") +
  theme(axis.text = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(hjust = 0, size = 12)) 

ggsave(file.path(graphpath, "ATS_class_size_2019.pdf"),
       plot = class_size_plot,
       height = 8.5,
       width = 11,
       useDingbats = FALSE)  
  
