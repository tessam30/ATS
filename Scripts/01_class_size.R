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
  

# SOL school by subject ---------------------------------------------------

excel_sheets(file.path(datapath, "SOL school-by-subject-2019.xlsx"))

sol_raw <- read_excel(file.path(datapath, "SOL school-by-subject-2019.xlsx"), sheet = "School by Subject by Subgroup") 

sol <- read_excel(file.path(datapath, "SOL school-by-subject-2019.xlsx"), sheet = "School by Subject by Subgroup") %>% 
  # Reshape to get values stacked for plotting
  gather(pass_rate, string_value, `2016-2017 Pass Rate`:`2018-2019 Pass Rate`) %>% 
  
  # Flagging the < values as they blow up class type of value (turn what should be numeric into a character) 
  mutate(value_flag = ifelse(string_value == "<", 1, 0),
         value = ifelse(value_flag == 1, NA, as.numeric(string_value))) %>% 
  
  # Keep only Arlington county schools and elementary school types
  filter(`Sch Type` == "Elem" & `Div Name` == "Arlington County") %>% 
  
  # split out pass_rate to recover years
  separate(pass_rate, into = c("year", "type"), sep = " ", extra = "merge") %>% 

  # drop elementary from the school names, takes up too much space in plots
  mutate(school_name = str_remove(`Sch Name`, " Elementary"),
         ats_flag = ifelse(school_name == "Arlington Traditional", 1, 0),
         year_color = case_when(
           year == "2016-2017" & ats_flag == 0 ~ "#bdbdbd",
           year == "2017-2018" & ats_flag == 0 ~ "#969696",
           year == "2018-2019" & ats_flag == 0 ~ "#737373",
           year == "2016-2017" & ats_flag == 1 ~ "#4292c6",
           year == "2017-2018" & ats_flag == 1 ~ "#2171b5",
           year == "2017-2018" & ats_flag == 1 ~ "#08519c"
         )) %>% 
  group_by(school_name, Subgroup, Subject) %>% 
  mutate(value_sort = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Subject) %>% 
  mutate(school_sort = reorder_within(school_name, value_sort, Subgroup)) %>% 
  ungroup() 

write_csv(sol, file.path(dataout, "SOL_Arlington_Elementary.csv"))

# SOL Plots -- what do we want to show? Let's take a look at results by school, by subject
# Create a generic plotting function to use to splay out all the subjects across graphs

dotplot <- function(df) {
df %>% 
  na.omit() %>% 
  mutate(school_sort = reorder_within(school_name, value_sort, Subgroup)) %>% 
  ggplot() + 
  geom_point(aes(x = value, y = school_sort, fill = year_color),
             size = 3, shape = 21, alpha = 0.80, colour = "white") +
  facet_wrap(~Subgroup, scale = "free_y",
             labeller = labeller(groupwrap = label_wrap_gen(10))) +
  scale_y_reordered() +
    labs(x = "", y = "") +
  theme_minimal() +
  scale_fill_identity() + 
  theme(axis.text = element_text(size = 8),
        panel.grid.minor = element_blank(),
        strip.text = element_text(hjust = 0, size = 10))
}

dotplot(sol)

plots <- 
  sol %>% 
  group_by(Subject) %>% 
  nest() %>% 
  mutate(plots = map2(data, Subject,
                      ~dotplot(.) +
                        labs(title = str_c(Subject, " standards of learning from 2016 - 2018."))))

plots$plots[[1]]

map2(file.path(imagepath, paste0(plots$Subject, ".pdf")),
     plots$plots,
     height = 8.5, 
     width = 11,
     useDingbats = FALSE,
     scale = 1.25,
     ggsave)

# State-wide? test results -- to be used for comparison points
state_test <- read_excel(file.path(datapath, "SOL school-by-subject-2019.xlsx"), sheet = "State by Test") %>% 
  gather(pass_rate, value, `2016-2017 Pass Rate`:`2018-2019 Adv Pass Rate`) %>% 
  separate(pass_rate, into = c("year", "type"), sep = " ", extra = "merge")






