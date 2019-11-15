# Purpose: Load and explore data provided so far
# Author: Tim Essam
# Date: 2019-11-13
# Notes: 


# Loading data ------------------------------------------------------------
dir(datapath)

ats_color <- "#2171b5"
non_ats <-  "#bdbdbd"

# Class size analysis -----------------------------------------------------
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
  facet_wrap(~grade_order, scales = "free_y") +
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

#
# Mold SOL data so we can calculate the opportunity gap (compare all subgroups to whites)
# Opportunity gap plots

sol_og <- 
  sol %>% 
  mutate(benchmark = ifelse(Subgroup == "White", value, NA)) %>% 
  group_by(year, Subject, school_name) %>% 
  fill(benchmark, .direction = c("updown")) %>% 
  ungroup() %>% 
  mutate(op_gap = value - benchmark,
         ats_flag_color = ifelse(school_name == "Arlington Traditional", ats_color, non_ats)) 


# Using this for the fill
df_poly <- data.frame(
  x = c(-Inf, Inf, -Inf),
  y = c(-Inf, Inf, Inf)
)


parity_plot <- function(df, sub_filt = "Mathmatics", yearfilt = "2018-2019") {
  df %>% 
    filter(Subject == {{sub_filt}} & year == {{yearfilt}}) %>% 
    filter(Subgroup != "White") %>% 
    mutate(school_sort = reorder_within(school_name, -op_gap, Subgroup)) %>% 
    {# By wrapping ggplot call in brackets we can control where the pipe flow enters (df)
      # This allows us to use filters within the ggplot call
      ggplot() +
        geom_abline(intercept = 0, slope = 1, color = non_ats, linetype = "dotted") +
        #geom_polygon(data = df_poly, aes(-x, -y), fill="#fde0ef", alpha=0.25) +
        geom_point(data = dplyr::filter(., school_name != "Arlington Traditional"),
                   aes(y = value, x = benchmark, fill = ats_flag_color),
                   size = 4, shape = 21, alpha = 0.75, colour = "white") +
        geom_point(data = dplyr::filter(., school_name == "Arlington Traditional"),
                   aes(y = value, x = benchmark, fill = ats_flag_color),
                   size = 4, shape = 21, alpha = 0.80, colour = "white") +
        facet_wrap(~Subgroup,
                   labeller = labeller(groupwrap = label_wrap_gen(10))) +
        coord_fixed(ratio = 1, xlim = c(40, 100), ylim = c(40, 100)) +
        scale_fill_identity() +
        theme_minimal() +
        labs(x = "benchmark test value", y = "Subgroup test value",
             title = str_c(sub_filt, " opportunity gap across subgroups (ATS in blue) for ", yearfilt),
             subtitle = "Each point is a school -- points below the 45 degree line indicate an opportunity gap") +
        theme(strip.text = element_text(hjust = 0))
    }
}

# Creating a list of levels in subjects to loop through in purrr call and then write to pdfs
sublist <- list(c("Science"), c("English: Reading"), c("Mathematics"), c("History and Social Sciences"))

parity_list <- map(sublist, ~parity_plot(sol_og, sub_filt = .))
parity_list[[2]] 

map2(sublist, parity_list, ggsave(filename = file.path(imagepath, str_c(.x, ".pdf")), plot = .y))


map2(file.path(imagepath, paste0(sublist, " opportunity gap parity plots.pdf")), 
     parity_list,
     height = 8.5, 
     width = 11,
     dpi = 300, 
     ggsave)

  

sol_og %>% 
  filter(Subject == "Mathematics" & year == "2018-2019") %>% 
  filter(Subgroup != "White") %>% 
  mutate(school_sort = reorder_within(school_name, -op_gap, Subgroup)) %>% 
  { 
    ggplot() +
  geom_abline(intercept = 0, slope = 1, color = non_ats, linetype = "dotted") +
  geom_point(data = dplyr::filter(., school_name != "Arlington Traditional"),
    aes(y = value, x = benchmark, fill = ats_flag_color),
             size = 3, shape = 21, alpha = 0.80, colour = "white") +
  geom_point(data = dplyr::filter(., school_name == "Arlington Traditional"),
                 aes(y = value, x = benchmark, fill = ats_flag_color),
                 size = 3, shape = 21, alpha = 0.80, colour = "white") +
  facet_wrap(~Subgroup,
             labeller = labeller(groupwrap = label_wrap_gen(10))) +
  coord_fixed(ratio = 1, xlim = c(40, 100), ylim = c(40, 100)) +
  scale_fill_identity() +
  theme_minimal() +
      labs(x = "benchmark test value", y = "Subgroup test value",
           title = "Comparison of ATS to all other schools across Subgroups",
           subtitle = "45 degree line reflects parity with benchmark") +
      theme(strip.text = element_text(hjust = 0))
  }


ats <- ifelse(sol_og$school_name == "Arlington Traditional", ats_color, "black")

opp_plot <- function(df, filtvar, yearvar = "2018-2019") {
  max_dev = unlist(df %>% summarise(max_dev = max(abs(op_gap), na.rm = TRUE)))
  
  df %>% 
    filter(Subject == {{filtvar}} & year == {{yearvar}}) %>% 
    filter(Subgroup != "White") %>% 
    mutate(subgroup_order = fct_reorder(Subgroup, op_gap),
      school_sort = reorder_within(school_name, op_gap, Subgroup)) %>% 
    filter(!is.na(op_gap)) %>% 
    ggplot() +
    #geom_rect(aes(xmin = 0, xmax = -75, ymin = -Inf, ymax = Inf), fill = "#dfc27d", alpha = 0.005) +
    #geom_rect(aes(xmin = 0, xmax = 75, ymin = -Inf, ymax = Inf), fill = "#80cdc1", alpha = 0.005) +
    # geom_rect(data = dplyr::filter(., school_name == "Arlington Traditional"), 
    #           aes(xmin = -75, xmax = 75, ymin = school_sort, ymax = school_sort), 
    #           fill = "black") +
    geom_segment(aes(x = 0, xend = op_gap, y = school_sort, yend = school_sort), colour = "#969696") +
                 #arrow = arrow(length = unit(0.30,"cm"), type = "closed")) +
    geom_point(aes(x = op_gap, y = school_sort, fill = op_gap, group = year),
               size = 3, shape = 21, colour = "#525252") +

    facet_wrap(~subgroup_order, scales = "free_y", drop = TRUE, 
               labeller = labeller(groupwrap = label_wrap_gen(10))) +
    scale_y_reordered() +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'PiYG'),
                         limits = c(-1 * max_dev, max_dev)) +
    theme_minimal() +
    scale_x_continuous(limits = c(-1 * max_dev, max_dev)) +
    labs(x = "Opportunity gap", y = "",
         title = str_c(filtvar, " opportunity gap for ", yearvar),
         subtitle = "Lenght of bar indicates severity of gap",
         caption = "Source: Standards of learning database") +
    theme(legend.position = "none",
          strip.text = element_text(hjust = 0))

}


# Loop over parity deviation plots and save them to pdfs
parity_dev <- map(sublist, ~opp_plot(sol_og, filtvar = .))
parity_dev[[3]]

map2(file.path(imagepath, paste0(sublist, " opportunity gap deviation plots.pdf")), 
     parity_dev,
     height = 8.5, 
     width = 11,
     dpi = 300, 
     scale = 1.25,
     ggsave)




tmp <- sol_og %>% 
  mutate(sub_year = str_c(Subject, " ", as.character(year))) %>% 
  group_by(sub_year) %>% 
  nest() %>% 
  mutate(plot = map2(data, sub_year, 
                     ~opp_plot(.)))

tmp$plot[[2]]



# SOL Plots -- what do we want to show? Let's take a look at results by school, by subject
# Create a generic plotting function to use to splay out all the subjects across graphs

dotplot <- function(df) {
df %>% 
  na.omit() %>% 
  mutate(school_sort = reorder_within(school_name, value_sort, Subgroup)) %>% 
  ggplot() + 
  geom_point(aes(x = value, y = school_sort, fill = year_color),
             size = 3, shape = 21, alpha = 0.80, colour = "white") +
  facet_wrap(~Subgroup, scales = "free_y",
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


# ESL data ----------------------------------------------------------------

esl <- read_csv(file.path(datapath, "esl_table1.csv")) %>% 
  filter(!is.na(school))

esl_totals <- esl %>% filter(str_detect(school, "^Element"))

esl_elem <- esl %>% 
  filter(!str_detect(school, "^Element")) %>% 
  gather(metric, value, tse_2018:share_2015) %>% 
  separate(metric, into = c("type", "year"), sep = "_") %>% 
  mutate(ats_flag = ifelse(school == "Arlington Traditional", ats_color, "#525252")) %>% 
  spread(type, value) %>% 
  mutate(share = nseh/tse) %>% 
  group_by(year) %>% 
  mutate_at(c("tse", "nseh"), .funs = funs(tot = sum(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(overall_share = nseh_tot / tse_tot,
         ave_text = ifelse(school == "Randolph" & year == 2015, "APS average", NA),
         year = as.integer(year))

# First, let's just look at ATS over time relative to the rest - spaghetti plot
(esl_plot <- 
  esl_elem %>% 
  mutate(school_order = fct_reorder(school, share, .desc = TRUE)) %>% 
  ggplot() +
  geom_area(aes(y = overall_share, x = year, group = school), fill = "#f0f0f0") +
  geom_line(aes(y = overall_share, x = year, group = school), color = "#969696", linetype = "dotted") +
  geom_line(aes(y = share, x = year, group = school, colour = "#bdbdbd"), size = 1) +
 geom_point(aes(y = share, x = year, group = school, fill = ats_flag), 
            shape = 21, size = 4, color = "white", stroke = 2) +
  geom_text_repel(aes(y = overall_share, x = year, label = ave_text),
                   nudge_x = 0.5,
                  nudge_y = 0.05,
                   na.rm = TRUE, segment.size = 0,
                  colour = "#969696") +
  scale_fill_identity() + scale_color_identity() +
  theme_minimal() + 
  facet_wrap(~school_order) +
  scale_y_continuous(limits = c(0, .8),
                     labels = percent_format(accuracy = 1)) +
  labs(x = "", y = "",
       caption = "Source: Fall Statistics, Arlington Public Schools, School Year 2018/2019",
       title = glue::glue(
       "ATS ranks 16th out of 25 in the relative share of english learners receiving English for Speakers 
       of Other Languages (ESOL) or High Intensity Language Training (HILT) training services")
       ) +
  theme(strip.text = element_text(hjust = 0, size = 10)) 
)

ggsave(file.path(imagepath, "ATS_ESL_summary.pdf"),
       plot = esl_plot,
       height = 8.5,
       width = 11,
       useDingbats = FALSE,
       scale = 1.25)


# GIS data ----------------------------------------------------------------
geo <- st_read(file.path(gispath, "tl_2019_51_tract", "tl_2019_51_tract.shp"))




