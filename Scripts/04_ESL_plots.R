# Purpose: Load and explore ESL data
# Author: Tim Essam
# Date: 2019-11-13
# Notes: 




# ATS colors --------------------------------------------------------------

ats_color <- "#2171b5"
non_ats <-  "#bdbdbd"


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
