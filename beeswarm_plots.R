#Load Packages
library(tidytuesdayR)
library(tidyverse)
library(beeswarm)
library(ggdark)

#Load in data
tuesdata <- tidytuesdayR::tt_load('2022-01-11')

#Split into the two different data sets
colony_data <- tuesdata$colony

stressor_data <- tuesdata$stressor

#Reduce data down to average per year, and clean up data
stressor_data_reduced <- stressor_data %>%
  filter(year != "5/") %>% #Remove 
  group_by(year, state, stressor) %>%
  mutate(stress_pct_yearly_av = mean(stress_pct)) %>%
  distinct(stress_pct_yearly_av, .keep_all = T) %>%
  select(-months) %>%
  ungroup() %>%
  mutate(stressor = replace(stressor, stressor == "Disesases", "Diseases")) %>%
  mutate(stressor = fct_relevel(stressor, 
                                c("Diseases", "Pesticides", "Varroa mites", 
                                  "Other pests/parasites", "Other", "Unknown")))


  #Make beeswarm (quasirandom) plot  
  ggplot(stressor_data_reduced, 
         aes(x = reorder(stressor, -stress_pct, mean, na.rm = T), #order by mean
             y = stress_pct)) +
    ggbeeswarm::geom_quasirandom(aes(colour = year), #Just for more colours
                                 size = 1.5, 
                                 shape="\U1F41D", #Bee is unicode!
                                 width = 0.5) +
    scale_color_gradient(low = "Yellow", high = "Dark Orange") +
    ylab("Stress Percentage (log10 scale)")+
    xlab("Stressor")+
    ggtitle("US Bee Colony Stressors (2015-2021)", 
            subtitle = "Percent of bee colonies in a state affected by a stressor") +
    scale_y_log10() +
    dark_theme_classic()+
    theme(text=element_text(size=16,  family="sans"))+ 
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),)
  
  ggsave("beeswarm_quasi_plot.png", width = 10, height = 5)

  