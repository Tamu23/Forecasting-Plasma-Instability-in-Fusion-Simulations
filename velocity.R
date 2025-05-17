library("tidyverse")

alldat <- read_csv("phase_data_with_stability.csv")
str(alldat)

# Folders with time_steps < 501: ("v1_4", "v1_8", "v2_0", "v2_2")
# v1_4 can be used for 0 to 375 timesteps
# v1_8 can be used for 245 to 500 timesteps
# v2_0 can be used for 0 to 375 timesteps
# v2_2 can be used for 295 to 375 timesteps

# Folders with instability: ("v1_8", "v2_0", "v2_2", "v2_4", "v2_6")

# Plotting KE feature for all folders
(avgplotall <- alldat %>% 
    group_by(folder_name, time_step) %>% 
    reframe(is_unstable = mean(is_unstable), 
            avgv = mean(velocity^2)) %>%
    ggplot(aes(x = time_step, y = avgv, colour = folder_name)) + 
    geom_point() + 
    labs(title = "Average Kinetic Energy of the System\n", x = "\nTime Step (1e-12 s)", 
         y = paste("Average KE (", expression(Velocity^2), ")\n")) + 
    scale_colour_discrete(name = "Folders") +
    theme_bw() + 
    theme(
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = , "cm"), 
      axis.text = element_text(size = 12), 
      axis.title = element_text(size = 14), 
      plot.title = element_text(size = 20, hjust = 0.5), 
      legend.text = element_text(size = 12), 
      legend.title = element_text(size = 14), 
      legend.background = element_rect(fill = "gray89")
    )
)

# Creating a folder with filtered data and a useful Y (response variable)
kedat <- alldat %>% 
  group_by(folder_name, time_step) %>% 
  filter((folder_name == "v1_4" & time_step %in% 0:375) | 
           (folder_name == "v2_0" & time_step %in% 0:375) | 
           folder_name %in% c("v0_0", "v0_2", "v0_4", "v0_6", 
                              "v0_8", "v1_0", "v1_2", "v2_4", "v2_6")) %>% 
  reframe(is_unstable = mean(is_unstable), 
          avgv = mean(velocity^2)) %>% 
  mutate(is_unstable = ifelse(folder_name %in% c("v2_0", "v2_4", "v2_6"), 
                              is_unstable + 1, is_unstable)) %>% 
  filter(!is_unstable == 2)

# Plotting only the filtered dataset, for review
(avgplot <- kedat %>% 
    ggplot(aes(x = time_step, y = avgv, colour = folder_name)) + 
    geom_point() + 
    labs(title = "Average Kinetic Energy of the System\n", x = "\nTime Step (1e-12 s)", 
         y = paste("Average KE (", expression(Velocity^2), ")\n")) + 
    scale_colour_discrete(name = "Folders") +
    theme_bw() + 
    theme(
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = , "cm"), 
      axis.text = element_text(size = 12), 
      axis.title = element_text(size = 14), 
      plot.title = element_text(size = 20, hjust = 0.5), 
      legend.text = element_text(size = 12), 
      legend.title = element_text(size = 14), 
      legend.background = element_rect(fill = "gray89")
    )
)

write_csv(kedat, "mldat.csv")




















