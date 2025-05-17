library("tidyverse")

alldat <- read_csv("phase_data_with_stability.csv")
str(alldat)

# Widening the data frame with separate columns for particles
widedat <- alldat %>% 
  filter((folder_name == "v1_4" & time_step %in% 0:375) | 
           (folder_name == "v2_0" & time_step %in% 0:375) | 
           folder_name %in% c("v0_0", "v0_2", "v0_4", "v0_6", 
                              "v0_8", "v1_0", "v1_2", "v2_4", "v2_6")) %>% 
  select(-position, -is_unstable) %>% 
  pivot_wider(names_from = particle_index, values_from = velocity)

# Replacing velocities with just signs (+1 or -1)
widedatsign <- widedat[ , 3:length(widedat[1, ])]
widedatsign <- sign(widedatsign)
widedatsign <- bind_cols(widedat[ , 1], widedatsign)

# Cumulative count for each folder
countsign <- widedatsign %>% 
  group_by(folder_name) %>% 
  mutate(across(.cols = where(is.numeric),  
                ~ cumsum(c(FALSE, .x[-1] != .x[-length(.x)])), 
                .names = "change_{.col}")) %>% 
  ungroup() %>% 
  select(change_0:change_9999) %>% 
  reframe(changecount = rowSums(.))

countsign <- bind_cols(widedat[ , 1:2], countsign)

# Preparing data frame for ML by adding appropriate Y values
stabcol <- alldat %>% 
  select(folder_name, time_step, is_unstable) %>% 
  group_by(folder_name, time_step) %>% 
  filter((folder_name == "v1_4" & time_step %in% 0:375) | 
           (folder_name == "v2_0" & time_step %in% 0:375) | 
           folder_name %in% c("v0_0", "v0_2", "v0_4", "v0_6", 
                              "v0_8", "v1_0", "v1_2", "v2_4", "v2_6")) %>% 
  reframe(is_unstable = mean(is_unstable)) %>% 
  mutate(is_unstable = ifelse(folder_name %in% c("v2_0", "v2_4", "v2_6"), 
                              is_unstable + 1, is_unstable)) %>% 
  bind_cols(countsign$changecount, .) %>% 
  rename(changecount = "...1") %>% 
  filter(!is_unstable == 2)

# Plotting sign count for review
ggplot(stabcol, aes(x = time_step, y = changecount, colour = folder_name)) + 
  geom_point(size = 3) + 
  xlim(0, 60) + 
  ylim(0, 6000) + 
labs(title = "Velocity Sign Changes in the System\n", x = "\nTime Step (1e-12 s)", 
     y = paste("Count of Change\n")) + 
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

write_csv(stabcol, "signdat.csv")













