#libs
library(dplyr)
library(ggplot2)

data <- df

percentage <- 100 - (( nrow(data) / nrow(data_raw) ) * 100)


#excluding 
df <- df %>% 
  filter(adjustment_duration > 1) %>% #adjustment time is too quick
  filter(adjustment_duration < 15) %>% #NOT SURE! 15 sec is too long ???
  filter(response_rt < 10) %>% #Yildirim & Sayim. Low accuracy and high confidence in redundancy masking
  filter(number_deviation < 4) %>% filter(number_deviation > -4) #same



df_width_space_deviations <- df %>% group_by(subID,correct_num,correct_width, number_deviation) %>%
  summarise(mean_width_deviation = mean(width_deviation),
            mean_space_deviation = mean(space_deviation),
            n = n())
one_point_data <- df_width_space_deviations %>% group_by(correct_num,number_deviation) %>%
  summarise(mean_width_deviation_all = mean(mean_width_deviation),
            sd_width_deviation_all = sd(mean_space_deviation),
            n = n(),
            se = sd_width_deviation_all / sqrt(n))


plot_last <- df_width_space_deviations %>% ggplot(aes(x =number_deviation , y = mean_width_deviation)) +
  geom_point(aes(color = ifelse(number_deviation < 0, "RM", ifelse(number_deviation > 0, "no RM", "Grey")), size = n),
             position = position_jitterdodge(0.4),
             stat = "identity",
             alpha = 0.2) +
  facet_wrap(~correct_num+correct_width) +
  geom_vline(xintercept = 0, color = "darkred", alpha = 0.3) +
  geom_hline(yintercept = 0, color = "darkred", alpha = 0.3) +
  ylab("Width Deviation") +
  xlab("Number Deviation") +
  scale_color_manual(values = c("RM" = "darkblue", "no RM" = "darkgreen", "Grey" = "darkgrey")) +
  labs(color = "") +
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), 
                     labels = c("-3", "-2", "-1", "0", "1", "2", "3"), limits = c(-3.2, 3.2))+
  theme_minimal() +
  geom_point(data = one_point_data, aes(x = number_deviation, y = mean_width_deviation_all)) +
  geom_errorbar(
    data = one_point_data,
    aes(
      x = number_deviation,
      y = mean_width_deviation_all,
      ymin = mean_width_deviation_all - se,
      ymax = mean_width_deviation_all + se
    ),
    size  = 1.2,
    width = .00,
    alpha = 0.5,
  )
print(plot_last)
ggsave("plot_last.jpg", plot = plot_last, device = "jpeg")

             
             

df_compare_RM_noRM <- df %>% group_by(RM, subID) %>%
  summarise(mean_width_deviation_part = mean(width_deviation),
            sd_width_deviation_part = sd(width_deviation),
            n = n(),
            se_width_deviation_part = sd_width_deviation_part / sqrt(n),
            mean_space_deviation_part = mean(space_deviation),
            sd_space_deviation_part = sd(space_deviation),
            se_space_deviation_part = sd_space_deviation_part / sqrt(n),
            mean_stim_length_deviation_part = mean(stim_length_deviation),
            sd_stim_length_deviation_part = sd(stim_length_deviation),
            se_stim_length_deviation_part = sd_stim_length_deviation_part / sqrt(n)
            ) %>%
  group_by(RM) %>% summarise(mean_width_deviation = mean(mean_width_deviation_part),
                             sd_width_deviation = sd(mean_width_deviation_part),
                             n = n(),
                             se_width_deviation = sd_width_deviation / sqrt(n),
                             mean_space_deviation = mean(mean_space_deviation_part),
                             sd_space_deviation = sd(mean_space_deviation_part),
                             se_space_deviation = sd_space_deviation / sqrt(n),
                             mean_stim_length_deviation = mean(mean_stim_length_deviation_part ),
                             sd_stim_length_deviation = sd(mean_stim_length_deviation_part ),
                             se_stim_length_deviation = sd_stim_length_deviation / sqrt(n))
