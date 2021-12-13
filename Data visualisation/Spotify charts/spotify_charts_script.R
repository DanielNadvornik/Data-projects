library(tidyverse)
library(ggplot2)
library(RColorBrewer)




spotify_df %>% 
   ggplot(aes(x = christmas_streams_per_10000_ppl, y = seasonal_rank, color = region, label = country)) +
   geom_hline (yintercept = mean(spotify_df$christmas_streams_per_10000_ppl, na.rm = TRUE)) + 
   geom_vline (xintercept = mean(spotify_df$seasonal_rank, na.rm = TRUE))  +
   theme_classic() +
   geom_point(size = 4, position = position_stack(vjust = 0.95),
              alpha = 0.8)+
   geom_label(data = highlight_CZSKSW,
              aes(x = christmas_streams_per_10000_ppl, y = seasonal_rank),
              color="blue",
              size=4) + 
   scale_color_brewer(palette = "Set1") +
   scale_x_continuous(limits = c(-20, 600)) +
   scale_y_continuous(limits = c(0, 220)) + 
   theme(text = element_text(size = 12)) + theme(legend.position = c(0.84, 0.8)) + 
   
   labs(title = "All I want for christmas is YOUUUUU",
     subtitle = "Spotify streams",
     x = "Number of Christmas streams per 10 000 people", 
     y = "Seasonal rank")


highlight_CZSKSW <- spotify_df %>% filter(country %in% c("Czech Republic", "Sweden", "Slovakia"))


spotify_df %>% ggplot(aes(x = christmas_streams_per_10000_ppl, y = seasonal_rank, color = region, label = country)) +
   geom_point() + 
   facet_wrap(~region, scales = "free") +
   labs(title = "All I want for christmas is YOUUUUU",
       subtitle = "Spotify streams",
       x = "Number of Christmas streams per 10 000 people", 
       y = "Seasonal rank") + 
   theme_bw() + 
   geom_point() + 
   theme(legend.position = c(0.85, 0.25))

