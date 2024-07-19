#trios_df <- sites %>% dplyr::filter(cluster_no %in% trios) %>% mutate(cluster_no = as.factor(cluster_no))

# If we want to have all the singles on the end. Assign these all to the same group and then join with the trios

singles_slim <- singles
singles_slim$cluster_no <- 0
singles_slim$cluster_no <- as.factor(as.character(singles_slim$cluster_no))
head(singles_slim)

trios_slim <- as.data.frame(trios_df) %>% dplyr::select("dat_time", "group_vect", "cluster_no")
singles_slim <- dplyr::select(singles_slim, c("dat_time", "group_vect", "cluster_no"))
singles

all_slim <- rbind(trios_slim, singles_slim)

all_slim$cluster_no <- as.character(all_slim$cluster_no)

#col_clust_by_time_all <-
  ggplot(data = all_slim, mapping = aes(x= dat_time, y = cluster_no, colour = group_vect)) +
  geom_line(linewidth = 0.5, colour = "black") +
  geom_point(size = 2.0, shape = 16 ) +
  labs( y = "Identified clusters", colour = "Species") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(linewidth =  0.5, linetype = 'solid',
                                        colour = "grey90"),
        panel.grid.minor = element_line(linewidth = 0.5, linetype = 'dashed',
                                        colour = "grey90"),
        legend.position = "bottom",
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10),
        plot.title = element_text(size = 15, face = "bold")) +
  #theme_minimal() +
  scale_x_continuous(name = "Date", breaks = year_breaks, labels = year_labs) +
  scale_colour_manual(values = group_cols, labels = c("Domestic", "Wildlife" )) +
  ggtitle("C")
#col_clust_by_time_all
#ggsave("for_paper/cluster_timeline_three_or_more.png")

#head(trios_df)

  library(ggplot2)
  library(dplyr)

  # identify singles vs clusters
  all_slim <- all_slim %>%
    mutate(is_bottom_row = ifelse(cluster_no=="0", TRUE, FALSE))

  filtered_data <- all_slim %>% filter(cluster_no !=  "0")

  # Define colors
  group_cols <- c("g1" = "red", "g2" = "blue")
  light_group_cols <- c("g1" = scales::alpha("red", 0.3), "g2" = scales::alpha("blue", 0.3))

ggplot(data = all_slim, mapping = aes(x = dat_time, y = cluster_no, colour = group_vect)) +
    geom_line(data = filtered_data, linewidth = 0.5, colour = "black") +
    geom_point(aes(colour = ifelse(is_bottom_row, paste0(group_vect, "_light"), group_vect)), size = 2.0, shape = 16) +
    labs(y = "Identified clusters", colour = "Species") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey50"),
          panel.grid.major = element_line(linewidth =  0.5, linetype = 'solid',
                                          colour = "grey90"),
          panel.grid.minor = element_line(linewidth = 0.5, linetype = 'dashed',
                                          colour = "grey90"),
          legend.position = "bottom",
          legend.key.size = unit(0.4, 'cm'),
          legend.text = element_text(size=8),
          legend.title = element_text(size=10),
          plot.title = element_text(size = 15, face = "bold")) +
    scale_x_continuous(name = "Date", breaks = year_breaks, labels = year_labs) +
    # scale_colour_manual(values = c(group_cols,
    #                                "g1_light" = light_group_cols["g1"],
    #                                "g2_light" = light_group_cols["g2"]),
  scale_colour_manual(values = c("red" ,"blue",
                                 "#FF00004C", "#0000FF40"),
                        labels = c("Domestic", "Wildlife")) +
    ggtitle("C")
  values


  # Not sure need to fade. Also don't want the black line joining the bottom rom
