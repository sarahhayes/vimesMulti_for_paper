library(ggplot2)
library(dplyr)
library(forcats)

# Preprocess the data to reorder `cluster_no` based on `dat_time` for points not in the bottom row
# Convert cluster_no to a factor with levels ordered by the minimum dat_time for non-bottom row points

all_slim <- rbind(trios_slim, singles_slim)
all_slim$cluster_no <- as.character(all_slim$cluster_no)

# identify singles vs clusters
all_slim <- all_slim %>%
  mutate(is_bottom_row = ifelse(cluster_no=="0", TRUE, FALSE))

all_slim$cluster_no <- as.numeric(as.character(all_slim$cluster_no))

filtered_data <- all_slim %>% filter(cluster_no !=  "0")

fd <- filtered_data %>%
  group_by(cluster_no) %>%
  summarise(lowest_value = min(dat_time)) %>%
  ungroup() %>%
  arrange(lowest_value) %>%
  mutate(rank = row_number()) %>%
  dplyr::select(cluster_no, rank)

all_slim <- left_join(all_slim, fd, by = "cluster_no")
all_slim$rank[is.na(all_slim$rank)] <- 100

filtered_data <- left_join(filtered_data, fd)

#all_slim[is.na(all_slim$rank), 'rank'] <- "0"
#all_slim$cluster_no <- as.factor(all_slim$cluster_no)
#levels(all_slim$cluster_no)

#filtered_data$cluster_no <- as.factor(filtered_data$cluster_no)
#levels(filtered_data$cluster_no)

all_slim$rank <- as.factor(all_slim$rank)
all_slim <- all_slim %>%
  dplyr::mutate(rank = forcats::fct_rev(rank))
#all_slim[which(is.na(all_slim2$rank)), "rank"]# <- "0"

#order(all_slim$rank)

# Print the factor variable to see current levels
# print(all_slim$rank)

# New order of levels (1 to 100)
#new_order <- as.character(0:98)

# Reorder the factor levels
#all_slim$rank <- factor(all_slim$rank, levels = new_order)

# Print the reordered factor variable
#print(all_slim$rank)

filtered_data$rank <- as.factor(filtered_data$rank)
filtered_data <- filtered_data %>%
  dplyr::mutate(rank = forcats::fct_rev(rank))

#levels(all_slim$rank)
#levels(filtered_data$rank)

#current_levels <- levels(filtered_data$rank)
#current_levels
#new_level <- "0"
#new_levels <- c(current_levels, new_level)

# Update the factor with the new levels
#filtered_data$rank <- factor(filtered_data$rank, levels = new_levels)
#print(filtered_data$rank)

#
# library(ggplot2)
# library(dplyr)
#
# # Ensure `all_slim` is ordered correctly
# all_slim <- all_slim %>%
#   arrange(dat_time, rank) %>%
#   mutate(rank = factor(rank, levels = unique(rank[order(dat_time)])))
#
# # Ensure `filtered_data` is ordered correctly if it affects the line plot
# filtered_data <- filtered_data %>%
#   arrange(dat_time, rank)



# Plot
# col_clust_by_time_with_singles <-
  ggplot(data = all_slim, mapping = aes(x = dat_time, y = rank, colour = group_vect)) +
  #geom_line(data = filtered_data, linewidth = 0.5, colour = "black") +
  geom_point(aes(colour = ifelse(is_bottom_row, paste0(group_vect, "_light"), group_vect)),
                size = 2.0, shape = 16) +
 geom_line(data = filtered_data, linewidth = 0.5, colour = "black") +
  labs(y = "Identified clusters", colour = "Species") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey90"),
        panel.grid.minor = element_line(linewidth = 0.5, linetype = 'dashed', colour = "grey90"),
        legend.position = "bottom",
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10),
        plot.title = element_text(size = 15, face = "bold")) +
  scale_x_continuous(name = "Date", breaks = year_breaks, labels = year_labs) +
  scale_colour_manual(values = c("red" ,"blue", "#FF00004C", "#0000FF40"),
                      labels = c("Domestic - cluster", "Wildlife - cluster", "Domestic - single", "Wildlife - single")) +
  ggtitle("C") #+
#    scale_y_discrete(drop = FALSE)

col_clust_by_time_with_singles


levels(all_slim$cluster_no_ordered)
