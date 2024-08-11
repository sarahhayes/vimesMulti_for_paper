### Extracting the cluster results from the second of best-fitting scenarios to incorporate in the paper supplement
### This is scenario 2, rayleigh distribution and assortativity 3.1

rm(list = ls())

library(devtools)

load_all()

library(tidyverse)
library(viridis)
library(ggplot2)

g1_obs <- 313
g2_obs <- 236
g1_rr <- 0.50
g2_rr <- 0.25
n = 10000000
q <- 0.95
si_mean <- 27.8175438596491
si_sd <- 36.8565433014125
params_temporal <- c(si_mean, si_sd, si_mean, si_sd, si_mean, si_sd)

assort_vect <- 3.1

set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

cuts_list_temporal[[1]]

# Repeat for distance
dist_mean <- 0.87
dist_sd <- NA
params_spatial <- c(dist_mean, dist_sd, dist_mean, dist_sd,  dist_mean, dist_sd)

set.seed(1234)

cuts_list_spatial <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_spatial[[i]] <- get_quantiles_multi(d_type = "spatial", distrib = "rayleigh",
                                                obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                params = params_spatial,
                                                n = n, q = q, assort_mix = assort_vect[i])
}

cuts_list_spatial[[1]]


## Need the case data
case_times <- read.csv("C:/Users/hayes/OneDrive - Nexus365/Documents/GitHub/vimesMulti_for_paper/data/case_time_diffs.csv")
case_dists <- read.csv("C:/Users/hayes/OneDrive - Nexus365/Documents/GitHub/vimesMulti_for_paper/data/case_dists.csv")
spp_vect <- read.csv("C:/Users/hayes/OneDrive - Nexus365/Documents/GitHub/vimesMulti_for_paper/data/case_sp_vect.csv")
spp_vect <- as.factor(spp_vect$x)

dat_time <- case_times$x
dat_geo1 <- case_dists$UTM.Easting
dat_geo2 <- case_dists$UTM.Northing


# run the vimesMulti function for each of the sets of cut-offs generated
vimes_multi_results_list <- list()

for (i in 1:length(assort_vect)) {
  vimes_multi_results_list[[i]] <- vimes_multi(
    dat_time = dat_time,
    dat_geo1 = dat_geo1,
    dat_geo2 = dat_geo2,
    temporal_cut_offs = cuts_list_temporal[[i]],
    distance_cut_offs = cuts_list_spatial[[i]],
    group_vect = spp_vect,
    graph_opt = vimes::vimes_graph_opt())
}

vimes_multi_results_list[[1]]$combined_results



# We want to calculate the chi-squared value for the different scenarios.

combined_results <- vimes_multi_results_list[[1]]$combined_results
chisq_vals <- sum((combined_results$data_count - combined_results$sim_count)^2/combined_results$sim_count)

# Create a data set from the inputs and add the cluster membership

dataset <- vimes_multi_results_list[[1]]$dataset
cluster_size_df <- vimes_multi_results_list[[1]]$cluster_size_df

table(cluster_size_df$trans_type)
nrow(cluster_size_df[which(cluster_size_df$trans_type %in% c("g1g1", "g2g2")),])/nrow(cluster_size_df)

# Review the outputs (Can use this to compare to original script)

vimes_multi_results_list[[1]]$vimes_results_list$clusters$K
mean(vimes_multi_results_list[[1]]$vimes_results_list$clusters$size)
hist(vimes_multi_results_list[[1]]$vimes_results_list$clusters$size, col = "pink", xlab = "Size of cluster",
     breaks = seq(-1,20,1),
     main = "Histogram of cluster sizes")
# barplot might be better as discrete values
# however, do barplot later coloured by transmission type which is preferable anyway.


table(vimes_multi_results_list[[1]]$vimes_results_list$clusters$size)

# plot by cluster composition
csl_1 <- cluster_size_df %>%
  dplyr::group_by(total, trans_type)%>%
  dplyr::count()

csl_1_trios_up <- csl_1[which(csl_1$total >=3),]

sum(csl_1_trios_up$n) # so 24 clusters of >= 3 in total.
sum(csl_1_trios_up[which(csl_1_trios_up$trans_type %in% c("g1g1", "g2g2")), "n"])


# plot of the clusters
own_cols <- c("red",  "blue", "purple")
own_labels <- c("Domestic only",  "Wildlife only", "Mixed")


gg_res_1 <- ggplot(csl_1, aes(x = total, y = n, fill = trans_type))  +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = own_cols, name = "Transmission type",
                    #guide = guide_legend(reverse=TRUE),
                    labels = own_labels) +
  theme_classic() +
  theme(axis.title.x = element_text(size=12, face="plain"),
        axis.title.y = element_text(size=12, face="plain"),
        axis.text.x  = element_text(size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        axis.ticks.length = unit(.2, "cm"),
        legend.text = element_text(size = 12, face = "plain"),
        legend.title = element_text(size = 12, face = "plain"),
        legend.position = c(0.8,0.7)) +
  ylim(0,60) +
  #  xlim(1,14) +
  labs(title = "A", y = "Number of clusters", x = "Size of cluster")
gg_res_1
ggsave("for_paper/scenario2_rayleigh_cluster_plot.png")

## Look at the singletons

singles <- as.data.frame(dataset %>% group_by(cluster_no) %>% dplyr::filter(n() == 1))

singles_counts <- hist(singles$dat_time, breaks =  seq(0, 3450, by = 30))$count
plot(singles_counts)

thirty_day_breaks <- seq(0, 3150, by = 30)
year_breaks <- c(0, 365, 731, 1096, 1461, 1826, 2192, 2557, 2922)#, 3287)
year_labs <- c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")#, "2020")

hist(dataset$dat_time, breaks =  thirty_day_breaks, ylab = "Number of cases", main = "", xlab = "Year",
     col = "#333333", xaxt = "n", ylim = c(0,25))
axis(1, at = year_breaks, labels = year_labs)
hist(singles$dat_time, breaks =  thirty_day_breaks, col = "orange", add = T, labs = "")


## Might be good to have a plot of total cases, coloured by domstic and wildlife with singletons coloured lighter.

single_clust_nos <- singles$cluster_no

dataset$clust_or_single <- "clust"
dataset[which(dataset$cluster_no %in% single_clust_nos),"clust_or_single"] <- "single"

table(dataset$clust_or_single)

dataset$clust_group <- paste0(dataset$clust_or_single, as.character(dataset$group_vect))
dataset$clust_group <- as.factor(dataset$clust_group)
levels(dataset$clust_group) <- list("G1 cluster" = "clustg1", "G1 single" = "singleg1",
                                    "G2 cluster" = "clustg2", "G2 single" = "singleg2")


hist30 <- hist(dataset$dat_time, breaks = thirty_day_breaks)
dataset$bin = cut(dataset$dat_time, br = hist30$breaks, labels = hist30$mids)

monthly_counts_gc <- dataset %>%
  count(clust_group, bin)

monthly_counts_gc$bin_num <- as.numeric(as.character(monthly_counts_gc$bin))

fill_colours <- c("red", "#FF9999","blue","#99CCFF")

ggplot(data = monthly_counts_gc, aes(x = bin_num, y = n, fill= clust_group, colour = clust_group)) +
  geom_col() +
  scale_fill_manual(values = c("red", "#FF9999", "blue", "#99CCFF"))+
  scale_color_manual(values = c("black", "black", "black", "black"))+
  labs(x = "Year", y = "Number of cases") +
  scale_x_continuous(breaks = year_breaks, labels = year_labs) +
  theme(legend.title = element_blank())


##############
## code to plot the clusters on a map

library(sf)
library(tmap)
library(spData)

study_regs <- c("Mtwara", "Lindi")

district_shp <- st_read("C:/Users/hayes/OneDrive - Nexus365/Documents/GitHub/Vimes/vimes_multi_sim/tests_sh/gis/TZ_District_2012_pop.shp") # Shape files for the districts of Tanzania
plot(district_shp)

study_dis <- subset(district_shp, district_shp$Region_Nam %in% study_regs)
plot(study_dis)
plot(study_dis$geometry)

study_dis_geom <- study_dis$geometry
class(study_dis_geom)
st_crs(study_dis_geom)

study_map <- st_transform(study_dis_geom, 32737)
study_map_full<- st_transform(study_dis, 32737)
st_crs(study_map)
# map is in WGS 84 (lat/long)
# coordinates are UTM easting/northing

case_points <- st_as_sf(dataset, coords = c("dat_geo1", "dat_geo2"), crs = 32737)
plot(case_points$geometry)
st_crs(case_points$geometry)

plot(study_map)
plot(case_points, add = T)

xy <- c(dat_geo1, dat_geo2)
(sites <- st_as_sf(dataset, coords = c("dat_geo1", "dat_geo2"),
                   crs = 32737, agr = "constant"))

group_cols = c("red", "blue")

ggplot(data = study_map, fill = group_vect) +
  geom_sf() +
  geom_sf(data = sites, size = 1, shape = 23, fill = "darkred")

# Also option to colour by species
ggplot(data = study_map) +
  #  geom_sf(fill = "ghostwhite") +
  geom_sf(fill = "khaki", alpha = 0.2) +
  geom_sf(data = sites, size = 2, shape = 16, aes(colour = group_vect)) +
  scale_colour_manual(values = group_cols, labels = c("Domestic", "Wildlife" )) +
  #  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank()) +
  labs(colour = "Species")


# also have a map where it is coloured by singleton or cluster

col_by_status_map <- ggplot(data = study_map) +
  #geom_sf(fill = "ghostwhite") +
  geom_sf(fill = "khaki", alpha = 0.2) +
  geom_sf(data = sites, size = 1.6, shape = 16, aes(colour = clust_group)) +
  scale_colour_manual(values = fill_colours,
                      labels = c("Domestic - cluster", "Domestic - single", "Wildlife - cluster", "Wildlife - single" )) +
  #  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10),
        plot.title = element_text(size = 15, face = "bold")) +
  labs(colour = "") +
  ggtitle("A")

col_by_status_map



# This is a map of all the cases. Be good to just map those in a cluster and colour them by cluster.

min_size = 3

cluster_locs <- sites %>% group_by(cluster_no) %>% count() %>% filter(n>min_size)
cluster_locs$cluster_no <- as.factor(as.character(cluster_locs$cluster_no))

ggplot(data = study_map) +
  geom_sf() +
  geom_sf(data = cluster_locs, aes(colour = cluster_no), show.legend = F)

#install.packages("randomcoloR")
library(randomcoloR)
nColor <- nrow(cluster_locs)
myColor <- randomcoloR::distinctColorPalette(k = nColor)

col_by_clust_map <- ggplot(data = study_map) +
  #geom_sf(fill = "ghostwhite") +
  geom_sf(fill = "khaki", alpha = 0.2) +
  geom_sf(data = cluster_locs, shape = 16, size = 1.6, aes(colour = cluster_no), show.legend = F) +
  scale_color_manual(values = myColor) +
  #  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 15, face = "bold")) +
  ggtitle("B")
col_by_clust_map


# Would also be good to have the clusters by time. Want to do something similar to figure in first paper

trios <- cluster_locs$cluster_no
trios <- as.numeric(as.character(trios))

trios_df <- sites %>% dplyr::filter(cluster_no %in% trios) %>% mutate(cluster_no = as.factor(cluster_no))

year_breaks


col_clust_by_time <- ggplot(data = trios_df, mapping = aes(x= dat_time, y = cluster_no, colour = group_vect)) +
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
col_clust_by_time


## Make a multi-panel plot.
library(ggpubr)

# if we want 3 panel plot

library(cowplot)
figure.list <- list(col_by_status_map, col_by_clust_map, col_clust_by_time)

top <- plot_grid(figure.list[[1]], figure.list[[2]], ncol = 2)
bottom <- plot_grid(figure.list[[3]], ncol = 1)
multi_map_fig_three <-  plot_grid(top, bottom,
                                  ncol=1, rel_heights=c(1,1))
multi_map_fig_three

# can use 'export > save as image" and set aspect to 900 x 750 to save


