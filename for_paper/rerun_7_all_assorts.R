# Run for all the assortativity values

rm(list = ls())

library(devtools)

load_all()

g1_obs <- 313
g2_obs <- 236
g1_rr <- 0.25
g2_rr <- 0.10
n = 10000000
q <- 0.95
si_mean <- 27.8175438596491
si_sd <- 36.8565433014125
params_temporal <- c(si_mean, si_sd, si_mean, si_sd, si_mean, si_sd)

# We want to run a number of different values for the shape_2 parameter.
# number of values that we want to check
n_grid <- 91
assort_vect <- seq(1,10,length.out = n_grid) # use log values as there is a smaller change for the unit
#assort_vect <- 2.4

set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                        obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                        params = params_temporal,
                                        n = n, q = q, assort_mix = assort_vect[i])
}

# cuts_list_temporal

# Repeat for distance
dist_mean <- 0.87
params_spatial <- c(dist_mean, NA, dist_mean, NA,  dist_mean, NA)

set.seed(1234)

cuts_list_spatial <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_spatial[[i]] <- get_quantiles_multi(d_type = "spatial", distrib = "rayleigh",
                                  obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                  params = params_spatial,
                                  n = n, q = q, assort_mix = assort_vect[i])
}

# cuts_list_spatial

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

chisq_vals <- c()

for (j in 1:length(assort_vect)) {
  combined_results <- vimes_multi_results_list[[j]]$combined_results
  chisq_vals[j] <- sum((combined_results$data_count - combined_results$sim_count)^2/combined_results$sim_count)
  }

chisq_vals

which(chisq_vals == min(chisq_vals))
assort_vect[which(chisq_vals == min(chisq_vals))]

assort_vect[which(chisq_vals <3.841)]
chisq_vals[which(chisq_vals < 3.841)]


# ################################################################################################
#
# ## Now want to repeat the whole analysis, but using the proportions below the cut-off value instead of all of them
#
#
# set.seed(1234)
#
# cuts_list_temporal_centile <- list()
#
# for (i in 1:length(assort_vect)) {
#   cuts_list_temporal_centile[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
#                                                  obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
#                                                  params = params_temporal,
#                                                  n = n, q = q, assort_mix = assort_vect[i],
#                                                  prop_type = "centile")
# }
#
#
# # Repeat for distance
#
# set.seed(1234)
#
# cuts_list_spatial_centile <- list()
#
# tictoc::tic()
# for (i in 1:length(assort_vect)) {
#   cuts_list_spatial_centile[[i]] <- get_quantiles_multi(d_type = "spatial", distrib = "rayleigh",
#                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
#                                                 params = params_spatial,
#                                                 n = n, q = q, assort_mix = assort_vect[i],
#                                                 prop_type = "centile")
# }
# tictoc::toc()
#
# # run the vimesMulti function for each of the sets of cut-offs generated
# vimes_multi_results_list_centile <- list()
#
# for (i in 1:length(assort_vect)) {
#   vimes_multi_results_list_centile[[i]] <- vimes_multi(
#     dat_time = dat_time,
#     dat_geo1 = dat_geo1,
#     dat_geo2 = dat_geo2,
#     temporal_cut_offs = cuts_list_temporal_centile[[i]],
#     distance_cut_offs = cuts_list_spatial_centile[[i]],
#     group_vect = spp_vect,
#     graph_opt = vimes::vimes_graph_opt())
# }
#
# vimes_multi_results_list_centile[[1]]$combined_results
#
# # We want to calculate the chi-squared value for the different scenarios.
#
# chisq_vals_centile <- c()
#
# for (j in 1:length(assort_vect)) {
#   combined_results <- vimes_multi_results_list_centile[[j]]$combined_results
#   chisq_vals_centile[j] <- sum((combined_results$data_count - combined_results$sim_count)^2/combined_results$sim_count)
# }
#
# chisq_vals_centile
#
# which(chisq_vals_centile == min(chisq_vals_centile))
# chisq_vals_centile[which(chisq_vals_centile == min(chisq_vals_centile))]
# assort_vect[which(chisq_vals_centile == min(chisq_vals_centile))]
#
# # critical value for Chi-sq with 1 degree of freedom is 3.841
# # we want to know the values of the shape vector which have Chi sq values below this.
#
# assort_vect[which(chisq_vals <3.841)]
# assort_vect[which(chisq_vals_centile < 3.841)]
