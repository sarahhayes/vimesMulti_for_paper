### 12/04/2024
### Script to check the impact of the assortativity values
### This is being rerun because we edited the method to extract the cut-offs (below quantile)

## We will have 500 G1 and 300 G2 to be distinct from the data

## For scenarios look at assortavity 1-10 in 1 incremements

rm(list = ls())

library(devtools)
load_all()

g1_obs <- 500
g2_obs <- 300
n = 10000000
q <- 0.95


# We want to run a number of different values for the shape_2 parameter.
# number of values that we want to check
n_grid <- 10
assort_vect <- seq(1,10,length.out = n_grid) # use log values as there is a smaller change for the unit


## First scenario - reporting = 1 for all scenarios and parameters the same for all transmissions.

g1_rr <- 1
g2_rr <- 1
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, si_mean, si_sd, si_mean, si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

cuts_list_temporal

S1 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S1$column_label <- as.numeric(S1$column_label)

# write.csv(S1, "for_paper/non_data_scenarios/S1.csv")

## Second scenario - still 100% reporting but different parameters - G2G2 = 2*G1.

g1_rr <- 1
g2_rr <- 1
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, si_mean, si_sd, 2*si_mean, 2*si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

cuts_list_temporal

S2 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S2$column_label <- as.numeric(S2$column_label)

write.csv(S2, "for_paper/non_data_scenarios/S2.csv")

### Third scenario - still 100% reporting but Mixed = 2*G1 and G2 = 3*G1

g1_rr <- 1
g2_rr <- 1
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, 2*si_mean, 2*si_sd, 3*si_mean, 3*si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

S3 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S3$column_label <- as.numeric(S3$column_label)

#write.csv(S3, "for_paper/non_data_scenarios/S3.csv")


### Fourth scenario - as for scenario 1 but reporting at 50% for G2

g1_rr <- 1
g2_rr <- 0.5
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, si_mean, si_sd, si_mean, si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

S4 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S4$column_label <- as.numeric(S4$column_label)

#write.csv(S4, "for_paper/non_data_scenarios/S4.csv")

### Fifth scenario - as for scenario 2 but reporting at 50% for G2

g1_rr <- 1
g2_rr <- 0.5
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, si_mean, si_sd, 2*si_mean, 2*si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

S5 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S5$column_label <- as.numeric(S5$column_label)

#write.csv(S5, "for_paper/non_data_scenarios/S5.csv")

### Sixth scenario - as for scenario 3 but reduced reporting

g1_rr <- 1
g2_rr <- 0.5
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, 2*si_mean, 2*si_sd, 3*si_mean, 3*si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

S6 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S6$column_label <- as.numeric(S6$column_label)

#write.csv(S6, "for_paper/non_data_scenarios/S6.csv")

## Scenario 7 - 50% reporting for both groups

g1_rr <- 0.5
g2_rr <- 0.5
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, si_mean, si_sd, si_mean, si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

S7 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S7$column_label <- as.numeric(S7$column_label)

write.csv(S7, "for_paper/non_data_scenarios/S7.csv")

## Scenario 8 - 50% reporting for both with G2G2 transmission longer

g1_rr <- 0.5
g2_rr <- 0.5
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, si_mean, si_sd, 2*si_mean, 2*si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

S8 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S8$column_label <- as.numeric(S8$column_label)

write.csv(S8, "for_paper/non_data_scenarios/S8.csv")

## Scenario 9 - 50% reporting for both with G2G2 transmission > mixec > G1G1

g1_rr <- 0.5
g2_rr <- 0.5
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, si_mean*2, si_sd*2, 3*si_mean, 3*si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

S9 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S9$column_label <- as.numeric(S9$column_label)

#write.csv(S9, "for_paper/non_data_scenarios/S9.csv")

## Scenario 10 - 40% reporting for G1 and 25% for G2. Parameters the same

g1_rr <- 0.4
g2_rr <- 0.25
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, si_mean, si_sd, si_mean, si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

S10 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S10$column_label <- as.numeric(S10$column_label)

#write.csv(S10, "for_paper/non_data_scenarios/S10.csv")

## Scenario 11 - as for 10 but G2G2 > others

g1_rr <- 0.4
g2_rr <- 0.25
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, si_mean, si_sd, 2*si_mean, 2*si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

S11 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S11$column_label <- as.numeric(S11$column_label)

#write.csv(S11, "for_paper/non_data_scenarios/S11.csv")

## Scenario 12 - as for 11 but with G2G2 transmission > mixed > G1G1

g1_rr <- 0.4
g2_rr <- 0.25
si_mean <- 30
si_sd <- 20
params_temporal <- c(si_mean, si_sd, si_mean*2, si_sd*2, 3*si_mean, 3*si_sd)


set.seed(1234)

cuts_list_temporal <- list()

for (i in 1:length(assort_vect)) {
  cuts_list_temporal[[i]] <- get_quantiles_multi(d_type = "temporal", distrib = "lognormal",
                                                 obs = c(g1_obs, g2_obs), rr = c(g1_rr, g2_rr),
                                                 params = params_temporal,
                                                 n = n, q = q, assort_mix = assort_vect[i])
}

S12 <- dplyr::bind_rows(cuts_list_temporal, .id = "column_label")
S12$column_label <- as.numeric(S12$column_label)

#write.csv(S12, "for_paper/non_data_scenarios/S12.csv")

all_tables <- S1 %>% cbind(S2[,2:4]) %>%
  cbind(S3[,2:4]) %>% cbind(S4[,2:4]) %>%
  cbind(S5[,2:4]) %>% cbind(S6[,2:4]) %>%
  cbind(S7[,2:4]) %>% cbind(S8[,2:4]) %>%
  cbind(S9[,2:4]) %>% cbind(S10[,2:4]) %>%
  cbind(S11[,2:4]) %>% cbind(S12[,2:4])

#write.csv(all_tables, "for_paper/non_data_scenarios/all_combined.csv")
