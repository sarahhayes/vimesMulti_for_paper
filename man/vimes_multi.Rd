% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vimes_multi.R
\name{vimes_multi}
\alias{vimes_multi}
\title{Generates clusters and associated information for two groups}
\usage{
vimes_multi(
  dat_time,
  dat_geo1,
  dat_geo2,
  temporal_cut_offs = NULL,
  distance_cut_offs = NULL,
  group_vect = group_vect,
  graph_opt = vimes_graph_opt(),
  ...
)
}
\arguments{
\item{dat_time}{numeric column specifying number of days since the first day of interest}

\item{dat_geo1}{first geographical coordinate}

\item{dat_geo2}{second geographical coordinate}

\item{temporal_cut_offs}{dataframe of 3 rows and 3 columns specifying temporal cut-offs
and proportions for each type of transmission. Either generated by
get_quantiles_multi or set_cutoff if user has pre-defined thresholds}

\item{distance_cut_offs}{dataframe of 3 rows and 3 columns specifying spatial cut-offs
and proportions for each type of transmission. Either generated by
get_quantiles_multi or set_cutoff if user has pre-defined thresholds}

\item{group_vect}{vector containing the group that each observation belongs to
Entered in same order as observations.}

\item{graph_opt}{graph options for igraph plots}

\item{...}{additional parameters for graph_opt}
}
\value{
a list of 4 components (add more details here)
}
\description{
Generates clusters and associated information for two groups
}
\examples{
g1_obs <- 300 # no of cases observed for species 1
g2_obs <- 200 # no of cases observed for species 2
obs <- g1_obs + g2_obs
group_vect <- as.factor(sample(c(rep("g1", g1_obs), rep("g2", g2_obs)), replace = FALSE))
dat_time <- round(runif(obs, min = 0, max = 400)) # days from start of study
time_dist <- dist(dat_time)
dat_geo1 <- runif(obs, min = 372900, max = 656900)
dat_geo2 <- runif(obs, min = 8744900, max = 9075900)
dat_dist <- as.matrix(c(dat_geo1, dat_geo2))
dat_dist <- fields::rdist(dat_dist)/1000 # convert to km
dat_x <- list(time_dist, dat_dist)
t_cut <- vimesMulti::set_cutoff("temporal", 142, 143, 143)
s_cut <- vimesMulti::set_cutoff("spatial", 2.3, 2.3, 2.3)


vimes_multi_results_list <- vimes_multi(
dat_time = dat_time,
dat_geo1 = dat_geo1,
dat_geo2 = dat_geo2,
temporal_cut_offs = t_cut,
distance_cut_offs = s_cut,
group_vect = group_vect,
graph_opt = vimes::vimes_graph_opt())

}
