# This script report was written by Alaina Pearce in January 2022 to
# examine  intake models by risk status for SSIB 2022
# (data from Food and Brain Study).
#
#     Copyright (C) 2022 Alaina L Pearce
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

############ Basic Data Load/Setup ############
# need to uncomment if running independently - not needed if compiling with 2022-01-27_PSU_CBBCsem.Rmd

# library(irr)
# library(vegan)
# library(pairwiseAdonis)
# library(rmcorr)
# source('functions.R')
# source('setup.R')

#### set up ####

# demographics check ####


#### Coding Reliability ####


## n bites
nbites_icc <- icc(ratings = data.frame(r01_micro[['nbites_c1']], r01_micro[['nbites_c2']]), model = 'twoway')
nbites_icc_ps1 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 1, 'nbites_c1'], r01_micro[r01_micro[['ps']] == 1, 'nbites_c2']), model = 'twoway')
nbites_icc_ps2 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 2, 'nbites_c1'], r01_micro[r01_micro[['ps']] == 2, 'nbites_c2']), model = 'twoway')
nbites_icc_ps3 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 3, 'nbites_c1'], r01_micro[r01_micro[['ps']] == 3, 'nbites_c2']), model = 'twoway')
nbites_icc_ps4 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 4, 'nbites_c1'], r01_micro[r01_micro[['ps']] == 4, 'nbites_c2']), model = 'twoway')

## n sips
nsips_icc <- icc(ratings = data.frame(r01_micro[['nsips_c1']], r01_micro[['nsips_c2']]), model = 'twoway')
nsips_icc_ps1 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 1, 'nsips_c1'], r01_micro[r01_micro[['ps']] == 1, 'nsips_c2']), model = 'twoway')
nsips_icc_ps2 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 2, 'nsips_c1'], r01_micro[r01_micro[['ps']] == 2, 'nsips_c2']), model = 'twoway')
nsips_icc_ps3 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 3, 'nsips_c1'], r01_micro[r01_micro[['ps']] == 3, 'nsips_c2']), model = 'twoway')
nsips_icc_ps4 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 4, 'nsips_c1'], r01_micro[r01_micro[['ps']] == 4, 'nsips_c2']), model = 'twoway')


## active eating
active_eat_icc <- icc(ratings = data.frame(r01_micro[['total_active_eating_c1']], r01_micro[['total_active_eating_c2']]), model = 'twoway')
active_eat_icc_ps1 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 1, 'total_active_eating_c1'], r01_micro[r01_micro[['ps']] == 1, 'total_active_eating_c2']), model = 'twoway')
active_eat_icc_ps2 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 2, 'total_active_eating_c1'], r01_micro[r01_micro[['ps']] == 2, 'total_active_eating_c2']), model = 'twoway')
active_eat_icc_ps3 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 3, 'total_active_eating_c1'], r01_micro[r01_micro[['ps']] == 3, 'total_active_eating_c2']), model = 'twoway')
active_eat_icc_ps4 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 4, 'total_active_eating_c1'], r01_micro[r01_micro[['ps']] == 4, 'total_active_eating_c2']), model = 'twoway')


## bite latency
bite_latency_icc <- icc(ratings = data.frame(r01_micro[['bite_latency_c1']], r01_micro[['bite_latency_c2']]), model = 'twoway')
bite_latency_icc_ps1 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 1, 'bite_latency_c1'], r01_micro[r01_micro[['ps']] == 1, 'bite_latency_c2']), model = 'twoway')
bite_latency_icc_ps2 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 2, 'bite_latency_c1'], r01_micro[r01_micro[['ps']] == 2, 'bite_latency_c2']), model = 'twoway')
bite_latency_icc_ps3 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 3, 'bite_latency_c1'], r01_micro[r01_micro[['ps']] == 3, 'bite_latency_c2']), model = 'twoway')
bite_latency_icc_ps4 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 4, 'bite_latency_c1'], r01_micro[r01_micro[['ps']] == 4, 'bite_latency_c2']), model = 'twoway')


## meal duration
meal_dur_icc <- icc(ratings = data.frame(r01_micro[['meal_duration_c1']], r01_micro[['meal_duration_c2']]), model = 'twoway')
meal_dur_icc_ps1 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 1, 'meal_duration_c1'], r01_micro[r01_micro[['ps']] == 1, 'meal_duration_c2']), model = 'twoway')
meal_dur_icc_ps2 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 2, 'meal_duration_c1'], r01_micro[r01_micro[['ps']] == 2, 'meal_duration_c2']), model = 'twoway')
meal_dur_icc_ps3 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 3, 'meal_duration_c1'], r01_micro[r01_micro[['ps']] == 3, 'meal_duration_c2']), model = 'twoway')
meal_dur_icc_ps4 <- icc(ratings = data.frame(r01_micro[r01_micro[['ps']] == 4, 'meal_duration_c1'], r01_micro[r01_micro[['ps']] == 4, 'meal_duration_c2']), model = 'twoway')

## Correlation Tables by Portion ####
micro_var_names <- c('nbites_c1', 'nsips_c1', 'total_active_eating_c1', 'bite_latency_c1', 'meal_duration_c1', 'bite_rate_c1', 'bite_rate_active_c1', 'sip_rate_c1', 'sip_rate_active_c1', 'bite_size_g_c1', 'bite_size_kcal_c1', 'eat_rate_g_c1', 'eat_rate_kcal_c1', 'eat_rate_active_g_c1','eat_rate_active_kcal_c1', 'prop_active_c1')

ps_var_names <- gsub('_c1', '', micro_var_names)

ps1_corvars <- r01_micro[r01_micro[['ps']] == 1, micro_var_names]
ps1_cormat <- cor.matrix(ps1_corvars, ps_var_names)

ps2_corvars <- r01_micro[r01_micro[['ps']] == 2, micro_var_names]
ps2_cormat <- cor.matrix(ps2_corvars, ps_var_names)

ps3_corvars <- r01_micro[r01_micro[['ps']] == 3, micro_var_names]
ps3_cormat <- cor.matrix(ps3_corvars, ps_var_names)

ps4_corvars <- r01_micro[r01_micro[['ps']] == 4, micro_var_names]
ps4_cormat <- cor.matrix(ps4_corvars, ps_var_names)

## Analysis of distance matrices - adonis ####
## decompose the changes in person-to-person eating characteristics between meals based on meal characteristics

portion.beh <- r01_micro[!(r01_micro[['ps']] == 4 & r01_micro[['sub']] == 128) & !(r01_micro[['ps']] == 2 & r01_micro[['sub']] == 83), micro_var_names]

adonis_dat <- r01_micro[!(r01_micro[['ps']] == 4 & r01_micro[['sub']] == 128) & !(r01_micro[['ps']] == 2 & r01_micro[['sub']] == 83), ]

sub_ids <- r01_micro[!(r01_micro[['ps']] == 4 & r01_micro[['sub']] == 128) & !(r01_micro[['ps']] == 2 & r01_micro[['sub']] == 83), 'sub']

ps_group <- as.factor(r01_micro[!(r01_micro[['ps']] == 4 & r01_micro[['sub']] == 128) & !(r01_micro[['ps']] == 2 & r01_micro[['sub']] == 83), 'ps'])

## check dispersion
all_beh_disp <- betadisper(dist(scale(portion.beh), method = 'euclidean'), group = as.factor(adonis_dat$ps))

## run adonis model
all_beh_psmod <- adonis2(scale(portion.beh) ~ ps, data = adonis_dat, strata = sub_ids, method="euclidean", permutations=999)

all_beh_vmod <- adonis2(scale(portion.beh) ~ visit, data = adonis_dat, strata = sub_ids, method="euclidean", permutations=999)

# post-hoc tests
all_beh_ps_pairs <- pairwise.adonis2(scale(portion.beh) ~ ps, data = adonis_dat, strata = 'sub', method='euclidean', p.adjust.m = 'fdr')

## Exploratory models by behavior ####
nbites_mod <- lmer(nbites_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
nsips_mod <- lmer(nsips_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
active_eat_mod <- lmer(total_active_eating_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
meal_dur_mod <- lmer(meal_duration_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
bite_latency_mod <- lmer(bite_latency_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
bite_rate_mod <- lmer(bite_rate_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
bite_rate_active_mod <- lmer(bite_rate_active_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
sip_rate_mod <- lmer(sip_rate_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
sip_rate_active_mod <- lmer(sip_rate_active_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
bite_size_g_mod <- lmer(bite_size_g_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
bite_size_kcal_mod <- lmer(bite_size_g_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
eat_rate_g_mod <- lmer(eat_rate_g_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
eat_rate_kcal_mod <- lmer(eat_rate_kcal_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
eat_rate_active_g_mod <- lmer(eat_rate_active_g_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
eat_rate_active_kcal_mod <- lmer(eat_rate_active_kcal_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)
prop_active_mod <- lmer(prop_active_c1~avg_vas + freddy_pre_meal + ps_order + ps + (1|sub), data = r01_micro)


## Across Portion ICC ####
icc_vars <- gsub('_c1', '', micro_var_names)
icc_vars <- gsub('total_', '', icc_vars)

ps_icc_tab <- sapply(icc_vars, function(x) icc(ratings = data.frame(r01_micro_ps[[paste0('ps1_', x)]], r01_micro_ps[[paste0('ps2_', x)]], r01_micro_ps[[paste0('ps3_', x)]], r01_micro_ps[[paste0('ps4_', x)]]), model = 'twoway', unit = 'single', type = 'agreement')$value)

## rmcorr ####
varnames_rmcorr <- names(r01_micro[micro_var_names])
varvect <- r01_micro[micro_var_names]

boot_mat <- rmcorr.matrix(varnames_rmcorr, 'sub', r01_micro[!(r01_micro[['ps']] == 4 & r01_micro[['sub']] == 128) & !(r01_micro[['ps']] == 2 & r01_micro[['sub']] == 83), ])

## Replicate 'Obesogenic' patterns of eating ####

## portion 1  - intake ####
ps1_kcal_mod <- lm(ps1_total_kcal ~ ps1_nbites + ps1_nsips + ps1_bite_size_kcal + ps1_prop_active + ps1_meal_duration + ps1_eat_rate_kcal, data = r01_micro_ps)

ps1_kcal_cov_mod <- lm(ps1_total_kcal ~ ps1_freddy_pre_meal + sex + age_yr + ps1_avg_vas + ps1_nbites + ps1_nsips + ps1_bite_size_kcal + ps1_prop_active + ps1_meal_duration + ps1_eat_rate_kcal, data = r01_micro_ps)

ps1_g_mod <- lm(ps1_total_g ~ ps1_nbites + ps1_nsips + ps1_bite_size_g + ps1_prop_active + ps1_meal_duration + ps1_eat_rate_g, data = r01_micro_ps)

ps1_g_cov_mod <- lm(ps1_total_g ~ ps1_freddy_pre_meal + sex + age_yr + ps1_avg_vas + ps1_nbites + ps1_nsips + ps1_bite_size_g + ps1_prop_active + ps1_meal_duration + ps1_eat_rate_g, data = r01_micro_ps)

ps1_g_stdmod <- lm(scale(ps1_total_g) ~ scale(ps1_nbites) + scale(ps1_nsips) + scale(ps1_bite_size_g) + scale(ps1_prop_active) + scale(ps1_meal_duration) + scale(ps1_eat_rate_g), data = r01_micro_ps)

ps1_g_cov_stdmod <- lm(scale(ps1_total_g) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(ps1_nbites) + scale(ps1_nsips) + scale(ps1_bite_size_g) + scale(ps1_prop_active) + scale(ps1_meal_duration) + scale(ps1_eat_rate_g), data = r01_micro_ps)

ps1_kcal_stdmod <- lm(scale(ps1_total_kcal) ~ scale(ps1_nbites) + scale(ps1_nsips) + scale(ps1_bite_size_kcal) + scale(ps1_prop_active) + scale(ps1_meal_duration) + scale(ps1_eat_rate_kcal), data = r01_micro_ps)

ps1_kcal_cov_stdmod <- lm(scale(ps1_total_kcal) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(ps1_nbites) + scale(ps1_nsips) + scale(ps1_bite_size_kcal) + scale(ps1_prop_active) + scale(ps1_meal_duration) + scale(ps1_eat_rate_kcal), data = r01_micro_ps)

## portion 2  - intake ####
ps2_kcal_mod <- lm(ps2_total_kcal ~ ps2_nbites + ps2_nsips + ps2_bite_size_kcal + ps2_prop_active + ps2_meal_duration + ps2_eat_rate_kcal, data = r01_micro_ps)

ps2_kcal_cov_mod <- lm(ps2_total_kcal ~ ps2_freddy_pre_meal + sex + age_yr + ps2_avg_vas + ps2_nbites + ps2_nsips + ps2_bite_size_kcal + ps2_prop_active + ps2_meal_duration + ps2_eat_rate_kcal, data = r01_micro_ps)

ps2_g_mod <- lm(ps2_total_g ~ ps2_nbites + ps2_nsips + ps2_bite_size_g + ps2_prop_active + ps2_meal_duration + ps2_eat_rate_g, data = r01_micro_ps)

ps2_g_cov_mod <- lm(ps2_total_g ~ ps2_freddy_pre_meal + sex + age_yr + ps2_avg_vas + ps2_nbites + ps2_nsips + ps2_bite_size_g + ps2_prop_active + ps2_meal_duration + ps2_eat_rate_g, data = r01_micro_ps)

ps2_g_cov_stdmod <- lm(scale(ps2_total_g) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(ps2_nbites) + scale(ps2_nsips) + scale(ps2_bite_size_g) + scale(ps2_prop_active) + scale(ps2_meal_duration) + scale(ps2_eat_rate_g), data = r01_micro_ps)

ps2_kcal_stdmod <- lm(scale(ps2_total_kcal) ~ scale(ps2_nbites) + scale(ps2_nsips) + scale(ps2_bite_size_kcal) + scale(ps2_prop_active) + scale(ps2_meal_duration) + scale(ps2_eat_rate_kcal), data = r01_micro_ps)

ps2_kcal_cov_stdmod <- lm(scale(ps2_total_kcal) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(ps2_nbites) + scale(ps2_nsips) + scale(ps2_bite_size_kcal) + scale(ps2_prop_active) + scale(ps2_meal_duration) + scale(ps2_eat_rate_kcal), data = r01_micro_ps)

## portion 3  - intake ####
ps3_kcal_mod <- lm(ps3_total_kcal ~ ps3_nbites + ps3_nsips + ps3_bite_size_kcal + ps3_prop_active + ps3_meal_duration + ps3_eat_rate_kcal, data = r01_micro_ps)

ps3_kcal_cov_mod <- lm(ps3_total_kcal ~ ps3_freddy_pre_meal + sex + age_yr + ps3_avg_vas + ps3_nbites + ps3_nsips + ps3_bite_size_kcal + ps3_prop_active + ps3_meal_duration + ps3_eat_rate_kcal, data = r01_micro_ps)

ps3_g_mod <- lm(ps3_total_g ~ ps3_nbites + ps3_nsips + ps3_bite_size_g + ps3_prop_active + ps3_meal_duration + ps3_eat_rate_g, data = r01_micro_ps)

ps3_g_cov_mod <- lm(ps3_total_g ~ ps3_freddy_pre_meal + sex + age_yr + ps3_avg_vas + ps3_nbites + ps3_nsips + ps3_bite_size_g + ps3_prop_active + ps3_meal_duration + ps3_eat_rate_g, data = r01_micro_ps)

ps3_g_cov_stdmod <- lm(scale(ps3_total_g) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(ps3_nbites) + scale(ps3_nsips) + scale(ps3_bite_size_g) + scale(ps3_prop_active) + scale(ps3_meal_duration) + scale(ps3_eat_rate_g), data = r01_micro_ps)

ps3_kcal_stdmod <- lm(scale(ps3_total_kcal) ~ scale(ps3_nbites) + scale(ps3_nsips) + scale(ps3_bite_size_kcal) + scale(ps3_prop_active) + scale(ps3_meal_duration) + scale(ps3_eat_rate_kcal), data = r01_micro_ps)

ps3_kcal_cov_stdmod <- lm(scale(ps3_total_kcal) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(ps3_nbites) + scale(ps3_nsips) + scale(ps3_bite_size_kcal) + scale(ps3_prop_active) + scale(ps3_meal_duration) + scale(ps3_eat_rate_kcal), data = r01_micro_ps)

## portion 4  - intake ####
ps4_kcal_mod <- lm(ps4_total_kcal ~ ps4_nbites + ps4_nsips + ps4_bite_size_kcal + ps4_prop_active + ps4_meal_duration + ps4_eat_rate_kcal, data = r01_micro_ps)

ps4_kcal_cov_mod <- lm(ps4_total_kcal ~ ps4_freddy_pre_meal + sex + age_yr + ps4_avg_vas + ps4_nbites + ps4_nsips + ps4_bite_size_kcal + ps4_prop_active + ps4_meal_duration + ps4_eat_rate_kcal, data = r01_micro_ps)

ps4_g_mod <- lm(ps4_total_g ~ ps4_nbites + ps4_nsips + ps4_bite_size_g + ps4_prop_active + ps4_meal_duration + ps4_eat_rate_g, data = r01_micro_ps)

ps4_g_cov_mod <- lm(ps4_total_g ~ ps4_freddy_pre_meal + sex + age_yr + ps4_avg_vas + ps4_nbites + ps4_nsips + ps4_bite_size_g + ps4_prop_active + ps4_meal_duration + ps4_eat_rate_g, data = r01_micro_ps)

ps4_g_cov_stdmod <- lm(scale(ps4_total_g) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(ps4_nbites) + scale(ps4_nsips) + scale(ps4_bite_size_g) + scale(ps4_prop_active) + scale(ps4_meal_duration) + scale(ps4_eat_rate_g), data = r01_micro_ps)

ps4_kcal_stdmod <- lm(scale(ps4_total_kcal) ~ scale(ps4_nbites) + scale(ps4_nsips) + scale(ps4_bite_size_kcal) + scale(ps4_prop_active) + scale(ps4_meal_duration) + scale(ps4_eat_rate_kcal), data = r01_micro_ps)

ps4_kcal_cov_stdmod <- lm(scale(ps4_total_kcal) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(ps4_nbites) + scale(ps4_nsips) + scale(ps4_bite_size_kcal) + scale(ps4_prop_active) + scale(ps4_meal_duration) + scale(ps4_eat_rate_kcal), data = r01_micro_ps)

## portion 1  - intake ####
ps1_bmi_kcal_mod <- lm(bmi_percentile ~ ps1_nbites + ps1_nsips + ps1_bite_size_kcal + ps1_prop_active + ps1_meal_duration + ps1_eat_rate_kcal, data = r01_micro_ps)

ps1_bmi_kcal_mod <- lm(bmi_percentile ~ ps1_freddy_pre_meal + sex + age_yr + ps1_avg_vas + ps1_nbites + ps1_nsips + ps1_bite_size_kcal + ps1_prop_active + ps1_meal_duration + ps1_eat_rate_kcal, data = r01_micro_ps)

ps1_bmi_g_mod <- lm(bmi_percentile ~ ps1_nbites + ps1_nsips + ps1_bite_size_g + ps1_prop_active + ps1_meal_duration + ps1_eat_rate_g, data = r01_micro_ps)

ps1_bmi_g_cov_mod <- lm(bmi_percentile ~ ps1_freddy_pre_meal + sex + age_yr + ps1_avg_vas + ps1_nbites + ps1_nsips + ps1_bite_size_g + ps1_prop_active + ps1_meal_duration + ps1_eat_rate_g, data = r01_micro_ps)

ps1_bmi_g_stdmod <- lm(scale(bmi_percentile) ~ scale(ps1_nbites) + scale(ps1_nsips) + scale(ps1_bite_size_g) + scale(ps1_prop_active) + scale(ps1_meal_duration) + scale(ps1_eat_rate_g), data = r01_micro_ps)

ps1_bmi_g_cov_stdmod <- lm(scale(bmi_percentile) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(ps1_nbites) + scale(ps1_nsips) + scale(ps1_bite_size_g) + scale(ps1_prop_active) + scale(ps1_meal_duration) + scale(ps1_eat_rate_g), data = r01_micro_ps)

ps1_bmi_kcal_stdmod <- lm(scale(bmi_percentile) ~ scale(ps1_nbites) + scale(ps1_nsips) + scale(ps1_bite_size_kcal) + scale(ps1_prop_active) + scale(ps1_meal_duration) + scale(ps1_eat_rate_kcal), data = r01_micro_ps)

ps1_bmi_kcal_cov_stdmod <- lm(scale(bmi_percentile) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(ps1_nbites) + scale(ps1_nsips) + scale(ps1_bite_size_kcal) + scale(ps1_prop_active) + scale(ps1_meal_duration) + scale(ps1_eat_rate_kcal), data = r01_micro_ps)


## portion 1  - intake ####
ps2_bmi_kcal_mod <- lm(bmi_percentile ~ ps2_nbites + ps2_nsips + ps2_bite_size_kcal + ps2_prop_active + ps2_meal_duration + ps2_eat_rate_kcal, data = r01_micro_ps)

ps2_bmi_kcal_mod <- lm(bmi_percentile ~ ps2_freddy_pre_meal + sex + age_yr + ps2_avg_vas + ps2_nbites + ps2_nsips + ps2_bite_size_kcal + ps2_prop_active + ps2_meal_duration + ps2_eat_rate_kcal, data = r01_micro_ps)

ps2_bmi_g_mod <- lm(bmi_percentile ~ ps2_nbites + ps2_nsips + ps2_bite_size_g + ps2_prop_active + ps2_meal_duration + ps2_eat_rate_g, data = r01_micro_ps)

ps2_bmi_g_cov_mod <- lm(bmi_percentile ~ ps2_freddy_pre_meal + sex + age_yr + ps2_avg_vas + ps2_nbites + ps2_nsips + ps2_bite_size_g + ps2_prop_active + ps2_meal_duration + ps2_eat_rate_g, data = r01_micro_ps)

ps2_bmi_g_stdmod <- lm(scale(bmi_percentile) ~ scale(ps2_nbites) + scale(ps2_nsips) + scale(ps2_bite_size_g) + scale(ps2_prop_active) + scale(ps2_meal_duration) + scale(ps2_eat_rate_g), data = r01_micro_ps)

ps2_bmi_g_cov_stdmod <- lm(scale(bmi_percentile) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(ps2_nbites) + scale(ps2_nsips) + scale(ps2_bite_size_g) + scale(ps2_prop_active) + scale(ps2_meal_duration) + scale(ps2_eat_rate_g), data = r01_micro_ps)

ps2_bmi_kcal_stdmod <- lm(scale(bmi_percentile) ~ scale(ps2_nbites) + scale(ps2_nsips) + scale(ps2_bite_size_kcal) + scale(ps2_prop_active) + scale(ps2_meal_duration) + scale(ps2_eat_rate_kcal), data = r01_micro_ps)

ps2_bmi_kcal_cov_stdmod <- lm(scale(bmi_percentile) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(ps2_nbites) + scale(ps2_nsips) + scale(ps2_bite_size_kcal) + scale(ps2_prop_active) + scale(ps2_meal_duration) + scale(ps2_eat_rate_kcal), data = r01_micro_ps)


## portion 1  - intake ####
ps3_bmi_kcal_mod <- lm(bmi_percentile ~ ps3_nbites + ps3_nsips + ps3_bite_size_kcal + ps3_prop_active + ps3_meal_duration + ps3_eat_rate_kcal, data = r01_micro_ps)

ps3_bmi_kcal_mod <- lm(bmi_percentile ~ ps3_freddy_pre_meal + sex + age_yr + ps3_avg_vas + ps3_nbites + ps3_nsips + ps3_bite_size_kcal + ps3_prop_active + ps3_meal_duration + ps3_eat_rate_kcal, data = r01_micro_ps)

ps3_bmi_g_mod <- lm(bmi_percentile ~ ps3_nbites + ps3_nsips + ps3_bite_size_g + ps3_prop_active + ps3_meal_duration + ps3_eat_rate_g, data = r01_micro_ps)

ps3_bmi_g_cov_mod <- lm(bmi_percentile ~ ps3_freddy_pre_meal + sex + age_yr + ps3_avg_vas + ps3_nbites + ps3_nsips + ps3_bite_size_g + ps3_prop_active + ps3_meal_duration + ps3_eat_rate_g, data = r01_micro_ps)

ps3_bmi_g_stdmod <- lm(scale(bmi_percentile) ~ scale(ps3_nbites) + scale(ps3_nsips) + scale(ps3_bite_size_g) + scale(ps3_prop_active) + scale(ps3_meal_duration) + scale(ps3_eat_rate_g), data = r01_micro_ps)

ps3_bmi_g_cov_stdmod <- lm(scale(bmi_percentile) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(ps3_nbites) + scale(ps3_nsips) + scale(ps3_bite_size_g) + scale(ps3_prop_active) + scale(ps3_meal_duration) + scale(ps3_eat_rate_g), data = r01_micro_ps)

ps3_bmi_kcal_stdmod <- lm(scale(bmi_percentile) ~ scale(ps3_nbites) + scale(ps3_nsips) + scale(ps3_bite_size_kcal) + scale(ps3_prop_active) + scale(ps3_meal_duration) + scale(ps3_eat_rate_kcal), data = r01_micro_ps)

ps3_bmi_kcal_cov_stdmod <- lm(scale(bmi_percentile) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(ps3_nbites) + scale(ps3_nsips) + scale(ps3_bite_size_kcal) + scale(ps3_prop_active) + scale(ps3_meal_duration) + scale(ps3_eat_rate_kcal), data = r01_micro_ps)


## portion 1  - intake ####
ps4_bmi_kcal_mod <- lm(bmi_percentile ~ ps4_nbites + ps4_nsips + ps4_bite_size_kcal + ps4_prop_active + ps4_meal_duration + ps4_eat_rate_kcal, data = r01_micro_ps)

ps4_bmi_kcal_mod <- lm(bmi_percentile ~ ps4_freddy_pre_meal + sex + age_yr + ps4_avg_vas + ps4_nbites + ps4_nsips + ps4_bite_size_kcal + ps4_prop_active + ps4_meal_duration + ps4_eat_rate_kcal, data = r01_micro_ps)

ps4_bmi_g_mod <- lm(bmi_percentile ~ ps4_nbites + ps4_nsips + ps4_bite_size_g + ps4_prop_active + ps4_meal_duration + ps4_eat_rate_g, data = r01_micro_ps)

ps4_bmi_g_cov_mod <- lm(bmi_percentile ~ ps4_freddy_pre_meal + sex + age_yr + ps4_avg_vas + ps4_nbites + ps4_nsips + ps4_bite_size_g + ps4_prop_active + ps4_meal_duration + ps4_eat_rate_g, data = r01_micro_ps)

ps4_bmi_g_stdmod <- lm(scale(bmi_percentile) ~ scale(ps4_nbites) + scale(ps4_nsips) + scale(ps4_bite_size_g) + scale(ps4_prop_active) + scale(ps4_meal_duration) + scale(ps4_eat_rate_g), data = r01_micro_ps)

ps4_bmi_g_cov_stdmod <- lm(scale(bmi_percentile) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(ps4_nbites) + scale(ps4_nsips) + scale(ps4_bite_size_g) + scale(ps4_prop_active) + scale(ps4_meal_duration) + scale(ps4_eat_rate_g), data = r01_micro_ps)

ps4_bmi_kcal_stdmod <- lm(scale(bmi_percentile) ~ scale(ps4_nbites) + scale(ps4_nsips) + scale(ps4_bite_size_kcal) + scale(ps4_prop_active) + scale(ps4_meal_duration) + scale(ps4_eat_rate_kcal), data = r01_micro_ps)

ps4_bmi_kcal_cov_stdmod <- lm(scale(bmi_percentile) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(ps4_nbites) + scale(ps4_nsips) + scale(ps4_bite_size_kcal) + scale(ps4_prop_active) + scale(ps4_meal_duration) + scale(ps4_eat_rate_kcal), data = r01_micro_ps)
