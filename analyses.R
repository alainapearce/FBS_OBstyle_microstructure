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

#respondant
nmom <- nrow(r01_micro_ps[r01_micro_ps[['parent_respondent']] == 0, ])
pmom <- nmom/nrow(r01_micro_ps)

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

## Across Portion ICC ####
icc_vars <- gsub('_c1', '', micro_var_names)
icc_vars <- gsub('total_', '', icc_vars)

ps_icc_tab <- sapply(icc_vars, function(x) icc(ratings = data.frame(r01_micro_ps[[paste0('ps1_', x)]], r01_micro_ps[[paste0('ps2_', x)]], r01_micro_ps[[paste0('ps3_', x)]], r01_micro_ps[[paste0('ps4_', x)]]), model = 'twoway', unit = 'single', type = 'agreement')$value)

## rmcorr ####
varnames_rmcorr <- names(r01_micro[micro_var_names])
varvect <- r01_micro[micro_var_names]

boot_mat <- rmcorr.matrix(varnames_rmcorr, 'sub', r01_micro[!(r01_micro[['ps']] == 4 & r01_micro[['sub']] == 128) & !(r01_micro[['ps']] == 2 & r01_micro[['sub']] == 83), ])

## PS Models ####
r01_micro$ps_prop <- ifelse(r01_micro$ps == 1, 0, ifelse(r01_micro$ps == 2, 0.33, ifelse(r01_micro$ps == 3, 0.66, 0.99)))

nbites_mod <- lmer(scale(nbites_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
nbites_sum <- summary(nbites_mod)

nsips_mod <- lmer(scale(nsips_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
nsips_sum <- summary(nsips_mod)

active_eat_mod <- lmer(scale(total_active_eating_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
active_eat_sum <- summary(active_eat_mod)

meal_dur_mod <- lmer(scale(meal_duration_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
meal_dur_sum <- summary(meal_dur_mod)

bite_latency_mod <- lmer(scale(bite_latency_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
bite_latency_sum <- summary(bite_latency_mod)

bite_rate_mod <- lmer(scale(bite_rate_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
bite_rate_sum <- summary(bite_rate_mod)

bite_rate_active_mod <- lmer(scale(bite_rate_active_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
bite_rate_active_sum <- summary(bite_rate_active_mod)

sip_rate_mod <- lmer(scale(sip_rate_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
sip_rate_sum <- summary(sip_rate_mod)

sip_rate_active_mod <- lmer(scale(sip_rate_active_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
sip_rate_active_sum <- summary(sip_rate_active_mod)

bite_size_g_mod <- lmer(scale(bite_size_g_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
bite_size_g_sum <- summary(bite_size_g_mod)

bite_size_kcal_mod <- lmer(scale(bite_size_g_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
bite_size_kcal_sum <- summary(bite_size_kcal_mod)

eat_rate_g_mod <- lmer(scale(eat_rate_g_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
eat_rate_g_sum <- summary(eat_rate_g_mod)

eat_rate_kcal_mod <- lmer(scale(eat_rate_kcal_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
eat_rate_kcal_sum <- summary(eat_rate_kcal_mod)

eat_rate_active_g_mod <- lmer(scale(eat_rate_active_g_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
eat_rate_active_g_sum <- summary(eat_rate_active_g_mod)

eat_rate_active_kcal_mod <- lmer(scale(eat_rate_active_kcal_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
eat_rate_active_kcal_sum <- summary(eat_rate_active_kcal_mod)

eat_prop_active_mod <- lmer(scale(prop_active_c1)~scale(avg_vas) + scale(freddy_pre_meal) + ps_order + ps_prop + (1|sub), data = r01_micro)
eat_prop_active_sum <- summary(eat_prop_active_mod)

ps_beh_betas <- c(nbites_sum$coefficients[5, 1], nsips_sum$coefficients[5, 1], bite_latency_sum$coefficients[5, 1], meal_dur_sum$coefficients[5, 1], active_eat_sum$coefficients[5, 1], bite_rate_sum$coefficients[5, 1], bite_rate_active_sum$coefficients[5, 1], sip_rate_sum$coefficients[5, 1], sip_rate_active_sum$coefficients[5, 1], bite_size_g_sum$coefficients[5, 1], bite_size_kcal_sum$coefficients[5, 1], eat_rate_g_sum$coefficients[5, 1], eat_rate_kcal_sum$coefficients[5, 1], eat_rate_active_g_sum$coefficients[5, 1], eat_rate_active_kcal_sum$coefficients[5, 1], eat_prop_active_sum$coefficients[5, 1])
ps_beh_betas <- data.frame(ps_beh_betas)
rownames(ps_beh_betas) <- c('Bites', 'Sips', 'Latency to 1st Bite', 'Meal Duriation', 'Active Eat Time', 'Bite Rate', 'Bite Rate Active', 'Sip Rate', 'Sip Rate Active', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Eat Rate Active, g', 'Eat Rate Active, kcal', 'Prop Active')

ps_beh_se <- c(nbites_sum$coefficients[5, 2], nsips_sum$coefficients[5, 2], bite_latency_sum$coefficients[5, 2], meal_dur_sum$coefficients[5, 2], active_eat_sum$coefficients[5, 2], bite_rate_sum$coefficients[5, 2], bite_rate_active_sum$coefficients[5, 2], sip_rate_sum$coefficients[5, 2], sip_rate_active_sum$coefficients[5, 2], bite_size_g_sum$coefficients[5, 2], bite_size_kcal_sum$coefficients[5, 2], eat_rate_g_sum$coefficients[5, 2], eat_rate_kcal_sum$coefficients[5, 2], eat_rate_active_g_sum$coefficients[5, 2], eat_rate_active_kcal_sum$coefficients[5, 2], eat_prop_active_sum$coefficients[5, 2])
ps_beh_se <- data.frame(ps_beh_se)
rownames(ps_beh_se) <- c('Bites', 'Sips', 'Latency to 1st Bite', 'Meal Duriation', 'Active Eat Time', 'Bite Rate', 'Bite Rate Active', 'Sip Rate', 'Sip Rate Active', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Eat Rate Active, g', 'Eat Rate Active, kcal', 'Prop Active')

ps_beh_padj <- p.adjust(c(nbites_sum$coefficients[5, 5], nsips_sum$coefficients[5, 5], bite_latency_sum$coefficients[5, 5], meal_dur_sum$coefficients[5, 5], active_eat_sum$coefficients[5, 5], bite_rate_sum$coefficients[5, 5], bite_rate_active_sum$coefficients[5, 5], sip_rate_sum$coefficients[5, 5], sip_rate_active_sum$coefficients[5, 5], bite_size_g_sum$coefficients[5, 5], bite_size_kcal_sum$coefficients[5, 5], eat_rate_g_sum$coefficients[5, 5], eat_rate_kcal_sum$coefficients[5, 5], eat_rate_active_g_sum$coefficients[5, 5], eat_rate_active_kcal_sum$coefficients[5, 5], eat_prop_active_sum$coefficients[5, 5]), method = 'fdr')

ps_beh_padj <- data.frame(ps_beh_padj)
rownames(ps_beh_padj) <- c('Bites', 'Sips', 'Latency to 1st Bite', 'Meal Duriation', 'Active Eat Time', 'Bite Rate', 'Bite Rate Active', 'Sip Rate', 'Sip Rate Active', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Eat Rate Active, g', 'Eat Rate Active, kcal', 'Prop Active')

## Replicate 'Obesogenic' patterns of eating ####

## portion 1  - intake ####
r01_micro_ps$sex_num <- ifelse(r01_micro_ps$sex == 'Male', 0, 1)

ps1_g_cov_stdmod <- lm(scale(ps1_total_g) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(ps1_nbites) + scale(ps1_nsips) + scale(ps1_bite_size_g) + scale(ps1_prop_active) + scale(ps1_meal_duration) + scale(ps1_eat_rate_g), data = r01_micro_ps)

ps1_g_rwa <- rwa(df = r01_micro_ps, outcome = 'ps1_total_g', predictors = c('ps1_freddy_pre_meal', 'sex_num', 'age_yr', 'ps1_avg_vas', 'ps1_nbites', 'ps1_nsips', 'ps1_bite_size_g', 'ps1_prop_active', 'ps1_meal_duration', 'ps1_eat_rate_g'))

ps1_kcal_cov_stdmod <- lm(scale(ps1_total_kcal) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(ps1_nbites) + scale(ps1_nsips) + scale(ps1_bite_size_kcal) + scale(ps1_prop_active) + scale(ps1_meal_duration) + scale(ps1_eat_rate_kcal), data = r01_micro_ps)

ps1_kcal_rwa <- rwa(df = r01_micro_ps, outcome = 'ps1_total_kcal', predictors = c('ps1_freddy_pre_meal', 'sex_num', 'age_yr', 'ps1_avg_vas', 'ps1_nbites', 'ps1_nsips', 'ps1_bite_size_kcal', 'ps1_prop_active', 'ps1_meal_duration', 'ps1_eat_rate_kcal'))


## portion 2  - intake ####

ps2_g_cov_stdmod <- lm(scale(ps2_total_g) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(ps2_nbites) + scale(ps2_nsips) + scale(ps2_bite_size_g) + scale(ps2_prop_active) + scale(ps2_meal_duration) + scale(ps2_eat_rate_g), data = r01_micro_ps)

ps2_g_rwa <- rwa(df = r01_micro_ps, outcome = 'ps2_total_g', predictors = c('ps2_freddy_pre_meal', 'sex_num', 'age_yr', 'ps2_avg_vas', 'ps2_nbites', 'ps2_nsips', 'ps2_bite_size_g', 'ps2_prop_active', 'ps2_meal_duration', 'ps2_eat_rate_g'))

ps2_kcal_cov_stdmod <- lm(scale(ps2_total_kcal) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(ps2_nbites) + scale(ps2_nsips) + scale(ps2_bite_size_kcal) + scale(ps2_prop_active) + scale(ps2_meal_duration) + scale(ps2_eat_rate_kcal), data = r01_micro_ps)

ps2_kcal_rwa <- rwa(df = r01_micro_ps, outcome = 'ps2_total_kcal', predictors = c('ps2_freddy_pre_meal', 'sex_num', 'age_yr', 'ps2_avg_vas', 'ps2_nbites', 'ps2_nsips', 'ps2_bite_size_kcal', 'ps2_prop_active', 'ps2_meal_duration', 'ps2_eat_rate_kcal'))

## portion 3  - intake ####

ps3_g_cov_stdmod <- lm(scale(ps3_total_g) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(ps3_nbites) + scale(ps3_nsips) + scale(ps3_bite_size_g) + scale(ps3_prop_active) + scale(ps3_meal_duration) + scale(ps3_eat_rate_g), data = r01_micro_ps)

ps3_g_rwa <- rwa(df = r01_micro_ps, outcome = 'ps3_total_g', predictors = c('ps3_freddy_pre_meal', 'sex_num', 'age_yr', 'ps3_avg_vas', 'ps3_nbites', 'ps3_nsips', 'ps3_bite_size_g', 'ps3_prop_active', 'ps3_meal_duration', 'ps3_eat_rate_g'))

ps3_kcal_cov_stdmod <- lm(scale(ps3_total_kcal) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(ps3_nbites) + scale(ps3_nsips) + scale(ps3_bite_size_kcal) + scale(ps3_prop_active) + scale(ps3_meal_duration) + scale(ps3_eat_rate_kcal), data = r01_micro_ps)

ps3_kcal_rwa <- rwa(df = r01_micro_ps, outcome = 'ps3_total_kcal', predictors = c('ps3_freddy_pre_meal', 'sex_num', 'age_yr', 'ps3_avg_vas', 'ps3_nbites', 'ps3_nsips', 'ps3_bite_size_kcal', 'ps3_prop_active', 'ps3_meal_duration', 'ps3_eat_rate_kcal'))

## portion 4  - intake ####

ps4_g_cov_stdmod <- lm(scale(ps4_total_g) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(ps4_nbites) + scale(ps4_nsips) + scale(ps4_bite_size_g) + scale(ps4_prop_active) + scale(ps4_meal_duration) + scale(ps4_eat_rate_g), data = r01_micro_ps)

ps4_g_rwa <- rwa(df = r01_micro_ps, outcome = 'ps4_total_g', predictors = c('ps4_freddy_pre_meal', 'sex_num', 'age_yr', 'ps4_avg_vas', 'ps4_nbites', 'ps4_nsips', 'ps4_bite_size_g', 'ps4_prop_active', 'ps4_meal_duration', 'ps4_meal_duration', 'ps4_eat_rate_g'))

ps4_kcal_cov_stdmod <- lm(scale(ps4_total_kcal) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(ps4_nbites) + scale(ps4_nsips) + scale(ps4_bite_size_kcal) + scale(ps4_prop_active) + scale(ps4_meal_duration) + scale(ps4_eat_rate_kcal), data = r01_micro_ps)

ps4_kcal_rwa <- rwa(df = r01_micro_ps, outcome = 'ps4_total_kcal', predictors = c('ps4_freddy_pre_meal', 'sex_num', 'age_yr', 'ps4_avg_vas', 'ps4_nbites', 'ps4_nsips', 'ps4_bite_size_kcal', 'ps4_prop_active', 'ps4_meal_duration', 'ps4_meal_duration', 'ps4_eat_rate_kcal'))

## portion 1  - BMI ####

ps1_bmi_bites_cov_stdmod <- lm(scale(ps1_nbites) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps1_bmi_bites_cov_sum <- summary(ps1_bmi_bites_cov_stdmod)

ps1_bmi_sips_cov_stdmod <- lm(scale(ps1_nsips) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps1_bmi_sips_cov_sum <- summary(ps1_bmi_sips_cov_stdmod)

ps1_bmi_bsize_g_cov_stdmod <- lm(scale(ps1_bite_size_g) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps1_bmi_bsize_g_cov_sum <- summary(ps1_bmi_bsize_g_cov_stdmod)


ps1_bmi_bsize_kcal_cov_stdmod <- lm(scale(ps1_bite_size_kcal) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps1_bmi_bsize_kcal_cov_sum <- summary(ps1_bmi_bsize_kcal_cov_stdmod)

ps1_bmi_propactive_cov_stdmod <- lm(scale(ps1_prop_active) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps1_bmi_propractive_cov_sum <- summary(ps1_bmi_propactive_cov_stdmod)


ps1_bmi_mealdur_cov_stdmod <- lm(scale(ps1_meal_duration) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps1_bmi_mealdur_cov_sum <- summary(ps1_bmi_mealdur_cov_stdmod)


ps1_bmi_rate_g_cov_stdmod <- lm(scale(ps1_eat_rate_g) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps1_bmi_rate_g_cov_sum <- summary(ps1_bmi_rate_g_cov_stdmod)


ps1_bmi_rate_kcal_cov_stdmod <- lm(scale(ps1_eat_rate_kcal) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps1_bmi_rate_kcal_cov_sum <- summary(ps1_bmi_rate_kcal_cov_stdmod)

ps1_bmi_betas <- c(ps1_bmi_bites_cov_sum$coefficients[6, 2], ps1_bmi_sips_cov_sum$coefficients[6, 1], ps1_bmi_mealdur_cov_sum$coefficients[6, 1], ps1_bmi_bsize_g_cov_sum$coefficients[6, 1], ps1_bmi_bsize_kcal_cov_sum$coefficients[6, 1],  ps1_bmi_rate_g_cov_sum$coefficients[6, 1], ps1_bmi_rate_kcal_cov_sum$coefficients[6, 1], ps1_bmi_propractive_cov_sum$coefficients[6, 1])
ps1_bmi_betas <- data.frame(ps1_bmi_betas)
rownames(ps1_bmi_betas) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps1_bmi_se <- c(ps1_bmi_bites_cov_sum$coefficients[6, 2], ps1_bmi_sips_cov_sum$coefficients[6, 2], ps1_bmi_mealdur_cov_sum$coefficients[6, 2], ps1_bmi_bsize_g_cov_sum$coefficients[6, 2], ps1_bmi_bsize_kcal_cov_sum$coefficients[6, 2],  ps1_bmi_rate_g_cov_sum$coefficients[6, 2], ps1_bmi_rate_kcal_cov_sum$coefficients[6, 2], ps1_bmi_propractive_cov_sum$coefficients[6, 2])
ps1_bmi_se <- data.frame(ps1_bmi_se)
rownames(ps1_bmi_se) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps1_bmi_adj <- p.adjust(c(ps1_bmi_bites_cov_sum$coefficients[6, 4], ps1_bmi_sips_cov_sum$coefficients[6, 4], ps1_bmi_mealdur_cov_sum$coefficients[6, 4], ps1_bmi_bsize_g_cov_sum$coefficients[6, 4], ps1_bmi_bsize_kcal_cov_sum$coefficients[6, 4], ps1_bmi_rate_g_cov_sum$coefficients[6, 4], ps1_bmi_rate_kcal_cov_sum$coefficients[6, 4], ps1_bmi_propractive_cov_sum$coefficients[6, 4]), method = 'fdr')

ps1_bmi_adj <- data.frame(ps1_bmi_adj)
rownames(ps1_bmi_adj) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

## portion 2  - BMI ####

ps2_bmi_bites_cov_stdmod <- lm(scale(ps2_nbites) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps2_bmi_bites_cov_sum <- summary(ps2_bmi_bites_cov_stdmod)

ps2_bmi_sips_cov_stdmod <- lm(scale(ps2_nsips) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps2_bmi_sips_cov_sum <- summary(ps2_bmi_sips_cov_stdmod)

ps2_bmi_bsize_g_cov_stdmod <- lm(scale(ps2_bite_size_g) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps2_bmi_bsize_g_cov_sum <- summary(ps2_bmi_bsize_g_cov_stdmod)


ps2_bmi_bsize_kcal_cov_stdmod <- lm(scale(ps2_bite_size_kcal) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps2_bmi_bsize_kcal_cov_sum <- summary(ps2_bmi_bsize_kcal_cov_stdmod)

ps2_bmi_propactive_cov_stdmod <- lm(scale(ps2_prop_active) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps2_bmi_propractive_cov_sum <- summary(ps2_bmi_propactive_cov_stdmod)


ps2_bmi_mealdur_cov_stdmod <- lm(scale(ps2_meal_duration) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps2_bmi_mealdur_cov_sum <- summary(ps2_bmi_mealdur_cov_stdmod)


ps2_bmi_rate_g_cov_stdmod <- lm(scale(ps2_eat_rate_g) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps2_bmi_rate_g_cov_sum <- summary(ps2_bmi_rate_g_cov_stdmod)


ps2_bmi_rate_kcal_cov_stdmod <- lm(scale(ps2_eat_rate_kcal) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps2_bmi_rate_kcal_cov_sum <- summary(ps2_bmi_rate_kcal_cov_stdmod)

ps2_bmi_betas <- c(ps2_bmi_bites_cov_sum$coefficients[6, 2], ps2_bmi_sips_cov_sum$coefficients[6, 1], ps2_bmi_mealdur_cov_sum$coefficients[6, 1], ps2_bmi_bsize_g_cov_sum$coefficients[6, 1], ps2_bmi_bsize_kcal_cov_sum$coefficients[6, 1],  ps2_bmi_rate_g_cov_sum$coefficients[6, 1], ps2_bmi_rate_kcal_cov_sum$coefficients[6, 1], ps2_bmi_propractive_cov_sum$coefficients[6, 1])
ps2_bmi_betas <- data.frame(ps2_bmi_betas)
rownames(ps2_bmi_betas) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps2_bmi_se <- c(ps2_bmi_bites_cov_sum$coefficients[6, 2], ps2_bmi_sips_cov_sum$coefficients[6, 2], ps2_bmi_mealdur_cov_sum$coefficients[6, 2], ps2_bmi_bsize_g_cov_sum$coefficients[6, 2], ps2_bmi_bsize_kcal_cov_sum$coefficients[6, 2],  ps2_bmi_rate_g_cov_sum$coefficients[6, 2], ps2_bmi_rate_kcal_cov_sum$coefficients[6, 2], ps2_bmi_propractive_cov_sum$coefficients[6, 2])
ps2_bmi_se <- data.frame(ps2_bmi_se)
rownames(ps2_bmi_se) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps2_bmi_adj <- p.adjust(c(ps2_bmi_bites_cov_sum$coefficients[6, 4], ps2_bmi_sips_cov_sum$coefficients[6, 4], ps2_bmi_mealdur_cov_sum$coefficients[6, 4], ps2_bmi_bsize_g_cov_sum$coefficients[6, 4], ps2_bmi_bsize_kcal_cov_sum$coefficients[6, 4], ps2_bmi_rate_g_cov_sum$coefficients[6, 4], ps2_bmi_rate_kcal_cov_sum$coefficients[6, 4], ps2_bmi_propractive_cov_sum$coefficients[6, 4]), method = 'fdr')

ps2_bmi_adj <- data.frame(ps2_bmi_adj)
rownames(ps2_bmi_adj) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

## portion 3  - BMI ####

ps3_bmi_bites_cov_stdmod <- lm(scale(ps3_nbites) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps3_bmi_bites_cov_sum <- summary(ps3_bmi_bites_cov_stdmod)

ps3_bmi_sips_cov_stdmod <- lm(scale(ps3_nsips) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps3_bmi_sips_cov_sum <- summary(ps3_bmi_sips_cov_stdmod)

ps3_bmi_bsize_g_cov_stdmod <- lm(scale(ps3_bite_size_g) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps3_bmi_bsize_g_cov_sum <- summary(ps3_bmi_bsize_g_cov_stdmod)


ps3_bmi_bsize_kcal_cov_stdmod <- lm(scale(ps3_bite_size_kcal) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps3_bmi_bsize_kcal_cov_sum <- summary(ps3_bmi_bsize_kcal_cov_stdmod)

ps3_bmi_propactive_cov_stdmod <- lm(scale(ps3_prop_active) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps3_bmi_propractive_cov_sum <- summary(ps3_bmi_propactive_cov_stdmod)


ps3_bmi_mealdur_cov_stdmod <- lm(scale(ps3_meal_duration) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps3_bmi_mealdur_cov_sum <- summary(ps3_bmi_mealdur_cov_stdmod)


ps3_bmi_rate_g_cov_stdmod <- lm(scale(ps3_eat_rate_g) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps3_bmi_rate_g_cov_sum <- summary(ps3_bmi_rate_g_cov_stdmod)


ps3_bmi_rate_kcal_cov_stdmod <- lm(scale(ps3_eat_rate_kcal) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps3_bmi_rate_kcal_cov_sum <- summary(ps3_bmi_rate_kcal_cov_stdmod)

ps3_bmi_betas <- c(ps3_bmi_bites_cov_sum$coefficients[6, 2], ps3_bmi_sips_cov_sum$coefficients[6, 1], ps3_bmi_mealdur_cov_sum$coefficients[6, 1], ps3_bmi_bsize_g_cov_sum$coefficients[6, 1], ps3_bmi_bsize_kcal_cov_sum$coefficients[6, 1],  ps3_bmi_rate_g_cov_sum$coefficients[6, 1], ps3_bmi_rate_kcal_cov_sum$coefficients[6, 1], ps3_bmi_propractive_cov_sum$coefficients[6, 1])
ps3_bmi_betas <- data.frame(ps3_bmi_betas)
rownames(ps3_bmi_betas) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps3_bmi_se <- c(ps3_bmi_bites_cov_sum$coefficients[6, 2], ps3_bmi_sips_cov_sum$coefficients[6, 2], ps3_bmi_mealdur_cov_sum$coefficients[6, 2], ps3_bmi_bsize_g_cov_sum$coefficients[6, 2], ps3_bmi_bsize_kcal_cov_sum$coefficients[6, 2],  ps3_bmi_rate_g_cov_sum$coefficients[6, 2], ps3_bmi_rate_kcal_cov_sum$coefficients[6, 2], ps3_bmi_propractive_cov_sum$coefficients[6, 2])
ps3_bmi_se <- data.frame(ps3_bmi_se)
rownames(ps3_bmi_se) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps3_bmi_adj <- p.adjust(c(ps3_bmi_bites_cov_sum$coefficients[6, 4], ps3_bmi_sips_cov_sum$coefficients[6, 4], ps3_bmi_mealdur_cov_sum$coefficients[6, 4], ps3_bmi_bsize_g_cov_sum$coefficients[6, 4], ps3_bmi_bsize_kcal_cov_sum$coefficients[6, 4], ps3_bmi_rate_g_cov_sum$coefficients[6, 4], ps3_bmi_rate_kcal_cov_sum$coefficients[6, 4], ps3_bmi_propractive_cov_sum$coefficients[6, 4]), method = 'fdr')

ps3_bmi_adj <- data.frame(ps3_bmi_adj)
rownames(ps3_bmi_adj) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

## portion 4  - BMI ####

ps4_bmi_bites_cov_stdmod <- lm(scale(ps4_nbites) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps4_bmi_bites_cov_sum <- summary(ps4_bmi_bites_cov_stdmod)

ps4_bmi_sips_cov_stdmod <- lm(scale(ps4_nsips) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps4_bmi_sips_cov_sum <- summary(ps4_bmi_sips_cov_stdmod)

ps4_bmi_bsize_g_cov_stdmod <- lm(scale(ps4_bite_size_g) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps4_bmi_bsize_g_cov_sum <- summary(ps4_bmi_bsize_g_cov_stdmod)


ps4_bmi_bsize_kcal_cov_stdmod <- lm(scale(ps4_bite_size_kcal) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps4_bmi_bsize_kcal_cov_sum <- summary(ps4_bmi_bsize_kcal_cov_stdmod)

ps4_bmi_propactive_cov_stdmod <- lm(scale(ps4_prop_active) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps4_bmi_propractive_cov_sum <- summary(ps4_bmi_propactive_cov_stdmod)


ps4_bmi_mealdur_cov_stdmod <- lm(scale(ps4_meal_duration) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps4_bmi_mealdur_cov_sum <- summary(ps4_bmi_mealdur_cov_stdmod)


ps4_bmi_rate_g_cov_stdmod <- lm(scale(ps4_eat_rate_g) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps4_bmi_rate_g_cov_sum <- summary(ps4_bmi_rate_g_cov_stdmod)


ps4_bmi_rate_kcal_cov_stdmod <- lm(scale(ps4_eat_rate_kcal) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(bmi_percentile), data = r01_micro_ps)
ps4_bmi_rate_kcal_cov_sum <- summary(ps4_bmi_rate_kcal_cov_stdmod)


## BMI padjust
ps4_bmi_betas <- c(ps4_bmi_bites_cov_sum$coefficients[6, 2], ps4_bmi_sips_cov_sum$coefficients[6, 1], ps4_bmi_mealdur_cov_sum$coefficients[6, 1], ps4_bmi_bsize_g_cov_sum$coefficients[6, 1], ps4_bmi_bsize_kcal_cov_sum$coefficients[6, 1],  ps4_bmi_rate_g_cov_sum$coefficients[6, 1], ps4_bmi_rate_kcal_cov_sum$coefficients[6, 1], ps4_bmi_propractive_cov_sum$coefficients[6, 1])
ps4_bmi_betas <- data.frame(ps4_bmi_betas)
rownames(ps4_bmi_betas) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps4_bmi_se <- c(ps4_bmi_bites_cov_sum$coefficients[6, 2], ps4_bmi_sips_cov_sum$coefficients[6, 2], ps4_bmi_mealdur_cov_sum$coefficients[6, 2], ps4_bmi_bsize_g_cov_sum$coefficients[6, 2], ps4_bmi_bsize_kcal_cov_sum$coefficients[6, 2],  ps4_bmi_rate_g_cov_sum$coefficients[6, 2], ps4_bmi_rate_kcal_cov_sum$coefficients[6, 2], ps4_bmi_propractive_cov_sum$coefficients[6, 2])
ps4_bmi_se <- data.frame(ps4_bmi_se)
rownames(ps4_bmi_se) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps4_bmi_adj <- p.adjust(c(ps4_bmi_bites_cov_sum$coefficients[6, 4], ps4_bmi_sips_cov_sum$coefficients[6, 4], ps4_bmi_mealdur_cov_sum$coefficients[6, 4], ps4_bmi_bsize_g_cov_sum$coefficients[6, 4], ps4_bmi_bsize_kcal_cov_sum$coefficients[6, 4], ps4_bmi_rate_g_cov_sum$coefficients[6, 4], ps4_bmi_rate_kcal_cov_sum$coefficients[6, 4], ps4_bmi_propractive_cov_sum$coefficients[6, 4]), method = 'fdr')

ps4_bmi_adj <- data.frame(ps4_bmi_adj)
rownames(ps4_bmi_adj) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

## portion 1  - FMI ####

ps1_fmi_bites_cov_stdmod <- lm(scale(ps1_nbites) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(fmi), data = r01_micro_ps)
ps1_fmi_bites_cov_sum <- summary(ps1_fmi_bites_cov_stdmod)

ps1_fmi_sips_cov_stdmod <- lm(scale(ps1_nsips) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(fmi), data = r01_micro_ps)
ps1_fmi_sips_cov_sum <- summary(ps1_fmi_sips_cov_stdmod)

ps1_fmi_bsize_g_cov_stdmod <- lm(scale(ps1_bite_size_g) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(fmi), data = r01_micro_ps)
ps1_fmi_bsize_g_cov_sum <- summary(ps1_fmi_bsize_g_cov_stdmod)

ps1_fmi_bsize_kcal_cov_stdmod <- lm(scale(ps1_bite_size_kcal) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(fmi), data = r01_micro_ps)
ps1_fmi_bsize_kcal_cov_sum <- summary(ps1_fmi_bsize_kcal_cov_stdmod)

ps1_fmi_propactive_cov_stdmod <- lm(scale(ps1_prop_active) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(fmi), data = r01_micro_ps)
ps1_fmi_propractive_cov_sum <- summary(ps1_fmi_propactive_cov_stdmod)


ps1_fmi_mealdur_cov_stdmod <- lm(scale(ps1_meal_duration) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(fmi), data = r01_micro_ps)
ps1_fmi_mealdur_cov_sum <- summary(ps1_fmi_mealdur_cov_stdmod)


ps1_fmi_rate_g_cov_stdmod <- lm(scale(ps1_eat_rate_g) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(fmi), data = r01_micro_ps)
ps1_fmi_rate_g_cov_sum <- summary(ps1_fmi_rate_g_cov_stdmod)


ps1_fmi_rate_kcal_cov_stdmod <- lm(scale(ps1_eat_rate_kcal) ~ scale(ps1_freddy_pre_meal) + sex + scale(age_yr) + scale(ps1_avg_vas) + scale(fmi), data = r01_micro_ps)
ps1_fmi_rate_kcal_cov_sum <- summary(ps1_fmi_rate_kcal_cov_stdmod)

ps1_fmi_betas <- c(ps1_fmi_bites_cov_sum$coefficients[6, 2], ps1_fmi_sips_cov_sum$coefficients[6, 1], ps1_fmi_mealdur_cov_sum$coefficients[6, 1], ps1_fmi_bsize_g_cov_sum$coefficients[6, 1], ps1_fmi_bsize_kcal_cov_sum$coefficients[6, 1],  ps1_fmi_rate_g_cov_sum$coefficients[6, 1], ps1_fmi_rate_kcal_cov_sum$coefficients[6, 1], ps1_fmi_propractive_cov_sum$coefficients[6, 1])
ps1_fmi_betas <- data.frame(ps1_fmi_betas)
rownames(ps1_fmi_betas) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps1_fmi_se <- c(ps1_fmi_bites_cov_sum$coefficients[6, 2], ps1_fmi_sips_cov_sum$coefficients[6, 2], ps1_fmi_mealdur_cov_sum$coefficients[6, 2], ps1_fmi_bsize_g_cov_sum$coefficients[6, 2], ps1_fmi_bsize_kcal_cov_sum$coefficients[6, 2],  ps1_fmi_rate_g_cov_sum$coefficients[6, 2], ps1_fmi_rate_kcal_cov_sum$coefficients[6, 2], ps1_fmi_propractive_cov_sum$coefficients[6, 2])
ps1_fmi_se <- data.frame(ps1_fmi_se)
rownames(ps1_fmi_se) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps1_fmi_adj <- p.adjust(c(ps1_fmi_bites_cov_sum$coefficients[6, 4], ps1_fmi_sips_cov_sum$coefficients[6, 4], ps1_fmi_mealdur_cov_sum$coefficients[6, 4], ps1_fmi_bsize_g_cov_sum$coefficients[6, 4], ps1_fmi_bsize_kcal_cov_sum$coefficients[6, 4], ps1_fmi_rate_g_cov_sum$coefficients[6, 4], ps1_fmi_rate_kcal_cov_sum$coefficients[6, 4], ps1_fmi_propractive_cov_sum$coefficients[6, 4]), method = 'fdr')

ps1_fmi_adj <- data.frame(ps1_fmi_adj)
rownames(ps1_fmi_adj) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

## portion 2  - FMI ####

ps2_fmi_bites_cov_stdmod <- lm(scale(ps2_nbites) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(fmi), data = r01_micro_ps)
ps2_fmi_bites_cov_sum <- summary(ps2_fmi_bites_cov_stdmod)

ps2_fmi_sips_cov_stdmod <- lm(scale(ps2_nsips) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(fmi), data = r01_micro_ps)
ps2_fmi_sips_cov_sum <- summary(ps2_fmi_sips_cov_stdmod)

ps2_fmi_bsize_g_cov_stdmod <- lm(scale(ps2_bite_size_g) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(fmi), data = r01_micro_ps)
ps2_fmi_bsize_g_cov_sum <- summary(ps2_fmi_bsize_g_cov_stdmod)


ps2_fmi_bsize_kcal_cov_stdmod <- lm(scale(ps2_bite_size_kcal) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(fmi), data = r01_micro_ps)
ps2_fmi_bsize_kcal_cov_sum <- summary(ps2_fmi_bsize_kcal_cov_stdmod)

ps2_fmi_propactive_cov_stdmod <- lm(scale(ps2_prop_active) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(fmi), data = r01_micro_ps)
ps2_fmi_propractive_cov_sum <- summary(ps2_fmi_propactive_cov_stdmod)


ps2_fmi_mealdur_cov_stdmod <- lm(scale(ps2_meal_duration) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(fmi), data = r01_micro_ps)
ps2_fmi_mealdur_cov_sum <- summary(ps2_fmi_mealdur_cov_stdmod)


ps2_fmi_rate_g_cov_stdmod <- lm(scale(ps2_eat_rate_g) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(fmi), data = r01_micro_ps)
ps2_fmi_rate_g_cov_sum <- summary(ps2_fmi_rate_g_cov_stdmod)


ps2_fmi_rate_kcal_cov_stdmod <- lm(scale(ps2_eat_rate_kcal) ~ scale(ps2_freddy_pre_meal) + sex + scale(age_yr) + scale(ps2_avg_vas) + scale(fmi), data = r01_micro_ps)
ps2_fmi_rate_kcal_cov_sum <- summary(ps2_fmi_rate_kcal_cov_stdmod)

ps2_fmi_betas <- c(ps2_fmi_bites_cov_sum$coefficients[6, 2], ps2_fmi_sips_cov_sum$coefficients[6, 1], ps2_fmi_mealdur_cov_sum$coefficients[6, 1], ps2_fmi_bsize_g_cov_sum$coefficients[6, 1], ps2_fmi_bsize_kcal_cov_sum$coefficients[6, 1],  ps2_fmi_rate_g_cov_sum$coefficients[6, 1], ps2_fmi_rate_kcal_cov_sum$coefficients[6, 1], ps2_fmi_propractive_cov_sum$coefficients[6, 1])
ps2_fmi_betas <- data.frame(ps2_fmi_betas)
rownames(ps2_fmi_betas) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps2_fmi_se <- c(ps2_fmi_bites_cov_sum$coefficients[6, 2], ps2_fmi_sips_cov_sum$coefficients[6, 2], ps2_fmi_mealdur_cov_sum$coefficients[6, 2], ps2_fmi_bsize_g_cov_sum$coefficients[6, 2], ps2_fmi_bsize_kcal_cov_sum$coefficients[6, 2],  ps2_fmi_rate_g_cov_sum$coefficients[6, 2], ps2_fmi_rate_kcal_cov_sum$coefficients[6, 2], ps2_fmi_propractive_cov_sum$coefficients[6, 2])
ps2_fmi_se <- data.frame(ps2_fmi_se)
rownames(ps2_fmi_se) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps2_fmi_adj <- p.adjust(c(ps2_fmi_bites_cov_sum$coefficients[6, 4], ps2_fmi_sips_cov_sum$coefficients[6, 4], ps2_fmi_mealdur_cov_sum$coefficients[6, 4], ps2_fmi_bsize_g_cov_sum$coefficients[6, 4], ps2_fmi_bsize_kcal_cov_sum$coefficients[6, 4], ps2_fmi_rate_g_cov_sum$coefficients[6, 4], ps2_fmi_rate_kcal_cov_sum$coefficients[6, 4], ps2_fmi_propractive_cov_sum$coefficients[6, 4]), method = 'fdr')

ps2_fmi_adj <- data.frame(ps2_fmi_adj)
rownames(ps2_fmi_adj) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

## portion 3  - FMI ####

ps3_fmi_bites_cov_stdmod <- lm(scale(ps3_nbites) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(fmi), data = r01_micro_ps)
ps3_fmi_bites_cov_sum <- summary(ps3_fmi_bites_cov_stdmod)

ps3_fmi_sips_cov_stdmod <- lm(scale(ps3_nsips) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(fmi), data = r01_micro_ps)
ps3_fmi_sips_cov_sum <- summary(ps3_fmi_sips_cov_stdmod)

ps3_fmi_bsize_g_cov_stdmod <- lm(scale(ps3_bite_size_g) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(fmi), data = r01_micro_ps)
ps3_fmi_bsize_g_cov_sum <- summary(ps3_fmi_bsize_g_cov_stdmod)


ps3_fmi_bsize_kcal_cov_stdmod <- lm(scale(ps3_bite_size_kcal) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(fmi), data = r01_micro_ps)
ps3_fmi_bsize_kcal_cov_sum <- summary(ps3_fmi_bsize_kcal_cov_stdmod)

ps3_fmi_propactive_cov_stdmod <- lm(scale(ps3_prop_active) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(fmi), data = r01_micro_ps)
ps3_fmi_propractive_cov_sum <- summary(ps3_fmi_propactive_cov_stdmod)


ps3_fmi_mealdur_cov_stdmod <- lm(scale(ps3_meal_duration) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(fmi), data = r01_micro_ps)
ps3_fmi_mealdur_cov_sum <- summary(ps3_fmi_mealdur_cov_stdmod)


ps3_fmi_rate_g_cov_stdmod <- lm(scale(ps3_eat_rate_g) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(fmi), data = r01_micro_ps)
ps3_fmi_rate_g_cov_sum <- summary(ps3_fmi_rate_g_cov_stdmod)


ps3_fmi_rate_kcal_cov_stdmod <- lm(scale(ps3_eat_rate_kcal) ~ scale(ps3_freddy_pre_meal) + sex + scale(age_yr) + scale(ps3_avg_vas) + scale(fmi), data = r01_micro_ps)
ps3_fmi_rate_kcal_cov_sum <- summary(ps3_fmi_rate_kcal_cov_stdmod)

ps3_fmi_betas <- c(ps3_fmi_bites_cov_sum$coefficients[6, 2], ps3_fmi_sips_cov_sum$coefficients[6, 1], ps3_fmi_mealdur_cov_sum$coefficients[6, 1], ps3_fmi_bsize_g_cov_sum$coefficients[6, 1], ps3_fmi_bsize_kcal_cov_sum$coefficients[6, 1],  ps3_fmi_rate_g_cov_sum$coefficients[6, 1], ps3_fmi_rate_kcal_cov_sum$coefficients[6, 1], ps3_fmi_propractive_cov_sum$coefficients[6, 1])
ps3_fmi_betas <- data.frame(ps3_fmi_betas)
rownames(ps3_fmi_betas) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps3_fmi_se <- c(ps3_fmi_bites_cov_sum$coefficients[6, 2], ps3_fmi_sips_cov_sum$coefficients[6, 2], ps3_fmi_mealdur_cov_sum$coefficients[6, 2], ps3_fmi_bsize_g_cov_sum$coefficients[6, 2], ps3_fmi_bsize_kcal_cov_sum$coefficients[6, 2],  ps3_fmi_rate_g_cov_sum$coefficients[6, 2], ps3_fmi_rate_kcal_cov_sum$coefficients[6, 2], ps3_fmi_propractive_cov_sum$coefficients[6, 2])
ps3_fmi_se <- data.frame(ps3_fmi_se)
rownames(ps3_fmi_se) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps3_fmi_adj <- p.adjust(c(ps3_fmi_bites_cov_sum$coefficients[6, 4], ps3_fmi_sips_cov_sum$coefficients[6, 4], ps3_fmi_mealdur_cov_sum$coefficients[6, 4], ps3_fmi_bsize_g_cov_sum$coefficients[6, 4], ps3_fmi_bsize_kcal_cov_sum$coefficients[6, 4], ps3_fmi_rate_g_cov_sum$coefficients[6, 4], ps3_fmi_rate_kcal_cov_sum$coefficients[6, 4], ps3_fmi_propractive_cov_sum$coefficients[6, 4]), method = 'fdr')

ps3_fmi_adj <- data.frame(ps3_fmi_adj)
rownames(ps3_fmi_adj) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

## portion 4  - FMI ####

ps4_fmi_bites_cov_stdmod <- lm(scale(ps4_nbites) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(fmi), data = r01_micro_ps)
ps4_fmi_bites_cov_sum <- summary(ps4_fmi_bites_cov_stdmod)

ps4_fmi_sips_cov_stdmod <- lm(scale(ps4_nsips) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(fmi), data = r01_micro_ps)
ps4_fmi_sips_cov_sum <- summary(ps4_fmi_sips_cov_stdmod)

ps4_fmi_bsize_g_cov_stdmod <- lm(scale(ps4_bite_size_g) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(fmi), data = r01_micro_ps)
ps4_fmi_bsize_g_cov_sum <- summary(ps4_fmi_bsize_g_cov_stdmod)


ps4_fmi_bsize_kcal_cov_stdmod <- lm(scale(ps4_bite_size_kcal) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(fmi), data = r01_micro_ps)
ps4_fmi_bsize_kcal_cov_sum <- summary(ps4_fmi_bsize_kcal_cov_stdmod)

ps4_fmi_propactive_cov_stdmod <- lm(scale(ps4_prop_active) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(fmi), data = r01_micro_ps)
ps4_fmi_propractive_cov_sum <- summary(ps4_fmi_propactive_cov_stdmod)


ps4_fmi_mealdur_cov_stdmod <- lm(scale(ps4_meal_duration) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(fmi), data = r01_micro_ps)
ps4_fmi_mealdur_cov_sum <- summary(ps4_fmi_mealdur_cov_stdmod)


ps4_fmi_rate_g_cov_stdmod <- lm(scale(ps4_eat_rate_g) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(fmi), data = r01_micro_ps)
ps4_fmi_rate_g_cov_sum <- summary(ps4_fmi_rate_g_cov_stdmod)


ps4_fmi_rate_kcal_cov_stdmod <- lm(scale(ps4_eat_rate_kcal) ~ scale(ps4_freddy_pre_meal) + sex + scale(age_yr) + scale(ps4_avg_vas) + scale(fmi), data = r01_micro_ps)
ps4_fmi_rate_kcal_cov_sum <- summary(ps4_fmi_rate_kcal_cov_stdmod)

ps4_fmi_betas <- c(ps4_fmi_bites_cov_sum$coefficients[6, 2], ps4_fmi_sips_cov_sum$coefficients[6, 1], ps4_fmi_mealdur_cov_sum$coefficients[6, 1], ps4_fmi_bsize_g_cov_sum$coefficients[6, 1], ps4_fmi_bsize_kcal_cov_sum$coefficients[6, 1],  ps4_fmi_rate_g_cov_sum$coefficients[6, 1], ps4_fmi_rate_kcal_cov_sum$coefficients[6, 1], ps4_fmi_propractive_cov_sum$coefficients[6, 1])
ps4_fmi_betas <- data.frame(ps4_fmi_betas)
rownames(ps4_fmi_betas) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps4_fmi_se <- c(ps4_fmi_bites_cov_sum$coefficients[6, 2], ps4_fmi_sips_cov_sum$coefficients[6, 2], ps4_fmi_mealdur_cov_sum$coefficients[6, 2], ps4_fmi_bsize_g_cov_sum$coefficients[6, 2], ps4_fmi_bsize_kcal_cov_sum$coefficients[6, 2],  ps4_fmi_rate_g_cov_sum$coefficients[6, 2], ps4_fmi_rate_kcal_cov_sum$coefficients[6, 2], ps4_fmi_propractive_cov_sum$coefficients[6, 2])
ps4_fmi_se <- data.frame(ps4_fmi_se)
rownames(ps4_fmi_se) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')

ps4_fmi_adj <- p.adjust(c(ps4_fmi_bites_cov_sum$coefficients[6, 4], ps4_fmi_sips_cov_sum$coefficients[6, 4], ps4_fmi_mealdur_cov_sum$coefficients[6, 4], ps4_fmi_bsize_g_cov_sum$coefficients[6, 4], ps4_fmi_bsize_kcal_cov_sum$coefficients[6, 4], ps4_fmi_rate_g_cov_sum$coefficients[6, 4], ps4_fmi_rate_kcal_cov_sum$coefficients[6, 4], ps4_fmi_propractive_cov_sum$coefficients[6, 4]), method = 'fdr')

ps4_fmi_adj <- data.frame(ps4_fmi_adj)
rownames(ps4_fmi_adj) <- c('Bites', 'Sips', 'Meal Duration', 'Bite Size, g', 'Bite Size, kcal', 'Eat Rate, g', 'Eat Rate, kcal', 'Prop Active')
