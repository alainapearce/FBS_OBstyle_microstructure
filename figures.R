# This script was written by Alaina Pearce in February 2023
# to set up figures for the a paper examining meal microstructure
# across portion size meals
#
#     Copyright (C) 2023 Alaina L Pearce
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

# library(ggplot2)
# library(vegan)

# source('setup.R')

#### set up ####
r01_micro_ps <- read.csv('data/micro_ps_compiled.csv')
r01_micro <- read.csv('data/micro_compiled.csv')

## Behavior across PS models ####
# ps_beh_models_fig <- bar_graph.se(ps_beh_betas$ps_beh_betas, er = ps_beh_se$ps_beh_se, xlab = 'Behavior', ylab = 'Standardized Coefficients for Portion Size', ymax = 0.6, ymin = -0.6, group = 0)

## intake across PS ####
rownames_intake <- c('Bites', 'Sips', 'Bite Size, g', 'Prop Active', 'Meal Duration', 'Eat Rate, g')
ps_g_betas <- rbind(summary(ps1_g_cov_stdmod)$coefficients[6:11], summary(ps2_g_cov_stdmod)$coefficients[6:11], summary(ps3_g_cov_stdmod)$coefficients[6:11], summary(ps4_g_cov_stdmod)$coefficients[6:11])

ps_g_se <- rbind(summary(ps1_g_cov_stdmod)$coefficients[6:11, 2], summary(ps2_g_cov_stdmod)$coefficients[6:11, 2], summary(ps3_g_cov_stdmod)$coefficients[6:11, 2], summary(ps4_g_cov_stdmod)$coefficients[6:11, 2])

ps_kcal_betas <-rbind(summary(ps1_kcal_cov_stdmod)$coefficients[6:11], summary(ps2_kcal_cov_stdmod)$coefficients[6:11], summary(ps3_kcal_cov_stdmod)$coefficients[6:11], summary(ps4_kcal_cov_stdmod)$coefficients[6:11])

ps_kcal_se <-rbind(summary(ps1_kcal_cov_stdmod)$coefficients[6:11, 2], summary(ps2_kcal_cov_stdmod)$coefficients[6:11, 2], summary(ps3_kcal_cov_stdmod)$coefficients[6:11, 2], summary(ps4_kcal_cov_stdmod)$coefficients[6:11, 2])


psxbeh_intake <- data.frame(c(rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6)), rep(rownames_intake, 4))
names(psxbeh_intake) <- c('Portion', 'Behavior')
psxbeh_intake$Portion <- as.factor(psxbeh_intake$Portion)
psxbeh_intake$Behavior <- as.factor(psxbeh_intake$Behavior)

ps_g_model_fig <- bar_graph.se(means = ps_g_betas, er = ps_g_se, xlab = 'Behavior', ylab = 'Standardized Coefficients for Inkate, g', ymax = 1, ymin = -0.2, group = psxbeh_intake$Portion)

ps_kcal_model_fig <- bar_graph.se(means = ps_kcal_betas, er = ps_kcal_se, xlab = 'Behavior', ylab = 'Standardized Coefficients for Inkate, kcal', ymax = 1, ymin = -0.2, group = psxbeh_intake$Portion)

## BMI across PS ####
ps_bmi_betas <- rbind(ps1_bmi_betas$ps1_bmi_betas, ps2_bmi_betas$ps2_bmi_betas, ps3_bmi_betas$ps3_bmi_betas, ps4_bmi_betas$ps4_bmi_betas)

ps_bmi_se <- rbind(ps1_bmi_se$ps1_bmi_se, ps2_bmi_se$ps2_bmi_se, ps3_bmi_se$ps3_bmi_se, ps4_bmi_se$ps4_bmi_se)

psxbeh <- data.frame(c(rep(1, 8), rep(2, 8), rep(3, 8), rep(4, 8)), rep(row.names(ps1_bmi_betas), 4))
names(psxbeh) <- c('Portion', 'Behavior')
psxbeh$Portion <- as.factor(psxbeh$Portion)
psxbeh$Behavior <- as.factor(psxbeh$Behavior)

ps_bmi_model_fig <- bar_graph.se(means = ps_bmi_betas, er = ps_bmi_se, xlab = 'Behavior', ylab = 'Standardized Coefficients for BMI', ymax = 0.75, ymin = -0.75, group = psxbeh$Portion)

## FMI across PS ####
ps_fmi_betas <- rbind(ps1_fmi_betas$ps1_fmi_betas, ps2_fmi_betas$ps2_fmi_betas, ps3_fmi_betas$ps3_fmi_betas, ps4_fmi_betas$ps4_fmi_betas)

ps_fmi_se <- rbind(ps1_fmi_se$ps1_fmi_se, ps2_fmi_se$ps2_fmi_se, ps3_fmi_se$ps3_fmi_se, ps4_fmi_se$ps4_fmi_se)

ps_fmi_model_fig <- bar_graph.se(ps_fmi_betas, er = ps_fmi_se, xlab = 'Behavior', ylab = 'Standardized Coefficients for FMI', ymax = 0.75, ymin = -0.75, group = psxbeh$Portion)
