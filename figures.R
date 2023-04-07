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

## Behavior across PS models ####
# ps_beh_models_fig <- bar_graph.se(ps_beh_betas$ps_beh_betas, er = ps_beh_se$ps_beh_se, xlab = 'Behavior', ylab = 'Standardized Coefficients for Portion Size', ymax = 0.6, ymin = -0.6, group = 0)

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
