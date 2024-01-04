# This script was written by Alaina Pearce in February 2023
# to set up tables for the a paper examining meal microstructure
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

# library(haven)
# library(feisr)
# library(lme4)
# library(lavaan)
# library(emmeans)
# library(gtsummary)
# theme_gtsummary_compact()
#
# source('functions.R')

# source('setup_data.R')

## demo table
micro_demo_data <- r01_micro_ps[c(5:6, 8, 14:15, 9:13)]
demo_tab <-
  tbl_summary(
    data = micro_demo_data,
    value = list(sex ~ "Sex", age_yr ~ "Age, yr",  bmi_percentile ~ "BMI %tile", dxa_est_vat_volume ~ "VAT, cm3", fmi ~ 'Fat Mass Index', ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income",  mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    label = list(sex ~ "Sex", age_yr ~ "Age, yr",  bmi_percentile ~ "BMI %tile", dxa_est_vat_volume ~ "VAT, cm3", fmi ~ 'Fat Mass Index', ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income",  mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    type = list(sex ~ "categorical", age_yr ~ "continuous", bmi_percentile ~ "continuous", dxa_est_vat_volume ~ "continuous", fmi ~ 'continuous', ethnicity ~ "categorical", race ~ "categorical", income ~ "categorical", mom_ed ~ "categorical", dad_ed ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)

demo_tab_sex <-
  tbl_summary(
    data = micro_demo_data,
    by = sex,
    value = list(age_yr ~ "Age, yr",  bmi_percentile ~ "BMI %tile", dxa_est_vat_volume ~ "VAT, cm3", fmi ~ 'Fat Mass Index', ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income",  mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    label = list(age_yr ~ "Age, yr",  bmi_percentile ~ "BMI %tile", dxa_est_vat_volume ~ "VAT, cm3", fmi ~ 'Fat Mass Index', ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income",  mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    type = list(age_yr ~ "continuous", bmi_percentile ~ "continuous", dxa_est_vat_volume ~ "continuous", fmi ~ 'continuous', ethnicity ~ "categorical", race ~ "categorical", income ~ "categorical", mom_ed ~ "categorical", dad_ed ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)%>%
  add_p()

demo_merge_tab <-
  tbl_merge(
    tbls = list(demo_tab, demo_tab_sex),
    tab_spanner = c("**Overall**", "**Sex**")
  )

## intake table
micro_intake_data <- r01_micro[c(2, 29, 31:32, 34:39)]
intake_tab <- tbl_summary(
    data = micro_intake_data,
    by = ps,
    value = list(freddy_pre_meal ~ "Pre-Meal Fullness", vas_mac_cheese ~ "Liking - Maccaroni", vas_chkn_nug ~ "Liking - Chicken", vas_grape ~ "Liking - Grapes", vas_broc ~ "Liking - Broccoli", avg_vas ~ "Avg. Liking", total_g ~ "Intake, g", total_kcal ~ "Intake, kcal", ps_plate_cleaner ~ "95% consumed"),
    label = list(freddy_pre_meal ~ "Pre-Meal Fullness", vas_mac_cheese ~ "Liking - Maccaroni", vas_chkn_nug ~ "Liking - Chicken", vas_grape ~ "Liking - Grapes", vas_broc ~ "Liking - Broccoli", avg_vas ~ "Avg. Liking",  total_g ~ "Intake, g", total_kcal ~ "Intake, kcal", ps_plate_cleaner ~ "95% consumed"),
    type = list(freddy_pre_meal ~ "continuous", vas_chkn_nug ~ "continuous", vas_grape ~ "continuous", vas_broc ~ "continuous", avg_vas ~ "continuous",  total_g ~ "continuous", total_kcal ~ "continuous", ps_plate_cleaner ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)

micro_intake_data_boy <- r01_micro[r01_micro[['sex']] == 'Male', c(2, 29, 31:32, 34:39)]
intake_tab_boy <-
  tbl_summary(
    data = micro_intake_data_boy,
    by = ps,
    value = list(freddy_pre_meal ~ "Pre-Meal Fullness", vas_mac_cheese ~ "Liking - Maccaroni", vas_chkn_nug ~ "Liking - Chicken", vas_grape ~ "Liking - Grapes", vas_broc ~ "Liking - Broccoli", avg_vas ~ "Avg. Liking",  total_g ~ "Intake, g", total_kcal ~ "Intake, kcal", ps_plate_cleaner ~ "95% consumed"),
    label = list(freddy_pre_meal ~ "Pre-Meal Fullness", vas_mac_cheese ~ "Liking - Maccaroni", vas_chkn_nug ~ "Liking - Chicken", vas_grape ~ "Liking - Grapes", vas_broc ~ "Liking - Broccoli", avg_vas ~ "Avg. Liking",  total_g ~ "Intake, g", total_kcal ~ "Intake, kcal", ps_plate_cleaner ~ "95% consumed"),
    type = list(freddy_pre_meal ~ "continuous", vas_chkn_nug ~ "continuous", vas_grape ~ "continuous", vas_broc ~ "continuous", avg_vas ~ "continuous",  total_g ~ "continuous", total_kcal ~ "continuous", ps_plate_cleaner ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)

micro_intake_data_girl <- r01_micro[r01_micro[['sex']] == 'Female', c(2, 29, 31:32, 34:39)]
intake_tab_girl <-
  tbl_summary(
    data = micro_intake_data_girl,
    by = ps,
    value = list(freddy_pre_meal ~ "Pre-Meal Fullness", vas_mac_cheese ~ "Liking - Maccaroni", vas_chkn_nug ~ "Liking - Chicken", vas_grape ~ "Liking - Grapes", vas_broc ~ "Liking - Broccoli", avg_vas ~ "Avg. Liking",  total_g ~ "Intake, g", total_kcal ~ "Intake, kcal", ps_plate_cleaner ~ "95% consumed"),
    label = list(freddy_pre_meal ~ "Pre-Meal Fullness", vas_mac_cheese ~ "Liking - Maccaroni", vas_chkn_nug ~ "Liking - Chicken", vas_grape ~ "Liking - Grapes", vas_broc ~ "Liking - Broccoli", avg_vas ~ "Avg. Liking",  total_g ~ "Intake, g", total_kcal ~ "Intake, kcal", ps_plate_cleaner ~ "95% consumed"),
    type = list(freddy_pre_meal ~ "continuous", vas_mac_cheese ~ "continuous", vas_chkn_nug ~ "continuous", vas_grape ~ "continuous", vas_broc ~ "continuous", avg_vas ~ "continuous",  total_g ~ "continuous", total_kcal ~ "continuous", ps_plate_cleaner ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)

## microstructure tables - summary
micro_beh_data <- r01_micro[c(2, 45:48, 50:54, 57:63)]

micro_beh_tab <-
  tbl_summary(
    data = micro_beh_data,
    by = ps,
    value = list(nbites_c1 ~ 'bites', nsips_c1 ~ 'sips', total_active_eating_c1 ~ 'active eating, min', bite_latency_c1 ~ '1st bite latency, min', meal_duration_c1 ~ 'meal duration, min' , bite_rate_c1 ~ 'bites/min', bite_rate_active_c1 ~ 'bite/min (active)', sip_rate_c1 ~ 'sips/min', sip_rate_active_c1 ~ 'sips/min (active)', bite_size_g_c1 ~ 'g/bite', bite_size_kcal_c1 ~ 'kcal/bite', eat_rate_g_c1 ~ 'g/min', eat_rate_kcal_c1 ~ 'kcal/min', eat_rate_active_g_c1 ~ 'g/min (active)', eat_rate_active_kcal_c1 ~ 'kcal/min (active)', prop_active_c1 ~ 'active eat/meal duration, min'),
    label = list(nbites_c1 ~ 'bites', nsips_c1 ~ 'sips', total_active_eating_c1 ~ 'active eating, min', bite_latency_c1 ~ '1st bite latency, min', meal_duration_c1 ~ 'meal duration, min' , bite_rate_c1 ~ 'bites/min', bite_rate_active_c1 ~ 'bite/min (active)', sip_rate_c1 ~ 'sips/min', sip_rate_active_c1 ~ 'sips/min (active)', bite_size_g_c1 ~ 'g/bite', bite_size_kcal_c1 ~ 'kcal/bite', eat_rate_g_c1 ~ 'g/min', eat_rate_kcal_c1 ~ 'kcal/min', eat_rate_active_g_c1 ~ 'g/min (active)', eat_rate_active_kcal_c1 ~ 'kcal/min (active)', prop_active_c1 ~ 'active eat/meal duration, min'),
    type = list(nbites_c1 ~ 'continuous', nsips_c1 ~ 'continuous', total_active_eating_c1 ~ 'continuous', bite_latency_c1 ~ 'continuous', meal_duration_c1 ~ 'continuous' , bite_rate_c1 ~ 'continuous', bite_rate_active_c1 ~ 'continuous', sip_rate_c1 ~ 'continuous', sip_rate_active_c1 ~ 'continuous', bite_size_g_c1 ~ 'continuous', bite_size_kcal_c1 ~ 'continuous', eat_rate_g_c1 ~ 'continuous', eat_rate_kcal_c1 ~ 'continuous', eat_rate_active_g_c1 ~ 'continuous', eat_rate_active_kcal_c1 ~ 'continuous', prop_active_c1 ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    #missing = "no",
    digits = all_continuous() ~ 3)

micro_beh_data_boy <- r01_micro[r01_micro[['sex']] == 'Male', c(2, 45:48, 50:54, 57:63)]
micro_beh_tab_boy <-
  tbl_summary(
    data = micro_beh_data_boy,
    by = ps,
    value = list(nbites_c1 ~ 'bites', nsips_c1 ~ 'sips', total_active_eating_c1 ~ 'active eating, min', bite_latency_c1 ~ '1st bite latency, min', meal_duration_c1 ~ 'meal duration, min' , bite_rate_c1 ~ 'bites/min', bite_rate_active_c1 ~ 'bite/min (active)', sip_rate_c1 ~ 'sips/min', sip_rate_active_c1 ~ 'sips/min (active)', bite_size_g_c1 ~ 'g/bite', bite_size_kcal_c1 ~ 'kcal/bite', eat_rate_g_c1 ~ 'g/min', eat_rate_kcal_c1 ~ 'kcal/min', eat_rate_active_g_c1 ~ 'g/min (active)', eat_rate_active_kcal_c1 ~ 'kcal/min (active)', prop_active_c1 ~ 'active eat/meal duration, min'),
    label = list(nbites_c1 ~ 'bites', nsips_c1 ~ 'sips', total_active_eating_c1 ~ 'active eating, min', bite_latency_c1 ~ '1st bite latency, min', meal_duration_c1 ~ 'meal duration, min' , bite_rate_c1 ~ 'bites/min', bite_rate_active_c1 ~ 'bite/min (active)', sip_rate_c1 ~ 'sips/min', sip_rate_active_c1 ~ 'sips/min (active)', bite_size_g_c1 ~ 'g/bite', bite_size_kcal_c1 ~ 'kcal/bite', eat_rate_g_c1 ~ 'g/min', eat_rate_kcal_c1 ~ 'kcal/min', eat_rate_active_g_c1 ~ 'g/min (active)', eat_rate_active_kcal_c1 ~ 'kcal/min (active)', prop_active_c1 ~ 'active eat/meal duration, min'),
    type = list(nbites_c1 ~ 'continuous', nsips_c1 ~ 'continuous', total_active_eating_c1 ~ 'continuous', bite_latency_c1 ~ 'continuous', meal_duration_c1 ~ 'continuous' , bite_rate_c1 ~ 'continuous', bite_rate_active_c1 ~ 'continuous', sip_rate_c1 ~ 'continuous', sip_rate_active_c1 ~ 'continuous', bite_size_g_c1 ~ 'continuous', bite_size_kcal_c1 ~ 'continuous', eat_rate_g_c1 ~ 'continuous', eat_rate_kcal_c1 ~ 'continuous', eat_rate_active_g_c1 ~ 'continuous', eat_rate_active_kcal_c1 ~ 'continuous', prop_active_c1 ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    #missing = "no",
    digits = all_continuous() ~ 3)

micro_beh_data_girl <- r01_micro[r01_micro[['sex']] == 'Female', c(2, 45:48, 50:54, 57:63)]
micro_beh_tab_girl <-
  tbl_summary(
    data = micro_beh_data_girl,
    by = ps,
    value = list(nbites_c1 ~ 'bites', nsips_c1 ~ 'sips', total_active_eating_c1 ~ 'active eating, min', bite_latency_c1 ~ '1st bite latency, min', meal_duration_c1 ~ 'meal duration, min' , bite_rate_c1 ~ 'bites/min', bite_rate_active_c1 ~ 'bite/min (active)', sip_rate_c1 ~ 'sips/min', sip_rate_active_c1 ~ 'sips/min (active)', bite_size_g_c1 ~ 'g/bite', bite_size_kcal_c1 ~ 'kcal/bite', eat_rate_g_c1 ~ 'g/min', eat_rate_kcal_c1 ~ 'kcal/min', eat_rate_active_g_c1 ~ 'g/min (active)', eat_rate_active_kcal_c1 ~ 'kcal/min (active)', prop_active_c1 ~ 'active eat/meal duration, min'),
    label = list(nbites_c1 ~ 'bites', nsips_c1 ~ 'sips', total_active_eating_c1 ~ 'active eating, min', bite_latency_c1 ~ '1st bite latency, min', meal_duration_c1 ~ 'meal duration, min' , bite_rate_c1 ~ 'bites/min', bite_rate_active_c1 ~ 'bite/min (active)', sip_rate_c1 ~ 'sips/min', sip_rate_active_c1 ~ 'sips/min (active)', bite_size_g_c1 ~ 'g/bite', bite_size_kcal_c1 ~ 'kcal/bite', eat_rate_g_c1 ~ 'g/min', eat_rate_kcal_c1 ~ 'kcal/min', eat_rate_active_g_c1 ~ 'g/min (active)', eat_rate_active_kcal_c1 ~ 'kcal/min (active)', prop_active_c1 ~ 'active eat/meal duration, min'),
    type = list(nbites_c1 ~ 'continuous', nsips_c1 ~ 'continuous', total_active_eating_c1 ~ 'continuous', bite_latency_c1 ~ 'continuous', meal_duration_c1 ~ 'continuous' , bite_rate_c1 ~ 'continuous', bite_rate_active_c1 ~ 'continuous', sip_rate_c1 ~ 'continuous', sip_rate_active_c1 ~ 'continuous', bite_size_g_c1 ~ 'continuous', bite_size_kcal_c1 ~ 'continuous', eat_rate_g_c1 ~ 'continuous', eat_rate_kcal_c1 ~ 'continuous', eat_rate_active_g_c1 ~ 'continuous', eat_rate_active_kcal_c1 ~ 'continuous', prop_active_c1 ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    #missing = "no",
    digits = all_continuous() ~ 3)

## ICC - reliability ####
icc_table <- as.data.frame(matrix(c('bites', 'sips', 'active eating', 'bite latency', 'meal duration', round(nbites_icc$value, 3), round(nsips_icc$value, 3), round(active_eat_icc$value, 3), round(bite_latency_icc$value, 3), round(meal_dur_icc$value, 3), round(nbites_icc_ps1$value, 3), round(nsips_icc_ps1$value, 3), round(active_eat_icc_ps1$value, 3), round(bite_latency_icc_ps1$value, 3), round(meal_dur_icc_ps1$value, 3), round(nbites_icc_ps2$value, 3), round(nsips_icc_ps2$value, 3), round(active_eat_icc_ps2$value, 3), round(bite_latency_icc_ps2$value, 3), round(meal_dur_icc_ps2$value, 3), round(nbites_icc_ps3$value, 3), round(nsips_icc_ps3$value, 3), round(active_eat_icc_ps3$value, 3), round(bite_latency_icc_ps3$value, 3), round(meal_dur_icc_ps3$value, 3), round(nbites_icc_ps4$value, 3), round(nsips_icc_ps4$value, 3), round(active_eat_icc_ps4$value, 3), round(bite_latency_icc_ps4$value, 3), round(meal_dur_icc_ps4$value, 3)), nrow = 5, byrow = FALSE))
names(icc_table) <- c('Behavior', 'Overall', 'Portion 1', 'Portion 2', 'Portion 3', 'Portion 4')
