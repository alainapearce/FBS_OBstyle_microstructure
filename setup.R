# This script was written by Alaina Pearce in February 2023
# to set up data for the a paper examining meal microstructure
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
# need to uncomment if running indipendently - not needed if compiling with 2023_micro_summary.Rmd

# library(haven)
# library(lme4)
# library(lmerTest)
#
# source('functions.R')

#### set up ####

## 1- Microstructure Data ####
# a) Load Data ####
r01_micro_beh <- as.data.frame(read_spss(("data/micro_beh_summary.sav")))
names(r01_micro_beh)[1] <- 'sub'

r01_micro_beh_labels <- lapply(r01_micro_beh, function(x) attributes(x)$label)

# b) Get Variable Labels and Re-Level ####

# risk status
r01_micro_beh$risk_status_mom <- droplevels(as_factor(r01_micro_beh$risk_status_mom))
r01_micro_beh$risk_status_both <- droplevels(as_factor(r01_micro_beh$risk_status_both))
r01_micro_beh$sex <- as_factor(r01_micro_beh$sex)

# race
r01_micro_beh$race <- factor(r01_micro_beh$race)

# ethnicity
r01_micro_beh$ethnicity <- ifelse(r01_micro_beh$ethnicity == 0, 'Not Hispanic/Lantinx', 'Hispanic/Lantinx')
r01_micro_beh$ethnicity <- factor(r01_micro_beh$ethnicity)

# income
r01_micro_beh$income <- ifelse(is.na(r01_micro_beh$income), NA, ifelse(r01_micro_beh$income < 3, '< $51,000', ifelse(r01_micro_beh$income < 5, "$51,000 - $100,000", '>$100,000')))
r01_micro_beh$income <- factor(r01_micro_beh$income)

# parental ed
r01_micro_beh$mom_ed <- ifelse(r01_micro_beh$measured_parent == 0, ifelse(r01_micro_beh$parent_ed == 0, 'High School/GED', ifelse(r01_micro_beh$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_micro_beh$parent_ed == 3, 'Bachelor Degree', ifelse(r01_micro_beh$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_micro_beh$partner_ed == 0, 'High School/GED', ifelse(r01_micro_beh$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_micro_beh$partner_ed == 3, 'Bachelor Degree', ifelse(r01_micro_beh$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))
r01_micro_beh$mom_ed <- factor(r01_micro_beh$mom_ed)

r01_micro_beh$dad_ed <- ifelse(r01_micro_beh$measured_parent == 1, ifelse(r01_micro_beh$parent_ed == 0, 'High School/GED', ifelse(r01_micro_beh$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_micro_beh$parent_ed == 3, 'Bachelor Degree', ifelse(r01_micro_beh$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_micro_beh$partner_ed == 0, 'High School/GED', ifelse(r01_micro_beh$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_micro_beh$partner_ed == 3, 'Bachelor Degree', ifelse(r01_micro_beh$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))
r01_micro_beh$dad_ed <- factor(r01_micro_beh$dad_ed)



## 2- Intake Data ####
# a) Load Data ####
r01_intake <- as.data.frame(read_spss(("data/intake_data.sav")))
names(r01_intake)[1] <- 'sub'

r01_intake_labels <- lapply(r01_intake, function(x) attributes(x)$label)

# get portion size order
#get portion order
r01_intake[['ps1_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 0, 1, ifelse(r01_intake[['v3_meal_ps']] == 0, 2, ifelse(r01_intake[['v4_meal_ps']] == 0, 3, 4))))

r01_intake[['ps2_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 1, 1, ifelse(r01_intake[['v3_meal_ps']] == 1, 2, ifelse(r01_intake[['v4_meal_ps']] == 1, 3, 4))))

r01_intake[['ps3_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 2, 1, ifelse(r01_intake[['v3_meal_ps']] == 2, 2, ifelse(r01_intake[['v4_meal_ps']] == 2, 3, 4))))

r01_intake[['ps4_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 3, 1, ifelse(r01_intake[['v3_meal_ps']] == 3, 2, ifelse(r01_intake[['v4_meal_ps']] == 3, 3, 4))))

#portion order
r01_intake[['ps_order']] <- ifelse(is.na(r01_intake[['v3_meal_ps']]), paste0(r01_intake[['v2_meal_ps']] + 1), ifelse(is.na(r01_intake[['v4_meal_ps']]), paste0(r01_intake[['v2_meal_ps']] + 1, r01_intake[['v3_meal_ps']] + 1), ifelse(is.na(r01_intake[['v5_meal_ps']]), paste0(r01_intake[['v2_meal_ps']] + 1, r01_intake[['v3_meal_ps']] + 1, r01_intake[['v4_meal_ps']] + 1), paste0(r01_intake[['v2_meal_ps']] + 1, r01_intake[['v3_meal_ps']] + 1, r01_intake[['v4_meal_ps']] + 1, r01_intake[['v5_meal_ps']] + 1))))

# reduce and make numeric
r01_intake <- r01_intake[c(1, 563:568, 606:607, 609:614, 652:653, 655:660, 698:699, 701:706, 744:750)]

r01_intake[c(8:9, 16:17, 24:25, 32:33)] <- sapply(r01_intake[c(8:9, 16:17, 24:25, 32:33)], FUN = as.numeric)

## average VAS
r01_intake[c("ps1_vas_mac_cheese","ps1_vas_chkn_nug", "ps1_vas_broccoli","ps1_vas_grape", "ps2_vas_mac_cheese","ps2_vas_chkn_nug", "ps2_vas_broccoli","ps2_vas_grape", "ps3_vas_mac_cheese","ps3_vas_chkn_nug", "ps3_vas_broccoli","ps3_vas_grape", "ps4_vas_mac_cheese","ps4_vas_chkn_nug", "ps4_vas_broccoli","ps4_vas_grape")] <- sapply(r01_intake[c("ps1_vas_mac_cheese","ps1_vas_chkn_nug", "ps1_vas_broccoli","ps1_vas_grape", "ps2_vas_mac_cheese","ps2_vas_chkn_nug", "ps2_vas_broccoli","ps2_vas_grape", "ps3_vas_mac_cheese","ps3_vas_chkn_nug", "ps3_vas_broccoli","ps3_vas_grape", "ps4_vas_mac_cheese","ps4_vas_chkn_nug", "ps4_vas_broccoli","ps4_vas_grape")], function(x) as.numeric(x, na.rm = TRUE))

r01_intake[['ps1_avg_vas']] <- rowMeans(r01_intake[c("ps1_vas_mac_cheese","ps1_vas_chkn_nug", "ps1_vas_broccoli","ps1_vas_grape")])
r01_intake[['ps2_avg_vas']] <- rowMeans(r01_intake[c("ps2_vas_mac_cheese","ps2_vas_chkn_nug", "ps2_vas_broccoli","ps2_vas_grape")])
r01_intake[['ps3_avg_vas']] <- rowMeans(r01_intake[c("ps3_vas_mac_cheese","ps3_vas_chkn_nug", "ps3_vas_broccoli","ps3_vas_grape")])
r01_intake[['ps4_avg_vas']] <- rowMeans(r01_intake[c("ps4_vas_mac_cheese","ps4_vas_chkn_nug", "ps4_vas_broccoli","ps4_vas_grape")])

## plate cleaners
r01_intake[['ps1_plate_cleaner']] <- ifelse(r01_intake[['ps1_total_g']] >= 769*.95, 1, 0)

#number 17 and 110
r01_intake[['ps2_plate_cleaner']] <- ifelse(r01_intake[['ps2_total_g']] >= 1011*.95, 1, 0)
r01_intake[['ps3_plate_cleaner']] <- ifelse(r01_intake[['ps3_total_g']] >= 1255*.95, 1, 0)
r01_intake[['ps4_plate_cleaner']] <- ifelse(r01_intake[['ps4_total_g']] >= 1499*.95, 1, 0)

r01_intake$plate_cleaner <- rowSums(r01_intake[42:45])

## make long by portion size
r01_intake_long <- melt(r01_intake[c(1:2, 10, 18, 26)], id.vars = 'sub')
names(r01_intake_long)[2] <- 'ps'

r01_intake_long[['ps']] <-ifelse(r01_intake_long[['ps']] == 'ps1_freddy_pre_meal', 1, ifelse(r01_intake_long[['ps']] == 'ps2_freddy_pre_meal', 2, ifelse(r01_intake_long$ps == 'ps3_freddy_pre_meal', 3, ifelse(r01_intake_long[['ps']] == 'ps4_freddy_pre_meal', 4, NA))))
names(r01_intake_long)[3] <- 'freddy_pre_meal'

r01_post_freddy <- melt(r01_intake[c(1, 3, 11, 19, 27)], id.vars = 'sub')
names(r01_post_freddy)[3] <- 'freddy_post_meal'

r01_total_g <- melt(r01_intake[c(1, 8, 16, 24, 32)], id.vars = 'sub')
names(r01_total_g)[3] <- 'total_g'

r01_total_kcal <- melt(r01_intake[c(1, 9, 17, 25, 33)], id.vars = 'sub')
names(r01_total_kcal)[3] <- 'total_kcal'

r01_ps_order <- melt(r01_intake[c(1, 34:37)], id.vars = 'sub')
names(r01_ps_order)[3] <- 'ps_order'

r01_avg_vas <- melt(r01_intake[c(1, 39:42)], id.vars = 'sub')
names(r01_avg_vas)[3] <- 'avg_vas'

r01_plate_cleaner <- melt(r01_intake[c(1, 43:46)], id.vars = 'sub')
names(r01_plate_cleaner)[3] <- 'ps_plate_cleaner'

r01_intake_long <- cbind.data.frame(r01_intake_long, r01_post_freddy[3], r01_total_g[3], r01_total_kcal[3], r01_ps_order[3], r01_avg_vas[3], r01_plate_cleaner[3])

r01_intake_long <- merge(r01_intake_long, r01_intake[c(1, 47)], by = 'sub')

# b) merge with micro data ####
r01_micro <- merge(r01_micro_beh, r01_intake_long, by = c('sub', 'ps'), all.x = TRUE, all.y = FALSE)
r01_micro <- r01_micro[!is.na(r01_micro[['ps']]), ]

# c) Calc new microstructure ####
r01_micro[['bite_size_g_c1']] <- r01_micro[['total_g']]/r01_micro[['nbites_c1']]
r01_micro[['bite_size_kcal_c1']] <- r01_micro[['total_kcal']]/r01_micro[['nbites_c1']]

r01_micro[['bite_size_g_c2']] <- r01_micro[['total_g']]/r01_micro[['nbites_c2']]
r01_micro[['bite_size_kcal_c2']]<- r01_micro[['total_kcal']]/r01_micro[['nbites_c2']]

r01_micro[['eat_rate_g_c1']] <- r01_micro[['total_g']]/r01_micro[['meal_duration_c1']]
r01_micro[['eat_rate_kcal_c1']] <- r01_micro[['total_kcal']]/r01_micro[['meal_duration_c1']]

r01_micro[['eat_rate_g_c2']] <- r01_micro[['total_g']]/r01_micro[['meal_duration_c2']]
r01_micro[['eat_rate_kcal_c2']] <- r01_micro[['total_kcal']]/r01_micro[['meal_duration_c2']]

r01_micro[['eat_rate_active_g_c1']] <- r01_micro[['total_g']]/r01_micro[['total_active_eating_c1']]
r01_micro[['eat_rate_active_kcal_c1']] <- r01_micro[['total_kcal']]/r01_micro[['total_active_eating_c1']]

r01_micro[['eat_rate_active_g_c2']] <- r01_micro[['total_g']]/r01_micro[['total_active_eating_c2']]
r01_micro[['eat_rate_active_kcal_c2']] <- r01_micro[['total_kcal']]/r01_micro[['total_active_eating_c2']]

r01_micro[['prop_active_c1']] <- r01_micro[['total_active_eating_c1']]/r01_micro[['meal_duration_c1']]
r01_micro[['prop_active_c2']] <- r01_micro[['total_active_eating_c2']]/r01_micro[['meal_duration_c2']]

## 3- CEBQ Data ####
# a) Load Data ####
r01_eatbeh_qs <- as.data.frame(read_spss(("data/qs_eatbeh_bodyimage.sav")))
names(r01_eatbeh_qs)[1] <- 'sub'

r01_eatbeh_qs_labels <- lapply(r01_eatbeh_qs, function(x) attributes(x)$label)

# b) Caluculate 3 factor scoring ####
r01_eatbeh_qs[['cebq3_rev']] <- ifelse(r01_eatbeh_qs[['cebq3']] == 5, 1, ifelse(r01_eatbeh_qs[['cebq3']] == 4, 2, ifelse(r01_eatbeh_qs[['cebq3']] == 2, 4, ifelse(r01_eatbeh_qs[['cebq3']] == 1, 5, 3))))

r01_eatbeh_qs[['cebq4_rev']] <- ifelse(r01_eatbeh_qs[['cebq4']] == 5, 1, ifelse(r01_eatbeh_qs[['cebq4']] == 4, 2, ifelse(r01_eatbeh_qs[['cebq4']] == 2, 4, ifelse(r01_eatbeh_qs[['cebq4']] == 1, 5, 3))))

r01_eatbeh_qs[['cebq8_rev']] <- ifelse(r01_eatbeh_qs[['cebq8']] == 5, 1, ifelse(r01_eatbeh_qs[['cebq8']] == 4, 2, ifelse(r01_eatbeh_qs[['cebq8']] == 2, 4, ifelse(r01_eatbeh_qs[['cebq8']] == 1, 5, 3))))


r01_eatbeh_qs[['cebq_re']] <- rowMeans(r01_eatbeh_qs[c('cebq1', 'cebq3_rev', 'cebq4_rev', 'cebq5', 'cebq8_rev', 'cebq12', 'cebq14', 'cebq19', 'cebq20', 'cebq28', 'cebq34')])
r01_eatbeh_qs[['cebq_pe']] <- rowMeans(r01_eatbeh_qs[c('cebq7', 'cebq10', 'cebq16', 'cebq24', 'cebq32', 'cebq33')])
r01_eatbeh_qs[['cebq_ee']] <- rowMeans(r01_eatbeh_qs[c('cebq2', 'cebq9', 'cebq13', 'cebq15', 'cebq23', 'cebq25')])

r01_cebq <- r01_eatbeh_qs[c(1, 479:486, 1050:1052)]

## b) merge with micro data ####
r01_micro <- merge(r01_micro, r01_cebq, by = c('sub'))


## 4- Demo Data (esier that casting wide for participant characteristics) ####
# a) Load Data ####
r01_demo <- as.data.frame(read_spss(("data/demographics_data.sav")))
names(r01_demo)[1] <- 'sub'

r01_demo_labels <- lapply(r01_demo, function(x) attributes(x)$label)

# b) Get Variable Labels and Re-Level ####

# risk status
r01_demo$risk_status_mom <- droplevels(as_factor(r01_demo$risk_status_mom))
r01_demo$risk_status_both <- droplevels(as_factor(r01_demo$risk_status_both))
r01_demo$sex <- as_factor(r01_demo$sex)

# race
r01_demo[['race']] <- factor(r01_demo[['race']])

# ethnicity
r01_demo[['ethnicity']] <- ifelse(r01_demo[['ethnicity']] == 0, 'Not Hispanic/Lantinx', 'Hispanic/Lantinx')
r01_demo[['ethnicity']] <- factor(r01_demo[['ethnicity']])

# income
r01_demo[['income']] <- ifelse(is.na(r01_demo[['income']]), NA, ifelse(r01_demo[['income']] < 3, '< $51,000', ifelse(r01_demo[['income']] < 5, "$51,000 - $100,000", '>$100,000')))
r01_demo[['income']] <- factor(r01_demo[['income']])

# parental ed
r01_demo[['mom_ed']] <- ifelse(r01_demo[['measured_parent']] == 0, ifelse(r01_demo[['parent_ed']] == 0, 'High School/GED', ifelse(r01_demo[['parent_ed']] < 3, 'AA/Technical Degree', ifelse(r01_demo[['parent_ed']] == 3, 'Bachelor Degree', ifelse(r01_demo[['parent_ed']] < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_demo[['partner_ed']] == 0, 'High School/GED', ifelse(r01_demo[['partner_ed']] < 3, 'AA/Technical Degree', ifelse(r01_demo[['partner_ed']] == 3, 'Bachelor Degree', ifelse(r01_demo[['partner_ed']] < 8, '> Bachelor Degree', 'Other/NA')))))
r01_demo[['mom_ed']] <- factor(r01_demo[['mom_ed']])

r01_demo[['dad_ed']] <- ifelse(r01_demo[['measured_parent']] == 1, ifelse(r01_demo[['parent_ed']] == 0, 'High School/GED', ifelse(r01_demo[['parent_ed']] < 3, 'AA/Technical Degree', ifelse(r01_demo[['parent_ed']] == 3, 'Bachelor Degree', ifelse(r01_demo[['parent_ed']] < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_demo[['partner_ed']] == 0, 'High School/GED', ifelse(r01_demo[['partner_ed']] < 3, 'AA/Technical Degree', ifelse(r01_demo[['partner_ed']] == 3, 'Bachelor Degree', ifelse(r01_demo[['partner_ed']] < 8, '> Bachelor Degree', 'Other/NA')))))
r01_demo[['dad_ed']] <- factor(r01_demo[['dad_ed']])



#### Single Coder/wide dataset ####

## 1 - Generate Dataset ####
# a) re-name and merge ####
ps_names <- c('visit', 'nbites', 'nsips', 'active_eating', 'bite_latency', 'meal_duration', 'bite_rate' ,'bite_rate_active', 'sip_rate', 'sip_rate_active', 'bite_size_g', 'bite_size_kcal', 'eat_rate_g', 'eat_rate_kcal', 'eat_rate_active_g', 'eat_rate_active_kcal', 'prop_active', 'freddy_pre_meal', 'freddy_post_meal', 'total_g', 'total_kcal', 'avg_vas', 'plate_cleaner')

r01_micro_1coder <- r01_micro[c(1:2, 28, 31:34, 36:40, 67:68, 71:72, 75:76, 79, 59:62, 64:65)]
r01_micro_ps <- r01_micro_1coder[r01_micro_1coder$ps == 1, ]
names(r01_micro_ps)[c(3:25)] <- sapply(ps_names, function(x) paste0('ps1_', x), USE.NAMES = FALSE)

r01_micro_ps <- merge(r01_micro_ps, r01_micro_1coder[r01_micro_1coder$ps == 2, c(1, 3:25)], by = 'sub', all = TRUE)
names(r01_micro_ps)[c(26:48)] <- sapply(ps_names, function(x) paste0('ps2_', x), USE.NAMES = FALSE)

r01_micro_ps <- merge(r01_micro_ps, r01_micro_1coder[r01_micro_1coder$ps == 3, c(1, 3:25)], by = 'sub', all = TRUE)
names(r01_micro_ps)[c(49:71)] <- sapply(ps_names, function(x) paste0('ps3_', x), USE.NAMES = FALSE)

r01_micro_ps <- merge(r01_micro_ps, r01_micro_1coder[r01_micro_1coder$ps == 4, c(1, 3:25)], by = 'sub', all = TRUE)
names(r01_micro_ps)[c(72:94)] <- sapply(ps_names, function(x) paste0('ps4_', x), USE.NAMES = FALSE)

r01_micro_ps <- merge(r01_demo[c(1, 4, 8:10, 12, 19:20, 14:16, 337:338)], r01_micro_ps[c(1, 3:94)], by = 'sub', all.x = FALSE, all.y = TRUE)

# b) add microstructure video notes ####
# ad micro ps number
r01_micro_ps[['n_micro_ps']] <- ifelse(!is.na(r01_micro_ps[['ps1_visit']]), ifelse(!is.na(r01_micro_ps[['ps2_visit']]), ifelse(!is.na(r01_micro_ps[['ps3_visit']]), ifelse(!is.na(r01_micro_ps[['ps4_visit']]), 4, 3), ifelse(!is.na(r01_micro_ps[['ps4_visit']]), 3, 2)), ifelse(!is.na(r01_micro_ps[['ps3_visit']]), ifelse(!is.na(r01_micro_ps[['ps4_visit']]), 3, 2), ifelse(!is.na(r01_micro_ps[['ps4_visit']]), 2, 1))), ifelse(!is.na(r01_micro_ps[['ps2_visit']]), ifelse(!is.na(r01_micro_ps[['ps3_visit']]), ifelse(!is.na(r01_micro_ps[['ps4_visit']]), 3, 2), ifelse(!is.na(r01_micro_ps[['ps4_visit']]), 2, 1)), ifelse(!is.na(r01_micro_ps[['ps3_visit']]), ifelse(!is.na(r01_micro_ps[['ps4_visit']]), 2, 1), ifelse(!is.na(r01_micro_ps[['ps4_visit']]), 1, NA))))

#add video issues notes
r01_micro_ps[['video_issues']] <- 0
r01_micro_ps[r01_micro_ps[['sub']] == 47 | r01_micro_ps[['sub']] == 56 | r01_micro_ps[['sub']] == 70 | r01_micro_ps[['sub']] == 72 | r01_micro_ps[['sub']] == 77 | r01_micro_ps[['sub']] == 78 | r01_micro_ps[['sub']] == 95 | r01_micro_ps[['sub']] == 98 | r01_micro_ps[['sub']] == 112,  'video_issues'] <- 1

r01_micro_ps[r01_micro_ps[['sub']] == 1,  'video_issues'] <- 2

r01_micro_ps <- merge(r01_micro_ps, r01_cebq, by = 'sub', all.x = TRUE, all.y = FALSE)
r01_micro_ps <- merge(r01_micro_ps, r01_intake[c(1, 38, 47)], by = 'sub', all.x = TRUE, all.y = FALSE)

r01_micro_ps <- r01_micro_ps[c(1:13, 119:120, 106:118, 14:105)]

# c) merge notes with long ps data ####
r01_micro <- merge(r01_micro, r01_micro_ps[c(1,16:17)], by = 'sub', all = TRUE)
r01_micro <- r01_micro[c(1:2, 28, 92:93, 9:11, 13, 15:17, 57:58, 22:23, 81:91, 59:66, 29:42, 67:68, 71:72, 75:76, 79, 43:56, 69:70, 73:74, 77:78, 80)]

## 3- remove participants ####

# a) remove 2 that were removed for ADHD ####
r01_micro_ps <- r01_micro_ps[r01_micro_ps[['sub']] != 31 & r01_micro_ps[['sub']] != 34, ]
r01_micro <- r01_micro[r01_micro[['sub']] != 31 & r01_micro[['sub']] != 34, ]

# b) remove for not eating ####
r01_micro_ps <- r01_micro_ps[r01_micro_ps[['sub']] != 113, ]
r01_micro <- r01_micro[r01_micro[['sub']] != 113, ]

# c) only 1 microstructure coded meal ####
r01_micro_ps <- r01_micro_ps[r01_micro_ps[['sub']] != 82, ]
r01_micro <- r01_micro[r01_micro[['sub']] != 82, ]



