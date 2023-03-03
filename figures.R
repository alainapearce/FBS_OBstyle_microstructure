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

## ps correlation heatmaps ####
x <- seq(1, 16, 1)
y <- sapply(varnames_rmcorr, function(x) gsub('_c1', '', x), USE.NAMES = FALSE)
y <- sapply(seq(1, 16, 1), function(x) paste0(x, '_', y[[x]]), USE.NAMES = FALSE)

ps_plot_dat <- expand.grid(y, x)
ps1_mat_vect <- as.numeric(c(ps1_cormat))
ps1_mat_vect <- ifelse(is.na(ps1_mat_vect), 0, ps1_mat_vect)
ps_plot_dat$ps1 <- ps1_mat_vect

ps2_mat_vect <- as.numeric(c(ps2_cormat))
ps2_mat_vect <- ifelse(is.na(ps2_mat_vect), 0, ps2_mat_vect)
ps_plot_dat$ps2 <- ps2_mat_vect

ps3_mat_vect <- as.numeric(c(ps3_cormat))
ps3_mat_vect <- ifelse(is.na(ps3_mat_vect), 0, ps3_mat_vect)
ps_plot_dat$ps3 <- ps3_mat_vect

ps4_mat_vect <- as.numeric(c(ps4_cormat))
ps4_mat_vect <- ifelse(is.na(ps4_mat_vect), 0, ps4_mat_vect)
ps_plot_dat$ps4 <- ps4_mat_vect

ps_plot_dat$Var1 <- ordered(ps_plot_dat$Var1, levels = rev(y))

ps1_mat_plot <- ggplot(data = ps_plot_dat, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill = cut(ps1, breaks = c(-1, -0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5, 1)))) +
  scale_fill_manual(values = c("darkblue", "blue", "lightblue", "white", "white",
                               "rosybrown1", "red", "red3")) +
  scale_x_continuous(breaks=seq(1, 15, 1)) +
  labs(title="Portion Size 1 Correlations",
       x="",
       y = "") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

ps2_mat_plot <- ggplot(data = ps_plot_dat, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill = cut(ps2, breaks = c(-1, -0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5, 1)))) +
  scale_fill_manual(values = c("darkblue", "blue", "lightblue", "white", "white",
                               "rosybrown1", "red", "red3")) +
  scale_x_continuous(breaks=seq(1, 15, 1)) +
  labs(title="Portion Size 2 Correlations",
       x="",
       y = "") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

ps3_mat_plot <- ggplot(data = ps_plot_dat, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill = cut(ps3, breaks = c(-1, -0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5, 1)))) +
  scale_fill_manual(values = c("darkblue", "blue", "lightblue", "white", "white",
                               "rosybrown1", "red", "red3")) +
  scale_x_continuous(breaks=seq(1, 15, 1)) +
  labs(title="Portion Size 3 Correlations",
       x="",
       y = "") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

ps4_mat_plot <- ggplot(data = ps_plot_dat, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill = cut(ps4, breaks = c(-1, -0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5, 1)))) +
  scale_fill_manual(values = c("darkblue", "blue", "lightblue", "white", "white",
                               "rosybrown1", "red", "red3")) +
  scale_x_continuous(breaks=seq(1, 15, 1)) +
  labs(title="Portion Size 4 Correlations",
       x="",
       y = "") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

## rmcorr heatmap ####
x <- seq(1, 16, 1)
y <- sapply(varnames_rmcorr, function(x) gsub('_c1', '', x), USE.NAMES = FALSE)
y <- sapply(seq(1, 16, 1), function(x) paste0(x, '_', y[[x]]), USE.NAMES = FALSE)

rmcorr_plot_dat <- expand.grid(y, x)
boot_mat_vect <- as.numeric(c(boot_mat))
boot_mat_vect <- ifelse(is.na(boot_mat_vect), 0, boot_mat_vect)
rmcorr_plot_dat$rmcorr_boot <- boot_mat_vect
rmcorr_plot_dat$Var1 <- ordered(rmcorr_plot_dat$Var1, levels = rev(y))

rmcorr_mat_plot <- ggplot(data = rmcorr_plot_dat, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill = cut(rmcorr_boot, breaks = c(-1, -0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5, 1)))) +
  scale_fill_manual(values = c("darkblue", "blue", "lightblue", "white", "white",
                               "rosybrown1", "red", "red3")) +
  scale_x_continuous(breaks=seq(1, 15, 1)) +
  labs(title="Repeated Measures Correlations",
       x="",
       y = "") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())
