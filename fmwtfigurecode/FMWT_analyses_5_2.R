# American Shad, Delta Smelt, Longfin Smelt, Striped Bass (Age0), Threadfin Shad
# FMWT indices + year type (Critical, Below Normal, etc) + year (ie change over time)
# note "regimes": year < 1988 "pre-clam", 1988-2001 "pre-pod", 2002+ "pod")
# started 31 October 2022
# last modified 14 November 2022

# set-up -----

# clear R environment except fmwt & color pallettes
rm(list = ls()[!ls() %in% c("fmwt", "fmwt_trs_long","pal_drought", "pal_yrtype")])

# packages to load -----

library(tidyverse)
library(ggsignif)
library(readxl)
library(DescTools)
library(emmeans)
library(jtools)

# data ----

load("data/fmwt.RData") # includes year type designations
load("data/fmwt_trs_long.RData") # data are ln(x), ln(x+1) for Delta Smelt

# colors
pal_drought <- c("D" = "#FDE333", "N" = "#53CC67","W" = "#00588B")
pal_yrtype <- c("Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#4B0055")

# print results ----
# NOTE! won't work until you run the code below line 38...
all_spp # boxplot for all 5 spp by year type
box_ts_ams # boxplot w pairwise and ts insert, American Shad
box_ts_ds # boxplot w pairwise and ts insert, Delta Smelt
box_ts_lfs # boxplot w pairwise and ts insert, Longfin Smelt
box_ts_sb0 # boxplot w pairwise and ts insert, Striped Bass, Age-0
box_ts_tfs # boxplot w pairwise and ts insert, Threadfin Shad

# plots w all spp -----

p <- 
  ggplot(fmwt_trs_long, aes(x = species, y = ln_index, fill = yr_type)) +
  geom_boxplot() + 
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Fall Midwater Trawl",
       x = NULL,
       y = "Index, ln(x) or ln(x+1)",
       fill = "year type") +
  theme_bw()

all_spp <- p + theme(plot.title = element_text(size = 28),
                axis.title = element_text(size = 18),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12))

all_spp
# Notes on 'all_spp' figure 
# 1) For every species (TFS?), AN & Wet are higher (if  not "significantly" so) than other year types.
# 2) BN years are consistently lower for all spp; seems counter to a "drought effect", but note that these years are found almost exclusively post-POD when most of these populations have already collapsed. See for example 'ts_ams' though any of the time series figures will do.

# analyses by species ----
## American Shad ----

### summary stats & figures ----
fmwt %>% 
  group_by(yr_type) %>% 
  summarise(mean = mean(american_shad, na.rm = T), years = n())

### time series ----
p <- 
  ggplot(fmwt) +
  aes(x = year, y = american_shad) +
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 14) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "American Shad", x = NULL, y = "FMWT Index",
       col = "year type", fill = "year type") +
  theme_bw()

ts_ams <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12))

ts_ams

### serial randomness ----
# H0: sequence produced in random manner--no pattern to any runs
(runs_ams <- RunsTest(fmwt$american_shad, exact = T, na.rm = T))
# Random, no pattern, no runs; what happens if we restrict to post-POD (2002)?
test <- subset(fmwt, year >= 2002, select = american_shad)
(RunsTest(test$american_shad, na.rm = T)) 
# post-POD sequence is non-random

### normality ----
layout(matrix(c(1,2), 1, 2, byrow = T))
hist(fmwt$american_shad,
     main = "American Shad",
     xlab = "American Shad Index")
hist(log(fmwt$american_shad),
     main = NULL,
     xlab = "American Shad Index, ln(x)")
# log transformation improves normality

### boxplot ----
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(american_shad), fill = yr_type)) +
  geom_boxplot() +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "American Shad",
       x = "Year Type",
       y = "Index, ln(x)",
       fill = "year type") +
  theme_bw()

amsbox <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 12), # retained for other uses
            legend.position = "none")

print(amsbox) # looks promising!

### boxplot+ ----
# boxplot w pairwise & ts insert

p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(american_shad), fill = yr_type)) +
  ylim(0, 11) + # was (0, 10.5)
  geom_boxplot() +
  geom_signif(
    comparisons = list(c("Critical", "Wet"), c("Below Normal", "Wet")),
    y_position = c(10,9), # added to test...
    annotation = c("*", "*"), # test = "t.test",
    map_signif_level = TRUE,
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "American Shad",
       x = "Year Type",
       y = "Index, ln(x)",
       fill = "year type") +
  theme_bw()

p0 <- p + theme(plot.title = element_text(size = 18),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12),
                legend.position = c(0.1, 0.2))

p1 <-
  ggplot(fmwt) +
  aes(x = year, y = american_shad) + 
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 6) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(x = NULL, y = "Index",
       col = "year type", fill = "year type") +
  theme_bw()

p2 <- 
  p1 + theme(axis.text = element_text(size = 8),
             legend.text = element_text(size = 8),
             legend.position = "none") # c(0.9, 0.7) or left, top, right, bottom

(box_ts_ams <- p0 + annotation_custom(ggplotGrob(p2), xmin = 2, xmax = 5.5,
                       ymin = -0.5, ymax = 4.5))

### linear models, log-transformed -----
# super basic drought effect
# ams1 <- lm(log(american_shad) ~ drought, data = fmwt)

# add year as a main effect
# ams2 <- lm(log(american_shad) ~ drought + year, data = fmwt)

ams3 <- lm(log(american_shad) ~ yr_type + year, data = fmwt) # year type instead of drought
layout(matrix(c(1:4), 2, 2, byrow = T))
plot(ams3)
summ(ams3, model.info = T, digits = 3) # strong linear effect of year type

# pairwise comparison for estimated marginal means in the lm
# emmeans(ams1, pairwise ~ drought) 
# emmeans(ams2, pairwise ~ drought)
(pw_ams <- emmeans(ams3, pairwise ~ yr_type)) # about what you'd hope for...
layout(matrix(1, 1, 1))

### notes -----
# AMS: random over time (ie no year effect); convincing drought effect

# index was substantially greater during "Above Normal" and "Wet" years compared to other year types
# note that Below Normal years predominate during POD, likely accounting for low values during
# these years for this and all other species

sink("results/am_shad.txt")
print("AMS: random over time (ie no year effect); convincing drought effect")
print(runs_ams)
print(summ(ams3, model.info = T, digits = 3))
print(pw_ams)
sink()

## Delta Smelt ----

### summary stats & figures ----
fmwt %>% 
  group_by(yr_type) %>% 
  summarise(mean = mean(delta_smelt, na.rm = T), years = n())

### time series ----
p <- 
  ggplot(fmwt) +
  aes(x = year, y = delta_smelt) +
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 14) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Delta Smelt", x = NULL, y = "FMWT Index",
       col = "year type", fill = "year type") +
  theme_bw()

ts_ds <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12))

ts_ds

# crashola

### serial randomness ----
# H0: sequence produced in random manner--no pattern to any runs
(runs_ds <- RunsTest(fmwt$delta_smelt, exact = T, na.rm = T))
# non-random...POD?! could be hard to tease out a year type effect

### normality ----
layout(matrix(c(1,2), 1, 2, byrow = T))
hist(fmwt$delta_smelt,
     main = "Delta Smelt",
     xlab = "Delta Smelt Index")
hist(log(fmwt$delta_smelt + 1),
     main = NULL,
     xlab = "Delta Smelt Index, ln(x+1)")
# still left skewed but log transformation improves normality

### boxplot ----
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(delta_smelt+1), fill = yr_type)) +
  geom_boxplot() +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Delta Smelt",
       x = "Year Type",
       y = "Index, ln(x+1)",
       fill = "year type") +
  theme_bw()

dsbox <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 12), # retained for other uses
            legend.position = "none")

print(dsbox)

### boxplot+ ----
# boxplot w pairwise & ts insert
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(delta_smelt + 1), fill = yr_type)) +
  ylim(0, 9) + # was (0, 10.5)
  geom_boxplot() +
  geom_signif(
    comparisons = list(c("Critical", "Wet")),
    y_position = 8, # added to test...
    annotation = "pairwise comparisons NS", 
    tip_length = 0,
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Delta Smelt",
       x = "Year Type",
       y = "Index, ln(x + 1)",
       fill = "year type") +
  theme_bw()

p0 <- p + theme(plot.title = element_text(size = 18),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12),
                legend.position = c(0.1, 0.2))

p1 <-
  ggplot(fmwt) +
  aes(x = year, y = delta_smelt) + 
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 6) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(x = NULL, y = "Index",
       col = "year type", fill = "year type") +
  theme_bw()

p2 <- 
  p1 + theme(axis.text = element_text(size = 8),
             legend.text = element_text(size = 8),
             legend.position = "none") # c(0.9, 0.7) or left, top, right, bottom

(box_ts_ds <- p0 + annotation_custom(ggplotGrob(p2), xmin = 2, xmax = 5.5,
                                     ymin = -0.5, ymax = 2.2))

### linear models, log-transformed -----
# super basic drought effect
# ds1 <- lm(log(delta_smelt + 1) ~ drought, data = fmwt)
# summary(ds1)
# layout(matrix(c(1:4), 2, 2, byrow = T))
# plot(ds1) # Q-Q plot: consider nonlinear model...

# add year as a main effect
# ds2 <- lm(log(delta_smelt + 1) ~ drought + year, data = fmwt)
# summary(ds2)
# layout(matrix(c(1:4), 2, 2, byrow = T))
# plot(ds2) 

ds3 <- lm(log(delta_smelt + 1) ~ yr_type + year, data = fmwt)
layout(matrix(c(1:4), 2, 2, byrow = T))
plot(ds3)
summary(ds3) # year effect (pop crash) but no year type effect
summ(ds3, model.info = F, digits = 3)

# ds4 <- lm(log(delta_smelt + 1) ~ yr_type, data = fmwt[-c(3, 36, 48, 50),])
# dropped "outlier" years: 1972, 2005, 2017, 2019; based on boxplots
# didn't use "year" as main effect...
# summary(ds4)
# layout(matrix(c(1:4), 2, 2, byrow = T))
# plot(ds4) # looks significantly better...

# ds5 <- lm(log(delta_smelt + 1) ~ yr_type + year, data = fmwt[-c(3, 36, 48, 50),])
# dropped "outlier" years: 1972, 2005, 2017, 2019; based on boxplots
# summary(ds5)
# layout(matrix(c(1:4), 2, 2, byrow = T))
# plot(ds5) # hmmm...

# pairwise comparison for estimated marginal means in the lm
# emmeans(ds1, pairwise ~ drought) 
# emmeans(ds2, pairwise ~ drought)
(pw_ds <- emmeans(ds3, pairwise ~ yr_type)) # none of the pairwise comps remotely close to significant
# emmeans(ds4, pairwise ~ yr_type) # very appealing!
# emmeans(ds5, pairwise ~ yr_type)
layout(matrix(1, 1, 1))

### notes ----
# index was greater during "Above Normal" and "Wet" years compared to other year types
# but POD and wacky outliers complicate things

sink("results/delta_smelt.txt")
print("DS: non-random over time (it's a crash, stupid!); no real indication of a drought effect")
print(runs_ds)
print(summ(ds3, model.info = F, digits = 3))
print(pw_ds)
sink()

## Longfin Smelt ----

### summary stats & figures ----
fmwt %>% 
  group_by(yr_type) %>% 
  summarise(mean = mean(longfin_smelt, na.rm = T), years = n())

### times series ----
p <- 
  ggplot(fmwt) +
  aes(x = year, y = longfin_smelt) +
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 14) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Longfin Smelt", x = NULL, y = "FMWT Index",
       col = "year type", fill = "year type") +
  theme_bw()


ts_lfs <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12))

ts_lfs
# yikes. note crash is post-clam and pre-POD. Also...population looks really responsive to year type, at least until the clams appear :-(

### serial randomness ----
# H0: sequence produced in random manner--no pattern to any runs
(runs_lfs <- RunsTest(fmwt$longfin_smelt, exact = T, na.rm = T))
# clearly NOT random

### normality ----
layout(matrix(c(1,2), 1, 2, byrow = T))
hist(fmwt$longfin_smelt,
     main = "Longfin Smelt",
     xlab = "Longfin Smelt Index")
hist(log(fmwt$longfin_smelt),
     main = NULL,
     xlab = "Longfin Smelt Index, ln(x)")
# log transformation greatly improves normality

### boxplot ----
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(longfin_smelt), fill = yr_type)) +
  geom_boxplot() +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Longfin Smelt",
       x = "Year Type",
       y = "Index, ln(x)",
       fill = "year type") +
  theme_bw()

lfsbox <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 12), # retained for other uses
            legend.position = "none")

print(lfsbox)

### boxplot+ ----
# boxplot w pairwise & ts insert
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(longfin_smelt), fill = yr_type)) +
  ylim(-0.5, 15) + # was (0, 15)
  geom_boxplot() +
  geom_signif(
    comparisons = list(c("Critical", "Wet"), c("Critical", "Above Normal"), 
                       c("Dry", "Wet"), c("Below Normal", "Wet")),
    y_position = c(14, 13, 12, 11), # added to test...
    annotation = c("****", "***", "**", "**"), # test = "t.test",
    map_signif_level = TRUE,
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Longfin Smelt",
       x = "Year Type",
       y = "Index, ln(x)",
       fill = "year type") +
  theme_bw()

p0 <- p + theme(plot.title = element_text(size = 18),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12),
                legend.position = c(0.1, 0.2))

p1 <-
  ggplot(fmwt) +
  aes(x = year, y = longfin_smelt) + 
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 6) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(x = NULL, y = "Index",
       col = "year type", fill = "year type") +
  theme_bw()

p2 <- 
  p1 + theme(axis.text = element_text(size = 8),
             legend.text = element_text(size = 8),
             legend.position = "none") # c(0.9, 0.7) or left, top, right, bottom

(box_ts_lfs <- p0 + annotation_custom(ggplotGrob(p2), xmin = 2, xmax = 5.5,
                                      ymin = -1, ymax = 3.3))



### linear models, log-transformed -----
# super basic drought effect
# lfs1 <- lm(log(longfin_smelt) ~ drought, data = fmwt)
# summary(lfs1)
# layout(matrix(c(1:4), 2, 2, byrow = T))
# plot(lfs1) # looks reasonable

# add year as a main effect
# lfs2 <- lm(log(longfin_smelt) ~ drought + year, data = fmwt)
# summary(lfs2)
# layout(matrix(c(1:4), 2, 2, byrow = T))
# plot(lfs2) 

lfs3 <- lm(log(longfin_smelt) ~ yr_type + year, data = fmwt)
layout(matrix(c(1:4), 2, 2, byrow = T))
plot(lfs3) # not bad

# pairwise comparison for estimated marginal means in the lm
# emmeans(lfs1, pairwise ~ drought) 
# emmeans(lfs2, pairwise ~ drought)
(pw_lfs <- emmeans(lfs3, pairwise ~ yr_type)) # woohoo!
+layout(matrix(1, 1, 1))

### notes ----

sink("results/longfin.txt")
print("LFS: non-random; convincing year type effect and strong year effect (ie crash)")
print(runs_lfs)
print(summ(lfs3, model.info = T, digits = 3))
print(pw_lfs)
sink()

## Striped Bass (age-0) ----

### summary stats & figures ----
fmwt %>% 
  group_by(yr_type) %>% 
  summarise(mean = mean(striped_bass_age0, na.rm = T), years = n())

### time series ----
p <- 
  ggplot(fmwt) +
  aes(x = year, y = striped_bass_age0) +
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 14) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Striped Bass, Age-0", x = NULL, y = "FMWT Index",
       col = "year type", fill = "year type") +
  theme_bw()


ts_sb0 <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12))

ts_sb0

# post-clam, pre-POD collapse like longfin...re-read Pascale's paper

### serial randomness ----
# H0: sequence produced in random manner--no pattern to any runs
(runs_sb0 <- RunsTest(fmwt$striped_bass_age0, exact = T, na.rm = T))
# clearly not random...with a crash like that, what'd you expect?!!!

### normality ----
layout(matrix(c(1,2), 1, 2, byrow = T))
hist(fmwt$striped_bass_age0,
     main = "Striped Bass (age-0)",
     xlab = "Striped Bass (age-0) Index")
hist(log(fmwt$striped_bass_age0),
     main = NULL,
     xlab = "Striped Bass (age-0) Index, ln(x)")
# tranformation okay but check some alternatives

### boxplot ----
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(striped_bass_age0), fill = yr_type)) +
  geom_boxplot() +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Striped Bass, Age-0",
       x = "Year Type",
       y = "Index, ln(x)",
       fill = "year type") +
  theme_bw()

sb0box <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 12), # retained for other uses
            legend.position = "none")

print(sb0box)

### boxplot+ ----
# boxplot w pairwise & ts insert
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(striped_bass_age0), fill = yr_type)) +
  ylim(0, 13) + # was (0, 10.5)
  geom_boxplot() +
  geom_signif(
    comparisons = list(c("Critical", "Wet"), c("Dry", "Wet"),
                       c("Below Normal", "Wet"), c("Above Normal", "Wet")),
    y_position = c(12.5, 11.5, 10.5, 9.5), # added to test...
    annotation = c("*", "*", "*", "*"), # test = "t.test",
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Striped Bass, Age-0",
       x = "Year Type",
       y = "Index, ln(x)",
       fill = "year type") +
  theme_bw()

p0 <- p + theme(plot.title = element_text(size = 18),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12),
                legend.position = c(0.1, 0.2))

p1 <-
  ggplot(fmwt) +
  aes(x = year, y = striped_bass_age0) + 
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 6) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(x = NULL, y = "Index",
       col = "year type", fill = "year type") +
  theme_bw()

p2 <- 
  p1 + theme(axis.text = element_text(size = 8),
             legend.text = element_text(size = 8),
             legend.position = "none") # c(0.9, 0.7) or left, top, right, bottom

(box_ts_sb0 <- p0 + annotation_custom(ggplotGrob(p2), xmin = 2, xmax = 5.5,
                                      ymin = -0.5, ymax = 3.5))

### linear models, log-transformed -----
# super basic drought effect
# sb01 <- lm(log(striped_bass_age0) ~ drought, data = fmwt)
# summary(sb01)
# layout(matrix(c(1:4), 2, 2, byrow = T))
# plot(sb01) # can you say "skewed"?!
# shapiro.test(log(fmwt$striped_bass_age0)) # not normally distributed

# add year as a main effect
# sb02 <- lm(log(striped_bass_age0) ~ drought + year, data = fmwt)
# summary(sb02)
# layout(matrix(c(1:4), 2, 2, byrow = T))
# plot(sb02) 

sb03 <- lm(log(striped_bass_age0) ~ yr_type + year, data = fmwt)
layout(matrix(c(1:4), 2, 2, byrow = T))
plot(sb03) # back to looking skewed
shapiro.test(sqrt(fmwt$striped_bass_age0)) # NOT normally distributed but we're not going to worry about it
summ(sb03, model.info = F, digits = 3) # year & year type...what to make of quadratic model result?


# sb04 <- lm(sqrt(striped_bass_age0) ~ yr_type + year, data = fmwt)
# plot(sb04) # for completeness-sake...
# summary(sb04)

# sb05 <- lm(sign(striped_bass_age0) * abs(striped_bass_age0)^(1/3) ~ yr_type + year, data = fmwt)
# plot(sb05)
# summary(sb05)

# pairwise comparison for estimated marginal means in the lm
# emmeans(sb01, pairwise ~ drought) 
# emmeans(sb02, pairwise ~ drought)
(pw_sb0 <- emmeans(sb03, pairwise ~ yr_type)) # looks good!
# emmeans(sb04, pairwise ~ yr_type)
# emmeans(sb05, pairwise ~ yr_type) # warning: results likely in error
layout(matrix(1, 1, 1))

### notes ----

sink("results/stripers.txt")
print("SB0: non-random and convincing year & year type effects")
print("interesting that there's evidence of a year type effect despite the population crash
")
print(summ(sb03, model.info = F, digits = 3))
print(pw_sb0)
print(runs_sb0)
sink()

## Threadfin Shad ----

### summary stats & figures ----
fmwt %>% 
  group_by(yr_type) %>% 
  summarise(mean = mean(threadfin_shad, na.rm = T), years = n())

### time series ----
p <- 
  ggplot(fmwt) +
  aes(x = year, y = threadfin_shad) +
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 14) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Threadfin Shad", x = NULL, y = "FMWT Index",
       col = "year type", fill = "year type") +
  theme_bw()

ts_tfs <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12))

ts_tfs

### serial randomness ----
# H0: sequence produced in random manner--no pattern to any runs
(runs_tfs <- RunsTest(fmwt$threadfin_shad, exact = T, na.rm = T))
# nope! not random...a crash will do that apparently

### normality ----
layout(matrix(c(1,2), 1, 2, byrow = T))
hist(fmwt$threadfin_shad,
     main = "Threadfin Shad",
     xlab = "Threadfin Shad Index")
hist(log(fmwt$threadfin_shad),
     main = NULL,
     xlab = "Threadfin Shad Index, ln(x)")
# log transformation improves normality

### boxplot ----
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(threadfin_shad), fill = yr_type)) +
  geom_boxplot() +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Threadfin Shad",
       x = "Year Type",
       y = "Index, ln(x)",
       fill = "year type") +
  theme_bw()

tfsbox <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 12), # retained for other uses
            legend.position = "none")

print(tfsbox)

### boxplot+ ----
# boxplot w pairwise & ts insert
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(threadfin_shad), fill = yr_type)) +
  ylim(0, 10.5) + # was (0, 10.5)
  geom_boxplot() +
  geom_signif(
    comparisons = list(c("Critical", "Wet")),
    y_position = 10, # added to test...
    annotation = "pairwise comparisons NS", 
    tip_length = 0,
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Delta Smelt",
       x = "Year Type",
       y = "Index, ln(x + 1)",
       fill = "year type") +
  theme_bw()

p0 <- p + theme(plot.title = element_text(size = 18),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12), 
                legend.position = c(0.1, 0.2))

p1 <-
  ggplot(fmwt) +
  aes(x = year, y = threadfin_shad) + 
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 6) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(x = NULL, y = "Index",
       col = "year type", fill = "year type") +
  theme_bw()

p2 <- 
  p1 + theme(axis.text = element_text(size = 8),
             legend.text = element_text(size = 8),
             legend.position = "none") # c(0.9, 0.7) or left, top, right, bottom

(box_ts_tfs <- p0 + annotation_custom(ggplotGrob(p2), xmin = 2, xmax = 5.5,
                                      ymin = -0.5, ymax = 4))

### linear models, log-transformed -----
# super basic drought effect
# tfs1 <- lm(log(threadfin_shad) ~ drought, data = fmwt)
# summary(tfs1)
# layout(matrix(c(1:4), 2, 2, byrow = T))
# plot(tfs1) # looks reasonable

# add year as a main effect
# tfs2 <- lm(log(threadfin_shad) ~ drought + year, data = fmwt)
# summary(tfs2)
# layout(matrix(c(1:4), 2, 2, byrow = T))
# plot(tfs2) # yikes! residuals are ugly (low abundance during "below normal" 
# years)--nonlinear seems unlikely, others look okay

tfs3 <- lm(log(threadfin_shad) ~ yr_type + year, data = fmwt)
layout(matrix(c(1:4), 2, 2, byrow = T))
plot(tfs3)
summ(tfs3, digits = 3) # sig year effect (POD crash)

# try as above but remove near-outliers: 1997, 2001, 2009
# tfs4 <- lm(log(threadfin_shad) ~ yr_type + year, data = fmwt[-c(28, 32, 40),])
# layout(matrix(c(1:4), 2, 2, byrow = T))
# plot(tfs4) # might be slightly worse
# summary(tfs4)

# pairwise comparison for estimated marginal means in the lm
# emmeans(tfs1, pairwise ~ drought) 
# emmeans(tfs2, pairwise ~ drought)
(pw_tfs <- emmeans(tfs3, pairwise ~ yr_type)) # nada
layout(matrix(1, 1, 1))

### notes ----
# clearly change over time (tfs3 year effect) but no support for a drought (year type) effect
sink("results/threadfin.txt")
print("TFS: non-random over time")
print("clearly change over time (tfs3 year effect) but no support for a drought (year type) effect")
print(summ(tfs3, model.info = F, digits = 3))
print(pw_tfs)
print(runs_tfs)
sink()

# data prep ----
# SHOULDN'T BE NECESSARY!

fmwt <- read.csv("data/FMWTindices.csv") %>% 
  clean_names()

yeartypes <- read_excel("data/Integrated data set.xlsx", sheet = "yearassignments") %>% clean_names()

fmwt <- left_join(fmwt, yeartypes, by = "year") %>%
  filter(year > 1969) %>% 
  mutate(yr_type = factor(yr_type, 
                          levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"), 
                          ordered = TRUE)) # "ordered" crucial b/c of course these are ordered factors!!!

save(fmwt, file = "data/fmwt.RData")
str(fmwt) # shows yr_type as ordered factor

fmwt_trs_long <-
  fmwt %>% 
  select(-(c(index, drought))) %>% 
  mutate("Threadfin Shad" = log(threadfin_shad),
         "American Shad" = log(american_shad),
         "Delta Smelt" = log(delta_smelt + 1),
         "Longfin Smelt" = log(longfin_smelt),
         "Striped Bass, Age-0" = log(striped_bass_age0),
         .keep = "unused") %>% 
  pivot_longer(cols = 3:7, names_to = "species", values_to = "ln_index")

save(fmwt_trs_long, file = "data/fmwt_trs_long.RData")
str(fmwt_trs_long)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SCRATCH ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(janitor)
library(forcats)
library(gridExtra)
library(rmarkdown)

fmwt_raw_long <-
  fmwt %>% 
  select(-(c(index, drought))) %>% 
  pivot_longer(cols = 2:6, names_to = "species", values_to = "index")

fmwt_dontuse_long <-
  fmwt %>% 
  select(-(c(index, drought))) %>% 
  mutate("Threadfin Shad" = log(threadfin_shad),
         "American Shad" = log(american_shad),
         "Delta Smelt" = log(delta_smelt + 1),
         "Longfin Smelt" = log(longfin_smelt),
         "Striped Bass, Age-0" = sign(striped_bass_age0) * abs(striped_bass_age0)^(1/3),
         .keep = "unused") %>% 
  pivot_longer(cols = 3:7, names_to = "species", values_to = "index")

ggplot(fmwt_trs_long, aes(x = yr_type, y = index, fill = species)) +
  geom_boxplot() + 
  theme_bw()

# poly model with threadfin
lm(log(threadfin_shad) ~ poly(unclass(yr_type), 4) + year, data = fmwt) %>% summary()
# slight differences in coefficients but...why bother?!

library(jtools)
# see https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
plot_summs(tfs3, inner_ci_level = 0.9)
# unclear to me what this tells you w/re to the model type (linear, quad, etc)...makes sense for 'year' but not the others
plot_summs(ams3, plot.distributions = TRUE, inner_ci_level = 0.9)
summ(ds3, model.info = T, digits = 3) # makes sense: "trend" is like falling of a cliff; of course no linear model is going to describe this ts well!
lm(log(delta_smelt + 1) ~ year, data = fmwt) %>% summ() # NOT satisfying...so that straight line is strongly inclined downwards, BFD. And yet, why bother trying to fit those linear models at all?
plot_summs(ds3, plot.distributions = F, inner_ci_level = 0.9)

## American Shad ts ----
### origional ----
ggplot(fmwt, aes(x = year, y = american_shad)) +
  geom_col(aes(fill = yr_type, y = max(american_shad, na.rm = T)), alpha = 0.5) +
  scale_fill_manual(values = pal_yrtype) +
  geom_point(shape = "-", size = 16) + 
  geom_line() +
  labs(title = "American Shad", x = "Year", y = "FMWT Index") +
  theme_bw()

### modified ----
ggplot(fmwt) +
  aes(x = year, y = american_shad) +
  geom_line() +
  geom_point(shape = 21, size = 16)

ggplot(fmwt) +
  aes(x = year, y = american_shad) +
  geom_line() +
  geom_point(aes(color = yr_type, fill = yr_type), 
             shape = 21, size = 14) +
  labs(title = "American Shad", x = "Year", y = "FMWT Index") +
  theme_bw()

ggplot(fmwt) +
  aes(x = year, y = american_shad) +
  geom_line() +
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 14) +
  labs(title = "American Shad", x = "Year", y = "FMWT Index",
       col = "year type", fill = "year type") +
  theme_bw()

p <- ggplot(fmwt) +
  aes(x = year, y = american_shad) +
  geom_line() +
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 14) +
  labs(title = "American Shad", x = "Year", y = "FMWT Index",
       col = "year type", fill = "year type") +
  theme_bw()

p + theme(plot.title = element_text(size = 28),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 12))

### boxplot+ ----

library(ggsignif)
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(longfin_smelt), fill = yr_type)) +
  ylim(0, 12) +
  geom_boxplot() +
  geom_signif( # added this
    comparisons = list(c("Critical", "Above Normal"), c("Critical", "Wet"),
                       c("Dry", "Wet"), c("Below Normal", "Wet")),
    test = "t.test",
    map_signif_level = TRUE,
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Longfin Smelt",
       x = "Year Type",
       y = "Index, ln(x)",
       fill = "year type") +
  theme_bw()

p0 <- p + theme(plot.title = element_text(size = 18),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12))

p1 <-
  ggplot(fmwt) +
  aes(x = year, y = longfin_smelt) + 
  geom_line() +
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 6) +
  labs(x = NULL, y = NULL,
       col = "year type", fill = "year type") +
  theme_bw()

p2 <- 
  p1 + theme(axis.text = element_text(size = 8),
             legend.text = element_text(size = 8),
             legend.position = "none") # c(0.9, 0.7) or left, top, right, bottom

p0 + annotation_custom(ggplotGrob(p2), xmin = 2, xmax = 5.5,
                       ymin = -0.5, ymax = 3.5)

lfsbox <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 12), # retained for other uses
            legend.position = "none")

print(lfsbox)

### boxplot+2 -----
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(longfin_smelt), fill = yr_type)) +
  ylim(0, 12) +
  geom_boxplot() +
  geom_signif( # added this
    comparisons = list(c("Critical", "Above Normal"), c("Critical", "Wet"),
                       c("Dry", "Wet"), c("Below Normal", "Wet")),
    test = "t.test",
    map_signif_level = TRUE,
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "Longfin Smelt",
       x = "Year Type",
       y = "Index, ln(x)",
       fill = "year type") +
  theme_bw()

p0 <- p + theme(plot.title = element_text(size = 18),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12))

p1 <-
  ggplot(fmwt) +
  aes(x = year, y = log(longfin_smelt)) + 
  geom_line() +
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 6) +
  labs(x = NULL, y = "Index, ln(x)",
       col = "year type", fill = "year type") +
  theme_bw()

p2 <- 
  p1 + theme(axis.text = element_text(size = 8),
             legend.text = element_text(size = 8),
             legend.position = "none") # c(0.9, 0.7) or left, top, right, bottom

p0 + annotation_custom(ggplotGrob(p2), xmin = 2, xmax = 5.5,
                       ymin = -0.5, ymax = 3.5)

### boxplot+3 ----
# boxplot w pairwise & ts insert 
# notes: ts not log-transformed, inset ylab changed to reflect
p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(american_shad), fill = yr_type)) +
  ylim(0, 10.5) +
  geom_boxplot() +
  geom_signif( # added this
    comparisons = list(c("Critical", "Above Normal"), c("Critical", "Wet"),
                       c("Dry", "Wet"), c("Below Normal", "Wet")),
    test = "t.test",
    map_signif_level = TRUE,
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "American Shad",
       x = "Year Type",
       y = "Index, ln(x)",
       fill = "year type") +
  theme_bw()

p0 <- p + theme(plot.title = element_text(size = 18),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12),
                legend.position = c(0.1, 0.2))

p1 <-
  ggplot(fmwt) +
  aes(x = year, y = american_shad) + 
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 6) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(x = NULL, y = "Index",
       col = "year type", fill = "year type") +
  theme_bw()

p2 <- 
  p1 + theme(axis.text = element_text(size = 8),
             legend.text = element_text(size = 8),
             legend.position = "none") # c(0.9, 0.7) or left, top, right, bottom

(box_ts_ams <- p0 + annotation_custom(ggplotGrob(p2), xmin = 2, xmax = 5.5,
                                      ymin = -0.5, ymax = 4.5))

box_ts_ams

### boxplot+4 ----

# boxplot w pairwise & ts insert 
# notes: ts not log-transformed, inset ylab changed to reflect
# modified boxplot+3 to make the pairwise stuff easier to interpret

p <- 
  ggplot(fmwt, aes(x = yr_type, y = log(american_shad), fill = yr_type)) +
  ylim(0, 10.5) +
  geom_boxplot() +
  geom_signif( # added this
    comparisons = list(c("Critical", "Above Normal"), c("Critical", "Wet"),
                       c("Dry", "Wet"), c("Below Normal", "Wet")),
    step_increase = 0.07, # vector w increase of fractional ht for every add'l comp to minimize overlap
    vjust = 0.6, # moves the text up or down relative to the bracket
    test = "t.test",
    map_signif_level = TRUE,
    textsize = 8
  ) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "American Shad",
       x = "Year Type",
       y = "Index, ln(x)",
       fill = "year type") +
  theme_bw()

p0 <- p + theme(plot.title = element_text(size = 18),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12),
                legend.position = c(0.1, 0.2))

p1 <-
  ggplot(fmwt) +
  aes(x = year, y = american_shad) + 
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 6) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(x = NULL, y = "Index",
       col = "year type", fill = "year type") +
  theme_bw()

p2 <- 
  p1 + theme(axis.text = element_text(size = 8),
             legend.text = element_text(size = 8),
             legend.position = "none") # c(0.9, 0.7) or left, top, right, bottom

(box_ts_ams <- p0 + annotation_custom(ggplotGrob(p2), xmin = 2, xmax = 5.5,
                                      ymin = -0.5, ymax = 4.5))

box_ts_ams # YAY!!!

### fixing yr_type colors -----
p <- 
  ggplot(fmwt) +
  aes(x = year, y = american_shad) +
  geom_line() +
  # coord_cartesian(ylim= c(0, 20000)) + # retain for longfin (and?)
  geom_point(aes(col = yr_type, fill = yr_type), 
             shape = 21, size = 14) +
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype) +
  labs(title = "American Shad", x = NULL, y = "FMWT Index",
       col = "year type", fill = "year type") +
  theme_bw()

ts_ams <- 
  p + theme(plot.title = element_text(size = 28),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12))

ts_ams

dat <- tibble(year = 1970:1974, 
              lfs = c(120,140,110,180,200), 
              yr_type = as_factor(c("Critical", "Dry", "Critical","Above Normal", "Wet")))
ggplot(dat) +
  aes(x = year, y = lfs) +
  geom_line() +
  geom_point(aes(color = yr_type, fill = yr_type), shape = 21, size = 6) + 
  scale_color_manual(values = pal_yrtype) +
  scale_fill_manual(values = pal_yrtype)
