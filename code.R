---
  title: "Hospital-related costs and length of stay of antibiotic-resistant Salmonella infections: a systematic review and meta-analysis"
---

# load libraries
library(googlesheets4)
library(data.table)
library(ggplot2)
library(tidyr)
library(gcookbook)
library(rworldmap)
library(dplyr)
library(meta)
library(metafor)
library(tidyverse)

# remove all objects from workspace
rm (list = ls ())
setwd("~/GitHub/amr_cost")

# start time
start_time <- Sys.time ()
print (paste0 ("start time = ", start_time))

# -----------------------------------------------------------------------------

### Importing data from Google Sheets
dat <- read_sheet('https://docs.google.com/spreadsheets/d/16I5yak3-j3rEqLLcdFCcqzeQSyYjSGihXIbmZxqEILs/edit#gid=2036014622')

### Study data
dat_study <- dat[, c("Study", "Pathogen", "Susceptible_profile", "Resistance_profile", "Country")]
dat_study$Pathogen <- factor(dat_study$Pathogen,
                                   levels = c("S. Typhi", 
                                              "S. Typhi and S. Paratyphi",
                                              "S. non-Typhi"))

dat_study$Resistance_profile <- factor(dat_study$Resistance_profile,
                             levels = c("Amikacin res.",
                                        "Ciprofloxacin res.",
                                        "Nalidixic acid res.",
                                        "MDR",
                                        "XDR"))

# Hospitalization day data -- resistance
dat_los_res <- dat[, 59:67]

dat_stu_hos_res <- cbind(dat_study, dat_los_res)

dat_stu_hos_res <- dat_stu_hos_res[dat_stu_hos_res$Resistance_profile != "Mixed" &
                                   dat_stu_hos_res$Resistance_profile != "Susceptible" &
                                   dat_stu_hos_res$`How many days did the hospitalisation last?_resistant` != "NULL", ]

# Hospitalization day data -- susceptible
dat_los_sus <- dat[, 50:58]

dat_stu_hos_sus <- cbind(dat_study, dat_los_sus)

dat_stu_hos_sus <- dat_stu_hos_sus[((dat_stu_hos_sus$Susceptible_profile != "Mixed") &
                                   !is.na(dat_stu_hos_sus$Susceptible_profile) &
                                   (dat_stu_hos_sus$`How many days did the hospitalisation last?_susceptible` != "NULL")), ]


# Duration of fever -- resistance
dat_fev_res <- dat[, 41:49]

dat_stu_fev_res <- cbind(dat_study, dat_fev_res)

dat_stu_fev_res <- dat_stu_fev_res[((dat_stu_fev_res$Resistance_profile != "Mixed") &
                                    !is.na(dat_stu_fev_res$Resistance_profile) &
                                    (dat_stu_fev_res$`How many days did the symptoms last?_resistant` != "NULL")), ]

# Duration of fever -- susceptible
dat_fev_sus <- dat[, 32:40]

dat_stu_fev_sus <- cbind(dat_study, dat_fev_sus)

dat_stu_fev_sus <- dat_stu_fev_sus[((dat_stu_fev_sus$Susceptible_profile != "Mixed") &
                                    !is.na(dat_stu_fev_sus$Susceptible_profile) &
                                    (dat_stu_fev_sus$`How many days did the symptoms last?_susceptible` != "NULL")), ]

# create function to generate forest plot
forest_plot <- function(dat, type, width_input, height_input, xlim_low, xlim_high, file_name){
  
  dat <- data.table(dat)
  
  colnames(dat)[6:14] <- c("day", "statistical_measure", "comment", "uncertainty/range",
                           "uncertainty/range_comment", "higher", "lower", "other", "n")
  
  dat[dat == "NULL"] <- NA
  
  dat$day <- as.numeric(dat$day)
  
  dat$higher <- as.numeric(dat$higher)
  
  dat$lower <- as.numeric(dat$lower)
  
  dat$other <- as.numeric(dat$other)
  
  # adjust variables
  dat[statistical_measure == "Mean", Mean := day]
  
  dat[statistical_measure == "Median", Median := day]
  
  dat[`uncertainty/range` == "Interquartile range", higher_i := higher]
  
  dat[`uncertainty/range` == "Interquartile range", lower_i := lower]
  
  dat[`uncertainty/range` == "Range", higher_r := higher]
  
  dat[`uncertainty/range` == "Range", lower_r := lower]
  
  dat[`uncertainty/range` == "Standard deviation", SD := other]
  
  # dat_meta <- dat_meta[!is.na(`uncertainty/range`), ]
  # dat_meta <- dat_meta[dat_meta$Pathogen != "Salmonella non-typhi", ]
  
  dat_meta <- dat[!(is.na(higher) & statistical_measure == "Median"), ]
  
  if (paste(type) == "Resistance") {
    dat_meta <- dat_meta %>% arrange(Pathogen, Resistance_profile)
    dat_meta <- dat_meta %>% rename("Resistance" = "Resistance_profile")
  } else {
    dat_meta <- dat_meta %>% arrange(Pathogen, Susceptible_profile)
    dat_meta <- dat_meta %>% rename("Susceptible" = "Susceptible_profile")
  }
  
  # meta & forestplot -- Salmonella_typhi
  ma.data <- metamean(n = n, 
                      mean = Mean, 
                      data = dat_meta, 
                      sd = SD,
                      median = Median,
                      q1 = lower_i, 
                      q3 = higher_i,
                      min = lower_r, 
                      max = higher_r,
  #                   subgroup = Pathogen,
                      common = FALSE,
                      random = TRUE,
                      overall = FALSE,
                      overall.hetstat = FALSE)
  
  png(file = paste(file_name), width = width_input, height = height_input, res = 300)
  
  forest(ma.data,
         leftcols = c("Study", "Pathogen", paste(type), "Country"),
         rightlabs = c("Mean", "95% CI"),
  #      subgroup = FALSE,
         xlim = c(xlim_low, xlim_high),
         print.byvar = TRUE)
  
  dev.off()
  
  return(ma.data)}


los_res <- forest_plot(dat          = dat_stu_hos_res,
                       type         = "Resistance",
                       width_input  = 3200,
                       height_input = 1100,
                       xlim_low     = 1, 
                       xlim_high    = 15,
                       file_name    = "lenth_of_hospital_stay_res.png")

los_sus <- forest_plot(dat          = dat_stu_hos_sus, 
                       type         = "Susceptible",
                       width_input  = 3200,
                       height_input = 600,
                       xlim_low     = 1, 
                       xlim_high    = 15,
                       file_name    = "lenth_of_hospital_stay_sus.png")

fev_res <- forest_plot(dat          = dat_stu_fev_res,
                       type         = "Resistance",
                       width_input  = 3200,
                       height_input = 1100,
                       xlim_low     = 1, 
                       xlim_high    = 30,
                       file_name    = "duration_of_fever_res.png")
fev_sus <- forest_plot(dat          = dat_stu_fev_sus, 
                       type         = "Susceptible",
                       width_input  = 3200,
                       height_input = 600,
                       xlim_low     = 1, 
                       xlim_high    = 30,
                       file_name    = "duration_of_fever_stay_sus.png")
