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
setwd("~/GitHub/WASH_typhoid")

# start time
start_time <- Sys.time ()
print (paste0 ("start time = ", start_time))

# -----------------------------------------------------------------------------

### Importing data from Google Sheets
dat <- read_sheet('https://docs.google.com/spreadsheets/d/16I5yak3-j3rEqLLcdFCcqzeQSyYjSGihXIbmZxqEILs/edit#gid=2036014622')

### hospitalization day
# data set
dat_study <- dat[, c("Study", "Pathogen", "Suceptible_profile", "Resistance_profile", "Country")]
         
dat_hos_res <- dat[, 50:59]

names(dat_hos_res) <- c("hospital_day_res", "statistical_measure", "comment", "uncertainty/range",
                        "SD", "uncertainty/range_comment", "higher", "lower", "other", "n")

dat_stu_hos_res <- cbind(dat_study, dat_hos_res)

dat_stu_hos_res <- dat_stu_hos_res[(dat_stu_hos_res$Resistance_profile != "Mixed") &
                                   (dat_stu_hos_res$Resistance_profile != "Susceptible") &
                                   (dat_stu_hos_res$hospital_day_res   != "NULL"), ]

dat_stu_hos_res <- data.table(dat_stu_hos_res)

dat_stu_hos_res$hospital_day_res <- as.numeric(dat_stu_hos_res$hospital_day_res)

dat_stu_hos_res[dat_stu_hos_res == "NULL"] <- NA

dat_stu_hos_res$higher <- as.numeric(dat_stu_hos_res$higher)

dat_stu_hos_res$lower <- as.numeric(dat_stu_hos_res$lower)

# adjust variables
dat_stu_hos_res[statistical_measure == "Mean", Mean := hospital_day_res]

dat_stu_hos_res[statistical_measure == "Median", Median := hospital_day_res]

dat_stu_hos_res[`uncertainty/range` == "Interquartile range", higher_i := higher]

dat_stu_hos_res[`uncertainty/range` == "Interquartile range", lower_i := lower]

dat_stu_hos_res[`uncertainty/range` == "Range", higher_r := higher]

dat_stu_hos_res[`uncertainty/range` == "Range", lower_r := lower]

dat_stu_hos_res$Pathogen <- factor(dat_stu_hos_res$Pathogen,
                                   levels = c("S. Typhi", 
                                              "S. Typhi and S. Paratyphi",
                                              "S. non-Typhi"))

dat_meta <- dat_stu_hos_res[!(is.na(higher) & statistical_measure == "Median"), ]
# dat_meta <- dat_meta[!is.na(`uncertainty/range`), ]
# dat_meta <- dat_meta[dat_meta$Pathogen != "Salmonella non-typhi", ]

dat_meta <- dat_meta %>% arrange(Pathogen, Resistance_profile)

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
       
png(file = "forestplot.png", width = 3300, height = 1000, res = 300)

forest(ma.data,
       leftcols = c("Study", "Pathogen", "Resistance_profile", "Country"),
       leftlabs = c("Study", "Pathogen", "Resistance", "Country"),
       rightlabs = c("Mean", "95% CI"),
#      subgroup = FALSE,
       xlim = c(1,15),
       print.byvar = TRUE)

dev.off()




