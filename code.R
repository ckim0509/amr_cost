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
                                              "S. Paratyphi",
                                              "S. Typhi and S. Paratyphi",
                                              "S. non-Typhi"))

# unique(dat_study$Resistance_profile)
dat_study$Resistance_profile <- factor(dat_study$Resistance_profile,
                             levels = c("Amikacin res.",
                                        "Ampicillin res.",
                                        "Ciprofloxacin res.",
                                        "Nalidixic acid res.",
                                        "res.",
                                        "Tetracycline res.",
                                        "MDR",
                                        "XDR"))

# sort(unique(dat_study$Susceptible_profile))
dat_study$Susceptible_profile <- factor(dat_study$Susceptible_profile,
                                       levels = c("Pansusceptible",
                                                  "Ampicillin sus.",
                                                  "Ciprofloxacin sus.",
                                                  "Nalidixic acid sus.", 
                                                  "Tetracycline sus.",
                                                  "Drug-sensitive",
                                                  "Mixed",
                                                  "Non-XDR"))

# Hospitalization day data -- resistance
dat_los_res <- dat[, 59:67]

dat_stu_hos_r <- cbind(dat_study, dat_los_res)

dat_stu_hos_r <- dat_stu_hos_r[dat_stu_hos_r$Resistance_profile != "Susceptible" &
                               dat_stu_hos_r$`How many days did the hospitalisation last?_resistant` != "NULL", ]

# Hospitalization day data -- susceptible
dat_los_sus <- dat[, 50:58]

dat_stu_hos_s <- cbind(dat_study, dat_los_sus)

dat_stu_hos_s <- dat_stu_hos_s[(!is.na(dat_stu_hos_s$Susceptible_profile) &
                                   (dat_stu_hos_s$`How many days did the hospitalisation last?_susceptible` != "NULL")), ]

# Hospitalization day data -- S. typhi & S. paratyphi / S. non-typhi 
dat_stu_hos_res <- data.table(dat_stu_hos_r)
dat_stu_hos_sus <- data.table(dat_stu_hos_s)

dat_stu_hos_res[, Profile := Resistance_profile]
dat_stu_hos_sus[, Profile := Susceptible_profile]

dat_stu_hos_res <- dat_stu_hos_res %>% rename("days" = "How many days did the hospitalisation last?_resistant")
dat_stu_hos_sus <- dat_stu_hos_sus %>% rename("days" = "How many days did the hospitalisation last?_susceptible")

dat_stu_hos_res_typhi <- dat_stu_hos_res[Pathogen == "S. Typhi" | 
                                         Pathogen == "S. Typhi and S. Paratyphi" |
                                         Pathogen == "S. Paratyphi",]
dat_stu_hos_sus_typhi <- dat_stu_hos_sus[Pathogen == "S. Typhi" | 
                                         Pathogen == "S. Typhi and S. Paratyphi" |
                                         Pathogen == "S. Paratyphi",]

dat_stu_hos_res_nontyphi <- dat_stu_hos_res[Pathogen == "S. non-Typhi",]
dat_stu_hos_sus_nontyphi <- dat_stu_hos_sus[Pathogen == "S. non-Typhi",]

# Duration of fever -- resistance
dat_fev_res <- dat[, 41:49]

dat_stu_fev_r <- cbind(dat_study, dat_fev_res)

dat_stu_fev_r <- dat_stu_fev_r[(!is.na(dat_stu_fev_r$Resistance_profile) &
                                    (dat_stu_fev_r$`How many days did the symptoms last?_resistant` != "NULL")), ]

# Duration of fever -- susceptible
dat_fev_sus <- dat[, 32:40]

dat_stu_fev_s <- cbind(dat_study, dat_fev_sus)

dat_stu_fev_s <- dat_stu_fev_s[(!is.na(dat_stu_fev_s$Susceptible_profile) &
                                    (dat_stu_fev_s$`How many days did the symptoms last?_susceptible` != "NULL")), ]

# Duration of fever data -- S. typhi & S. paratyphi / S. non-typhi 
dat_stu_fev_res <- data.table(dat_stu_fev_r)
dat_stu_fev_sus <- data.table(dat_stu_fev_s)

dat_stu_fev_res <- dat_stu_fev_res[`What statistical measure was used to report the length of a symptomatic infection?_comment...43` 
                == "fever clearance time",]
dat_stu_fev_sus <- dat_stu_fev_sus[`What statistical measure was used to report the length of a symptomatic infection?_comment...34` 
                == "fever clearance time",]

dat_stu_fev_res[, Profile := Resistance_profile]
dat_stu_fev_sus[, Profile := Susceptible_profile]

dat_stu_fev_res <- dat_stu_fev_res %>% rename("days" = "How many days did the symptoms last?_resistant")
dat_stu_fev_sus <- dat_stu_fev_sus %>% rename("days" = "How many days did the symptoms last?_susceptible")

dat_stu_fev_res_typhi <- dat_stu_fev_res[Pathogen == "S. Typhi" | 
                                         Pathogen == "S. Typhi and S. Paratyphi" |
                                         Pathogen == "S. Paratyphi",]
dat_stu_fev_sus_typhi <- dat_stu_fev_sus[Pathogen == "S. Typhi" | 
                                         Pathogen == "S. Typhi and S. Paratyphi" |
                                         Pathogen == "S. Paratyphi",]

dat_stu_fev_res_nontyphi <- dat_stu_fev_res[Pathogen == "S. non-Typhi",]
dat_stu_fev_sus_nontyphi <- dat_stu_fev_sus[Pathogen == "S. non-Typhi",]


# Cost -- resistance
dat_cost_res <- dat[, c(68:69, 70:78)]

dat_stu_cost_res <- cbind(dat_study, dat_cost_res)

dat_stu_cost_res <- dat_stu_cost_res[!is.na(dat_stu_cost_res$`What cost was measured?`),]
  
dat_stu_cost_res <- dat_stu_cost_res[!is.na(dat_stu_cost_res$Resistance_profile),]
dat_stu_cost_res <- dat_stu_cost_res[,-c(6:7)]

# Cost -- susceptible
dat_cost_sus <- dat[, c(68:69, 79:87)]

dat_stu_cost_sus <- cbind(dat_study, dat_cost_sus)

dat_stu_cost_sus <- dat_stu_cost_sus[!is.na(dat_stu_cost_sus$`What cost was measured?`),]

dat_stu_cost_sus <- dat_stu_cost_sus[dat_stu_cost_sus$`What cost was measured?` == "Medical costs"|
                                     dat_stu_cost_sus$`What cost was measured?` == "Hospital costs" |
                                     dat_stu_cost_sus$`What cost was measured?` == "Direct medical costs" |
                                     dat_stu_cost_sus$`What cost was measured?` == "Inpatient treatment costs",]

dat_stu_cost_sus <- dat_stu_cost_sus[!is.na(dat_stu_cost_sus$Susceptible_profile),]
dat_stu_cost_sus <- dat_stu_cost_sus[,-c(6:7)]

# Cost data -- S. typhi & S. paratyphi / S. non-typhi 
dat_stu_cost_res_r <- data.table(dat_stu_cost_res)
dat_stu_cost_sus_s <- data.table(dat_stu_cost_sus)

dat_stu_cost_res_r <- dat_stu_cost_res_r %>% rename("days" = "What was the per-patient cost of treating a patient infected with a drug resistant pathogen?")
dat_stu_cost_sus_s <- dat_stu_cost_sus_s %>% rename("days" = "What was the per-patient cost of treating a patient infected with a drug-susceptible pathogen or with a pathogen where resistance was not measured?")

dat_stu_cost_res_r[, resistance := "resistance"]
dat_stu_cost_sus_s[, resistance := "susceptible"]

dat_stu_cost <- rbind(dat_stu_cost_res_r, dat_stu_cost_sus_s, use.names=FALSE)

dat_stu_cost[resistance == "resistance", Profile := Resistance_profile]
dat_stu_cost[resistance == "susceptible", Profile := Susceptible_profile]


dat <- dat_stu_hos_res_typhi

# create function to generate forest plot
forest_plot <- function(dat, subgroup_input, width_input, height_input, xlim_low, xlim_high, file_name){
  
  dat <- data.table(dat)
  
  colnames(dat)[6:14] <- c("day", "statistical_measure", "comment", "uncertainty/range",
                           "uncertainty/range_comment", "higher", "lower", "other", "n")
  
  dat[dat == "NULL"] <- NA
  
  dat$day <- as.numeric(dat$day)
  
  dat$higher <- as.numeric(dat$higher)
  
  dat$lower <- as.numeric(dat$lower)
  
  dat$other <- as.numeric(dat$other)
  
  dat$n <- as.numeric(dat$n)
  
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

  dat_meta <- dat_meta %>% arrange(Pathogen, Profile)
  
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
                      subgroup = Profile,
                      random = TRUE,
                      fixed = FALSE,
                      overall = FALSE)
  
  png(file = paste(file_name), width = width_input, height = height_input, res = 300)
  
  if(subgroup_input == "TRUE"){
  forest(ma.data,
         leftcols = c("Study", "Pathogen", "Profile", "Country"),
         rightlabs = c("Mean", "95% CI"),
         subgroup = TRUE,
         xlim = c(xlim_low, xlim_high),
         print.byvar = TRUE)}
  
  else{
  forest(ma.data,
    leftcols = c("Study", "Pathogen", "Profile", "Country"),
    rightlabs = c("Mean", "95% CI"),
    subgroup = FALSE,
    xlim = c(xlim_low, xlim_high),
    print.byvar = TRUE)}
  
  dev.off()
  
  return(ma.data)}


# length of hospital stay;
los_res_typhi <- forest_plot(dat            = dat_stu_hos_res_typhi,
                             subgroup_input = "TRUE",
                             width_input    = 3200,
                             height_input   = 1600,
                             xlim_low       = 1, 
                             xlim_high      = 15,
                             file_name      = "length_of_hospital_stay_res_typhi.png")

los_sus_typhi <- forest_plot(dat            = dat_stu_hos_sus_typhi,
                             subgroup_input = "TRUE",
                             width_input    = 3200,
                             height_input   = 1900,
                             xlim_low       = 1, 
                             xlim_high      = 15,
                             file_name      = "length_of_hospital_stay_sus_typhi.png")

los_res_nontyphi <- forest_plot(dat            = dat_stu_hos_res_nontyphi,
                                subgroup_input = "TRUE",
                                width_input    = 3200,
                                height_input   = 1400,
                                xlim_low       = 1, 
                                xlim_high      = 15,
                                file_name      = "length_of_hospital_stay_res_nontyphi.png")

los_sus_nontyphi <- forest_plot(dat            = dat_stu_hos_sus_nontyphi,
                                subgroup_input = "TRUE",
                                width_input    = 3200,
                                height_input   = 1600,
                                xlim_low       = 1, 
                                xlim_high      = 15,
                                file_name      = "length_of_hospital_stay_sus_nontyphi.png")


fev_res_typhi <- forest_plot(dat            = dat_stu_fev_res_typhi,
                              subgroup_input = "TRUE",
                              width_input    = 3200,
                              height_input   = 1500,
                              xlim_low       = 1, 
                              xlim_high      = 30,
                              file_name      = "duration_of_fever_res_typhi.png")


fev_sus_typhi <- forest_plot(dat            = dat_stu_fev_sus_typhi,
                             subgroup_input = "TRUE",
                             width_input    = 3200,
                             height_input   = 1300,
                             xlim_low       = 1, 
                             xlim_high      = 30,
                             file_name      = "duration_of_fever_sus_typhi.png")

fev_res_nontyphi <- forest_plot(dat         = dat_stu_fev_res_nontyphi,
                             subgroup_input = "TRUE",
                             width_input    = 3200,
                             height_input   = 1600,
                             xlim_low       = 1, 
                             xlim_high      = 30,
                             file_name      = "duration_of_fever_res_nontyphi.png")

fev_sus_nontyphi <- forest_plot(dat         = dat_stu_fev_sus_nontyphi,
                             subgroup_input = "TRUE",
                             width_input    = 3200,
                             height_input   = 600,
                             xlim_low       = 1, 
                             xlim_high      = 30,
                             file_name      = "duration_of_fever_sus_nontyphi.png")

# cost
cost     <- forest_plot(dat            = dat_stu_cost, 
                        subgroup_input = "TRUE",
                        width_input    = 3500,
                        height_input   = 1200,
                        xlim_low       = 1, 
                        xlim_high      = 5000,
                        file_name      = "cost.png")
