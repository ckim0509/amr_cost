---
  title: "Hospital-related costs of antibiotic-resistant enteric infections: 
  a systematic review and meta-analysis"
---

Sys.setenv(LANG = "en")

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
dat <- data.table(read_sheet('https://docs.google.com/spreadsheets/d/1Bjl8VqHE9Cfy5pFuCunVg1IZyE_n7tVLYl86vF27UBI/edit?usp=sharing'))
dat[is.na(`Should this study be included?_comment`),
    `Should this study be included?_comment` := "Yes"]
dat <- dat[((`Should this study be included?` != "No") &
              (`Should this study be included?_comment` != "No")),]

### ETEC & Shigella
dat <- dat[((Pathogen == "E Coli" | Pathogen == "Shigella spp.")), ]

### Study data
dat_study <- dat[, c("Study", "Pathogen", "Susceptible_profile", "Resistance_profile", "Country")]

# Hospitalization day data -- susceptible
dat_hos_sus <- cbind(dat_study, dat[, 49:57])
dat_hos_sus <- dat_hos_sus[(!is.na(dat_hos_sus$Susceptible_profile) &
                              (dat_hos_sus$`How many days did the hospitalisation last?_susceptible` != "NULL")), ]

# Hospitalization day data -- resistance
dat_hos_res <- cbind(dat_study, dat[, 58:66])
dat_hos_res <- dat_hos_res[dat_hos_res$Resistance_profile != "Susceptible" &
                             dat_hos_res$`How many days did the hospitalisation last?_resistant` != "NULL", ]

# Hospitalization day data
dat_hos_sus <- data.table(dat_hos_sus)
dat_hos_res <- data.table(dat_hos_res)
dat_hos_sus[, Profile := Susceptible_profile]
dat_hos_res[, Profile := Resistance_profile]
dat_hos_sus[, Category := "hos_sus"]
dat_hos_res[, Category := "hos_res"]
dat_hos_sus <- dat_hos_sus %>% rename("days" = "How many days did the hospitalisation last?_susceptible")
dat_hos_res <- dat_hos_res %>% rename("days" = "How many days did the hospitalisation last?_resistant")

# Duration of symptom -- susceptible
dat_sym_sus <- cbind(dat_study, dat[, 31:39])
dat_sym_sus <- dat_sym_sus[(!is.na(dat_sym_sus$Susceptible_profile) &
                              (dat_sym_sus$`How many days did the symptoms last?_susceptible` != "NULL")), ]

# Duration of symptom -- resistance
dat_sym_res <- cbind(dat_study, dat[, 40:48])
dat_sym_res <- dat_sym_res[(!is.na(dat_sym_res$Resistance_profile) &
                              (dat_sym_res$`How many days did the symptoms last?_resistant` != "NULL")), ]

# Duration of symptom data
dat_sym_sus <- data.table(dat_sym_sus)
dat_sym_res <- data.table(dat_sym_res)
dat_sym_sus[, Profile := Susceptible_profile]
dat_sym_res[, Profile := Resistance_profile]
dat_sym_sus[, Category := "sym_sus"]
dat_sym_res[, Category := "sym_res"]
dat_sym_sus <- dat_sym_sus %>% rename("days" = "How many days did the symptoms last?_susceptible")
dat_sym_res <- dat_sym_res %>% rename("days" = "How many days did the symptoms last?_resistant")
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# Cost -- susceptible
dat_cost_sus <- dat[, c(67:68, 78:86, 96:97)]
dat_cost_sus <- cbind(dat_study, dat_cost_sus)
dat_cost_sus <- dat_cost_sus[!is.na(dat_cost_sus$`What cost was measured?`),]
dat_cost_sus <- dat_cost_sus[!is.na(dat_cost_sus$Susceptible_profile),]
dat_cost_sus <- dat_cost_sus[, note...82 := `What cost was measured?`]
dat_cost_sus <- dat_cost_sus[, -c(6:7)]

# Cost -- resistance
dat_cost_res <- dat[, c(67:68, 69:77, 96:97)]
dat_cost_res <- cbind(dat_study, dat_cost_res)
dat_cost_res <- dat_cost_res[!is.na(dat_cost_res$`What cost was measured?`),]
dat_cost_res <- dat_cost_res[!is.na(dat_cost_res$Resistance_profile),]
dat_cost_res <- dat_cost_res[, note...73 := `What cost was measured?`]
dat_cost_res <- dat_cost_res[,-c(6:7)]

# Cost data
dat_cost_sus <- data.table(dat_cost_sus)
dat_cost_res <- data.table(dat_cost_res)
dat_cost_sus[, Profile := Susceptible_profile]
dat_cost_res[, Profile := Resistance_profile]
dat_cost_sus[, Category := "cost_sus"]
dat_cost_res[, Category := "cost_res"]
dat_cost_sus <- dat_cost_sus %>% rename("days" = "What was the per-patient cost of treating a patient infected with a drug-susceptible pathogen or with a pathogen where resistance was not measured?")
dat_cost_res <- dat_cost_res %>% rename("days" = "What was the per-patient cost of treating a patient infected with a drug resistant pathogen?")
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# create function to generate table and forest plot
generate_table_plot <- function(dat, subgroup_input, width_input, height_input, xlim_low, xlim_high,
                                mode, figure_name){
  dat <- data.table(dat)
  colnames(dat)[6:14] <- c("day", "statistical_measure", "comment", "uncertainty/range",
                           "note", "higher", "lower", "other", "n")
  dat[dat == "NULL"] <- NA
  dat$day    <- as.numeric(dat$day)
  dat$higher <- as.numeric(dat$higher)
  dat$lower  <- as.numeric(dat$lower)
  dat$other  <- as.numeric(dat$other)
  dat$n      <- as.numeric(dat$n)

  # adjust variables
  dat[statistical_measure == "Mean", Mean := day]
  dat[statistical_measure == "Median", Median := day]
  dat[`uncertainty/range` == "Interquartile range", higher_i := higher]
  dat[`uncertainty/range` == "Interquartile range", lower_i := lower]
  dat[`uncertainty/range` == "Range", higher_r := higher]
  dat[`uncertainty/range` == "Range", lower_r := lower]
  dat[`uncertainty/range` == "Standard deviation", SD := other]
  dat_meta <- dat[!(is.na(higher) & statistical_measure == "Median"), ]
  dat_meta <- dat_meta %>% arrange(Pathogen, Profile)
  
  # meta & forestplot
  ma.data <- metamean(n = n,
                      mean = Mean,
                      data = dat_meta,
                      sd = SD,
                      median = Median,
                      q1 = lower_i,
                      q3 = higher_i,
                      min = lower_r,
                      max = higher_r,
                      subgroup = Pathogen,
                      random = TRUE,
                      fixed = FALSE,
                      overall = FALSE)
  
  # Create data table with metamean result
  if(mode == "cost"){
    dt <- data.table(Category       = ma.data$data$Category,
                     Pathogen       = ma.data$data$Pathogen,
                     Classification = ma.data$data$Profile,
                     Country        = ma.data$data$Country,
                     Sample_size    = ma.data$n,
                     Mean_value     = ma.data$mean,
                     Lower          = ma.data$lower,
                     Upper          = ma.data$upper,
                     Reference      = ma.data$data$Study,
                     Value_comment  = ma.data$data$comment,
                     Note           = ma.data$data$note,
                     Cost_currency  = ma.data$data$`In what currency was the cost calculated?`,
                     Cost_year      = ma.data$data$`For what year was the cost calculated?`)
  } else{
    dt <- data.table(Category       = ma.data$data$Category,
                     Pathogen       = ma.data$data$Pathogen,
                     Classification = ma.data$data$Profile,
                     Country        = ma.data$data$Country,
                     Sample_size    = ma.data$n,
                     Mean_value     = ma.data$mean,
                     Lower          = ma.data$lower,
                     Upper          = ma.data$upper,
                     Reference      = ma.data$data$Study,
                     Value_comment  = ma.data$data$comment,
                     Note           = ma.data$data$note)
  }
 
  dt <- dt[order(dt$Pathogen, dt$Classification),]
  
  # save the forest plot
  png(file = paste(file.path("figures", figure_name)), width = width_input, height = height_input, res = 300)
  if(subgroup_input == "TRUE"){
    forest(ma.data,
           leftcols    = c("Study", "Pathogen", "Profile", "Country"),
           rightlabs   = c("Mean", "95% CI"),
           subgroup    = TRUE,
           xlim        = c(xlim_low, xlim_high))
  } else{
    forest(ma.data,
           leftcols = c("Study", "Pathogen", "Profile", "Country"),
           rightlabs = c("Mean", "95% CI"),
           subgroup = FALSE,
           xlim = c(xlim_low, xlim_high),
           print.byvar = TRUE)
  }
  dev.off()
  return(dt)}
# -----------------------------------------------------------------------------
# generate forest plot
# length of symptom
plot_sym_sus_enteric <- generate_table_plot(dat            = dat_sym_sus,
                                            subgroup_input = "TRUE",
                                            width_input    = 3200,
                                            height_input   = 1300,
                                            xlim_low       = 1,
                                            xlim_high      = 30,
                                            mode           = "length",
                                            figure_name    = "duration_of_symptom_sus_enteric.png")

plot_sym_res_enteric <- generate_table_plot(dat            = dat_sym_res,
                                            subgroup_input = "TRUE",
                                            width_input    = 3200,
                                            height_input   = 1500,
                                            xlim_low       = 1,
                                            xlim_high      = 30,
                                            mode           = "length",
                                            figure_name    = "duration_of_symptom_res_enteric.png")

# length of hospital stay
plot_los_sus_enteric <- generate_table_plot(dat            = dat_hos_sus,
                                            subgroup_input = "TRUE",
                                            width_input    = 3200,
                                            height_input   = 1900,
                                            xlim_low       = 1,
                                            xlim_high      = 15,
                                            mode           = "length",
                                            figure_name    = "length_of_hospital_stay_sus_enteric.png")

plot_los_res_enteric <- generate_table_plot(dat            = dat_hos_res,
                                            subgroup_input = "TRUE",
                                            width_input    = 3200,
                                            height_input   = 1600,
                                            xlim_low       = 1,
                                            xlim_high      = 15,
                                            mode           = "length",
                                            figure_name    = "length_of_hospital_stay_res_enteric.png")

# cost
plot_cost_sus_enteric <- generate_table_plot(dat           = dat_cost_sus,
                                             subgroup_input = "TRUE",
                                             width_input    = 3200,
                                             height_input   = 1300,
                                             xlim_low       = 1,
                                             xlim_high      = 5000,
                                             mode           = "cost",
                                             figure_name    = "cost_sus_enteric.png")

plot_cost_res_enteric <- generate_table_plot(dat           = dat_cost_res,
                                             subgroup_input = "TRUE",
                                             width_input    = 3200,
                                             height_input   = 1300,
                                             xlim_low       = 1,
                                             xlim_high      = 5000,
                                             mode           = "cost",
                                             figure_name    = "cost_res_enteric.png")
# -----------------------------------------------------------------------------
# generate the table
# length of symptom & hospital stay
table_length  <- do.call("rbind", list(plot_sym_sus_enteric, plot_sym_res_enteric,
                                       plot_los_sus_enteric, plot_los_res_enteric))
fwrite (x    = table_length,
        file = file.path("tables", "enteric_legnth.csv"))

# cost
table_cost <- rbind(plot_cost_sus_enteric, plot_cost_res_enteric)
fwrite (x    = table_cost,
        file = file.path("tables", "enteric_cost.csv"))
# -----------------------------------------------------------------------------
# end.