---
# title: "Hospital-related costs and length of stay of antibiotic-resistant 
# Salmonella infections: a systematic review and meta-analysis"
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

# move to base directory (run code from source directory)
setwd("~/Documents/GitHub/amr_cost/code")
source_wd <- getwd ()
setwd ("../")

# source a code file ("inflation_enteric.R")
source("code/inflation.R")

# -----------------------------------------------------------------------------

### Importing data from Google Sheets
dat <- data.table(read_sheet('https://docs.google.com/spreadsheets/d/16I5yak3-j3rEqLLcdFCcqzeQSyYjSGihXIbmZxqEILs/edit#gid=2036014622'))

# add iso3c
dat$iso3c <- countrycode(dat$Country, origin="country.name", destination="iso3c")

### add quantile level of GDP per capita
# GDP per capita (current US$)
# https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
# gdp_per_capita_data <- wb_data(indicator = "NY.GDP.PCAP.CD")
# gdp_per_capita_data <- as.data.table(gdp_per_capita_data)
# save(gdp_per_capita_data, file="data/gdp_per_capita_data.RData")
load("data/gdp_per_capita_data.RData")

# filter rows where the "date" column is "2019"
gdp_per_capita_2019 <- gdp_per_capita_data[date == "2019", c("iso3c", "NY.GDP.PCAP.CD")]

# generate quantile levels for GDP per capita
quantiles <- quantile(gdp_per_capita_2019$NY.GDP.PCAP.CD, probs = seq(0, 1, by = 0.25), na.rm = TRUE)

# create a new column for quantile levels
gdp_per_capita_2019[, Quantile := cut(NY.GDP.PCAP.CD, quantiles, labels = FALSE, include.lowest = TRUE)]

# add quantile level of GDP per capita to data
dat <- left_join(dat, gdp_per_capita_2019, by = c("iso3c"))

# add quantile level for Taiwan ($25,903, 2019)
dat <- dat[iso3c == "TWN", Quantile := 4]

### pathogen
names(dat)
unique(dat$Pathogen)

### Study data
dat_study <- dat[, c("Study", "Pathogen", "Susceptible_profile", "Resistance_profile", 
                     "Country","Cohort", "iso3c", "Quantile")]
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
                                                   "Not reported",
                                                   "Non-XDR"))

# Hospitalization day data -- susceptible
dat_hos_sus <- cbind(dat_study, dat[, 50:58])

dat_hos_sus <- dat_hos_sus[(!is.na(dat_hos_sus$Susceptible_profile) &
                                   (dat_hos_sus$`How many days did the hospitalisation last?_susceptible` != "NULL")), ]

# Hospitalization day data -- resistance
dat_hos_res <- cbind(dat_study, dat[, 59:67])

dat_hos_res <- dat_hos_res[dat_hos_res$Resistance_profile != "Susceptible" &
                                 dat_hos_res$`How many days did the hospitalisation last?_resistant` != "NULL", ]

# Hospitalization day data -- S. typhi & S. paratyphi / S. non-typhi 
dat_hos_sus <- data.table(dat_hos_sus)
dat_hos_res <- data.table(dat_hos_res)

dat_hos_sus[, Profile := Susceptible_profile]
dat_hos_res[, Profile := Resistance_profile]
dat_hos_sus[, Category := "hos_sus"]
dat_hos_res[, Category := "hos_res"]

dat_hos_sus <- dat_hos_sus %>% rename("values" = "How many days did the hospitalisation last?_susceptible")
dat_hos_res <- dat_hos_res %>% rename("values" = "How many days did the hospitalisation last?_resistant")

dat_hos_sus_typhi <- dat_hos_sus[Pathogen == "S. Typhi" | 
                                         Pathogen == "S. Typhi and S. Paratyphi" |
                                         Pathogen == "S. Paratyphi",]
dat_hos_res_typhi <- dat_hos_res[Pathogen == "S. Typhi" | 
                                         Pathogen == "S. Typhi and S. Paratyphi" |
                                         Pathogen == "S. Paratyphi",]

dat_hos_sus_nontyphi <- dat_hos_sus[Pathogen == "S. non-Typhi",]
dat_hos_res_nontyphi <- dat_hos_res[Pathogen == "S. non-Typhi",]


# Duration of fever -- susceptible
dat_fev_sus <- cbind(dat_study, dat[, 32:40])

dat_fev_sus <- dat_fev_sus[(!is.na(dat_fev_sus$Susceptible_profile) &
                                    (dat_fev_sus$`How many days did the symptoms last?_susceptible` != "NULL")), ]

# Duration of fever -- resistance
dat_fev_res <- cbind(dat_study, dat[, 41:49])

dat_fev_res <- dat_fev_res[(!is.na(dat_fev_res$Resistance_profile) &
                                  (dat_fev_res$`How many days did the symptoms last?_resistant` != "NULL")), ]


# Duration of fever data -- S. typhi & S. paratyphi / S. non-typhi 
dat_fev_sus  <- data.table(dat_fev_sus)
dat_fev_res  <- data.table(dat_fev_res)

dat_fev_sus <- dat_fev_sus[`What statistical measure was used to report the length of a symptomatic infection?_comment...34` 
                           == "fever clearance time",]
dat_fev_res <- dat_fev_res[`What statistical measure was used to report the length of a symptomatic infection?_comment...43` 
                           == "fever clearance time",]

dat_fev_sus[, Profile := Susceptible_profile]
dat_fev_res[, Profile := Resistance_profile]

dat_fev_sus[, Category := "sym_sus"]
dat_fev_res[, Category := "sym_res"]

dat_fev_sus <- dat_fev_sus %>% rename("values" = "How many days did the symptoms last?_susceptible")
dat_fev_res <- dat_fev_res %>% rename("values" = "How many days did the symptoms last?_resistant")

dat_fev_sus_typhi <- dat_fev_sus[Pathogen == "S. Typhi" | 
                                Pathogen == "S. Typhi and S. Paratyphi" |
                                Pathogen == "S. Paratyphi",]
dat_fev_res_typhi <- dat_fev_res[Pathogen == "S. Typhi" | 
                                 Pathogen == "S. Typhi and S. Paratyphi" |
                                 Pathogen == "S. Paratyphi",]

dat_fev_sus_nontyphi <- dat_fev_sus[Pathogen == "S. non-Typhi",]
dat_fev_res_nontyphi <- dat_fev_res[Pathogen == "S. non-Typhi",]
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# Cost -- susceptible
dat_cost_sus <- dat[, c(68:69, 79:87, 88:89)]

dat_cost_sus <- cbind(dat_study, dat_cost_sus)

dat_cost_sus <- dat_cost_sus[!is.na(dat_cost_sus$`What cost was measured?`),]

# dat_cost_sus <- dat_cost_sus[dat_cost_sus$`What cost was measured?` == "Medical costs"|
#                                     dat_cost_sus$`What cost was measured?` == "Hospital costs" |
#                                     dat_cost_sus$`What cost was measured?` == "Direct medical costs" |
#                                     dat_cost_sus$`What cost was measured?` == "Inpatient treatment costs",]

dat_cost_sus <- dat_cost_sus[!is.na(dat_cost_sus$Susceptible_profile),]

# Cost -- resistance
dat_cost_res <- dat[, c(68:69, 70:78, 88:89)]

dat_cost_res <- cbind(dat_study, dat_cost_res)

dat_cost_res <- dat_cost_res[!is.na(dat_cost_res$`What cost was measured?`),]

dat_cost_res <- dat_cost_res[!is.na(dat_cost_res$Resistance_profile),]

# Cost data -- S. typhi & S. paratyphi / S. non-typhi 
dat_cost_sus <- data.table(dat_cost_sus)
dat_cost_res <- data.table(dat_cost_res)

dat_cost_sus[, Profile := Susceptible_profile]
dat_cost_res[, Profile := Resistance_profile]

dat_cost_sus[, Category := "cost_sus"]
dat_cost_res[, Category := "cost_res"]

dat_cost_sus <- dat_cost_sus %>% rename("values" = "What was the per-patient cost of treating a patient infected with a drug-susceptible pathogen or with a pathogen where resistance was not measured?")
dat_cost_res <- dat_cost_res %>% rename("values" = "What was the per-patient cost of treating a patient infected with a drug resistant pathogen?")

# re-organise
dat_cost_sus <- dat_cost_sus %>% 
  select(-c(`What cost was measured?`, `What cost was measured?_comment`,
            `In what currency was the cost calculated?`,
            `For what year was the cost calculated?`), 
         `What cost was measured?`, `What cost was measured?_comment`,
         `In what currency was the cost calculated?`,
         `For what year was the cost calculated?`)
dat_cost_res <- dat_cost_res %>% 
  select(-c(`What cost was measured?`, `What cost was measured?_comment`,
            `In what currency was the cost calculated?`,
            `For what year was the cost calculated?`), 
         `What cost was measured?`, `What cost was measured?_comment`,
         `In what currency was the cost calculated?`,
         `For what year was the cost calculated?`)

dat_cost_sus_typhi <- dat_cost_sus[Pathogen == "S. Typhi" | 
                                   Pathogen == "S. Typhi and S. Paratyphi" |
                                   Pathogen == "S. Paratyphi",]
dat_cost_res_typhi <- dat_cost_res[Pathogen == "S. Typhi" | 
                                   Pathogen == "S. Typhi and S. Paratyphi" |
                                   Pathogen == "S. Paratyphi",]

dat_cost_sus_nontyphi <- dat_cost_sus[Pathogen == "S. non-Typhi",]
dat_cost_res_nontyphi <- dat_cost_res[Pathogen == "S. non-Typhi",]

# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# create function to generate forest plot

generate_table_plot <- function(dat, subgroup_input,
                                width_input, height_input,
                                xlim_low, xlim_high,
                                mode, figure_name){
  
  dat <- data.table(dat)
  dat$Quantile <- factor(dat$Quantile, levels = c(1, 2, 3, 4))
  
  colnames(dat)[9:17] <- c("value", "statistical_measure", "comment", "uncertainty",
                           "note", "higher", "lower", "other", "n")
  dat[dat == "NULL"] <- NA
  dat$value <- as.numeric(dat$value)
  dat$higher <- as.numeric(dat$higher)
  dat$lower <- as.numeric(dat$lower)
  dat$other <- as.numeric(dat$other)
  dat$n <- as.numeric(dat$n)
  
  # adjust variables
  dat[statistical_measure == "Mean", Mean := value]
  dat[statistical_measure == "Median", Median := value]
  dat[uncertainty == "Interquartile range", higher_i := higher]
  dat[uncertainty == "Interquartile range", lower_i := lower]
  dat[uncertainty == "Range", higher_r := higher]
  dat[uncertainty == "Range", lower_r := lower]
  dat[uncertainty == "Standard deviation", SD := other]
  dat[uncertainty == "Standard error", SE := other]
  
  dat_meta <- dat[!(is.na(higher) & statistical_measure == "Median"), ]
  dat_meta <- dat_meta %>% arrange(Pathogen, Profile)
  # Update the "SD" column where "uncertainty" is "95% confidence interval" and "higher" is NA
  dat_meta[(uncertainty == "95% confidence intervals" & !is.na(higher)),
           SE := (higher - lower) / (2 * qnorm(0.975))]
  dat_meta[(uncertainty == "Standard error" & !is.na(SE)),
           SD := SE * sqrt(n)]
  dat_meta$subgroup_pathogen_GPC <- paste(dat_meta$Pathogen, dat_meta$Quantile, sep = "")
  # ----------------------------------------------------------------------------

  ### adjustment in cost
  if(mode == "cost"){
    # add cost_currency
    dat_meta <- merge(dat_meta, currency_country, by="iso3c", all.x=TRUE, all.y=FALSE)
    # add WHO region
    dat_meta <- merge(dat_meta, who_whoc_wb, by="iso3c", all.x=TRUE,all.y=FALSE)
    dat_meta <- dat_meta %>%
      rename("Cost_currency" = "In what currency was the cost calculated?",
             "Cost_year" = "For what year was the cost calculated?")
    # convert cost to 2019 USD
    for (i in 1:nrow(dat_meta)){
      dat_meta[i, Mean := cost_adj_lit(2019,dat_meta[i],"Mean",inf_xch_4function)]
      dat_meta[i, SD := cost_adj_lit(2019,dat_meta[i],"SD",inf_xch_4function)]
      dat_meta[i, Median := cost_adj_lit(2019,dat_meta[i],"Median",inf_xch_4function)]
      dat_meta[i, lower_i := cost_adj_lit(2019,dat_meta[i],"lower_i",inf_xch_4function)]
      dat_meta[i, higher_i := cost_adj_lit(2019,dat_meta[i],"higher_i",inf_xch_4function)]
      dat_meta[i, lower_r := cost_adj_lit(2019,dat_meta[i],"lower_r",inf_xch_4function)]
      dat_meta[i, higher_r := cost_adj_lit(2019,dat_meta[i],"higher_r",inf_xch_4function)]
    }
  } else{
  }
  # ----------------------------------------------------------------------------
  
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
                      subgroup = Quantile,
                      random = TRUE,
                      fixed = FALSE,
                      overall = FALSE)

  # Create data table with metamean result
  if(mode == "cost"){
    dt <- data.table(Category       = ma.data$data$Category,
                     Pathogen       = ma.data$data$Pathogen,
                     Classification = ma.data$data$Profile,
                     Country        = ma.data$data$Country,
                     Quantile_level = ma.data$data$Quantile,
                     Sample_size    = ma.data$n,
                     Mean_value     = ma.data$mean,
                     Lower          = ma.data$lower,
                     Upper          = ma.data$upper,
                     Reference      = ma.data$data$Study,
                     Value_comment  = ma.data$data$comment,
                     Note           = ma.data$data$note,
                     Cohort         = ma.data$data$Cohort,
                     Cost_currency  = ma.data$data$Cost_currency,
                     Cost_year      = ma.data$data$Cost_year,
                     Cost_measure   = ma.data$data$`What cost was measured?`,
                     Cost_measure_c = ma.data$data$`What cost was measured?_comment`)
  } else{
    dt <- data.table(Category       = ma.data$data$Category,
                     Pathogen       = ma.data$data$Pathogen,
                     Classification = ma.data$data$Profile,
                     Country        = ma.data$data$Country,
                     Quantile_level = ma.data$data$Quantile,
                     Sample_size    = ma.data$n,
                     Mean_value     = ma.data$mean,
                     Lower          = ma.data$lower,
                     Upper          = ma.data$upper,
                     Reference      = ma.data$data$Study,
                     Value_comment  = ma.data$data$comment,
                     Note           = ma.data$data$note,
                     Cohort         = ma.data$data$Cohort)}
  
  dt <- dt[order(dt$Quantile_level,
                 dt$Classification,
                 dt$Country),]
  
  # save the forest plot
  png(file = paste(file.path("figures", figure_name)), width = width_input, height = height_input, res = 300)
  if(subgroup_input == "TRUE"){
    forest(ma.data,
           leftcols = c("Study", "Pathogen", "Profile", "Country"),
           rightlabs = c("Mean", "95% CI"),
           subgroup = TRUE,
           xlim = c(xlim_low, xlim_high))
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
plot_fev_sus_typhi <- generate_table_plot(dat            = dat_fev_sus_typhi,
                                          subgroup_input = "TRUE",
                                          width_input    = 3200,
                                          height_input   = 1300,
                                          xlim_low       = 1,
                                          xlim_high      = 30,
                                          mode           = "sym",
                                          figure_name    = "duration_of_fever_sus_typhi.png")

plot_fev_res_typhi  <- generate_table_plot(dat            = dat_fev_res_typhi,
                                           subgroup_input = "TRUE",
                                           width_input    = 3200,
                                           height_input   = 1500,
                                           xlim_low       = 1,
                                           xlim_high      = 30,
                                           mode           = "sym",
                                           figure_name    = "duration_of_fever_res_typhi.png")

plot_fev_sus_nontyphi <- generate_table_plot(dat            = dat_fev_sus_nontyphi,
                                             subgroup_input = "TRUE",
                                             width_input    = 3200,
                                             height_input   = 600,
                                             xlim_low       = 1,
                                             xlim_high      = 30,
                                             mode           = "sym",
                                             figure_name    = "duration_of_fever_sus_nontyphi.png")

# 0 no study available for duration of fever (Nontyphi)
# plot_fev_res_nontyphi  <- generate_table_plot(dat            = dat_fev_res_nontyphi,
#                                               subgroup_input = "TRUE",
#                                               width_input    = 3200,
#                                               height_input   = 1600,
#                                               xlim_low       = 1,
#                                               xlim_high      = 30,
#                                               mode           = "sym",
#                                               figure_name    = "duration_of_fever_res_nontyphi.png")


# length of hospital stay
plot_los_sus_typhi <- generate_table_plot(dat            = dat_hos_sus_typhi,
                                          subgroup_input = "TRUE",
                                          width_input    = 3200,
                                          height_input   = 2200,
                                          xlim_low       = 1,
                                          xlim_high      = 15,
                                          mode           = "los",
                                          figure_name    = "length_of_hospital_stay_sus_typhi.png")

plot_los_res_typhi <- generate_table_plot(dat            = dat_hos_res_typhi,
                                          subgroup_input = "TRUE",
                                          width_input    = 3200,
                                          height_input   = 1600,
                                          xlim_low       = 1,
                                          xlim_high      = 15,
                                          mode           = "los",
                                          figure_name    = "length_of_hospital_stay_res_typhi.png")

plot_los_sus_nontyphi <- generate_table_plot(dat            = dat_hos_sus_nontyphi,
                                             subgroup_input = "TRUE",
                                             width_input    = 3200,
                                             height_input   = 1600,
                                             xlim_low       = 1,
                                             xlim_high      = 15,
                                             mode           = "los",
                                             figure_name    = "length_of_hospital_stay_sus_nontyphi.png")

plot_los_res_nontyphi <- generate_table_plot(dat            = dat_hos_res_nontyphi,
                                             subgroup_input = "TRUE",
                                             width_input    = 3200,
                                             height_input   = 1400,
                                             xlim_low       = 1,
                                             xlim_high      = 15,
                                             mode           = "los",
                                             figure_name    = "length_of_hospital_stay_res_nontyphi.png")

# cost
plot_cost_sus_typhi <- generate_table_plot(dat            = dat_cost_sus_typhi,
                                           subgroup_input = "TRUE",
                                           width_input    = 3500,
                                           height_input   = 1800,
                                           xlim_low       = 1,
                                           xlim_high      = 50000,
                                           mode           = "cost",
                                           figure_name    = "cost_sus_typhi.png")

plot_cost_res_typhi <- generate_table_plot(dat            = dat_cost_res_typhi,
                                           subgroup_input = "TRUE",
                                           width_input    = 3200,
                                           height_input   = 1200,
                                           xlim_low       = 1,
                                           xlim_high      = 5000,
                                           mode           = "cost",
                                           figure_name    = "cost_res_typhi")

plot_cost_sus_nontyphi <- generate_table_plot(dat            = dat_cost_sus_nontyphi,
                                              subgroup_input = "TRUE",
                                              width_input    = 3500,
                                              height_input   = 1200,
                                              xlim_low       = 1,
                                              xlim_high      = 50000,
                                              mode           = "cost",
                                              figure_name    = "cost_sus_nontyphi.png")

plot_cost_res_nontyphi <- generate_table_plot(dat            = dat_cost_res_nontyphi,
                                              subgroup_input = "TRUE",
                                              width_input    = 3200,
                                              height_input   = 1200,
                                              xlim_low       = 1,
                                              xlim_high      = 5000,
                                              mode           = "cost",
                                              figure_name    = "cost_res_nontyphi.png")
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------

### save the table
## length of symptom & hospital stay
table_length <- do.call("rbind",
                        list(plot_fev_sus_typhi, plot_fev_sus_nontyphi,
                             plot_fev_res_typhi,
                             plot_los_sus_typhi, plot_los_sus_nontyphi, 
                             plot_los_res_typhi, plot_los_res_nontyphi))

fwrite (x = table_length,
        file = file.path("tables", "typhoid_legnth.csv"))

## cost
cost_table <- rbind(plot_cost_sus_typhi, plot_cost_sus_nontyphi,
                    plot_cost_res_typhi, plot_cost_res_nontyphi)

# remove NA values
cost_table <- cost_table[!is.na(Mean_value)]
table_cost <- cost_table[, c("Category", "Pathogen", "Classification",
                             "Country", "Quantile_level", "Sample_size", "Mean_value",
                             "Lower", "Upper", "Reference", "Value_comment",
                             "Note", "Cohort", "Cost_measure", "Cost_measure_c")]

fwrite (x = table_cost,
        file = file.path("tables", "typhoid_cost.csv"))
# -----------------------------------------------------------------------------
# end.

