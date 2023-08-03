# ----------------------------------------------------------------------------
# inflation_enteric.R
#
# applying inflation function
# Purchasing Power Parity exchange rates
# Local currency unit exchange rates
# GDP deflation indices
#
# inflation_enteric.R code is adapted from the code developed by Nichola R. Naylor
# Reference: Nichola R. Naylor, The Antimicrobial Resistance Unit Cost Repository (AMR-UCR). 
# GitHub (https://github.com/NikkiR08/AMR-UCR/tree/main) [Last Access Date: July 25, 2023] 
# -----------------------------------------------------------------------------
# load libraries
library(wbstats)
library(data.table)
library(dplyr)
library(countrycode)
library(Rilostat)
library(wbstats)

### set negate %in% function
`%!in%` <-  Negate(`%in%`)

# ###### exchange rates #######
# ppp_exchange <- wb_data(indicator=c("PA.NUS.PPP"))
# ppp_exchange <- as.data.table(ppp_exchange)
# save(ppp_exchange, file="data/ppp_exchange.RData")
load("data/ppp_exchange.RData")
### local currency units to USD
# wb_exchange <- wb_data(indicator=c("PA.NUS.FCRF"))
# wb_exchange <- as.data.table(wb_exchange)
# save(wb_exchange, file="data/wb_exchange.RData")
load("data/wb_exchange.RData")


# ### inflation data creation #######
# gdpdeflator_data <- wb_data(indicator = "NY.GDP.DEFL.ZS")
# inflation_source <- as.data.table(gdpdeflator_data)
# save(inflation_source, file="data/inflation_source.RData")
load("data/inflation_source.RData")


# ## match countries with currencies
# currency_country <- read.csv("data/currency_country_adj.csv")
# currency_country <- as.data.table(currency_country)
# currency_country$iso3c <- countrycode(currency_country$country, origin="country.name", destination="iso3c")
# ## remove NA values
# currency_country <- currency_country[!is.na(currency_code) & !is.na(number)]
# # save(currency_country, file="data/currency_country.RData")
load("data/currency_country.RData")

# region data
load("data/who_whoc_wb.RData")


##### Defining Eurozone exchange rates
## EMU not in wbstats download
### downloaded csv of "PA.NUS.FCRF" https://data.worldbank.org/indicator/PA.NUS.FCRF
# 12/03/2021 ## LCU per 1$
euro_wb_exc <- read.csv("data/PA_NUS_FCRF_20210312.csv") %>%
  as.data.table()

# 2023/06/29
# euro_wb_exc <- read_excel("data/PA_NUS_FCRF_20230725.xls") %>% as.data.table()

euro_wb_exc <- euro_wb_exc[Country.Code=="EMU"]
var.n <- ncol(euro_wb_exc) 
euro_wb_exc <-  melt(euro_wb_exc, id.vars = 1:4,
                     measure.vars = 5:var.n, value.name="eu_val") %>% 
  as.data.table()
euro_wb_exc[ , date := as.numeric(substring(euro_wb_exc$variable, 2))]

##### Defining Eurozone inflation % linked series
### downloaded csv "https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG?locations=XC
# 25/03/21 ## annual % change
euro_wb_gdp <- read.csv("data/NY_GDP_DEFL_KD_ZG_EUR-20210325.csv") %>%
  as.data.table()
var.n <- ncol(euro_wb_gdp)
euro_wb_gdp <-  melt(euro_wb_gdp, id.vars = 1:4,
                     measure.vars = 5:var.n, value.name="eu_val") %>% 
  as.data.table()
euro_wb_gdp[ , date := as.numeric(substring(euro_wb_gdp$variable, 2))]

# merge data
inf_xch_data <- merge(inflation_source,ppp_exchange,by=c("iso3c","date"),all=TRUE)
inf_xch_data <- merge(inf_xch_data,wb_exchange,by=c("iso3c","date"), all=TRUE)
inf_xch_data <- merge(inf_xch_data, currency_country, by="iso3c", allow.cartesian = TRUE)
inf_xch_data <- inf_xch_data %>%
  select(iso3c, date, NY.GDP.DEFL.ZS, PA.NUS.PPP, PA.NUS.FCRF, currency_code) %>%
  as.data.table()

inf_eur <- inf_xch_data[currency_code=="EUR"]
inf_Neur <- inf_xch_data[currency_code!="EUR"]

### need to replace XC rate for EUR
inf_eur <- merge(inf_eur, euro_wb_exc, by="date")
inf_eur$PA.NUS.FCRF <- inf_eur$eu_val
inf_eur <- inf_eur %>%
  select(iso3c, date, NY.GDP.DEFL.ZS, PA.NUS.PPP, PA.NUS.FCRF, currency_code) %>%
  as.data.table() # official exchange rate (LCU per US$, period average)

### recombine
inf_xch_4function <- rbind(inf_eur, inf_Neur)

inf_xch_4function <- inf_xch_4function %>%
  filter(!is.na(NY.GDP.DEFL.ZS) & 
           !is.na(PA.NUS.FCRF)) %>% ## remove if no deflation data and no exchange rate data
  as.data.table()
rm(inf_xch_data) ## remove old data table to avoid confusion

#### load in EU PPP conversions
# downloaded from OECD.stat on 12/03/21 last
eur_ppp <- read.csv("data/EUR_PPP.csv") %>%
  as.data.table()

#### FUNCTION FOR THE LITERATURE COSTS
cost_adj_lit <- function(to_year,
                         cost_dt_row,
                         column_ref_cost,
                         inf_xch_dt) {
  
  ## to_year is a numeric
  ## cost_dt_row a row of the literature extract
  ## column_ref_cost the column with the "to_cost" to be adjusted
  ## inf_xch_dt is the exchange rate and inflation dataset created above
  ## needs currency_country dataset preloaded to run (see above in this script)
  
  #### !!! note if you have other currencies with other regions not listed in this function
  ## (e.g. CAD for a non-Canadian country)
  # would need to add more code in here to account for this
  
  #### first pulling out the data we need for conversions
  iso <- as.character(cost_dt_row$iso3c) ## country of study 
  from_cost <- cost_dt_row[[column_ref_cost]] ## cost to be adapted
  from_year <- as.numeric(cost_dt_row$Cost_year) ## base year
  ## see if dates in set
  from_year_data <- inf_xch_dt[date == from_year] ## from year financial data 
  to_year_data <- inf_xch_dt[date == to_year] ## to year financial data 
  temp_iso_dt <- inf_xch_dt[iso3c==iso] ## to be used in calculations below
  
  ### !!! if get error on future iterations try to add more variations on which to use
  # if both base and Cost_currency and currency_code unavailable in inflation/exchange rates
  # added complexity that was not needed in this run/project so not done in this iteration
  
  if(cost_dt_row$iso3c=="EUSA"&cost_dt_row$
     Cost_currency=="EUR"& 
     cost_dt_row$who.region=="EURO"){
    #### IF iso3 is NA & COST_CURRENCY = EUR AND REGION = EURO
    ## inflate based on average GDP deflator growth
    gdp_i_av <- euro_wb_gdp[date<=to_year & date >= from_year]
    gdp_i_av <- mean(gdp_i_av$eu_val) ## average Inflation, GDP deflator (annual %) over the period
    ## use compound interest formulae to adjust for annual inflation rate equal to the average
    to_index_est <- from_cost*(1+(gdp_i_av/100))^(to_year-from_year)
    ## convert to USD from EUR
    euro_exchange <- inf_xch_dt[currency_code=="EUR"]
    euro_exchange <- euro_exchange %>%
      group_by(date, currency_code) %>%
      filter(row_number() == 1)%>% ## take just 1 per study + who.region/region combination
      as.data.table()  # get so just one per year
    eur_usd_xc <- euro_exchange[date==to_year,"PA.NUS.FCRF"]
    to_cost <- as.numeric(to_index_est*eur_usd_xc)
    
  } else if (cost_dt_row$iso3c=="EUSA"&
             cost_dt_row$Cost_currency=="USD"& 
             cost_dt_row$who.region=="EURO"){
    #### IF iso3 is NA & COST_CURRENCY = US AND REGION = EURO
    ## get euro exchange data
    ### could just use euro csv directly, but that adds another input into the function
    euro_exchange <- inf_xch_dt[currency_code=="EUR"]
    ## get so just one per year
    euro_exchange <- euro_exchange %>%
      group_by(date, currency_code) %>%
      filter(row_number() == 1)%>% ## take just 1 per study + who.region/region combination
      as.data.table()
    
    from_year_index <- euro_exchange[date == from_year]
    ## convert to EUR
    from_index_est <- from_cost*from_year_index$PA.NUS.FCRF
    ## inflate based on average GDP deflator growth
    gdp_i_av <- euro_wb_gdp[date<=to_year & date >= from_year]
    gdp_i_av <- mean(gdp_i_av$eu_val) ## average GDP inflation for the euro zone over the period
    to_index_est <- from_index_est*(1+(gdp_i_av/100))^(to_year-from_year)
    ## convert to USD from EUR
    eur_usd_xc <- euro_exchange[date==to_year,"PA.NUS.FCRF"]
    to_cost <- as.numeric(to_index_est/eur_usd_xc)

    } else if (cost_dt_row$iso3c!="EUSA"&
               cost_dt_row$Cost_currency!=cost_dt_row$
               currency_code& cost_dt_row$Cost_currency=="EUR"){
    ### IF iso3 != na and Cost_currency!=currency_code & (currency = EUR)
    ## convert to USD then to local currency 
    euro_exchange <- inf_xch_dt[currency_code=="EUR"]
    ## get so just one per year
    euro_exchange <- euro_exchange %>%
      group_by(date, currency_code) %>%
      filter(row_number() == 1)%>% ## take just 1 per study + who.region/region combination
      as.data.table()
    
    ## convert to local currency
    eur_usd_xc <- euro_exchange[date==from_year,"PA.NUS.FCRF"]
    usd_lcu_xc <- inf_xch_dt[date==from_year &
                               iso3c==iso,"PA.NUS.FCRF"]
    xc_cost <- as.numeric(from_cost*(1/eur_usd_xc)*usd_lcu_xc)

    ## inflate based on GDP deflator
    from_index <- temp_iso_dt[temp_iso_dt$date == from_year, "NY.GDP.DEFL.ZS"] 
    to_index <-   temp_iso_dt[temp_iso_dt$date == to_year, "NY.GDP.DEFL.ZS"]
    to_cost <- xc_cost * (to_index/from_index)
    
    ## then convert back to USD 2019
    usd_xc <- temp_iso_dt[date==to_year,"PA.NUS.FCRF"]
    to_cost <- as.numeric(to_cost/usd_xc)

   } else if (cost_dt_row$iso3c!="EUSA"& 
              cost_dt_row$Cost_currency!=cost_dt_row$currency_code& 
              cost_dt_row$Cost_currency=="USD"){
    ### IF iso3 != na and Cost_currency!=currency_code & (currency = US)
    ## convert to local currency 
    usd_lcu_xc <- inf_xch_dt[date==from_year &
                               iso3c==iso,"PA.NUS.FCRF"]
    xc_cost <- as.numeric(from_cost*usd_lcu_xc)
    
    ## inflate based on GDP deflator
    from_index <- temp_iso_dt[temp_iso_dt$date == from_year, "NY.GDP.DEFL.ZS"] 
    to_index <-   temp_iso_dt[temp_iso_dt$date == to_year, "NY.GDP.DEFL.ZS"]
    to_cost <- xc_cost * (to_index/from_index)
    
    ## then convert back to USD 2019
    usd_xc <- temp_iso_dt[date==to_year,"PA.NUS.FCRF"]
    to_cost <- as.numeric(to_cost/usd_xc)

   } else if (cost_dt_row$iso3c!="EUSA"& 
              cost_dt_row$Cost_currency!=cost_dt_row$currency_code& 
              (cost_dt_row$Cost_currency!="USD" & cost_dt_row$Cost_currency!="EUR")) {
     ## convert to USD then to local currency 
     currency_exchange <- inf_xch_dt[currency_code==cost_dt_row$Cost_currency]
     ## get so just one per year
     currency_exchange <- currency_exchange %>%
       group_by(date, currency_code) %>%
       filter(row_number() == 1) %>% ## take just 1 per study + who.region/region combination
       as.data.table()
     
     ## convert to local currency
     currency_usd_xc <- currency_exchange[date==from_year,"PA.NUS.FCRF"]
     usd_lcu_xc <- inf_xch_dt[date==from_year & iso3c==iso,"PA.NUS.FCRF"]
     xc_cost <- as.numeric(from_cost*(1/currency_usd_xc)*usd_lcu_xc)
     
     ## inflate based on GDP deflator
     from_index <- temp_iso_dt[temp_iso_dt$date == from_year, "NY.GDP.DEFL.ZS"] 
     to_index <-   temp_iso_dt[temp_iso_dt$date == to_year, "NY.GDP.DEFL.ZS"]
     to_cost <- xc_cost * (to_index/from_index)
     
     ## then convert back to USD 2019
     usd_xc  <-   temp_iso_dt[date==to_year,"PA.NUS.FCRF"]
     to_cost <- as.numeric(to_cost/usd_xc)
     
   } else if (cost_dt_row$iso3c!="EUSA"&
             cost_dt_row$Cost_currency==cost_dt_row$currency_code) {
    ### IF  iso3 != na and Cost_currency==currency_code
    
    ## inflate based on GDP deflator
    from_index <- temp_iso_dt[temp_iso_dt$date == from_year, "NY.GDP.DEFL.ZS"] 
    to_index <-   temp_iso_dt[temp_iso_dt$date == to_year, "NY.GDP.DEFL.ZS"]
    to_cost <- from_cost * (to_index/from_index)
  
    ## then convert to USD 2019
    usd_xc  <- temp_iso_dt[date==to_year,"PA.NUS.FCRF"]
    to_cost <- as.numeric(to_cost/usd_xc)

  } else {
    is.na(to_cost)
  }
  return(to_cost)
}
# -----------------------------------------------------------------------------
# end.

