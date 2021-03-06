# Information -------------------------------------------------------------

# How does Scotland compare with the rest of the UK, and the rest of Western Europe?

# Paper suggested by Gerry MacCartney 
# 29/5/2015

# Do this in a single script file

# UK
# - Northern Ireland
# - England & Wales
# - Scotland

# Western Europe:
# - Northern Ireland
# - England & Wales
# - Scotland 
# - Ireland
# - France
# - Belgium
# - Luxembourg
# - Netherlands
# - Germany
# - Switzerland
# - Austria

# Prereqs -----------------------------------------------------------------

rm(list=ls())

# data management
require(readr)

require(plyr)
require(tidyr)
require(stringr)
require(dplyr)


#graphics
require(lattice)
require(latticeExtra)
require(ggplot2) 
require(RColorBrewer)
require(grid)

# for smoothing
require(fields) 
require(spatstat)

#tables
require(xtable)
require(xlsx)



# functions ---------------------------------------------------------------
source("scripts/smoother_function.R")
source("scripts/scotland_in_context__helper_functions.R")


# base data  --------------------------------------------------------------


dta <- read_csv("data/tidy/counts_germany_combined.csv")

# country group definitions
source("scripts/scotland_in_context__country_group_definitions.R")


# country_group_selections ------------------------------------------------

dta_uk <- dta %>% filter(country %in% uk_codes)
dta_we <- dta %>% filter(country %in% w_europe_codes)
dta_europe <- dta %>% filter(country %in% europe_codes)
dta_all <- dta %>% filter(country %in% all_codes)
dta_ne <- dta %>% filter(country %in% europe_northern)
dta_se <- dta %>% filter(country %in% europe_southern)
dta_ee <- dta %>% filter(country %in% europe_eastern)
dta_na <- dta %>% filter(country %in% north_america)
dta_anglo <- dta %>% filter(country %in% anglophone)

dta_uk_noscot <- dta_uk %>% filter(country !="GBR_SCO")
dta_we_noscot <- dta_we %>% filter(country !="GBR_SCO")
dta_europe_noscot <- dta_europe %>% filter(country !="GBR_SCO")
dta_all_noscot <- dta_all %>% filter(country !="GBR_SCO")
dta_anglo_noscot <- dta_anglo %>% filter(country !="GBR_SCO")


# country group selections - smoothed -------------------------------------

fn <- function(DTA) {
  out <- DTA %>%   
    mutate(
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base=10)     
    ) %>% smooth_var(
      dta =.,
      group_vars= c("country", "sex"),
      smooth_var = "lg_cmr",
      smooth_par = 1.3
    ) 
  return(out)
}


dta_uk_smoothed <- fn(dta_uk)
dta_we_smoothed <- fn(dta_we)
dta_europe_smoothed <- fn(dta_europe)
dta_all_smoothed <- fn(dta_all)
dta_ne_smoothed <- fn(dta_ne)
dta_se_smoothed <- fn(dta_se)
dta_ee_smoothed <- fn(dta_ee)
dta_na_smoothed <- fn(dta_na)
dta_anglo_smoothed <- fn(dta_anglo)


dta_uk_noscot_smoothed <- fn(dta_uk_noscot) 
dta_we_noscot_smoothed <- fn(dta_we_noscot) 
dta_europe_noscot_smoothed <- fn(dta_europe_noscot)
dta_all_noscot_smoothed <- fn(dta_all_noscot)
dta_anglo_noscot_smoothed <- fn(dta_anglo_noscot)



rm(fn)
# country group selections - combined  ---------------------------------------


dta_uk_overall <- grouper(dta_uk)
dta_we_overall <- grouper(dta_we)
dta_europe_overall <- grouper(dta_europe)
dta_all_overall <- grouper(dta_all)
dta_ne_overall <- grouper(dta_ne)
dta_se_overall <- grouper(dta_se)
dta_ee_overall <- grouper(dta_ee)
dta_na_overall <- grouper(dta_na)
dta_anglo_overall <- grouper(dta_anglo)


dta_uk_noscot_overall <- grouper(dta_uk_noscot)
dta_we_noscot_overall <- grouper(dta_we_noscot)
dta_europe_noscot_overall <- grouper(dta_europe_noscot)
dta_all_noscot_overall <- grouper(dta_all_noscot)
dta_anglo_noscot_overall <- grouper(dta_anglo_noscot)




# Data allowing comparison of broad European regions against European average

tmp1 <- dta_we_overall %>% mutate(country="Western Europe")
tmp2 <- dta_ee_overall %>% mutate(country="Eastern Europe")
tmp3 <- dta_ne_overall %>% mutate(country="Northern Europe")
tmp4 <- dta_se_overall %>% mutate(country="Southern Europe")
tmp5 <- dta_europe_overall %>% mutate(country = "Europe Overall")

dta_euro_regions <- tmp1 %>% 
  bind_rows(tmp2) %>% 
  bind_rows(tmp3) %>% 
  bind_rows(tmp4) %>% 
  bind_rows(tmp5)
  
tmp1 <- dta_we_noscot_overall %>% mutate(country="Western Europe")

dta_euro_regions_noscot <- tmp1 %>% 
  bind_rows(tmp2) %>% 
  bind_rows(tmp3) %>% 
  bind_rows(tmp4) %>% 
  bind_rows(tmp5)


dta_euro_regions_smoothed <- 
  dta_euro_regions %>% 
  smooth_var(dta=., group_vars = c("sex", "country"), smooth_var="lg_cmr", smooth_par=1.3)

dta_euro_regions_noscot_smoothed <-
  dta_euro_regions_noscot %>% 
  smooth_var(dta=., group_vars = c("sex", "country"), smooth_var="lg_cmr", smooth_par=1.3)



rm(tmp1, tmp2, tmp3, tmp4, tmp5)

fn <- function(DTA) {
  out <- DTA %>%   
    mutate(
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base=10)     
    ) %>% smooth_var(
      dta =.,
      group_vars= c("sex"),
      smooth_var = "lg_cmr",
      smooth_par = 1.3
    ) 
  return(out)
}

dta_uk_overall_smoothed <- fn(dta_uk_overall)
dta_we_overall_smoothed <- fn(dta_we_overall)
dta_europe_overall_smoothed <- fn(dta_europe_overall)
dta_all_overall_smoothed <- fn(dta_all_overall)

dta_ne_overall_smoothed <- fn(dta_ne_overall)
dta_se_overall_smoothed <- fn(dta_se_overall)
dta_ee_overall_smoothed <- fn(dta_ee_overall)
dta_na_overall_smoothed <- fn(dta_na_overall)
dta_anglo_overall_smoothed <- fn(dta_anglo_overall)


dta_uk_noscot_overall_smoothed <- fn(dta_uk_noscot_overall)
dta_we_noscot_overall_smoothed <- fn(dta_we_noscot_overall)
dta_europe_noscot_overall_smoothed <- fn(dta_europe_noscot_overall)
dta_all_noscot_overall_smoothed <- fn(dta_all_noscot_overall)
dta_anglo_noscot_overall_smoothed <- fn(dta_anglo_noscot_overall)


rm(fn)


# country group selections - combined and smoothed -------------------------------




# SCP of rest of UK Overall -------------------------------------------------------

png(filename="figures/scotland_in_context/rest_uk_overall_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp_overall(dta_uk_noscot_overall, dta_uk_noscot_overall_smoothed,
                       COL.REGIONS=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200))
                       ))
dev.off()

# SCP of Western Europe Overall

png(filename="figures/scotland_in_context/rest_we_overall_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp_overall(dta_we_noscot_overall, dta_we_noscot_overall_smoothed,
                       COL.REGIONS=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200))
                       ))
dev.off()

# SCP of Europe Overall
png(filename="figures/scotland_in_context/rest_europe_overall_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp_overall(dta_europe_noscot_overall, dta_europe_noscot_overall_smoothed,
                       COL.REGIONS=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200))
                       ))
dev.off()



# SCP of North  America
png(filename="figures/scotland_in_context/rest_anglophone_overall_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp_overall(dta_anglo_noscot_overall, dta_anglo_noscot_overall_smoothed, YEAR_RANGE=c(1900, 2010),
                       COL.REGIONS=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200))
))
dev.off()





# SCP of all data Overall

png(filename="figures/scotland_in_context/rest_allcountries_overall_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp_overall(dta_all_noscot_overall, dta_all_noscot_overall_smoothed,
                       COL.REGIONS=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200))
                       ))
dev.off()




# Latticeplots ------------------------------------------------------------



# Region level SCP latticeplot  -------------------------------------------

tmp1 <- c(
  `Western Europe`="Western Europe",
  `Eastern Europe` = "Eastern Europe",
  `Northern Europe` = "Northern Europe",
  `Southern Europe` = "Southern Europe"
)

tmp2 <- dta_euro_regions_noscot %>% filter(country !="Europe Overall")
tmp3 <- dta_euro_regions_noscot_smoothed %>% filter(country !="Europe Overall")

png(filename="figures/scotland_in_context/rest_euro_regions_scp_lattice_spectral.png", 
    width=70, height=40, res=300, units="cm"
)

make_scp_lattice(tmp2, tmp3, tmp1, 
                 COL.REGIONS=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200))
)
dev.off()


# Create CLP comparing Scotland against each of the main European regions


tmp3 <- dta %>% filter(country == "GBR_SCO") %>% select(-country) %>% 
  mutate(cmr = death_count/population_count, lg_cmr = log(cmr, base = 10))

png(filename="figures/scotland_in_context/scotland_compared_with_rest_euro_regions_clp_lattice_1900_2010.png",
    width=60, height=30, res=300, units="cm"
)
print(make_clp_lattice(
  tmp2, tmp3, tmp1, 
  COL.REGIONS= colorRampPalette(brewer.pal(6, "RdBu"))(64),
  ADD_CONTOURS = T    
))
dev.off()

png(filename="figures/scotland_in_context/scotland_compared_with_rest_euro_regions_clp_lattice_1950_2010.png",
    width=30, height=30, res=300, units="cm"
)

print(make_clp_lattice(
  tmp2, tmp3, tmp1, 
  YEAR_RANGE=c(1950, 2010),
  COL.REGIONS= colorRampPalette(brewer.pal(6, "RdBu"))(64),
  ADD_CONTOURS = T    
))
dev.off()




# CLP single  -------------------------------------------------------------


png(filename="figures/scotland_in_context/clp_scotland_against_rest_UK.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_single_clp(
  dta_uk, 
  dta_uk_noscot_overall, 
  "GBR_SCO",
  AT = seq(from= -1.2, to = 1.2, by=0.1),
  ADD_CONTOURS = T
))
dev.off()


# CLP Scotland against Western Europe -------------------------------------

png(filename="figures/scotland_in_context/clp_scotland_against_rest_western_europe.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_single_clp(
  dta_we, 
  dta_we_noscot_overall, 
  "GBR_SCO",
  AT = seq(from= -1.2, to = 1.2, by=0.1),
  ADD_CONTOURS = T    
))
dev.off()

# CLP Scotland against Europe -------------------------------------
png(filename="figures/scotland_in_context/clp_scotland_against_rest_europe.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_single_clp(
  dta_europe, 
  dta_europe_noscot_overall, 
  "GBR_SCO",
  ADD_CONTOURS = T    
))
dev.off()

# CLP Scotland against Affluent World -------------------------------------
png(filename="figures/scotland_in_context/clp_scotland_against_rest_affluent_world.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_single_clp(
  dta_all, 
  dta_all_noscot_overall, 
  "GBR_SCO",
  ADD_CONTOURS = T    
))
dev.off()

# CLP Scotland against Anglophone nations -------------------------------------
png(filename="figures/scotland_in_context/clp_scotland_against_rest_anglophone_nations.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_single_clp(
  dta_anglo, 
  dta_anglo_noscot_overall, 
  "GBR_SCO",
  ADD_CONTOURS = T    
))
dev.off()



# Tables showing excess deaths by particular ages per 100 000 popu --------


ages <- c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80)

init_cohort_size <- 50000 # 50 000 as males and females 
# need to be combined
calculate_survivors <- function(x, init_size = init_cohort_size){
    k <- dim(x)[1]
    cohort_size <- rep(NA, k)
    cohort_size[1] <- init_size
    for (i in 2:k){
      cohort_size[i] <- cohort_size[i - 1] * (1 - x$death_rate[i - 1])
    }
    output <- data.frame(x, cohort_size = cohort_size)
}

# Period-based, Scotland against rest of UK -------------------------------

dta_scot <- dta %>% 
  filter(country == "GBR_SCO") %>% 
  filter(year >= 1950) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland")

dta_ruk <- dta_uk_noscot_overall %>% 
  mutate(country = "ruk") %>% 
  filter(sex != "total") %>% 
  select(country, year, age, sex, death_count, population_count) %>% 
  filter(year >= 1950)

dta_both <- bind_rows(dta_ruk, dta_scot)

# for each region, year, and sex, produce cumulative mortality by 
# different ages 
# 


dta_synth <- dta_both %>% 
  arrange(country, year, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, year, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)

# this calculates and presents the excess deaths per 100 000 population 
# for each decade by certain ages 
dta_synth %>% 
  select(country, year, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = scotland - ruk) %>% 
  filter(age %in% ages) %>% 
  mutate(
    decade = cut(
      year, 
      seq(1950, 2010, by = 10), 
      include.lowest = T,
      labels = c("1950s", "1960S", "1970S", "1980S", "1990s", "2000s")
    )) %>% 
  group_by(decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total) %>% 
  filter(!is.na(decade)) -> excess_deaths_scot_ruk



# Now to compare Scotland with rest of Western Europe

dta_scot <- dta %>% 
  filter(country == "GBR_SCO") %>% 
  filter(year >= 1950) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland")

dta_rwe <- dta_we_noscot_overall %>% 
  mutate(country = "rwe") %>% 
  filter(sex != "total") %>% 
  select(country, year, age, sex, death_count, population_count) %>% 
  filter(year >= 1950)

dta_both <- bind_rows(dta_rwe, dta_scot)


dta_synth <- dta_both %>% 
  arrange(country, year, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, year, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)

# this calculates and presents the excess deaths per 100 000 population 
# for each decade by certain ages 

dta_synth %>% 
  select(country, year, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = scotland - rwe) %>% 
  filter(age %in% ages) %>% 
  mutate(
    decade = cut(
      year, 
      seq(1950, 2010, by = 10), 
      include.lowest = T,
      labels = c("1950s", "1960S", "1970S", "1980S", "1990s", "2000s")
    )) %>% 
  group_by(decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total) %>% 
  filter(!is.na(decade)) -> excess_deaths_scot_rwe


# Now to compare UK with rest of Western Europe

dta_rwe <- dta_we %>% 
  filter(!(country %in% c("GRBCENW", "GBR_NIR", "GBR_SCO"))) %>% 
  filter(year >= 1950) %>%
  filter(sex != "total") %>% 
  group_by(year, age, sex) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(country = "rwe") %>% 
  select(country, year, age, sex, death_count, population_count)

dta_uk <- dta_uk_overall %>% 
  mutate(country = "uk") %>% 
  filter(sex != "total") %>% 
  select(country, year, age, sex, death_count, population_count) %>% 
  filter(year >= 1950)

dta_both <- bind_rows(dta_rwe, dta_uk)


dta_synth <- dta_both %>% 
  arrange(country, year, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, year, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)

# this calculates and presents the excess deaths per 100 000 population 
# for each decade by certain ages 

dta_synth %>% 
  select(country, year, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = uk - rwe) %>% 
  filter(age %in% ages) %>% 
  mutate(
    decade = cut(
      year, 
      seq(1950, 2010, by = 10), 
      include.lowest = T,
      labels = c("1950s", "1960S", "1970S", "1980S", "1990s", "2000s")
    )) %>% 
  group_by(decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total) %>% 
  filter(!is.na(decade)) -> excess_deaths_uk_rwe


# Something similar by cohort 


# cohort-based, Scotland against rest of UK -------------------------------

dta_scot <- dta %>%
  mutate(birth_cohort = year - age) %>% 
  filter(country == "GBR_SCO") %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland") %>% 
  select(country, birth_cohort, age, sex, death_count, population_count)


dta_ruk <- dta_uk_noscot_overall %>%
  mutate(birth_cohort = year - age) %>% 
  mutate(country = "ruk") %>% 
  filter(sex != "total") %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  select(country, birth_cohort, age, sex, death_count, population_count) 
  
dta_both <- bind_rows(dta_ruk, dta_scot)

dta_synth <- dta_both %>% 
  arrange(country, birth_cohort, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, birth_cohort, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)


dta_synth %>% 
  select(country, birth_cohort, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = scotland - ruk) %>% 
  filter(age %in% ages) %>% 
  mutate(
    cohort_decade = cut(
      birth_cohort, 
      seq(1930, 1980, by = 10), 
      include.lowest = T,
      labels = c("1930s", "1940S", "1950S", "1960S", "1970s")
    )) %>% 
  group_by(cohort_decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(cohort_decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total)  -> cohort_excess_deaths_scot_ruk


# Cohort-based, Scotland compared with rest of WE

dta_scot <- dta %>%
  mutate(birth_cohort = year - age) %>% 
  filter(country == "GBR_SCO") %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland") %>% 
  select(country, birth_cohort, age, sex, death_count, population_count)


dta_rwe <- dta_we_noscot_overall %>% 
  mutate(birth_cohort = year - age) %>% 
  mutate(country = "rwe") %>% 
  filter(sex != "total") %>%   
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  select(country, birth_cohort, age, sex, death_count, population_count)  


dta_both <- bind_rows(dta_rwe, dta_scot)


dta_synth <- dta_both %>% 
  arrange(country, birth_cohort, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, birth_cohort, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)

dta_synth %>% 
  select(country, birth_cohort, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = scotland - rwe) %>% 
  filter(age %in% ages ) %>% 
  mutate(
    cohort_decade = cut(
      birth_cohort, 
      seq(1930, 1980, by = 10), 
      include.lowest = T,
      labels = c("1930s", "1940S", "1950S", "1960S", "1970s")
    )) %>% 
  group_by(cohort_decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(cohort_decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total)  -> cohort_excess_deaths_scot_rwe


# Now to compare UK with rest of Western Europe

dta_rwe <- dta_we %>% 
  filter(!(country %in% c("GRBCENW", "GBR_NIR", "GBR_SCO"))) %>% 
  mutate(birth_cohort = year - age) %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  filter(sex != "total") %>% 
  group_by(birth_cohort, age, sex) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(country = "rwe") %>% 
  select(country, birth_cohort, age, sex, death_count, population_count)


dta_uk <- dta_uk_overall %>% 
  mutate(country = "uk") %>% 
  mutate(birth_cohort = year - age) %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  filter(sex != "total") %>% 
  select(country, birth_cohort, age, sex, death_count, population_count)  

dta_both <- bind_rows(dta_rwe, dta_uk)


dta_synth <- dta_both %>% 
  arrange(country, birth_cohort, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, birth_cohort, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)

dta_synth %>% 
  select(country, birth_cohort, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = uk - rwe) %>% 
  filter(age %in% ages ) %>% 
  mutate(
    cohort_decade = cut(
      birth_cohort, 
      seq(1930, 1980, by = 10), 
      include.lowest = T,
      labels = c("1930s", "1940S", "1950S", "1960S", "1970s")
    )) %>% 
  group_by(cohort_decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(cohort_decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total)  -> cohort_excess_deaths_uk_rwe



# produce excel table of these period excess charts 

wb <- createWorkbook()

addDataFrame(
  x = excess_deaths_scot_ruk, 
  sheet = createSheet(wb, sheetName = "period_Scotland_less_rUK")
  )

addDataFrame(
  x = excess_deaths_scot_rwe, 
  sheet = createSheet(wb, sheetName = "period_Scotland_less_rWE")
)

addDataFrame(
  x = excess_deaths_uk_rwe, 
  sheet = createSheet(wb, sheetName = "period_UK_less_rWE")
)

addDataFrame(
  x = cohort_excess_deaths_scot_ruk, 
  sheet = createSheet(wb, sheetName = "cohort_Scotland_less_rUK")
)

addDataFrame(
  x = cohort_excess_deaths_scot_rwe, 
  sheet = createSheet(wb, sheetName = "cohort_Scotland_less_rWE")
)

addDataFrame(
  x = cohort_excess_deaths_uk_rwe, 
  sheet = createSheet(wb, sheetName = "cohort_UK_less_rWE")
)

saveWorkbook(wb, 
             file = "tables/scotland_excess_deaths.xlsx"
             )


