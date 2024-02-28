library(tidyverse)
library(haven)
library(dplyr)

LAPOP <- read_dta("Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_w.dta")


#filter for desired years and countries
LAPOP1 =
  LAPOP |> 
  filter(
    year %in% (2010:2023),
    pais %in% c(8, 9, 3, 2, 5, 12, 11, 4)
  )

#select desired variables
LAPOP2 =
  LAPOP1 |>
  select(
    pais,
    year,
    fecha,
    wt,
    q1tc_r,
    l1n,
    eff2,
    ing4,
    pn4,
    pop101,
    pop107,
    soct2
  )

#add r_id and source variables
LAPOP3 =
  LAPOP2 |> 
  mutate(
    source = c("LAPOP"),
    r_id = row_number()
  )

#extract dates from fecha for new variable r_month
LAPOP3$r_month <-
  format(as.Date(LAPOP3$fecha, format="%Y-%m-%d"),"%m")

#recode and rename variables
LAPOP4 =
  LAPOP3 |>
  mutate(r_month = recode(r_month,
                          '01' = 'January',
                          '02' = 'February',
                          '03' = 'March',
                          '04' = 'April',
                          '05' = 'May',
                          '06' = 'June',
                          '07' = 'July',
                          '08' = 'August',
                          '09' = 'September',
                          '10' = 'October',
                          '11' = 'November',
                          '12' = 'December'),
         c_name = recode(pais,
                         '2' = 'Guatemala', 
                         '9' = 'Ecuador', 
                         '3' = 'ElSalvador',
                         '8' = 'Colombia',
                         '4' = 'Honduras',
                         '5' = 'Nicaragua',
                         '11' = 'Peru',
                         '12' = 'Paraguy'),
         r_female = recode(q1tc_r,
                           '1' = '0',
                           '2' = '1',
                           '3' = '0'),
         r_swt = wt,
         p_ideology = l1n,
         p_interest = eff2,
         p_economy = soct2,
         d_populism = pop107,
         d_opposition = pop101,
         d_satisfaction = pn4,
         d_support = ing4,
         r_year = year
  )

#select and order desired variables
LAPOP5 =
  LAPOP4 |>
  select(
    source,
    r_id,
    c_name,
    r_year,
    r_month,
    r_swt,
    r_female,
    p_ideology,
    p_interest,
    p_economy,
    d_populism,
    d_opposition,
    d_satisfaction,
    d_support
  )

#convert categorical variables from characters to factors
LAPOP6 =
  LAPOP5 |>
  mutate_at(vars(p_ideology, 
                 p_interest, 
                 d_populism,
                 d_opposition,
                 d_support,
                 r_female,
                 d_satisfaction,
                 p_economy
                 ), as.factor)

#order factors 
LAPOP7 = 
  LAPOP6 |>
  mutate(d_satisfaction = factor(d_satisfaction,
                                 levels = c("1", "2", "3", "4"), ordered = TRUE),
         p_ideology = factor(p_ideology,
                             levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), ordered = TRUE),
         p_interest = factor(p_interest,
                             levels = c("1", "2", "3", "4", "5", "6", "7"), ordered = TRUE),
         p_economy = factor(p_economy,
                            levels = c("1", "2", "3"), ordered = TRUE),
         d_populism = factor(d_populism,
                             levels = c("1", "2", "3", "4", "5", "6", "7"), ordered = TRUE),
         d_opposition = factor(d_opposition,
                               levels = c("1", "2", "3", "4", "5", "6", "7"), ordered = TRUE),
         d_support = factor(d_support,
                            levels = c("1", "2", "3", "4", "5", "6", "7"), ordered = TRUE),
         r_female = factor(r_female,
                                 levels = c("0", "1"), ordered = TRUE))

save(LAPOP7, file = "LAPOP_clean1.Rdata")
write_csv(LAPOP17, file = "LAPOP_clean1.csv")

