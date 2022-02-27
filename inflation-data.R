
library(tidyverse)
library(here)
library(lubridate)

## Stats Can inflation data
## https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1810000401&pickMembers%5B0%5D=1.2&cubeTimeFrame.startMonth=01&cubeTimeFrame.startYear=1970&cubeTimeFrame.endMonth=01&cubeTimeFrame.endYear=2022&referencePeriods=19700101%2C20220101
## only download option i could get to work was 'Download entire table' -> all years, all dimensions (13MB, 1M+ rows)
## deleted after cleaning due to size
scinf_all <- read_csv(here('data','statscan-inflation-all.csv'))
#summary(scinf_all)

## set date format
scinf_all$REF_DATE <- ym(scinf_all$REF_DATE)
#summary(scinf_all$REF_DATE)

## restrict date range
sc_inflation_1970 <- scinf_all %>% filter(REF_DATE>='1970-01-01')

## set up filter for key categories
cats <- c('All-items',
          'All-items excluding food',
          'All-items excluding food and energy',
          'Food', 
          'Mortgage interest cost',
          'Transportation',
          'Energy',
          'Goods',
          'Services')

sc_inflation_1970_keycat <- sc_inflation_1970 %>% filter(`Products and product groups` %in% cats)

## reduce cols
## select Canada only
sc_inflation_1970_keycat_can <- sc_inflation_1970_keycat %>% filter(GEO=='Canada')
## remove unneeded cols
sc_inflation_1970_keycat_can_sel <- sc_inflation_1970_keycat_can %>% select(-DGUID, -UOM_ID, -SCALAR_FACTOR, -SCALAR_ID,
                                                                        -VECTOR, -STATUS, -SYMBOL, -TERMINATED, -DECIMALS)
## save results
write_csv(sc_inflation_1970_keycat_can_sel, here('data','statscan-inflation-smry-1970.csv'))
