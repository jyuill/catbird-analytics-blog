## canadian stock index data

library(tidyverse)
library(quantmod)
library(here)
library(lubridate)
library(scales)

## reference: ####
# https://www.taxtips.ca/stocksandbonds/historical-investment-returns-stocks-bonds-tbills.htm 

## inflation comp ####
## get inflation data for comparison
infl <- read_csv(here('data','statscan-inflation-smry-1970.csv'))
infl <- infl %>% rename(category=`Products and product groups`,
                        date=REF_DATE)
infl %>% filter(category=='All-items') %>% ggplot(aes(x=date, y=VALUE))+geom_line()

## ^GSPTSE ####
## looks like ^GSPTSE only goes back to Jun 28, 1979
getSymbols(Symbols='^GSPTSE', from='1970-01-01', to='2022-01-31')

## stats can ####
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1010012501&cubeTimeFrame.startMonth=01&cubeTimeFrame.startYear=1970&cubeTimeFrame.endMonth=12&cubeTimeFrame.endYear=2021&referencePeriods=19700101%2C20211201
## how to cite: Statistics Canada, [Table 10-10-0125-01 Toronto Stock Exchange statistics](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1010012501)  
## as before only option that would work was 'Download all data' (2MB)
tse <- read_csv(here('data','10100125.csv'))

## monthly data with 10 metrics each month
## `Toronto Stock Exchange Statistics` are mostly different types of indexes, with different date coverage
## clean
tse$REF_DATE <- ym(tse$REF_DATE)
## filter year
tse_1970 <- tse %>% filter(REF_DATE>='1970-01-01')
## remove unneeded cols
#tse_1970 %>% filter(is.na(TERMINATED)) %>% count(`Toronto Stock Exchange Statistics`)
tse_1970_col <- tse_1970 %>% select(-GEO, -DGUID, -SCALAR_FACTOR, -SCALAR_ID, -VECTOR, -STATUS, -SYMBOL, -DECIMALS )

## statistics - take a look ####
tse_cat <- tse_1970_col %>% group_by(`Toronto Stock Exchange Statistics`) %>% summarize(min_dt=min(REF_DATE),
                                                                                        max_dt=max(REF_DATE))

## get the visual on date range coverage
tse_cat %>% ggplot(aes(x=`Toronto Stock Exchange Statistics`, y=min_dt))+geom_point()+
  coord_flip()+
  geom_point(aes(x=`Toronto Stock Exchange Statistics`, y=max_dt))+geom_point()

tse_cat_lng <- tse_cat %>% pivot_longer(cols=c('min_dt', 'max_dt'), names_to='minmax', values_to='date')
## chart
tse_cat_lng %>% ggplot(aes(x=`Toronto Stock Exchange Statistics`, y=date, color=minmax))+geom_point()+
  #geom_col()+
  #geom_bar(stat='identity', position=position_dodge(0))+
  #scale_fill_manual(values=c('blue','white'))+
  coord_flip()+theme_bw()

## options ####
## - Standard and Poor's/Toronto Stock Exchange Composite Index, close is the ONLY statistic 
##   that spans the 1970-2022 date range
##   - SAVE this separately, since it is key overall
## - Would be nice to delve into sectors to see who does well/poorly
## - Possibly some are different names for same thing - can compare, especially since some have overlap

## clean up statistics col name
## rename for simplification
tse_1970_col <- tse_1970_col %>% rename(
  TSE_stat=`Toronto Stock Exchange Statistics`
)
tse_1970_col <- tse_1970_col %>% rename(
  date=REF_DATE
)
## clean up text
tse_1970_clean <- tse_1970_col %>% mutate(
  TSE_stat=str_replace_all(TSE_stat, 'Toronto Stock Exchange', 'TSE')
)
tse_1970_clean <- tse_1970_clean %>% mutate(
  TSE_stat=str_replace_all(TSE_stat, "Standard and Poor's", 'S&P')
)
tse_1970_clean <- tse_1970_clean %>% mutate(
  TSE_stat=str_replace_all(TSE_stat, "quotations", '')
)

## RECHECK ####
## statistics - take a look ####
tse_cat <- tse_1970_clean %>% group_by(TSE_stat) %>% summarize(min_dt=min(date),
                                                               max_dt=max(date))

tse_cat_lng <- tse_cat %>% pivot_longer(cols=c('min_dt', 'max_dt'), names_to='minmax', values_to='date')
## chart
tse_cat_lng %>% ggplot(aes(x=TSE_stat, y=date, color=minmax))+geom_point()+
  #geom_col()+
  #geom_bar(stat='identity', position=position_dodge(0))+
  #scale_fill_manual(values=c('blue','white'))+
  coord_flip()+theme_bw()

## SAVE ALL ####
write_csv(tse_1970_clean, here('data','tse-1970.csv'))

## SAVE RELEVANT ####
relevant <- tse_cat[c(13, 16:23),]
relevant_cat <- relevant[1]
tse_1970_clean_relevant <- tse_1970_clean %>% 
  filter(TSE_stat %in% relevant_cat[1])

tse_1970_cln_rel <- left_join(relevant_cat, tse_1970_clean, by='TSE_stat')
table(tse_1970_cln_rel$TSE_stat)

write_csv(tse_1970_cln_rel, here('data','tse-1970-relevant-stat.csv'))
