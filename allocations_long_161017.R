#=== SET WORKING DIRECTORY AND LOAD R PACKAGES ===
setwd("c://users/jo122989/desktop/phst2/ampaper/data")
library(plyr)
library(stringr)
library(dplyr)
#=== LOAD FUNDING FILES ===
#=== AND LOOKUP FILES ===
oa.pct <- read.csv("OA to PCT lookup.csv")
oa.lsoa <- read.csv("OA to LSOA lookup.csv")
lsoa.popns <- read.csv("LSOA Populations.csv")
lsoa.ccg <- read.csv("LSOA to CCG lookup.csv")
lat.ccg <- read.csv("AT to CCG lookup.csv")
utla.ltla <- read.csv("UTLA to LTLA lookup.csv")
pct.fund <- read.csv("pct.allocations.2007_2012.csv")
pct.fund <- read.csv("pct.allocations.2007-2012.csv")
ccg.fund <- read.csv("ccg.allocations.2013-2014.csv")
lat.fund <- read.csv("lat.allocations.2013-2014.csv")
ltph.fund <- read.csv("ltla.ph.allocations.2013-2014.csv")
utph.fund <- read.csv("utla.ph.allocations.2013-2014.csv")
#=== CREATE MERGED LOOKUP ===
pct.lsoa <- left_join(oa.pct, oa.lsoa, by = "oa.code")
pct.ccg <- left_join(pct.lsoa, lsoa.ccg, by = "lsoa.code")
pct.lat <- left_join(pct.ccg, lat.ccg, by = "ccg.name")
pct.utla <- left_join(pct.lat, utla.ltla, by = "ltla.name")
lookup <- left_join(pct.utla, lsoa.popns, by = "lsoa.code")
#=== INCORPORATE FUNDING DATA ===
lookup <- left_join(lookup, pct.fund, by = "pct.name")
lookup <- left_join(lookup, ccg.fund, by = "ccg.name")
lookup <- left_join(lookup, lat.fund, by = "lat.name")
lookup <- left_join(lookup, ltph.fund, by = "ltla.name")
lookup <- left_join(lookup, utph.fund, by = "utla.name")
#=== INSPECT NEW DATA FRAME ===
lookup %>% names()
#=== GENERATE POPULATION DENOMINATORS FOR EACH GEOGRAPHICAL LEVEL ===
pct.popn <- lookup %>% group_by(pct.name) %>% summarise(pct.popn=sum(lsoa.popn))
ccg.popn < lookup %>% group_by_ccg.name) %>% summarise(ccg.popn=sum(lsoa.popn))
ccg.popn <- lookup %>% group_by(ccg.name) %>% summarise(ccg.popn=sum(lsoa.popn))
lat.popn <- lookup %>% group_by(lat.name) %>% summarise(lat.popn=sum(lsoa.popn))
ltla.popn <- lookup %>% group_by(ltla.name) %>% summarise(ltla.popn=sum(lsoa.popn))
utla.popn <- lookup %>% group_by(utla.name) %>% summarise(utla.popn=sum(lsoa.popn))
#=== MERGE POPULATION COUNTS WITH LOOKUP ===
add.pct.popn <- left_join(lookup, pct.popn, by = "pct.name")
add.ccg.popn <- left_join(add.pct.popn, ccg.popn, by = "ccg.name")
add.lat.popn <- left_join(add.ccg.popn, lat.popn, by = "lat.name")
add.ltla.popn <- left_join(add.lat.popn, ltla.popn, by = "ltla.name")
add.utla.popn <- left_join(add.ltla.popn, utla.popn, by = "utla.name")
#=== AMEND PH VARIABLES SUCH THAT NA BECOMES 0 ===
ltph.13.zero <- add.utla.popn %>% mutate(ltla.ph.allocation.2013 = ifelse(is.na(ltla.ph.allocation.2013),0,ltla.ph.allocation.2013))
ltph.14.zero <- ltph.13.zero %>% mutate(ltla.ph.allocation.2014 = ifelse(is.na(ltla.ph.allocation.2014),0,ltla.ph.allocation.2014))
utph.13.zero <- ltph.14.zero %>% mutate(utla.ph.allocation.2013 = ifelse(is.na(utla.ph.allocation.2013),0,utla.ph.allocation.2013))
utph.14.zero <- utph.13.zero %>% mutate(utla.ph.allocation.2014 = ifelse(is.na(utla.ph.allocation.2014),0,utla.ph.allocation.2014))
#=== GENERATE LSOA LEVEL FUNDING ALLOCATIONS FOR EACH GEOGRAPHICAL LEVEL ===
pct.2007 <- mutate(utph.14.zero, lsoa.2007 = pct.allocation.2007 * (lsoa.popn/pct.popn))
pct.2008 <- mutate(pct.2007, lsoa.2008 = pct.allocation.2008 * (lsoa.popn/pct.popn))
pct.2009 <- mutate(pct.2008, lsoa.2009 = pct.allocation.2009 * (lsoa.popn/pct.popn))
pct.2010 <- mutate(pct.2010, lsoa.2010 = pct.allocation.2010 * (lsoa.popn/pct.popn))
pct.2010 <- mutate(pct.2009, lsoa.2010 = pct.allocation.2010 * (lsoa.popn/pct.popn))
pct.2011 <- mutate(pct.2010, lsoa.2011 = pct.allocation.2011 * (lsoa.popn/pct.popn))
pct.2012 <- mutate(pct.2011, lsoa.2012 = pct.allocation.2012 * (lsoa.popn/pct.popn))
ccg.2013 <- mutate(pct.2012, ccg.2013 = ccg.allocation.13 * (lsoa.popn/ccg.popn))
ccg.2014 <- mutate(ccg.2013, ccg.2014 = ccg.allocation.14 * (lsoa.popn/ccg.popn))
lat.2013 <- mutate(ccg.2014, lat.2013 = lat.allocation.2013 * (lsoa.popn/lat.popn))
lat.2014 <- mutate(lat.2013, lat.2014 = lat.allocation.2014 * (lsoa.popn/lat.popn))
ltph.2013 <- mutate(lat.2014, ltph.2014 = ltla.ph.allocation.2013 * (lsoa.popn/ltla.popn))
ltph.2013 <- mutate(lat.2014, ltph.2013 = ltla.ph.allocation.2013 * (lsoa.popn/ltla.popn))
ltph.2014 <- mutate(ltph.2013, ltph.2014 = ltla.ph.allocation.2014 * (lsoa.popn/ltla.popn))
utph.2013 <- mutate(ltph.2014, utph.2013 = utla.ph.allocation.2013 * (lsoa.popn/utla.popn))
utph.2014 <- mutate(utph.2013, utph.2014 = utla.ph.allocation.2014 * (lsoa.popn/utla.popn))
#=== AGGREGATE 2013 AND 2014 ALLOCATIONS FOR CCG, LAT, LT PH AND UTPH ALLOCATIONS ===
agg_2013 <- mutate(utph.2014, lsoa.2013 = (ccg.2013 + lat.2013 + ltph.2013 + utph.2013))
agg_2014 <- mutate(agg_2013, lsoa.2014 = (ccg.2014 + lat.2014 + ltph.2014 + utph.2014))
#=== CREATE ANNUAL ALLOCATION SUMMARIES BY LTLA ===
grouped_fund <- group_by(agg_2014, ltla.name)
alloc.07 <- summarise(grouped_fund, funding.2007 = sum(lsoa.2007))
alloc.08 <- summarise(grouped_fund, funding.2008 = sum(lsoa.2008))
alloc.09 <- summarise(grouped_fund, funding.2009 = sum(lsoa.2009))
alloc.10 <- summarise(grouped_fund, funding.2010 = sum(lsoa.2010))
alloc.11 <- summarise(grouped_fund, funding.2011 = sum(lsoa.2011))
alloc.12 <- summarise(grouped_fund, funding.2012 = sum(lsoa.2012))
alloc.13 <- summarise(grouped_fund, funding.2013 = sum(lsoa.2013))
alloc.14 <- summarise(grouped_fund, funding.2014 = sum(lsoa.2014))
#=== MERGE ANNUAL ALLOCATION DATA ===
alloc.0708 <- left_join(alloc.07, alloc.08, by = "ltla.name")
alloc.0709 <- left_join(alloc.0708, alloc.09, by = "ltla.name")
alloc.0710 <- left_join(alloc.0709, alloc.10, by = "ltla.name")
alloc.0711 <- left_join(alloc.0710, alloc.11, by = "ltla.name")
alloc.0712 <- left_join(alloc.0711, alloc.12, by = "ltla.name")
alloc.0713 <- left_join(alloc.0712, alloc.13, by = "ltla.name")
alloc.0714 <- left_join(alloc.0713, alloc.14, by = "ltla.name")
#=== INSPECT NEW DATA FRAME ===
alloc.0714 %>% names()
#=== LOAD 2011 CENSUS POPULATION DATA AND REVISED 2000 IMD SCORES FOR EACH LTLA ===
imd.scores <- read.csv("ltla.imd.data.csv")
census.popns <- read.csv("LA Census Populations.csv")
#=== INCORPORATE THESE INTO DATA FRAME ===
add.imds <- left_join(alloc.0714, imd.scores, by = "ltla.name")
add.popns <- left_join(add.imds, census.popns, by = "ltla.name")
#=== ADJUST ALLOCATION DATA TO PER PERSON ===
pc.allocs <- add.popns
pc.2007 <- mutate(pc.allocs, funding.2007 = (funding.2007/census.popn))
pc.2008 <- mutate(pc.2007, funding.2008 = (funding.2008/census.popn))
pc.2009 <- mutate(pc.2008, funding.2009 = (funding.2009/census.popn))
pct.2010 <- mutate(pct.2009, funding.2010 = (funding.2010/census.popn))
add.popns
pc.allocs
pc.2010 <- mutate(pc.2009, funding.2010 = (funding.2010/census.popn))
pc.2011 <- mutate(pc.2010, funding.2011 = (funding.2011/census.popn))
pc.2012 <- mutate(pc.2011, funding.2012 = (funding.2012/census.popn))
pc.2013 <- mutate(pc.2012, funding.2013 = (funding.2013/census.popn))
pc.2014 <- mutate(pc.2013, funding.2014 = (funding.2014/census.popn))
#=== CREATE QUINTILES OF LA ACCORING TO IMD SCORE ===
allocs.imdq <- pc.2014 %>% mutate(imdq = ntile(ltla.imd, 5))
#=== EXPORT DATA FRAME TO CSV FILE ===
write.csv(allocs.imdq, "NHS Funding Allocations by LTLA 2007-2014 per capita.csv")
#=== LOAD MORTALITY, UNEMPLOYMENT AND HOUSEHOLD INCOME VARIABLES ===
amen.ltla <- read.csv("amenable.mortality.ltlas.csv")
gdhi.ltla <- read.csv("gdhi.2007_2014.csv")
unemp.ltla <- read.csv("unemp.2007_2014.csv")
#=== INCORPORATE INTO FUNDING DATA FRAME ===
add.am <- left_join(allocs.imdq, amen.ltla, by = "ltla.name")
add.gdhi <- left_join(add.am, gdhi.ltla, by = "ltla.name")
add.unemp <- left_join(add.gdhi, unemp.ltla, by = "ltla.name")
#=== LOAD TIDYR PACKAGE ===
library(tidyr)
#=== RENAME FUNDING VARIABLES ===
 test.imdq <- rename(alloc.popn.imdq, c("funding.2007" = "alloc2007", "funding.2008" = "alloc2008", "funding.2009" = "alloc2009", "funding.2010" = "alloc2010", "funding.2011" = "alloc2011", "funding.2012" = "alloc2012", "funding.2013" = "alloc2013", "funding.2014" = "alloc2014"))
 data_wide <- rename(add.unemp, c("funding.2007" = "alloc2007", "funding.2008" = "alloc2008", "funding.2009" = "alloc2009", "funding.2010" = "alloc2010", "funding.2011" = "alloc2011", "funding.2012" = "alloc2012", "funding.2013" = "alloc2013", "funding.2014" = "alloc2014"))
add.unemp %>% names()
detach_package(plyr)
detach(plyr)
detach(package:plyr, unload=TRUE)
detach(package:dplyr, unload=TRUE)
library(plyr)
data_wide <- rename(add.unemp, c("funding.2007" = "alloc2007", "funding.2008" = "alloc2008", "funding.2009" = "alloc2009", "funding.2010" = "alloc2010", "funding.2011" = "alloc2011", "funding.2012" = "alloc2012", "funding.2013" = "alloc2013", "funding.2014" = "alloc2014"))
data_wide %>% name()
data_wide %>% names()
library(dplyr)
data_long <- gather(data_wide, year, allocation, alloc2007:alloc:)
data_long <- gather(data_wide, year, allocation, alloc2007:alloc2014)
data_long %>% names()
#=== REMOVE MORTALITY, GDHI, UNEMPLOYMENT VARIABLES AND CHANGE THESE INTO LONG FORMAT BEFORE MERGING ===
long_alloc = select(1, 2, 3, 4, 29, 30)
long_alloc = select(data_long, 1,2,3,4,29,30)
library(dplyr)
long_alloc = select(data_long, 1, 2, 3, 4, 29, 30)
data_long %>% head()
data_long %>% tail()
data_long2 <- gather(data_long, year, amen, am.dsr.07:am.dsr.14)
data_long2 <- gather(data_long, year2, amen, am.dsr.07:am.dsr.14)
data_long3 <- gather(data_long2, year3, gdhi, gdhi.2007:gdhi.2014)
data_long4 <- gather(data_long3, year4, unemp, unemp.2007:unemp.2014)
data_long4 %>% names()
data_long4
data_long4 %>% names()
data_long <- data_long4[,-2,-3,-7,-9,-11]
data_long <- data_long4[-c(2,3,7,9,11)]
data_long
sapply(data_long, class)
data_long$year <- as.factor(data_long$year)
levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
levels(data_long$year)[levels(data_long$year)=="alloc2014"] <- "2014"
data_long
data_long <- data_long4[-c(2,3,7,9,11)]
data_long <- data_long4[-c(2,7,9,11)]
> data_long$year <- as.factor(data_long$year)
> levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
> levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
> levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
> levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
> levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
> levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
> levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
> levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
> levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
> levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
> levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
> levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
> levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
> levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
> levels(data_long$year)[levels(data_long$year)=="alloc2014"] <- "2014"
data_long
data_long$year <- as.factor(data_long$year)
levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
levels(data_long$year)[levels(data_long$year)=="alloc2014"] <- "2014"
data_long
#=== NOW WITH DATA IN LONG FORMAT, CREATE GRAPHS OF QUINTILE ALLOCATIONS ===
grouped_data <- data_long %>% group_by(imdq) %>% summarise(imdq_pop = sum(census.popn))
detach(package:plyr)
detach(package:dplyr)
library(dplyr)
grouped_data <- data_long %>% group_by(imdq) %>% summarise(imdq_pop = sum(census.popn))
grouped_data <- data_long %>% group_by(imdq)
data_long
#=== EXPORT LONG FORMATTED DATA TO CSV FILE TO RELOAD ===
write.csv(data_long, "data_long.csv")
savehistory("allocations_long_161017.RHistory")
