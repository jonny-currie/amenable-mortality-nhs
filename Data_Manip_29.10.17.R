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
pct.2010 <- mutate(pct.2009, lsoa.2010 = pct.allocation.2010 * (lsoa.popn/pct.popn))
pct.2011 <- mutate(pct.2010, lsoa.2011 = pct.allocation.2011 * (lsoa.popn/pct.popn))
pct.2012 <- mutate(pct.2011, lsoa.2012 = pct.allocation.2012 * (lsoa.popn/pct.popn))
ccg.2013 <- mutate(pct.2012, ccg.2013 = ccg.allocation.13 * (lsoa.popn/ccg.popn))
ccg.2014 <- mutate(ccg.2013, ccg.2014 = ccg.allocation.14 * (lsoa.popn/ccg.popn))
lat.2013 <- mutate(ccg.2014, lat.2013 = lat.allocation.2013 * (lsoa.popn/lat.popn))
lat.2014 <- mutate(lat.2013, lat.2014 = lat.allocation.2014 * (lsoa.popn/lat.popn))
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
#=== LOAD 2011 CENSUS POPULATION DATA AND REVISED 2000 IMD SCORES FOR EACH LTLA ===
imd.scores <- read.csv("ltla.imd.data.csv")
census.popns <- read.csv("LA Census Populations.csv")
#=== INCORPORATE THESE INTO DATA FRAME ===
add.imds <- left_join(alloc.0714, imd.scores, by = "ltla.name")
add.popns <- left_join(add.imds, census.popns, by = "ltla.name")
#=== CREATE QUINTILES OF LA ACCORING TO IMD SCORE ===
allocs.imdq <- pc.2014 %>% mutate(imdq = ntile(ltla.imd, 5))
#=== CREATE QUINTILES OF LA ACCORING TO IMD SCORE ===
allocs.imdq <- add.popns %>% mutate(imdq = ntile(ltla.imd, 5))
allocs.imdq %>% head()
#=== LOAD MORTALITY, UNEMPLOYMENT AND HOUSEHOLD INCOME VARIABLES ===
amen.ltla <- read.csv("amenable.mortality.ltlas.csv")
gdhi.ltla <- read.csv("gdhi.2007_2014.csv")
unemp.ltla <- read.csv("unemp.2007_2014.csv")
#=== INCORPORATE INTO FUNDING DATA FRAME ===
add.am <- left_join(allocs.imdq, amen.ltla, by = "ltla.name")
add.gdhi <- left_join(add.am, gdhi.ltla, by = "ltla.name")
add.unemp <- left_join(add.gdhi, unemp.ltla, by = "ltla.name")
add.unemp %>% head()
sapply(add.unemp, class)
rm(add.am, add.ghdi, add.unemp)
rm(add.gdhi)
sapply(allocs.imdq, class)
alloc_long <- gather(allocs.imdq, year, allocation, funding.2007:funding.2014)
library(dplyr)
allocs_long <- gather(allocs.imdq, year, allocation, funding.2007:funding.2014)
library(tidyr)
allocs_long <- gather(allocs.imdq, year, allocation, funding.2007:funding.2014)
allocs_long %>% head()
sapply(allocs_long, class)
amen.ltla %>% head()
amen_long <- gather(amen.ltla, year, am_rate, am.dsr.07:am.dsr.14)
amen_long %>% head()
gdhi_long %>% head()
gdhi.ltla %>% head()
gdhi_long <- gather(gdhi.ltla, year, gdhi, gdhi.2007:gdhi.2014)
gdhi_long %>% head()
add.unemp %>% head()
unemp.ltla %>% head()
unemp_long <- gather(unemp.ltla, year, unemp, unemp.2007:unemp.2014)
alloc_am <- left_join(allocs_long, amen_long, by = "ltla.name")
add_gdhi <- left_join(alloc_am, gdhi_long, by = "ltla.name")
data_long <- left_join(add_gdhi, unemp_long, by = "ltla.name")
write.csv(data_long, "testdata.csv")
gdhi2 <- read.csv("gdhi.2007_2014.csv")
gdhi2 %>% head()
gdhi2_long <- gather(gdhi2, year, gdhi, gdhi.2007:gdhi.2014)
add_gdhi2 <- left_join(allocs_long, gdhi2_long, by = "ltla.name")
data_long2 <- left_join(add_gdhi2, unemp_long, by = "ltla.name")
write.csv(data_long2, "testdata.csv")
sapply(allocs_long, class)
allocs_long$year <- as.factor(allocs_long$year)
sapply(allocs_long, class)
sapply(amen_long, class)
amen_long$year <- as.factor(amen_long$year)
sapply(amen_long, class)
sapply(gdhi2, class)
sapply(gdhi2_long, class)
gdhi2_long$year <- as.factor(gdhi2_long$year)
sapply(gdhi2_long, class)
sapply(unemp_long, class)
unemp_long$year <- as.factor(unemp_long$year)
allocs_long$year
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2007"] <- "2007"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2008"] <- "2008"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2009"] <- "2009"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2010"] <- "2010"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2011"] <- "2011"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2012"] <- "2012"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2013"] <- "2013"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2014"] <- "2014"
allocs_long$year
amen_long$year
levels(amen_long$year)[levels(amen_long$year)=="am.dsr.07"] <- "2007"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2007"] <- "2007"
levels(amen_long$year)[levels(amen_long$year)=="am.dsr.08"] <- "2008"
levels(amen_long$year)[levels(amen_long$year)=="am.dsr.09"] <- "2009"
levels(amen_long$year)[levels(amen_long$year)=="am.dsr.10"] <- "2010"
levels(amen_long$year)[levels(amen_long$year)=="am.dsr.11"] <- "2011"
levels(amen_long$year)[levels(amen_long$year)=="am.dsr.12"] <- "2012"
levels(amen_long$year)[levels(amen_long$year)=="am.dsr.13"] <- "2013"
levels(amen_long$year)[levels(amen_long$year)=="am.dsr.14"] <- "2014"
amen_long$year
gdhi2_long$year
levels(gdhi2_long$year)[levels(gdhi2_long$year)=="gdhi.2007"] <- "2007"
levels(gdhi2_long$year)[levels(gdhi2_long$year)=="gdhi.2008"] <- "2008"
levels(gdhi2_long$year)[levels(gdhi2_long$year)=="gdhi.2009"] <- "2009"
levels(gdhi2_long$year)[levels(gdhi2_long$year)=="gdhi.2010"] <- "2010"
levels(gdhi2_long$year)[levels(gdhi2_long$year)=="gdhi.2011"] <- "2011"
levels(gdhi2_long$year)[levels(gdhi2_long$year)=="gdhi.2012"] <- "2012"
levels(gdhi2_long$year)[levels(gdhi2_long$year)=="gdhi.2013"] <- "2013"
levels(gdhi2_long$year)[levels(gdhi2_long$year)=="gdhi.2014"] <- "2014"
gdhi2_long$year
unemp_long$year
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2007"] <- "2007"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2008"] <- "2008"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2009"] <- "2009"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2010"] <- "2010"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2011"] <- "2011"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2012"] <- "2012"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2013"] <- "2013"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2014"] <- "2014"
unemp_long$year
alloc_am2 <- left_join(allocs_long, amen_long, by = "ltla.name")
add_gdhi <- left_join(alloc_am2, gdhi2_long, by = "ltla.name")
final <- left_join(add_gdhi, unemp_long, by = "ltla.name")
final %>% head()
final = select(final, -2, -3, 7, -9, -11)
final %>% head()
write.csv(final, "testing.csv")
sapply(unemp.ltla, class)
sapply(gdhi.ltla, class)
sapply(gdhi2, class)
write.csv(allocs_long, "test_alloc.csv")
write.csv(amen_long, "test_amen.csv")
write.csv(gdhi2_long, "test_gdhi.csv")
write.csv(unemp_long, "test_unemp.csv")
write.csv(alloc_am2, test_allam.csv")
write.csv(alloc_am2, "test_allam.csv")
test <- left_join(allocs_long, amen_long, by = "ltla.name" AND "year")
test2 <- inner_join(allocs_long, amen_long, by = "ltla.name")
write.csv(test2, "inner_join.csv")
sapply(alloc_long, class)
sapply(allocs_long, class)
merged <- merge(allocs_long, amen_long, by=c("ltla.name", "year")
)
merged %>% head()
add_gdhi <- merge(merged, gdhi2_long, by=c("ltla.name", "year"))
add_gdhi %>% head()
add_unemp <- merge(add_gdhi, unemp_long, by=c("ltla.name", "year"))
add_unemp %>% head()
write.csv(add_unemp, "merged_data.csv")
sapply(add_unemp, class)
data = select(add_unemp, -3, -4)
library(plm)
amen.fe <- plm(am_rate ~ allocation + gdhi + unemp, data = data, model = "within")
amen.re <- plm(am_rate ~ allocation + gdhi + unemp, data = data, model = "random")
summary(amen.fe)
summary(amen.re)
