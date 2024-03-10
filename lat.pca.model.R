getAnywhere(phe_life_expectancy)
install.packages("LifeTables")
#=== THIS IS THE CODE FOR DATA MANIPULATION OF NHS FUNDING ALLOCATIONS DATA FOR ENGLAND BETWEEN 2007-2014 AND MERGING WITH OTHER ANALYTICAL VARIABLES ===
#=== SET WORKING DIRECTORY TO FOLDER WITH LOOKUPS AND FUNDING DATA ===
setwd("c://users/jo122989/desktop/phst2/ampaper/data")
#=== LOAD LOOKUP FILES ===
oa.pct <- read.csv("OA to PCT lookup.csv")
oa.lsoa <- read.csv("OA to LSOA lookup.csv")
lsoa.popns <- read.csv("LSOA Populations.csv")
lsoa.ccg <- read.csv("LSOA to CCG lookup.csv")
lat.ccg <- read.csv("AT to CCG lookup.csv")
utla.ltla <- read.csv("UTLA to LTLA lookup.csv")
#=== LOAD DPLYR AND STRINGR PACKAGES ===
library(dplyr)
library(stringr)
#=== CREATE MERGED LOOKUP ===
pct.lsoa <- left_join(oa.pct, oa.lsoa, by = "oa.code")
pct.ccg <- left_join(pct.lsoa, lsoa.ccg, by = "lsoa.code")
pct.lat <- left_join(pct.ccg, lat.ccg, by = "ccg.name")
pct.utla <- left_join(pct.lat, utla.ltla, by = "ltla.name")
lookup <- left_join(pct.utla, lsoa.popns, by = "lsoa.code")
#=== LOAD FUNDING DATA FILES ===
pct.fund <- read.csv("pct.allocations.2007-2012.csv")
ccg.fund <- read.csv("ccg.allocations.2013-2014.csv")
lat.fund <- read.csv("lat.allocations.2013-2014.csv")
ltph.fund <- read.csv("ltla.ph.allocations.2013-2014.csv")
utph.fund <- read.csv("utla.ph.allocations.2013-2014.csv")
#=== MERGE FUNDING DATA WITH LOOKUP FILE ===
pct.fund.lookup <- left_join(lookup, pct.fund, by = "pct.name")
add.ccg.fund <- left_join(pct.fund.lookup, ccg.fund, by = "ccg.name")
add.lat.fund <- left_join(add.ccg.fund, lat.fund, by = "lat.name")
add.ltph.fund <- left_join(add.lat.fund, ltph.fund, by = "ltla.name")
utph.fund <- left_join(add.ltph.fund, utph.fund, by = "utla.name")
#===REMOVE DUPLICATE ROWS AT OA LEVEL===
utph.fund =select(utph.fund, -1)
utph.fund <- distinct(utph.fund)
#=== CALCULATE POPULATION DENOMINATORS FOR EACH GEOGRAPHICAL LEVEL ===
pct.popn <- utph.fund %>% group_by(pct.name) %>% summarise(pct.popn=sum(lsoa.popn))
ccg.popn <- utph.fund %>% group_by(ccg.name) %>% summarise(ccg.popn=sum(lsoa.popn))
lat.popn <- utph.fund %>% group_by(lat.name) %>% summarise(lat.popn=sum(lsoa.popn))
ltla.popn <- utph.fund %>% group_by(ltla.name) %>% summarise(ltla.popn=sum(lsoa.popn))
utla.popn <- utph.fund %>% group_by(utla.name) %>% summarise(utla.popn=sum(lsoa.popn))
#=== MERGE POPULATION COUNTS WITH FUNDING LOOKUP ===
add.pct.popn <- left_join(utph.fund, pct.popn, by = "pct.name")
add.ccg.popn <- left_join(add.pct.popn, ccg.popn, by = "ccg.name")
add.lat.popn <- left_join(add.ccg.popn, lat.popn, by = "lat.name")
add.ltla.popn <- left_join(add.lat.popn, ltla.popn, by = "ltla.name")
add.utla.popn <- left_join(add.ltla.popn, utla.popn, by = "utla.name")
#=== AMEND PH FUNDING VARIABLES SUCH THAT NA BECOMES 0 ===
ltph.13.zero <- add.utla.popn %>% mutate(ltla.ph.allocation.2013 = ifelse(is.na(ltla.ph.allocation.2013),0,ltla.ph.allocation.2013))
ltph.14.zero <- ltph.13.zero %>% mutate(ltla.ph.allocation.2014 = ifelse(is.na(ltla.ph.allocation.2014),0,ltla.ph.allocation.2014))
utph.13.zero <- ltph.14.zero %>% mutate(utla.ph.allocation.2013 = ifelse(is.na(utla.ph.allocation.2013),0,utla.ph.allocation.2013))
pre_agg_fund <- utph.13.zero %>% mutate(utla.ph.allocation.2014 = ifelse(is.na(utla.ph.allocation.2014),0,utla.ph.allocation.2014))
#=== GENERATE LSOA-LEVEL FUNDING ALLOCATIONS FOR EACH GEOGRAPHICAL LEVEL ===
pct.2007 <- mutate(pre_agg_fund, lsoa.2007 = pct.allocation.2007 * (lsoa.popn/pct.popn))
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
#=== AGGREGATE 2013 AND 2014 ALLOCATIONS FOR CCG, LAT, LT PH AND UT PH DATA ===
agg_2013_alloc <- mutate(utph.2014, lsoa.2013 = (ccg.2013 + lat.2013 + ltph.2013 + utph.2013))
lsoa.alloc <- mutate(agg_2013_alloc, lsoa.2014 = (ccg.2014 + lat.2014 + ltph.2014 + utph.2014))
#=== CREATE ANNUAL ALLOCATION SUMMARIES BY LOWER TIER LOCAL AUTHORITY ===
grouped.fund <- group_by(lsoa.alloc, ltla.name)
alloc.07 <- summarise(grouped.fund, funding.2007 = sum(lsoa.2007))
alloc.08 <- summarise(grouped.fund, funding.2008 = sum(lsoa.2008))
alloc.09 <- summarise(grouped.fund, funding.2009 = sum(lsoa.2009))
alloc.10 <- summarise(grouped.fund, funding.2010 = sum(lsoa.2010))
alloc.11 <- summarise(grouped.fund, funding.2011 = sum(lsoa.2011))
alloc.12 <- summarise(grouped.fund, funding.2012 = sum(lsoa.2012))
alloc.13 <- summarise(grouped.fund, funding.2013 = sum(lsoa.2013))
alloc.14 <- summarise(grouped.fund, funding.2014 = sum(lsoa.2014))
#=== MERGE ANNUAL ALLOCATION FRAMES ===
alloc.0708 <- left_join(alloc.07, alloc.08, by = "ltla.name")
alloc.0709 <- left_join(alloc.0708, alloc.09, by = "ltla.name")
alloc.0710 <- left_join(alloc.0709, alloc.10, by = "ltla.name")
alloc.0711 <- left_join(alloc.0710, alloc.11, by = "ltla.name")
alloc.0712 <- left_join(alloc.0711, alloc.12, by = "ltla.name")
alloc.0713 <- left_join(alloc.0712, alloc.13, by = "ltla.name")
alloc.0714 <- left_join(alloc.0713, alloc.14, by = "ltla.name")
#===CONVERT FUNDING ALLOCATIONS TO REAL TERMS USING GDP MARKET DEFLATORS===
alloc.0714 <- mutate(alloc.0714, funding.2014 = funding.2014 * 0.97204)
alloc.0714 <- mutate(alloc.0714, funding.2013 = funding.2013 * 0.95815)
alloc.0714 <- mutate(alloc.0714, funding.2012 = funding.2012 * 0.94206)
alloc.0714 <- mutate(alloc.0714, funding.2011 = funding.2011 * 0.92289)
alloc.0714 <- mutate(alloc.0714, funding.2010 = funding.2010 * 0.90979)
alloc.0714 <- mutate(alloc.0714, funding.2009 = funding.2009 * 0.89346)
alloc.0714 <- mutate(alloc.0714, funding.2008 = funding.2008 * 0.88067)
alloc.0714 <- mutate(alloc.0714, funding.2007 = funding.2007 * 0.85834)
#=== EXPORT TO CSV FILE ===
write.csv(alloc.0714, "allocations.2007_2014.csv")
#=== LOAD IMD AND LA POPULATION FILES ===
ltla.popns <- read.csv("ltla.2011.popns.csv")
ltla.imd.scores <- read.csv("ltla.imd.data.csv")
#=== MERGE LA POPULATION AND IMD DATA ===
alloc.ltlapopn <- left_join(alloc.0714, ltla.popns, by = "ltla.name")
alloc.popn.imd <- left_join(alloc.ltlapopn, ltla.imd.scores, by = "ltla.name")
#=== CREATE QUINTILES OF LA ACCORDING TO IMD SCORE ===
alloc.popn.imdq <- alloc.popn.imd %>% mutate(imdq = ntile(ltla.imd, 5))
#=== LOAD  UNEMPLOYMENT AND HOUSEHOLD INCOME VARIABLES ===
gdhi.ltla <- read.csv("ltla.gdhipc.csv")
unemp.ltla <- read.csv("unemp.2007_2014.csv")
#=== CONVERT VARIABLES FROM WIDE TO LONG FORMAT FOR ANALYSIS ===
library(tidyr)
allocs_long <- gather(alloc.popn.imdq, year, allocation, funding.2007:funding.2014)
gdhi_long <- gather(gdhi.ltla, year, gdhi, gdhipc.2007:gdhipc.2014)
unemp_long <- gather(unemp.ltla, year, unemp, unemp.2007:unemp.2014)
#=== TRANSFORM YEAR VARIABLE INTO NUMERICAL FIGURE ===
allocs_long$year <- as.factor(allocs_long$year)
gdhi_long$year <- as.factor(gdhi_long$year)
unemp_long$year <- as.factor(unemp_long$year)
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2007"] <- "2007"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2008"] <- "2008"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2009"] <- "2009"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2010"] <- "2010"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2011"] <- "2011"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2012"] <- "2012"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2013"] <- "2013"
levels(allocs_long$year)[levels(allocs_long$year)=="funding.2014"] <- "2014"
levels(gdhi_long$year)[levels(gdhi_long$year)=="gdhipc.2007"] <- "2007"
levels(gdhi_long$year)[levels(gdhi_long$year)=="gdhipc.2008"] <- "2008"
levels(gdhi_long$year)[levels(gdhi_long$year)=="gdhipc.2009"] <- "2009"
levels(gdhi_long$year)[levels(gdhi_long$year)=="gdhipc.2010"] <- "2010"
levels(gdhi_long$year)[levels(gdhi_long$year)=="gdhipc.2011"] <- "2011"
levels(gdhi_long$year)[levels(gdhi_long$year)=="gdhipc.2012"] <- "2012"
levels(gdhi_long$year)[levels(gdhi_long$year)=="gdhipc.2013"] <- "2013"
levels(gdhi_long$year)[levels(gdhi_long$year)=="gdhipc.2014"] <- "2014"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2007"] <- "2007"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2008"] <- "2008"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2009"] <- "2009"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2010"] <- "2010"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2011"] <- "2011"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2012"] <- "2012"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2013"] <- "2013"
levels(unemp_long$year)[levels(unemp_long$year)=="unemp.2014"] <- "2014"
#=== MERGE VARIABLES INTO SINGLE DATAFRAME FOR ANALYSIS
alloc_gdhi <- merge(allocs_long, gdhi_long, by=c("ltla.name", "year"))
alloc_gdhi_unem <- merge(alloc_gdhi, unemp_long, by=c("ltla.name", "year"))
#=== LOAD AMENABLE MORTALITY FILES ===
am_male <- read.csv("am.male.csv", stringsAsFactors = TRUE)
am_female <- read.csv("am.female.csv", stringsAsFactors = TRUE)
am_male$year <- as.factor(am_male$year)
am_female$year <- as.factor(am_female$year)
#=== MERGE AMENABLE MORTALITY DATA WITH EXPLANATORY VARIABLE DATA FRAME
add_am_male <- merge(alloc_gdhi_unem, am_male, by=c("ltla.name", "year"))
model_data <- merge(add_am_male, am_female, by=c("ltla.name", "year"))
#===CREATE PER CAPITA ALLOCATION VARIABLE BASED ON LTLA POPULATIONS===
model_data <- mutate(model_data, pc.alloc = (allocation/ltla.popn.2011))
#===LOAD AMENABLE MORTALITY EXCLUDING IHD AND NON-AMENABLE MORTALITY DATA AND MERGE WITH DATA FRAME===
male.yll <- read.csv("male.yll.csv", na.strings="*")
female.yll <- read.csv("female.yll.csv", na.strings="*")
male.yll$year <- as.factor(male.yll$year)
female.yll$year <- as.factor(female.yll$year)
male.am.exc.ihd <- read.csv("male.am.exc.ihd.csv")
fem.am.exc.ihd <- read.csv("female.am.exc.ihd.csv")
male.non.am <- read.csv("male.non.am.csv")
fem.non.am <- read.csv("female.non.am.csv")
model_data <- merge(model_data, male.am.exc.ihd, by =c("ltla.name", "year"))
model_data <- merge(model_data, fem.am.exc.ihd, by =c("ltla.name", "year"))
model_data <- merge(model_data, male.non.am, by =c("ltla.name", "year"))
model_data <- merge(model_data, fem.non.am, by =c("ltla.name", "year"))
model_data <- merge(model_data, male.yll, by=c("ltla.name", "year"))
model_data <- merge(model_data, female.yll, by=c("ltla.name", "year"))
#===REMOVE DATA FOR ISLES OF SCILLY AND CITY OF LONDON===
model_data <- model_data[! grepl("City of London", model_data$ltla.name),]
model_data <- model_data[! grepl("Isles of Scilly", model_data$ltla.name),]
#===CREATE PER CAPITA ALLOCATION VARIABLE BASED ON LTLA POPULATIONS===
model_data <- mutate(model_data, pc.alloc = (allocation/ltla.popn.2011))
#=== EXPORT DATASET FOR INSPECTION TO CSV FILE ===
write.csv(model_data, "merged_data.csv")
#=== CREATE GRAPH OF TRENDS IN ALLOCATION BY LTLAQ (LTLA QUINTILES BY IMD DEPRIVATION SCORE) BETWEEN 2007 AND 2014===
model_data$imdq <- as.factor(model_data$imdq)
calc_imdq_popns = select(model_data, -2,-4,-6,-7,-8,-9,-10,-11, -12, -13, -14, -15, -16, -17)
calc_imdq_popns <- distinct(calc_imdq_popns)
imdq_popns<- calc_imdq_popns %>% group_by(imdq) %>% summarise(quin_popn = sum(ltla.popn.2011))
model_data_imdq_popns <- left_join(model_data, imdq_popns, by = "imdq")
imdq_wts <- mutate(model_data_imdq_popns, weight = (ltla.popn.2011/quin_popn))
imdq_alloc_wts <- mutate(imdq_wts, alloc_wt = (pc.alloc * weight))
grouped_imdq_alloc <- imdq_alloc_wts %>% group_by(imdq, year)
wt_imdq_alloc <- summarise(grouped_imdq_alloc, wtd_alloc = sum(alloc_wt))
alloc.1 <- subset(wt_imdq_alloc, imdq=="1")
alloc.5 <- subset(wt_imdq_alloc, imdq=="5")
names(alloc.1)[names(alloc.1) == "wtd_alloc"] <- "imdq1.alloc"
names(alloc.5)[names(alloc.5) == "wtd_alloc"] <- "imdq5.alloc"
allocs <- merge(alloc.1, alloc.5, by="year")
allocs = select(allocs, -2, -4)
allocs <- mutate(allocs, abs=imdq5.alloc-imdq1.alloc)
allocs <- mutate(allocs, rel=((imdq5.alloc-imdq1.alloc)/imdq1.alloc)*100)
allocs$year <- as.numeric(as.character(allocs$year))
library(ggplot2)
p1 <- ggplot(data=allocs, aes(x=year)) + geom_line(aes(y=imdq1.alloc, colour="Most affluent areas"), size=1.5)
p1 <- p1 + geom_line(aes(y=imdq5.alloc, colour="Most deprived areas"), size=1.5)
p1 <- p1 + geom_line(aes(y=abs, colour="Absolute difference"), linetype=5, size=1.5)
p1 <- p1 + geom_line(aes(y=rel*2000/30, colour="Relative difference"), linetype=2, size=1.5)
p1 <- p1 + scale_y_continuous(sec.axis = sec_axis(~.*30/2000, name = "Percentage difference"), limits=c(0,2000), breaks=seq(0,2000,by=500))
p1 <- p1 + labs(title="Trends in NHS funding allocations to local areas", x="Year", y="NHS funding allocation per person") 
p1 <- p1 + scale_x_continuous(breaks=c(2007,2008,2009,2010,2011,2012,2013,2014))
p1 <- p1 + theme(axis.text.x=element_text(colour="black", size=12, angle=45), axis.text.y=element_text(colour="black", size=12))
p1 <- p1 + theme(axis.title=element_text(face="bold"))
p1 <- p1 + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
p1 <- p1 + theme(plot.title=element_text(face="bold"))
p1 <- p1 + theme(axis.ticks.x=element_blank())
p1 <- p1 + scale_colour_discrete(name="Legend")
p1
#=== CREATE GRAPHS OF TRENDS IN MALE AMENABLE MORTALITY RATES BY LTLAQ 2007 - 2014 ===
imdq_male_dsr_wts <- mutate(imdq_wts, male_dsr_wt = (male_dsr * weight))
grouped_imdq_male_dsr_wts <- imdq_male_dsr_wts %>% group_by(imdq, year)
wt_male_dsr_imdq <- summarise(grouped_imdq_male_dsr_wts, wt_male_dsr = sum(male_dsr_wt))
am.male.1 <- subset(wt_male_dsr_imdq, imdq=="1")
am.male.5 <- subset(wt_male_dsr_imdq, imdq=="5")
names(am.male.1)[names(am.male.1) == "wt_male_dsr"] <- "imdq1.am"
names(am.male.5)[names(am.male.5) == "wt_male_dsr"] <- "imdq5.am"
male.am.quin <- merge(am.male.1, am.male.5, by = "year")
male.am.quin = select(male.am.quin, -2, -4)
male.am.quin <- mutate(male.am.quin, abs=imdq5.am-imdq1.am)
male.am.quin <- mutate(male.am.quin, rel=((imdq5.am-imdq1.am)/imdq1.am)*100)
male.am.quin$year <- as.numeric(as.character(male.am.quin$year))
p2 <- ggplot(data=male.am.quin, aes(x=year)) + geom_line(aes(y=imdq1.am, colour="Most affluent areas"), size=1.5)
p2 <- p2 + geom_line(aes(y=imdq5.am, colour="Most deprived areas"), size=1.5)
p2 <- p2 + geom_line(aes(y=abs, colour="Absolute difference"), linetype=5, size=1.5)
p2 <- p2 + geom_line(aes(y=rel*250/100, colour="Relative difference"), linetype=2, size=1.5)
p2 <- p2 + scale_y_continuous(sec.axis = sec_axis(~.*100/250, name = "Percentage difference"), limits=c(0,250), breaks=seq(0,250,by=50))
p2 <- p2 + labs(title="Trends in mortality amenable to healthcare in deprived and affluent areas", x="Year", y="Under 75 AS mortality rate/100,000 population") 
p2 <- p2 + scale_x_continuous(breaks=c(2007,2008,2009,2010,2011,2012,2013,2014))
p2 <- p2 + theme(axis.text.x=element_text(colour="black", size=12, angle=45), axis.text.y=element_text(colour="black", size=12))
p2 <- p2 + theme(axis.title=element_text(face="bold"))
p2 <- p2 + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
p2 <- p2 + theme(plot.title=element_text(face="bold"))
p2 <- p2 + theme(axis.ticks.x=element_blank())
p2 <- p2 + scale_colour_discrete(name="Legend")
#=== CREATE GRAPHS OF TRENDS IN FEMALE AMENABLE MORTALITY RATES BY LTLAQ 2007 - 2014 ===
imdq_fem_dsr_wts <- mutate(imdq_wts, fem_dsr_wt = (fem_dsr * weight))
grouped_imdq_fem_dsr_wts <- imdq_fem_dsr_wts %>% group_by(imdq, year)
wt_fem_dsr_imdq <- summarise(grouped_imdq_fem_dsr_wts, wt_female_dsr = sum(fem_dsr_wt))
am.female.1 <- subset(wt_fem_dsr_imdq, imdq=="1")
am.female.5 <- subset(wt_fem_dsr_imdq, imdq=="5")
names(am.female.1)[names(am.female.1) == "wt_female_dsr"] <- "imdq1.am"
names(am.female.5)[names(am.female.5) == "wt_female_dsr"] <- "imdq5.am"
female.am.quin <- merge(am.female.1, am.female.5, by = "year")
female.am.quin = select(female.am.quin, -2, -4)
female.am.quin <- mutate(female.am.quin, abs=imdq5.am-imdq1.am)
female.am.quin <- mutate(female.am.quin, rel=((imdq5.am-imdq1.am)/imdq1.am)*100)
female.am.quin$year <- as.numeric(as.character(female.am.quin$year))
p3 <- ggplot(data=female.am.quin, aes(x=year)) + geom_line(aes(y=imdq1.am, colour="Most affluent areas"), size=1.5)
p3 <- p3 + geom_line(aes(y=imdq5.am, colour="Most deprived areas"), size=1.5)
p3 <- p3 + geom_line(aes(y=abs, colour="Absolute difference"), linetype=5, size=1.5)
p3 <- p3 + geom_line(aes(y=rel*150/100, colour="Relative difference"), linetype=2, size=1.5)
p3 <- p3 + scale_y_continuous(sec.axis = sec_axis(~.*100/150, name = "Percentage difference"), limits=c(0,150), breaks=seq(0,150,by=50))
p3 <- p3 + labs(title="Trends in mortality amenable to healthcare in deprived and affluent areas", x="Year", y="Under 75 AS mortality rate/100,000 population") 
p3 <- p3 + scale_x_continuous(breaks=c(2007,2008,2009,2010,2011,2012,2013,2014))
p3 <- p3 + theme(axis.text.x=element_text(colour="black", size=12, angle=45), axis.text.y=element_text(colour="black", size=12))
p3 <- p3 + theme(axis.title=element_text(face="bold"))
p3 <- p3 + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
p3 <- p3 + theme(plot.title=element_text(face="bold"))
p3 <- p3 + theme(axis.ticks.x=element_blank())
p3 <- p3 + scale_colour_discrete(name="Legend")
#=== CREATE GRAPH OF TRENDS IN MALE NON-AMENABLE MORTALITY RATES BY LTLAQ 2007 - 2014 ===
imdq_male_non_am_wts <- mutate(imdq_wts, male_non_am_wt = (male_dsr_non_amen * weight))
grouped_imdq_male_non_am_wts <- imdq_male_non_am_wts %>% group_by(imdq, year)
wt_male_non_am <- summarise(grouped_imdq_male_non_am_wts, wt_male_non_am = sum(male_non_am_wt))
male.nonam.1 <- subset(wt_male_non_am, imdq=="1")
male.nonam.5 <- subset(wt_male_non_am, imdq=="5")
names(male.nonam.1)[names(male.nonam.1) == "wt_male_non_am"] <- "imdq1.dsr"
names(male.nonam.5)[names(male.nonam.5) == "wt_male_non_am"] <- "imdq5.dsr"
male.non.am <- merge(male.nonam.1, male.nonam.5, by = "year")
male.non.am = select(male.non.am, -2, -4)
male.non.am <- mutate(male.non.am, abs=imdq5.dsr-imdq1.dsr)
male.non.am <- mutate(male.non.am, rel=((imdq5.dsr-imdq1.dsr)/imdq1.dsr)*100)
male.non.am$year <- as.numeric(as.character(male.non.am$year))
p4 <- ggplot(data=male.non.am, aes(x=year)) + geom_line(aes(y=imdq1.dsr, colour="Most affluent areas"), size=1.5)
p4 <- p4 + geom_line(aes(y=imdq5.dsr, colour="Most deprived areas"), size=1.5)
p4 <- p4 + geom_line(aes(y=abs, colour="Absolute difference"), linetype=5, size=1.5)
p4 <- p4 + geom_line(aes(y=rel*400/100, colour="Relative difference"), linetype=2, size=1.5)
p4 <- p4 + scale_y_continuous(sec.axis = sec_axis(~.*100/400, name = "Percentage difference"), limits=c(0,400), breaks=seq(0,400,by=50))
p4 <- p4 + labs(title="Trends in mortality not amenable to healthcare in deprived and affluent areas", x="Year", y="Under 75 AS mortality rate/100,000 population") 
p4 <- p4 + scale_x_continuous(breaks=c(2007,2008,2009,2010,2011,2012,2013,2014))
p4 <- p4 + theme(axis.text.x=element_text(colour="black", size=12, angle=45), axis.text.y=element_text(colour="black", size=12))
p4 <- p4 + theme(axis.title=element_text(face="bold"))
p4 <- p4 + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
p4 <- p4 + theme(plot.title=element_text(face="bold"))
p4 <- p4 + theme(axis.ticks.x=element_blank())
p4 <- p4 + scale_colour_discrete(name="Legend")
#=== CREATE GRAPH OF TRENDS IN FEMALE NON-AMENABLE MORTALITY RATES BY LTLAQ 2007 - 2014 ===
imdq_female_non_am_wts <- mutate(imdq_wts, female_non_am_wt = (fem_dsr_non_am * weight))
grouped_imdq_female_non_am_wts <- imdq_female_non_am_wts %>% group_by(imdq, year)
wt_female_non_am <- summarise(grouped_imdq_female_non_am_wts, wt_female_non_am = sum(female_non_am_wt))
female.nonam.1 <- subset(wt_female_non_am, imdq=="1")
female.nonam.5 <- subset(wt_female_non_am, imdq=="5")
names(female.nonam.1)[names(female.nonam.1) == "wt_female_non_am"] <- "imdq1.dsr"
names(female.nonam.5)[names(female.nonam.5) == "wt_female_non_am"] <- "imdq5.dsr"
female.non.am <- merge(female.nonam.1, female.nonam.5, by = "year")
female.non.am = select(female.non.am, -2, -4)
female.non.am <- mutate(female.non.am, abs=imdq5.dsr-imdq1.dsr)
female.non.am <- mutate(female.non.am, rel=((imdq5.dsr-imdq1.dsr)/imdq1.dsr)*100)
female.non.am$year <- as.numeric(as.character(female.non.am$year))
p5 <- ggplot(data=female.non.am, aes(x=year)) + geom_line(aes(y=imdq1.dsr, colour="Most affluent areas"), size=1.5)
p5 <- p5 + geom_line(aes(y=imdq5.dsr, colour="Most deprived areas"), size=1.5)
p5 <- p5 + geom_line(aes(y=abs, colour="Absolute difference"), linetype=5, size=1.5)
p5 <- p5 + geom_line(aes(y=rel*250/100, colour="Relative difference"), linetype=2, size=1.5)
p5 <- p5 + scale_y_continuous(sec.axis = sec_axis(~.*100/250, name = "Percentage difference"), limits=c(0,250), breaks=seq(0,250,by=50))
p5 <- p5 + labs(title="Trends in mortality not amenable to healthcare in deprived and affluent areas", x="Year", y="Under 75 AS mortality rate/100,000 population") 
p5 <- p5 + scale_x_continuous(breaks=c(2007,2008,2009,2010,2011,2012,2013,2014))
p5 <- p5 + theme(axis.text.x=element_text(colour="black", size=12, angle=45), axis.text.y=element_text(colour="black", size=12))
p5 <- p5 + theme(axis.title=element_text(face="bold"))
p5 <- p5 + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
p5 <- p5 + theme(plot.title=element_text(face="bold"))
p5 <- p5 + theme(axis.ticks.x=element_blank())
p5 <- p5 + scale_colour_discrete(name="Legend")
#=== CENTRE PREDICTOR VARIABLES USING SCALE()
centre_scale <- function(x) { 
scale(x, scale = FALSE) 
}
centred_data <- model_data %>% mutate(allocation = centre_scale(pc.alloc), gdhi = centre_scale(gdhi), unemp = centre_scale(unemp))
#=== REORDER IMDQ LEVELS SUCH THAT IMDQ1 IS REFERENCE CATEGORY FOR DUMMY VARIABLE===
data <- within(centred_data, imdq <- relevel(imdq, ref = 1))
#=== LOAD PLM PACKAGE FOR FIXED AND RANDOM EFFECTS MODELLING OF PANEL DATA ===
library(plm)
#===RUN FIXED EFFECTS MODELS FOR MALE AND FEMALE AMENABLE MORTALITY INCORPORATING INDIVIDUAL AND TIME EFFECTS===
model.m <- plm(male_dsr ~ allocation + gdhi + unemp + (allocation * imdq), data = data, model = "within", index=c("ltla.name", "year"), effect = "twoways")
model.f <- plm(fem_dsr ~ allocation + gdhi + unemp + (allocation * imdq), data = data, model = "within", index=c("ltla.name", "year"), effect = 
"twoways")
#===APPLY ROBUST STANDARD ERRORS TO FIXED EFFECTS MODELS FOR MALE AND FEMALE AMENABLE MORTALITY, USING COEFTEST FROM LMTEST PACKAGE 
#AND GENERATE ESTIMATES AND CONFIDENCE INTERVALS FOR LINEAR CMOBINATIONS OF ALLOCATION:IMDQ INTERACTIONS===
library(sandwich)
library(multcomp)
imdq2m <- glht(model.m, linfct = c("allocation + allocation:imdq2 = 0"), vcov=vcovHC)
imdq3m <- glht(model.m, linfct = c("allocation + allocation:imdq3 = 0"), vcov=vcovHC)
imdq4m <- glht(model.m, linfct = c("allocation + allocation:imdq4 = 0"), vcov=vcovHC)
imdq5m <- glht(model.m, linfct = c("allocation + allocation:imdq5 = 0"), vcov=vcovHC)
imdq2f <- glht(model.f, linfct = c("allocation + allocation:imdq2 = 0"), vcov=vcovHC)
imdq3f <- glht(model.f, linfct = c("allocation + allocation:imdq3 = 0"), vcov=vcovHC)
imdq4f <- glht(model.f, linfct = c("allocation + allocation:imdq4 = 0"), vcov=vcovHC)
imdq5f <- glht(model.f, linfct = c("allocation + allocation:imdq5 = 0"), vcov=vcovHC)
#===RETRIEVE SUMMARIES FOR EACH MODEL AND PRINT TO TEXT FILE===
sink("model.outputs.txt", append=TRUE, split=TRUE)
summary(model.m)
confint(model.m)
summary(model.f)
confint(model.f)
summary(imdq2m)
confint(imdq2m)
summary(imdq3m)
confint(imdq3m)
summary(imdq4m)
confint(imdq4m)
summary(imdq5m)
confint(imdq5m)
summary(imdq2f)
confint(imdq2f)
summary(imdq3f)
confint(imdq3f)
summary(imdq4f)
confint(imdq4f)
summary(imdq5f)
confint(imdq5f)
#===TURN OFF SINK===
sink(file=NULL)
#===RUN MODELS WITH POTENTIAL YEARS OF LIFE LOST, AMENABLE MORTALITY EXCLUDING IHD AND NON-AMENABLE MORTALITY===
model.m.yll <- plm(male.pyll ~ allocation + gdhi + unemp + (allocation * imdq), data = data, model = "within", index=c("ltla.name", "year"), effect = "twoways")
model.f.yll <- plm(female.pyll ~ allocation + gdhi + unemp + (allocation * imdq), data = data, model = "within", index=c("ltla.name", "year"), effect = "twoways")
model.m.ex.ihd <- plm(male_dsr_am_exihd ~ allocation + gdhi + unemp + (allocation * imdq), data = data, model = "within", index=c("ltla.name", "year"), effect = "twoways")
model.f.ex.ihd <- plm(fem_dsr_am_exihd ~ allocation + gdhi + unemp + (allocation * imdq), data = data, model = "within", index=c("ltla.name", "year"), effect = 
"twoways")
model.m.nonam <- plm(male_dsr_non_amen ~ allocation + gdhi + unemp + (allocation * imdq), data = data, model = "within", index=c("ltla.name", "year"), effect = 
"twoways")
model.f.nonam <- plm(fem_dsr_non_am ~ allocation + gdhi + unemp + (allocation * imdq), data = data, model = "within", index=c("ltla.name", "year"), effect = 
"twoways")
#===APPLY ROBUST STANDARD ERRORS TO PYLL MODELS AND EXTRACT GENERATE LINEAR COMBINATION COEFFICIENT ESTIMATES WITH 95% CIS===
imdq2m.yll <- glht(model.m.yll, linfct = c("allocation + allocation:imdq2 = 0"), vcov=vcovHC)
imdq3m.yll <- glht(model.m.yll, linfct = c("allocation + allocation:imdq3 = 0"), vcov=vcovHC)
imdq4m.yll <- glht(model.m.yll, linfct = c("allocation + allocation:imdq4 = 0"), vcov=vcovHC)
imdq5m.yll <- glht(model.m.yll, linfct = c("allocation + allocation:imdq5 = 0"), vcov=vcovHC)
imdq2f.yll <- glht(model.f.yll, linfct = c("allocation + allocation:imdq2 = 0"), vcov=vcovHC)
imdq3f.yll <- glht(model.f.yll, linfct = c("allocation + allocation:imdq3 = 0"), vcov=vcovHC)
imdq4f.yll <- glht(model.f.yll, linfct = c("allocation + allocation:imdq4 = 0"), vcov=vcovHC)
imdq5f.yll <- glht(model.f.yll, linfct = c("allocation + allocation:imdq5 = 0"), vcov=vcovHC)
#===APPLY ROBUST STANDARD ERRORS TO AM EXCLUDING IHD MODELS AND EXTRACT GENERATE LINEAR COMBINATION COEFFICIENT ESTIMATES WITH 95% CIS===
imdq2m.ex.ihd <- glht(model.m.ex.ihd, linfct = c("allocation + allocation:imdq2 = 0"), vcov=vcovHC)
imdq3m.ex.ihd <- glht(model.m.ex.ihd, linfct = c("allocation + allocation:imdq3 = 0"), vcov=vcovHC)
imdq4m.ex.ihd <- glht(model.m.ex.ihd, linfct = c("allocation + allocation:imdq4 = 0"), vcov=vcovHC)
imdq5m.ex.ihd <- glht(model.m.ex.ihd, linfct = c("allocation + allocation:imdq5 = 0"), vcov=vcovHC)
imdq2f.ex.ihd <- glht(model.f.ex.ihd, linfct = c("allocation + allocation:imdq2 = 0"), vcov=vcovHC)
imdq3f.ex.ihd <- glht(model.f.ex.ihd, linfct = c("allocation + allocation:imdq3 = 0"), vcov=vcovHC)
imdq4f.ex.ihd <- glht(model.f.ex.ihd, linfct = c("allocation + allocation:imdq4 = 0"), vcov=vcovHC)
imdq5f.ex.ihd <- glht(model.f.ex.ihd, linfct = c("allocation + allocation:imdq5 = 0"), vcov=vcovHC)
#===APPLY ROBUST STANDARD ERRORS TO NON AM MODELS AND EXTRACT GENERATE LINEAR COMBINATION COEFFICIENT ESTIMATES WITH 95% CIS===
imdq2m.nonam <- glht(model.m.nonam, linfct = c("allocation + allocation:imdq2 = 0"), vcov=vcovHC)
imdq3m.nonam <- glht(model.m.nonam, linfct = c("allocation + allocation:imdq3 = 0"), vcov=vcovHC)
imdq4m.nonam <- glht(model.m.nonam, linfct = c("allocation + allocation:imdq4 = 0"), vcov=vcovHC)
imdq5m.nonam <- glht(model.m.nonam, linfct = c("allocation + allocation:imdq5 = 0"), vcov=vcovHC)
imdq2f.nonam <- glht(model.f.nonam, linfct = c("allocation + allocation:imdq2 = 0"), vcov=vcovHC)
imdq3f.nonam <- glht(model.f.nonam, linfct = c("allocation + allocation:imdq3 = 0"), vcov=vcovHC)
imdq4f.nonam <- glht(model.f.nonam, linfct = c("allocation + allocation:imdq4 = 0"), vcov=vcovHC)
imdq5f.nonam <- glht(model.f.nonam, linfct = c("allocation + allocation:imdq5 = 0"), vcov=vcovHC)
#===RETRIEVE SUMMARIES FOR PYLL MODELS AND PRINT TO TEXT FILE===
sink("model.outputs.txt", append=TRUE, split=TRUE)
summary(model.m.yll)
confint(model.m.yll)
summary(model.f.yll)
confint(model.f.yll)
summary(imdq2m.yll)
confint(imdq2m.yll)
summary(imdq3m.yll)
confint(imdq3m.yll)
summary(imdq4m.yll)
confint(imdq4m.yll)
summary(imdq5m.yll)
confint(imdq5m.yll)
summary(imdq2f.yll)
confint(imdq2f.yll)
summary(imdq3f.yll)
confint(imdq3f.yll)
summary(imdq4f.yll)
confint(imdq4f.yll)
summary(imdq5f.yll)
confint(imdq5f.yll)
#===RETRIEVE SUMMARIES FOR AM EXCLUDING IHD MODELS AND PRINT TO TEXT FILE===
summary(model.m.ex.ihd)
confint(model.m.ex.ihd)
summary(model.f.ex.ihd)
confint(model.f.ex.ihd)
summary(imdq2m.ex.ihd)
confint(imdq2m.ex.ihd)
summary(imdq3m.ex.ihd)
confint(imdq3m.ex.ihd)
summary(imdq4m.ex.ihd)
confint(imdq4m.ex.ihd)
summary(imdq5m.ex.ihd)
confint(imdq5m.ex.ihd)
summary(imdq2f.ex.ihd)
confint(imdq2f.ex.ihd)
summary(imdq3f.ex.ihd)
confint(imdq3f.ex.ihd)
summary(imdq4f.ex.ihd)
confint(imdq4f.ex.ihd)
summary(imdq5f.ex.ihd)
confint(imdq5f.ex.ihd)
#===RETRIEVE SUMMARIES FOR AM EXCLUDING IHD MODELS AND PRINT TO TEXT FILE===
summary(model.m.nonam)
confint(model.m.nonam)
summary(model.f.nonam)
confint(model.f.nonam)
summary(imdq2m.nonam)
confint(imdq2m.nonam)
summary(imdq3m.nonam)
confint(imdq3m.nonam)
summary(imdq4m.nonam)
confint(imdq4m.nonam)
summary(imdq5m.nonam)
confint(imdq5m.nonam)
summary(imdq2f.nonam)
confint(imdq2f.nonam)
summary(imdq3f.nonam)
confint(imdq3f.nonam)
summary(imdq4f.nonam)
confint(imdq4f.nonam)
summary(imdq5f.nonam)
confint(imdq5f.nonam)
#===RUN MODEL DIAGNOSTICS INCLUDING NORMAL RESIDUAL PLOT, TEST FOR HETEROSKEDASTICITY AND PARTIAL RESIDUAL PLOT===
sink(file = NULL)
detach("package:plm", unload=TRUE)
detach("package:dplyr", unload=TRUE)
#===NORMAL RESIDUAL PLOT===
qqnorm(residuals(model.m), ylab = "Standardised Residuals", main = "Normal Q-Q Plot", xlab = "Inverse Normal")
#===HISTOGRAM OF RESIDUALS WITH OVERLYING NORMAL DISTRIBUTION===
x <- residuals(model.m)
h <- hist(x, breaks = 10, density = 10, col = "lightgray", xlab = "Residuals", main = NULL)
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col="black", lwd = 2)
#===TEST FOR HETEROSKEDASTICITY===
library(plm)
library(dplyr)
model.m <- plm(male_dsr ~ allocation + gdhi + unemp, data = data, model = "within", index=c("ltla.name", "year"), effect = "individual")
new.X <- as.matrix(select(data, allocation, gdhi, unemp))
fe <- rep(as.numeric(fixef(model.m)), each = 8)
preds <- (new.X %*% coef(model.m)) + fe
plot(preds, residuals(model.m), xlab = "Predicted Value", ylab = "Standardised Residuals", main="Standardised Residuals against Predicted Values")
#===TEST FOR LINEARITY IN RELATIONSHIP BETWEEN ALLOCATION AND MORTALITY VARIABLES===
#===FIRST USING CRPLOTS FROM CAR PACKAGE===
library(car)
model <- lm(male_dsr~pc.alloc+gdhi+unemp,data=model_data)
crPlots(model)
#===NEXT BY PLOTTING FITTED VALUES AGAINST RESIDUAL VALUES TO VISUALLY INSPECT SUGGESTION OF NON-LINEARITY===
library(broom)
model.m.values <- plm(male_dsr ~ pc.alloc+ gdhi + unemp, data = model_data, model = "within", index=c("ltla.name", "year"), effect = "twoways", subset = imdq=="5") %>% augment()
ggplot(data = model.m.values, aes(x=.fitted, y = .resid)) + geom_hline(yintercept=0, colour = "firebrick3") + geom_point() + 
geom_smooth(se = FALSE)
#===NEXT BY MANUALLY CREATING A COMPONENT PLUS RESIDUAL PLOT===
model.m <- plm(male_dsr ~ pc.alloc+ gdhi + unemp, data = model_data, model = "within", index=c("ltla.name", "year"), effect = "twoways", subset = imdq=="5")
model.m.values <- plm(male_dsr ~ pc.alloc+ gdhi + unemp, data = model_data, model = "within", index=c("ltla.name", "year"), effect = "twoways", subset = imdq=="5") %>% augment()
summary(model.m)
model.m.values <- mutate(model.m.values, y = (-0.0334491 * pc.alloc) + .resid)
plot(model.m.values$pc.alloc, model.m.values$y, xlab = "NHS allocation per head", ylab = "Component plus residual", main = "Component plus residual plot")
fit <- lm(y~pc.alloc, data = model.m.values)
abline(fit)
ggplot(model.m.values, aes(x=pc.alloc, y=y)) + geom_point(na.rm=TRUE) + geom_smooth(method = lm, se = FALSE) + labs(x = "NHS allocation per head", y = "Component plus residual") + geom_smooth(method = "loess", se = FALSE, col = "red")
#FINALLY CHECK DEGREE OF MULTICOLLINEARITY BY CALCULATING VARIANCE INFLATION FACTOR FOR VARIABLES IN POOLED MODELS USING SCALED DATA
pooled <- lm(male_dsr ~ allocation + gdhi + unemp + (allocation * imdq), data=data)
vif(pooled)
#=== SAVE HISTORY AND ENVIRONMENT ===
savehistory("lat.pca.model.RHistory")
