setwd("c://users/jo122989/desktop/phst2/ampaper/data")
load("fund_data_manipulation.RData")
library(dplyr)
library(stringr)
library(tidyr)
ls()
 test.imdq <- rename(alloc.popn.imdq, c("funding.2007" = "alloc2007", "funding.2008" = "alloc2008", "funding.2009" = "alloc2009", "funding.2010" = "alloc2010", "funding.2011" = "alloc2011", "funding.2012" = "alloc2012", "funding.2013" = "alloc2013", "funding.2014" = "alloc2014"))
library(plyr)
 test.imdq <- rename(alloc.popn.imdq, c("funding.2007" = "alloc2007", "funding.2008" = "alloc2008", "funding.2009" = "alloc2009", "funding.2010" = "alloc2010", "funding.2011" = "alloc2011", "funding.2012" = "alloc2012", "funding.2013" = "alloc2013", "funding.2014" = "alloc2014"))
test.imdq %>% names()
sapply(test.imdq, class)
test.imdq$imdq <- as.factor(test.imdq$imdq)
sapply(test.imdq, class)
data_long <- gather(test.imdq, year, allocation, alloc2007:alloc2014)
data_long %>% names()
data_long %>% head()
data_long$year <- as.factor(data_long$year)
sapply(data_long, class)
levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
data_long
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
data_long %>% tail()
grouped_data <- data_long %>% group_by(imdq) %>% summarise(imdq_pop = sum(ltla.popn.2011))
grouped_data %>% names()
grouped_data
rm(grouped_data)
data_long%imdq <- as.character(data_long$imdq)
data_long$imdq <- is.character(data_long$imdq)
grouped_data <- data_long %>% group_by(imdq) %>% summarise(imdq_pop = sum(ltla.popn.2011))
grouped_data %>% names()
rm(grouped_data)
sapply(data_long, class)
data_long$imdq <- is.numeric(data_long$imdq)
data_long
rm(data_long)
 test.imdq <- rename(alloc.popn.imdq, c("funding.2007" = "alloc2007", "funding.2008" = "alloc2008", "funding.2009" = "alloc2009", "funding.2010" = "alloc2010", "funding.2011" = "alloc2011", "funding.2012" = "alloc2012", "funding.2013" = "alloc2013", "funding.2014" = "alloc2014"))
data_long <- gather(test.imdq, year, allocation, alloc2007:alloc2014)
levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
levels(data_long$year)[levels(data_long$year)=="alloc2014"] <- "2014"
data_long
data_long$year <- as.factor(data_long$year)
levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
levels(data_long$year)[levels(data_long$year)=="alloc2014"] <- "2014"
data_long
sapply(data_long, class)
grouped_data <- data_long %>% group_by(imdq) %>% summarise(imdq_pop = sum(ltla.popn.2011))
grouped_data
rm(grouped_data)
sapply(data_long, class)
grouped_data <- data_long %>% group_by(imdq)
grouped_data
imdq_quin_pop <- mutate(grouped_data, imdq_popn = sum(ltla.popn.2011))
imdq_quin_pop
imdq_quin_pop %>% tail()
rm(imdq_quin_pop)
grouped_data$imdq <- is.character(grouped_data$imdq)
sapply(grouped_data, class)
rm(grouped_data)
 test.imdq <- rename(alloc.popn.imdq, c("funding.2007" = "alloc2007", "funding.2008" = "alloc2008", "funding.2009" = "alloc2009", "funding.2010" = "alloc2010", "funding.2011" = "alloc2011", "funding.2012" = "alloc2012", "funding.2013" = "alloc2013", "funding.2014" = "alloc2014"))
> data_long <- gather(test.imdq, year, allocation, alloc2007:alloc2014)
> levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
> levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
> levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
> levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
> levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
> levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
> levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
> levels(data_long$year)[levels(data_long$year)=="alloc2014"] <- "2014"
> test.imdq <- rename(alloc.popn.imdq, c("funding.2007" = "alloc2007", "funding.2008" = "alloc2008", "funding.2009" = "alloc2009", "funding.2010" = "alloc2010", "funding.2011" = "alloc2011", "funding.2012" = "alloc2012", "funding.2013" = "alloc2013", "funding.2014" = "alloc2014"))
> data_long <- gather(test.imdq, year, allocation, alloc2007:alloc2014)
> levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
> levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
> levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
> levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
> levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
> levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
> levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
> levels(data_long$year)[levels(data_long$year)=="alloc2014"] <- "2014"
test.imdq <- rename(alloc.popn.imdq, c("funding.2007" = "alloc2007", "funding.2008" = "alloc2008", "funding.2009" = "alloc2009", "funding.2010" = "alloc2010", "funding.2011" = "alloc2011", "funding.2012" = "alloc2012", "funding.2013" = "alloc2013", "funding.2014" = "alloc2014"))
data_long <- gather(test.imdq, year, allocation, alloc2007:alloc2014)
levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
levels(data_long$year)[levels(data_long$year)=="alloc2014"] <- "2014"
data_long
data_long$year <- as.factor(data_long$year)
levels(data_long$year)[levels(data_long$year)=="alloc2007"] <- "2007"
levels(data_long$year)[levels(data_long$year)=="alloc2008"] <- "2008"
levels(data_long$year)[levels(data_long$year)=="alloc2009"] <- "2009"
levels(data_long$year)[levels(data_long$year)=="alloc2010"] <- "2010"
levels(data_long$year)[levels(data_long$year)=="alloc2011"] <- "2011"
levels(data_long$year)[levels(data_long$year)=="alloc2012"] <- "2012"
levels(data_long$year)[levels(data_long$year)=="alloc2013"] <- "2013"
levels(data_long$year)[levels(data_long$year)=="alloc2014"] <- "2014"
data_long
levels(data_long$imdq)[levels(data_long$imdq)=="1"] <- "one"
data_long
data_long$imdq <- as.factor(data_long$imdq)
data_long
levels(data_long$imdq)[levels(data_long$imdq)=="1"] <- "one"
data_long
levels(data_long$imdq)[levels(data_long$imdq)=="2"] <- "two"
levels(data_long$imdq)[levels(data_long$imdq)=="3"] <- "three"
levels(data_long$imdq)[levels(data_long$imdq)=="4"] <- "four"
levels(data_long$imdq)[levels(data_long$imdq)=="5"] <- "five"
data_long
quin.popn <- data_long %>% group_by(imdq) %>% summarise(quin.popn = sum(ltla.popn.2011))
quin.popn
test_long <- data_long
test_long
test_long$imdq <- is.character(test_long$imdq)
test_long
rm(test_long)
data_long
test_long <- data_long %>% group_by(imdq) %>% summarize(quin_popn = sum(ltla.popn.2011))
test_long
rm(test_long)
test_long <- data_long %>% group_by(imdq) %>% summarize(quin_popn = sum(ltla.popn.2011)) %>% stringsAsFactors = FALSE
test_long <- data_long %>% group_by(data_long, imdq) %>% summarise(quin_popn = sum(ltla.popn.2011))
data_long
test_long <- data_long %>% group_by(test_long, imdq) %>% summarise(quin_popn = sum(ltla.popn.2011))
detach(package:plyr)
test_long <- data_long %>% group_by(imdq) %>% summarise(quin_popn = sum(ltla.popn.2011))
test_long
imdq_popns <- left_join(data_long, test_long, by = "imdq")
imdq_popns
imdq_wts <- mutate(imdq_popns, weight = (ltla.popn.2011/quin_popn))
imdq_wts
imdq_alloc_wts <- mutate(imdq_wts, alloc_wt = (allocation * weight))
imdq_alloc_wts
grouped_imdq_alloc <- group_by(imdq_alloc_wts, imdq)
weighted_imdq_alloc <- summarise(grouped_imdq_alloc, wtd_alloc = sum(alloc_wt))
weighted_imdq_alloc
rm(weighted_imdq_alloc, grouped_imdq_alloc)
grouped_imdq_alloc <- imdq_alloc_wts %>% group_by(imdq, year)
wt_imdq_alloc <- summarise(grouped_imdq_alloc, wtd_alloc = sum(alloc_wt))
wt_imdq_alloc
library(ggplot2)
plot(wt_imdq_alloc, type="o", col="blue")
write.csv(wt_imdq_alloc, "wt_imdq_alloc.csv")
ggplot(data = wt_imdq_alloc, aes(x=year, y=wtd_alloc)) + geom_line(aes(colour=variable))
ggplot(data = wt_imdq_alloc, aes(x=year, y=wtd_alloc)) + geom_line(aes(colour=imdq))
plot(wt_imdq_alloc, type ="o", col = "red", xlab = "year", ylab = "allocation")
ggplot(data = wt_imdq_alloc, aes(x=year, y=allocation, group = imdq, colour = imdq)) + geom_line() + geom_point(size = 4, shape = 21, fill = "white")
ggplot(data = wt_imdq_alloc, aes(x=year, y=wtd_alloc, group = imdq, colour = imdq) + geom_line() + geom_point(size = 4, shape = 21, fill = "white")
)
ggplot(data = wt_imdq_alloc, aes(x=year, y=wtd_alloc, group = imdq, colour = imdq)) + geom_line() + geom_point(size = 4, shape = 21, fill = "white")
savehistory("long_quintile_plots.csv")
getwd()
savehistory("long_quintile_plots.RHistory")
