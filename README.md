# GRE.project
Assessing effects of requiring GRE as application requirement on racial disparity in STEM PhD programs. Preparing dataset for analysis and examining initial trends.
title: "Pores"
author: "Madelyn Mathai"
date: "11/2/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set
setwd("~/Desktop/PORES FELLOWSHIP/pores")

library(rio)
library(data.table)
library(haven)
library(knitr)
library(purrr)
library(dplyr)
library(gtools)
library(plyr)
library(tidyr)
library(maps)
library(mapdata)
library(tidyverse) 
library(rgdal)
library(leaflet)
library(readr)
library(dplyr)
library(tidyr)
library(magick)
library(ggplot2)
library(shiny)
library(reshape2)
library(readxl)
library(zoo)
```

## Loading the data
```{r}
gss2014 <- read_excel("gss2014_Code.xlsx")
gss2015 <- read_excel("gss2015_Code.xlsx")
gss2016 <- read_excel("gss2016_Code.xlsx")
gss2017 <- read_excel("gss2017_Code.xlsx")
```
## Cleaning the data
```{r}
pre2017 <- rbind(gss2014,gss2015,gss2016)

# Only interested in full time right now
pre2017c <- pre2017[-c(21:56)]

# Removing variables whose columns is all NA
pre2017c <- pre2017c[,colSums(is.na(pre2017c))<nrow(pre2017c)]
```
# Checking that the 2017 specific masters and doctors data is unneccessary
```{r}
all(gss2017$ft_men_black_v == gss2017$ma_ft_men_black_v + gss2017$dr_ft_men_black_v)
# It returned true so I can drop these variables

clean2017 <- gss2017[-c(21:50, 111:290)]



```



## Appending the data
```{r}
# Renaming pre2017 so that they match up with 2017 variables
colnames(pre2017c)[colnames(pre2017c)=="ft_tot_unknown_v"] <- "ft_tot_unk_v"
colnames(pre2017c)[colnames(pre2017c)=="ft_men_unknown_v"] <- "ft_men_unk_v"
colnames(pre2017c)[colnames(pre2017c)=="ft_wmen_unknown_v"] <- "ft_wmen_unk_v"
colnames(pre2017c)[colnames(pre2017c)=="ft_frst_tot_unknown_v"] <- "ft_frst_tot_unk_v"
colnames(pre2017c)[colnames(pre2017c)=="ft_tot_multi_non_hisp_v"] <- "ft_tot_multi_v"
colnames(pre2017c)[colnames(pre2017c)== "ft_men_multi_non_hisp_v"] <- "ft_men_multi_v"
colnames(pre2017c)[colnames(pre2017c)== "ft_wmen_multi_non_hisp_v"] <- "ft_wmen_multi_v"
colnames(pre2017c)[colnames(pre2017c)=="ft_frst_tot_multi_non_hisp_v"] <- "ft_frst_tot_multi_v"
colnames(pre2017c)[colnames(pre2017c)== "ft_frst_men_multi_non_hisp_v"] <- "ft_frst_men_multi_v"
colnames(pre2017c)[colnames(pre2017c)== "ft_frst_men_unknown_v"] <- "ft_frst_men_unk_v"
colnames(pre2017c)[colnames(pre2017c)== "ft_frst_wmen_multi_non_hisp_v"] <- "ft_frst_wmen_multi_v"
colnames(pre2017c)[colnames(pre2017c)== "ft_frst_wmen_unknown_v"] <- "ft_frst_wmen_unk_v"

gss <- rbind(pre2017c, clean2017)
```

```{r}

gss$ID <- unite(gss, "ID", c("gss_code","year"), sep = "-")
```


## Subsetting into programs we care about
```{r}
biosciences <- subset(gss, gss$gss_code > 599 & gss$gss_code < 700 )
clinicalmed <- subset(gss, gss$gss_code > 699 & gss$gss_code < 800 )
```




# 1. GENDER AND BIO SCIENCES
## Keeping First Year Only and Aggregating Biomedical Science by Gender
```{r}
first.race.biogen <- biosciences[c(5, 51, 61, 71)]

allbio <- aggregate(first.race.biogen$ft_frst_tot_all_races_v ~ first.race.biogen$year, biosciences, sum)
wombio <- aggregate(first.race.biogen$ft_frst_wmen_all_races_v ~ first.race.biogen$year, biosciences, sum)
menbio <- aggregate(first.race.biogen$ft_frst_men_all_races_v ~ first.race.biogen$year, biosciences, sum)

biogenagg <- cbind(allbio, wombio, menbio)

biogenagg <- biogenagg[c(1,2,4,6)]

```

## Making percents by Gender
```{r}
biogenagg$women <- biogenagg$`first.race.biogen$ft_frst_wmen_all_races_v` / biogenagg$`first.race.biogen$ft_frst_tot_all_races_v`
biogenagg$men <- biogenagg$`first.race.biogen$ft_frst_men_all_races_v` / biogenagg$`first.race.biogen$ft_frst_tot_all_races_v`

```

## Biological Sciences Graph of Gender
```{r}
plot(biogenagg$`first.race.biogen$year`, biogenagg$women, 
     type = "l", 
     main = "First Year Biological Sciences Students by  Gender",
     ylim = (c(0.35,0.6)), ylab = "Percentage of Students Enrolled",
     xlim = c(2014,2017), xaxt="n", xlab= "Year",
     col = "red")
lines(biogenagg$`first.race.biogen$year`, biogenagg$men , col = "blue")

# add gray lines
abline(h = seq(0.35,0.6,0.05),
       col = "gray",
       lwd = .4)

# add axis labels
axis(1, at = c(2014:2017),
     tick = T)



legend("bottomleft", legend=c( "Men",  "Women"),
       col=c( "blue","red"), lty = 1, cex=0.8)



```
