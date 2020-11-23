# Penn Opinion Research and Election Studies Fellowship
## GRE Project
Assessing effects of requiring GRE as application requirement on racial disparity in STEM PhD programs. Preparing dataset for analysis and examining initial trends.


          title: "Penn Opinion Research and Election Studies Fellowship"
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
          ## Checking that the 2017 specific masters and doctors data is unneccessary
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

# 1. Gender and Biological Sciences
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

# 2. Race and Biological Sciences

          ## Keeping First Year Only and Aggregating by Race
          ```{r}
          first.race.bio <- biosciences[c(5, 51:60)]


          allbio <- aggregate(first.race.bio$ft_frst_tot_all_races_v ~ first.race.bio$year, first.race.bio, sum)
          allbio$year <- allbio$`first.race.bio$year` 

          forgnbio <- aggregate(first.race.bio$ft_frst_tot_forgn_v ~ first.race.bio$year, first.race.bio, sum)

          hispbio <- aggregate(first.race.bio$ft_frst_tot_hisp_v ~ first.race.bio$year, first.race.bio, sum)

          indbio <- aggregate(first.race.bio$ft_frst_tot_indian_v ~ first.race.bio$year, first.race.bio, sum)

          asianbio <- aggregate(first.race.bio$ft_frst_tot_asian_v ~ first.race.bio$year, first.race.bio, sum)

          blackbio <- aggregate(first.race.bio$ft_frst_tot_black_v ~ first.race.bio$year, first.race.bio, sum)

          pacfbio <- aggregate(first.race.bio$ft_frst_tot_pacific_v ~ first.race.bio$year, first.race.bio, sum)

          whitebio <- aggregate(first.race.bio$ft_frst_tot_white_v ~ first.race.bio$year, first.race.bio, sum)

          multibio <- aggregate(first.race.bio$ft_frst_tot_multi_v ~ first.race.bio$year, first.race.bio, sum)

          bioagg <- cbind(allbio, forgnbio, hispbio, indbio, asianbio, blackbio, pacfbio, whitebio)
          bioagg <- bioagg[c(2,4,5,7,9,11,13,15,17)]

          ```

          ## Making Percents by Race
          ```{r}
          bioagg$foreign <- bioagg$`first.race.bio$ft_frst_tot_forgn_v` / bioagg$`first.race.bio$ft_frst_tot_all_races_v`
          bioagg$hispanic <- bioagg$`first.race.bio$ft_frst_tot_hisp_v` / bioagg$`first.race.bio$ft_frst_tot_all_races_v`
          bioagg$indian <- bioagg$`first.race.bio$ft_frst_tot_indian_v` / bioagg$`first.race.bio$ft_frst_tot_all_races_v`
          bioagg$asian <- bioagg$`first.race.bio$ft_frst_tot_asian_v` / bioagg$`first.race.bio$ft_frst_tot_all_races_v`
          bioagg$black <- bioagg$`first.race.bio$ft_frst_tot_black_v` / bioagg$`first.race.bio$ft_frst_tot_all_races_v`
          bioagg$pacific <- bioagg$`first.race.bio$ft_frst_tot_pacific_v` / bioagg$`first.race.bio$ft_frst_tot_all_races_v`
          bioagg$white <- bioagg$`first.race.bio$ft_frst_tot_white_v`/ bioagg$`first.race.bio$ft_frst_tot_all_races_v`
          ```

          ## Biological Sciences Graph of Race  
          ```{r}

          ## totals do not add to 100% because I ommitted unknown, foreign, multi
          plot(bioagg$`first.race.bio$year`, bioagg$bioagg$asian, 
               type = "l", 
               main = "First Year Biological Sciences Students by Racial Group",
               ylim = (c(0,0.5)), ylab = "Percentage of Students Enrolled",
               xlim = c(2014,2017), xaxt="n", xlab= "Year",
               col = "black")
            lines(bioagg$`first.race.bio$year`, bioagg$hispanic, col = "blue")
            lines(bioagg$`first.race.bio$year`, bioagg$indian, col = "aquamarine")
            lines(bioagg$`first.race.bio$year`, bioagg$black, col = "green")
            lines(bioagg$`first.race.bio$year`, bioagg$white, col = "coral")
            lines(bioagg$`first.race.bio$year`, bioagg$pacific, col = "red")

          # add gray lines
          abline(h = seq(0,0.5,0.05),
                 col = "gray",
                 lwd = .4)

          # add axis labels
          axis(1, at = c(2014:2017),
               tick = T)



          legend("left", legend=c( "Asian", "Hispanic", "Indian", "Black","White", "Pacific"),
                 col=c("black", "blue","aquamarine", "green", "coral", "red"), lty = 1, cex=0.8)

          ```
          ## Biological Sciences Graph of Race Among Minorities
          ```{r}
          ## totals do not add to 100% because I ommitted unknown, foreign, multi
          plot(bioagg$`first.race.bio$year`, bioagg$bioagg$asian, 
               type = "l", 
               main = "Minority First Year Biological Sciences Students",
               ylim = (c(0,0.12)), ylab = "Percentage of Students Enrolled",
               xlim = c(2014,2017), xaxt="n", xlab= "Year",
               col = "black")
            lines(bioagg$`first.race.bio$year`, bioagg$hispanic, col = "blue")
            lines(bioagg$`first.race.bio$year`, bioagg$indian, col = "aquamarine")
            lines(bioagg$`first.race.bio$year`, bioagg$black, col = "green")
            lines(bioagg$`first.race.bio$year`, bioagg$pacific, col = "red")

          # add gray lines
          abline(h = seq(0,0.12,0.01),
                 col = "gray",
                 lwd = .4)

          # add axis labels
          axis(1, at = c(2014:2017),
               tick = T)

          legend("topleft", legend=c( "Asian", "Hispanic", "Indian", "Black", "Pacific"),
                 col=c("black", "blue","aquamarine", "green", "red"), lty = 1, cex=0.8)

          ```

# 3. Gender and Clinical Medicine
          ## Keeping First Year Only  and Aggregating Clinical Medicine by Gender
          ```{r}
          first.gen.med <- clinicalmed[c(5, 51, 61, 71)]

          allmedgen <- aggregate(first.gen.med$ft_frst_tot_all_races_v ~ first.gen.med$year, clinicalmed , sum)
          wommed <- aggregate(first.gen.med$ft_frst_wmen_all_races_v ~ first.gen.med$year, clinicalmed, sum)
          menmed <- aggregate(first.gen.med$ft_frst_men_all_races_v ~ first.gen.med$year, clinicalmed, sum)

          medgenagg <- cbind(allmedgen, wommed, menmed)
          medgenagg <- medgenagg[c(1,2,4,6)]

          ```
          ## Making Percents by Gender
          ```{r}
          medgenagg$women <-medgenagg$`first.gen.med$ft_frst_wmen_all_races_v` / medgenagg$`first.gen.med$ft_frst_tot_all_races_v`


          medgenagg$men <- medgenagg$`first.gen.med$ft_frst_men_all_races_v`/ medgenagg$`first.gen.med$ft_frst_tot_all_races_v`

          ```

          ## Clinical Medicine Graph of Gender
          ```{r}
          plot(medgenagg$`first.gen.med$year`, medgenagg$women, 
               type = "l", 
               main = "First Year Clinical Medicine Students by Gender",
               ylim = (c(0.2,0.8)), ylab = "Percentage of Students Enrolled",
               xlim = c(2014,2017), xaxt="n", xlab= "Year",
               col = "red")
            lines(medgenagg$`first.gen.med$year`, medgenagg$men , col = "blue")

          # add gray lines
          abline(h = seq(0.2,0.8,0.05),
                 col = "gray",
                 lwd = .4)

          # add axis labels
          axis(1, at = c(2014:2017),
               tick = T)

          legend("left", legend=c("Men", "Women"),
                 col=c( "blue","red"), lty = 1, cex=0.8)

          ```

          ## Keeping First Year Only and Aggregating by Race
          ```{r}
          first.race.med <- clinicalmed[c(5, 51:60)]




          allmed <- aggregate(first.race.med$ft_frst_tot_all_races_v ~ first.race.med$year, first.race.med , sum)


          forgnmed <- aggregate(first.race.med$ft_frst_tot_forgn_v ~ first.race.med$year, first.race.med, sum)

          hispmed <- aggregate(first.race.med$ft_frst_tot_hisp_v ~ first.race.med$year, first.race.med, sum)

          indmed <- aggregate(first.race.med$ft_frst_tot_indian_v ~ first.race.med$year, first.race.med, sum)

          asianmed <- aggregate(first.race.med$ft_frst_tot_asian_v ~ first.race.med$year, first.race.med, sum)

          blackmed <- aggregate(first.race.med$ft_frst_tot_black_v ~ first.race.med$year, first.race.med, sum)

          pacfmed <- aggregate(first.race.med$ft_frst_tot_pacific_v ~ first.race.med$year, first.race.med, sum)

          whitemed <- aggregate(first.race.med$ft_frst_tot_white_v ~ first.race.med$year, first.race.med, sum)

          multimed <- aggregate(first.race.med$ft_frst_tot_multi_v ~ first.race.med$year, first.race.med, sum)

          medagg <- cbind(allmed, forgnmed, hispmed, indmed, asianmed, blackmed, pacfmed, whitemed)


          medagg <- medagg[c(1,2,4,6,8,10,12,14,16)]



          ```


          ## Making Percents by Race
          ```{r}
          medagg$foreign <- medagg$`first.race.med$ft_frst_tot_forgn_v` / medagg$`first.race.med$ft_frst_tot_all_races_v`
          medagg$hispanic <- medagg$`first.race.med$ft_frst_tot_hisp_v` /medagg$`first.race.med$ft_frst_tot_all_races_v`
          medagg$indian <- medagg$`first.race.med$ft_frst_tot_indian_v` / medagg$`first.race.med$ft_frst_tot_all_races_v`
          medagg$asian <- medagg$`first.race.med$ft_frst_tot_asian_v` / medagg$`first.race.med$ft_frst_tot_all_races_v`
          medagg$black <- medagg$`first.race.med$ft_frst_tot_black_v` /medagg$`first.race.med$ft_frst_tot_all_races_v`
          medagg$pacific <- medagg$`first.race.med$ft_frst_tot_pacific_v` /medagg$`first.race.med$ft_frst_tot_all_races_v`
          medagg$white <- medagg$`first.race.med$ft_frst_tot_white_v` / medagg$`first.race.med$ft_frst_tot_all_races_v`

          ```


          ## Clinical Medicine Graph of Race
          ```{r}
          plot(medagg$`first.race.med$year`  , medagg$asian , 
               type = "l", 
               main = "First Year Clinical Medicine Students by Racial Group",
               ylim = (c(0,0.6)), ylab = "Percentage of Students Enrolled",
               xlim = c(2014,2017), xaxt="n", xlab= "Year",
               col = "black")
            lines(medagg$`first.race.med$year`  , medagg$hispanic, col = "blue")
            lines(medagg$`first.race.med$year`  , medagg$indian , col = "aquamarine")
            lines(medagg$`first.race.med$year`  , medagg$black  , col = "green")
            lines(medagg$`first.race.med$year`  , medagg$white , col = "coral")
            lines(medagg$`first.race.med$year`  , medagg$pacific , col = "red")

          # add gray lines
          abline(h = seq(0,0.6,0.05),
                 col = "gray",
                 lwd = .4)

          # add axis labels
          axis(1, at = c(2014:2017),
               tick = T

          legend("left", legend=c( "Asian", "Hispanic", "Indian", "Black","White", "Pacific"),
                 col=c("black", "blue","aquamarine", "green", "coral", "red"), lty = 1, cex=0.8)


          ```

          ## Clinical Medicine Graph of Race Among Minorities Only
          ```{r}
          plot(medagg$`first.race.med$year`  , medagg$asian , 
               type = "l", 
               main = "Minority First Year Clinical Medicine Students",
               ylim = (c(0,0.125)), ylab = "Percentage of Students Enrolled",
               xlim = c(2014,2017), xaxt="n", xlab= "Year",
               col = "black")
            lines(medagg$`first.race.med$year`  , medagg$hispanic, col = "blue")
            lines(medagg$`first.race.med$year`  , medagg$indian , col = "aquamarine")
            lines(medagg$`first.race.med$year`  , medagg$black  , col = "green")
            lines(medagg$`first.race.med$year`  , medagg$pacific , col = "red")

          # add gray lines
          abline(h = seq(0,0.125,0.025),
                 col = "gray",
                 lwd = .4)

          # add axis labels
          axis(1, at = c(2014:2017),
               tick = T)



          legend("topleft", legend=c( "Asian", "Hispanic", "Indian", "Black", "Pacific"),
                 col=c("black", "blue","aquamarine", "green", "red"), lty = 1, cex=0.8)

          ```


          ```{r}
          DroppingGRERequirementDataset_1_ <- read_excel("DroppingGRERequirementDataset (1).xlsx")
          DroppingGRERequirementDataset_1_$Institution_Name <- DroppingGRERequirementDataset_1_$Institution
          sum(table(DroppingGRERequirementDataset_1_$Institution_Name))

          inner <- merge(gss, DroppingGRERequirementDataset_1_, by = "Institution_Name", all = F)
          onlygss <- merge(gss, DroppingGRERequirementDataset_1_, by = "Institution_Name", all.x = T)

          school <- unique(inner$Institution_Name)
          count(unique(inner$Institution_Name))
          count(unique(onlygss$Institution_Name))

          r <- unique(inner$Institution_Name)
          print(r)

          for (i in r) {
          onlygss1 <- subset(onlygss, onlygss$Institution_Name != i)
          }
          View(onlygss1)

          onlygss3 <- unite(onlygss1, "InstitutionName_gsscode", c("Institution_Name", "gss_code"), sep = "-")

          onlygss2 <- table(onlygss3$InstitutionName_gsscode)
          onlygss2 <- as.data.frame(onlygss2)
          onlygss2 <- separate(onlygss2, Var1, c("Institution_Name", "gss_code"), sep = "-")


          write.table(onlygss2, file="additionaldata.csv",sep=",",row.names=F)


          ```

          ```{r}

          inner_googlesheet <- subset(inner,inner$year == 2017)
          deduped.data <- unique(inner_googlesheet[ , 1:2 ] )

          ```

