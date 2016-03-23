######################################################################
######################################################################
## do all calculations for intert analysis

library(RPostgreSQL)
library(DBI)
library(plyr); library(dplyr)
library(foreach)
library(lubridate)
library(reshape2)

setwd("/home/jackerma/Documents/ZhangLab/R/InertAnalysis/")
source("functions.R")
source("clean.R")

## postgres connection
con <- src_postgres(dbname="california",
                  host="ziram.lawr.ucdavis.edu",
                   user="jonathan"
                   ## ,options="-c search_path=bee"
                    )

## retrieve yearly total usage for each product
sql <- "SELECT
               product_name, sum(lbs_prd_used) AS total_lbs, year
        FROM (
              SELECT
                     product_name, lbs_prd_used,
                     date_part('year', applic_dt) AS year
              FROM
                     pur.udc INNER JOIN pur.product
              ON
                     pur.udc.prodno = pur.product.prodno
              WHERE
                     date_part('year', applic_dt) = 2011 OR
                     date_part('year', applic_dt) = 2012 OR
                     date_part('year', applic_dt) = 2013
              ) as yearUse
        GROUP BY product_name, year"

productYearTotals <- collect(tbl(con, dplyr::sql(sql)))

## calculations:

## 1) Total lbs of all the listed products used in each year from 2011-2013,
## and what % of total lbs of ALL products in PUR they represent (should
## be about 90% each year).

yearTotals <- productYearTotals %>%
    group_by(year) %>%
    filter(!is.na(total_lbs)) %>%
    dplyr::summarise(total = sum(total_lbs))

d <- left_join(d, productYearTotals, by = "product_name")
d <- left_join(d, yearTotals, by = "year")

values <- d %>%
    mutate(percentOfTotal = total_lbs / total) %>%
    select(product_name, year, total_lbs, percentOfTotal)

## 2) The "% AI (PUR)" column tells us which rows are AIs and which are interts.
## For each year, generate a single sum value for all AIs on the list (and
## % of total lbs of all products listed), and a single value for all inerts
## with percentage values give (low, mid, high).

AIvalues <- d %>%
    filter(!is.na(PercentAI.PUR.or.inert) &
           PercentAI.PUR.or.inert != "inert") %>%
    dplyr::mutate(lbs_ai = total_lbs *
                  (as.numeric(PercentAI.PUR.or.inert) / 100)) %>%
    group_by(year, total) %>%
    dplyr::summarise(total_lbs_from_AIs = sum(lbs_ai, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::mutate(percent_from_AIs = 100 * (total_lbs_from_AIs / total))

InertValues <- d %>%
    filter(PercentAI.PUR.or.inert == "inert" &
           !is.na(total_lbs)) %>%
    dplyr::mutate(low_lbs_inert = total_lbs * Low.Percent / 100,
                  med_lbs_inert = total_lbs * Med.Percent / 100,
                  high_lbs_inert = total_lbs * High.Percent / 100) %>%
    group_by(year, total) %>%
    dplyr::summarise(
        low_total_lbs_from_inerts = sum(low_lbs_inert, na.rm = TRUE),
        med_total_lbs_from_inerts = sum(med_lbs_inert, na.rm = TRUE),
        high_total_lbs_from_inerts = sum(high_lbs_inert, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::mutate(
        low_percent_from_inerts =  100 * (low_total_lbs_from_inerts / total),
        med_percent_from_inerts =  100 * (med_total_lbs_from_inerts / total),
        high_percent_from_inerts = 100 * ( high_total_lbs_from_inerts / total)
        )

## 3) Number of products and lbs for each unique "chemical name (std)" entry
## for each year 2011-2013. The generated list will be cleaned up manually

chemValues <- d %>%
    group_by(year, Chemical.name.Std) %>%
    dplyr::summarise(total_lbs = sum(total_lbs, na.rm = TRUE),
                     number_of_products = length(unique(product_name))) %>%
    ungroup()

## 4) Number of unique product names and total lbs for all entries with
## "Yes" in the "Low MW Aromatic hydrocarbon solvents" column

numUniqueProducts <- length(unique(d$product_name))

totalLbsLowMWAromaticHCSolvents <- d %>%
    filter(Low.MW.Aromatic.hydrocarbon.solvents == "Yes") %>%
    group_by(year) %>%
    dplyr::summarise(total_lbs = sum(total_lbs, na.rm = TRUE))

    
## 5) Number of rows, number of unique product names and total lbs
## of all inerts for each of the entries in "Regulatory lists"
## column (which should all be separated by semi-colons)

regulatoryItems <- unique(na.omit(unlist(strsplit(d$Regulatory.lists,";"))))

regulatoryItems <- ldply(regulatoryItems, function(x){
   
    x <- inertFuns$trim(x)
    counts <- d %>%
        filter(grepl(x, Regulatory.lists)) %>%
            group_by(year) %>%
                dplyr::summarise(numRows = n(),
                                 numProds = length(unique(product_name)))
    
    total_lbs <- d %>%
    filter(PercentAI.PUR.or.inert == "inert" &
           grepl(x, Regulatory.lists)) %>%
    dplyr::mutate(low_lbs_inert = total_lbs * Low.Percent / 100,
                  med_lbs_inert = total_lbs * Med.Percent / 100,
                  high_lbs_inert = total_lbs * High.Percent / 100) %>%
    group_by(year) %>%
    dplyr::summarise(
        low_total_lbs_from_inerts = sum(low_lbs_inert, na.rm = TRUE),
        med_total_lbs_from_inerts = sum(med_lbs_inert, na.rm = TRUE),
        high_total_lbs_from_inerts = sum(high_lbs_inert, na.rm = TRUE))
    
    if(empty(total_lbs)){
        total_lbs <- data.frame(year = 2011:2013,
                                low_total_lbs_from_inerts = NA,
                                med_total_lbs_from_inerts = NA,
                                high_total_lbs_from_inerts = NA
                                )
    }
    
    tmp_df <- merge(counts, total_lbs, by = "year")
    tmp_df$RegulatoryItem <- x
    tmp_df
    
})

                       
## 6) check all the CAS numbers
strings <- c(",", "and", "&", "or", "AND", ";", "\\[sic", "\\]")
casNums <- d$CAS.number.Std
for (string in strings){
    casNums <- unique(unlist(strsplit(casNums, string)))
}

casNums <- casNums[grepl("[0-9]", casNums)]
casNums <- inertFuns$trim(casNums)

bools <- sapply(casNums, inertFuns$checkCASNum)

## the only ones that don't check out are 64741-88-9 and 15708-07-7

