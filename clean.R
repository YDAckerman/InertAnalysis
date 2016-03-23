######################################################################
######################################################################
## do all basic cleaning and massaging for inert analysis

library(plyr); library(dplyr)

source("load.R")

## correct column names
corrections <- list(c("%", "Percent"),
                    c("[\\(\\)]", ""),
                    c("\\\"", ""))
for (correction in corrections){
    colnames(d) <- gsub(correction[1],
                        correction[2],
                        colnames(d))
}

## create low, med, high percentage columns
p <- ldply(d$Percentage.Std, inertFuns$fixPercent)
d[,c("Low.Percent", "Med.Percent", "High.Percent")] <- p

rm(corrections, p)
