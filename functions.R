######################################################################
######################################################################
## functions for the InertAnalysis project

inertFuns <- new.env()

inertFuns$fixPercent <- function(x){
    if(!grepl("%", x)){
        return(
            data.frame(
                Low.Percent = NA,
                Med.Percent = NA,
                High.Percent = NA)
            )
    }
    x <- gsub("%", "", x)
    if(grepl("-", x)){
        x <- as.numeric(unlist(strsplit(x, "-")))
        return(
            data.frame(Low.Percent = x[1],
                 Med.Percent = mean(x),
                 High.Percent = x[2])
               )
    }
    if(grepl("<", x)){
        x <- as.numeric(gsub("<", "", x))
        return(
            data.frame(Low.Percent = 0,
                 Med.Percent = x/2,
                 High.Percent = x)
            )
    }
    if(grepl(">", x)){
        x <- as.numeric(gsub(">", "", x))
        return(
            data.frame(Low.Percent = x,
                 Med.Percent = x,
                 High.Percent = x)
            )
    } else {
        x <- as.numeric(x)
        return(
            data.frame(Low.Percent = x,
                 Med.Percent = x,
                 High.Percent = x)
            )
    }
} 

inertFuns$trim <- function (x) gsub("^\\s+|\\s+$", "", x)

inertFuns$checkCASNum <- function(x){
    vals <- unlist(strsplit(x, "-"))
    check <- as.numeric(unlist(strsplit(vals[1:2], "")))
    sum <- as.numeric(vals[3])
    sum == sum((length(check):1) * check) %% 10
}
