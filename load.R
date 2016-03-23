############################################################################
############################################################################
## load all data

library(openxlsx)
options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx8000m")

d <- read.xlsx(paste0("~/Dropbox/ZhangLabData/ExcelData/",
                      "Inert-Data-Mar2016-ForYoni.xlsx"),
               sheet = 1)

