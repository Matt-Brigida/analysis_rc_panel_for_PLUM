## to read sas7bdat file --------

library(haven)
library(plm)

call_all <- read_sas("../../Data/call_all.sas7bdat")

### removing duplicate rows-----
## create one index
theindex <- paste0(call_all$DATE, "_", call_all$RSSD9001)

call_all <- cbind(theindex, call_all)

call_all <- subset(call_all, !duplicated(call_all$theindex))

## MDI data -----

mdi <- read.csv("../MDIs.csv", header = TRUE, stringsAsFactors = FALSE)

### create MDI indicator-----

mdi_ind <- 0

for (i in 1:dim(call_all)[1]){
    mdi_ind[i] <- ifelse(call_all$RSSD9001[i] %in% mdi$IDRSSD, 1, 0)
}

call_all <- cbind(call_all, mdi_ind)

rm(mdi_ind)


## convert to pdata.frame for panel data calculations------

call_all <- pdata.frame(call_all, index = c("RSSD9001", "DATE"), drop.index=TRUE, row.names=TRUE)

saveRDS(call_all, "callall_pdata.rds")

