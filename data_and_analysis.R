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



### analysis------

### data already in data set (call_all)

## with a 0 at the end is " period t+1 LAG variables from t+0 variables"

## SBLTOT_TA: total small business loans as a percent of total assets
## SBLTOT_TA0: lagged

## ROA0
## ROE0

## LNTA: log of total assets
## LNTA0: lagged

## TLTA: total loans over total assets (what type of loans???)


## BADBKFC: bad bank times financial crisis ind
## BADBKPC: bad bank times post crisis ind

fe <- plm(SBLTOT0 ~ TLTA + BADBANK + BADBKFC + BADBKPC + ROA0 + TE0 + DENOVO + LIQTA0 + Y1995 + Y1996 + Y1997 + Y1998 + Y1999 + Y2000 + Y2001 + Y2002 + Y2003 + Y2004 + Y2005 + Y2006 + Y2007 + Y2008 + Y2009 + Y2010 + Y2011 + Y2012 + Y2013 + mdi_ind*1 + mdi_ind*TE0, data = call_all, model = "within")

fixedeffects <- fixef(fe)

## include Y2014, dummy variable trap?? What years does the data cover??

re <- plm(SBLTOT0 ~ TLTA + BADBANK + BADBKFC + BADBKPC + ROA0 + TE0 + DENOVO + LIQTA0 + Y1995 + Y1996 + Y1997 + Y1998 + Y1999 + Y2000 + Y2001 + Y2002 + Y2003 + Y2004 + Y2005 + Y2006 + Y2007 + Y2008 + Y2009 + Y2010 + Y2011 + Y2012 + Y2013 + mdi_ind*1 + mdi_ind*TE0, data = call_all, model = "random")  ## doesnt work

po <- plm(SBLTOT0 ~ TLTA + BADBANK + BADBKFC + BADBKPC + ROA0 + TE0 + DENOVO + LIQTA0 + Y1995 + Y1996 + Y1997 + Y1998 + Y1999 + Y2000 + Y2001 + Y2002 + Y2003 + Y2004 + Y2005 + Y2006 + Y2007 + Y2008 + Y2009 + Y2010 + Y2011 + Y2012 + Y2013 + mdi_ind*1 + mdi_ind*TE0, data = call_all, model = "pooling")  ## coefficient on mdi_ind is negative

be <- plm(SBLTOT0 ~ TLTA + BADBANK + BADBKFC + BADBKPC + ROA0 + TE0 + DENOVO + LIQTA0 + Y1995 + Y1996 + Y1997 + Y1998 + Y1999 + Y2000 + Y2001 + Y2002 + Y2003 + Y2004 + Y2005 + Y2006 + Y2007 + Y2008 + Y2009 + Y2010 + Y2011 + Y2012 + Y2013 + mdi_ind*1 + mdi_ind*TE0, data = call_all, model = "between")  ## coefficient on mdi_ind is negative

fd <- plm(SBLTOT0 ~ TLTA + BADBANK + BADBKFC + BADBKPC + ROA0 + TE0 + DENOVO + LIQTA0 + Y1995 + Y1996 + Y1997 + Y1998 + Y1999 + Y2000 + Y2001 + Y2002 + Y2003 + Y2004 + Y2005 + Y2006 + Y2007 + Y2008 + Y2009 + Y2010 + Y2011 + Y2012 + Y2013 + mdi_ind*1 + mdi_ind*TE0, data = call_all, model = "fd")  ## really no reason to do this

lm <- lm(SBLTOT0 ~ TLTA + BADBANK + BADBKFC + BADBKPC + ROA0 + TE0 + DENOVO + LIQTA0 + Y1995 + Y1996 + Y1997 + Y1998 + Y1999 + Y2000 + Y2001 + Y2002 + Y2003 + Y2004 + Y2005 + Y2006 + Y2007 + Y2008 + Y2009 + Y2010 + Y2011 + Y2012 + Y2013 + mdi_ind*1 + mdi_ind*TE0, data = call_all)  ## regular regression





















### do i need to do the below---is it already in the dataset???
## use to calcualte the change by each IDRSSD-----

library(data.table)
library(quantmod)
call_all <- data.table(call_all)
call_all[, new_column_name := c(NA, Delt(column_name_changing)), by = RSSD9001]

## but then we have to shift up to match the percent change (at end of year) with variables at beninning of year----


