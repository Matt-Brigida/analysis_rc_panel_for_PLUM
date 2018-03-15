### analysis------

library(plm)

call_all <- readRDS("./callall_pdata.rds")

### data already in data set (call_all)

## with a 0 at the end is " period t+1 LAG variables from t+0 variables"

## SBLTOT_TA: total small business loans as a percent of total assets
## SBLTOT_TA0: lagged

       ## CSBLTOT = MIN(2, SBLTOT   / SBLTOT0 - 1);
       ##  CBLTOT = MIN(2,  BLTOT   /  BLTOT0 - 1);


## ROA0
## ROE0

## LNTA: log of total assets
## LNTA0: lagged

## TLTA: total loans over total assets (what type of loans???)


## BADBKFC: bad bank times financial crisis ind
## BADBKPC: bad bank times post crisis ind



## using variables from Table 1-----

## model should be one from Table 6-----

## fe4 works!!!  The only questionable variable is the Loans (SBLTOT_TA), but doesnt affect other coefficients and signs

fe4 <- plm(CSBLTOT ~  mdi_ind*EQTA0 + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")

fixedeffects4 <- fixef(fe4)

## Coefficients:
##                 Estimate Std. Error  t-value  Pr(>|t|)    
## EQTA0          1.4542334  0.0192169  75.6747 < 2.2e-16 ***
## SBLTOT_TA      1.8835005  0.0154367 122.0147 < 2.2e-16 ***
## ROA0          -5.4593135  0.1162753 -46.9516 < 2.2e-16 ***
## NPA0          -1.6711286  0.0497745 -33.5740 < 2.2e-16 ***
## LIQTA0         0.7102791  0.0097735  72.6739 < 2.2e-16 ***
## CORETA0       -0.1567010  0.0112857 -13.8850 < 2.2e-16 ***
## BCOMMITTAC0    0.2989920  0.0263706  11.3381 < 2.2e-16 ***
## DENOVO         0.1379163  0.0048423  28.4818 < 2.2e-16 ***
## mdi_ind:EQTA0  0.6637962  0.1110326   5.9784 2.259e-09 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Total Sum of Squares:    16865
## Residual Sum of Squares: 12974
## R-Squared:      0.23073
## Adj. R-Squared: 0.16039
## F-statistic: 4927.31 on 9 and 147855 DF, p-value: < 2.22e-16


fe5 <- plm(CSBLTOT ~  mdi_ind*EQTA0 + TLTA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")

fixedeffects5 <- fixef(fe5)




## old other variables-------

fe1 <- plm(SBLTOT0 ~ TLTA + BADBANK + BADBKFC + BADBKPC + ROA0 + TE0 + DENOVO + LIQTA0 + Y1995 + Y1996 + Y1997 + Y1998 + Y1999 + Y2000 + Y2001 + Y2002 + Y2003 + Y2004 + Y2005 + Y2006 + Y2007 + Y2008 + Y2009 + Y2010 + Y2011 + Y2012 + Y2013 + mdi_ind*1 + mdi_ind*TE0, data = call_all, model = "within")

fixedeffects1 <- fixef(fe1)

## dont need time dummies if we use both firm *and* time fixed effects:

fe11 <- plm(SBLTOT0 ~ TLTA + BADBANK + BADBKFC + BADBKPC + ROA0 + TE0 + DENOVO + LIQTA0 + mdi_ind*1 + mdi_ind*TE0, data = call_all, model = "within", effect = "twoways")

fixedeffects11 <- fixef(fe11)



fe2 <- plm(SBLTOT ~ TLTA + BADBANK + BADBKFC + BADBKPC + ROA0 + TE0 + DENOVO + LIQTA0 + mdi_ind*1 + mdi_ind*TE0, data = call_all, model = "within", effect = "twoways")

fixedeffects2 <- fixef(fe2)


fe3 <- plm(CBLTOT ~ TLTA + BADBANK + BADBKFC + BADBKPC + ROA0 + TE0 + DENOVO + LIQTA + mdi_ind*1 + mdi_ind*TE0, data = call_all, model = "within", effect = "twoways")

fixedeffects3 <- fixef(fe3)



### other types of models; no point to them, just testing package------

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


