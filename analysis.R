### analysis------

library(plm)
library(stargazer)

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



#### Amount of Small-Business Loans

## using variables from Table 1, this model should be one from *Table 6*-----

## The only questionable variable is the Loans (SBLTOT_TA), but doesnt affect other coefficients and signs

fe3 <- plm(CSBLTOT ~  mdi_ind*EQTA0 + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")

fixedeffects3 <- fixef(fe3)


## Table 4

fe4 <- plm(CSBLTOT ~  BIGBANK + BIGBKFC + BIGBKPC + mdi_ind*EQTA0 + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")


## Table 5

fe5 <- plm(CSBLTOT ~  BADBANK + BADBKFC + BADBKPC + mdi_ind*EQTA0 + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")


## Output three models (tables 3, 4, 5) to word via html-----

stargazer(fe3, fe4, fe5, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo", "Total Equity x MDI"), dep.var.labels = "Annual % Change in the Amount of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "Annual % Change in the Amount of Small-Business Loans Outstanding", out = "tables345.htm", intercept.bottom = TRUE, notes = "Results are from fixed-effects models with both time and bank fixed effects, for the years 1995 through 2015.  The dependent variable is the annual percent change in the amount of small-business loans outstanding. Business loans are defined as the sum of commercial, industrial, and commercial real-estate loans.  ")


#### Number of Small-Business Loans------

fe6 <- plm(CNUMSBL ~  mdi_ind*EQTA0 + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")

## Table 7

fe7 <- plm(CNUMSBL ~  BIGBANK + BIGBKFC + BIGBKPC + mdi_ind*EQTA0 + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")

## Table 8

fe8 <- plm(CNUMSBL ~  BADBANK + BADBKFC + BADBKPC + mdi_ind*EQTA0 + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO, data = call_all, model = "within", effect = "twoways")


## Output three models (tables 6, 7, 8) to word via html-----

stargazer(fe6, fe7, fe8, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo", "Total Equity x MDI"), dep.var.labels = "Annual % Change in the Number of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "Annual % Change in the Number of Small-Business Loans Outstanding", out = "tables678.htm", intercept.bottom = TRUE, notes = "Results are from fixed-effects models with both time and bank fixed effects, for the years 1995 through 2015.  The dependent variable is the annual percent change in the number of small-business loans outstanding. Business loans are defined as the sum of commercial, industrial, and commercial real-estate loans.  ")


## analyze effect of firm size----

## amount of loans

fe.size1 <- plm(CSBLTOT ~  SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")


fe.size2 <- plm(CSBLTOT ~  BIGBANK + BIGBKFC + BIGBKPC + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")

## Table size 3

fe.size3 <- plm(CSBLTOT ~  BADBANK + BADBKFC + BADBKPC + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")


## Output three models to word via html-----

stargazer(fe.size1, fe.size2, fe.size3, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo", "ln(Assets)", "Total Equity x MDI", "Total Equity x ln(Assets)"), dep.var.labels = "Annual % Change in the Amount of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "Annual % Change in the Amount of Small-Business Loans Outstanding", out = "tables_size123.htm", intercept.bottom = TRUE, notes = "Results are from fixed-effects models with both time and bank fixed effects, for the years 1995 through 2015.  The dependent variable is the annual percent change in the amount of small-business loans outstanding. Business loans are defined as the sum of commercial, industrial, and commercial real-estate loans.  ")


## number of loans

fe.size4 <- plm(CNUMSBL ~  SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")


fe.size5 <- plm(CNUMSBL ~  BIGBANK + BIGBKFC + BIGBKPC + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")

## Table size 3

fe.size6 <- plm(CNUMSBL ~  BADBANK + BADBKFC + BADBKPC + SBLTOT_TA + EQTA0 + ROA0 + NPA0 + LIQTA0 + CORETA0 + BCOMMITTAC0 + DENOVO + mdi_ind*EQTA0 + EQTA0*LNTA0, data = call_all, model = "within", effect = "twoways")


## Output three models to word via html-----

stargazer(fe.size4, fe.size5, fe.size6, covariate.labels = c("Large Bank", "Large Bank x Fin. Crisis", "Large Bank x Post Crisis", "Troubled Bank", "Troubled Bank x Fin. Crisis", "Troubled Bank x Post Crisis", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "De Novo", "ln(Assets)", "Total Equity x MDI", "Total Equity x ln(Assets)"), dep.var.labels = "Annual % Change in the Number of Small-Business Loans Outstanding", digits = 3, no.space=TRUE, header=FALSE, type='html', omit.stat=c("LL"), title = "Annual % Change in the Number of Small-Business Loans Outstanding", out = "tables_size456.htm", intercept.bottom = TRUE, notes = "Results are from fixed-effects models with both time and bank fixed effects, for the years 1995 through 2015.  The dependent variable is the annual percent change in the number of small-business loans outstanding. Business loans are defined as the sum of commercial, industrial, and commercial real-estate loans.  ")











### old-------










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


