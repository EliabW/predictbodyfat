# clear workspace and load packages
rm(list = ls())
library(haven)
library(dplyr)

# load NHANES data
library(nhanesA)

# load data files for Blood Pressure
BPX <- nhanes("BPX")
BPX_B <- nhanes("BPX_B")
BPX_C <- nhanes("BPX_C")
BPX_D <- nhanes("BPX_D")

# load data files for Body Measures
BMX <- nhanes("BMX")
BMX_B <- nhanes("BMX_B")
BMX_C <- nhanes("BMX_C")
BMX_D <- nhanes("BMX_D")

# load data files for Demographics
DEMO <- nhanes("DEMO")
DEMO_B <- nhanes("DEMO_B")
DEMO_C <- nhanes("DEMO_C")
DEMO_D <- nhanes("DEMO_D")

# load data files for DEXA
DXX <- nhanesDXA(1999)
DXX_B <- nhanesDXA(2001)
DXX_C <- nhanesDXA(2003)
DXX_D <- nhanesDXA(2005)

# merge the data sets to form an analytic data set

# 1999 - 2000
a1 <- merge(DEMO, DXX, by = "SEQN")
a2 <- merge(BMX, BPX, by = "SEQN")
file99to00 <- merge(a1, a2, by = "SEQN")

# 2001 - 2002
b1 <- merge(DEMO_B, DXX_B, by = "SEQN")
b2 <- merge(BMX_B, BPX_B, by = "SEQN")
file01to02 <- merge(b1, b2, by = "SEQN")

# 2003 - 2004
c1 <- merge(DEMO_C, DXX_C, by = "SEQN")
c2 <- merge(BMX_C, BPX_C, by = "SEQN")
file03to04 <- merge(c1, c2, by = "SEQN")

# 2005 - 2006
d1 <- merge(DEMO_D, DXX_D, by = "SEQN")
d2 <- merge(BMX_D, BPX_D, by = "SEQN")
file05to06 <- merge(d1, d2, by = "SEQN")

# add a new column to each data set containing the year
file99to00 <- file99to00 %>% mutate(year = "99-00")
file01to02 <- file01to02 %>% mutate(year = "01-02")
file03to04 <- file03to04 %>% mutate(year = "03-04")
file05to06 <- file05to06 %>% mutate(year = "05-06")

# subset to variables of interest
file99to00 <- file99to00 %>% select(DXDTOPF, RIDEXPRG, DMDEDUC2, RIDRETH1,
                                    BMXARMC, BMXWAIST, RIAGENDR, RIDAGEYR,
                                    BMXHT, BMXWT, BMXBMI, X_MULT_, year,
                                    SDDSRVYR, WTMEC4YR, WTMEC2YR, BPXSY1)
file01to02 <- file01to02 %>% select(DXDTOPF, RIDEXPRG, DMDEDUC2, RIDRETH1,
                                    BMXARMC, BMXWAIST, RIAGENDR, RIDAGEYR,
                                    BMXHT, BMXWT, BMXBMI, X_MULT_, year,
                                    SDDSRVYR, WTMEC4YR, WTMEC2YR, BPXSY1)
file03to04 <- file03to04 %>% select(DXDTOPF, RIDEXPRG, DMDEDUC2, RIDRETH1,
                                    BMXARMC, BMXWAIST, RIAGENDR, RIDAGEYR,
                                    BMXHT, BMXWT, BMXBMI, X_MULT_, year,
                                    SDDSRVYR, WTMEC2YR, BPXSY1)
file05to06 <- file05to06 %>% select(DXDTOPF, RIDEXPRG, DMDEDUC2, RIDRETH1,
                                    BMXARMC, BMXWAIST, RIAGENDR, RIDAGEYR,
                                    BMXHT, BMXWT, BMXBMI, X_MULT_, year,
                                    SDDSRVYR, WTMEC2YR, BPXSY1)


# combine all the years together
combined_data <- bind_rows(file99to00, file01to02, file03to04, file05to06)
