library(tidyverse)
library(forcats)
library(lmtest)
library(gt)
library(table1)
library(gtsummary)
library(modelsummary)
library(did)
library(plm)

output_path <- "session_6/output"

ID <-as.numeric(c(1,1,2,2,3,3,4,4))
YEAR <-as.numeric(c(1984,1985,1984,1985,1984,1985,1984,1985))
TREAT <-as.numeric(c(1985,1985,1985,1985,1985,1985,0,0))
SATIS <-as.numeric(c(5,6,6,8,6,8,5,5))
EMP <-as.numeric(c(0,1,0,1,0,1,0,0))
DATA <- data.frame(ID,YEAR,SATIS,EMP,TREAT) |> 
  mutate(after = ifelse(YEAR == 1985, 1, 0)) |> 
  mutate(treatment = max(EMP), .by = ID)

lm(DATA$SATIS ~ DATA$EMP)
lm(SATIS ~ EMP + as.factor(ID), data = DATA)
lm(SATIS ~ treatment * after, data = DATA)
plm(SATIS ~ EMP, data = DATA, 
    index = c("ID", "YEAR"), model = "within")

ID <-as.numeric(c(1,1,1,2,2,2))
YEAR <-as.numeric(c(1984,1985,1986,1984,1985,1986))
SATIS <-as.numeric(c(10,6,9,10,7,8))
EMP <-as.numeric(c(0,1,1,0,1,1))
DATA01 = data.frame(ID,YEAR,SATIS,EMP)
#FE
plm(SATIS ~ EMP, data=DATA01, index=c("ID","YEAR"), model = "within")









