library(tidyverse)
library(forcats)
library(lmtest)
library(gt)
library(table1)
library(gtsummary)
library(modelsummary)
library(plm)

output_path <- "session_5/output"

ID <- as.numeric(c(1, 2, 2, 3, 3, 4, 4, 5))
YEAR <- as.numeric(c(1984, 1984, 1985, 1984, 1985, 1984, 1985, 1987))
SATIS <- as.numeric(c(5, 6, 8, 1, 2, 2, 3, 1))
EMP <- as.numeric(c(0, 0, 1, 0, 0, 0, 0, 1))
DATA01 <- data.frame(ID,YEAR,SATIS, EMP) |> 
  arrange(ID, YEAR) |> 
  mutate(spell_number = row_number(),
         .by = ID) |> 
  filter(max(spell_number) > 1,
         .by = ID)

ID <- as.numeric(c(1,1,2,2,3,3,4,4))
YEAR <- as.numeric(c(1984,1985,1984,1985,1984,1985,1984,1985))
SATIS <- as.numeric(c(5,7,4,5,3,3,1,1))
MARRIED <- as.numeric(c(0,1,0,1,0,0,0,0))
DATA01 <-  data.frame(ID,YEAR,SATIS,MARRIED) |> 
  mutate(married = case_when(MARRIED == 1 ~ YEAR,
                             MARRIED == 0 ~ 0,
                             .default = NA))


#-------------------------------------------------------------------
# Exercise 5.2
#-------------------------------------------------------------------
data <- read_rds("data/EXERCISE05.rds")

label(data$AGE) <- "Age"
label(data$YEAR) <- "Year"
table1(~AGE + YEAR + SEX + EDU + SATIS | FAM, data = data)

model_edu <- lm(SATIS ~ FAM + YEAR + SEX + AGE + EDU, 
                data = data)
summary(model_edu)
ols_man <- lm(SATIS ~ FAM + YEAR + AGE + EDU, 
              data = data |> filter(SEX == "2-Men"))
ols_woman <- lm(SATIS ~ FAM + YEAR + AGE + EDU, 
                data = data |> filter(SEX == "1-Women"))
modelsummary(list("man"= ols_man, "woman" = ols_woman))

model_did <- plm(
  SATIS ~ FAM + SEX + AGE + EDU,
  data = data,
  index = c("ID", "YEAR"),
  effect = "individual",
  model = "within"
)
summary(model_did)

model_woman <- plm(
  SATIS ~ FAM + AGE + EDU,
  data = data |> filter(SEX == "1-Women"),
  index = c("ID", "YEAR"),
  effect = "individual",
  model = "within"
)
model_man <- plm(
  SATIS ~ FAM + AGE + EDU,
  data = data |> filter(SEX == "2-Men"),
  index = c("ID", "YEAR"),
  effect = "individual",
  model = "within"
)

modelsummary(list("man" = model_man,
                  "woman" = model_woman))


