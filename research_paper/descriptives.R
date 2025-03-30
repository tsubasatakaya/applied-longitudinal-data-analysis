source("research_paper/setup.R")
source("research_paper/process_data.R")
################################################
# Sample statistics
################################################
table1::label(data_processed$sex) <- "Sex"
table1::label(data_processed$age) <- "Age"
table1::label(data_processed$edu) <- "Education"
table1::label(data_processed$has_kid) <- "Child status"
table1::label(data_processed$emp) <- "Employment status"
table1::label(data_processed$log_income) <- "Log income"
table1::label(data_processed$depression) <- "Depression"
desc_tab <- table1(~ sex + age + edu + has_kid + emp + log_income 
                   + depression | partnership,
                   data = data_processed)
desc_tab