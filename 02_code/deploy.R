suppressPackageStartupMessages(library(xgboost))
library(plumber)
library(jsonlite)

library(caret)
library(data.table)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)
library(zeallot)

options(scipen = 999,
       readr.num_columns = 0)

print("Starting Workflow")

# set up the environment
load("/opt/ml/02_code/sysdata.rda")
source("/opt/ml/02_code/scripts.R")
source("/opt/ml/02_code/main.R")
print("R Environment Setup Done")

# import dataframe
df <- read_csv("/opt/ml/01_data/input.csv")
df1 <- read_csv("/opt/ml/01_data/expected.csv")
print("Sample DataFrame imported from the container")

# Run the predict function
expected <- predict(df)
print("Using the predict function")
print(expected)
expected <- data.frame(t(sapply(expected,c)))
# print(expected)

# Save the predicted data to the output folder
write_csv(expected, "/opt/ml/03_output/expected.csv")
print("Storing the output DataFrame to a different folder")

# # Testing the output
print("Testing the result with the sample expected")
test1 <- data.frame(matrix(unlist(expected), ncol=13, byrow=TRUE),stringsAsFactors=FALSE) %>% `colnames<-`(.[1, ]) %>% .[-1, ]
test2 <- df1 %>% mutate_if(is.double, as.character)
print(all.equal(test1,test2))


app <- plumb('endpoints.R')
app$run(host='0.0.0.0', port=8080)
