#library some packages
library(plumber)
library(readr)
library(dplyr)
library(tidymodels)
library(tidyverse)
library(caret)
library(yardstick)
library(glmnet)
library(rpart)
library(parsnip)
library(dials)
library(ranger)

#read in the data
diabetes_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv", col_types = c("Diabetes_binary" = "factor", "HighBP" = "factor", "HighChol" = "factor", "CholCheck" = "factor", "Smoker" = "factor", "Stroke" = "factor", "HeartDiseaseorAttack" = "factor", "PhysActivity" = "factor", "Fruits" = "factor", "Veggies" = "factor", "HvyAlcoholConsump" = "factor", "AnyHealthcare" = "factor", "NoDocbcCost" = "factor", "GenHlth" = "factor", "DiffWalk" = "factor", "Sex" = "factor", "Age" = "factor", "Education" = "factor", "Income" = "factor")) |>
  mutate(Diabetes_binary = recode(Diabetes_binary, "0.0" = "No diabetes", "1.0" = "Diabetes")) |>
  mutate(HighBP = recode(HighBP, "0.0" = "no high BP", "1.0" = "high BP")) |>
  mutate(HighChol = recode(HighChol, "0.0" = "no high cholesterol", "1.0" = "high cholesterol")) |>
  mutate(CholCheck = recode(CholCheck, "0.0" = "no cholesterol check", "1.0" = "cholesterol check")) |>
  mutate(Smoker = recode(Smoker, "0.0" = "No", "1.0" = "Yes")) |>
  mutate(Stroke = recode(Stroke, "0.0" = "No", "1.0" = "Yes")) |>
  mutate(HeartDiseaseorAttack = recode(HeartDiseaseorAttack, "0.0" = "No", "1.0" = "Yes")) |>
  mutate(PhysActivity = recode(PhysActivity, "0.0" = "No", "1.0" = "Yes")) |>
  mutate(Fruits = recode(Fruits, "0.0" = "No", "1.0" = "Yes")) |>
  mutate(Veggies = recode(Veggies, "0.0" = "No", "1.0" = "Yes")) |>
  mutate(HvyAlcoholConsump = recode(HvyAlcoholConsump, "0.0" = "No", "1.0" = "Yes")) |>
  mutate(AnyHealthcare = recode(AnyHealthcare, "0.0" = "No", "1.0" = "Yes")) |>
  mutate(NoDocbcCost = recode(NoDocbcCost, "0.0" = "No", "1.0" = "Yes")) |>
  mutate(Fruits = recode(Fruits, "0.0" = "No", "1.0" = "Yes")) |>
  mutate(GenHlth = recode(GenHlth, "1.0" = "excellent", "2.0" = "very good", "3.0" = "good", "4.0" = "fair", "5.0" = "poor")) |>
  mutate(DiffWalk = recode(DiffWalk, "0.0" = "No", "1.0" = "Yes")) |>
  mutate(Sex = recode(Sex, "0.0" = "female", "1.0" = "male")) |>
  mutate(Age = recode(Age, "1.0" = "18-24", "2.0" = "25-29", "3.0" = "30-34", "4.0" = "35-39", "5.0" = "40-44", "6.0" = "45-49", "7.0" = "50-54", "8.0" = "55-59", "9.0" = "60-64", "10.0" = "65-69", "11.0" = "70-74", "12.0" = "75-79", "13.0" = "80 or older")) |>
  mutate(Education = recode(Education, "1.0" = "never attended school", "2.0" = "elementary", "3.0" = "junior high", "4.0" = "high school", "5.0" = "undergraduate", "6.0" = "postgraduate")) |>
  mutate(Income = recode(Income, "1.0" = "less than $10,000", "2.0" = "less than $15,000", "3.0" = "less than $20,000", "4.0" = "less than $$25,000", "5.0" = "less than $35,000", "6.0" = "less than $50,000", "7.0" = "less than $75,000", "8.0" = "$75,000 or more"))

#Create the API
#* @apiTitle Diabetes Prediction Model API
#* @apiDescription This API will have a model to predict for diabetes based on some variables and a link to the github pages.

#* Echo back the input
#* @param msg The message to echo
#* @get /info
function() {
    msg = paste0("Tamdan Le", \n, "https://tle24.github.io/Final-Project/")
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
