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

#* Logistic regression model for predicting diabetes
#* @param HighBP 0 for no high BP, 1 for high BP
#* @param HighChol 0 for no high cholesterol, 1 for high cholesterol
#* @param BMI value of BMI
#* @param Stroke 0 for no stroke, 1 for stroke
#* @param HeartDiseaseorAttack 0 for no heart disease or heart attack, 1 for heart disease or heart attack
#* @get /pred
function(HighBP = 0, HighChol = 0, BMI = 28.4, Stroke = 0, HeartDiseaseorAttack = 0) {
  -3.02367993 + 0.06461834*as.numeric(BMI) + -1.15403139*as.numeric(HighBP) + -0.74280903*as.numeric(HighChol) + 0.51060086*as.numeric(Stroke) + 0.71177963*as.numeric(HeartDiseaseorAttack)
}

#Example function calls
# http://127.0.0.1:37194/pred?HighBP=1&HighChol=1&BMI=28.4&Stroke=1&HeartDiseaseorAttack=1
# http://127.0.0.1:37194/pred?HighBP=1&HighChol=0&BMI=33&Stroke=1&HeartDiseaseorAttack=0
# http://127.0.0.1:37194/pred?HighBP=0&HighChol=1&BMI=20.6&Stroke=0&HeartDiseaseorAttack=1


#* Echo back the input
#* @get /info
function() {
  "Name: Tamdan Le, URL: https://tle24.github.io/Final-Project/"
}



