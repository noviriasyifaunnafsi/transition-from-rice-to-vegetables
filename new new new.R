library(tidyverse)
library(ggplot2)
library(dplyr)
library(decisionSupport)


transition_rice_to_vegetables <- function(){
  
  #weighting the benefits, assuming each benefit has impact on livelihood (?)
  
  ##assuming income have higher weight (more important) than other benefits
  income_weight <- 5
  
  ##assuming water use weight
  water_use_weight <- 2
  
  ##assuming nutrient weight
  nutrient_weight <- 3
  
  
  
  # Estimate benefits of no intervention (rice cultivation)
  rice_income <- vv(rice_yield * rice_price, n=n_year, var_CV=100)
  rice_water_use <- vv(rice_water_use, n=n_year, var_CV=100)
  #nutrient
  rice_calories <- vv(rice_calories, n=n_year, var_CV=100)
  rice_protein <- vv(rice_protein, n=n_year, var_CV=100)
  rice_fat <- vv(rice_fat, n=n_year, var_CV=100)
  rice_carbohydrates <- vv(rice_carbohydrates, n=n_year, var_CV=100)
  rice_fiber <- vv(rice_fiber, n=n_year, var_CV=100)
  rice_vitB3 <- vv(rice_vitB3, n=n_year, var_CV=100)
  rice_folate <- vv(rice_folate, n=n_year, var_CV=100)
  rice_calcium <- vv(rice_folate,  n=n_year, var_CV=100)
  rice_iron <- vv(rice_iron,  n=n_year, var_CV=100)
  rice_magnesium <- vv(rice_magnesium, n=n_year, var_CV=100)
  rice_phosphorus <- vv(rice_phosphorus, n=n_year, var_CV=100)
  rice_potassium <- vv(rice_potassium, n=n_year, var_CV=100)
  rice_sodium <- vv(rice_sodium,  n=n_year, var_CV=100)
  
  rice_nutrient_precal <- sum(rice_calories, rice_protein, rice_fat,
                               rice_carbohydrates, rice_fiber,
                               rice_vitB3, rice_folate, rice_calcium,
                               rice_iron, rice_magnesium, rice_phosphorus,
                               rice_potassium, rice_sodium)
  rice_nutrient <- vv(rice_nutrient_precal, n=n_year, var_CV=100)
  
  
  
  ##standardize values of rice benefits 
  ###income
  min_rice_income_precal <- min(rice_income)
  min_rice_income <- vv(min_rice_income_precal, n=n_year, var_CV=100)
  
  max_rice_income_precal <- max(rice_income)
  max_rice_income <- max(max_rice_income_precal, n=n_year, var_CV=100)
  
  estimated_mean_income_rice <- (min_rice_income+max_rice_income)/2
  estimated_sd_income_rice <- (max_rice_income-min_rice_income)/4
  
  scaled_rice_income_precal <- (rice_income - estimated_mean_income_rice) / estimated_sd_income_rice
  scaled_rice_income <- vv(scaled_rice_income_precal, n=n_year, var_CV=100)
  
  
  ###water use
  min_rice_water_use_precal <- min(rice_water_use)
  min_rice_water_use <- vv(min_rice_water_use_precal, n=n_year, var_CV=100)
  
  max_rice_water_use_precal <- max(rice_water_use)
  max_rice_water_use <- max(max_rice_water_use_precal, n=n_year, var_CV=100)
  
  estimated_mean_wu_rice <- (min_rice_income+max_rice_income)/2
  estimated_sd_wu_rice <- (max_rice_income-min_rice_income)/4
  
  scaled_rice_water_use_precal <- (rice_water_use - estimated_mean_wu_rice) / estimated_sd_wu_rice
  scaled_rice_water_use <- vv(scaled_rice_water_use_precal, n=n_year, var_CV=100)
  
  
  ###nutrient
  min_rice_nutrient_precal <- min(rice_nutrient)
  min_rice_nutrient <- vv(min_rice_nutrient_precal, n=n_year, var_CV=100)
  
  max_rice_nutrient_precal <- max(rice_nutrient)
  max_rice_nutrient <- max(max_rice_nutrient_precal, n=n_year, var_CV=100)
  
  estimated_mean_nutrient_rice <- (min_rice_nutrient+max_rice_income)/2
  estimated_sd_nutrient_rice <- (max_rice_nutrient-min_rice_income)/4
  
  scaled_rice_nutrient_precal <- (rice_nutrient - estimated_mean_nutrient_rice) / estimated_sd_nutrient_rice
  scaled_rice_nutrient <- vv(scaled_rice_nutrient_precal, n=n_year, var_CV=100)
  
  
  #total benefits results of no intervention (rice)
  rice_total_benefits_precal <- sum((income_weight * scaled_rice_income), 
                                    (water_use_weight * scaled_rice_water_use),
                                    (nutrient_weight * scaled_rice_nutrient))
  rice_total_benefit <- vv(rice_total_benefits_precal, n=n_year, var_CV=100)
  
  
  
  #Estimate costs of no intervention
  rice_production_cost <- vv(rice_production_cost, n=n_year, var_CV=100)
  #depreciation cost
  rice_warehouse_precal <- warehouse / 25  
  rice_warehouse <- vv(rice_warehouse_precal, n=n_year, var_CV=100)
  #total cost
  rice_total_cost_precal <- sum(rice_production_cost, rice_warehouse)
  rice_total_cost <- vv(rice_total_cost_precal, n=n_year, var_CV=100)
  
  
  #rice result
  rice_result_precal <- rice_total_benefit - rice_total_cost
  rice_result <- vv(rice_result_precal, n=n_year, var_CV=100)
  
  
  
  #Estimate benefits with intervention (vegetables cultivation) --> to be calculated for each crop
  
  #potato
  potato_income <- vv( potato_yield * potato_price, n=n_year, var_CV=100)
  potato_water_use <- vv(potato_water_use, n=n_year, var_CV=100)
  #nutrient
  potato_calories <- vv(potato_calories, n=n_year, var_CV=100)
  potato_protein <- vv(potato_protein, n=n_year, var_CV=100)
  potato_fat <- vv(potato_fat, n=n_year, var_CV=100)
  potato_carbohydrates <- vv(potato_carbohydrates, n=n_year, var_CV=100)
  potato_fiber <- vv(potato_fiber, n=n_year, var_CV=100)
  potato_vitC <- vv(potato_vitC, n=n_year, var_CV=100)
  potato_vitB3 <- vv(potato_vitB3, n=n_year, var_CV=100)
  potato_folate <- vv(potato_folate, n=n_year, var_CV=100)
  potato_calcium <- vv(potato_folate,  n=n_year, var_CV=100)
  potato_iron <- vv(potato_iron,  n=n_year, var_CV=100)
  potato_magnesium <- vv(potato_magnesium, n=n_year, var_CV=100)
  potato_phosphorus <- vv(potato_phosphorus, n=n_year, var_CV=100)
  potato_potassium <- vv(potato_potassium, n=n_year, var_CV=100)
  potato_sodium <- vv(potato_sodium,  n=n_year, var_CV=100)
  
  potato_nutrient_precal <- sum(potato_calories, potato_protein, potato_fat,
                                 potato_carbohydrates, potato_fiber,
                                 potato_vitC, potato_vitB3, potato_folate, potato_calcium,
                                 potato_iron, potato_magnesium, potato_phosphorus,
                                 potato_potassium, potato_sodium)
  potato_nutrient <- vv(potato_nutrient_precal, n=n_year, var_CV=100)
  
  
  ##standardize values of potato benefits 
  ###potato income
  min_potato_income_precal <- min(potato_income)
  min_potato_income <- vv(min_potato_income_precal, n=n_year, var_CV=100)
  
  max_potato_income_precal <- max(potato_income)
  max_potato_income <- max(max_potato_income_precal, n=n_year, var_CV=100)
  
  estimated_mean_income_potato <- (min_potato_income+max_potato_income)/2
  estimated_sd_income_potato <- (max_potato_income-min_potato_income)/4
  
  scaled_potato_income_precal <- (potato_income - estimated_mean_income_potato) / estimated_sd_income_potato
  scaled_potato_income <- vv(scaled_potato_income_precal, n=n_year, var_CV=100)
  
  
  ###potato water use
  min_potato_water_use_precal <- min(potato_water_use)
  min_potato_water_use <- vv(min_potato_water_use_precal, n=n_year, var_CV=100)
  
  max_potato_water_use_precal <- max(potato_water_use)
  max_potato_water_use <- max(max_potato_water_use_precal, n=n_year, var_CV=100)
  
  estimated_mean_wu_potato <- (min_potato_water_use+max_potato_water_use)/2
  estimated_sd_wu_potato <- (max_potato_water_use-min_potato_water_use)/4
  
  scaled_potato_water_use_precal <- (potato_water_use - estimated_mean_wu_potato) / estimated_sd_wu_potato
  scaled_potato_water_use <- vv(scaled_potato_water_use_precal, n=n_year, var_CV=100)
  
  
  ###potato nutrient
  min_potato_nutrient_precal <- min(potato_nutrient)
  min_potato_nutrient <- vv(min_potato_nutrient_precal, n=n_year, var_CV=100)
  
  max_potato_nutrient_precal <- max(potato_nutrient)
  max_potato_nutrient <- max(max_potato_nutrient_precal, n=n_year, var_CV=100)
  
  estimated_mean_nutrient_potato <- (min_potato_nutrient+max_potato_nutrient)/2
  estimated_sd_nutrient_potato <- (max_potato_nutrient-min_potato_nutrient)/4
  
  scaled_potato_nutrient_precal <- (potato_nutrient - estimated_mean_nutrient_potato) / estimated_sd_nutrient_potato
  scaled_potato_nutrient <- vv(scaled_potato_nutrient_precal, n=n_year, var_CV=100)
  
  
  #total benefits of potato
  potato_total_benefits_precal <- sum((income_weight * scaled_potato_income), 
                                      (water_use_weight * scaled_potato_water_use),
                                      (nutrient_weight * scaled_potato_nutrient))
  potato_total_benefit <- vv(potato_total_benefits_precal, n=n_year, var_CV=100)
  
  
  #shallot
  shallot_income <- vv(shallot_yield * shallot_price, n=n_year, var_CV=100)
  shallot_water_use <- vv(shallot_water_use, n=n_year, var_CV=100)
  #nutrient
  shallot_calories <- vv(shallot_calories, n=n_year, var_CV=100)
  shallot_protein <- vv(shallot_protein, n=n_year, var_CV=100)
  shallot_fat <- vv(shallot_fat, n=n_year, var_CV=100)
  shallot_carbohydrates <- vv(shallot_carbohydrates, n=n_year, var_CV=100)
  shallot_fiber <- vv(shallot_fiber, n=n_year, var_CV=100)
  shallot_vitC <- vv(shallot_vitC, n=n_year, var_CV=100)
  shallot_vitB3 <- vv(shallot_vitB3, n=n_year, var_CV=100)
  shallot_folate <- vv(shallot_folate, n=n_year, var_CV=100)
  shallot_calcium <- vv(shallot_folate,  n=n_year, var_CV=100)
  shallot_iron <- vv(shallot_iron,  n=n_year, var_CV=100)
  shallot_magnesium <- vv(shallot_magnesium, n=n_year, var_CV=100)
  shallot_phosphorus <- vv(shallot_phosphorus, n=n_year, var_CV=100)
  shallot_potassium <- vv(shallot_potassium, n=n_year, var_CV=100)
  shallot_sodium <- vv(shallot_sodium,  n=n_year, var_CV=100)
  
  shallot_nutrient_precal <- sum(shallot_calories, shallot_protein, shallot_fat,
                                  shallot_carbohydrates, shallot_fiber,
                                  shallot_vitC, shallot_vitB3, shallot_folate, shallot_calcium,
                                  shallot_iron, shallot_magnesium, shallot_phosphorus,
                                  shallot_potassium, shallot_sodium)
  shallot_nutrient <- vv(shallot_nutrient_precal, n=n_year, var_CV=100)
  
  
  ##standardize values of shallot benefits 
  ###shallot income
  min_shallot_income_precal <- min(shallot_income)
  min_shallot_income <- vv(min_shallot_income_precal, n=n_year, var_CV=100)
  
  max_shallot_income_precal <- max(shallot_income)
  max_shallot_income <- max(max_shallot_income_precal, n=n_year, var_CV=100)
  
  estimated_mean_income_shallot <- (min_shallot_income+max_shallot_income)/2
  estimated_sd_income_shallot <- (max_shallot_income-min_shallot_income)/4
  
  scaled_shallot_income_precal <- (shallot_income - estimated_mean_income_shallot) / estimated_sd_income_shallot
  scaled_shallot_income <- vv(scaled_shallot_income_precal, n=n_year, var_CV=100)
  
  
  ###shallot water use
  min_shallot_water_use_precal <- min(shallot_water_use)
  min_shallot_water_use <- vv(min_shallot_water_use_precal, n=n_year, var_CV=100)
  
  max_shallot_water_use_precal <- max(shallot_water_use)
  max_shallot_water_use <- max(max_shallot_water_use_precal, n=n_year, var_CV=100)
  
  estimated_mean_wu_shallot <- (min_shallot_water_use+max_shallot_water_use)/2
  estimated_sd_wu_shallot <- (max_shallot_water_use-min_shallot_water_use)/4
  
  scaled_shallot_water_use_precal <- (shallot_water_use - estimated_mean_wu_shallot) / estimated_sd_wu_shallot
  scaled_shallot_water_use <- vv(scaled_shallot_water_use_precal, n=n_year, var_CV=100)
  
  
  ###shallot nutrient
  min_shallot_nutrient_precal <- min(shallot_nutrient)
  min_shallot_nutrient <- vv(min_shallot_nutrient_precal, n=n_year, var_CV=100)
  
  max_shallot_nutrient_precal <- max(shallot_nutrient)
  max_shallot_nutrient <- max(max_shallot_nutrient_precal, n=n_year, var_CV=100)
  
  estimated_mean_nutrient_shallot <- (min_shallot_nutrient+max_shallot_nutrient)/2
  estimated_sd_nutrient_shallot <- (max_shallot_nutrient-min_shallot_nutrient)/4
  
  scaled_shallot_nutrient_precal <- (shallot_nutrient - estimated_mean_nutrient_shallot) / estimated_sd_nutrient_shallot
  scaled_shallot_nutrient <- vv(scaled_shallot_nutrient_precal, n=n_year, var_CV=100)
  
  #total benefits of shallot
  shallot_total_benefits_precal <- sum((income_weight * scaled_shallot_income), 
                                       (water_use_weight * scaled_shallot_water_use),
                                       (nutrient_weight * scaled_shallot_nutrient))
  shallot_total_benefit <- vv(shallot_total_benefits_precal, n=n_year, var_CV=100)
  
  
  
  #chili
  chili_income <- vv(chili_yield * chili_price, n=n_year, var_CV=100)
  chili_water_use <- vv(chili_water_use, n=n_year, var_CV=100)
  #nutrient
  chili_calories <- vv(chili_calories, n=n_year, var_CV=100)
  chili_protein <- vv(chili_protein, n=n_year, var_CV=100)
  chili_fat <- vv(chili_fat, n=n_year, var_CV=100)
  chili_carbohydrates <- vv(chili_carbohydrates, n=n_year, var_CV=100)
  chili_fiber <- vv(chili_fiber, n=n_year, var_CV=100)
  chili_vitA <- vv(chili_vitA, n=n_year, var_CV=100)
  chili_vitC <- vv(chili_vitC, n=n_year, var_CV=100)
  chili_vitB3 <- vv(chili_vitB3, n=n_year, var_CV=100)
  chili_folate <- vv(chili_folate, n=n_year, var_CV=100)
  chili_calcium <- vv(chili_folate,  n=n_year, var_CV=100)
  chili_iron <- vv(chili_iron,  n=n_year, var_CV=100)
  chili_magnesium <- vv(chili_magnesium, n=n_year, var_CV=100)
  chili_phosphorus <- vv(chili_phosphorus, n=n_year, var_CV=100)
  chili_potassium <- vv(chili_potassium, n=n_year, var_CV=100)
  chili_sodium <- vv(chili_sodium,  n=n_year, var_CV=100)
  
  chili_nutrient_precal <- sum(chili_calories, chili_protein, chili_fat,
                                chili_carbohydrates, chili_fiber, chili_vitA,
                                chili_vitC, chili_vitB3, chili_folate, chili_calcium,
                                chili_iron, chili_magnesium, chili_phosphorus,
                                chili_potassium, chili_sodium)
  chili_nutrient <- vv(chili_nutrient_precal, n=n_year, var_CV=100)
  
  
  ##standardize values of chili benefits 
  ###chili income
  min_chili_income_precal <- min(chili_income)
  min_chili_income <- vv(min_chili_income_precal, n=n_year, var_CV=100)
  
  max_chili_income_precal <- max(chili_income)
  max_chili_income <- max(max_chili_income_precal, n=n_year, var_CV=100)
  
  estimated_mean_income_chili <- (min_chili_income+max_chili_income)/2
  estimated_sd_income_chili <- (max_chili_income-min_chili_income)/4
  
  scaled_chili_income_precal <- (chili_income - estimated_mean_income_chili) / estimated_sd_income_chili
  scaled_chili_income <- vv(scaled_chili_income_precal, n=n_year, var_CV=100)
  
  
  ###chili water use
  min_chili_water_use_precal <- min(chili_water_use)
  min_chili_water_use <- vv(min_chili_water_use_precal, n=n_year, var_CV=100)
  
  max_chili_water_use_precal <- max(chili_water_use)
  max_chili_water_use <- max(max_chili_water_use_precal, n=n_year, var_CV=100)
  
  estimated_mean_wu_chili <- (min_chili_water_use+max_chili_water_use)/2
  estimated_sd_wu_chili <- (max_chili_water_use-min_chili_water_use)/4
  
  scaled_chili_water_use_precal <- (chili_water_use - estimated_mean_wu_chili) / estimated_sd_wu_chili
  scaled_chili_water_use <- vv(scaled_chili_water_use_precal, n=n_year, var_CV=100)
  
  
  ###chili nutrient
  min_chili_nutrient_precal <- min(chili_nutrient)
  min_chili_nutrient <- vv(min_chili_nutrient_precal, n=n_year, var_CV=100)
  
  max_chili_nutrient_precal <- max(chili_nutrient)
  max_chili_nutrient <- max(max_chili_nutrient_precal, n=n_year, var_CV=100)
  
  estimated_mean_nutrient_chili <- (min_chili_nutrient+max_chili_nutrient)/2
  estimated_sd_nutrient_chili <- (max_chili_nutrient-min_chili_nutrient)/4
  
  scaled_chili_nutrient_precal <- (chili_nutrient - estimated_mean_nutrient_chili) / estimated_sd_nutrient_chili
  scaled_chili_nutrient <- vv(scaled_chili_nutrient_precal, n=n_year, var_CV=100)
  
  
  #total benefits of chili
  chili_total_benefits_precal <- sum((income_weight * scaled_chili_income), 
                                     (water_use_weight * scaled_chili_water_use),
                                     (nutrient_weight * scaled_chili_nutrient))
  chili_total_benefit <- vv(chili_total_benefits_precal, n=n_year, var_CV=100)
  
  
  
  #carrot
  carrot_income <- vv(carrot_yield * carrot_price, n=n_year, var_CV=100)
  carrot_water_use <- vv(carrot_water_use, n=n_year, var_CV=100)
  #nutrient
  carrot_calories <- vv(carrot_calories, n=n_year, var_CV=100)
  carrot_protein <- vv(carrot_protein, n=n_year, var_CV=100)
  carrot_fat <- vv(carrot_fat, n=n_year, var_CV=100)
  carrot_carbohydrates <- vv(carrot_carbohydrates, n=n_year, var_CV=100)
  carrot_fiber <- vv(carrot_fiber, n=n_year, var_CV=100)
  carrot_vitA <- vv(carrot_vitA, n=n_year, var_CV=100)
  carrot_vitC <- vv(carrot_vitC, n=n_year, var_CV=100)
  carrot_vitB3 <- vv(carrot_vitB3, n=n_year, var_CV=100)
  carrot_folate <- vv(carrot_folate, n=n_year, var_CV=100)
  carrot_calcium <- vv(carrot_folate,  n=n_year, var_CV=100)
  carrot_iron <- vv(carrot_iron,  n=n_year, var_CV=100)
  carrot_magnesium <- vv(carrot_magnesium, n=n_year, var_CV=100)
  carrot_phosphorus <- vv(carrot_phosphorus, n=n_year, var_CV=100)
  carrot_potassium <- vv(carrot_potassium, n=n_year, var_CV=100)
  carrot_sodium <- vv(carrot_sodium,  n=n_year, var_CV=100)
  
  carrot_nutrient_precal <- sum(carrot_calories, carrot_protein, carrot_fat,
                                 carrot_carbohydrates, carrot_fiber, carrot_vitA,
                                 carrot_vitC, carrot_vitB3, carrot_folate, carrot_calcium,
                                 carrot_iron, carrot_magnesium, carrot_phosphorus,
                                 carrot_potassium, carrot_sodium)
  carrot_nutrient <- vv(carrot_nutrient_precal, n=n_year, var_CV=100)
  
  ##standardize values of carrot benefits 
  ###carrot income
  min_carrot_income_precal <- min(carrot_income)
  min_carrot_income <- vv(min_carrot_income_precal, n=n_year, var_CV=100)
  
  max_carrot_income_precal <- max(carrot_income)
  max_carrot_income <- max(max_carrot_income_precal, n=n_year, var_CV=100)
  
  estimated_mean_income_carrot <- (min_carrot_income+max_carrot_income)/2
  estimated_sd_income_carrot <- (max_carrot_income-min_carrot_income)/4
  
  scaled_carrot_income_precal <- (carrot_income - estimated_mean_income_carrot) / estimated_sd_income_carrot
  scaled_carrot_income <- vv(scaled_carrot_income_precal, n=n_year, var_CV=100)
  
  
  ###carrot water use
  min_carrot_water_use_precal <- min(carrot_water_use)
  min_carrot_water_use <- vv(min_carrot_water_use_precal, n=n_year, var_CV=100)
  
  max_carrot_water_use_precal <- max(carrot_water_use)
  max_carrot_water_use <- max(max_carrot_water_use_precal, n=n_year, var_CV=100)
  
  estimated_mean_wu_carrot <- (min_carrot_water_use+max_carrot_water_use)/2
  estimated_sd_wu_carrot <- (max_carrot_water_use-min_carrot_water_use)/4
  
  scaled_carrot_water_use_precal <- (carrot_water_use - estimated_mean_wu_carrot) / estimated_sd_wu_carrot
  scaled_carrot_water_use <- vv(scaled_carrot_water_use_precal, n=n_year, var_CV=100)
  
  
  ###carrot nutrient
  min_carrot_nutrient_precal <- min(carrot_nutrient)
  min_carrot_nutrient <- vv(min_carrot_nutrient_precal, n=n_year, var_CV=100)
  
  max_carrot_nutrient_precal <- max(carrot_nutrient)
  max_carrot_nutrient <- max(max_carrot_nutrient_precal, n=n_year, var_CV=100)
  
  estimated_mean_nutrient_carrot <- (min_carrot_nutrient+max_carrot_nutrient)/2
  estimated_sd_nutrient_carrot <- (max_carrot_nutrient-min_carrot_nutrient)/4
  
  scaled_carrot_nutrient_precal <- (carrot_nutrient - estimated_mean_nutrient_carrot) / estimated_sd_nutrient_carrot
  scaled_carrot_nutrient <- vv(scaled_carrot_nutrient_precal, n=n_year, var_CV=100)
  
  
  #total benefits of carrot
  carrot_total_benefits_precal <- sum((income_weight * scaled_carrot_income), 
                                      (water_use_weight * scaled_carrot_water_use),
                                      (nutrient_weight * scaled_carrot_nutrient))
  carrot_total_benefit <- vv(carrot_total_benefits_precal, n=n_year, var_CV=100)
  
  
  #cabbage
  cabbage_income <- vv(cabbage_yield * cabbage_price, n=n_year, var_CV=100)
  cabbage_water_use <- vv(cabbage_water_use, n=n_year, var_CV=100)
  #nutrient
  cabbage_calories <- vv(cabbage_calories, n=n_year, var_CV=100)
  cabbage_protein <- vv(cabbage_protein, n=n_year, var_CV=100)
  cabbage_fat <- vv(cabbage_fat, n=n_year, var_CV=100)
  cabbage_carbohydrates <- vv(cabbage_carbohydrates, n=n_year, var_CV=100)
  cabbage_fiber <- vv(cabbage_fiber, n=n_year, var_CV=100)
  cabbage_vitA <- vv(cabbage_vitA, n=n_year, var_CV=100)
  cabbage_vitC <- vv(cabbage_vitC, n=n_year, var_CV=100)
  cabbage_vitB3 <- vv(cabbage_vitB3, n=n_year, var_CV=100)
  cabbage_folate <- vv(cabbage_folate, n=n_year, var_CV=100)
  cabbage_calcium <- vv(cabbage_folate,  n=n_year, var_CV=100)
  cabbage_iron <- vv(cabbage_iron,  n=n_year, var_CV=100)
  cabbage_magnesium <- vv(cabbage_magnesium, n=n_year, var_CV=100)
  cabbage_phosphorus <- vv(cabbage_phosphorus, n=n_year, var_CV=100)
  cabbage_potassium <- vv(cabbage_potassium, n=n_year, var_CV=100)
  cabbage_sodium <- vv(cabbage_sodium,  n=n_year, var_CV=100)
  
  cabbage_nutrient_precal <- sum(cabbage_calories, cabbage_protein, cabbage_fat,
                                  cabbage_carbohydrates, cabbage_fiber, cabbage_vitA,
                                  cabbage_vitC, cabbage_vitB3, cabbage_folate, cabbage_calcium,
                                  cabbage_iron, cabbage_magnesium, cabbage_phosphorus,
                                  cabbage_potassium, cabbage_sodium)
  cabbage_nutrient <- vv(cabbage_nutrient_precal, n=n_year, var_CV=100)
  
  ##standardize values of cabbage benefits 
  ###cabbage income
  min_cabbage_income_precal <- min(cabbage_income)
  min_cabbage_income <- vv(min_cabbage_income_precal, n=n_year, var_CV=100)
  
  max_cabbage_income_precal <- max(cabbage_income)
  max_cabbage_income <- max(max_cabbage_income_precal, n=n_year, var_CV=100)
  
  estimated_mean_income_cabbage <- (min_cabbage_income+max_cabbage_income)/2
  estimated_sd_income_cabbage <- (max_cabbage_income-min_cabbage_income)/4
  
  scaled_cabbage_income_precal <- (cabbage_income - estimated_mean_income_cabbage) / estimated_sd_income_cabbage
  scaled_cabbage_income <- vv(scaled_cabbage_income_precal, n=n_year, var_CV=100)
  
  
  ###cabbage water use
  min_cabbage_water_use_precal <- min(cabbage_water_use)
  min_cabbage_water_use <- vv(min_cabbage_water_use_precal, n=n_year, var_CV=100)
  
  max_cabbage_water_use_precal <- max(cabbage_water_use)
  max_cabbage_water_use <- max(max_cabbage_water_use_precal, n=n_year, var_CV=100)
  
  estimated_mean_wu_cabbage <- (min_cabbage_water_use+max_cabbage_water_use)/2
  estimated_sd_wu_cabbage <- (max_cabbage_water_use-min_cabbage_water_use)/4
  
  scaled_cabbage_water_use_precal <- (cabbage_water_use - estimated_mean_wu_cabbage) / estimated_sd_wu_cabbage
  scaled_cabbage_water_use <- vv(scaled_cabbage_water_use_precal, n=n_year, var_CV=100)
  
  
  ###cabbage nutrient
  min_cabbage_nutrient_precal <- min(cabbage_nutrient)
  min_cabbage_nutrient <- vv(min_cabbage_nutrient_precal, n=n_year, var_CV=100)
  
  max_cabbage_nutrient_precal <- max(cabbage_nutrient)
  max_cabbage_nutrient <- max(max_cabbage_nutrient_precal, n=n_year, var_CV=100)
  
  estimated_mean_nutrient_cabbage <- (min_cabbage_nutrient+max_cabbage_nutrient)/2
  estimated_sd_nutrient_cabbage <- (max_cabbage_nutrient-min_cabbage_nutrient)/4
  
  scaled_cabbage_nutrient_precal <- (cabbage_nutrient - estimated_mean_nutrient_cabbage) / estimated_sd_nutrient_cabbage
  scaled_cabbage_nutrient <- vv(scaled_cabbage_nutrient_precal, n=n_year, var_CV=100)
  
  #total benefits of cabbage
  cabbage_total_benefits_precal <- sum((income_weight * scaled_cabbage_income), 
                                      (water_use_weight * scaled_cabbage_water_use),
                                      (nutrient_weight * scaled_cabbage_nutrient))
  cabbage_total_benefit <- vv(cabbage_total_benefits_precal, n=n_year, var_CV=100)
  
  
  
  
  #Estimate costs with intervention (vegetables cultivation) --> to be calculated for each crop
  ##potato costs
  potato_production_cost <- vv(potato_production_cost, n=n_year, var_CV=100)
  #depreciation cost
  potato_warehouse_precal <- warehouse / 25  
  potato_warehouse <- vv(potato_warehouse_precal, n=n_year, var_CV=100)
  potato_irrigation_precal <- pipe_installation / 25
  potato_irrigation <- vv(potato_irrigation_precal, n=n_year, var_CV=100)
  #total cost
  potato_total_cost_precal <- sum(potato_production_cost, potato_warehouse, potato_irrigation)
  potato_total_cost <- vv(potato_total_cost_precal, n=n_year, var_CV=100)
  
  ##shallot costs
  shallot_production_cost <- vv(shallot_production_cost, n=n_year, var_CV=100)
  #depreciation cost
  shallot_warehouse_precal <- warehouse / 25  
  shallot_warehouse <- vv(shallot_warehouse_precal, n=n_year, var_CV=100)
  shallot_irrigation_precal <- pipe_installation / 25
  shallot_irrigation <- vv(shallot_irrigation_precal, n=n_year, var_CV=100)
  #total cost
  shallot_total_cost_precal <- sum(shallot_production_cost, shallot_warehouse, shallot_irrigation)
  shallot_total_cost <- vv(shallot_total_cost_precal, n=n_year, var_CV=100)
  
  
  ##chili costs
  chili_production_cost <- vv(chili_production_cost, n=n_year, var_CV=100)
  #depreciation cost
  chili_warehouse_precal <- warehouse / 25  
  chili_warehouse <- vv(chili_warehouse_precal, n=n_year, var_CV=100)
  chili_irrigation_precal <- pipe_installation / 25
  chili_irrigation <- vv(chili_irrigation_precal, n=n_year, var_CV=100)
  #total cost
  chili_total_cost_precal <- sum(chili_production_cost, chili_warehouse, chili_irrigation)
  chili_total_cost <- vv(chili_total_cost_precal, n=n_year, var_CV=100)
  
  
  ##carrot costs
  carrot_production_cost <- vv(carrot_production_cost, n=n_year, var_CV=100)
  #depreciation cost
  carrot_warehouse_precal <- warehouse / 25  
  carrot_warehouse <- vv(carrot_warehouse_precal, n=n_year, var_CV=100)
  carrot_irrigation_precal <- pipe_installation / 25
  carrot_irrigation <- vv(carrot_irrigation_precal, n=n_year, var_CV=100)
  #total cost
  carrot_total_cost_precal <- sum(carrot_production_cost, carrot_warehouse, carrot_irrigation)
  carrot_total_cost <- vv(carrot_total_cost_precal, n=n_year, var_CV=100)
  
  
  ##cabbage costs
  cabbage_production_cost <- vv(cabbage_production_cost, n=n_year, var_CV=100)
  #depreciation cost
  cabbage_warehouse_precal <- warehouse / 25  
  cabbage_warehouse <- vv(cabbage_warehouse_precal, n=n_year, var_CV=100)
  cabbage_irrigation_precal <- pipe_installation / 25
  cabbage_irrigation <- vv(cabbage_irrigation_precal, n=n_year, var_CV=100)
  #total cost
  cabbage_total_cost_precal <- sum(cabbage_production_cost, cabbage_warehouse, cabbage_irrigation)
  cabbage_total_cost <- vv(cabbage_total_cost_precal, n=n_year, var_CV=100)
  
  
  #vegetable result
  #potato result
  potato_result_precal <- potato_total_benefit - potato_total_cost
  potato_result <- vv(potato_result_precal, n=n_year, var_CV=100)
  
  #shallot result
  shallot_result_precal <- shallot_total_benefit - shallot_total_cost
  shallot_result <- vv(shallot_result_precal, n=n_year, var_CV=100)
  
  #chili result
  chili_result_precal <- chili_total_benefit - chili_total_cost
  chili_result <- vv(chili_result_precal, n=n_year, var_CV=100)
  
  #carrot result
  carrot_result_precal <- carrot_total_benefit - carrot_total_cost
  carrot_result <- vv(carrot_result_precal, n=n_year, var_CV=100)
  
  #cabbage result
  cabbage_result_precal <- cabbage_total_benefit - cabbage_total_cost
  cabbage_result <- vv(cabbage_result_precal, n=n_year, var_CV=100)
  
  
  
  #Generate random crop selection for each year over 25 years 
  random_vegetable_selection <- function() {
    
    vegetables <- c(potato_result, shallot_result, chili_result, carrot_result, cabbage_result)
    probabilities <- c(0.25, 0.25, 0.1, 0.25, 0.25)
    
    
    set.seed(123) #for reproducibility, set a seed for randomized
    
    selected_vegetable <- sample(vegetables, size=25, replace=TRUE, prob=probabilities)
    
    return(selected_vegetable)
    
  }
  
  
  
  #NPV
  NPV_intervention_vegetables <- discount(random_vegetable_selection, discount_rate, calculate_NPV = TRUE)
  NPV_no_intervention_rice <- discount(rice_result, discount_rate, calculate_NPV = TRUE)
  
  
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(Interv_NPV = NPV_intervention_vegetables,
              NO_Interv_NPV = NPV_no_intervention_rice,
              NPV_decision_do = NPV_intervention_vegetables - NPV_no_intervention_rice,
              Cashflow_decision_do = NPV_intervention_vegetables- NPV_no_intervention_rice
  ))
}




# Run the Monte Carlo simulation using the model function
input_estimates <- read.csv("new_variable_estimates~fix.csv", sep = ";")
<<<<<<< HEAD
=======

str(input_estimates)
>>>>>>> 22dc5946f465a5ec4c0490946cf0ab57a8e47703

TRV_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                  model_function = transition_rice_to_vegetables,
                                  numberOfModelRuns = 1000,
                                  functionSyntax = "plainNames")




# plot NPV distribution analysis
decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_NPV","NO_Interv_NPV"),
                                    method = 'smooth_simple_overlay')

decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_NPV","NO_Interv_NPV"),
                                    method = 'boxplot')



# cashflow analysis
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_do")



# Projection to Latent Structures (PLS) analysis
pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[3], ncomp = 1)
plot_pls(pls_result, threshold = 0)


# VoI analysis
mcSimulation_table <- data.frame(TRV_mc_simulation$x, TRV_mc_simulation$y[1:3])
evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_NPV")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_do")




compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_do", 
                cashflow_var_name = "Cashflow_decision_do", 
                base_size = 7)



