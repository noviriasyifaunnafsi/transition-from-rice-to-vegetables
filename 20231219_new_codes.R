library(tidyverse)
library(ggplot2)
library(dplyr)
library(decisionSupport)


transition_rice_to_vegetables <- function(){
  
  
  # Estimate benefits of no intervention (rice cultivation)
  rice_income <- vv(rice_yield * rice_price, n=n_year, var_CV=100)
  
  
  # Estimate  'price' for water use
  rice_water_use_price <- vv(rice_income/rice_water_use, n=n_year, var_CV=100)
  rice_wateruse <- vv(rice_water_use_price, n=n_year, var_CV=100)
  
  
  #nutrient
  ##convert yield to g to calculate nutrient 'price' in rice
  rice_yield_g <- vv(rice_yield*1000, n=n_year, var_CV=100)
  
  ##estimate  rice price per g
  rice_price_g <- vv(rice_yield_g * rice_price, n=n_year, var_CV=100)
  
  ##estimate  nutrient price in rice
  rice_calories <- vv(rice_calories, n=n_year, var_CV=100)
  rice_calories_price <- vv(rice_price_g*rice_calories, n=n_year, var_CV=100)
  
  rice_protein <- vv(rice_protein, n=n_year, var_CV=100)
  rice_protein_price <- vv(rice_price_g*rice_protein, n=n_year, var_CV=100)
  
  rice_fat <- vv(rice_fat, n=n_year, var_CV=100)
  rice_fat_price <- vv(rice_price_g*rice_fat, n=n_year, var_CV=100)
  
  rice_carbohydrates <- vv(rice_carbohydrates, n=n_year, var_CV=100)
  rice_carbohydrates_price <- vv(rice_price_g*rice_carbohydrates, n=n_year, var_CV=100)
  
  rice_fiber <- vv(rice_fiber, n=n_year, var_CV=100)
  rice_fiber_price <- vv(rice_price_g*rice_fiber, n=n_year, var_CV=100)
  
  rice_vitA <- vv(rice_vitA, n=n_year, var_CV=100)
  rice_vitA_price <- vv(rice_price_g*rice_vitA, n=n_year, var_CV=100)
  
  rice_vitB3 <- vv(rice_vitB3, n=n_year, var_CV=100)
  rice_vitB3_price <- vv(rice_price_g*rice_vitB3, n=n_year, var_CV=100)
  
  rice_vitC <- vv(rice_vitB3, n=n_year, var_CV=100)
  rice_vitC_price <- vv(rice_price_g*rice_vitC, n=n_year, var_CV=100)
  
  rice_folate <- vv(rice_folate, n=n_year, var_CV=100)
  rice_folate_price <- vv(rice_price_g*rice_folate, n=n_year, var_CV=100)
  
  rice_calcium <- vv(rice_folate,  n=n_year, var_CV=100)
  rice_calcium_price <- vv(rice_price_g*rice_calcium, n=n_year, var_CV=100)
  
  rice_iron <- vv(rice_iron,  n=n_year, var_CV=100)
  rice_iron_price <- vv(rice_price_g*rice_iron, n=n_year, var_CV=100)
  
  rice_magnesium <- vv(rice_magnesium, n=n_year, var_CV=100)
  rice_magnesium_price <- vv(rice_price_g*rice_magnesium, n=n_year, var_CV=100)
  
  rice_phosphorus <- vv(rice_phosphorus, n=n_year, var_CV=100)
  rice_phosphorus_price <- vv(rice_price_g*rice_phosphorus, n=n_year, var_CV=100)
  
  rice_potassium <- vv(rice_potassium, n=n_year, var_CV=100)
  rice_potassium_price <- vv(rice_price_g*rice_potassium, n=n_year, var_CV=100)
  
  rice_sodium <- vv(rice_sodium,  n=n_year, var_CV=100)
  rice_sodium_price <- vv(rice_price_g*rice_sodium, n=n_year, var_CV=100)
  
  rice_nutrient_precal <- sum(rice_calories_price, 
                              rice_protein_price, 
                              rice_fat_price,
                              rice_carbohydrates_price, 
                              rice_fiber_price, 
                              rice_vitA_price, 
                              rice_vitB3_price, 
                              rice_vitC_price, 
                              rice_folate_price, 
                              rice_calcium_price,
                              rice_iron_price, 
                              rice_magnesium_price, 
                              rice_phosphorus_price,
                              rice_potassium, 
                              rice_sodium)
  rice_nutrient <- vv(rice_nutrient_precal, n=n_year, var_CV=100)
  
  
  
  #total benefits results of no intervention (rice) per year (3 season)
  rice_total_benefits_precal <- sum(rice_income, rice_wateruse, rice_nutrient)*3
  rice_total_benefit <- vv(rice_total_benefits_precal, n=n_year, var_CV=100)
  
  
  
  #Estimate costs of no intervention
  rice_production_cost <- vv(rice_production_cost*3, n=n_year, var_CV=100)
  rice_warehouse <- vv(warehouse/25, n=n_year, var_CV=100)
  #total cost
  rice_total_cost_precal <- sum(rice_production_cost, rice_warehouse)
  rice_total_cost <- vv(rice_total_cost_precal, n=n_year, var_CV=100)
  
  
  #rice result
  rice_result_precal <- rice_total_benefit - rice_total_cost
  rice_result <- vv(rice_result_precal, n=n_year, var_CV=100)
  
  
  
  #Estimate benefits with intervention (vegetables cultivation) --> to be calculated for each crop
  
  #potato
  potato_income <- vv( potato_yield * potato_price, n=n_year, var_CV=100)
  
  # Estimate  'price' for water use
  potato_water_use_price <- vv(potato_income/potato_water_use, n=n_year, var_CV=100)
  potato_wateruse <- vv(potato_water_use_price, n=n_year, var_CV=100)
  
  ##convert yield to g to calculate nutrient 'price' in potato
  potato_yield_g <- vv(potato_yield*1000, n=n_year, var_CV=100)
  
  ##estimate  potato price per g
  potato_price_g <- vv(potato_yield_g * potato_price, n=n_year, var_CV=100)
  
  ##estimate  nutrient price in potato
  potato_calories <- vv(potato_calories, n=n_year, var_CV=100)
  potato_calories_price <- vv(potato_price_g*potato_calories, n=n_year, var_CV=100)
  
  potato_protein <- vv(potato_protein, n=n_year, var_CV=100)
  potato_protein_price <- vv(potato_price_g*potato_protein, n=n_year, var_CV=100)
  
  potato_fat <- vv(potato_fat, n=n_year, var_CV=100)
  potato_fat_price <- vv(potato_price_g*potato_fat, n=n_year, var_CV=100)
  
  potato_carbohydrates <- vv(potato_carbohydrates, n=n_year, var_CV=100)
  potato_carbohydrates_price <- vv(potato_price_g*potato_carbohydrates, n=n_year, var_CV=100)
  
  potato_fiber <- vv(potato_fiber, n=n_year, var_CV=100)
  potato_fiber_price <- vv(potato_price_g*potato_fiber, n=n_year, var_CV=100)
  
  potato_vitA <- vv(potato_vitA, n=n_year, var_CV=100)
  potato_vitA_price <- vv(potato_price_g*potato_vitA, n=n_year, var_CV=100)
  
  potato_vitB3 <- vv(potato_vitB3, n=n_year, var_CV=100)
  potato_vitB3_price <- vv(potato_price_g*potato_vitB3, n=n_year, var_CV=100)
  
  potato_vitC <- vv(potato_vitB3, n=n_year, var_CV=100)
  potato_vitC_price <- vv(potato_price_g*potato_vitC, n=n_year, var_CV=100)
  
  potato_folate <- vv(potato_folate, n=n_year, var_CV=100)
  potato_folate_price <- vv(potato_price_g*potato_folate, n=n_year, var_CV=100)
  
  potato_calcium <- vv(potato_folate,  n=n_year, var_CV=100)
  potato_calcium_price <- vv(potato_price_g*potato_calcium, n=n_year, var_CV=100)
  
  potato_iron <- vv(potato_iron,  n=n_year, var_CV=100)
  potato_iron_price <- vv(potato_price_g*potato_iron, n=n_year, var_CV=100)
  
  potato_magnesium <- vv(potato_magnesium, n=n_year, var_CV=100)
  potato_magnesium_price <- vv(potato_price_g*potato_magnesium, n=n_year, var_CV=100)
  
  potato_phosphorus <- vv(potato_phosphorus, n=n_year, var_CV=100)
  potato_phosphorus_price <- vv(potato_price_g*potato_phosphorus, n=n_year, var_CV=100)
  
  potato_potassium <- vv(potato_potassium, n=n_year, var_CV=100)
  potato_potassium_price <- vv(potato_price_g*potato_potassium, n=n_year, var_CV=100)
  
  potato_sodium <- vv(potato_sodium,  n=n_year, var_CV=100)
  potato_sodium_price <- vv(potato_price_g*potato_sodium, n=n_year, var_CV=100)
  
  potato_nutrient_precal <- sum(potato_calories_price, 
                              potato_protein_price, 
                              potato_fat_price,
                              potato_carbohydrates_price, 
                              potato_fiber_price, 
                              potato_vitA_price, 
                              potato_vitB3_price, 
                              potato_vitC_price, 
                              potato_folate_price, 
                              potato_calcium_price,
                              potato_iron_price, 
                              potato_magnesium_price, 
                              potato_phosphorus_price,
                              potato_potassium, 
                              rice_sodium)
  potato_nutrient <- vv(potato_nutrient_precal, n=n_year, var_CV=100)
  
  #total benefits of potato
  potato_total_benefits_precal <- sum(potato_income, potato_wateruse, potato_nutrient) 
  potato_total_benefit <- vv(potato_total_benefits_precal, n=n_year, var_CV=100)
  
  
  #shallot
  shallot_income <- vv(shallot_yield * shallot_price, n=n_year, var_CV=100)
  
  # Estimate  'price' for water use
  shallot_water_use_price <- vv(shallot_income/shallot_water_use, n=n_year, var_CV=100)
  shallot_wateruse <- vv(shallot_water_use_price, n=n_year, var_CV=100)
  
  ##convert yield to g to calculate nutrient 'price' in shallot
  shallot_yield_g <- vv(shallot_yield*1000, n=n_year, var_CV=100)
  
  ##estimate  shallot price per g
  shallot_price_g <- vv(shallot_yield_g * shallot_price, n=n_year, var_CV=100)
  
  ##estimate  nutrient price in shallot
  shallot_calories <- vv(shallot_calories, n=n_year, var_CV=100)
  shallot_calories_price <- vv(shallot_price_g*shallot_calories, n=n_year, var_CV=100)
  
  shallot_protein <- vv(shallot_protein, n=n_year, var_CV=100)
  shallot_protein_price <- vv(shallot_price_g*shallot_protein, n=n_year, var_CV=100)
  
  shallot_fat <- vv(shallot_fat, n=n_year, var_CV=100)
  shallot_fat_price <- vv(shallot_price_g*shallot_fat, n=n_year, var_CV=100)
  
  shallot_carbohydrates <- vv(shallot_carbohydrates, n=n_year, var_CV=100)
  shallot_carbohydrates_price <- vv(shallot_price_g*shallot_carbohydrates, n=n_year, var_CV=100)
  
  shallot_fiber <- vv(shallot_fiber, n=n_year, var_CV=100)
  shallot_fiber_price <- vv(shallot_price_g*shallot_fiber, n=n_year, var_CV=100)
  
  shallot_vitA <- vv(shallot_vitA, n=n_year, var_CV=100)
  shallot_vitA_price <- vv(shallot_price_g*shallot_vitA, n=n_year, var_CV=100)
  
  shallot_vitB3 <- vv(shallot_vitB3, n=n_year, var_CV=100)
  shallot_vitB3_price <- vv(shallot_price_g*shallot_vitB3, n=n_year, var_CV=100)
  
  shallot_vitC <- vv(shallot_vitB3, n=n_year, var_CV=100)
  shallot_vitC_price <- vv(shallot_price_g*shallot_vitC, n=n_year, var_CV=100)
  
  shallot_folate <- vv(shallot_folate, n=n_year, var_CV=100)
  shallot_folate_price <- vv(shallot_price_g*shallot_folate, n=n_year, var_CV=100)
  
  shallot_calcium <- vv(shallot_folate,  n=n_year, var_CV=100)
  shallot_calcium_price <- vv(shallot_price_g*shallot_calcium, n=n_year, var_CV=100)
  
  shallot_iron <- vv(shallot_iron,  n=n_year, var_CV=100)
  shallot_iron_price <- vv(shallot_price_g*shallot_iron, n=n_year, var_CV=100)
  
  shallot_magnesium <- vv(shallot_magnesium, n=n_year, var_CV=100)
  shallot_magnesium_price <- vv(shallot_price_g*shallot_magnesium, n=n_year, var_CV=100)
  
  shallot_phosphorus <- vv(shallot_phosphorus, n=n_year, var_CV=100)
  shallot_phosphorus_price <- vv(shallot_price_g*shallot_phosphorus, n=n_year, var_CV=100)
  
  shallot_potassium <- vv(shallot_potassium, n=n_year, var_CV=100)
  shallot_potassium_price <- vv(shallot_price_g*shallot_potassium, n=n_year, var_CV=100)
  
  shallot_sodium <- vv(shallot_sodium,  n=n_year, var_CV=100)
  shallot_sodium_price <- vv(shallot_price_g*shallot_sodium, n=n_year, var_CV=100)
  
  shallot_nutrient_precal <- sum(shallot_calories_price, 
                                shallot_protein_price, 
                                shallot_fat_price,
                                shallot_carbohydrates_price, 
                                shallot_fiber_price, 
                                shallot_vitA_price, 
                                shallot_vitB3_price, 
                                shallot_vitC_price, 
                                shallot_folate_price, 
                                shallot_calcium_price,
                                shallot_iron_price, 
                                shallot_magnesium_price, 
                                shallot_phosphorus_price,
                                shallot_potassium, 
                                rice_sodium)
  shallot_nutrient <- vv(shallot_nutrient_precal, n=n_year, var_CV=100)
  
  #total benefits of shallot
  shallot_total_benefits_precal <- sum(shallot_income, shallot_wateruse, shallot_nutrient) 
  shallot_total_benefit <- vv(shallot_total_benefits_precal, n=n_year, var_CV=100)
  

  
  #chili
  chili_income <- vv(chili_yield * chili_price, n=n_year, var_CV=100)
  
  # Estimate  'price' for water use
  chili_water_use_price <- vv(chili_income/chili_water_use, n=n_year, var_CV=100)
  chili_wateruse <- vv(chili_water_use_price, n=n_year, var_CV=100)
  
  ##convert yield to g to calculate nutrient 'price' in chili
  chili_yield_g <- vv(chili_yield*1000, n=n_year, var_CV=100)
  
  ##estimate  chili price per g
  chili_price_g <- vv(chili_yield_g * chili_price, n=n_year, var_CV=100)
  
  ##estimate  nutrient price in chili
  chili_calories <- vv(chili_calories, n=n_year, var_CV=100)
  chili_calories_price <- vv(chili_price_g*chili_calories, n=n_year, var_CV=100)
  
  chili_protein <- vv(chili_protein, n=n_year, var_CV=100)
  chili_protein_price <- vv(chili_price_g*chili_protein, n=n_year, var_CV=100)
  
  chili_fat <- vv(chili_fat, n=n_year, var_CV=100)
  chili_fat_price <- vv(chili_price_g*chili_fat, n=n_year, var_CV=100)
  
  chili_carbohydrates <- vv(chili_carbohydrates, n=n_year, var_CV=100)
  chili_carbohydrates_price <- vv(chili_price_g*chili_carbohydrates, n=n_year, var_CV=100)
  
  chili_fiber <- vv(chili_fiber, n=n_year, var_CV=100)
  chili_fiber_price <- vv(chili_price_g*chili_fiber, n=n_year, var_CV=100)
  
  chili_vitA <- vv(chili_vitA, n=n_year, var_CV=100)
  chili_vitA_price <- vv(chili_price_g*chili_vitA, n=n_year, var_CV=100)
  
  chili_vitB3 <- vv(chili_vitB3, n=n_year, var_CV=100)
  chili_vitB3_price <- vv(chili_price_g*chili_vitB3, n=n_year, var_CV=100)
  
  chili_vitC <- vv(chili_vitB3, n=n_year, var_CV=100)
  chili_vitC_price <- vv(chili_price_g*chili_vitC, n=n_year, var_CV=100)
  
  chili_folate <- vv(chili_folate, n=n_year, var_CV=100)
  chili_folate_price <- vv(chili_price_g*chili_folate, n=n_year, var_CV=100)
  
  chili_calcium <- vv(chili_folate,  n=n_year, var_CV=100)
  chili_calcium_price <- vv(chili_price_g*chili_calcium, n=n_year, var_CV=100)
  
  chili_iron <- vv(chili_iron,  n=n_year, var_CV=100)
  chili_iron_price <- vv(chili_price_g*chili_iron, n=n_year, var_CV=100)
  
  chili_magnesium <- vv(chili_magnesium, n=n_year, var_CV=100)
  chili_magnesium_price <- vv(chili_price_g*chili_magnesium, n=n_year, var_CV=100)
  
  chili_phosphorus <- vv(chili_phosphorus, n=n_year, var_CV=100)
  chili_phosphorus_price <- vv(chili_price_g*chili_phosphorus, n=n_year, var_CV=100)
  
  chili_potassium <- vv(chili_potassium, n=n_year, var_CV=100)
  chili_potassium_price <- vv(chili_price_g*chili_potassium, n=n_year, var_CV=100)
  
  chili_sodium <- vv(chili_sodium,  n=n_year, var_CV=100)
  chili_sodium_price <- vv(chili_price_g*chili_sodium, n=n_year, var_CV=100)
  
  chili_nutrient_precal <- sum(chili_calories_price, 
                                chili_protein_price, 
                                chili_fat_price,
                                chili_carbohydrates_price, 
                                chili_fiber_price, 
                                chili_vitA_price, 
                                chili_vitB3_price, 
                                chili_vitC_price, 
                                chili_folate_price, 
                                chili_calcium_price,
                                chili_iron_price, 
                                chili_magnesium_price, 
                                chili_phosphorus_price,
                                chili_potassium, 
                                rice_sodium)
  chili_nutrient <- vv(chili_nutrient_precal, n=n_year, var_CV=100)
  
  #total benefits of chili
  chili_total_benefits_precal <- sum(chili_income, chili_wateruse, chili_nutrient) 
  chili_total_benefit <- vv(chili_total_benefits_precal, n=n_year, var_CV=100)
  
  
  #carrot
  carrot_income <- vv(carrot_yield * carrot_price, n=n_year, var_CV=100)
  
  # Estimate  'price' for water use
  carrot_water_use_price <- vv(carrot_income/carrot_water_use, n=n_year, var_CV=100)
  carrot_wateruse <- vv(carrot_water_use_price, n=n_year, var_CV=100)
  
  ##convert yield to g to calculate nutrient 'price' in carrot
  carrot_yield_g <- vv(carrot_yield*1000, n=n_year, var_CV=100)
  
  ##estimate carrot price per g
  carrot_price_g <- vv(carrot_yield_g * carrot_price, n=n_year, var_CV=100)
  
  ##estimate nutrient price in carrot
  carrot_calories <- vv(carrot_calories, n=n_year, var_CV=100)
  carrot_calories_price <- vv(carrot_price_g*carrot_calories, n=n_year, var_CV=100)
  
  carrot_protein <- vv(carrot_protein, n=n_year, var_CV=100)
  carrot_protein_price <- vv(carrot_price_g*carrot_protein, n=n_year, var_CV=100)
  
  carrot_fat <- vv(carrot_fat, n=n_year, var_CV=100)
  carrot_fat_price <- vv(carrot_price_g*carrot_fat, n=n_year, var_CV=100)
  
  carrot_carbohydrates <- vv(carrot_carbohydrates, n=n_year, var_CV=100)
  carrot_carbohydrates_price <- vv(carrot_price_g*carrot_carbohydrates, n=n_year, var_CV=100)
  
  carrot_fiber <- vv(carrot_fiber, n=n_year, var_CV=100)
  carrot_fiber_price <- vv(carrot_price_g*carrot_fiber, n=n_year, var_CV=100)
  
  carrot_vitA <- vv(carrot_vitA, n=n_year, var_CV=100)
  carrot_vitA_price <- vv(carrot_price_g*carrot_vitA, n=n_year, var_CV=100)
  
  carrot_vitB3 <- vv(carrot_vitB3, n=n_year, var_CV=100)
  carrot_vitB3_price <- vv(carrot_price_g*carrot_vitB3, n=n_year, var_CV=100)
  
  carrot_vitC <- vv(carrot_vitB3, n=n_year, var_CV=100)
  carrot_vitC_price <- vv(carrot_price_g*carrot_vitC, n=n_year, var_CV=100)
  
  carrot_folate <- vv(carrot_folate, n=n_year, var_CV=100)
  carrot_folate_price <- vv(carrot_price_g*carrot_folate, n=n_year, var_CV=100)
  
  carrot_calcium <- vv(carrot_folate,  n=n_year, var_CV=100)
  carrot_calcium_price <- vv(carrot_price_g*carrot_calcium, n=n_year, var_CV=100)
  
  carrot_iron <- vv(carrot_iron,  n=n_year, var_CV=100)
  carrot_iron_price <- vv(carrot_price_g*carrot_iron, n=n_year, var_CV=100)
  
  carrot_magnesium <- vv(carrot_magnesium, n=n_year, var_CV=100)
  carrot_magnesium_price <- vv(carrot_price_g*carrot_magnesium, n=n_year, var_CV=100)
  
  carrot_phosphorus <- vv(carrot_phosphorus, n=n_year, var_CV=100)
  carrot_phosphorus_price <- vv(carrot_price_g*carrot_phosphorus, n=n_year, var_CV=100)
  
  carrot_potassium <- vv(carrot_potassium, n=n_year, var_CV=100)
  carrot_potassium_price <- vv(carrot_price_g*carrot_potassium, n=n_year, var_CV=100)
  
  carrot_sodium <- vv(carrot_sodium,  n=n_year, var_CV=100)
  carrot_sodium_price <- vv(carrot_price_g*carrot_sodium, n=n_year, var_CV=100)
  
  carrot_nutrient_precal <- sum(carrot_calories_price, 
                                carrot_protein_price, 
                                carrot_fat_price,
                                carrot_carbohydrates_price, 
                                carrot_fiber_price, 
                                carrot_vitA_price, 
                                carrot_vitB3_price, 
                                carrot_vitC_price, 
                                carrot_folate_price, 
                                carrot_calcium_price,
                                carrot_iron_price, 
                                carrot_magnesium_price, 
                                carrot_phosphorus_price,
                                carrot_potassium, 
                                rice_sodium)
  carrot_nutrient <- vv(carrot_nutrient_precal, n=n_year, var_CV=100)
  
  #total benefits of carrot
  carrot_total_benefits_precal <- sum(carrot_income, carrot_wateruse, carrot_nutrient) 
  carrot_total_benefit <- vv(carrot_total_benefits_precal, n=n_year, var_CV=100)
  
  
  #cabbage
  cabbage_income <- vv(cabbage_yield * cabbage_price, n=n_year, var_CV=100)
  
  # Estimate  'price' for water use
  cabbage_water_use_price <- vv(cabbage_income/cabbage_water_use, n=n_year, var_CV=100)
  cabbage_wateruse <- vv(cabbage_water_use_price, n=n_year, var_CV=100)
  
  ##convert yield to g to calculate nutrient 'price' in cabbage
  cabbage_yield_g <- vv(cabbage_yield*1000, n=n_year, var_CV=100)
  
  ##estimate  cabbage price per g
  cabbage_price_g <- vv(cabbage_yield_g * cabbage_price, n=n_year, var_CV=100)
  
  ##estimate  nutrient price in cabbage
  cabbage_calories <- vv(cabbage_calories, n=n_year, var_CV=100)
  cabbage_calories_price <- vv(cabbage_price_g*cabbage_calories, n=n_year, var_CV=100)
  
  cabbage_protein <- vv(cabbage_protein, n=n_year, var_CV=100)
  cabbage_protein_price <- vv(cabbage_price_g*cabbage_protein, n=n_year, var_CV=100)
  
  cabbage_fat <- vv(cabbage_fat, n=n_year, var_CV=100)
  cabbage_fat_price <- vv(cabbage_price_g*cabbage_fat, n=n_year, var_CV=100)
  
  cabbage_carbohydrates <- vv(cabbage_carbohydrates, n=n_year, var_CV=100)
  cabbage_carbohydrates_price <- vv(cabbage_price_g*cabbage_carbohydrates, n=n_year, var_CV=100)
  
  cabbage_fiber <- vv(cabbage_fiber, n=n_year, var_CV=100)
  cabbage_fiber_price <- vv(cabbage_price_g*cabbage_fiber, n=n_year, var_CV=100)
  
  cabbage_vitA <- vv(cabbage_vitA, n=n_year, var_CV=100)
  cabbage_vitA_price <- vv(cabbage_price_g*cabbage_vitA, n=n_year, var_CV=100)
  
  cabbage_vitB3 <- vv(cabbage_vitB3, n=n_year, var_CV=100)
  cabbage_vitB3_price <- vv(cabbage_price_g*cabbage_vitB3, n=n_year, var_CV=100)
  
  cabbage_vitC <- vv(cabbage_vitB3, n=n_year, var_CV=100)
  cabbage_vitC_price <- vv(cabbage_price_g*cabbage_vitC, n=n_year, var_CV=100)
  
  cabbage_folate <- vv(cabbage_folate, n=n_year, var_CV=100)
  cabbage_folate_price <- vv(cabbage_price_g*cabbage_folate, n=n_year, var_CV=100)
  
  cabbage_calcium <- vv(cabbage_folate,  n=n_year, var_CV=100)
  cabbage_calcium_price <- vv(cabbage_price_g*cabbage_calcium, n=n_year, var_CV=100)
  
  cabbage_iron <- vv(cabbage_iron,  n=n_year, var_CV=100)
  cabbage_iron_price <- vv(cabbage_price_g*cabbage_iron, n=n_year, var_CV=100)
  
  cabbage_magnesium <- vv(cabbage_magnesium, n=n_year, var_CV=100)
  cabbage_magnesium_price <- vv(cabbage_price_g*cabbage_magnesium, n=n_year, var_CV=100)
  
  cabbage_phosphorus <- vv(cabbage_phosphorus, n=n_year, var_CV=100)
  cabbage_phosphorus_price <- vv(cabbage_price_g*cabbage_phosphorus, n=n_year, var_CV=100)
  
  cabbage_potassium <- vv(cabbage_potassium, n=n_year, var_CV=100)
  cabbage_potassium_price <- vv(cabbage_price_g*cabbage_potassium, n=n_year, var_CV=100)
  
  cabbage_sodium <- vv(cabbage_sodium,  n=n_year, var_CV=100)
  cabbage_sodium_price <- vv(cabbage_price_g*cabbage_sodium, n=n_year, var_CV=100)
  
  cabbage_nutrient_precal <- sum(cabbage_calories_price, 
                                cabbage_protein_price, 
                                cabbage_fat_price,
                                cabbage_carbohydrates_price, 
                                cabbage_fiber_price, 
                                cabbage_vitA_price, 
                                cabbage_vitB3_price, 
                                cabbage_vitC_price, 
                                cabbage_folate_price, 
                                cabbage_calcium_price,
                                cabbage_iron_price, 
                                cabbage_magnesium_price, 
                                cabbage_phosphorus_price,
                                cabbage_potassium, 
                                rice_sodium)
  cabbage_nutrient <- vv(cabbage_nutrient_precal, n=n_year, var_CV=100)
  
  #total benefits of cabbage
  cabbage_total_benefits_precal <- sum(cabbage_income, cabbage_wateruse, cabbage_nutrient) 
  cabbage_total_benefit <- vv(cabbage_total_benefits_precal, n=n_year, var_CV=100)
  
  
  #Estimate costs with intervention (vegetables cultivation) --> to be calculated for each crop
  
  warehouse <- vv(warehouse/25, n=n_year, var_CV=100)
  pipes <- vv(pipe_installation/25, n=n_year, var_CV=100)
  
  ##potato costs
  potato_production_cost <- vv(potato_production_cost, n=n_year, var_CV=100)
  #total cost
  potato_total_cost_precal <- sum(potato_production_cost, warehouse, pipes, warehouse_maintenance_cost)
  potato_total_cost <- vv(potato_total_cost_precal, n=n_year, var_CV=100)
  
  ##shallot costs
  shallot_production_cost <- vv(shallot_production_cost, n=n_year, var_CV=100)
  #total cost
  shallot_total_cost_precal <- sum(shallot_production_cost, warehouse, pipes, warehouse_maintenance_cost)
  shallot_total_cost <- vv(shallot_total_cost_precal, n=n_year, var_CV=100)
  
  
  ##chili costs
  chili_production_cost <- vv(chili_production_cost, n=n_year, var_CV=100)
  #total cost
  chili_total_cost_precal <- sum(chili_production_cost, warehouse, pipes, warehouse_maintenance_cost)
  chili_total_cost <- vv(chili_total_cost_precal, n=n_year, var_CV=100)
  
  ##carrot costs
  carrot_production_cost <- vv(carrot_production_cost, n=n_year, var_CV=100)
  #total cost
  carrot_total_cost_precal <- sum(carrot_production_cost, warehouse, pipes, warehouse_maintenance_cost)
  carrot_total_cost <- vv(carrot_total_cost_precal, n=n_year, var_CV=100)
  
  ##cabbage costs
  cabbage_production_cost <- vv(cabbage_production_cost, n=n_year, var_CV=100)
  #total cost
  cabbage_total_cost_precal <- sum(cabbage_production_cost, warehouse, pipes, warehouse_maintenance_cost)
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
  
  #vegetable result
  vegetable_result_precal <- sum(potato_result, shallot_result, chili_result,
                                 carrot_result, cabbage_result)
  vegetable_result <- vv(vegetable_result_precal, n=n_year, var_CV=100)
  
  
  
  ## Intervention vegetable 1 (potato, shallot, carrot, cabbage)
  intervention_vegetable1_precal <- sum(potato_result, shallot_result, carrot_result, cabbage_result)
  intervention_vegetable1 <- vv(intervention_vegetable1_precal, n=n_year, var_CV=100)
  
  ## Intervention vegetable 2 (potato, shallot, carrot)
  intervention_vegetable2_precal <- sum(potato_result, shallot_result, carrot_result)
  intervention_vegetable2 <- vv(intervention_vegetable2_precal, n=n_year, var_CV=100)
  
  ## Intervention vegetable 3 (potato, shallot, cabbage)
  intervention_vegetable3_precal <- sum(potato_result, shallot_result, cabbage_result)
  intervention_vegetable3 <- vv(intervention_vegetable3_precal, n=n_year, var_CV=100)
  
  ## Intervention vegetable 4 (potato, carrot, cabbage)
  intervention_vegetable4_precal <- sum(potato_result, carrot_result, cabbage_result)
  intervention_vegetable4 <- vv(intervention_vegetable4_precal, n=n_year, var_CV=100)
  
  ## Intervention vegetable 5 (chili, potato)
  intervention_vegetable5_precal <- sum(potato_result, chili_result)
  intervention_vegetable5 <- vv(intervention_vegetable5_precal, n=n_year, var_CV=100)
  
  ## Intervention vegetable 6 (chili, shallot)
  intervention_vegetable6_precal <- sum(chili_result, shallot_result)
  intervention_vegetable6 <- vv(intervention_vegetable6_precal, n=n_year, var_CV=100)
  
  ## Intervention vegetable 7 (chili, carrot)
  intervention_vegetable7_precal <- sum(chili_result, carrot_result)
  intervention_vegetable7 <- vv(intervention_vegetable7_precal, n=n_year, var_CV=100)
  
  ## Intervention vegetable 8 (chili, cabbage)
  intervention_vegetable8_precal <- sum(chili_result, cabbage_result)
  intervention_vegetable8 <- vv(intervention_vegetable8_precal, n=n_year, var_CV=100)
  
  
  
  #NPV
  NPV_intervention_vegetable1 <- discount(intervention_vegetable1, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable2 <- discount(intervention_vegetable2, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable3 <- discount(intervention_vegetable3, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable4 <- discount(intervention_vegetable4, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable5 <- discount(intervention_vegetable5, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable6 <- discount(intervention_vegetable6, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable7 <- discount(intervention_vegetable7, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable8 <- discount(intervention_vegetable8, discount_rate, calculate_NPV = TRUE)
  NPV_no_intervention_rice <- discount(rice_result, discount_rate, calculate_NPV = TRUE)
  
  
  #Cashflow
  cashflow_veg1 <- intervention_vegetable1 - rice_result
  cashflow_veg2 <- intervention_vegetable2 - rice_result
  cashflow_veg3 <- intervention_vegetable3 - rice_result
  cashflow_veg4 <- intervention_vegetable4 - rice_result
  cashflow_veg5 <- intervention_vegetable5 - rice_result
  cashflow_veg6 <- intervention_vegetable6 - rice_result
  cashflow_veg7 <- intervention_vegetable7 - rice_result
  cashflow_veg8 <- intervention_vegetable8 - rice_result
  
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(Interv_veg1_NPV = NPV_intervention_vegetable1,
              Interv_veg2_NPV = NPV_intervention_vegetable2,
              Interv_veg3_NPV = NPV_intervention_vegetable3,
              Interv_veg4_NPV = NPV_intervention_vegetable4,
              Interv_veg5_NPV = NPV_intervention_vegetable5,
              Interv_veg6_NPV = NPV_intervention_vegetable6,
              Interv_veg7_NPV = NPV_intervention_vegetable7,
              Interv_veg8_NPV = NPV_intervention_vegetable8,
              NO_Interv_rice_NPV = NPV_no_intervention_rice,
              NPV_decision_veg1 = NPV_intervention_vegetable1 - NPV_no_intervention_rice,
              NPV_decision_veg2 = NPV_intervention_vegetable2 - NPV_no_intervention_rice,
              NPV_decision_veg3 = NPV_intervention_vegetable3 - NPV_no_intervention_rice,
              NPV_decision_veg4 = NPV_intervention_vegetable4 - NPV_no_intervention_rice,
              NPV_decision_veg5 = NPV_intervention_vegetable5 - NPV_no_intervention_rice,
              NPV_decision_veg6 = NPV_intervention_vegetable6 - NPV_no_intervention_rice,
              NPV_decision_veg7 = NPV_intervention_vegetable7 - NPV_no_intervention_rice,
              NPV_decision_veg8 = NPV_intervention_vegetable8 - NPV_no_intervention_rice,
              Cashflow_decision_veg1 = cashflow_veg1,
              Cashflow_decision_veg2 = cashflow_veg2,
              Cashflow_decision_veg3 = cashflow_veg3,
              Cashflow_decision_veg4 = cashflow_veg4,
              Cashflow_decision_veg5 = cashflow_veg5,
              Cashflow_decision_veg6 = cashflow_veg6,
              Cashflow_decision_veg7 = cashflow_veg7,
              Cashflow_decision_veg8 = cashflow_veg8
  ))
}




# Run the Monte Carlo simulation using the model function
input_estimates <- read.csv("20231219_new_variable_estimates~fix.csv", sep = ";")

TRV_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                  model_function = transition_rice_to_vegetables,
                                  numberOfModelRuns = 1000,
                                  functionSyntax = "plainNames")




# plot NPV distribution analysis
decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_veg1_NPV", 
                                             "Interv_veg2_NPV",
                                             "Interv_veg3_NPV",
                                             "Interv_veg4_NPV",
                                             "Interv_veg5_NPV",
                                             "Interv_veg6_NPV",
                                             "Interv_veg7_NPV",
                                             "Interv_veg2_NPV",
                                             "NO_Interv_rice_NPV"),
                                    method = 'smooth_simple_overlay')

decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_veg1_NPV",
                                             "NO_Interv_rice_NPV"),
                                    method = 'smooth_simple_overlay')

decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_veg2_NPV",
                                             "NO_Interv_rice_NPV"),
                                    method = 'smooth_simple_overlay')

decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_veg3_NPV",
                                             "NO_Interv_rice_NPV"),
                                    method = 'smooth_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_veg4_NPV",
                                             "NO_Interv_rice_NPV"),
                                    method = 'smooth_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_veg5_NPV",
                                             "NO_Interv_rice_NPV"),
                                    method = 'smooth_simple_overlay')

decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_veg6_NPV",
                                             "NO_Interv_rice_NPV"),
                                    method = 'smooth_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_veg7_NPV",
                                             "NO_Interv_rice_NPV"),
                                    method = 'smooth_simple_overlay')

decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_veg8_NPV",
                                             "NO_Interv_rice_NPV"),
                                    method = 'smooth_simple_overlay')




decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_veg1_NPV", 
                                             "Interv_veg2_NPV",
                                             "Interv_veg3_NPV",
                                             "Interv_veg4_NPV",
                                             "Interv_veg5_NPV",
                                             "Interv_veg6_NPV",
                                             "Interv_veg7_NPV",
                                             "Interv_veg2_NPV",
                                             "NO_Interv_rice_NPV"),
                                    method = 'boxplot')


# cashflow analysis
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg1")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg2")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg3")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg4")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg5")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg6")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg7")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg8")




# Projection to Latent Structures (PLS) analysis
pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[10], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[11], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[12], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[13], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[14], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[15], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[16], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[17], ncomp = 1)
plot_pls(pls_result, threshold = 0)



# VoI analysis
mcSimulation_table <- data.frame(TRV_mc_simulation$x, TRV_mc_simulation$y[1:17])

evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_veg1_NPV")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_veg1")


evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_veg2_NPV")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_veg2")

evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_veg3_NPV")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_veg3")

evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_veg4_NPV")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_veg4")

evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_veg5_NPV")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_veg5")

evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_veg6_NPV")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_veg6")

evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_veg7_NPV")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_veg7")

evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_veg8_NPV")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_veg8")



compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_veg1", 
                cashflow_var_name = "Cashflow_decision_veg1", 
                base_size = 7)


compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_veg2", 
                cashflow_var_name = "Cashflow_decision_veg2", 
                base_size = 7)

compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_veg3", 
                cashflow_var_name = "Cashflow_decision_veg3", 
                base_size = 7)

compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_veg4", 
                cashflow_var_name = "Cashflow_decision_veg4", 
                base_size = 7)

compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_veg5", 
                cashflow_var_name = "Cashflow_decision_veg5", 
                base_size = 7)

compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_veg6", 
                cashflow_var_name = "Cashflow_decision_veg6", 
                base_size = 7)


compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_veg7", 
                cashflow_var_name = "Cashflow_decision_veg7", 
                base_size = 7)

compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_veg8", 
                cashflow_var_name = "Cashflow_decision_veg8", 
                base_size = 7)


