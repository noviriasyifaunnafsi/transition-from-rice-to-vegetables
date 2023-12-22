library(tidyverse)
library(ggplot2)
library(dplyr)
library(decisionSupport)


transition_rice_to_vegetables <- function(){
  
  
  # Estimate the benefits of no intervention (rice cultivation)
  rice_revenue <- vv(rice_yield * rice_price, n=n_year, var_CV=100)
  
  # Estimate the benefit of water use
  rice_wateruse <- vv(rice_revenue/rice_water_use, n=n_year, var_CV=100)
  
  ##estimate  the nutrient benefit in rice
  rice_calories_value <- vv(rice_price/rice_calories*rice_yield, n=n_year, var_CV=100)
  rice_protein_value <- vv(rice_price/rice_protein*rice_yield, n=n_year, var_CV=100)
  rice_fat_value <- vv(rice_price/rice_fat*rice_yield, n=n_year, var_CV=100)
  rice_carbohydrates_value <- vv(rice_price/rice_carbohydrates*rice_yield, n=n_year, var_CV=100)
  rice_fiber_value <- vv(rice_price/rice_fiber*rice_yield, n=n_year, var_CV=100)
  rice_vitA_value <- vv(rice_price/rice_vitA*rice_yield, n=n_year, var_CV=100)
  rice_vitC_value <- vv(rice_price/rice_vitC*rice_yield, n=n_year, var_CV=100)
  rice_folate_value <- vv(rice_price/rice_folate*rice_yield, n=n_year, var_CV=100)
  rice_calcium_value <- vv(rice_price/rice_calcium*rice_yield, n=n_year, var_CV=100)
  rice_iron_value <- vv(rice_price/rice_iron*rice_yield, n=n_year, var_CV=100)
  rice_magnesium_value <- vv(rice_price/rice_magnesium*rice_yield, n=n_year, var_CV=100)
  rice_phosphorus_value <- vv(rice_price/rice_phosphorus*rice_yield, n=n_year, var_CV=100)
  rice_potassium_value <- vv(rice_price/rice_potassium*rice_yield, n=n_year, var_CV=100)
  rice_sodium_value <- vv(rice_price/rice_sodium*rice_yield, n=n_year, var_CV=100)

  rice_nutrient_precal <- sum(rice_calories_value, 
                              rice_protein_value, 
                              rice_fat_value,
                              rice_carbohydrates_value, 
                              rice_fiber_value, 
                              rice_vitA_value, 
                              rice_vitC_value, 
                              rice_folate_value, 
                              rice_calcium_value,
                              rice_iron_value, 
                              rice_magnesium_value, 
                              rice_phosphorus_value,
                              rice_potassium_value, 
                              rice_sodium_value)
  rice_nutrient <- vv(rice_nutrient_precal, n=n_year, var_CV=100)
  
  
  
  #total benefits results of no intervention (rice) per year (3 season)
  rice_total_benefits_precal <- sum(rice_revenue, rice_wateruse, rice_nutrient)*3
  rice_total_benefit <- vv(rice_total_benefits_precal, n=n_year, var_CV=100)
  
  
  
  #Estimate the costs of no intervention
  rice_production_cost <- vv(rice_production_cost*3, n=n_year, var_CV=100)
  rice_warehouse <- vv(warehouse/25, n=n_year, var_CV=100)
  #total cost
  rice_total_cost_precal <- sum(rice_production_cost, rice_warehouse)
  rice_total_cost <- vv(rice_total_cost_precal, n=n_year, var_CV=100)
  
  
  #rice result
  rice_result_precal <- rice_total_benefit - rice_total_cost
  rice_result <- vv(rice_result_precal, n=n_year, var_CV=100)
  
  
  
  #Estimate the benefits with intervention (vegetables cultivation) --> to be calculated for each crop
  
  #potato
  potato_revenue <- vv(potato_yield * potato_price, n=n_year, var_CV=100)
  
  # Estimate  the benefit of water use
  potato_wateruse <- vv(potato_revenue/potato_water_use, n=n_year, var_CV=100)
  
  ##estimate  the nutrient benefit in potato
  potato_calories_value <- vv(potato_price/potato_calories*potato_yield, n=n_year, var_CV=100)
potato_protein_value <- vv(potato_price/potato_protein*potato_yield, n=n_year, var_CV=100)
potato_fat_value <- vv(potato_price/potato_fat*potato_yield, n=n_year, var_CV=100)
potato_carbohydrates_value <- vv(potato_price/potato_carbohydrates*potato_yield, n=n_year, var_CV=100)
potato_fiber_value <- vv(potato_price/potato_fiber*potato_yield, n=n_year, var_CV=100)
potato_vitA_value <- vv(potato_price/potato_vitA*potato_yield, n=n_year, var_CV=100)
potato_vitC_value <- vv(potato_price/potato_vitC*potato_yield, n=n_year, var_CV=100)
potato_folate_value <- vv(potato_price/potato_folate*potato_yield, n=n_year, var_CV=100)
potato_calcium_value <- vv(potato_price/potato_calcium*potato_yield, n=n_year, var_CV=100)
potato_iron_value <- vv(potato_price/potato_iron*potato_yield, n=n_year, var_CV=100)
potato_magnesium_value <- vv(potato_price/potato_magnesium*potato_yield, n=n_year, var_CV=100)
potato_phosphorus_value <- vv(potato_price/potato_phosphorus*potato_yield, n=n_year, var_CV=100)
potato_potassium_value <- vv(potato_price/potato_potassium*potato_yield, n=n_year, var_CV=100)
potato_sodium_value <- vv(potato_price/potato_sodium*potato_yield, n=n_year, var_CV=100)
  
  potato_nutrient_precal <- sum(potato_calories_value, 
                                potato_protein_value, 
                                potato_fat_value,
                                potato_carbohydrates_value, 
                                potato_fiber_value, 
                                potato_vitA_value, 
                                potato_vitC_value, 
                                potato_folate_value, 
                                potato_calcium_value,
                                potato_iron_value, 
                                potato_magnesium_value, 
                                potato_phosphorus_value,
                                potato_potassium_value, 
                                potato_sodium_value)
  potato_nutrient <- vv(potato_nutrient_precal, n=n_year, var_CV=100)
  
  #total benefits of potato
  potato_total_benefits_precal <- sum(potato_revenue, potato_wateruse, potato_nutrient)
  potato_total_benefit <- vv(potato_total_benefits_precal, n=n_year, var_CV=100)
  
  
  #shallot
  shallot_revenue <- vv(shallot_yield * shallot_price, n=n_year, var_CV=100)
  
  # Estimate  the benefit of water use
  shallot_wateruse <- vv(shallot_revenue/shallot_water_use, n=n_year, var_CV=100)
  
  ##estimate  the nutrient benefit in shallot
  shallot_calories_value <- vv(shallot_price/shallot_calories*shallot_yield, n=n_year, var_CV=100)
shallot_protein_value <- vv(shallot_price/shallot_protein*shallot_yield, n=n_year, var_CV=100)
shallot_fat_value <- vv(shallot_price/shallot_fat*shallot_yield, n=n_year, var_CV=100)
shallot_carbohydrates_value <- vv(shallot_price/shallot_carbohydrates*shallot_yield, n=n_year, var_CV=100)
shallot_fiber_value <- vv(shallot_price/shallot_fiber*shallot_yield, n=n_year, var_CV=100)
shallot_vitA_value <- vv(shallot_price/shallot_vitA*shallot_yield, n=n_year, var_CV=100)
shallot_vitC_value <- vv(shallot_price/shallot_vitC*shallot_yield, n=n_year, var_CV=100)
shallot_folate_value <- vv(shallot_price/shallot_folate*shallot_yield, n=n_year, var_CV=100)
shallot_calcium_value <- vv(shallot_price/shallot_calcium*shallot_yield, n=n_year, var_CV=100)
shallot_iron_value <- vv(shallot_price/shallot_iron*shallot_yield, n=n_year, var_CV=100)
shallot_magnesium_value <- vv(shallot_price/shallot_magnesium*shallot_yield, n=n_year, var_CV=100)
shallot_phosphorus_value <- vv(shallot_price/shallot_phosphorus*shallot_yield, n=n_year, var_CV=100)
shallot_potassium_value <- vv(shallot_price/shallot_potassium*shallot_yield, n=n_year, var_CV=100)
shallot_sodium_value <- vv(shallot_price/shallot_sodium*shallot_yield, n=n_year, var_CV=100) 

  shallot_nutrient_precal <- sum(shallot_calories_value, 
                                 shallot_protein_value, 
                                 shallot_fat_value,
                                 shallot_carbohydrates_value, 
                                 shallot_fiber_value, 
                                 shallot_vitA_value, 
                                 shallot_vitC_value, 
                                 shallot_folate_value, 
                                 shallot_calcium_value,
                                 shallot_iron_value, 
                                 shallot_magnesium_value, 
                                 shallot_phosphorus_value,
                                 shallot_potassium_value, 
                                 shallot_sodium_value)
  shallot_nutrient <- vv(shallot_nutrient_precal, n=n_year, var_CV=100)
  
  
  #total benefits of shallot
  shallot_total_benefits_precal <- sum(shallot_revenue, shallot_wateruse, shallot_nutrient) 
  shallot_total_benefit <- vv(shallot_total_benefits_precal, n=n_year, var_CV=100)
  
  
  
  #chili
  chili_revenue <- vv(chili_yield * chili_price, n=n_year, var_CV=100)
  
  # Estimate the benefit of water use
  chili_wateruse <- vv(chili_revenue/chili_water_use, n=n_year, var_CV=100)
  
  ##estimate the  nutrient benefit in chili
  chili_calories_value <- vv(chili_price/chili_calories*chili_yield, n=n_year, var_CV=100)
chili_protein_value <- vv(chili_price/chili_protein*chili_yield, n=n_year, var_CV=100)
chili_fat_value <- vv(chili_price/chili_fat*chili_yield, n=n_year, var_CV=100)
chili_carbohydrates_value <- vv(chili_price/chili_carbohydrates*chili_yield, n=n_year, var_CV=100)
chili_fiber_value <- vv(chili_price/chili_fiber*chili_yield, n=n_year, var_CV=100)
chili_vitA_value <- vv(chili_price/chili_vitA*chili_yield, n=n_year, var_CV=100)
chili_vitC_value <- vv(chili_price/chili_vitC*chili_yield, n=n_year, var_CV=100)
chili_folate_value <- vv(chili_price/chili_folate*chili_yield, n=n_year, var_CV=100)
chili_calcium_value <- vv(chili_price/chili_calcium*chili_yield, n=n_year, var_CV=100)
chili_iron_value <- vv(chili_price/chili_iron*chili_yield, n=n_year, var_CV=100)
chili_magnesium_value <- vv(chili_price/chili_magnesium*chili_yield, n=n_year, var_CV=100)
chili_phosphorus_value <- vv(chili_price/chili_phosphorus*chili_yield, n=n_year, var_CV=100)
chili_potassium_value <- vv(chili_price/chili_potassium*chili_yield, n=n_year, var_CV=100)
chili_sodium_value <- vv(chili_price/chili_sodium*chili_yield, n=n_year, var_CV=100)
  
  chili_nutrient_precal <- sum(chili_calories_value, 
                               chili_protein_value, 
                               chili_fat_value,
                               chili_carbohydrates_value, 
                               chili_fiber_value, 
                               chili_vitA_value, 
                               chili_vitC_value, 
                               chili_folate_value, 
                               chili_calcium_value,
                               chili_iron_value, 
                               chili_magnesium_value, 
                               chili_phosphorus_value,
                               chili_potassium_value, 
                               chili_sodium_value)
  chili_nutrient <- vv(chili_nutrient_precal, n=n_year, var_CV=100)
  
  
  #total benefits of chili
  chili_total_benefits_precal <- sum(chili_revenue, chili_wateruse, chili_nutrient) 
  chili_total_benefit <- vv(chili_total_benefits_precal, n=n_year, var_CV=100)
  
  
  #carrot
  carrot_revenue <- vv(carrot_yield * carrot_price, n=n_year, var_CV=100)
  
  # Estimate  the benefit of water use
  carrot_wateruse <- vv(carrot_revenue/carrot_water_use, n=n_year, var_CV=100)
  
  ##estimate the nutrient benefit in carrot
  carrot_calories_value <- vv(carrot_price/carrot_calories*carrot_yield, n=n_year, var_CV=100)
carrot_protein_value <- vv(carrot_price/carrot_protein*carrot_yield, n=n_year, var_CV=100)
carrot_fat_value <- vv(carrot_price/carrot_fat*carrot_yield, n=n_year, var_CV=100)
carrot_carbohydrates_value <- vv(carrot_price/carrot_carbohydrates*carrot_yield, n=n_year, var_CV=100)
carrot_fiber_value <- vv(carrot_price/carrot_fiber*carrot_yield, n=n_year, var_CV=100)
carrot_vitA_value <- vv(carrot_price/carrot_vitA*carrot_yield, n=n_year, var_CV=100)
carrot_vitC_value <- vv(carrot_price/carrot_vitC*carrot_yield, n=n_year, var_CV=100)
carrot_folate_value <- vv(carrot_price/carrot_folate*carrot_yield, n=n_year, var_CV=100)
carrot_calcium_value <- vv(carrot_price/carrot_calcium*carrot_yield, n=n_year, var_CV=100)
carrot_iron_value <- vv(carrot_price/carrot_iron*carrot_yield, n=n_year, var_CV=100)
carrot_magnesium_value <- vv(carrot_price/carrot_magnesium*carrot_yield, n=n_year, var_CV=100)
carrot_phosphorus_value <- vv(carrot_price/carrot_phosphorus*carrot_yield, n=n_year, var_CV=100)
carrot_potassium_value <- vv(carrot_price/carrot_potassium*carrot_yield, n=n_year, var_CV=100)
carrot_sodium_value <- vv(carrot_price/carrot_sodium*carrot_yield, n=n_year, var_CV=100)
  
  carrot_nutrient_precal <- sum(carrot_calories_value, 
                                carrot_protein_value, 
                                carrot_fat_value,
                                carrot_carbohydrates_value, 
                                carrot_fiber_value, 
                                carrot_vitA_value, 
                                carrot_vitC_value, 
                                carrot_folate_value, 
                                carrot_calcium_value,
                                carrot_iron_value, 
                                carrot_magnesium_value, 
                                carrot_phosphorus_value,
                                carrot_potassium_value, 
                                carrot_sodium_value)
  carrot_nutrient <- vv(carrot_nutrient_precal, n=n_year, var_CV=100)
  
  
  #total benefits of carrot
  carrot_total_benefits_precal <- sum(carrot_revenue, carrot_wateruse, carrot_nutrient) 
  carrot_total_benefit <- vv(carrot_total_benefits_precal, n=n_year, var_CV=100)
  
  
  #cabbage
  cabbage_revenue <- vv(cabbage_yield * cabbage_price, n=n_year, var_CV=100)
  
  # Estimate  the benefit for water use
  cabbage_wateruse <- vv(cabbage_revenue/cabbage_water_use, n=n_year, var_CV=100)
  
  ##estimate  the nutrient benefit in cabbage
  cabbage_calories_value <- vv(cabbage_price/cabbage_calories*cabbage_yield, n=n_year, var_CV=100)
cabbage_protein_value <- vv(cabbage_price/cabbage_protein*cabbage_yield, n=n_year, var_CV=100)
cabbage_fat_value <- vv(cabbage_price/cabbage_fat*cabbage_yield, n=n_year, var_CV=100)
cabbage_carbohydrates_value <- vv(cabbage_price/cabbage_carbohydrates*cabbage_yield, n=n_year, var_CV=100)
cabbage_fiber_value <- vv(cabbage_price/cabbage_fiber*cabbage_yield, n=n_year, var_CV=100)
cabbage_vitA_value <- vv(cabbage_price/cabbage_vitA*cabbage_yield, n=n_year, var_CV=100)
cabbage_vitC_value <- vv(cabbage_price/cabbage_vitC*cabbage_yield, n=n_year, var_CV=100)
cabbage_folate_value <- vv(cabbage_price/cabbage_folate*cabbage_yield, n=n_year, var_CV=100)
cabbage_calcium_value <- vv(cabbage_price/cabbage_calcium*cabbage_yield, n=n_year, var_CV=100)
cabbage_iron_value <- vv(cabbage_price/cabbage_iron*cabbage_yield, n=n_year, var_CV=100)
cabbage_magnesium_value <- vv(cabbage_price/cabbage_magnesium*cabbage_yield, n=n_year, var_CV=100)
cabbage_phosphorus_value <- vv(cabbage_price/cabbage_phosphorus*cabbage_yield, n=n_year, var_CV=100)
cabbage_potassium_value <- vv(cabbage_price/cabbage_potassium*cabbage_yield, n=n_year, var_CV=100)
cabbage_sodium_value <- vv(cabbage_price/cabbage_sodium*cabbage_yield, n=n_year, var_CV=100)
  
  cabbage_nutrient_precal <- sum(cabbage_calories_value, 
                                 cabbage_protein_value, 
                                 cabbage_fat_value,
                                 cabbage_carbohydrates_value, 
                                 cabbage_fiber_value, 
                                 cabbage_vitA_value, 
                                 cabbage_vitC_value, 
                                 cabbage_folate_value, 
                                 cabbage_calcium_value,
                                 cabbage_iron_value, 
                                 cabbage_magnesium_value, 
                                 cabbage_phosphorus_value,
                                 cabbage_potassium_value, 
                                 cabbage_sodium_value)
  cabbage_nutrient <- vv(cabbage_nutrient_precal, n=n_year, var_CV=100)
  
  #total benefits of cabbage
  cabbage_total_benefits_precal <- sum(cabbage_revenue, cabbage_wateruse, cabbage_nutrient)
  cabbage_total_benefit <- vv(cabbage_total_benefits_precal, n=n_year, var_CV=100)
  
  
  #Estimate costs with intervention (vegetables cultivation) --> to be calculated for each crop
  
  warehouse <- vv(warehouse/25, n=n_year, var_CV=100)
  warehouse_maintenance_cost <- vv(warehouse_maintenance_cost, n=n_year, var_CV=100)
  
  ##potato costs
  potato_production_cost <- vv(potato_production_cost, n=n_year, var_CV=100)
  #total cost
  potato_total_cost_precal <- sum(potato_production_cost, warehouse, warehouse_maintenance_cost)
  potato_total_cost <- vv(potato_total_cost_precal, n=n_year, var_CV=100)
  
  ##shallot costs
  shallot_production_cost <- vv(shallot_production_cost, n=n_year, var_CV=100)
  #total cost
  shallot_total_cost_precal <- sum(shallot_production_cost, warehouse, warehouse_maintenance_cost)
  shallot_total_cost <- vv(shallot_total_cost_precal, n=n_year, var_CV=100)
  
  
  ##chili costs
  chili_production_cost <- vv(chili_production_cost, n=n_year, var_CV=100)
  #total cost
  chili_total_cost_precal <- sum(chili_production_cost, warehouse, warehouse_maintenance_cost)
  chili_total_cost <- vv(chili_total_cost_precal, n=n_year, var_CV=100)
  
  ##carrot costs
  carrot_production_cost <- vv(carrot_production_cost, n=n_year, var_CV=100)
  #total cost
  carrot_total_cost_precal <- sum(carrot_production_cost, warehouse, warehouse_maintenance_cost)
  carrot_total_cost <- vv(carrot_total_cost_precal, n=n_year, var_CV=100)
  
  ##cabbage costs
  cabbage_production_cost <- vv(cabbage_production_cost, n=n_year, var_CV=100)
  #total cost
  cabbage_total_cost_precal <- sum(cabbage_production_cost, warehouse, warehouse_maintenance_cost)
  cabbage_total_cost <- vv(cabbage_total_cost_precal, n=n_year, var_CV=100)
  
  
  #vegetable result
  potato_result <- vv(potato_total_benefit - potato_total_cost, n=n_year, var_CV=100)
  shallot_result <- vv(shallot_total_benefit - shallot_total_cost, n=n_year, var_CV=100)
  chili_result <- vv(chili_total_benefit - chili_total_cost, n=n_year, var_CV=100)
  carrot_result <- vv(carrot_total_benefit - carrot_total_cost, n=n_year, var_CV=100)
  cabbage_result <- vv(cabbage_total_benefit - cabbage_total_cost, n=n_year, var_CV=100)
  
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
  
  ## Intervention vegetable 5 (chili, shallot)
  intervention_vegetable5_precal <- sum(chili_result, shallot_result)
  intervention_vegetable5 <- vv(intervention_vegetable5_precal, n=n_year, var_CV=100)
  
  ## Intervention vegetable 6 (chili, carrot)
  intervention_vegetable6_precal <- sum(chili_result, carrot_result)
  intervention_vegetable6 <- vv(intervention_vegetable6_precal, n=n_year, var_CV=100)
  
  
  
  #NPV
  NPV_intervention_vegetable1 <- discount(intervention_vegetable1, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable2 <- discount(intervention_vegetable2, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable3 <- discount(intervention_vegetable3, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable4 <- discount(intervention_vegetable4, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable5 <- discount(intervention_vegetable5, discount_rate, calculate_NPV = TRUE)
  NPV_intervention_vegetable6 <- discount(intervention_vegetable6, discount_rate, calculate_NPV = TRUE)
  NPV_no_intervention_rice <- discount(rice_result, discount_rate, calculate_NPV = TRUE)
  
  
  #Cashflow
  cashflow_veg1 <- intervention_vegetable1 - rice_result
  cashflow_veg2 <- intervention_vegetable2 - rice_result
  cashflow_veg3 <- intervention_vegetable3 - rice_result
  cashflow_veg4 <- intervention_vegetable4 - rice_result
  cashflow_veg5 <- intervention_vegetable5 - rice_result
  cashflow_veg6 <- intervention_vegetable6 - rice_result
  
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(Interv_veg1_NPV = NPV_intervention_vegetable1,
              Interv_veg2_NPV = NPV_intervention_vegetable2,
              Interv_veg3_NPV = NPV_intervention_vegetable3,
              Interv_veg4_NPV = NPV_intervention_vegetable4,
              Interv_veg5_NPV = NPV_intervention_vegetable5,
              Interv_veg6_NPV = NPV_intervention_vegetable6,
              NO_Interv_rice_NPV = NPV_no_intervention_rice,
              NPV_decision_veg1 = NPV_intervention_vegetable1 - NPV_no_intervention_rice,
              NPV_decision_veg2 = NPV_intervention_vegetable2 - NPV_no_intervention_rice,
              NPV_decision_veg3 = NPV_intervention_vegetable3 - NPV_no_intervention_rice,
              NPV_decision_veg4 = NPV_intervention_vegetable4 - NPV_no_intervention_rice,
              NPV_decision_veg5 = NPV_intervention_vegetable5 - NPV_no_intervention_rice,
              NPV_decision_veg6 = NPV_intervention_vegetable6 - NPV_no_intervention_rice,
              Cashflow_decision_veg1 = cashflow_veg1,
              Cashflow_decision_veg2 = cashflow_veg2,
              Cashflow_decision_veg3 = cashflow_veg3,
              Cashflow_decision_veg4 = cashflow_veg4,
              Cashflow_decision_veg5 = cashflow_veg5,
              Cashflow_decision_veg6 = cashflow_veg6
  ))
}




# Run the Monte Carlo simulation using the model function
input_estimates <- read.csv("20231222_new_variable_estimates~fix.csv", sep = ";")

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
                                             "NO_Interv_rice_NPV"),
                                    method = 'smooth_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_veg1_NPV", 
                                             "Interv_veg2_NPV",
                                             "Interv_veg3_NPV",
                                             "Interv_veg4_NPV",
                                             "Interv_veg5_NPV",
                                             "Interv_veg6_NPV",
                                             "NO_Interv_rice_NPV"),
                                    method = 'boxplot')


# cashflow analysis
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg1")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg2")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg3")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg4")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg5")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "Cashflow_decision_veg6")


# Projection to Latent Structures (PLS) analysis
pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[8], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[9], ncomp = 1)
plot_pls(pls_result, threshold = 0)

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
