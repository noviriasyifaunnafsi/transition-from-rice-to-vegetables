library(tidyverse)
library(ggplot2)
library(dplyr)
library(decisionSupport)

read.csv("new_variable_estimates.csv", sep=";")


transition_rice_to_vegetables <- function(){
  
  
  # Estimate income
  ##Vegetables
  potato_income <- vv(potato_yield * potato_price, n=n_year, var_CV=100)
  shallot_income <- vv(shallot_yield * shallot_price, n=n_year, var_CV=100)
  chili_income <- vv(chili_yield * chili_price, n=n_year, var_CV=100)
  carrot_income <- vv(carrot_yield * carrot_price, n=n_year, var_CV=100)
  cabbage_income <- vv(cabbage_yield * cabbage_price, n=n_year, var_CV=100)

  precalc_intervention_vegetable_benefits <- vv(potato_income+shallot_income+
                                                  chili_income+carrot_income+
                                                  cabbage_income, n=n_year,
                                                var_CV=100)
  
  ##rice
  rice_income <- vv(rice_yield * rice_price, n=n_year, var_CV=100)
  
  # Estimate water use
  ##Vegetables
  potato_water_use <- vv(potato_water_use * water_price, n=n_year, var_CV=100)
  shallot_water_use <- vv(shallot_water_use * water_price, n=n_year, var_CV=100)
  chili_water_use <- vv(chili_water_use * water_price, n=n_year, var_CV=100)
  carrot_water_use <- vv(carrot_water_use * water_price, n=n_year, var_CV=100)
  cabbage_water_use <- vv(cabbage_water_use * water_price, n=n_year, var_CV=100)
  
  ##rice
  rice_water_use <- vv(rice_water_use * water_price, n=n_year, var_CV=100)
  
  
  #Estimate the cost of vegetables
  
  ##potato
  potato_cost_precal <- sum(potato_production_cost, potato_water_use, storage_maintenance_cost)
  potato_cost <- vv(potato_cost_precal+storage_facility, n=n_year, var_CV=100)
  
  ##shallot
  shallot_cost_precal <- sum(shallot_production_cost, shallot_water_use, storage_maintenance_cost)
  shallot_cost <- vv(shallot_cost_precal+storage_facility, n=n_year, var_CV=100)
  
  ##chili
  chili_cost_precal <- sum(chili_production_cost, chili_water_use, storage_maintenance_cost)
  chili_cost <- vv(chili_cost_precal+storage_facility, n=n_year, var_CV=100)
  
  ##carrot
  carrot_cost_precal <- sum(carrot_production_cost, carrot_water_use, storage_maintenance_cost)
  carrot_cost <- vv(carrot_cost_precal+storage_facility, n=n_year, var_CV=100)
  
  ##cabbage
  cabbage_cost_precal <- sum(cabbage_production_cost, cabbage_water_use, storage_maintenance_cost)
  cabbage_cost <- vv(cabbage_cost_precal+storage_maintenance_cost, n=n_year, var_CV=100)
  
  
  ##all vegetables
  vegetables_cost_precal <- sum(potato_cost, shallot_cost, chili_cost, carrot_cost, cabbage_cost)
  vegetables_cost <- vv(vegetables_cost_precal, n=n_year, var_CV=100)
  
  
  #Estimate the cost of rice
  ##rice
  rice_cost_precal <- sum(rice_production_cost, rice_water_use, storage_maintenance_cost)
  rice_cost <- vv(rice_cost_precal+storage_facility, n=n_year, var_CV=100)
  
  
  ### Intervention of transition from rice to vegetables (TRV)
  TRV_potato <- vv(potato_income - potato_cost, n=n_year, var_CV=100)
  TRV_shallot <- vv(shallot_income - shallot_cost, n=n_year, var_CV=100)
  TRV_chili <- vv(chili_income - chili_cost, n=n_year, var_CV=100)
  TRV_carrot <- vv(carrot_income - carrot_cost, n=n_year, var_CV=100)
  TRV_cabbage <- vv(cabbage_income - cabbage_cost, n=n_year, var_CV=100)
  
  ### No intervention (rice)
  TRV_no_intervention <- vv(rice_income - rice_cost, n=n_year, var_CV=100)
  
  
  #NPV
  NPV_interv_potato <- discount(TRV_potato, discount_rate, calculate_NPV = TRUE)
  NPV_interv_shallot <- discount(TRV_shallot, discount_rate, calculate_NPV = TRUE)
  NPV_interv_chili <- discount(TRV_chili, discount_rate, calculate_NPV = TRUE)
  NPV_interv_carrot <- discount(TRV_carrot, discount_rate, calculate_NPV = TRUE)
  NPV_interv_cabbage <- discount(TRV_cabbage, discount_rate, calculate_NPV = TRUE)
  NPV_n_interv <- discount(TRV_no_intervention, discount_rate, calculate_NPV = TRUE)
  

  
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(Interv_NPV = (NPV_interv_potato+NPV_interv_shallot+
                                     NPV_interv_chili+NPV_interv_carrot+
                                     NPV_interv_cabbage),
              NO_Interv_NPV = NPV_n_interv,
              NPV_decision_do = (NPV_interv_potato+NPV_interv_shallot+
                                   NPV_interv_chili+NPV_interv_carrot+
                                   NPV_interv_cabbage) - NPV_n_interv,
              Cashflow_decision_do = (TRV_potato+TRV_shallot+TRV_chili+
                                        TRV_carrot+TRV_cabbage)- TRV_no_intervention
  ))
}




# Run the Monte Carlo simulation using the model function
input_estimates <- read.csv("new_variable_estimates.csv", sep=";")

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

decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_NPV","NO_Interv_NPV"),
                                    method = 'boxplot_density')





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



