library(decisionSupport)
install.packages("gtExtras")
library(gtExtras)
install.packages("svglite")
library(svglite)

input_estimates <- read.csv("input_rice_to_vegs.csv", sep = ";")
                                  
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_estimates))

transition_rice_to_vegetables <- function(x, varnames){
  
  # Estimate the benefits
  
  ##if farmers grow rice (assuming they grow rice 3 times per year)
  rice_income_precal <- (rice_yield * rice_price)*3 #Indonesian Rupee
  rice_income <- vv(rice_income_precal, n_year, var_CV=CV_value)
  
  annual_irrigation_infra_mgmnt <- 100
  cost_of_pipes_per_ha_irrigated <- 10000
  
  cost_of_rice_water_use <- vv(annual_irrigation_infra_mgmnt, n_year, var_CV=CV_value)
  cost_of_rice_water_use[1] <- cost_of_pipes_per_ha_irrigated
  
  rice_water_use <- vv(rice_income/rice_water_use, n_year, var_CV=CV_value)
  rice_nutrition <- vv(rice_nutrition_to_health_value, n_year, var_CV=CV_value)
  
  rice_total_benefit_precal <- rice_income + rice_water_use + rice_nutrition
  rice_total_benefit <- vv(rice_total_benefit_precal, n_year, var_CV=CV_value)
  
  
  ##if farmers grow vegetables 
  ##(assuming they grow 3 different vegetables per year)
  ##to be calculated for each crop
  ###income
  potato_income <- vv(potato_yield*potato_price, n_year, var_CV=CV_value)
  shallot_income <- vv(shallot_yield*shallot_price, n_year, var_CV=CV_value)
  cabbage_income <- vv(cabbage_yield*cabbage_price, n_year, var_CV=CV_value) 
  vegetables_income_precal <- potato_income + shallot_income + cabbage_income
  vegetables_income <- vv(vegetables_income_precal, n_year, var_CV=CV_value)
  
  ###water use
  vegetables_water_use <- vv(vegetables_water_use, n_year, var_CV=CV_value)
  
  ###nutrition
  vegetables_nutrition <- vv(vegetables_nutrition_to_health_value, n_year, var_CV=CV_value)
  
  ##vegetables total benefit
  vegetables_total_benefit_precal <- vegetables_income + vegetables_water_use + vegetables_nutrition
  vegetables_total_benefit <- vv(vegetables_total_benefit_precal, n_year, var_CV=CV_value)
  
  
  ##if farmers do crop rotation (vegetable-rice-vegetable)
  ###water use
  crop_rotation_water_use_precal <- rice_water_use + (vegetables_water_use*2) 
  crop_rotation_water_use <- vv(crop_rotation_water_use_precal, n_year, var_CV=CV_value)
  
  ###nutrition
  crop_rotation_nutrition_precal <- rice_nutrition + (vegetables_nutrition*2)
  crop_rotation_nutrition <- vv(crop_rotation_nutrition_precal, n_year, var_CV=CV_value)
  
  
  ### total benefits crop rotation 1 
  crop_rotation_income1_precal <- potato_income + rice_income + shallot_income
  crop_rotation_income1 <- vv(crop_rotation_income1_precal, n_year, var_CV=CV_value)
  crop_rotation1_total_benefits_precal <- crop_rotation_income1 +
    crop_rotation_water_use +
    crop_rotation_nutrition
  crop_rotation1_total_benefit <- vv(crop_rotation1_total_benefits_precal, n_year, var_CV=CV_value)
  
  
  ### total benefits crop rotation 2
  crop_rotation_income2_precal <- potato_income + rice_income + cabbage_income
  crop_rotation_income2 <- vv(crop_rotation_income2_precal, n_year, var_CV=CV_value)
  crop_rotation2_total_benefits_precal <- crop_rotation_income2 +
    crop_rotation_water_use +
    crop_rotation_nutrition
  crop_rotation2_total_benefit <- vv(crop_rotation2_total_benefits_precal, n_year, var_CV=CV_value)
  
  
  
  ### total benefits crop rotation 3
  crop_rotation_income3_precal <- shallot_income + rice_income + cabbage_income
  crop_rotation_income3 <- vv(crop_rotation_income3_precal, n_year, var_CV=CV_value)
  crop_rotation3_total_benefits_precal <- crop_rotation_income3 +
    crop_rotation_water_use +
    crop_rotation_nutrition
  crop_rotation3_total_benefit <- vv(crop_rotation3_total_benefits_precal, n_year, var_CV=CV_value)
  
  
  # Estimate the cost
  ##if farmers grow rice (assuming they grow rice 3 times per year)
  rice_costs_precal <- (rice_farming_input_costs + rice_labor_costs + rice_machinery_costs)*3 +
    warehouse_establishment_costs + warehouse_maintenance_costs
  rice_total_costs <- vv(rice_costs_precal, n_year,var_CV=CV_value)
  
  
  ##if farmers grow vegetables 
  ##(assuming they grow 3 different vegetables per year)
  potato_costs_precal <- potato_farming_input_costs + potato_labor_costs + potato_machinery_costs
  potato_costs <- vv(potato_costs_precal, n_year, var_CV=CV_value)
  
  shallot_costs_precal <- shallot_farming_input_costs + shallot_labor_costs + shallot_machinery_costs
  shallot_costs <- vv(shallot_costs_precal, n_year, var_CV=CV_value)
  
  cabbage_costs_precal <- cabbage_farming_input_costs + cabbage_labor_costs + cabbage_machinery_costs
  cabbage_costs <- vv(cabbage_costs_precal, n_year, var_CV=CV_value)
  
  vegetables_costs_precal <- potato_costs + shallot_costs + cabbage_costs + 
    vegetables_farming_equipment_costs +
    warehouse_establishment_costs +
    warehouse_maintenance_costs
  vegetables_total_costs <- vv(vegetables_costs_precal, n_year, var_CV=CV_value)
  
  
  ##if farmers do crop rotation 
  ### total cost crop rotation 1 (potato-rice-shallot) 
  crop_rotation_cost1_precal <- potato_costs + rice_total_costs + shallot_costs
  crop_rotation_cost1 <- vv(crop_rotation_cost1_precal, n_year, var_CV=CV_value)
  crop_rotation1_total_costs_precal <- crop_rotation_cost1 +
    vegetables_farming_equipment_costs +
    warehouse_establishment_costs +
    warehouse_maintenance_costs
  crop_rotation1_total_costs <- vv(crop_rotation1_total_costs_precal, n_year, var_CV=CV_value)
  
  
  ### total benefits crop rotation 2 (potato-rice-cabbage)
  crop_rotation_cost2_precal <- potato_costs + rice_total_costs + cabbage_costs
  crop_rotation_cost2 <- vv(crop_rotation_cost2_precal, n_year, var_CV=CV_value)
  crop_rotation2_total_costs_precal <- crop_rotation_cost2 + 
    vegetables_farming_equipment_costs +
    warehouse_establishment_costs + 
    warehouse_maintenance_costs
  crop_rotation2_total_costs <- vv(crop_rotation2_total_costs_precal, n_year, var_CV=CV_value)
  
  
  ### total benefits crop rotation 3 (shallot-rice-cabbage)
  crop_rotation_cost3_precal <- shallot_costs + rice_total_costs + cabbage_costs
  crop_rotation_cost3 <- vv(crop_rotation_cost3_precal, n_year, var_CV=CV_value)
  crop_rotation3_total_costs_precal <- crop_rotation_cost3 +
    vegetables_farming_equipment_costs +
    warehouse_establishment_costs +
    warehouse_maintenance_costs
  crop_rotation3_total_costs <- vv(crop_rotation3_total_costs_precal, n_year, var_CV=CV_value)
  
  
  
  # Final results
  ## option 1: if farmers grow rice
  rice_result <- rice_total_benefit - rice_total_costs
  
  ## option 2: if farmers grow vegetables
  vegetables_result <- vegetables_total_benefit - vegetables_total_costs
  
  ## option 3: if farmers do crop rotation
  crop_rotation1_result <- crop_rotation1_total_benefit - crop_rotation1_total_costs
  crop_rotation2_result <- crop_rotation2_total_benefit - crop_rotation2_total_costs
  crop_rotation3_result <- crop_rotation3_total_benefit - crop_rotation3_total_costs
  
  
  # Calculate NPV
  
  NPV_no_interv <- discount(x = rice_result, 
                            discount_rate = discount_rate,
                            calculate_NPV = TRUE)
  
  NPV_vegetables_interv <- discount(x = vegetables_result, 
                                    discount_rate = discount_rate,
                                    calculate_NPV = TRUE)
  
  NPV_crop_rotation1_interv <- discount(x = crop_rotation1_result, 
                                        discount_rate = discount_rate,
                                        calculate_NPV = TRUE)
  
  NPV_crop_rotation2_interv <- discount(x = crop_rotation2_result, 
                                        discount_rate = discount_rate,
                                        calculate_NPV = TRUE)
  
  NPV_crop_rotation3_interv <- discount(x = crop_rotation3_result, 
                                        discount_rate = discount_rate,
                                        calculate_NPV = TRUE)
  
  
  return(list(Interv_NPV_vegetables = NPV_vegetables_interv,
              Interv_NPV_crop_rotation1 = NPV_crop_rotation1_interv,
              Interv_NPV_crop_rotation2 = NPV_crop_rotation2_interv,
              Interv_NPV_crop_rotation3 = NPV_crop_rotation3_interv,
              NO_interv_NPV_rice = NPV_no_interv,
              NPV_decision_vegetables = NPV_vegetables_interv - NPV_no_interv,
              NPV_decision_crop_rotation1 = NPV_crop_rotation1_interv - NPV_no_interv,
              NPV_decision_crop_rotation2 = NPV_crop_rotation2_interv - NPV_no_interv,
              NPV_decision_crop_rotation3 = NPV_crop_rotation3_interv - NPV_no_interv,
              cashflow_vegetables = vegetables_result - rice_result,
              cashflow_crop_rotation1 = crop_rotation1_result - rice_result,
              cashflow_crop_rotation2 = crop_rotation2_result - rice_result,
              cashflow_crop_rotation3 = crop_rotation3_result - rice_result))
  
  
}



# Run the Monte Carlo simulation using the model function
input_estimates <- read.csv("input_rice_to_vegs.csv", sep = ";")

TRV_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                  model_function = transition_rice_to_vegetables,
                                  numberOfModelRuns = 1000,
                                  functionSyntax = "plainNames")


# Subset the outputs from the mcSimulation function (y) to summarize only on the variables that we want.
# names(TRV_mc_simulation$x)
mcSimulation_summary <- data.frame(TRV_mc_simulation$x[5:32],
                                   # names(TRV_mc_simulation$x)
                                   TRV_mc_simulation$y[1:9])

gtExtras::gt_plt_summary(mcSimulation_summary)



# plot NPV distribution analysis
decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_NPV_vegetables", 
                                             "Interv_NPV_crop_rotation1",
                                             "Interv_NPV_crop_rotation2",
                                             "Interv_NPV_crop_rotation3",
                                             "NO_interv_NPV_rice"),
                                    method = 'smooth_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_NPV_vegetables", 
                                             "Interv_NPV_crop_rotation1",
                                             "Interv_NPV_crop_rotation2",
                                             "Interv_NPV_crop_rotation3"),
                                    method = 'smooth_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_NPV_vegetables", 
                                             "Interv_NPV_crop_rotation1",
                                             "Interv_NPV_crop_rotation2",
                                             "Interv_NPV_crop_rotation3",
                                             "NO_interv_NPV_rice"),
                                    method = 'hist_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("Interv_NPV_vegetables", 
                                             "Interv_NPV_crop_rotation1",
                                             "Interv_NPV_crop_rotation2",
                                             "Interv_NPV_crop_rotation3",
                                             "NO_interv_NPV_rice"),
                                    method = 'boxplot')



# cashflow analysis
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_vegetables")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_crop_rotation1")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_crop_rotation2")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_crop_rotation3")


# Projection to Latent Structures (PLS) analysis
pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[6], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[8], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[9], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[10], ncomp = 1)
plot_pls(pls_result, threshold = 0)



# VoI analysis
mcSimulation_table <- data.frame(TRV_mc_simulation$x, TRV_mc_simulation$y[1:9])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_NPV_vegetables")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_vegetables")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_crop_rotation1")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_crop_rotation2")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_crop_rotation3")



compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_vegetables", 
                cashflow_var_name = "cashflow_vegetables", 
                base_size = 7)


compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_crop_rotation1", 
                cashflow_var_name = "cashflow_crop_rotation1", 
                base_size = 7)

compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_crop_rotation2", 
                cashflow_var_name = "cashflow_crop_rotation2", 
                base_size = 7)

compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_crop_rotation3", 
                cashflow_var_name = "cashflow_crop_rotation3", 
                base_size = 7)




