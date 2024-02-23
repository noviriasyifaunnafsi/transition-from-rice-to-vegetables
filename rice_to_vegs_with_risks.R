library(decisionSupport)
install.packages("gtExtras")
library(gtExtras)
install.packages("svglite")
library(svglite)

input_estimates <- read.csv("input_rice_to_vegs_(USD).csv", sep = ";")

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_estimates))

transition_rice_to_vegetables <- function(x, varnames){
  
  
  # Estimate the cost #
  
  ## farm and water management skills training cost
  ### this training is for farmers, will be held at least once per year (or two year?)
  trainer_fee_costs <- vv(trainer_management_costs, n_year, var_CV=CV_value, relative_trend = inflation_rate) # IDR/year
  training_costs <- vv(training_management_costs, n_year, var_CV=CV_value, relative_trend = inflation_rate) #IDR/year
  
  ### monitoring and evaluation will be held at least twice per year, to see how is the progress after the training
  monitoring_and_evaluation_cost <- vv(monitoring_and_evaluation_costs, n_year, var_CV=CV_value, relative_trend = inflation_rate) #IDR/year
  
  # costs for rice
  ##if farmers grow rice
  annual_rice_irrigation_costs <- rice_irrigation_costs * 2 #IDR/ha times 2 
  
  rice_costs_precal <- (rice_farming_input_costs + rice_labor_costs + rice_machinery_costs)*2 +
    annual_rice_irrigation_costs + warehouse_establishment_costs + warehouse_maintenance_costs + trainer_fee_costs + training_costs
  
  rice_costs <- vv(rice_costs_precal, n_year,var_CV=CV_value)

  
  ##if farmers grow vegetables
  
  ### cost of equipment, warehouse establishment and maintenance for vegetables
  cost_of_equipment_and_warehouse <- vegetables_farming_equipment_costs + warehouse_establishment_costs + 
    warehouse_maintenance_costs
  
  ##assuming they grow 2-3 different vegetables per year 
  potato_costs <- potato_farming_input_costs + potato_labor_costs + potato_machinery_costs + vegetables_irrigation_costs
  
  shallot_costs <- shallot_farming_input_costs + shallot_labor_costs + shallot_machinery_costs + vegetables_irrigation_costs
  
  cabbage_costs <- cabbage_farming_input_costs + cabbage_labor_costs + cabbage_machinery_costs + vegetables_irrigation_costs
  
  chili_costs <- chili_farming_input_costs + chili_labor_costs + chili_machinery_costs + vegetables_irrigation_costs
  chili_monthly_costs <- chili_costs/8 # calculate the monthly cost for estimating the costs of crop rotation (rice and chili)
  
  tomato_costs <- tomato_farming_input_costs + tomato_labor_costs + tomato_machinery_costs + vegetables_irrigation_costs
  tomato_monthly_costs <- tomato_costs/8 # calculate the monthly cost for estimating the costs of crop rotation (rice and chili)
    
  vegetables1_total_costs <- potato_costs + shallot_costs + cabbage_costs + cost_of_equipment_and_warehouse
  vegetables2_total_costs <- chili_costs + shallot_costs + cost_of_equipment_and_warehouse
  vegetables3_total_costs <- chili_costs + cabbage_costs + cost_of_equipment_and_warehouse
  vegetables4_total_costs <- chili_costs + potato_costs + cost_of_equipment_and_warehouse
  vegetables5_total_costs <- tomato_costs + shallot_costs + cost_of_equipment_and_warehouse
  vegetables6_total_costs <- tomato_costs + cabbage_costs + cost_of_equipment_and_warehouse
  vegetables7_total_costs <- tomato_costs + potato_costs + cost_of_equipment_and_warehouse
  
  #estimate the average costs of growing vegetables (5 common vegetables) per year
  vegetables_costs_average_precal <- mean(vegetables1_total_costs, vegetables2_total_costs, vegetables3_total_costs,
                           vegetables4_total_costs, vegetables5_total_costs, vegetables6_total_costs,
                           vegetables7_total_costs)
  vegetables_total_costs <- vv(vegetables_costs_average_precal, n_year, var_CV=CV_value, relative_trend = inflation_rate)
  
  
  ##if farmers do crop rotation 
  ### total cost crop rotation 1 (potato-rice-shallot) 
  crop_rotation_production_cost1 <- potato_costs + rice_costs + shallot_costs
  crop_rotation1_total_costs <- crop_rotation_production_cost1 + cost_of_equipment_and_warehouse
  crop_rotation1_total_costs_average <- mean(crop_rotation1_total_costs)
  
  ### total costs crop rotation 2 (potato-rice-cabbage)
  crop_rotation_production_cost2 <- potato_costs + rice_costs + cabbage_costs
  crop_rotation2_total_costs <- crop_rotation_production_cost2 + cost_of_equipment_and_warehouse
  crop_rotation2_total_costs_average <- mean(crop_rotation2_total_costs)
  
  ### total costs crop rotation 3 (shallot-rice-cabbage)
  crop_rotation_production_cost3 <- shallot_costs + rice_costs + cabbage_costs
  crop_rotation3_total_costs <- crop_rotation_production_cost3 + cost_of_equipment_and_warehouse
  crop_rotation3_total_costs_average <- mean(crop_rotation3_total_costs)
  
  ### total costs crop rotation 4 (rice-chili)
  crop_rotation_production_cost4 <- rice_costs + (chili_monthly_costs*6)
  crop_rotation4_total_costs <- crop_rotation_production_cost3 + cost_of_equipment_and_warehouse
  crop_rotation4_total_costs_average <- mean(crop_rotation4_total_costs)
  
  ### total costs crop rotation 5 (rice-tomato)
  crop_rotation_production_cost5 <- rice_costs + (tomato_monthly_costs*6)
  crop_rotation5_total_costs <- crop_rotation_production_cost3 + cost_of_equipment_and_warehouse
  crop_rotation5_total_costs_average <- mean(crop_rotation5_total_costs)
  
  #estimate the average costs of doing crop rotation (rice and vegetables) per year
  crop_rotation_costs_average_precal <- mean(crop_rotation1_total_costs_average, crop_rotation2_total_costs_average, crop_rotation3_total_costs_average,
                                             crop_rotation4_total_costs_average, crop_rotation5_total_costs_average)
  crop_rotation_total_costs <- vv(crop_rotation_costs_average_precal, n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
  
  # Risks ##
  
  ## Risks that may occur in rice production
  ## these risks may affect farmers' income if they grow rice
  rice_production_risk <- (risk_crop_failure_rice + #loss or low harvest
                              risk_water_shortage_rice + # water shortage during planting
                              risk_farming_input_shortage_rice + #farming input shortage
                              risk_seasonal_variability_rice + # changes in cultivation and harvest schedule
                              risk_price_fluctuation_rice) # low market price
  
  ## Risk that may affect water resource
  rice_water_resource_risk <- risk_water_shortage_rice 
  
  # Risk that may 'indirectly' affect nutritional health value of people
  rice_nutritional_to_health_value_risk <- (risk_crop_failure_rice + # crop failure may influence the rice consumption pattern (?)
                                               risk_price_fluctuation_rice) # price fluctuation may influence the rice consumption pattern (?)
  
  
  ## Risk that may occur in vegetables production
  ## these risks may affect farmers' income if they grow vegetables
  vegetables_production_risk <- (risk_crop_failure_vegetables + #loss or low harvest
                              risk_water_shortage_vegetables + # water shortage during planting
                              risk_farming_input_shortage_vegetables + #farming input shortage
                              risk_seasonal_variability_vegetables + # changes in cultivation and harvest schedule
                              risk_price_fluctuation_rice) # low market price
  
  ## Risk that may affect water resource
  vegetables_water_resource_risk <- risk_water_shortage_vegetables # risk of water shortage due to vegetables cultivation
  
  # Risk that may 'indirectly' affect nutritional health value of people
  vegetables_nutritional_to_health_value_risk <- (risk_crop_failure_vegetables + # crop failure may influence the vegetables consumption pattern (?)
                                               risk_price_fluctuation_vegetables) # price fluctuation may influence the consumption pattern of vegetables (?)
  
  
  
  ## Risk that may occur in crop rotation
  crop_rotation_production_risk <- (risk_crop_failure_crop_rotation + #loss or low harvest
                                    risk_water_shortage_crop_rotation + # water shortage during planting
                                    risk_farming_input_shortage_rice + #farming input shortage for rice
                                    risk_farming_input_shortage_vegetables + #farming input shortage for vegetables
                                    risk_seasonal_variability_rice + # changes in cultivation and harvest schedule
                                    risk_seasonal_variability_vegetables + # changes in cultivation and harvest schedule
                                    risk_price_fluctuation_rice + # low market price for rice
                                    risk_price_fluctuation_vegetables) # low market price for vegetables
  
  ## Risk that may affect water resource
  crop_rotation_water_resource_risk <- (risk_water_shortage_rice + # risk of water shortage due to rice cultivation
                                     risk_water_shortage_vegetables) # risk of water shortage due to vegetables cultivation

  # Risk that may 'indirectly' affect nutritional health value of people
  crop_rotation_nutritional_to_health_value_risk <- (risk_crop_failure_rice + # crop failure may influence the rice consumption pattern (?)
                                               risk_price_fluctuation_rice + # price fluctuation may influence the consumption pattern of rice (?)
                                               risk_crop_failure_vegetables + # crop failure may influence the rice consumption pattern (?)
                                               risk_price_fluctuation_vegetables) # price fluctuation may influence the consumption pattern of vegetables (?) 

  
  
  # Estimate the benefits
  
  ##if farmers grow rice (assuming they grow rice 2 times per year)
  rice_income <- (rice_yield * rice_price)*2 #Indonesian Rupiah (IDR)
  rice_income_value <- vv(rice_income, n_year, var_CV=CV_value, 
                    relative_trend = inflation_rate) * rice_production_risk
   
  
  ###value of water in rice production (IDR/ha)
  rice_water_value_precal <- (rice_water_use * rice_water_value)*2 #rice water use is in (m3/ha) times estimated value of water used in rice production (IDR/m3)
  rice_water_value <- vv(rice_water_value_precal, n_year, var_CV=CV_value,
                         relative_trend = inflation_rate) * rice_water_resource_risk # consider water shortage may happen (?)
  
  
  ###rice nutrition to health value (IDR/ha) #estimated yearly value for accessing nutrition from rice
  rice_nutrition_to_health_value <- vv(rice_nutrition_to_health_value, n_year, var_CV=CV_value,
                                       relative_trend = inflation_rate) * rice_nutritional_to_health_value_risk
  
    
  ##if farmers grow vegetables 
  ##assuming they grow 3 different vegetables per year
  ###income in IDR (Indonesian Rupiah) --> income for one cycle
  potato_income <- potato_yield*potato_price #for one cycle is about 3 months
  shallot_income <- shallot_yield*shallot_price #for one cycle is about 3 months
  cabbage_income <- cabbage_yield*cabbage_price #for one cycle is about 3 months
  chili_income <- chili_yield*chili_price #for one cycle is about 8 months
  tomato_income <- tomato_yield*tomato_price #for one cycle is about 8 months
  
  # income from vegetables  
  vegetables1_income <- potato_income + shallot_income + cabbage_income
  vegetables2_income <- chili_income + shallot_income
  vegetables3_income <- chili_income + cabbage_income
  vegetables4_income <- chili_income + potato_income
  vegetables5_income <- tomato_income + shallot_income
  vegetables6_income <- tomato_income + cabbage_income
  vegetables7_income <- tomato_income + potato_income
  
  ## calculate average income from vegetables
  vegetables_income <- mean(vegetables1_income, vegetables2_income, vegetables3_income, vegetables4_income,
                            vegetables5_income, vegetables6_income, vegetables7_income)
  
  ### calculate vegetables income value with risk
  vegetables_income_value <- vv(vegetables_income, n_year, var_CV=CV_value,
                                relative_trend = inflation_rate) * vegetables_production_risk
  
  
  ## value of water used for vegetable production (IDR/ha) --> assuming that the value of water used for all vegetables are similar
  vegetables_water_value_precal <- (vegetables_water_use * vegetables_water_value) * 3 # assuming vegetables can be cultivated at least 3 times per year
  vegetables_water_value <- vv(vegetables_water_value_precal, #value of water used for production (IDR/ha)
                               n_year, var_CV=CV_value, relative_trend = inflation_rate) * vegetables_water_resource_risk # consider water shortage may happen (?)
  
  
  ###vegetables nutrition to health value (IDR/ha)
  vegetables_nutrition_to_health_value <- vv(vegetables_nutrition_to_health_value, n_year, var_CV=CV_value,
                                             relative_trend = inflation_rate) * vegetables_nutritional_to_health_value_risk #estimated yearly value for accessing nutrition from vegetables
  
  
  
  
  ##if farmers do crop rotation (vegetable-rice-vegetable) or (rice-vegetable)
  # calculate monthly income from chili and tomato
  monthly_chili_income <- (chili_yield*chili_price)/8
  monthly_tomato_income <- (tomato_yield*tomato_price)/8
  
  crop_rotation_income1 <- potato_income + rice_income + shallot_income
  crop_rotation_income2 <- potato_income + rice_income + cabbage_income
  crop_rotation_income3 <- shallot_income + rice_income + cabbage_income
  crop_rotation_income4 <- rice_income + (monthly_chili_income*6)
  crop_rotation_income5 <- rice_income + (monthly_tomato_income*6)
  
  ## calculate average income from crop rotation
  crop_rotation_income <- mean(crop_rotation_income1, crop_rotation_income2, crop_rotation_income3,
                               crop_rotation_income4, crop_rotation_income5)
  
  ### calculate crop rotation income value with risk
  crop_rotation_income_value <- vv(crop_rotation_income, n_year, var_CV=CV_value,
                                relative_trend = inflation_rate) * crop_rotation_production_risk
  
  
  ###value of water used in crop rotation (IDR/ha) 
  crop_rotation_water_value_precal <- rice_water_value + vegetables_water_value # assuming that rice and vegetables can be cultivated ate least once per year
  crop_rotation_water_value <- vv(crop_rotation_water_value_precal, #value of water used for production (IDR/ha)
                                  n_year, var_CV=CV_value, relative_trend = inflation_rate) * crop_rotation_water_resource_risk # consider water shortage may happen (?)
  
  
  ###nutrition to health value from crop rotation (IDR/ha)
  
  ### estimating monthly nutrition value from rice
  monthly_rice_to_nutrition_health_value <- rice_nutrition_to_health_value/12
  ### estimating monthly nutrition value from vegetables
  monthly_vegetables_to_nutrition_health_value <- vegetables_nutrition_to_health_value/12
  
  crop_rotation_nutrition_to_health_value_precal <- (monthly_rice_to_nutrition_health_value*6) + (monthly_vegetables_to_nutrition_health_value*6) 
  crop_rotation_nutrition_to_health_value <- vv(crop_rotation_nutrition_to_health_value_precal, #estimated yearly value for accessing nutrition from crop rotation
                                                n_year, var_CV=CV_value, relative_trend = inflation_rate) * crop_rotation_nutritional_to_health_value_risk #estimated yearly value for accessing nutrition from crop rotation)
  
  
  
  # Sum up all benefits
  rice_total_benefit <- rice_income_value + rice_water_value + rice_nutrition_to_health_value
  vegetables_total_benefit <- vegetables_income_value + vegetables_water_value + vegetables_nutrition_to_health_value
  crop_rotation_total_benefit <- crop_rotation_income_value + crop_rotation_water_value + crop_rotation_nutrition_to_health_value
  
  
  
  # Final results
  ## option 1: if farmers grow rice
  rice_result <- rice_total_benefit - rice_costs
  
  ## option 2: if farmers grow vegetables
  vegetables_result <- vegetables_total_benefit - vegetables_total_costs
  
  ## option 3: if farmers do crop rotation
  crop_rotation_result <- crop_rotation_total_benefit - crop_rotation_total_costs
  
  
  # Calculate NPV
  # no intervention --> if farmers still growing rice?
  NPV_no_interv <- discount(x = rice_result, 
                            discount_rate = discount_rate,
                            calculate_NPV = TRUE)
  
  NPV_vegetables_interv <- discount(x = vegetables_result, 
                                    discount_rate = discount_rate,
                                    calculate_NPV = TRUE)
  
  NPV_crop_rotation_interv <- discount(x = crop_rotation_result, 
                                        discount_rate = discount_rate,
                                        calculate_NPV = TRUE)
  
  
  return(list(NPV_vegetables = NPV_vegetables_interv,
              NPV_crop_rotation = NPV_crop_rotation_interv,
              NPV_rice = NPV_no_interv,
              NPV_decision_rice = NPV_no_interv,
              NPV_decision_vegetables = NPV_vegetables_interv - NPV_no_interv,
              NPV_decision_crop_rotation = NPV_crop_rotation_interv - NPV_no_interv,
              cashflow_rice = rice_result,
              cashflow_vegetables = vegetables_result - rice_result,
              cashflow_crop_rotation = crop_rotation_result - rice_result))
  
  
}



# Run the Monte Carlo simulation using the model function
input_estimates <- read.csv("input_rice_to_vegs_(USD).csv", sep = ";")

TRV_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                  model_function = transition_rice_to_vegetables,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")


# Subset the outputs from the mcSimulation function (y) to summarize only on the variables that we want.
# names(TRV_mc_simulation$x)
mcSimulation_summary <- data.frame(TRV_mc_simulation$x[5:32],
                                   # names(TRV_mc_simulation$x)
                                   TRV_mc_simulation$y[1:9])

gtExtras::gt_plt_summary(mcSimulation_summary)



# plot NPV distribution analysis
decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("NPV_vegetables", 
                                             "NPV_crop_rotation",
                                             "NPV_rice"),
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
                                    vars = c("NPV_vegetables", 
                                              "NPV_crop_rotation",
                                              "NPV_rice"),
                                    method = 'boxplot')



# cashflow analysis
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_vegetables")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_crop_rotation")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_rice")

# Projection to Latent Structures (PLS) analysis
pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[4], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[5], ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[6], ncomp = 1)
plot_pls(pls_result, threshold = 0)



# VoI analysis
mcSimulation_table <- data.frame(TRV_mc_simulation$x, TRV_mc_simulation$y[1:6])
evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_vegetables")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_vegetables")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_crop_rotation")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_rice")

compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_vegetables", 
                cashflow_var_name = "cashflow_vegetables", 
                base_size = 7)


compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_crop_rotation", 
                cashflow_var_name = "cashflow_crop_rotation", 
                base_size = 7)


compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_rice", 
                cashflow_var_name = "cashflow_rice", 
                base_size = 7)
