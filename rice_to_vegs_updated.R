library(decisionSupport)
install.packages("gtExtras")
library(gtExtras)
install.packages("svglite")
library(svglite)

input_estimates <- read.csv("input_rice_to_vegs_usd.csv", sep = ",")

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_estimates))

transition_rice_to_vegetables <- function(x, varnames){
  

  # Estimate the cost for 1 ha of field #
  
  # option 1 (rice)
  # annual cost for rice, assuming farmers grow rice twice per year
  ## annual rice costs
  rice_cost_precal <- ((rice_farming_input_costs + rice_machinery_costs + rice_labor_costs)*2) + 
                        irrigation_maintenance_costs #to be paid to farmers' group or water user association of local farmers' group
  rice_cost <- vv(rice_cost_precal, n_year, var_CV=CV_value)
  
  # option 2 (vegetables)
  # annual cost for vegetables, assuming farmers grow vegetables with mix cropping (5 crops per year)
  ## annual vegetable costs
  vegetables_cost_precal <- vegetables_farming_input_costs + #seeds, fertilizer, pesticides, plastic mulch, etc.
                              vegetables_machinery_costs + #assuming the machinery is used alternately among farmers, cost is only for fuel
                              vegetables_labor_costs +  irrigation_maintenance_costs
  vegetables_cost <- vv(vegetables_cost_precal, n_year, var_CV=CV_value)

  # calculating the implementation costs of growing vegetables (other costs that might need to be paid for vegetables)
    
    if (implementation_vegetables){
      cost_implementation_vegetables <- vegetables_cost + vegetables_equipment_costs + vegetables_irrigation_costs
    } else{
      cost_implementation_vegetables <- 0
    }
    
    # calculating the establishment costs that will be paid only for the first year of implementation cultivating vegetables
    establishment_vegetables_cost <- rep(0, n_year)
    
    if (implementation_transition_to_vegetables){
      establishment_vegetables_cost <-
        establishment_vegetables_cost + vv(vegetables_equipment_costs + vegetables_irrigation_costs, 
                                           var_CV=CV_value, n_year)
      
      intervention_vegetable_cost <- establishment_vegetables_cost
      
      #intervention cost for first year 
      intervention_vegetable_cost[1] <- cost_implementation_vegetables 
    }
    
    
  # option 3 (cultivate both rice and vegetables separately)
  ## cultivate rice in 0.5 ha and vegetables in 0.5 ha
  ### total cost both rice and vegetables 
  rice_vegs_total_costs_precal <- (rice_cost/2) + (vegetables_cost/2)
  rice_vegs_total_costs <-   vv(rice_vegs_total_costs_precal, n_year, var_CV=CV_value, relative_trend = inflation_rate)
  
  
  # calculating the implementation costs of growing vegetables (other costs that might need to be paid for vegetables)
  
  if (implementation_rice_and_vegetables){
    cost_implementation_rice_and_vegetables <- rice_cost + vegetables_cost + 
                                              (vegetables_equipment_costs/2) + (vegetables_irrigation_costs/2)
  } else{
    cost_implementation_rice_and_vegetables <- 0
  }
  
  # calculating the establishment costs that will be paid only for the first year of implementation cultivating vegetables
  establishment_rice_and_vegetables_cost <- rep(0, n_year)
  
  if (implementation_rice_and_vegetables){
    establishment_rice_and_vegetables_cost <-
      establishment_rice_and_vegetables_cost + vv(vegetables_equipment_costs + vegetables_irrigation_costs, 
                                         var_CV=CV_value, n_year)
    
    intervention_rice_and_vegetable_cost <- establishment_rice_and_vegetables_cost
    
    #intervention cost for first year 
    intervention_rice_and_vegetable_cost[1] <- cost_implementation_rice_and_vegetables 
  }
  
  
  # Risks ##
  
  ## Risks that may occur in rice production (option 1)
  ## these risks may affect farmers' income if they grow rice
  
  risk_crop_failure_rice <- risk_failure_climate_weather_rice + risk_failure_pest_disease_rice
  
  rice_production_risk <- (risk_crop_failure_rice + #loss or low harvest
                             risk_price_fluctuation_rice + # low market price
                             risk_capital_rice + # low capital to continue grow rice
                             risk_water_shortage_rice + # water shortage during planting
                             risk_labor_shortage_rice) #labor shortage in growing rice
                             
  ## Risk that may affect water resource
  rice_water_resource_risk <- risk_water_shortage_rice 
  
  # Risk that may 'indirectly' affect nutritional health value of people
  rice_nutritional_to_health_value_risk <- (risk_crop_failure_rice + # crop failure may influence the rice consumption pattern (?)
                                              risk_price_fluctuation_rice) # price fluctuation may influence the rice consumption pattern (?)
  
  
  ## Risk that may occur in vegetables production (option 2)
  ## these risks may affect farmers' income if they grow vegetables
  
  risk_crop_failure_vegetables <- risk_failure_climate_weather_vegetables + risk_failure_pest_disease_vegetables
  
  vegetables_production_risk <- (risk_crop_failure_vegetables + #loss or low harvest
                                   risk_price_fluctuation_vegetables + # low market price
                                   risk_capital_vegetables + # low capital to continue grow vegetables
                                   risk_water_shortage_vegetables + # water shortage during planting
                                   risk_labor_shortage_vegetables) #labor shortage in growing vegetables
                                   
  ## Risk that may affect water resource
  vegetables_water_resource_risk <- risk_water_shortage_vegetables # risk of water shortage due to vegetables cultivation
  
  # Risk that may 'indirectly' affect nutritional health value of people
  vegetables_nutritional_to_health_value_risk <- (risk_crop_failure_vegetables + # crop failure may influence the vegetables consumption pattern (?)
                                                    risk_price_fluctuation_vegetables) # price fluctuation may influence the consumption pattern of vegetables (?)
  
  
  
  ## Risk that may occur in growing both rice and vegetables (option 3)
  rice_vegetables_production_risk <- risk_crop_failure_rice + #loss or low harvest
                                      risk_crop_failure_vegetables + #loss or low harvest  
                                      risk_price_fluctuation_rice + # low market price for rice
                                      risk_price_fluctuation_vegetables + # low market price for vegetables
                                      risk_capital_rice + # low capital to continue grow rice
                                      risk_capital_vegetables + # low capital to continue grow vegetables
                                      risk_water_shortage_rice + # water shortage during planting
                                      risk_water_shortage_vegetables + # water shortage during planting
                                      risk_labor_shortage_rice + #labor shortage in growing rice
                                      risk_labor_shortage_vegetables #labor shortage in growing vegetables
                                          
  ## Risk that may affect water resource
  rice_vegetables_water_resource_risk <- risk_water_shortage_rice + # risk of water shortage due to rice cultivation
                                          risk_water_shortage_vegetables # risk of water shortage due to vegetables cultivation
  
  # Risk that may 'indirectly' affect nutritional health value of people
  rice_vegetables_nutritional_to_health_value_risk <- risk_crop_failure_rice + # crop failure may influence the rice consumption pattern (?)
                                                      risk_price_fluctuation_vegetables + # price fluctuation may influence the consumption pattern of rice (?)
                                                      risk_crop_failure_vegetables + # crop failure may influence the rice consumption pattern (?)
                                                      risk_price_fluctuation_vegetables # price fluctuation may influence the consumption pattern of vegetables (?) 
  
  
  
  # Estimate the benefits
  
  ## annual income for rice, assuming they grow rice 2 times per year
  ## annual income for rice per ha
  rice_income_precal <- (rice_yield *rice_price)*2
  rice_income <- vv(rice_income_precal, n_year, var_CV=CV_value)
  
  ## calculate rice income with risks
  rice_income_value <- vv(rice_income, n_year, var_CV=CV_value, 
                          relative_trend = inflation_rate) * rice_production_risk
  
  
  
  ## value of water in rice production (USD/ha)
  
  ## value of water in 1 ha
  rice_water_value_precal <- (rice_water_use * rice_water_value)*2 #rice water use is in (m3/ha) times estimated value of water used in rice production (USD/m3)
  
  rice_water_value <- vv(rice_water_value_precal, n_year, var_CV=CV_value,
                         relative_trend = inflation_rate) * rice_water_resource_risk # consider water shortage may happen (?)
  
  
  ###rice nutrition to health value (USD/ha) #estimated yearly value for accessing nutrition from rice
  rice_nutrition_health_value <- vv(rice_nutrition_to_health_value, 
                                       n_year, var_CV=CV_value,
                                       relative_trend = inflation_rate) * rice_nutritional_to_health_value_risk
  
  
  ### annual income for vegetables, assuming they grow 5 different vegetables per year
  chinese_mustard_green_income <- (chinese_mustard_green_yield*chinese_mustard_green_price) * 0.4 #assuming it is cultivated in 0.4 ha for each season and twice per year
  green_bean_income <- (green_bean_yield*green_bean_price) *  0.8 #assuming it is cultivated in 0.8 ha for each season and once per year
  cabbage_income <- (cabbage_yield*cabbage_price) * 0.8 ##assuming it is cultivated in 0.4 ha for each season and twice per year
  chili_income <- (chili_yield*chili_price) * 0.8 #assuming it is cultivated in 0.8 ha for each season and once per year
  tomato_income <- (tomato_yield*tomato_price) * 0.8 #assuming it is cultivated in 0.8 ha for each season and once per year
  spring_onion_income <- (spring_onion_yield*spring_onion_price) #assuming it is mixed with any crop, 0.2 ha for each season
  
  # annual income from vegetables per ha
  vegetables_income_precal <- chinese_mustard_green_income + green_bean_income +
                              cabbage_income + chili_income + spring_onion_income
  vegetables_income <- vv(vegetables_income_precal, n_year, var_CV=CV_value)
  
  ### calculate vegetables income value with risk
  vegetables_income_value <- vv(vegetables_income, n_year, var_CV=CV_value,
                                relative_trend = inflation_rate) * vegetables_production_risk
  
  
  ## value of water used for vegetable production (USD/ha), assuming that the value of water used for all vegetables are similar
  
  ## value of water in growing vegetables 
  vegetables_water_value <- (vegetables_water_use * vegetables_water_value) * 3 # assuming vegetables can be cultivated at least 3 season per year
  
  
  ###vegetables nutrition to health value (USD/ha)
  vegetables_nutrition_health_value <- vv(vegetables_nutrition_to_health_value,
                                             n_year, var_CV=CV_value,
                                             relative_trend = inflation_rate) * vegetables_nutritional_to_health_value_risk #estimated yearly value for accessing nutrition from vegetables
  
  
  
  ##if farmers grow both rice and vegetables
  ## calculate annual income both rice and vegetables
  rice_vegetables_income_value_precal <- (rice_income/2) + (vegetables_income/2) #assuming rice and vegetables are cultivated in 0.5 ha field for each
  rice_vegetables_income_value <- vv(rice_vegetables_income_value_precal,
                                          n_year, var_CV=CV_value, relative_trend = inflation_rate) * rice_vegetables_production_risk
  
  
  ###value of water used both rice and vegetables 
  rice_vegetables_water_value_precal <- rice_water_value + vegetables_water_value # assuming that rice and vegetables can be cultivated ate least once per year
  rice_vegetables_water_value <- vv(rice_vegetables_water_value_precal, #value of water used for production (USD/ha)
                                  n_year, var_CV=CV_value, relative_trend = inflation_rate) * rice_vegetables_water_resource_risk # consider water shortage may happen (?)
  
  
  ###nutrition to health value from both rice and vegetables
  rice_vegetables_nutritional_health_value_precal <- (rice_nutrition_health_value/2) + 
                                                  (vegetables_nutrition_health_value/2)
  rice_vegetables_nutritional_health_value <- vv(rice_vegetables_nutritional_health_value_precal, 
                                                 n_year, var_CV=CV_value, relative_trend = inflation_rate )
  
  # Sum up all benefits
  rice_total_benefit <- rice_income_value + rice_water_value + rice_nutrition_to_health_value
  vegetables_total_benefit <- vegetables_income_value + vegetables_water_value + vegetables_nutrition_to_health_value
  rice_vegetables_total_benefit <- rice_vegetables_income_value + rice_vegetables_water_value + rice_vegetables_nutritional_health_value
  
  
  # Final results
  ## option 1: if farmers grow rice
  rice_result <- rice_total_benefit - rice_cost
  
  ## option 2: if farmers grow vegetables
  vegetables_result <- vegetables_total_benefit - intervention_vegetable_cost
  
  ## option 3: if farmers grow both rice and vegetables
  rice_vegetables_result <- rice_vegetables_total_benefit - intervention_rice_and_vegetable_cost
  
  # Calculate NPV
  NPV_no_interv <- discount(x = rice_result, 
                            discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_vegetables_interv <- discount(x = vegetables_result, 
                                    discount_rate = discount_rate,
                                    calculate_NPV = TRUE)
  
  NPV_rice_vegetables_interv <- discount(x = rice_vegetables_result,
                                       discount_rate = discount_rate,
                                       calculate_NPV = TRUE)
  
  
  return(list(NPV_rice = NPV_no_interv,
              NPV_vegetables = NPV_vegetables_interv,
              NPV_rice_vegetables = NPV_rice_vegetables_interv,
              NPV_decision_rice = NPV_no_interv,
              NPV_decision_vegetables = NPV_vegetables_interv - NPV_no_interv,
              NPV_decision_rice_vegetables = NPV_rice_vegetables_interv - NPV_no_interv,
              cashflow_rice = rice_result,
              cashflow_vegetables = vegetables_result,
              cashflow_rice_vegetables = rice_vegetables_result))
}



# Run the Monte Carlo simulation using the model function
input_estimates <- read.csv("input_rice_to_vegs_usd.csv", sep = ",")

TRV_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                  model_function = transition_rice_to_vegetables,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")


# Subset the outputs from the mcSimulation function (y) to summarize only on the variables that we want.
# names(TRV_mc_simulation$x)
mcSimulation_summary <- data.frame(TRV_mc_simulation$x[5:35],
                                   # names(TRV_mc_simulation$x)
                                   TRV_mc_simulation$y[1:6])

gtExtras::gt_plt_summary(mcSimulation_summary)



# plot NPV distribution analysis
decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("NPV_decision_rice",
                                             "NPV_decision_vegetables", 
                                             "NPV_decision_rice_vegetables"),
                                             method = 'smooth_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("NPV_decision_vegetables", 
                                             "NPV_decision_rice_vegetables"),
                                    method = 'smooth_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("NPV_decision_rice", 
                                             "NPV_decision_vegetables",
                                             "NPV_decision_rice_vegetables"),
                                    method = 'boxplot')


decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("NPV_decision_vegetables", 
                                             "NPV_decision_rice_vegetables"),
                                    method = 'boxplot')



# cashflow analysis
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_rice")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_vegetables")
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_rice_vegetables")

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
mcSimulation_table <- data.frame(TRV_mc_simulation$x, TRV_mc_simulation$y[4:6])
evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_decision_rice")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_rice")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_vegetables")
plot_evpi(evpi_TRV, decision_vars = "NPV_decision_rice_vegetables")


compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_vegetables", 
                cashflow_var_name = "cashflow_vegetables", 
                base_size = 7)


compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_rice_vegetables", 
                cashflow_var_name = "cashflow_rice_vegetables", 
                base_size = 7)


compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_decision_rice", 
                cashflow_var_name = "cashflow_rice", 
                base_size = 7)
