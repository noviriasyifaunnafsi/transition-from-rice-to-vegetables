library(decisionSupport)
library(gtExtras)
library(svglite)

input_estimates <- read.csv("input_rice_to_vegs_usd.csv", sep = ";")

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_estimates))

transition_rice_to_vegetables <- function(x, varnames){
  

  ## Estimate the cost for 1 ha of field ##
  
  ## Rice ##
  
  # Annual rice cost under normal condition
  rice_cost_precal <- (rice_farming_input_costs + rice_machinery_costs + 
                         rice_labor_costs) + irrigation_maintenance_costs  
  
  rice_cost <- vv(rice_cost_precal, n_year, var_CV=CV_value, 
                  relative_trend = inflation_rate)
  
  
  # chance event options #
  
  # considering production risk (pest, disease, heavy rainfall, water shortage, etc.)
  # so farmers need to pay higher cost for production management risk
  
  # event-1, farmers need to buy more pesticides to overcome pest and disease outbreak
  rice_farmer_pay_higher_farming_input_costs_yes_no <- chance_event(chance_production_risk)
  
  rice_farming_input_costs_with_more_pesticides <- if(rice_farmer_pay_higher_farming_input_costs_yes_no == 1) {
    rice_farming_input_costs * # farming input cost under normal condition
      (1+portion_rice_farming_input_cost_for_pest_disease_management)
  } else {
    rice_farming_input_costs = rice_farming_input_costs 
  }
  
  # 
  # # event-2, when water shortage occur, farmers may need to rent water pump
  # rice_farmer_pay_higher_irrigation_costs_yes_no <- chance_event(chance_water_shortage)
  # 
  # rice_machinery_cost_with_pump <- if(rice_farmer_pay_higher_irrigation_costs_yes_no == 1) {
  #   rice_machinery_costs + # rice cost under normal condition
  #     water_pump_rent_fee
  # } else {
  #   rice_machinery_costs = rice_machinery_costs 
  # }
  # 
  
  # annual rice cost after considering production risk
  rice_cost_with_risk_precal <- rice_farming_input_costs_with_more_pesticides +
    rice_machinery_costs + rice_labor_costs + irrigation_maintenance_costs
  
  final_rice_cost <- vv(rice_cost_with_risk_precal, n_year, var_CV=CV_value,
                            relative_trend = inflation_rate)
  
    
 
  ## Vegetables ####
  
  # annual cost for vegetables, assuming farmers grow vegetables with mix 
  # cropping (6 crops per year)
  
  ## annual vegetable costs (all season throughout the year)
  vegetables_cost_precal <- vegetables_farming_input_costs + 
                              vegetables_machinery_costs + 
                              #assuming the machinery is used alternately among 
                              #farmers, cost is only for fuel
                              vegetables_labor_costs +  
                              irrigation_maintenance_costs
  vegetables_cost <- vv(vegetables_cost_precal, n_year, var_CV=CV_value)

  # calculating the implementation costs of growing vegetables 
  # costs in the first year is higher than following other years
  # considering farmers need to purchase more equipment and irrigation tools
  # for growing vegetables
  vegetables_cost[1]  <- vegetables_cost[1] + 
                          vegetables_equipment_costs_first_year + 
                          vegetables_irrigation_costs_first_year
  
  
  # chance event options #
  
  # considering production risk (pest, disease, heavy rainfall, water shortage, etc.)
  # so farmers need to pay higher cost for production management risk
  
  # event-1, farmers need to buy more pesticides to overcome pest and disease outbreak
  vegetable_farmer_pay_higher_farming_input_costs_yes_no <- chance_event(chance_production_risk)
  
  vegetable_farming_input_costs_with_more_pesticides <- if(vegetable_farmer_pay_higher_farming_input_costs_yes_no == 1) {
    vegetables_farming_input_costs * # farming input cost under normal condition
      (1+portion_vegetable_farming_input_cost_for_pest_disease_management)
  } else {
    vegetables_farming_input_costs = vegetables_farming_input_costs 
  }
  
  
  # event-2, when water shortage occur, farmers may need to rent water pump
  vegetable_farmer_pay_higher_irrigation_costs_yes_no <- chance_event(chance_water_shortage)
  
  vegetables_machinery_cost_with_pump <- if(vegetable_farmer_pay_higher_irrigation_costs_yes_no == 1) {
    vegetables_machinery_costs + # vegetable machinery cost under normal condition
      water_pump_rent_fee
  } else {
    vegetables_machinery_costs = vegetables_machinery_costs 
  }
  
  
  # annual vegetable cost after considering production risk
  vegetable_cost_with_risk <- vegetable_farming_input_costs_with_more_pesticides +
    vegetables_machinery_cost_with_pump + vegetables_labor_costs + irrigation_maintenance_costs + 
    irrigation_maintenance_costs

  final_vegetable_cost <- vv(vegetable_cost_with_risk, n_year, var_CV=CV_value,
                            relative_trend = inflation_rate)
  
  final_vegetable_cost[1] <- final_vegetable_cost + 
    vegetables_equipment_costs_first_year + 
    vegetables_irrigation_costs_first_year
  
  
  
  # Estimate the benefits
  
  # benefits of rice
  ## annual income for rice per ha under normal condition
  rice_revenue <- (rice_yield * rice_price)
  rice_income <- rice_revenue - rice_cost
  rice_income_benefit <- vv(rice_income, n_year, var_CV=CV_value, 
                           relative_trend = inflation_rate)
  
  
  ## chance event ##
  
  # considering production and financial risk damage on rice yield
  # assuming there are some risks like pest, disease, heavy rainfall, storm, and water shortage
  # causing some lose to rice yield and reduction to rice capital/cost
  # but may also reducing yield
  rice_farmer_loss_from_production_financial_risk_yes_no <- chance_event(chance_production_risk+chance_financial_risk_rice)
  
  rice_yield_with_production_financial_risk <- if(rice_farmer_loss_from_production_financial_risk_yes_no == 1) {
    rice_yield * # rice yield under normal condition
      (1-(prob_damage_production_risk_rice+prob_damage_market_risk_rice))
  } else {
    rice_yield = rice_yield 
  }
  
  
  # considering market risk damage on rice price
  # causing reduction to farmers' income
  rice_revenue_loss_from_market_risk_yes_no <- chance_event(chance_market_risk_rice)
  
  rice_income_with_market_risk <- if(rice_revenue_loss_from_market_risk_yes_no == 1) {
    rice_price * # rice yield under normal condition
      (1-prob_damage_market_risk_rice)
  } else {
    rice_price = rice_price 
  }
  
  
  # annual rice income after considering risk
  rice_revenue_with_risk_precal <- (rice_income_with_market_risk * 
                                     rice_yield_with_production_financial_risk)
  rice_revenue_with_risk <- vv(rice_revenue_with_risk_precal, n_year, var_CV=CV_value,
                              relative_trend = inflation_rate)  
    
    
  
  ## benefit of water in rice production (USD/ha)

  ## to calculate the value of water, we need to estimate the additional revenue
  ## comparing rainfed and well irrigated system
  
  ## value of water in 1 ha
  
  ## first, calculate water use value if compared to upland rice in rainfed system and irrigation system
  ## we use the information from rainfed system as baseline comparison for calculating
  ## rice water value benefit
  
  # estimate value of water use in rice production by subtracting rice water use 
  # in rainfed system to irrigated system (m3/ha/year)
  rice_water_use_value <- rice_water_use_upland_irrigated - rice_water_use_upland_rainfed
  
  # estimate the additional revenue if rice is well-irrigated
  # rice rainfed system --> yield is lower than well-irrigated system
  # (USD/ha/year)
  rice_rainfed_value <- rice_price * rice_yield_rainfed
  rice_irrigated_value <- rice_price * rice_yield_irrigated
  
  # additional revenue is calculated by subtracting the estimated revenue 
  # from rainfed system to irrigation system (USD/ha/year)
  rice_additional_revenue_from_well_irrigation <- rice_irrigated_value - rice_rainfed_value
  
  # estimate economic value of water in rice production (USD/m3)
  rice_water_value <- (rice_additional_revenue_from_well_irrigation / rice_water_use_value) 
                           
  rice_water_benefit <- vv(rice_water_value, n_year, var_CV=CV_value,
                          relative_trend = inflation_rate)
   
  
  ### rice nutrition to health value (USD/ha) 
  ### annual health saving costs from rice
  rice_nutrition_health_benefit <- vv(rice_nutrition_to_health_value, 
                                    n_year, var_CV=CV_value,
                                    relative_trend = inflation_rate) 
  
  
  
  
  # benefits of vegetables
  ### annual income for vegetables, assuming they grow 6 different vegetables per year
  ### spring onion is always cultivated in every season and always mixed with main crops
  
  chinese_mustard_green_income <- (chinese_mustard_green_yield * chinese_mustard_green_price) * 0.6 
                                  #assuming only 60% of 1 ha field is cultivated per year
  
  green_bean_income <- (green_bean_yield * green_bean_price) *  0.8 
                        #assuming only 80% of 1 ha field is cultivated per year
  
  cabbage_income <- (cabbage_yield * cabbage_price) * 0.6 
                      #assuming only 60% of 1 ha field is cultivated per year
  
  chili_income <- (chili_yield * chili_price) * 0.8 
                      #assuming only 80% of 1 ha field is cultivated per year
  
  tomato_income <- (tomato_yield * tomato_price) * 0.8 
                      #assuming only 80% of 1 ha field is cultivated per year
  
  spring_onion_income <- (spring_onion_yield * spring_onion_price) * 1.4 
                          #assuming 1.4 ha of field is cultivated per year
  
  
  # annual income from vegetables per ha under normal condition
  vegetables_revenue <- chinese_mustard_green_income + green_bean_income +
    cabbage_income + chili_income + spring_onion_income
  vegetables_income <- vegetables_revenue - vegetables_cost
  vegetables_income_benefit <- vv(vegetables_income, n_year, var_CV=CV_value)
  
  
  
  ## calculate total vegetables yield for estimating production risk 
  chinese_mustard_green_yield_per_year <- chinese_mustard_green_yield * 0.6
  #assuming only 60% of 1 ha field is cultivated per year
  
  green_bean_yield_per_year <- green_bean_yield *  0.8 
  #assuming only 80% of 1 ha field is cultivated per year
  
  cabbage_yield_per_year <- cabbage_yield * 0.6 
  #assuming only 60% of 1 ha field is cultivated per year
  
  chili_yield_per_year <- chili_yield * 0.8 
  #assuming only 80% of 1 ha field is cultivated per year
  
  tomato_yield_per_year <- tomato_yield * 0.8 
  #assuming only 80% of 1 ha field is cultivated per year
  
  spring_onion_yield_per_year <- spring_onion_yield * 1.4 
  #assuming 1.4 ha of field is cultivated per year
  
  total_vegetables_yield_precal <- chinese_mustard_green_yield_per_year +
    green_bean_yield_per_year +
    cabbage_yield_per_year +
    chili_yield_per_year +
    tomato_yield_per_year +
    spring_onion_yield_per_year
  
  total_vegetables_yield <- vv(total_vegetables_yield_precal, 
                               n_year, var_CV=CV_value,
                               relative_trend = inflation_rate) 
  
  ## chance event ##
  
  # considering production and financial risk damage on vegetable yield
  # assuming there are some risks like pest, disease, heavy rainfall, storm, and water shortage
  # causing some lose to vegetable yield and reduction to vegetable capital/cost
  # but may also reducing yield
  vegetable_farmer_loss_from_production_financial_risk_yes_no <- chance_event(chance_production_risk +
                                                                      chance_financial_risk_vegetables)
  
  vegetable_yield_with_production_financial_risk <- if(vegetable_farmer_loss_from_production_financial_risk_yes_no == 1) {
    total_vegetables_yield * # vegetable yield under normal condition
      (1-(prob_damage_production_risk_vegetables + prob_damage_market_risk_vegetables))
  } else {
    total_vegetables_yield = total_vegetables_yield 
  }
  
  
  # considering market risk damage on vegetable price
  # causing reduction to farmers' income
  vegetable_income_loss_from_market_risk_yes_no <- chance_event(chance_market_risk_vegetables)
  
  vegetable_income_with_market_risk <- if(vegetable_income_loss_from_market_risk_yes_no == 1) {
    vegetable_prices * # rice yield under normal condition
      (1-prob_damage_market_risk_vegetables)
  } else {
    vegetable_prices = vegetable_prices 
  }
  
  
  # annual vegetables income after considering risk
  vegetables_revenue_with_risk_precal <- (vegetable_yield_with_production_financial_risk *
                                           vegetable_income_with_market_risk)
  
  vegetables_revenue_with_risk <- vv(vegetables_revenue_with_risk_precal, n_year, var_CV=CV_value,
                              relative_trend = inflation_rate)
  
  
  ## value of water used for vegetable production (USD/ha), assuming that the value of water used for all vegetables are similar
  
  ## value of water in growing vegetables
  ## first, estimate water savings from vegetables production over rice
  vegetables_water_savings <- rice_water_use - vegetables_water_use
  
  # estimate the additional revenue if growing vegetables
  # additional revenue is calculated by subtracting the estimated vegetables income 
  # to rice income (USD/ha/year)
  vegetables_additional_revenue <- vegetables_income - rice_income
  
  # estimate economic value of water in rice production
  vegetables_water_value <- (vegetables_additional_revenue / vegetables_water_savings) 
  
  vegetables_water_benefit <- vv(vegetables_water_value, n_year, var_CV=CV_value,
                          relative_trend = inflation_rate)
  
  
  ### vegetables nutrition to health value (USD/ha)
  ### annual health saving costs from vegetables
  vegetables_nutrition_health_benefit <- vv(vegetables_nutrition_to_health_value,
                                          n_year, var_CV=CV_value,
                                          relative_trend = inflation_rate) 
  
  
  
  
  # Sum up all benefits under normal condition
  benefit_rice <- rice_income_benefit + rice_water_benefit + rice_nutrition_to_health_value
  benefit_vegetables <- vegetables_income_benefit + vegetables_water_benefit + vegetables_nutrition_to_health_value
  
  # Sum up all benefits after considering risks
  final_benefit_rice <- rice_revenue_with_risk + rice_water_benefit + rice_nutrition_to_health_value
  final_benefit_vegetables <- vegetables_revenue_with_risk + vegetables_water_benefit + vegetables_nutrition_to_health_value
  

  # Final results
  ## option 1: if farmers grow rice
  rice_result <- final_benefit_rice - final_rice_cost
  
  ## option 2: if farmers grow vegetables
  vegetables_result <- final_benefit_vegetables - final_vegetable_cost
  
  # Calculate NPV
  NPV_no_interv <- discount(x = rice_result, 
                            discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_vegetables_interv <- discount(x = vegetables_result, 
                                    discount_rate = discount_rate,
                                    calculate_NPV = TRUE)
  
  
  return(list(NPV_rice = NPV_no_interv,
              NPV_vegetables = NPV_vegetables_interv,
              NPV_rice = NPV_no_interv,
              NPV_decision_do = NPV_vegetables_interv - NPV_no_interv,
              cashflow_rice = rice_result,
              cashflow_vegetables = vegetables_result))
}



# Run the Monte Carlo simulation using the model function
TRV_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                  model_function = transition_rice_to_vegetables,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")


# Subset the outputs from the mcSimulation function (y) to summarize only on the variables that we want.
# names(TRV_mc_simulation$x)
mcSimulation_summary <- data.frame(TRV_mc_simulation$x[5:33],
                                   # names(TRV_mc_simulation$x)
                                   TRV_mc_simulation$y[1:2])

gtExtras::gt_plt_summary(mcSimulation_summary)



# plot NPV distribution analysis

# with smooth simple overlay method
decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("NPV_rice",
                                             "NPV_vegetables",
                                             "NPV_tradeoff"),
                                    colors = c("skyblue", "seagreen1", "yellow"),
                                    method = 'smooth_simple_overlay',
                                    bins = 10, binwidth = 500, base_size = 10)

# with histogram method
decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("NPV_rice",
                                             "NPV_vegetables"),
                                             method = 'smooth_simple_overlay')

decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("NPV_rice",
                                             "NPV_decision_do"),
                                    method = 'smooth_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("NPV_rice",
                                             "NPV_vegetables"),
                                    method = 'boxplot')

decisionSupport::plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("NPV_rice",
                                             "NPV_vegetables"),
                                    method = 'hist_simple_overlay', , bins = 200, binwidth = 1e6)


# cashflow analysis
plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_rice",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "yellow2", color_5_95 = "green4",
              color_median = "red")

plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_vegetables",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "lightblue", color_5_95 = "blue4",
              color_median = "red")

# Projection to Latent Structures (PLS) analysis
pls_result_rice <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[1], ncomp = 1)
plot_pls(pls_result_rice, threshold = 0)

pls_result_vegetables <- plsr.mcSimulation(object = TRV_mc_simulation,
                                resultName = names(TRV_mc_simulation$y)[2], ncomp = 1)
plot_pls(pls_result_vegetables, threshold = 0)



# VoI analysis
mcSimulation_table <- data.frame(TRV_mc_simulation$x, TRV_mc_simulation$y[1:2])
evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_rice")
plot_evpi(evpi_TRV, decision_vars = "NPV_vegetables")


# compound figure
compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_rice, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_rice", 
                cashflow_var_name = "cashflow_rice", 
                base_size = 7)

compound_figure(mcSimulation_object = TRV_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_vegetables, 
                EVPIresults = evpi_TRV, decision_var_name = "NPV_vegetables", 
                cashflow_var_name = "cashflow_vegetables", 
                base_size = 7)

