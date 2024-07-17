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
  
  
  # Rice system ####
  
  # Considering rice production as baseline #
  
  ## Rice costs ####
  
  ### Rice farming costs ####
  
  # Annual rice farming cost under normal condition
  # Assuming rice is cultivated twice per year
  rice_farming_cost_precal <- rice_farming_input_costs + rice_machinery_costs + 
    rice_labor_costs + irrigation_maintenance_costs  
  
  rice_farming_cost <- vv(rice_farming_cost_precal, n_year, var_CV=CV_value, 
                          relative_trend = inflation_rate)
  
  
  ## Chance event options on rice farming cost #
  
  # considering production risk (pest, disease, heavy rainfall, water shortage, etc.)
  # so farmers need to pay higher cost for production management risk
  # event-1, farmers need to buy more pesticides to overcome pest and disease outbreak
  
  rice_farming_input_costs_with_more_pesticides_precal <- rice_farming_input_costs * # farming input cost under normal condition
    (1+portion_rice_farming_input_cost_for_pest_disease_management)
  
  rice_farming_input_costs_with_more_pesticides <- chance_event(chance_production_risk,
                                                                value_if = rice_farming_input_costs_with_more_pesticides_precal,
                                                                value_if_not = rice_farming_input_costs,
                                                                n = n_year,
                                                                CV_if = 10,
                                                                CV_if_not = CV_value)
  
  
  
  # considering financial risk (lack of capital) damage on farming cost
  # farmers may not have enough capital to purchase farming inputs
  # then, they might get loan from bank with micro-credit program (KUR)
  # we can consider that farmer who own 1 ha of field can get loan from IDR 10 mio to
  # 50 mio (USD 625 to 3125)
  
  # event-2, farmers get loan from bank, so they need to pay annual interest
  # this can be add up to their farming cost
  # consider if the farming input cost is under normal condition
  
  rice_farmer_get_bank_loan <- if(rice_farming_cost_precal == range_loan_for_farmers) {
    rice_farmer_get_bank_loan = TRUE
  } else {
    rice_farmer_get_bank_loan = FALSE
  }
  
  rice_farming_cost_if_get_bank_loan <- if(rice_farmer_get_bank_loan == TRUE) {
    rice_farming_cost + (rice_farming_cost * annual_bank_interest)
  } else {
    rice_farming_cost = rice_farming_cost
  }
  
  rice_farming_cost_with_bank_loan <- chance_event(chance_production_risk,
                                                   value_if = rice_farming_cost_if_get_bank_loan,
                                                   value_if_not = rice_farming_cost,
                                                   n = n_year,
                                                   CV_if = 10,
                                                   CV_if_not = CV_value)
  
  # event-3, farmers get loan from bank, so they need to pay annual interest
  # this can be add up to their farming cost
  # consider if the farming input cost is higher because farmers need to buy 
  # more pesticides to overcome pest and disease outbreak
  
  rice_farming_high_input_cost_with_bank_loan_precal <- rice_farming_input_costs_with_more_pesticides_precal +
    rice_machinery_costs + rice_labor_costs + irrigation_maintenance_costs 
  
  rice_farming_high_input_cost_with_bank_loan <- chance_event(chance_production_risk,
                                                              value_if = rice_farming_high_input_cost_with_bank_loan_precal,
                                                              value_if_not = rice_farming_cost_precal,
                                                              n = n_year,
                                                              CV_if = 10,
                                                              CV_if_not = CV_value)
  
  
  
  # annual rice farming cost after considering production and financial risk
  rice_farming_cost_with_risk_precal <- rice_farming_high_input_cost_with_bank_loan
  
  final_rice_farming_cost <- vv(rice_farming_cost_with_risk_precal, n_year, var_CV=CV_value,
                                relative_trend = inflation_rate)
  
  
  
  ### Rice compost cost ####
  
  # Normally, most rice farmers don't utilize the rice biomass residue as compost
  # They will just simply throw the waste away or burn it
  
  # Here, we can assume that what if the rice farmers utilize the rice biomass
  # residue into compost
  
  # Annual cost for composting rice biomass residue
  # First, we need to calculate the establishment cost for composting
  # by preparing composting facility, like compost bin and equipment
  # the cost would be paid only for the first year
  
  first_year_composting_cost <- compost_bin_cost + composter_equipment_cost
  
  # Calculate labor cost for composting
  # Assuming that labor is mostly needed at the end of cultivation season
  # let's say around 5 days/season. So, farmer need to hire labors to help 
  # them at least 10 days/year.
  # The rest work would be handle by the farmers themselves
  
  annual_cost_labor_rice_composting <- daily_labor_cost * 10 #10 days for hiring labor
  
  # Then, we calculate the annual rice biomass composting cost
  # Which is included for the cost of labor, compost activator, and maintenance
  rice_compost_cost_precal <- annual_cost_labor_rice_composting +
    compost_activator_cost + composter_maintenance_cost
  
  rice_compost_cost <- vv(rice_compost_cost_precal, n_year, var_CV=10)
  
  # Calculate the cost with the first year establishment cost
  rice_compost_cost[1] <- rice_compost_cost[1] + first_year_composting_cost 
  
  
  ## Chance event options on rice compost cost ##
  
  # Considering financial risk (lack of capital) damage on composting rice
  # farmers may not have enough capital to purchase composter tools and equipment
  # So they might get loan from bank with micro-credit program (KUR)
  # we can consider that farmer who own 1 ha of field can get loan from IDR 10 mio to
  # 50 mio (USD 625 to 3125)
  
  # event-1, farmers get loan from bank, so they need to pay annual interest
  # this can be add up to their first year composting cost
  
  rice_compost_first_year_get_bank_loan <- if(first_year_composting_cost == range_loan_for_farmers) {
    rice_compost_first_year_get_bank_loan = TRUE
  } else {
    rice_compost_first_year_get_bank_loan = FALSE
  }
  
  rice_compost_cost_first_year_if_get_bank_loan <- if(rice_compost_first_year_get_bank_loan == TRUE) {
    first_year_composting_cost + (first_year_composting_cost * annual_bank_interest)
  } else {
    first_year_composting_cost = first_year_composting_cost
  }
  
  rice_compost_first_year_cost_with_bank_loan <- chance_event(chance_production_risk,
                                                   value_if = rice_compost_cost_first_year_if_get_bank_loan,
                                                   value_if_not = first_year_composting_cost,
                                                   n = n_year,
                                                   CV_if = 10,
                                                   CV_if_not = CV_value)
  
  # event-2, farmers get loan from bank, so they need to pay annual interest
  # this can be add up to their composting cost
  
  rice_compost_get_bank_loan <- if(rice_compost_cost_precal == range_loan_for_farmers) {
    rice_farmer_compost_get_bank_loan = TRUE
  } else {
    rice_farmer_compost_get_bank_loan = FALSE
  }
  
  rice_compost_cost_if_get_bank_loan <- if(rice_compost_get_bank_loan == TRUE) {
    rice_compost_cost + (rice_compost_cost * annual_bank_interest)
  } else {
    rice_compost_cost = rice_compost_cost
  }
  
  rice_compost_cost_with_bank_loan <- chance_event(chance_production_risk,
                                                   value_if = rice_compost_cost_if_get_bank_loan,
                                                   value_if_not = rice_compost_cost,
                                                   n = n_year,
                                                   CV_if = 10,
                                                   CV_if_not = CV_value)
  
  
  
  # annual rice compost cost after considering production and financial risk
  rice_compost_cost_if_get_bank_loan[1] <- rice_compost_cost_if_get_bank_loan[1] + rice_compost_first_year_cost_with_bank_loan
  
  final_rice_compost_cost <- vv(rice_compost_cost_if_get_bank_loan, n_year, var_CV=CV_value,
                                relative_trend = inflation_rate)
  
  
  
  ### Processed rice product cost ####
  
  # In case of rice, this is probably not feasible to process rice as other
  # processed product. So, we can consider that the processed rice product cost
  # as zero
  
  processed_rice_product_cost <- 0
  
  
  ### Eco-tourism for rice field cost ####
  
  # First, we need to calculate the establishment cost for eco-tourism
  # by preparing some signs to be installed in the area
  # the cost would be paid only for the first year
  
  first_year_rice_ecotourism_cost <- sign_installation_cost
  
  # Then, we calculate the annual cost of eco-tourism for rice field
  # Which is included for marketing and promotion
  annual_rice_ecotourism_cost <- marketing_and_promotion_ecotourism
  
  rice_ecotourism_cost <- vv(annual_rice_ecotourism_cost, n_year, var_CV=10)
  
  # Calculate the cost with the first year establishment cost
  rice_ecotourism_cost[1] <- rice_ecotourism_cost[1] + first_year_rice_ecotourism_cost 
  
  # We consider that there is no risk affect cost of rice eco-tourism cost
  # since the cost might be not so high
  
  
  ## Rice benefits ####
  
  ### Rice farming benefits ####
  
  # Annual rice farming income under normal condition
  # Assuming rice is cultivated twice per year
  rice_farming_revenue_precal <- rice_yield * rice_price
  rice_farming_revenue <- vv(rice_farming_revenue_precal, n_year, var_CV=CV_value, 
                             relative_trend = inflation_rate) 
  
  
  
  ## Chance event options on rice farming benefits ##
  
  # considering production risk damage on rice yield
  # assuming there are some risks like pest, disease, heavy rainfall, storm, 
  # and water shortage causing some lose to rice yield and reduction to rice 
  # capital/cost may also reducing yield
  
  # event-1 rice yield reduction from production and financial risks
  rice_yield_loss_with_production_financial_risk <- rice_yield * (prob_damage_production_risk_rice+prob_damage_market_risk_rice)
  rice_yield_with_production_financial_risk_precal <- rice_yield - (rice_yield_loss_with_production_financial_risk)
  
  rice_yield_with_production_financial_risk <- chance_event((chance_production_risk +
                                                               chance_financial_risk_rice),
                                                            value_if = rice_yield_with_production_financial_risk_precal,
                                                            value_if_not = rice_yield,
                                                            n = n_year,
                                                            CV_if = 10,
                                                            CV_if_not = CV_value)
  
  
  # considering market risk damage on rice price
  # causing reduction to farmers' revenue
  
  # event-2 rice revenue reduction from market risk under normal condition
  rice_price_with_market_risk <- rice_price * prob_damage_market_risk_rice
  rice_farming_revenue_loss_with_market_risk_normal <- rice_price_with_market_risk * rice_yield
  
  rice_farming_revenue_with_market_risk_normal <- chance_event(chance_market_risk_rice,
                                                               value_if = rice_farming_revenue_loss_with_market_risk_normal,
                                                               value_if_not = rice_farming_revenue_precal,
                                                               n = n_year,
                                                               CV_if = 10,
                                                               CV_if_not = CV_value)
  
  
  # event-3 rice revenue reduction from market risk under production and financial risk
  rice_farming_revenue_loss_with_market_production_financial_risk <- rice_price_with_market_risk * rice_yield_with_production_financial_risk_precal
  
  rice_farming_revenue_with_market_production_financial_risk <- chance_event(chance_market_risk_rice,
                                                                             value_if = rice_farming_revenue_loss_with_market_production_financial_risk,
                                                                             value_if_not = rice_farming_revenue_precal,
                                                                             n = n_year,
                                                                             CV_if = 10,
                                                                             CV_if_not = CV_value)
  
  
  # annual rice farming revenue after considering risk
  final_rice_farming_revenue <- vv(rice_farming_revenue_with_market_production_financial_risk, n_year, var_CV=CV_value,
                                   relative_trend = inflation_rate) 
  
  
  ### Rice compost benefits ####
  
  # Assuming that 50% of biomass resulting in compost
  rice_compost <- biomass_rice * 0.5
  
  # Annual income for composting rice biomass residue
  rice_compost_revenue_precal <- rice_compost * price_rice_compost
  rice_compost_revenue <- vv(rice_compost_revenue_precal, n_year, var_CV=10,
                             relative_trend = inflation_rate)
  
  
  ## Chance event options on rice compost benefits ##
  
  # Considering market risk damage on composting rice
  # farmers may not be able to sell their compost due to low market demand
  # this can reduce the revenue from rice compost
  
  rice_compost_revenue_loss_market_risk <- rice_compost_revenue - 
    (rice_compost_revenue * portion_compost_loss_revenue)
  
  rice_compost_revenue_with_market_risk <- chance_event(chance_market_risk_compost,
                                                        value_if = rice_compost_revenue_loss_market_risk,
                                                        value_if_not = rice_compost_revenue,
                                                        n = n_year,
                                                        CV_if = 10,
                                                        CV_if_not = CV_value)
  
  # annual rice compost revenue after considering risk
  final_rice_compost_revenue <- vv(rice_compost_revenue_with_market_risk, n_year, var_CV=CV_value,
                                   relative_trend = inflation_rate) 
  
  
  
  ### Processed rice production benefits ####
  
  # In case of rice, this is probably not feasible to process rice as other
  # processed product. So, we can consider that the processed rice product benefit
  # as zero
  processed_rice_product_revenue <- 0
  
  
  ### Eco-tourism for rice field benefits ####
  
  # Annual income from rice field eco-tourism  (based on current condition)
  rice_ecotourism_revenue_precal <- rice_ecotourism_value
  rice_ecotourism_revenue <- vv(rice_ecotourism_revenue_precal, n_year, var_CV=10)
  
  
  ### Health outcomes from rice field benefits ####
  
  # We can assume that the benefit by consuming rice can reduce hospital
  # or medication bills
  
  # annual health saving costs from consuming rice
  rice_nutrition_health_benefit <- vv(health_cost_savings_from_rice,
                                      n_year, var_CV=CV_value,
                                      relative_trend = inflation_rate)
  
  
  ## Rice system outcomes #### 
  
  ### Calculate income of each option after considering risks
  
  # Rice farming income
  rice_farming_income <- final_rice_farming_revenue - final_rice_farming_cost
  
  # Rice compost income
  rice_compost_income <- final_rice_compost_revenue - final_rice_farming_cost
  
  # Rice processed production income
  rice_processed_income <- processed_rice_product_revenue - processed_rice_product_cost
  
  # Rice eco-tourism income 
  rice_ecotourism_income <- rice_ecotourism_revenue - rice_ecotourism_cost
  
  # Rice health
  rice_health_benefit <- rice_nutrition_health_benefit
  
  #### Final result for rice ##
  
  final_rice_benefits_precal <- rice_farming_income + rice_compost_income +
    rice_processed_income + rice_ecotourism_income + rice_health_benefit
  
  rice_result <- vv(final_rice_benefits_precal, n_year, var_CV=CV_value, 
                    relative_trend = inflation_rate)
  
  
  
  # Vegetable system ####
  
  ## Vegetable costs ####
  
  #### Vegetable farming cost ####
  
  # Annual vegetable farming cost under normal condition
  # Assuming farmers grow vegetables with mix cropping (6 crops per year)
  
  ## annual vegetable costs (all season throughout the year)
  vegetables_farming_cost_precal <- vegetables_farming_input_costs + 
    vegetables_machinery_costs + 
    vegetables_labor_costs +  
    irrigation_maintenance_costs
  
  vegetables_farming_cost <- vv(vegetables_farming_cost_precal, n_year, var_CV=10,
                                relative_trend = inflation_rate)
  
  # calculating the implementation costs of growing vegetables 
  # costs in the first year is higher than following other years
  # considering farmers need to purchase more equipment and irrigation tools
  # for growing vegetables
  vegetables_farming_cost[1]  <- vegetables_farming_cost[1] + 
    vegetables_equipment_costs_first_year + 
    vegetables_irrigation_costs_first_year
  
  
  # chance event options on vegetable farming costs #
  
  # considering production risk (pest, disease, heavy rainfall, water shortage, etc.)
  # so farmers need to pay higher cost for production management risk
  
  # event-1, farmers need to buy more pesticides to overcome pest and disease outbreak
  vegetable_farming_input_costs_with_more_pesticides_precal <- vegetables_farming_input_costs * # farming input cost under normal condition
    (1+portion_vegetable_farming_input_cost_for_pest_disease_management)
  
  vegetable_farming_input_costs_with_more_pesticides <- chance_event(chance_production_risk,
                                                                     value_if = vegetable_farming_input_costs_with_more_pesticides_precal,
                                                                     value_if_not = vegetables_farming_input_costs,
                                                                     n = n_year,
                                                                     CV_if = 10,
                                                                     CV_if_not = CV_value)
  
  
  # considering financial risk (lack of capital) damage on farming cost
  # farmers may not have enough capital to purchase farming inputs
  # then, they might get loan from bank with micro-credit program (KUR)
  # we can consider that farmer who own 1 ha of field can get loan from IDR 10 mio to
  # 50 mio (USD 625 to 3125)
  
  # event-2, farmers get loan from bank, so they need to pay annual interest
  # this can be add up to their farming cost
  # consider if the farming input cost is under normal condition
  
  vegetable_farmer_get_bank_loan <- if(vegetables_farming_cost_precal == range_loan_for_farmers) {
    vegetable_farmer_get_bank_loan = TRUE
  } else {
    vegetable_farmer_get_bank_loan = FALSE
  }
  
  vegetable_farming_cost_if_get_bank_loan <- if(vegetable_farmer_get_bank_loan == TRUE) {
    vegetables_farming_cost + (vegetables_farming_cost * annual_bank_interest)
  } else {
    vegetables_farming_cost = vegetables_farming_cost
  }
  
  vegetable_farming_cost_with_bank_loan <- chance_event(chance_production_risk,
                                                        value_if = vegetable_farming_cost_if_get_bank_loan,
                                                        value_if_not = vegetables_farming_cost,
                                                        n = n_year,
                                                        CV_if = 10,
                                                        CV_if_not = CV_value)
  
  # event-3, farmers get loan from bank, so they need to pay annual interest
  # this can be add up to their farming cost
  # consider if the farming input cost is higher because farmers need to buy 
  # more pesticides to overcome pest and disease outbreak
  
  vegetable_farming_high_input_cost_with_bank_loan_precal <- vegetable_farming_input_costs_with_more_pesticides +
    vegetables_machinery_costs + 
    vegetables_labor_costs +  
    irrigation_maintenance_costs 
  
  vegetable_farming_high_input_cost_with_bank_loan <- chance_event(chance_production_risk,
                                                                   value_if = vegetable_farming_high_input_cost_with_bank_loan_precal,
                                                                   value_if_not = vegetables_farming_cost,
                                                                   n = n_year,
                                                                   CV_if = 10,
                                                                   CV_if_not = CV_value)
  
  
  
  # annual vegetable farming cost after considering production risk
  final_vegetable_farming_cost <- vv(vegetable_farming_high_input_cost_with_bank_loan, n_year, var_CV=CV_value,
                                     relative_trend = inflation_rate)
  
  final_vegetable_farming_cost[1] <- final_vegetable_farming_cost[1] + 
    vegetables_equipment_costs_first_year + 
    vegetables_irrigation_costs_first_year
  
  
  
  #### Vegetable compost cost ####
  
  # Annual cost for composting vegetable biomass residue
  
  # First, we need to calculate the establishment cost for composting
  # by preparing composting facility, like compost bin and equipment
  # the cost would be paid only for the first year
  
  first_year_composting_cost <- compost_bin_cost + composter_equipment_cost
  
  # Calculate labor cost for composting
  # Assuming that labor is mostly needed at the end of cultivation season
  # let's say around 5 days/season. So, farmer need to hire labors to help 
  # them at least 15 days/year.
  # The rest work would be handle by the farmers themselves
  
  annual_cost_labor_vegetable_composting <- daily_labor_cost * 15 #15 days for hiring labor
  
  # Then, we calculate the annual vegetable biomass composting cost
  # Which is included for the cost of labor, compost activator, and maintenance
  vegetable_compost_cost_precal <- annual_cost_labor_vegetable_composting +
    compost_activator_cost + composter_maintenance_cost
  
  vegetable_compost_cost <- vv(vegetable_compost_cost_precal, n_year, var_CV=5)
  
  # Calculate the cost with the first year establishment cost
  vegetable_compost_cost[1] <- vegetable_compost_cost[1] + first_year_composting_cost
  
  
  ## Chance event option on vegetables compost cost ##
  
  # Considering financial risk (lack of capital) damage on composting vegetables
  # farmers may not have enough capital to purchase composter tools and equipment
  # So they might get loan from bank with micro-credit program (KUR)
  # we can consider that farmer who own 1 ha of field can get loan from IDR 10 mio to
  # 50 mio (USD 625 to 3125)
  
  # event-1, farmers get loan from bank, so they need to pay annual interest
  # this can be add up to their first year composting cost
  
  vegetable_compost_first_year_get_bank_loan <- if(first_year_composting_cost == range_loan_for_farmers) {
    vegetable_compost_first_year_get_bank_loan = TRUE
  } else {
    rice_compost_first_year_get_bank_loan = FALSE
  }
  
  vegetable_compost_cost_first_year_if_get_bank_loan <- if(vegetable_compost_first_year_get_bank_loan == TRUE) {
    first_year_composting_cost + (first_year_composting_cost * annual_bank_interest)
  } else {
    first_year_composting_cost = first_year_composting_cost
  }
  
  vegetable_compost_first_year_cost_with_bank_loan <- chance_event(chance_production_risk,
                                                              value_if = vegetable_compost_cost_first_year_if_get_bank_loan,
                                                              value_if_not = first_year_composting_cost,
                                                              n = n_year,
                                                              CV_if = 10,
                                                              CV_if_not = CV_value)
  
  
  # event-2, farmers get loan from bank, so they need to pay annual interest
  # this can be add up to their composting cost
  
  vegetable_compost_get_bank_loan <- if(vegetable_compost_cost_precal == range_loan_for_farmers) {
    vegetable_compost_get_bank_loan = TRUE
  } else {
    vegetable_compost_get_bank_loan = FALSE
  }
  
  vegetable_compost_cost_if_get_bank_loan <- if(vegetable_compost_get_bank_loan == TRUE) {
    vegetable_compost_cost + (vegetable_compost_cost * annual_bank_interest)
  } else {
    vegetable_compost_cost = vegetable_compost_cost
  }
  
  vegetable_compost_cost_with_bank_loan <- chance_event(chance_production_risk,
                                                        value_if = vegetable_compost_cost_if_get_bank_loan,
                                                        value_if_not = vegetable_compost_cost,
                                                        n = n_year,
                                                        CV_if = 10,
                                                        CV_if_not = CV_value)
  
  
  
  # annual vegetable compost cost after considering production and financial risk
  vegetable_compost_cost_if_get_bank_loan[1] <- vegetable_compost_cost_if_get_bank_loan[1] + vegetable_compost_first_year_cost_with_bank_loan
  
  final_vegetable_compost_cost <- vv(vegetable_compost_cost_if_get_bank_loan, n_year, var_CV=CV_value,
                                     relative_trend = inflation_rate)
  
  
  #### Processed vegetables product cost ####
  
  # First, we need to calculate the establishment cost for processing vegetable
  # products. In this case, we consider that the processing facilities including
  # post-harvest warehouse, processing factory, and machine are already 
  # facilitated by government funding. So farmer only need to bear cost of 
  # maintenance, packaging product, marketing, and distribution
  
  maintenance_vegetable_processing_cost <- vegetable_processing_equipment_maintenance_cost +
    vegetable_storage_maintenance_cost
  
  marketing_distribution_vegetable_processing_cost <- marketing_and_distribution_for_vegetable_product
  
  # Calculate labor cost for processing vegetables
  # Normally, the processing is done by the women farmers' group
  # Each group only processing the products from their own group
  # So they rarely hire labor
  
  # Here, we can just assume that if they hire labor during the peak season (?)
  # Let's say, they can hire labor 2 person for 1 week per season
  # If vegetable season is 3 times per year, then it's gonna be 6 person hired
  # per year. Daily labor will be calculated for 2 person x 7 days x 3 = 42 labors
  
  annual_cost_labor_vegetable_processing <- daily_labor_cost * 42 #42 days for hiring labor
  
  # Then, we calculate the annual vegetable processing product cost
  # Which is included for the cost of labor, maintenance, marketing, and distribution
  vegetable_processing_cost_precal <- annual_cost_labor_vegetable_processing +
    maintenance_vegetable_processing_cost + marketing_distribution_vegetable_processing_cost
  
  vegetable_processing_cost <- vv(vegetable_processing_cost_precal, n_year, var_CV=10,
                                  relative_trend = inflation_rate)
  
  
  
  # chance event options on vegetable processing costs #
  
  # considering financial risk (lack of capital)
  # first, we simply consider that farmers can't proceed to process vegetable
  # so they probably just simply throw away their 
  
  # event-1, farmers don't have enough capital for processing vegetable
  vegetable_farmer_have_no_capital_for_processing <- chance_event(chance_financial_risk_vegetables,
                                                                  value_if = 0,
                                                                  value_if_not = vegetable_processing_cost_precal,
                                                                  n = n_year)
  
  
  # Second, we can consider farmers need to request loan for processing vegetable
  # we consider that farmer get a loan from a bank
  # they need to pay annual interest, and this is can add up their costs
  
  vegetable_processing_get_bank_loan <- if(vegetable_processing_cost_precal == range_loan_for_farmers) {
    vegetable_processing_get_bank_loan = TRUE
  } else {
    vegetable_processing_get_bank_loan = FALSE
  }
  
  vegetable_processing_cost_if_get_bank_loan <- if(vegetable_processing_get_bank_loan == TRUE) {
    vegetable_processing_cost + (vegetable_processing_cost * annual_bank_interest)
  } else {
    vegetable_processing_cost = vegetable_processing_cost
  }
  
  vegetable_processing_cost_with_bank_loan <- chance_event(chance_production_risk,
                                                           value_if = vegetable_processing_cost_if_get_bank_loan,
                                                           value_if_not = vegetable_processing_cost,
                                                           n = n_year,
                                                           CV_if = 10,
                                                           CV_if_not = CV_value)
  
  
  # annual vegetable processing cost after considering financial risk
  # we consider if farmer get loan from bank
  final_vegetable_processing_cost <- vv(vegetable_processing_cost_with_bank_loan, 
                                        n_year, var_CV=CV_value,
                                        relative_trend = inflation_rate)
  
  
  #### Eco-tourism for vegetable field cost ####
  
  # First, we need to calculate the establishment cost for eco-tourism
  # by preparing some signs to be installed in the area
  # the cost would be paid only for the first year
  
  first_year_vegetable_ecotourism_cost <- sign_installation_cost
  
  # Then, we calculate the annual cost of eco-tourism for rice field
  # Which is included for marketing and promotion
  annual_vegetable_ecotourism_cost <- marketing_and_promotion_ecotourism
  
  vegetable_ecotourism_cost <- vv(annual_vegetable_ecotourism_cost, n_year, var_CV=5)
  
  # Calculate the cost with the first year establishment cost
  vegetable_ecotourism_cost[1] <- + vegetable_ecotourism_cost[1] + first_year_vegetable_ecotourism_cost
  
  
  ## Vegetables benefits ####
  
  ### Vegetable farming benefits ####
  
  # Annual vegetable farming income under normal condition
  # Assuming they grow 6 different vegetables per year, spring onion is always 
  # cultivated in every season and always mixed with main crops
  
  # calculating income from each crop considering the cultivated field size
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
                               n_year, var_CV=CV_value) 
  
  # calculating vegetable farming income per ha per year under normal condition
  vegetables_farming_revenue_precal <- chinese_mustard_green_income + green_bean_income +
    cabbage_income + chili_income + spring_onion_income
  
  vegetables_farming_revenue <- vv(vegetables_farming_revenue_precal, n_year, var_CV=CV_value,
                                   relative_trend = inflation_rate)
  
  
  ## chance event on vegetable farming benefits ##
  
  # considering production, financial, and market risk damage on vegetable yield
  
  # considering production risk damage on vegetable yield
  # assuming there are some risks like pest, disease, heavy rainfall, storm, 
  # and water shortage causing some lose to yield and reduction to vegetable
  # capital/cost may also reducing yield
  
  # event-1 vegetable yield reduction from production and financial risks
  vegetable_yield_loss_with_production_financial_risk <- total_vegetables_yield * (prob_damage_production_risk_vegetables+prob_damage_market_risk_vegetables)
  vegetable_yield_with_production_financial_risk_precal <- total_vegetables_yield - (vegetable_yield_loss_with_production_financial_risk)
  
  vegetable_yield_with_production_financial_risk <- chance_event((chance_production_risk +
                                                                    chance_financial_risk_vegetables),
                                                                 value_if = vegetable_yield_with_production_financial_risk_precal,
                                                                 value_if_not = total_vegetables_yield,
                                                                 n = n_year,
                                                                 CV_if = 10,
                                                                 CV_if_not = CV_value)
  
  
  # considering market risk damage on vegetable price
  # causing reduction to farmers' revenue
  
  # event-2 vegetable revenue reduction from market risk under normal condition
  vegetable_price_with_market_risk <- vegetable_prices * prob_damage_market_risk_vegetables
  vegetable_farming_revenue_loss_with_market_risk_normal <- vegetable_price_with_market_risk * total_vegetables_yield
  
  vegetable_farming_revenue_with_market_risk_normal <- chance_event(chance_market_risk_vegetables,
                                                                    value_if = vegetable_farming_revenue_loss_with_market_risk_normal,
                                                                    value_if_not = vegetables_farming_revenue,
                                                                    n = n_year,
                                                                    CV_if = 10,
                                                                    CV_if_not = CV_value)
  
  
  # event-3 vegetable revenue reduction from market risk under production and financial risk
  vegetable_farming_revenue_loss_with_market_production_financial_risk <- vegetable_price_with_market_risk * vegetable_yield_with_production_financial_risk
  
  vegetable_farming_revenue_with_market_production_financial_risk <- chance_event(chance_market_risk_rice,
                                                                                  value_if = vegetable_farming_revenue_loss_with_market_production_financial_risk,
                                                                                  value_if_not = vegetables_farming_revenue,
                                                                                  n = n_year,
                                                                                  CV_if = 10,
                                                                                  CV_if_not = CV_value)
  
  
  # annual vegetables farming revenue after considering risk
  final_vegetable_farming_revenue <- vv(vegetable_farming_revenue_with_market_production_financial_risk, n_year, var_CV=CV_value,
                                        relative_trend = inflation_rate)
  
  
  ### Vegetable compost benefits ####
  
  # Assuming that 50% of biomass resulting in compost
  vegetable_compost <- biomass_vegetables * 0.5
  
  # Annual income for composting vegetables biomass residue
  vegetables_compost_revenue_precal <- vegetable_compost * price_vegetable_compost
  vegetables_compost_revenue <- vv(vegetables_compost_revenue_precal, n_year, var_CV=10)
  
  ## Chance event options on vegetable compost benefits ##
  
  # Considering market risk damage on composting vegetables
  # farmers may not be able to sell their compost due to low market demand
  # this can reduce the revenue from vegetable compost
  
  vegetable_compost_revenue_loss_market_risk <- vegetables_compost_revenue - 
    (vegetables_compost_revenue * portion_compost_loss_revenue)
  
  vegetable_compost_revenue_with_market_risk <- chance_event(chance_market_risk_compost,
                                                             value_if = vegetable_compost_revenue_loss_market_risk,
                                                             value_if_not = vegetables_compost_revenue,
                                                             n = n_year,
                                                             CV_if = 10,
                                                             CV_if_not = CV_value)
  
  # annual vegetable compost revenue after considering risk
  final_vegetable_compost_revenue <- vv(vegetable_compost_revenue_with_market_risk, n_year, var_CV=CV_value,
                                        relative_trend = inflation_rate) 
  
  
  
  ### Processed vegetable production benefits ####
  
  # Assuming that around 10 to 30% of yield are low grade
  # The lower grade can be processed as sauces, paste, etc.
  # We only consider tomato and chili that can be processed as processed food
  
  # calculate tomato raw material
  tomato_raw_material_precal <- tomato_yield_per_year * low_grade_vegetable_product
  tomato_raw_material <- vv(tomato_raw_material_precal, n_year, var_CV=CV_value)
  
  # calculate chili raw material
  chili_raw_material_precal <- chili_yield_per_year * low_grade_vegetable_product
  chili_raw_material <- vv(chili_raw_material_precal, n_year, var_CV=CV_value)
  
  # revenue from the processed tomato sauce/paste product
  # assuming that the result is 50% from the raw material
  tomato_sauce_product <- tomato_raw_material * 0.5 # 50%
  tomato_sauce_product_revenue_precal <- tomato_sauce_product * tomato_sauce_price
  tomato_sauce_product_revenue <- vv(tomato_sauce_product_revenue_precal, n_year,
                                     var_CV=10, relative_trend = inflation_rate)
  
  # revenue from the processed chili sauce/paste product
  # assuming that the result is 50% from the raw material
  chili_sauce_product <- chili_raw_material * 0.5 # 50%
  chili_sauce_product_revenue_precal <- chili_sauce_product * chili_sauce_price
  chili_sauce_product_revenue <- vv(chili_sauce_product_revenue_precal, n_year,
                                    var_CV=10, relative_trend = inflation_rate)
  
  # sum up the revenue from both products
  processed_vegetable_product_revenue <- tomato_sauce_product_revenue +
    chili_sauce_product_revenue
  
  
  # chance event options ###
  
  # Considering market risk damage on processed vegetable product
  # farmers may not be able to sell their product due to low market demand
  # this can reduce the revenue from vegetable processed product
  
  vegetable_processed_revenue_loss_market_risk <- processed_vegetable_product_revenue - 
    (processed_vegetable_product_revenue * vegetable_processed_income_loss_market_risk)
  
  vegetable_processed_revenue_with_market_risk <- chance_event(chance_market_risk_vegetable_processed,
                                                               value_if = vegetable_processed_revenue_loss_market_risk,
                                                               value_if_not = processed_vegetable_product_revenue,
                                                               n = n_year,
                                                               CV_if = 10,
                                                               CV_if_not = CV_value)
  
  # annual vegetable processed product revenue after considering risk
  final_vegetable_processed_revenue <- vv(vegetable_processed_revenue_with_market_risk, n_year, var_CV=CV_value,
                                          relative_trend = inflation_rate) 
  
  
  ### Eco-tourism for vegetable field benefits ####
  
  # Annual income from vegetable field for eco-tourism
  vegetables_ecotourism_revenue_precal <- vegetable_ecotourism_value
  vegetables_ecotourism_revenue <- vv(vegetables_ecotourism_revenue_precal, n_year, var_CV=10)
  
  
  ### Health outcomes from vegetable field benefits ####
  
  # We can assume that the benefit by consuming vegetables can reduce hospital
  # or medication bills
  
  # annual health saving costs from consuming vegetables
  vegetables_nutrition_health_benefit <- vv(health_cost_savings_from_vegetables,
                                            n_year, var_CV=CV_value,
                                            relative_trend = inflation_rate)
  
  
  ## Vegetables system outcomes #### 
  
  ### Calculate income of each option after considering risks
  
  # Vegetables farming income
  vegetable_farming_income <- final_vegetable_farming_revenue - final_vegetable_farming_cost
  
  # Vegetables compost income
  vegetable_compost_income <- final_vegetable_compost_revenue - final_vegetable_compost_cost
  
  # Vegetables processed production income
  vegetable_processed_income <- final_vegetable_processed_revenue - final_vegetable_processing_cost
  
  # Vegetables eco-tourism income 
  vegetable_ecotourism_income <- vegetables_ecotourism_revenue - vegetable_ecotourism_cost
  
  # Vegetables health income 
  vegetable_health_benefit <- vegetables_nutrition_health_benefit
  
  
  #### Final result for vegetables ##
  
  final_vegetables_benefits_precal <- vegetable_farming_income + vegetable_compost_income +
    vegetable_processed_income + vegetable_ecotourism_income
  
  vegetable_result <- vv(final_vegetables_benefits_precal, n_year, var_CV=CV_value, 
                         relative_trend = inflation_rate)
  
  
  # Calculate NPV ####
  
  ## NPV all options
  
  NPV_rice <- discount(x = rice_result, 
                       discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_vegetables <- discount(x = vegetable_result, 
                             discount_rate = discount_rate,
                             calculate_NPV = TRUE)
  ## NPV farming
  
  NPV_rice_farming <- discount(x = rice_farming_income, 
                               discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_vegetables_farming <- discount(x = vegetable_farming_income, 
                                     discount_rate = discount_rate,
                                     calculate_NPV = TRUE)
  
  ## NPV compost
  
  NPV_rice_compost <- discount(x = rice_compost_income, 
                               discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_vegetables_compost <- discount(x = vegetable_compost_income, 
                                     discount_rate = discount_rate,
                                     calculate_NPV = TRUE)
  
  ## NPV processed product
  
  NPV_rice_processed <- discount(x = rice_processed_income, 
                                 discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_vegetables_processed <- discount(x = vegetable_processed_income, 
                                       discount_rate = discount_rate,
                                       calculate_NPV = TRUE)
  
  ## NPV ecotourism
  
  NPV_rice_ecotourism <- discount(x = rice_ecotourism_income, 
                                  discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_vegetables_ecotourism <- discount(x = vegetable_ecotourism_income, 
                                        discount_rate = discount_rate,
                                        calculate_NPV = TRUE)
  
  ## NPV health
  
  NPV_rice_health <- discount(x = rice_nutrition_health_benefit, 
                              discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_vegetables_health <- discount(x = vegetables_nutrition_health_benefit, 
                                    discount_rate = discount_rate,
                                    calculate_NPV = TRUE)
  
  
  
  return(list(NPV_rice = NPV_rice,
              NPV_vegetables = NPV_vegetables,
              NPV_tradeoff = NPV_vegetables - NPV_rice,
              NPV_rice_farming = NPV_rice_farming,
              NPV_vegetables_farming = NPV_vegetables_farming,
              NPV_rice_compost = NPV_rice_compost,
              NPV_vegetables_compost = NPV_vegetables_compost,
              NPV_rice_processed = NPV_rice_processed,
              NPV_vegetables_processed = NPV_vegetables_processed,
              NPV_rice_ecotourism = NPV_rice_ecotourism,
              NPV_vegetables_ecotourism = NPV_vegetables_ecotourism,
              NPV_rice_health = NPV_rice_health,
              NPV_vegetables_health = NPV_vegetables_health,
              cashflow_rice = rice_result,
              cashflow_vegetables = vegetable_result))
}


# Monte Carlo Simulation ####

# Run the Monte Carlo simulation using the model function #
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


# NPV distribution #####

# plot NPV distribution for rice and vegetables
NPV_rice_vegetables <- plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                          vars = c("NPV_rice",
                                                   "NPV_vegetables",
                                                   "NPV_tradeoff"),
                                          colors = c("skyblue", "seagreen1", "yellow"),
                                          y_axis_name = "",
                                          method = 'hist_simple_overlay', bins = 500, binwidth = 1e4)+
  labs(title = "NPV rice and vegetable systems")


# plot NPV distribution for rice and vegetables farming
NPV_farming_system <- plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                         vars = c("NPV_rice_farming",
                                                  "NPV_vegetables_farming"),
                                         colors = c("skyblue", "seagreen1"),
                                         y_axis_name = "",
                                         method = 'hist_simple_overlay')+
  labs(title = "NPV farming systems")

# plot NPV distribution for rice and vegetables compost
NPV_compost <- plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                  vars = c("NPV_rice_compost",
                                           "NPV_vegetables_compost"),
                                  colors = c("skyblue", "seagreen2"),
                                  y_axis_name = "",
                                  method = 'hist_simple_overlay')+
  labs(title = "NPV compost")

# plot NPV distribution for rice and vegetables processed product
NPV_processed_product <- plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                            vars = c("NPV_rice_processed",
                                                     "NPV_vegetables_processed"),
                                            colors = c("skyblue", "seagreen1"),
                                            y_axis_name = "",
                                            method = 'hist_simple_overlay', bins = 300, binwidth = 1e3)+
  labs(title = "NPV Processed Product")


# plot NPV distribution for rice and vegetables ecotourism
NPV_ecotourism <- plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                     vars = c("NPV_rice_ecotourism",
                                              "NPV_vegetables_ecotourism"),
                                     colors = c("skyblue", "seagreen1"),
                                     y_axis_name = "",
                                     method = 'hist_simple_overlay')+
  labs(title = "NPV Ecotourism")


# plot NPV distribution for rice and vegetables health benefit
NPV_health <- plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                 vars = c("NPV_rice_health",
                                          "NPV_vegetables_health"),
                                 colors = c("skyblue", "seagreen1"),
                                 y_axis_name = "",
                                 method = 'hist_simple_overlay')+
  labs(title = "NPV Health")


# Compund NPV figures
compound_figures_NPV <- (NPV_rice_vegetables | NPV_farming_system) / 
  (NPV_compost | NPV_processed_product) / (NPV_ecotourism | NPV_health)


# Cashflow analysis ####

# Plot cashflow rice
cashflow_rice <- plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_rice",
                               x_axis_name = "Years with intervention",
                               y_axis_name = "Annual cashflow in USD",
                               color_25_75 = "skyblue", color_5_95 = "wheat",
                               color_median = "red")

# Plot cashflow vegetables
cashflow_vegetables <- plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_vegetables",
                                     x_axis_name = "Years with intervention",
                                     y_axis_name = "Annual cashflow in USD",
                                     color_25_75 = "seagreen2", color_5_95 = "wheat",
                                     color_median = "red")

# Compound cashflow figures
compound_figures_cashflow <- (cashflow_rice | cashflow_vegetables) 


# Projection to Latent Structures (PLS) analysis ####

# Plot PLS rice
pls_result_rice <- plsr.mcSimulation(object = TRV_mc_simulation,
                                     resultName = names(TRV_mc_simulation$y)[1], ncomp = 1)
plot_pls_rice <- plot_pls(pls_result_rice, threshold = 0.5, base_size = 10,
                          pos_color = "skyblue", neg_color = "firebrick3")+
  labs(title = "PLS Rice", size = 8)

# Plot PLS vegetables
pls_result_vegetables <- plsr.mcSimulation(object = TRV_mc_simulation,
                                           resultName = names(TRV_mc_simulation$y)[2], ncomp = 1)
plot_pls_vegetables <- plot_pls(pls_result_vegetables, threshold = 0.5, base_size = 10,
                                pos_color = "seagreen2", neg_color = "firebrick3")+
  labs(title = "PLS Vegetables", size = 8)

# Compund PLS figures
compound_figures_PLS <- (plot_pls_rice | plot_pls_vegetables) 


# VoI analysis ####
mcSimulation_table <- data.frame(TRV_mc_simulation$x, TRV_mc_simulation$y[1:3])
evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_rice")

# Plot EVPI
plot_evpi_rice <- plot_evpi(evpi_TRV, decision_vars = "NPV_rice")
plot_evpi_vegetables <- plot_evpi(evpi_TRV, decision_vars = "NPV_vegetables")

# Compound EVPI figures
compound_figures_evpi <- (plot_evpi_rice | plot_evpi_vegetables)




# Compound figure ####
compound_figure_rice <- compound_figure(mcSimulation_object = TRV_mc_simulation, 
                                        input_table = input_estimates, plsrResults = pls_result_rice, 
                                        EVPIresults = evpi_TRV, decision_var_name = "NPV_rice", 
                                        cashflow_var_name = "cashflow_rice", 
                                        base_size = 8)

compound_figure_vegetables <- compound_figure(mcSimulation_object = TRV_mc_simulation, 
                                              input_table = input_estimates, plsrResults = pls_result_vegetables, 
                                              EVPIresults = evpi_TRV, decision_var_name = "NPV_vegetables", 
                                              cashflow_var_name = "cashflow_vegetables", 
                                              base_size = 8)
