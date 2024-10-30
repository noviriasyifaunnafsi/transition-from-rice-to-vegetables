library(decisionSupport)
library(gtExtras)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(svglite)

input_estimates <- read.csv("input_rice_to_vegs_usd.csv", sep = ";")

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_estimates))

transition_rice_to_vegetables <- function(x, varnames){
  
# 1st scenario: assuming farmer do everything by their own (?) ####
  
# Rice system ####
  
  # Considering rice production as baseline #
  
   ## Rice farming costs ####
  
  # Annual rice farming cost under normal condition
  # Assuming rice is cultivated twice per year
  rice_farming_cost_precal <- rice_farming_input_costs + rice_machinery_costs + 
    rice_labor_costs + irrigation_maintenance_costs  
  
  rice_farming_cost <- vv(rice_farming_cost_precal, n_year, var_CV=10, 
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
                                                                CV_if = CV_value,
                                                                CV_if_not = 10)
  
  
  
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
                                                    CV_if = CV_value,
                                                    CV_if_not = 10)
  
  
  # event-3, farmers get loan from bank, so they need to pay annual interest
  # this can be add up to their farming cost
  # consider if the farming input cost is higher because farmers need to buy 
  # more pesticides to overcome pest and disease outbreak
  
  rice_farming_high_input_cost_with_bank_loan_precal <- rice_farming_input_costs_with_more_pesticides +
    rice_machinery_costs + rice_labor_costs + irrigation_maintenance_costs 
  
  rice_farming_high_input_cost_with_bank_loan <- chance_event(chance_production_risk,
                                                   value_if = rice_farming_high_input_cost_with_bank_loan_precal,
                                                   value_if_not = rice_farming_cost,
                                                   n = n_year,
                                                   CV_if = CV_value,
                                                   CV_if_not = 10)
  
  
  
  # annual rice farming cost after considering production and financial risk
  rice_farming_cost_with_risk_precal <- rice_farming_high_input_cost_with_bank_loan
  
  final_rice_farming_cost <- vv(rice_farming_cost_with_risk_precal, n_year, var_CV=CV_value,
                        relative_trend = inflation_rate)
  
  
  ## Rice benefits ####
  
  #### Rice farming ####
  
  # Annual rice farming income under normal condition
  # Assuming rice is cultivated twice per year
  rice_farming_revenue_precal <- rice_yield * rice_farmgate_price
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
                                                   CV_if = CV_value,
                                                   CV_if_not = 10)
  
  
  # considering market risk damage on rice price
  # causing reduction to farmers' revenue
  
  # event-2 rice revenue reduction from market risk under normal condition (without natural risks)
  rice_farmgate_price_with_market_risk <- rice_farmgate_price * prob_damage_market_risk_rice
  rice_farming_revenue_loss_with_market_risk_normal <- rice_farmgate_price_with_market_risk * rice_yield
  
  rice_farming_revenue_with_market_risk_normal <- chance_event(chance_market_risk_rice,
                                                              value_if = rice_farming_revenue_loss_with_market_risk_normal,
                                                            value_if_not = rice_farming_revenue_precal,
                                                            n = n_year,
                                                            CV_if = CV_value,
                                                            CV_if_not = 10)
  
  
  # event-3 rice revenue reduction from market risk under production and financial risk
  rice_farming_revenue_loss_with_market_production_financial_risk <- rice_farmgate_price_with_market_risk * rice_yield_with_production_financial_risk_precal
  
  
  # annual rice farming revenue after considering risk
  final_rice_farming_revenue <- vv(rice_farming_revenue_loss_with_market_production_financial_risk, n_year, var_CV=CV_value,
                               relative_trend = inflation_rate) 
  
  
  #### Rice cultural value ####
  
  # here we also add cultural value of growing rice as a benefit
  # we consider farmers keep growing rice due to cultural and social reasons
  # in which that they think producing their own rice would give them 'pride'
  # and make them feel secure for having their own staple food
  # the cultural value is quantified by considering the differences between
  # market price of milled rice and farm-gate price of rice
  
  rice_market_price_vv <- vv(rice_market_price, n=n_year, var_CV=CV_value)
  
  rice_cultural_value <- (rice_yield * rice_market_price_vv) - rice_farming_cost
  # from cory's mom's tomato example
  
  
  
  
  # Vegetable system ####
  
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
                                                                CV_if = CV_value,
                                                                CV_if_not = 10)
  
  
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
                                                   CV_if = CV_value,
                                                   CV_if_not = 10)
  
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
                                                              CV_if = CV_value,
                                                              CV_if_not = 10)
  
  
  
  # Annual vegetable farming cost after considering production risk
  
  # Typically, field sizes in Sinjai are small, and it's very rare to find a 
  # 1 ha plot. Therefore, let's assume that a farmer has two different fields, 
  # each with a size of 0.5 ha, making a total of 1 ha. The farmer can 
  # grow 6 different vegetables per year. 
  
  # In this model, we consider that 'Field 1' is cultivated with Chinese mustard 
  # green, green beans, and cabbage in rotation as main crops, with spring onions 
  # as a side crop. In 'Field 2', tomatoes and chili are cultivated in rotation as 
  # main crops, with spring onions as a side crop.
  
  # For Field 1, the proportion of the field used for the main crops is 0.3 ha, 
  # while the side crop uses 0.2 ha per season (0.6 ha per year) --> 1.5 ha in total
  # For Field 2, the proportion of the field used for the main crops is 0.4 ha, 
  # while the side crop uses 0.1 ha per season (0.2 ha per year) --> 1 ha in total
  # Therefore, the total effective field size per year is 2.5 ha
  
  # Since the unit cost is for 1 ha per year, then the annual total cost should be calculated
  # based on the total effective field size per year
  
  annual_final_vegetable_farming_cost <- vegetable_farming_high_input_cost_with_bank_loan * 2.5
  
  final_vegetable_farming_cost <- vv(annual_final_vegetable_farming_cost, n_year, var_CV=CV_value,
                             relative_trend = inflation_rate)
  
  final_vegetable_farming_cost[1] <- final_vegetable_farming_cost[1] + 
    vegetables_equipment_costs_first_year + 
    vegetables_irrigation_costs_first_year
  

  ### Vegetable farming benefits ####
  
  # Annual vegetable farming revenue under normal condition
  
  # Typically, field sizes in Sinjai are small, and it's very rare to find a 
  # 1 ha plot. Therefore, let's assume that a farmer has two different fields, 
  # each with a size of 0.5 ha, making a total of 1 ha. The farmer can 
  # grow 6 different vegetables per year. 
  
  # In this model, we consider that 'Field 1' is cultivated with Chinese mustard 
  # green, green beans, and cabbage in rotation as main crops, with spring onions 
  # as a side crop. In 'Field 2', tomatoes and chili are cultivated in rotation as 
  # main crops, with spring onions as a side crop.
  
  # Calculating revenue from 'Field 1'
  # For Field 1, the proportion of the field used for the main crops is 0.3 ha, 
  # while the side crop uses 0.2 ha per season (0.6 ha per year)
  
  chinese_mustard_green_revenue_precal <- (chinese_mustard_green_yield * chinese_mustard_green_price) * 0.3 
  chinese_mustard_green_revenue <- vv(chinese_mustard_green_revenue_precal, n_year, var_CV=40)
  
  green_bean_revenue_precal <- (green_bean_yield * green_bean_price) *  0.3 
  green_bean_revenue <- vv(green_bean_revenue_precal, n_year, var_CV=40)
  
  cabbage_revenue_precal <- (cabbage_yield * cabbage_price) * 0.3 
  cabbage_revenue <- vv(cabbage_revenue_precal, n_year, var_CV=40)
  
  spring_onion_revenue_precal_1 <- (spring_onion_yield * spring_onion_price) * 0.6 
  spring_onion_revenue_1 <- vv(spring_onion_revenue_precal_1, n_year, var_CV=40)
  
  # total revenue from 'Field 1'
  total_revenue_field_1 <- chinese_mustard_green_revenue + green_bean_revenue +
    cabbage_revenue + spring_onion_revenue_1
  
  
  # Calculating revenue from 'Field 2'
  # For Field 2, the proportion of the field used for the main crops is 0.4 ha, 
  # while the side crop uses 0.1 ha per season (0.2 ha per year)
  
  chili_revenue_precal <- (chili_yield * chili_price) * 0.4 
  chili_revenue <- vv(chili_revenue_precal, n_year, var_CV=40)
  
  tomato_revenue_precal <- (tomato_yield * tomato_price) * 0.4
  tomato_revenue <- vv(tomato_revenue_precal, n_year, var_CV=40)
  
  spring_onion_revenue_precal_2 <- (spring_onion_yield * spring_onion_price) * 0.2 
  spring_onion_revenue_2 <- vv(spring_onion_revenue_precal_2, n_year, var_CV=40)
  
  # total revenue from 'Field 2'
  total_revenue_field_2 <- chili_revenue + tomato_revenue + spring_onion_revenue_2 
  
  # total revenue from all vegetable fields
  total_revenue_vegetables <- total_revenue_field_1 + total_revenue_field_2
  
  
  ## chance event on vegetable farming benefits ##
  
  # considering production, financial, and market risk damage on vegetable yield
  
  # considering production risk damage on vegetable yield
  # assuming there are some risks like pest, disease, heavy rainfall, storm, 
  # and water shortage causing some lose to yield and reduction to vegetable
  # capital/cost may also reducing yield
  
  # calculate the annual total yield of vegetables for production risks
  
  annual_chinese_mustard_green_yield <- vv((chinese_mustard_green_yield * 0.3), n_year, var_CV=CV_value)
  annual_green_bean_yield <- vv((green_bean_yield * 0.3), n_year, var_CV=CV_value)
  annual_cabbage_yield <- vv((cabbage_yield * 0.3), n_year, var_CV=CV_value)
  annual_chili_yield <- vv((chili_yield * 0.4), n_year, var_CV=CV_value)
  annual_tomato_yield <- vv((tomato_yield * 0.4), n_year, var_CV=CV_value)
  annual_spring_onion_yield <- vv((spring_onion_yield * 0.8), #total from field 1 and field 2
                                  n_year, var_CV=CV_value) 
  
  annual_total_yield_vegetables <- annual_chinese_mustard_green_yield + annual_green_bean_yield +
    annual_cabbage_yield + annual_chili_yield + annual_tomato_yield + annual_spring_onion_yield
  
  # event-1 vegetable yield reduction from production and financial risks
  vegetable_yield_loss_with_production_risk <- annual_total_yield_vegetables * prob_damage_production_risk_vegetables
  vegetable_yield_with_production_risk_precal <- annual_total_yield_vegetables - vegetable_yield_loss_with_production_risk
  
  vegetable_yield_with_production_risk <- chance_event(chance_production_risk,
                                                            value_if = vegetable_yield_with_production_risk_precal,
                                                            value_if_not = annual_total_yield_vegetables,
                                                            n = n_year,
                                                            CV_if = CV_value,
                                                            CV_if_not = 10)
  
  vegetable_yield_loss_with_financial_risk <- annual_total_yield_vegetables * prob_damage_financial_risk_vegetables
  vegetable_yield_with_financial_risk_precal <- annual_total_yield_vegetables - vegetable_yield_loss_with_financial_risk
  
  vegetable_yield_with_financial_risk <- chance_event(chance_financial_risk_vegetables,
                                                                 value_if = vegetable_yield_with_financial_risk_precal,
                                                                 value_if_not = annual_total_yield_vegetables,
                                                                 n = n_year,
                                                                 CV_if = CV_value,
                                                                 CV_if_not = 10)
  
  vegetable_yield_loss_with_production_and_financial_risk <- annual_total_yield_vegetables * (prob_damage_production_risk_vegetables + 
                                                                                           prob_damage_financial_risk_vegetables)
  vegetable_yield_with_production_and_financial_risk_precal <- annual_total_yield_vegetables - vegetable_yield_loss_with_production_and_financial_risk
  
  vegetable_yield_with_production_and_financial_risk <- chance_event((chance_production_risk + chance_financial_risk_vegetables),
                                                                     value_if = vegetable_yield_with_production_and_financial_risk_precal,
                                                                     value_if_not = annual_total_yield_vegetables,
                                                                     n = n_year,
                                                                     CV_if = CV_value,
                                                                     CV_if_not = 10)
  
  
  # considering market risk damage on vegetable price
  # causing reduction to farmers' revenue
  
  # event-2 vegetable revenue reduction from market risk under normal condition
  vegetable_price_with_market_risk <- vv((vegetable_prices * prob_damage_market_risk_vegetables), n_year, var_CV=CV_value)
  vegetable_farming_revenue_with_market_risk_normal <- vegetable_price_with_market_risk * annual_total_yield_vegetables
  
  vegetable_farming_revenue_with_market_risk_normal <- chance_event(chance_market_risk_vegetables,
                                                               value_if = vegetable_farming_revenue_with_market_risk_normal,
                                                               value_if_not = total_revenue_vegetables,
                                                               n = n_year,
                                                               CV_if = CV_value,
                                                               CV_if_not = 10)
  
  
  
  # event-3 vegetable revenue reduction from market risk under production and financial risk
  vegetable_farming_revenue_loss_with_market_production_financial_risk <- vegetable_price_with_market_risk * vegetable_yield_with_production_and_financial_risk
  vegetable_farming_revenue_with_market_production_financial_risk_precal <- total_revenue_vegetables - vegetable_farming_revenue_loss_with_market_production_financial_risk
  
  vegetable_farming_revenue_with_market_production_financial_risk <- chance_event(chance_market_risk_rice,
                                                                             value_if = vegetable_farming_revenue_with_market_production_financial_risk_precal,
                                                                             value_if_not = total_revenue_vegetables,
                                                                             n = n_year,
                                                                             CV_if = CV_value,
                                                                             CV_if_not = 10)
  
  
  # annual vegetables farming revenue after considering risk
  final_vegetable_farming_revenue <- vv(vegetable_farming_revenue_with_market_production_financial_risk_precal, n_year, var_CV=CV_value,
                                   relative_trend = inflation_rate)
     
  
  ### Health benefits from vegetables ####
  
  # We can assume that the benefit by growing vegetables
  # people can consume vegetables easily and cheaper
  # which can be associated to improving nutrition
  
  # annual savings for purchasing vegetables 
  
  annual_spending_for_vegetables <- vv((average_vegetables_price_market * annual_vegs_consumption_per_person), 
                                       n_year, var_CV=CV_value, relative_trend = inflation_rate)
  
  annual_savings_for_purchasing_vegetables <- annual_spending_for_vegetables - (final_vegetable_farming_revenue - final_vegetable_farming_cost)
  
  # chance of farmers can do savings from growing vegetable
  chance_savings_from_vegetables <- chance_event(chance_consume_own_vegetables,
                                                 value_if = annual_savings_for_purchasing_vegetables,
                                                 value_if_not = annual_spending_for_vegetables,
                                                 n = n_year,
                                                 CV_if = CV_value,
                                                 CV_if_not = 10)
  
  vegetables_nutrition_health_benefit <- vv(chance_savings_from_vegetables,
                                            n_year, var_CV=CV_value,
                                            relative_trend = inflation_rate)

  
  
  
  # Chance event ####
  
  ## Vegetable processing ####
  
  # Probability of vegetable processing 
  
  chance_vegetable_processing <- chance_event(chance = prob_of_processing_vegetables, value_if = 1, 
                                     value_if_not = 0) 
  if(chance_vegetable_processing==1){
    
    #### Cost of vegetable processing ####
    
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
                                                             CV_if = CV_value,
                                                             CV_if_not = 10)
    
    
    # annual vegetable processing cost after considering financial risk
    # we consider if farmer get loan from bank
    final_vegetable_processing_cost <- vv(vegetable_processing_cost_with_bank_loan, 
                                          n_year, var_CV=CV_value,
                                          relative_trend = inflation_rate)
    
    ## Benefit of vegetable processing ####
    
    # Assuming that around 1 to 10% of yield are low grade
    # The lower grade can be processed as sauces, paste, etc.
    # We only consider tomato and chili that can be processed as processed food
    
    # calculate tomato raw material
    tomato_raw_material <- vv(annual_tomato_yield * low_grade_vegetable_product, 
                              n_year, var_CV=40) #CV is quite higher considering 
                                                  # that not all farmers would do processing
    
    # calculate chili raw material
    chili_raw_material <- vv(annual_chili_yield * low_grade_vegetable_product, 
                             n_year, var_CV=40) #CV is quite higher considering 
                                                  # that not all farmers would do processing
    
    # revenue from the processed tomato sauce/paste product
    # assuming that the result is 30% from the raw material
    tomato_sauce_product <- tomato_raw_material * 0.3 # 30%
    tomato_sauce_product_revenue <- vv(tomato_sauce_product * tomato_sauce_price, n_year,
                                       var_CV=10, relative_trend = inflation_rate)
    
    # revenue from the processed chili sauce/paste product
    # assuming that the result is 30% from the raw material
    chili_sauce_product <- chili_raw_material * 0.3 # 30%
    chili_sauce_product_revenue <- vv(chili_sauce_product * chili_sauce_price, n_year,
                                      var_CV=10, relative_trend = inflation_rate)
    
    # sum up the revenue from both products
    processed_vegetable_product_revenue <- tomato_sauce_product_revenue +
      chili_sauce_product_revenue
    
    
    # chance event options ###
    
    # Considering market risk damage on processed vegetable product
    # farmers may not be able to sell their product due to low market demand
    # this can reduce the revenue from vegetable processed product
    
    vegetable_processed_revenue_loss_market_risk <- (processed_vegetable_product_revenue - 
      (processed_vegetable_product_revenue * vegetable_processed_income_loss_market_risk)) 
   
    vegetable_processed_revenue_with_market_risk <- chance_event(chance_market_risk_vegetable_processed,
                                                                 value_if = vegetable_processed_revenue_loss_market_risk,
                                                                 value_if_not = processed_vegetable_product_revenue,
                                                                 n = n_year,
                                                                 CV_if = CV_value,
                                                                 CV_if_not = 10)

    # annual vegetable processed product revenue after considering risk
    final_vegetable_processed_revenue <- vv(vegetable_processed_revenue_loss_market_risk, n_year, var_CV=CV_value,
                                            relative_trend = inflation_rate) 
    
    
    
  }else{
    final_vegetable_processing_cost <- rep(x = 0, times=n_year)
    final_vegetable_processed_revenue <- rep(x = 0, times=n_year)
  }
  
  
  ## Compost ####
  
  # Probability of compost 
  
  chance_compots <- chance_event(chance = prob_of_compost, value_if = 1, 
                                     value_if_not = 0) 
  if(chance_compots==1){
    
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
    # The rest work would be handle by farmers themselves
    
    annual_cost_labor_rice_composting <- daily_labor_cost * 10 #10 days for hiring labor
    
    # Then, we calculate the annual rice biomass composting cost
    # Which is included for the cost of labor, compost activator, and maintenance
    annual_rice_compost_cost <- annual_cost_labor_rice_composting +
      compost_activator_cost + composter_maintenance_cost + manure_for_compost_cost
    
    rice_compost_cost <- vv(annual_rice_compost_cost, n_year, var_CV=10)
    
    # Calculate the cost with the first year establishment cost
    rice_compost_cost[1] <- rice_compost_cost[1] + first_year_composting_cost 
    
    
    ## Chance event options on rice compost cost ##
    
    # Considering financial risk (lack of capital) damage on composting rice
    # farmers may not have enough capital to purchase composter tools and equipment
    # So they might get loan from bank with micro-credit program (KUR)
    # we can consider that farmer who own 1 ha of field can get loan from IDR 10 mio to
    # 50 mio (USD 625 to 3125)
    
    # event-1, farmers get loan from bank, so they need to pay annual interest
    # this can be add up to their composting cost
    
    rice_compost_get_bank_loan <- if(annual_rice_compost_cost == range_loan_for_farmers) {
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
                                                     CV_if = CV_value,
                                                     CV_if_not = 10)
    
    
    
    # annual rice compost cost after considering production and financial risk
    rice_compost_cost_if_get_bank_loan[1] <- rice_compost_cost_if_get_bank_loan[1] + first_year_composting_cost
    
    final_rice_compost_cost <- vv(rice_compost_cost_if_get_bank_loan, n_year, var_CV=CV_value,
                                  relative_trend = inflation_rate)
    
    
    ## Rice compost benefits ####
    
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
                                                          CV_if = CV_value,
                                                          CV_if_not = 10)
    
    # annual rice compost revenue after considering risk
    final_rice_compost_revenue <- vv(rice_compost_revenue_with_market_risk, n_year, var_CV=CV_value,
                                     relative_trend = inflation_rate) 
    
    
    
    ## Vegetable compost cost ####
    
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
    
    vegetable_compost_cost <- vv(vegetable_compost_cost_precal, n_year, var_CV=10)
    
    # Calculate the cost with the first year establishment cost
    vegetable_compost_cost[1] <- vegetable_compost_cost[1] + first_year_composting_cost
    
    
    ## Chance event option on vegetables compost cost ##
    
    # Considering financial risk (lack of capital) damage on composting vegetables
    # farmers may not have enough capital to purchase composter tools and equipment
    # So they might get loan from bank with micro-credit program (KUR)
    # we can consider that farmer who own 1 ha of field can get loan from IDR 10 mio to
    # 50 mio (USD 625 to 3125)
    
    # event-1, farmers get loan from bank, so they need to pay annual interest
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
                                                          CV_if = CV_value,
                                                          CV_if_not = 10)
    
    
    
    # annual vegetable compost cost after considering production and financial risk
    vegetable_compost_cost_if_get_bank_loan[1] <- vegetable_compost_cost_if_get_bank_loan[1] + first_year_composting_cost
    
    final_vegetable_compost_cost <- vv(vegetable_compost_cost_if_get_bank_loan, n_year, var_CV=CV_value,
                                       relative_trend = inflation_rate)
    
    
    ## Vegetable compost benefits ####
    
    # Assuming that 30% of biomass resulting in compost
    vegetable_compost <- biomass_vegetables * 0.3
    
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
                                                               CV_if = CV_value,
                                                               CV_if_not = 10)
    
    # annual vegetable compost revenue after considering risk
    final_vegetable_compost_revenue <- vv(vegetable_compost_revenue_with_market_risk, n_year, var_CV=CV_value,
                                          relative_trend = inflation_rate) 
    
    
  }else{
    final_rice_compost_cost <- rep(x = 0, times=n_year)
    final_rice_compost_revenue <- rep(x = 0, times=n_year)
    final_vegetable_compost_cost <- rep(x = 0, times=n_year)
    final_vegetable_compost_revenue <- rep(x = 0, times=n_year)
  }
  
  
  
  
  
  ## Agrotourism ####
  
  # Probability of agrotourism 
  
  chance_agrotourism <- chance_event(chance = prob_of_agrotourism, value_if = 1, 
                                    value_if_not = 0) 
  if(chance_agrotourism==1){
    
  ### Agrotourism for rice field cost ####
  
  # First, we need to calculate the establishment cost for agrotourism
  # by preparing some signs to be installed in the area
  # the cost would be paid only for the first year
  
  first_year_rice_agrotourism_cost <- sign_installation_cost
  
  # Then, we calculate the annual cost of agrotourism for rice field
  # Which is included for marketing and promotion
  annual_rice_agrotourism_cost <- marketing_and_promotion_ecotourism
  
  rice_agrotourism_cost <- vv(annual_rice_agrotourism_cost, n_year, var_CV=10)
  
  # Calculate the cost with the first year establishment cost
  rice_agrotourism_cost[1] <- rice_agrotourism_cost[1] + first_year_rice_agrotourism_cost 
  
  # We consider that there is no risk affect cost of rice agrotourism cost
  # since the cost might be not so high
  
  # Annual income from rice field agrotourism  (based on current condition)
  rice_agrotourism_revenue <- vv(rice_ecotourism_value, n_year, var_CV=CV_value)
  
  
    ## Agrotourism for vegetable field cost ####
    
    # First, we need to calculate the establishment cost for agrotourism
    # by preparing some signs to be installed in the area
    # the cost would be paid only for the first year
    
    first_year_vegetable_agrotourism_cost <- sign_installation_cost
    
    # Then, we calculate the annual cost of agrotourism for rice field
    # Which is included for marketing and promotion
    annual_vegetable_agrotourism_cost <- marketing_and_promotion_ecotourism
    
    vegetable_agrotourism_cost <- vv(annual_vegetable_agrotourism_cost, n_year, 
                                     var_CV=10)
    
    # Calculate the cost with the first year establishment cost
    vegetable_agrotourism_cost[1] <- + vegetable_agrotourism_cost[1] + 
      first_year_vegetable_agrotourism_cost
    
    # Annual income from vegetables field agrotourism  (based on current condition)
    vegetables_agrotourism_revenue <- vv(vegetable_ecotourism_value, n_year, 
                                         var_CV=10)
  }else{
    rice_agrotourism_cost <- rep(x = 0, times=n_year)
    rice_agrotourism_revenue <- rep(x = 0, times=n_year)
    vegetable_agrotourism_cost <- rep(x = 0, times=n_year)
    vegetables_agrotourism_revenue <- rep(x = 0, times=n_year)
  }
  
  
  ## Rice outcomes #### 
  
  ### Final rice benefits ####
  final_rice_benefits <- final_rice_farming_revenue + final_rice_compost_revenue +
    rice_agrotourism_revenue + rice_cultural_value
  
  ### Final rice costs ####
  final_rice_costs <- final_rice_farming_cost + final_rice_compost_cost +
    rice_agrotourism_cost
  
  ### Final result for rice ####
  final_rice_result <- final_rice_benefits - final_rice_costs
  
  
  ## For rice scenario options ####
  
  # Rice farming result
  rice_farming_result <- rice_farming_revenue - final_rice_farming_cost
  
  # Rice farming + compost result
  rice_farming_compost_benefit <- rice_farming_revenue + final_rice_compost_revenue
  rice_farming_compost_cost <- final_rice_farming_cost + final_rice_compost_cost
  rice_farming_compost_result <- rice_farming_compost_benefit - rice_farming_compost_cost
  
  # Rice farming + agrotourism result
  rice_farming_agrotourism_benefit <- rice_farming_revenue + rice_agrotourism_revenue
  rice_farming_agrotourism_cost <- final_rice_farming_cost + rice_agrotourism_cost
  rice_farming_agrotourism_result <- rice_farming_agrotourism_benefit - rice_farming_agrotourism_cost
  
  
  ## Vegetables outcomes #### 
  
  #### Final benefits for vegetables ####
  final_vegetable_benefits <- final_vegetable_farming_revenue + final_vegetable_compost_revenue +
    final_vegetable_processed_revenue + vegetables_agrotourism_revenue + vegetables_nutrition_health_benefit
  
  #### Final costs for vegetables ####
  final_vegetable_costs <- final_vegetable_farming_cost + final_vegetable_compost_cost +
    final_vegetable_processing_cost + vegetable_agrotourism_cost
  
  #### Final result for vegetables ####
  final_vegetable_result <- final_vegetable_benefits - final_vegetable_costs
  
  
  ## For vegetables scenario options ####
  
  # Vegetables farming result
  vegetable_farming_result <- final_vegetable_farming_revenue - final_vegetable_farming_cost
  
  # Vegetables farming + compost result
  vegetables_farming_compost_benefit <- final_vegetable_farming_revenue + final_vegetable_compost_revenue
  vegetables_farming_compost_cost <- final_vegetable_farming_cost + final_vegetable_compost_cost
  vegetables_farming_compost_result <- vegetables_farming_compost_benefit - vegetables_farming_compost_cost
  
  # Vegetables agrotourism result 
  vegetables_farming_agrotourism_benefit <- final_vegetable_farming_revenue + vegetables_agrotourism_revenue
  vegetables_farming_agrotourism_cost <- final_vegetable_farming_cost + vegetable_agrotourism_cost
  vegetables_farming_agrotourism_result <- vegetables_farming_agrotourism_benefit - vegetables_farming_agrotourism_cost
  
  
  
  
  # 2nd scenario: What if farmers engage in contract farming? ####
  
  # This is maybe more applicable only for vegetables.
  # Although there are also few cases for rice in some areas in Java, but 
  # I think I will just focus on vegetables.
  
  # If farmers engage in contract farming, the market risk can be reduced 
  # because they can get fixed price under the contract.
  # But then the range of market price will be so much narrower
  # So maybe the NPV could be also narrower (?)
  # hmmmm.. let's see...
  
  # let's define the chance event under contract farming scenario
  
  # How likely farmer will engage in contract farming?
  # Based on my experience, most likely farmer willing to engage in contract 
  # farming. However, the issue is that very few "contractors," such as private 
  # companies or the government, offer contract farming to farmers. 
  # But I think, this has great potential to help farmers with more secure and
  # stable income from growing vegetables.
  
  # But does it still make sense to make this scenario though we know that the
  # chance of contract farming is very low?
  # Hmm.. But I think that still worth to try..
  
  # Next time, I want to do the analysis for different POV, so the decision maker 
  # would be the 'contractor'. It will be something like, is it worth it for them 
  # to offer contract farming to farmers while also gain their own profit?
  # It's gonna be complicated maybe...
  # But seems interesting..
  # But don't know when I can do it...
  # AAAAAAAAAAAA
  
  # Let's just focus on ONE AND GET EVERYTHING DONE BEFORE DEADLINE!!!!!!!!
  # I JUST WANT TO GRADUATE AND DON'T GET EXPELLED JUST BECAUSE I CANT MEET THE DEADLINE!!!!
  
  # NOPIIII YOU CAN DO IT!!!!!!
  # GANBATTTEEEEEE!!!!!
  # SEMANGATTTTTTTTT!!!!!
  
  # Anyway...
  # Let's try...
  
  # But wait...
  # Then I need to add more variables in input table
  # Like the contract price for each vegs? :o
  # Hmmm....
  
  # Ok, I already added more variables in input table
  
  # So the calculation for this scenario is only for farming benefits because i 
  # think the the costs will remain the same
  # I also change the chance of market risk to be lower than the 1st scenario
  
  
  ### Vegetable farming benefits with contract farming ####
  
  # Annual vegetable farming revenue under normal condition
  
  # Typically, field sizes in Sinjai are small, and it's very rare to find a 
  # 1 ha plot. Therefore, let's assume that a farmer has two different fields, 
  # each with a size of 0.5 ha, making a total of 1 ha. The farmer can 
  # grow 6 different vegetables per year. 
  
  # In this model, we consider that 'Field 1' is cultivated with Chinese mustard 
  # green, green beans, and cabbage in rotation as main crops, with spring onions 
  # as a side crop. In 'Field 2', tomatoes and chili are cultivated in rotation as 
  # main crops, with spring onions as a side crop.
  
  # Calculating revenue from 'Field 1'
  # For Field 1, the proportion of the field used for the main crops is 0.3 ha, 
  # while the side crop uses 0.2 ha per season (0.6 ha per year)
  
  chinese_mustard_green_revenue_cont <- (chinese_mustard_green_yield * 
                                           chinese_mustard_green_contract_price) * 0.3 
  chinese_mustard_green_revenue_contract_farming <- vv(chinese_mustard_green_revenue_cont,
                                                       n_year, var_CV=40)
  
  green_bean_revenue_cont <- (green_bean_yield * green_bean_contract_price) * 0.3 
  green_bean_revenue_contract_farming <- vv(green_bean_revenue_cont, n_year, var_CV=40)
  
  cabbage_revenue_cont <- (cabbage_yield * cabbage_contract_price) * 0.3 
  cabbage_revenue_contract_farming <- vv(cabbage_revenue_cont, n_year, var_CV=40)
  
  spring_onion_revenue_cont_1 <- (spring_onion_yield * spring_onion_contract_price) * 0.6 
  spring_onion_revenue_contract_farming_1 <- vv(spring_onion_revenue_cont_1, n_year, var_CV=40)
  
  # total revenue from 'Field 1'
  total_revenue_field_contract_farming_1 <- chinese_mustard_green_revenue_contract_farming + 
                                            green_bean_revenue_contract_farming +
                                            cabbage_revenue_contract_farming + 
                                            spring_onion_revenue_contract_farming_1
  
  
  # Calculating revenue from 'Field 2'
  # For Field 2, the proportion of the field used for the main crops is 0.4 ha, 
  # while the side crop uses 0.1 ha per season (0.2 ha per year)
  
  chili_revenue_cont <- (chili_yield * chili_contract_price) * 0.4 
  chili_revenue_contract_farming <- vv(chili_revenue_cont, n_year, var_CV=40)
  
  tomato_revenue_cont <- (tomato_yield * tomato_contract_price) * 0.4
  tomato_revenue_contract_farming <- vv(tomato_revenue_cont, n_year, var_CV=40)
  
  spring_onion_revenue_cont_2 <- (spring_onion_yield * spring_onion_contract_price) * 0.2 
  spring_onion_revenue_contract_farming_2 <- vv(spring_onion_revenue_cont_2, n_year, var_CV=40)
  
  # total revenue from 'Field 2'
  total_revenue_field_contract_farming_2 <- chili_revenue_contract_farming + 
                                              tomato_revenue_contract_farming + 
                                              spring_onion_revenue_contract_farming_2 
  
  # total revenue from all vegetable fields
  total_revenue_vegetables_contract_farming <- total_revenue_field_contract_farming_1 + 
                                                total_revenue_field_contract_farming_2
  
  
  ## chance event on vegetable farming benefits ##
  
  # considering market risk damage on vegetable price under contract farming scenario
  # causing reduction to farmers' revenue
  
  # event-2 vegetable revenue reduction from market risk under normal condition
  vegetable_contract_price_with_market_risk <- vv((vegetable_contract_prices * prob_damage_market_risk_contract_farming_vegetables), n_year, var_CV=CV_value)
  vegetable_contract_farming_revenue_with_market_risk_normal <- vegetable_price_with_market_risk * annual_total_yield_vegetables
  
  vegetable_contract_farming_revenue_with_market_risk_normal <- chance_event(chance_market_risk_contract_farming_vegetables,
                                                                    value_if = vegetable_contract_farming_revenue_with_market_risk_normal,
                                                                    value_if_not = total_revenue_vegetables_contract_farming,
                                                                    n = n_year,
                                                                    CV_if = CV_value,
                                                                    CV_if_not = 10)
  
  
  
  # event-3 vegetable revenue reduction from market risk under production and financial risk
  vegetable_contract_farming_revenue_loss_with_market_production_financial_risk <- vegetable_contract_price_with_market_risk * vegetable_yield_with_production_and_financial_risk
  vegetable_contract_farming_revenue_with_market_production_financial_risk_precal <- total_revenue_vegetables_contract_farming - vegetable_contract_farming_revenue_loss_with_market_production_financial_risk
  
  vegetable_contract_farming_revenue_with_market_production_financial_risk <- chance_event(chance_market_risk_contract_farming_vegetables,
                                                                                  value_if = vegetable_contract_farming_revenue_with_market_production_financial_risk_precal,
                                                                                  value_if_not = total_revenue_vegetables_contract_farming,
                                                                                  n = n_year,
                                                                                  CV_if = CV_value,
                                                                                  CV_if_not = 10)
  
  
  # annual vegetables farming revenue after considering risk
  final_vegetable_contract_farming_revenue <- vv(vegetable_contract_farming_revenue_with_market_production_financial_risk, n_year, var_CV=CV_value,
                                        relative_trend = inflation_rate)
  
  
  ## Final result for vegetables contract farming ####
  
  #### Final benefits for vegetables contract farming####
  final_vegetable_contract_farming_benefits <- final_vegetable_contract_farming_revenue + final_vegetable_compost_revenue +
    final_vegetable_processed_revenue + vegetables_agrotourism_revenue + vegetables_nutrition_health_benefit
  
  
  #### Final costs for vegetables contract farming ####
  #### Assuming it is the same as the first scenario

  #### Final result for vegetables contract farming ####
  final_vegetable_contract_farming_result <- final_vegetable_contract_farming_benefits - final_vegetable_costs


  # Calculate NPV ####
  
  ## NPV for 1st scenario ####
  
  NPV_rice_all <- discount(x = final_rice_result,
                       discount_rate = discount_rate, calculate_NPV = TRUE)

  NPV_vegetables_all <- discount(x = final_vegetable_result,
                             discount_rate = discount_rate,
                             calculate_NPV = TRUE)
  
  NPV_vegetable_decision <- discount(x = (final_vegetable_result - final_rice_result), 
                                     discount_rate = discount_rate,
                                     calculate_NPV = TRUE)
  
  
  ## NPV for 2nd scenario ####
  
  NPV_rice_all <- discount(x = final_rice_result,
                           discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_vegetables_contract_farming <- discount(x = final_vegetable_contract_farming_result,
                                 discount_rate = discount_rate,
                                 calculate_NPV = TRUE)
  
  NPV_vegetable_contract_farming_decision <- discount(x = (final_vegetable_contract_farming_result - final_rice_result), 
                                     discount_rate = discount_rate,
                                     calculate_NPV = TRUE)
  
  
  # NPV farming scenario (rice vs vegetables) 
  
  # NPV_rice_farming <- discount(x = rice_farming_result, 
  #                      discount_rate = discount_rate, calculate_NPV = TRUE)
  # 
  # NPV_vegetables_farming <- discount(x = vegetable_farming_result, 
  #                            discount_rate = discount_rate,
  #                            calculate_NPV = TRUE)
  # 
  # NPV_farming_decision <- discount(x = (vegetable_farming_result - rice_farming_result), 
  #                                    discount_rate = discount_rate,
  #                                    calculate_NPV = TRUE)
  # 
  # ## NPV compost scenario (rice vs vegetables)
  # 
  # NPV_rice_compost <- discount(x = rice_farming_compost_result, 
  #                           discount_rate = discount_rate, calculate_NPV = TRUE)
  # 
  # NPV_vegetables_compost <- discount(x = vegetables_farming_compost_result, 
  #                                   discount_rate = discount_rate,
  #                                   calculate_NPV = TRUE)
  # 
  # NPV_farming_compost_decision <- discount(x = (vegetables_farming_compost_result - rice_farming_compost_result), 
  #                                  discount_rate = discount_rate,
  #                                  calculate_NPV = TRUE)
  # 
  # ## NPV agrotourism scenario (rice vs vegetables)
  # 
  # NPV_rice_agrotourism <- discount(x = rice_farming_agrotourism_result, 
  #                                discount_rate = discount_rate, calculate_NPV = TRUE)
  # 
  # NPV_vegetables_agrotourism <- discount(x = vegetables_farming_agrotourism_result, 
  #                                      discount_rate = discount_rate,
  #                                      calculate_NPV = TRUE)
  # 
  # NPV_farming_decision_agrotourism <- discount(x = vegetables_farming_agrotourism_result - rice_farming_agrotourism_result, 
  #                                        discount_rate = discount_rate,
  #                                        calculate_NPV = TRUE)
   
  # Return list ####
  
  return(list(NPV_rice = NPV_rice_all,
              NPV_vegetables = NPV_vegetables_all,
              NPV_vegetables_decision_do = NPV_vegetable_decision,
              NPV_vegetables_contract_farming_decision_do = NPV_vegetable_contract_farming_decision,
              cashflow_vegetables = final_vegetable_result,
              cashflow_vegetables_contract_farming = final_vegetable_contract_farming_result))
}


# Monte Carlo Simulation ####

# Run the Monte Carlo simulation using the model function #
TRV_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                  model_function = transition_rice_to_vegetables,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")


# Subset the outputs from the mcSimulation function (y) to summarize only on the variables that we want.
# names(TRV_mc_simulation$x)
mcSimulation_summary <- data.frame(TRV_mc_simulation$x[5:84],
                                   # names(TRV_mc_simulation$x)
                                   TRV_mc_simulation$y[3])

gtExtras::gt_plt_summary(mcSimulation_summary)


# NPV distribution #####

# plot NPV distribution for rice and vegetables
NPV_vegetable_decision <- plot_distributions(mcSimulation_object = TRV_mc_simulation, 
                                    vars = c("NPV_vegetables_decision_do",
                                             "NPV_vegetables_contract_farming_decision_do"),
                                    y_axis_name = "",
                                    color = c("lightblue", "lightgreen"),
                                    method = 'boxplot',
                                    base_size = 15)+
  labs(title = "NPV decisions")


# Cashflow analysis ####

# Plot cashflow vegetables
cashflow_vegetables <- plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_vegetables",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "lightblue", color_5_95 = "grey",
              color_median = "blue", base_size = 15)

# Plot cashflow vegetables under contract farming
cashflow_vegetables_contract_farming <- plot_cashflow(mcSimulation_object = TRV_mc_simulation, cashflow_var_name = "cashflow_vegetables_contract_farming",
                                     x_axis_name = "Years with intervention",
                                     y_axis_name = "Annual cashflow in USD",
                                     color_25_75 = "lightgreen", color_5_95 = "grey",
                                     color_median = "blue", base_size = 15)



# Projection to Latent Structures (PLS) analysis ####

# Plot PLS vegetables
pls_result_vegetables <- plsr.mcSimulation(object = TRV_mc_simulation,
                                           resultName = names(TRV_mc_simulation$y)[3], ncomp = 1)
plot_pls_vegetables <- plot_pls(pls_result_vegetables, threshold = 1, base_size = 20,
                                pos_color = "lightblue", neg_color = "firebrick3")+
  labs(title = "PLS Vegetables Decision", size = 8)


pls_result_vegetables <- plsr.mcSimulation(object = TRV_mc_simulation,
                                           resultName = names(TRV_mc_simulation$y)[3], ncomp = 1)
plot_pls_vegetables <- plot_pls(pls_result_vegetables, threshold = 1, base_size = 15)+
  labs(title = "PLS Vegetables Decision", size = 6)


# Plot PLS vegetables under contract farming
pls_result_vegetables_contract_farming <- plsr.mcSimulation(object = TRV_mc_simulation,
                                           resultName = names(TRV_mc_simulation$y)[4], ncomp = 1)

plot_pls_vegetables_contract_farming <- plot_pls(pls_result_vegetables, threshold = 1, base_size = 15)+
  labs(title = "PLS Vegetables Contract Farming Decision", size = 6)



# VoI analysis ####
mcSimulation_table <- data.frame(TRV_mc_simulation$x, TRV_mc_simulation$y[1:4])
evpi_TRV <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_rice", write_table = TRUE)

# Plot EVPI
plot_evpi_rice  <- plot_evpi(evpi_TRV, decision_vars = "NPV_rice", base_size = 15)
plot_evpi_vegetables  <- plot_evpi(evpi_TRV, decision_vars = "NPV_vegetables", base_size = 15)
plot_evpi_vegetables_decision  <- plot_evpi(evpi_TRV, decision_vars = "NPV_vegetables_decision_do", base_size = 15)
plot_evpi_vegetables_contract_farming_decision  <- plot_evpi(evpi_TRV, decision_vars = "NPV_vegetables_contract_farming_decision_do", base_size = 15)



