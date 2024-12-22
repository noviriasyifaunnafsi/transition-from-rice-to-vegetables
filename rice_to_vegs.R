library(decisionSupport)
library(gtExtras)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(svglite)
library(patchwork)
library(dplyr)

input_estimates <- read.csv("input_rice_to_vegs_usd_new.csv", sep = ";")

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_estimates))

transition_rice_to_vegetables <- function(x, varnames)
{
  
  # Background ####
  
  
  
  # This model uses two scenarios: 
  # (1) If farmers receive assistance from the government, and
  # (2) If farmers do not receive assistance from the government.
  # The government assistance in this model includes: fertilizer subsidies, 
  # agricultural machinery assistance, micro-credit, and vegetable processing
  # facilities (including buildings, processing machinery, and storage).
  
  
  # Scenario 1: If farmers receive assistance from the government ####
  
  ## 1. Rice #1 ####
  
  ###Rice farming costs #1 ####
  
  # Annual rice farming costs under normal conditions
  
  # Assuming rice is cultivated twice per year
  rice_farming_cost_with_gov_assistance_precal <- 
    rice_farming_input_costs_with_gov_assistance + 
    rice_machinery_costs_with_gov_assistance + 
    irrigation_maintenance_costs 
  rice_labor_costs # Normally, farmers do not pay for labor costs in cash. 
  # Instead, they share their harvest with those who help them during 
  # cultivation. Therefore, rice labor costs are estimated based on the 
  # amount of harvest that they will share with those who assist them.
  
  rice_farming_cost_with_gov_assistance <- 
    vv(rice_farming_cost_with_gov_assistance_precal, 
       n_year, 
       var_CV=10,
       relative_trend = inflation_rate)
  
  
  #### -> Rice risks (farming cost) #1 ####
  
  # Considering production risks (pests, diseases, heavy rainfall, water 
  # shortages, etc.), farmers need to pay higher costs for production 
  # management risks.
  
  # Event 1: Farmers need to buy more pesticides to manage pest and disease 
  # outbreaks
  
  
  rice_farming_input_costs_with_more_pesticides_precal <- 
    rice_farming_input_costs * 
    # farming input cost under normal condition
    (1+portion_rice_farming_input_cost_for_pest_disease_management)
  
  rice_farming_input_costs_with_more_pesticides <- 
    chance_event(chance_production_risk,
                 value_if = rice_farming_input_costs_with_more_pesticides_precal,
                 value_if_not = rice_farming_input_costs,
                 n = n_year,
                 CV_if = CV_value,
                 CV_if_not = 10)
  
  
  # Considering financial risks (lack of capital) and their impact on farming 
  # costs, farmers may not have enough capital to purchase farming inputs.
  # They might then obtain a loan from a bank through the micro-credit program 
  # (KUR). It is assumed that a farmer who owns 1 ha of land can receive a loan 
  # ranging from IDR 10 to 50 million (USD 625 to 3,125).
  
  # Event 2: Farmers obtain a loan from the bank, so they need to pay annual 
  # interest. This can add to their farming costs.
  
  # Consider if the farming input costs are higher because farmers need to buy 
  # more pesticides to manage pest and disease outbreaks.
  
  chance_risk_rice_farming_high_input_cost_with_bank_loan <- 
    chance_event(chance = chance_farmers_take_loan, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_farmers_take_loan == 1){
    
    rice_farming_input_costs_with_more_pesticides_with_gov_assistance <- 
      rice_farming_input_costs_with_gov_assistance * 
      # farming input cost under normal condition
      (1 + portion_rice_farming_input_cost_for_pest_disease_management)
    
    rice_farming_high_input_cost_with_bank_loan_with_gov_assistance_precal <- 
      (rice_farming_input_costs_with_more_pesticides_with_gov_assistance +
         rice_machinery_costs_with_gov_assistance + 
         rice_labor_costs) * annual_bank_interest
    
    rice_farming_high_input_cost_with_bank_loan_with_gov_assistance <- 
      vv(rice_farming_high_input_cost_with_bank_loan_with_gov_assistance_precal,
         n_year, var_CV=10, relative_trend = inflation_rate)
    
    # annual rice farming cost after considering production and financial risk
    final_rice_farming_cost_with_risk_with_gov_assistance <- 
      rice_farming_input_costs_with_more_pesticides +
      rice_machinery_costs_with_gov_assistance + 
      irrigation_maintenance_costs +
      rice_labor_costs
    
  }else{
    final_rice_farming_cost_with_risk_with_gov_assistance <- rep(x = 0, times=n_year)
  } 
  
  
  ###  Rice benefits #1 ####
  
  #### Rice farming #1 ####
  
  # Annual rice farming income under normal condition
  # Assuming rice is cultivated twice per year
  rice_farming_revenue <- vv((rice_yield * rice_farmgate_price), n_year, 
                             var_CV=CV_value, relative_trend = inflation_rate) 
  
  ##### -> Rice risks (farming benefits) #1 ####
  
  # Considering the impact of production risks on rice yield,
  # assuming there are risks such as pests, diseases, heavy rainfall, storms, 
  # and water shortages, which can cause losses to rice yield. A reduction in 
  # rice capital/cost may also lead to a decrease in yield.
  
  # Event 1: Rice yield reduction due to production and financial risks.
  rice_yield_loss_with_production_financial_risk <- rice_yield * 
    (prob_damage_production_risk_rice + prob_damage_market_risk_rice)
  
  rice_yield_with_production_financial_risk_precal <- rice_yield - 
    (rice_yield_loss_with_production_financial_risk)
  
  rice_yield_with_production_financial_risk <- 
    chance_event((chance_production_risk + chance_financial_risk_rice),
                 value_if = rice_yield_with_production_financial_risk_precal,
                 value_if_not = rice_yield,
                 n = n_year,
                 CV_if = CV_value,
                 CV_if_not = 10)
  
  
  # Considering market risks and their impact on rice prices,
  # which can cause a reduction in farmers' revenue.
  
  # Event 2: Rice revenue reduction due to market risks under normal conditions 
  # (without natural risks).
  
  rice_farmgate_price_with_market_risk <- 
    rice_farmgate_price * prob_damage_market_risk_rice
  
  rice_farming_revenue_loss_with_market_risk_normal <- 
    vv((rice_farmgate_price_with_market_risk * rice_yield), n=n_year,
       var_CV=CV_value, relative_trend = inflation_rate)
  
  rice_farming_revenue_with_market_risk_normal <- 
    chance_event(chance_market_risk_rice,
                 value_if = rice_farming_revenue_loss_with_market_risk_normal,
                 value_if_not = rice_farming_revenue, 
                 n = n_year,
                 CV_if = CV_value,
                 CV_if_not = 10)
  
  
  # Event 3: Rice revenue reduction due to market risks under production and 
  # financial risks.
  rice_farming_revenue_loss_with_market_production_financial_risk <- 
    rice_farmgate_price_with_market_risk * 
    rice_yield_with_production_financial_risk_precal
  
  
  # Annual rice farming revenue after considering risks
  final_rice_farming_revenue_with_gov_assistance <- 
    vv(rice_farming_revenue_loss_with_market_production_financial_risk,
       n_year, var_CV=CV_value, relative_trend = inflation_rate) 
  
  ####  Rice cultural value #1 ####
  
  # Here, we also include the cultural value of growing rice as a benefit.
  # We consider that farmers continue growing rice for cultural and social 
  # reasons, believing that producing their own rice provides them with a 
  # sense of 'pride' and security, knowing they have their own staple food.
  # The cultural value is quantified by considering the difference between 
  # the market price of milled rice and the farm-gate price of rice.
  
  # In addition, rice farmers typically engage in communal work ('gotong 
  # royong'), where they prepare, plant, and harvest together. This mutual 
  # support allows them to build strong bonds and save on labor costs. 
  # Thus, this can also be considered a cultural value.
  
  rice_cultural_value_with_gov_assistance <-  
    rice_cultural_value - rice_farming_cost_with_gov_assistance
  # rice cultural value is subtracted from the rice farming cost
  
  
  ## 2. Vegetable #1 ####
  
  ### Vegetable farming cost #1 ####
  
  # Annual vegetable farming costs under normal conditions
  # Assuming farmers grow vegetables with mixed cropping (6 crops per year)
  
  ## Annual vegetable costs (for all seasons throughout the year)
  vegetables_farming_cost_with_gov_assistance_precal <- 
    vegetables_farming_input_costs_with_gov_assistance + 
    vegetables_machinery_costs_with_gov_assistance + 
    vegetables_labor_costs
  
  vegetables_farming_cost_with_gov_assistance <- 
    vv(vegetables_farming_cost_with_gov_assistance_precal, 
       n_year, var_CV=10, relative_trend = inflation_rate)
  
  # Calculating the investment costs of growing vegetables.
  # Costs in the first year are higher than in subsequent years,
  # as farmers need to purchase more equipment and irrigation tools
  # for growing vegetables.
  vegetables_farming_cost_with_gov_assistance[1]  <- 
    vegetables_farming_cost_with_gov_assistance[1] + 
    vegetables_equipment_costs_first_year_with_gov_assistance + 
    vegetables_irrigation_costs_first_year_with_gov_assistance
  
  #### -> Vegetable risks (costs) #1 ####
  
  # Considering production risks (pests, diseases, heavy rainfall, water 
  # shortage, etc.), farmers need to pay higher costs for production management 
  # risk.
  
  # Event 1: Farmers need to buy more pesticides to overcome pest and disease 
  # outbreaks.
  
  vegetable_farming_input_costs_with_more_pesticides_precal <-
    vegetables_farming_input_costs_with_gov_assistance *
    # farming input cost under normal condition
    (1 + portion_vegetable_farming_input_cost_for_pest_disease_management)
  
  vegetable_farming_input_costs_with_more_pesticides <-
    chance_event(chance_production_risk,
                 value_if = vegetable_farming_input_costs_with_more_pesticides_precal,
                 value_if_not = vegetables_farming_input_costs,
                 n = n_year,
                 CV_if = CV_value,
                 CV_if_not = 10)
  
  
  # Considering financial risks (lack of capital) affecting farming costs.
  # Farmers may not have enough capital to purchase farming inputs.
  # They might then get a loan from a bank through the micro-credit program 
  # (KUR). We can consider that a farmer who owns 1 ha of land can get a loan 
  # ranging from IDR 10 to 50 million (USD 625 to 3125).
  
  # Event 2: Farmers receive a loan from the bank, so they need to pay annual 
  # interest, which can be added to their farming costs. Also, consider if the 
  # farming input costs are higher because farmers need to buy more pesticides 
  # to overcome pest and disease outbreaks.
  
  chance_risk_vegetable_farming_high_input_cost_with_bank_loan <- 
    chance_event(chance = chance_farmers_take_loan, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_farmers_take_loan == 1){
    
    vegetable_farming_input_costs_with_more_pesticides_with_gov_assistance <- 
      vegetables_farming_input_costs_with_gov_assistance * 
      # farming input cost under normal condition
      (1 + portion_vegetable_farming_input_cost_for_pest_disease_management)
    
    vegetable_farming_high_input_cost_with_bank_loan_with_gov_assistance_precal <- 
      (vegetable_farming_input_costs_with_more_pesticides_with_gov_assistance +
         vegetables_machinery_costs_with_gov_assistance + 
         vegetables_labor_costs) * annual_bank_interest
    
    vegetable_farming_high_input_cost_with_bank_loan_with_gov_assistance <- 
      vv(vegetable_farming_high_input_cost_with_bank_loan_with_gov_assistance_precal,
         n_year, var_CV=10, relative_trend = inflation_rate)
    
    
    # Annual vegetable farming costs after considering production risks.
    
    # Typically, field sizes in Sinjai are small, and it is very rare to find a 
    # 1 ha plot. Therefore, let's assume that a farmer has two different fields, 
    # each with a size of 0.5 ha, making a total of 1 ha. The farmer can 
    # grow 6 different vegetables per year. 
    
    # In this model, we assume that 'Field 1' is cultivated with Chinese 
    # mustard greens, green beans, and cabbage in rotation as main crops, with 
    # spring onions as a side crop. In 'Field 2', tomatoes and chilies are 
    # cultivated in rotation as main crops, with spring onions as a side crop.
    
    # For Field 1, the proportion of the field used for the main crops is 0.3 
    # ha, while the side crop uses 0.2 ha per season (0.6 ha per year) --> 1.5 
    # ha in total. For Field 2, the proportion of the field used for the main 
    # crops is 0.4 ha, while the side crop uses 0.1 ha per season (0.2 ha per 
    # year) --> 1 ha in total. Therefore, the total effective field size per year is 2.5 ha.
    
    # Since the unit cost is for 1 ha per year, the annual total cost should be calculated
    # based on the total effective field size per year.
    
    
    annual_final_vegetable_farming_cost_with_gov_assistance <- 
      vegetable_farming_high_input_cost_with_bank_loan_with_gov_assistance * 
      2.5 # effective field size
    
    final_vegetable_farming_cost_with_gov_assistance <- 
      vv(annual_final_vegetable_farming_cost_with_gov_assistance, 
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    final_vegetable_farming_cost_with_gov_assistance[1] <- 
      final_vegetable_farming_cost_with_gov_assistance[1] + 
      vegetables_equipment_costs_first_year_with_gov_assistance + 
      vegetables_irrigation_costs_first_year_with_gov_assistance
    
  }else{
    final_vegetable_farming_cost_with_gov_assistance <- rep(x = 0, times=n_year)
  }
  
  
  ### Vegetable benefits #1 ####
  
  #### -> Vegetable farming #1 ####
  
  # Annual vegetable farming revenue under normal condition
  
  # Typically, field sizes in Sinjai are small, and it's very rare to find a 
  # single 1 ha plot. Therefore, let's assume that this calculation is not for 
  # individual farmer, but farmers collectively (?) let's say for one 'tesang' 
  # (tenant) farmer and one for farmer who own the field and provide the inputs. 
  
  # In this case, we consider that they manage two different fields, 
  # each with a size of 0.5 ha, making a total of 1 ha. The farmer can 
  # grow 6 different vegetables per year. 
  
  # In this model, we consider that 'Field 1' is cultivated with Chinese mustard 
  # green, green beans, and cabbage in rotation as main crops, with spring onions 
  # as a side crop. In 'Field 2', tomatoes and chili are cultivated in rotation as 
  # main crops, with spring onions as a side crop.
  
  # Calculating revenue from 'Field 1'
  # For Field 1, the proportion of the field used for the main crops is 0.3 ha, 
  # while the side crop uses 0.2 ha per season (0.6 ha per year)
  
  chinese_mustard_green_revenue_precal <- (chinese_mustard_green_yield * 
                                             chinese_mustard_green_price) * 0.3 
  chinese_mustard_green_revenue <- vv(chinese_mustard_green_revenue_precal, 
                                      n_year, var_CV=40)
  
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
  
  # we can just assume that the total revenue for both scenarios are the same
  # except for the income, they are different because of the costs differ 
  # depends on how much the support from government to farmers
  
  
  ###### ->> Vegetable risks (farming benefits) ####
  
  ## chance event on vegetable farming benefits ##
  
  # considering production, financial, and market risk damage on vegetable yield
  
  # considering production risk damage on vegetable yield
  # assuming there are some risks like pest, disease, heavy rainfall, storm, 
  # and water shortage causing some lose to yield and reduction to vegetable
  # capital/cost may also reducing yield
  
  # calculate the annual total yield of vegetables for production risks
  
  annual_chinese_mustard_green_yield <- vv((chinese_mustard_green_yield * 0.3), 
                                           n_year, var_CV=CV_value)
  
  annual_green_bean_yield <- vv((green_bean_yield * 0.3), n_year, 
                                var_CV=CV_value)
  
  annual_cabbage_yield <- vv((cabbage_yield * 0.3), n_year, var_CV=CV_value)
  
  annual_chili_yield <- vv((chili_yield * 0.4), n_year, var_CV=CV_value)
  
  annual_tomato_yield <- vv((tomato_yield * 0.4), n_year, var_CV=CV_value)
  
  annual_spring_onion_yield <- vv((spring_onion_yield * 0.8), 
                                  #total from field 1 and field 2
                                  n_year, var_CV=CV_value) 
  
  annual_total_yield_vegetables <- annual_chinese_mustard_green_yield + 
    annual_green_bean_yield + annual_cabbage_yield + annual_chili_yield + 
    annual_tomato_yield + annual_spring_onion_yield
  
  # event-1 vegetable yield reduction from production and financial risks
  
  ## calculate the yield loss with production risk
  vegetable_yield_loss_with_production_risk <- annual_total_yield_vegetables * 
    prob_damage_production_risk_vegetables
  
  ## calculate the yield with reduction from production risk
  vegetable_yield_with_production_risk_precal <- 
    vv((annual_total_yield_vegetables - 
          vegetable_yield_loss_with_production_risk), n_year, var_CV=CV_value)
  
  
  ## chance event with production risk
  vegetable_yield_with_production_risk <- 
    chance_event(chance_production_risk,
                 value_if = vegetable_yield_with_production_risk_precal,
                 value_if_not = annual_total_yield_vegetables,
                 n = n_year,
                 CV_if = CV_value,
                 CV_if_not = 10) # the variance may not so differ 
  # if without production risk
  
  ## calculate the yield loss with financial risk
  vegetable_yield_loss_with_financial_risk <- annual_total_yield_vegetables * 
    prob_damage_financial_risk_vegetables
  
  ## calculate the yield with reduction from financial risk
  vegetable_yield_with_financial_risk_precal <- annual_total_yield_vegetables - 
    vegetable_yield_loss_with_financial_risk
  
  ## chance event with financial risk
  vegetable_yield_with_financial_risk <- 
    chance_event(chance_financial_risk_vegetables,
                 value_if = vegetable_yield_with_financial_risk_precal,
                 value_if_not = annual_total_yield_vegetables,
                 n = n_year, 
                 CV_if = CV_value,
                 CV_if_not = 10) # the variance may not so differ 
  # if without financial risk
  
  ## calculate the yield loss if production and financial risk occur the same time
  vegetable_yield_loss_with_production_and_financial_risk <- 
    annual_total_yield_vegetables * 
    (prob_damage_production_risk_vegetables +
       prob_damage_financial_risk_vegetables)
  
  ## calculate the yield with reduction from production and financial risk 
  vegetable_yield_with_production_and_financial_risk_precal <- 
    annual_total_yield_vegetables - 
    vegetable_yield_loss_with_production_and_financial_risk
  
  ## chance event if production and financial risk occur the same time
  vegetable_yield_with_production_and_financial_risk <- 
    chance_event((chance_production_risk + chance_financial_risk_vegetables),
                 value_if = vegetable_yield_with_production_and_financial_risk_precal,
                 value_if_not = annual_total_yield_vegetables,
                 n = n_year,
                 CV_if = CV_value,
                 CV_if_not = 10) # the variance may not so differ 
  # if without financial risk
  
  
  
  # event-2 vegetable revenue reduction from market risk under normal condition
  ## (not include other risks e.g. production and financial)
  
  # considering market risk damage on vegetable price
  # causing reduction to farmers' revenue
  
  ## calculate vegetable price with market risk
  vegetable_price_with_market_risk <- 
    vv((vegetable_farmgate_prices * prob_damage_market_risk_vegetables), 
       n_year, var_CV=CV_value)
  
  ## calculate vegetable farming revenue with market risk only 
  vegetable_farming_revenue_with_market_risk_normal <- 
    vegetable_price_with_market_risk * annual_total_yield_vegetables
  
  ## chance event vegetable revenue with market risk only
  vegetable_farming_revenue_with_market_risk_normal <- 
    chance_event(chance_market_risk_vegetables,
                 value_if = vegetable_farming_revenue_with_market_risk_normal,
                 value_if_not = total_revenue_vegetables,
                 n = n_year,
                 CV_if = CV_value,
                 CV_if_not = 10) # the variance may not so differ 
  # if without financial risk
  
  
  # event-3 vegetable revenue reduction from market risk under production 
  # and financial risk
  
  vegetable_farming_revenue_loss_with_market_production_financial_risk <- 
    vegetable_price_with_market_risk * 
    vegetable_yield_with_production_and_financial_risk
  
  vegetable_farming_revenue_with_market_production_financial_risk_precal <- 
    total_revenue_vegetables - 
    vegetable_farming_revenue_loss_with_market_production_financial_risk
  
  vegetable_farming_revenue_with_market_production_financial_risk <- 
    chance_event(chance_market_risk_rice,
                 value_if = vegetable_farming_revenue_with_market_production_financial_risk_precal,
                 value_if_not = total_revenue_vegetables,
                 n = n_year,
                 CV_if = CV_value,
                 CV_if_not = 10) # the variance may not so differ 
  # if without financial risk
  
  
  # annual vegetables farming revenue after considering all risks
  final_vegetable_farming_revenue <- 
    vv(vegetable_farming_revenue_with_market_production_financial_risk_precal, 
       n_year, var_CV=CV_value,
       relative_trend = inflation_rate)
  
  
  
  #### -> Vegetable health benefit ####
  
  # We can assume that the benefit by growing vegetables
  # people can consume vegetables easily and cheaper
  # which can be associated to improving nutrition
  
  chance_savings_from_vegetables <- 
    chance_event(chance = chance_consume_own_vegetables, 
                 value_if = 1, value_if_not = 0) 
  
  if(chance_savings_from_vegetables == 1){
    
    # annual savings for purchasing vegetables   
    
    average_vegetables_price_market_vv <- vv(average_vegetables_price_market,
                                             n_year, var_CV=CV_value, 
                                             relative_trend = inflation_rate)
    
    annual_vegs_consumption_per_person_vv <- vv(annual_vegs_consumption_per_person,
                                                n_year, var_CV=CV_value)
    
    annual_spending_for_vegetables <- average_vegetables_price_market_vv *
      annual_vegs_consumption_per_person_vv
    
    # we can assume that the amount of annual spending for vegetables as benefit 
    # as if they grow vegetables then they don't need to spend those money
    # so this amount is added up to the total benefits
    
    vegetables_nutrition_health_benefit <- annual_spending_for_vegetables
    
  }else{
    vegetables_nutrition_health_benefit <- rep(x = 0, times=n_year)
  }  
  
  
  # 2nd scenario: if farmers don't get assistance from the government ####
  
  ## 3. Rice #2 ####
  
  ### Rice farming costs #2 ####
  
  # Annual rice farming cost under normal condition
  # Assuming rice is cultivated twice per year
  rice_farming_cost_without_gov_assistance_precal <- 
    rice_farming_input_costs + 
    rice_machinery_costs + 
    irrigation_maintenance_costs +
    rice_labor_costs #normally farmers don't pay for labor cost in cash, instead 
  # they share their harvest to those who help them during the cultivation. 
  # So, rice labor costs is estimated based on how much the amount of harvest 
  # that they will share with those who help them.
  
  rice_farming_cost_without_gov_assistance <- 
    vv(rice_farming_cost_without_gov_assistance_precal, 
       n_year, 
       var_CV=10,
       relative_trend = inflation_rate)
  
  
  #### -> Rice risks (farming cost) #2 ####
  
  # considering production risk (pest, disease, heavy rainfall, water shortage, 
  # etc.), so farmers need to pay higher cost for production management risk
  # event-1, farmers need to buy more pesticides to overcome pest and disease 
  # outbreak
  
  rice_farming_input_costs_with_more_pesticides_precal <- 
    rice_farming_input_costs * 
    # farming input cost under normal condition
    (1+portion_rice_farming_input_cost_for_pest_disease_management)
  
  rice_farming_input_costs_with_more_pesticides <- 
    chance_event(chance_production_risk,
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
  
  # consider if the farming input cost is higher because farmers need to buy 
  # more pesticides to overcome pest and disease outbreak
  
  chance_risk_rice_farming_high_input_cost_with_bank_loan <- 
    chance_event(chance = chance_farmers_take_loan, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_farmers_take_loan == 1){
    
    rice_farming_input_costs_with_more_pesticides_without_gov_assistance <- 
      rice_farming_input_costs * 
      # farming input cost under normal condition
      (1 + portion_rice_farming_input_cost_for_pest_disease_management)
    
    rice_farming_high_input_cost_with_bank_loan_without_gov_assistance_precal <- 
      (rice_farming_input_costs_with_more_pesticides_without_gov_assistance +
         rice_machinery_costs + 
         rice_labor_costs) * annual_bank_interest
    
    rice_farming_high_input_cost_with_bank_loan_without_gov_assistance <- 
      vv(rice_farming_high_input_cost_with_bank_loan_without_gov_assistance_precal,
         n_year, var_CV=10, relative_trend = inflation_rate)
    
    
    
    # annual rice farming cost after considering production and financial risk
    final_rice_farming_cost_with_risk_without_gov_assistance <- 
      rice_farming_input_costs_with_more_pesticides +
      rice_machinery_costs + 
      irrigation_maintenance_costs +
      rice_labor_costs #normally farmers don't pay for labor cost in cash, instead 
    # they share their harvest to those who help them during the cultivation. 
    # So, rice labor costs is estimated based on how much the amount of harvest 
    # that they will share with those who help them.
    
    
  }else{
    final_rice_farming_cost_with_risk_without_gov_assistance <- 
      rep(x = 0, times=n_year)
  } 
  
  
  ### Rice benefits #2 ####
  
  #### Rice farming #2 ####
  
  # Annual rice farming income under normal condition
  # Assuming rice is cultivated twice per year
  rice_farming_revenue <- vv((rice_yield * rice_farmgate_price), n_year, 
                             var_CV=CV_value, relative_trend = inflation_rate) 
  
  
  ##### -> Rice risks (farming benefits) #2 ####
  
  ## Chance event options on rice farming benefits ##
  
  # considering production risk damage on rice yield
  # assuming there are some risks like pest, disease, heavy rainfall, storm, 
  # and water shortage causing some lose to rice yield and reduction to rice 
  # capital/cost may also reducing yield
  
  # event-1 rice yield reduction from production and financial risks
  rice_yield_loss_with_production_financial_risk <- rice_yield * 
    (prob_damage_production_risk_rice + prob_damage_market_risk_rice)
  
  rice_yield_with_production_financial_risk_precal <- rice_yield - 
    (rice_yield_loss_with_production_financial_risk)
  
  rice_yield_with_production_financial_risk <- 
    chance_event((chance_production_risk + chance_financial_risk_rice),
                 value_if = rice_yield_with_production_financial_risk_precal,
                 value_if_not = rice_yield,
                 n = n_year,
                 CV_if = CV_value,
                 CV_if_not = 10)
  
  
  # considering market risk damage on rice price
  # causing reduction to farmers' revenue
  
  # event-2 rice revenue reduction from market risk under normal condition 
  # (without natural risks)
  rice_farmgate_price_with_market_risk <- 
    rice_farmgate_price * prob_damage_market_risk_rice
  
  rice_farming_revenue_loss_with_market_risk_normal <- 
    vv((rice_farmgate_price_with_market_risk * rice_yield), n=n_year,
       var_CV=CV_value, relative_trend = inflation_rate)
  
  rice_farming_revenue_with_market_risk_normal <- 
    chance_event(chance_market_risk_rice,
                 value_if = rice_farming_revenue_loss_with_market_risk_normal,
                 value_if_not = rice_farming_revenue, 
                 n = n_year,
                 CV_if = CV_value,
                 CV_if_not = 10)
  
  
  # event-3 rice revenue reduction from market risk under production and 
  # financial risk
  rice_farming_revenue_loss_with_market_production_financial_risk <- 
    rice_farmgate_price_with_market_risk * 
    rice_yield_with_production_financial_risk_precal
  
  
  # annual rice farming revenue after considering risk
  final_rice_farming_revenue_without_gov_assistance <- 
    vv(rice_farming_revenue_loss_with_market_production_financial_risk,
       n_year, var_CV=CV_value, relative_trend = inflation_rate) 
  
  
  #### Rice cultural value #2 ####
  
  # here we also add cultural value of growing rice as a benefit
  # we consider those farmers keep growing rice due to cultural and social 
  # reasons, in which that they think producing their own rice would give them 
  # 'pride' and make them feel secure for having their own staple food
  # the cultural value is quantified by considering the differences between
  # market price of milled rice and farm-gate price of rice
  
  # In addition, rice farmers normally do communal work (?) by preparing all
  # from planting until harvesting together, so they help each other to prepare 
  # and harvest their field simultaneously and this allow them to build strong
  # bond (?) and save the cost of labor. So this is also can be considered as 
  # cultural value.
  
  # rice_market_price_vv <- vv(rice_market_price, n = n_year, var_CV=CV_value)
  
  rice_cultural_value_with_gov_assistance <-  
    rice_cultural_value - final_rice_farming_cost_with_risk_without_gov_assistance
  # rice cultural value is subtracted to rice farming cost 
  
  
  ## 4. Vegetable #2 ####
  
  ### Vegetable farming cost #2 ####
  
  # Annual vegetable farming cost under normal condition
  # Assuming farmers grow vegetables with mix cropping (6 crops per year)
  
  ## annual vegetable costs (all season throughout the year)
  vegetables_farming_cost_without_gov_assistance_precal <- 
    vegetables_farming_input_costs + 
    vegetables_machinery_costs + 
    vegetables_labor_costs
  
  vegetables_farming_cost_without_gov_assistance <- 
    vv(vegetables_farming_cost_without_gov_assistance_precal, 
       n_year, var_CV=10, relative_trend = inflation_rate)
  
  # calculating the implementation costs of growing vegetables 
  # costs in the first year is higher than following other years
  # considering farmers need to purchase more equipment and irrigation tools
  # for growing vegetables
  vegetables_farming_cost_without_gov_assistance[1]  <- 
    vegetables_farming_cost_without_gov_assistance[1] + 
    vegetables_equipment_costs_first_year + 
    vegetables_irrigation_costs_first_year
  
  #### -> Vegetable risks (costs) #2 ####
  
  # considering production risk (pest, disease, heavy rainfall, water shortage, 
  # etc.) so farmers need to pay higher cost for production management risk
  
  # event-1, farmers need to buy more pesticides to overcome pest and disease 
  # outbreak
  
  vegetable_farming_input_costs_with_more_pesticides_precal <-
    vegetables_farming_input_costs *
    # farming input cost under normal condition
    (1 + portion_vegetable_farming_input_cost_for_pest_disease_management)
  
  vegetable_farming_input_costs_with_more_pesticides <-
    chance_event(chance_production_risk,
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
  # consider if the farming input cost is higher because farmers need to buy 
  # more pesticides to overcome pest and disease outbreak
  
  chance_risk_vegetable_farming_high_input_cost_with_bank_loan <- 
    chance_event(chance = chance_farmers_take_loan, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_farmers_take_loan == 1){
    
    vegetable_farming_input_costs_with_more_pesticides_without_gov_assistance <- 
      vegetables_farming_input_costs * 
      # farming input cost under normal condition
      (1 + portion_vegetable_farming_input_cost_for_pest_disease_management)
    
    vegetable_farming_high_input_cost_with_bank_loan_without_gov_assistance_precal <- 
      (vegetable_farming_input_costs_with_more_pesticides_without_gov_assistance +
         vegetables_machinery_costs + 
         vegetables_labor_costs) * annual_bank_interest
    
    vegetable_farming_high_input_cost_with_bank_loan_without_gov_assistance <- 
      vv(vegetable_farming_high_input_cost_with_bank_loan_without_gov_assistance_precal,
         n_year, var_CV=10, relative_trend = inflation_rate)
    
    
    # Annual vegetable farming cost after considering production risk
    
    # Typically, field sizes in Sinjai are small, and it's very rare to find a 
    # 1 ha plot. Therefore, let's assume that a farmer has two different fields, 
    # each with a size of 0.5 ha, making a total of 1 ha. The farmer can 
    # grow 6 different vegetables per year. 
    
    # In this model, we consider that 'Field 1' is cultivated with Chinese 
    # mustard green, green beans, and cabbage in rotation as main crops, with 
    # spring onions as a side crop. In 'Field 2', tomatoes and chili are 
    # cultivated in rotation as main crops, with spring onions as a side crop.
    
    # For Field 1, the proportion of the field used for the main crops is 0.3 
    # ha, while the side crop uses 0.2 ha per season (0.6 ha per year) --> 1.5 
    # ha in total. For Field 2, the proportion of the field used for the main 
    # crops is 0.4 ha, while the side crop uses 0.1 ha per season (0.2 ha per 
    # year) --> 1 ha in total. Therefore, the total effective field size per 
    # year is 2.5 ha
    
    # Since the unit cost is for 1 ha per year, then the annual total cost 
    # should be calculated based on the total effective field size per year
    
    annual_final_vegetable_farming_cost_without_gov_assistance <- 
      vegetable_farming_high_input_cost_with_bank_loan_without_gov_assistance * 
      2.5 # effective field size
    
    final_vegetable_farming_cost_without_gov_assistance <- 
      vv(annual_final_vegetable_farming_cost_without_gov_assistance, 
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    final_vegetable_farming_cost_without_gov_assistance[1] <- 
      final_vegetable_farming_cost_without_gov_assistance[1] + 
      vegetables_equipment_costs_first_year + 
      vegetables_irrigation_costs_first_year
    
  }else{
    final_vegetable_farming_cost_without_gov_assistance <- 
      rep(x = 0, times=n_year)
  }
  
  
  ### Vegetable benefits #2 ####
  
  #### -> Vegetable farming #2 ####
  
  # Annual vegetable farming revenue under normal condition
  
  # Typically, field sizes in Sinjai are small, and it's very rare to find a 
  # single 1 ha plot. 
  # Therefore, let's assume that this calculation is not for individual farmer, 
  # but farmers collectively (?) let's say for one 'tesang' (tenant) farmer 
  # and one for farmer who own the field and provide the inputs. 
  
  # In this case, we consider that they manage two different fields, 
  # each with a size of 0.5 ha, making a total of 1 ha. The farmer can 
  # grow 6 different vegetables per year. 
  
  # In this model, we consider that 'Field 1' is cultivated with Chinese 
  # mustard green, green beans, and cabbage in rotation as main crops, 
  # with spring onions as a side crop. In 'Field 2', tomatoes and chili 
  # are cultivated in rotation as main crops, with spring onions as a side crop.
  
  # Calculating revenue from 'Field 1'
  # For Field 1, the proportion of the field used for the main crops is 0.3 ha, 
  # while the side crop uses 0.2 ha per season (0.6 ha per year)
  
  chinese_mustard_green_revenue_precal <- (chinese_mustard_green_yield * 
                                             chinese_mustard_green_price) * 0.3 
  chinese_mustard_green_revenue <- vv(chinese_mustard_green_revenue_precal, 
                                      n_year, var_CV=40)
  
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
  total_revenue_field_2 <- chili_revenue + tomato_revenue + 
    spring_onion_revenue_2 
  
  # total revenue from all vegetable fields
  
  total_revenue_vegetables <- total_revenue_field_1 + total_revenue_field_2
  
  # we can just assume that the total revenue for both scenarios are the same
  # except for the income, they are different because of the costs differ 
  # depends on how much the support from governement to farmers
  
  
  
  
  ##### ->> Vegetable risks (farming benefits) #2 ####
  
  # since we assume that the total revenue for both scenarios are the same
  # so we skip the calculation for vegetable risks for farming benefits 
  # for the 2nd scenario since it should be the same as the calculation
  # for the 1st scenario
  
  
  #### -> Vegetable health benefit #2 ####
  
  # We assume that the health benefits from vegetable benefits are the same for 
  # both scenarios, so we skip the calculation for vegetable health benefit #2 
  
  
  
  # Chance event ####
  
  # We assume there are some chance events for farmers who grow vegetables
  # to diversify their income. Farmers may do processing for their vegetables
  # which can help their income when the market price reduce significantly.
  # In addition, farmers may also consider to do composting from the biomass
  # waste of their vegetable farms. There is also a chance where they can promote
  # agrotourism from their own field. However, the chance of this event might 
  # be quite lower.
  
  ## Vegetable processing ####
  
  ## Farmers may do processing for their vegetables which can help their 
  # income when the market price reduce significantly.
  
  # Probability of vegetable processing 
  
  chance_vegetable_processing <- 
    chance_event(chance = prob_of_processing_vegetables, 
                 value_if = 1, value_if_not = 0) 
  
  if(chance_vegetable_processing == 1){
    
    ### 1st scenario: Cost of vegetable processing ####
    
    # For the first scenario, farmers get assistance from the government, so in 
    # this case, we consider that the processing facilities including
    # post-harvest warehouse, processing factory, and machine are already 
    # facilitated by government funding. So farmer only need to bear cost of 
    # maintenance, packaging product, marketing, and distribution.
    
    # First, we need to calculate the establishment cost for processing 
    # vegetable products.
    
    maintenance_vegetable_processing_cost <- 
      vv((vegetable_processing_equipment_maintenance_cost +
            vegetable_storage_maintenance_cost), n_year, var_CV = CV_value, 
         relative_trend = inflation_rate) 
    
    marketing_distribution_vegetable_processing_cost <- 
      vv(marketing_and_distribution_for_vegetable_product, n_year, 
         var_CV = CV_value, relative_trend = inflation_rate)
    
    # Calculate labor cost for processing vegetables
    # Normally, the processing is done by the women farmers' group
    # Each group only processing the products from their own group
    # So they rarely hire labor
    
    # Here, we can just assume that if they hire labor during the peak season (?)
    # Let's say, they can hire labor 2 person for 1 week per season
    # If vegetable season is 3 times per year, then it's gonna be 6 person hired
    # per year. Daily labor will be calculated for 2 person x 7 days x 3 = 42 labors
    
    annual_cost_labor_vegetable_processing <- vv((daily_labor_cost * 42), 
                                                 #42 days for hiring labor
                                                 n_year, var_CV=CV_value, 
                                                 relative_trend = inflation_rate)
    
    # Then, calculate the annual vegetable processing product cost
    # Which is included for the cost of labor, maintenance, marketing, 
    # and distribution
    vegetable_processing_cost_with_gov_assistance_precal <- 
      annual_cost_labor_vegetable_processing +
      maintenance_vegetable_processing_cost + 
      marketing_distribution_vegetable_processing_cost
    
    final_vegetable_processing_cost_with_gov_assitance <- 
      vv(vegetable_processing_cost_with_gov_assistance_precal,
         n_year, var_CV = CV_value, relative_trend = inflation_rate)
    
    # chance event options on vegetable processing costs #
    
    # considering financial risk (lack of capital)
    # first, we simply consider that farmers can't proceed to process vegetable
    # so they probably just simply throw away their
    
    # event-1, farmers don't have enough capital for processing vegetable
    
    chance_risk_farmer_have_no_capital_for_processing <- 
      chance_event(chance = chance_financial_risk_vegetables, value_if = 0, 
                   value_if_not = 1) 
    
    if(chance_financial_risk_vegetables == 1){
      
      vegetable_processing_cost_with_gov_assistance_precal <- 
        annual_cost_labor_vegetable_processing +
        maintenance_vegetable_processing_cost + 
        marketing_distribution_vegetable_processing_cost
      
      final_vegetable_processing_cost_with_gov_assitance <- 
        vv(vegetable_processing_cost_with_gov_assistance_precal,
           n_year, var_CV = CV_value, relative_trend = inflation_rate)
      
    }else{
      final_vegetable_processing_cost_with_gov_assitance <- 
        rep(x = 0, times=n_year)
    }
    
    
    # vegetable_farmer_have_no_capital_for_processing <-
    #   chance_event(chance_financial_risk_vegetables,
    #                value_if = 0,
    #                value_if_not = vegetable_processing_cost_with_gov_assistance_precal,
    #                n = n_year)
    
    
    ## 2nd scenario: Cost of vegetable processing ####
    
    # For the second scenario, farmers don't get assistance from government
    # So then we can consider farmers need to request loan to bank for the 
    # capital of processing vegetable. If farmers get loan from bank, they need 
    # to pay annual interest, and this is can add up their costs
    
    # First, we need to calculate the overall cost of vegetable processing
    # without government assistance
    
    vegetable_processing_cost_without_gov_assistance_precal <- 
      vegetable_processing_building_facility_cost +
      annual_cost_labor_vegetable_processing +
      vegetable_processing_equipment_procurement_cost +
      vegetable_processing_storage_equipment_procurement_cost +
      maintenance_vegetable_processing_cost + 
      marketing_distribution_vegetable_processing_cost
    
    vegetable_processing_cost_without_gov_assistance <- 
      vv(vegetable_processing_cost_without_gov_assistance_precal, 
         n_year, var_CV=10, relative_trend = inflation_rate)
    
    
    # annual vegetable processing cost after considering financial risk
    # we consider if farmer get loan from bank
    chance_risk_vegetable_processing_cost_with_bank_loan <- 
      chance_event(chance = chance_farmers_take_loan, value_if = 1, 
                   value_if_not = 0) 
    
    if(chance_farmers_take_loan == 1){
      
      vegetable_processing_costs_with_bank_loan <- 
        vegetable_processing_cost + (vegetable_processing_cost * 
                                       annual_bank_interest)
      
      final_vegetable_processing_cost_without_gov_assistance <- 
        vv(vegetable_processing_costs_with_bank_loan,
           n_year, var_CV=10, relative_trend = inflation_rate)
      
    }else{
      final_vegetable_processing_cost_without_gov_assistance <- 
        rep(x = 0, times=n_year)
    }
    
    
    ## Benefit of vegetable processing ####
    
    # Assuming that around 1 to 10% of yield are low grade
    # The lower grade can be processed as sauces, paste, etc.
    # We only consider tomato and chili that can be processed as processed food
    
    # calculate annual tomato raw material
    tomato_raw_material <- vv(annual_tomato_yield * 
                                portion_low_grade_vegetable_product, 
                              n_year, var_CV=40) #CV is quite higher considering 
    # that not all farmers would do processing
    
    # calculate chili raw material
    chili_raw_material <- vv(annual_chili_yield * 
                               portion_low_grade_vegetable_product, 
                             n_year, var_CV=40) #CV is quite higher considering 
    # that not all farmers would do processing
    
    # revenue from the processed tomato sauce/paste product
    # assuming that the result is 30% from the raw material
    tomato_sauce_product <- tomato_raw_material * 0.3 # 30%
    tomato_sauce_product_revenue <- vv(tomato_sauce_product * tomato_sauce_price, 
                                       n_year, var_CV=10, 
                                       relative_trend = inflation_rate)
    
    # revenue from the processed chili sauce/paste product
    # assuming that the result is 30% from the raw material
    chili_sauce_product <- chili_raw_material * 0.3 # 30%
    chili_sauce_product_revenue <- vv(chili_sauce_product * chili_sauce_price, 
                                      n_year, var_CV=10, 
                                      relative_trend = inflation_rate)
    
    # sum up the revenue from both products
    processed_vegetable_product_revenue <- tomato_sauce_product_revenue +
      chili_sauce_product_revenue
    
    
    # chance event options ###
    
    # Considering market risk damage on processed vegetable product
    # farmers may not be able to sell their product due to low market demand
    # this can reduce the revenue from vegetable processed product
    
    vegetable_processed_revenue_loss_market_risk <- 
      (processed_vegetable_product_revenue - 
         (processed_vegetable_product_revenue * 
            vegetable_processed_income_loss_market_risk)) 
    
    vegetable_processed_revenue_with_market_risk <- 
      chance_event(chance_market_risk_vegetable_processed,
                   value_if = vegetable_processed_revenue_loss_market_risk,
                   value_if_not = processed_vegetable_product_revenue,
                   n = n_year,
                   CV_if = CV_value,
                   CV_if_not = 10)
    
    # annual vegetable processed product revenue after considering risk
    final_vegetable_processed_revenue <- 
      vv(vegetable_processed_revenue_loss_market_risk, 
         n_year, var_CV=CV_value, relative_trend = inflation_rate) 
    
  }else{
    final_vegetable_processing_cost_with_gov_assitance <- rep(x = 0, times=n_year)
    final_vegetable_processing_cost_without_gov_assitance <- rep(x = 0, times=n_year)
    final_vegetable_processed_revenue <- rep(x = 0, times=n_year)
  }
  
  
  ## Compost ####
  
  # farmers may also consider to do composting from the biomass waste of 
  # their farms.
  # so all the costs will be bear by farmers themselves.
  
  # since there is no particular program from the government for composting
  # we can just consider that the chance event for composting is all under
  # the 2nd scenario, which is without gov assistance
  
  # Probability of compost 
  
  chance_compots <- chance_event(chance = prob_of_compost, value_if = 1, 
                                 value_if_not = 0) 
  if(chance_compots == 1){
    
    ### Rice compost cost ####
    
    # Normally, most rice farmers don't utilize the rice biomass residue as 
    # compost. They will just simply throw the waste away or burn it
    
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
    
    annual_cost_labor_rice_composting <- vv((daily_labor_cost * 10),
                                            #10 days for hiring labor
                                            n_year, var_CV = CV_value,
                                            relative_trend = inflation_rate)
    
    # Then, we calculate the annual rice biomass composting cost
    # Which is included for the cost of labor, compost activator, and maintenance
    annual_rice_compost_cost <- annual_cost_labor_rice_composting +
      compost_activator_cost + composter_maintenance_cost + 
      manure_for_compost_cost
    
    rice_compost_cost <- vv(annual_rice_compost_cost, n_year, var_CV=10)
    
    # Calculate the cost with the first year establishment cost
    final_rice_compost_cost[1] <- rice_compost_cost[1] + first_year_composting_cost 
    
    
    ## Rice compost benefits ####
    
    # Assuming that 50% of biomass resulting in compost
    amount_rice_compost <- vv((biomass_rice * 0.5), n_year, 
                              var_CV=CV_value, relative_trend = inflation_rate)
    
    # Annual income for composting rice biomass residue
    rice_compost_revenue <- vv((amount_rice_compost * price_rice_compost), 
                               n_year, var_CV=CV_value, 
                               relative_trend = inflation_rate)
    
    
    ## Chance event options on rice compost benefits ##
    
    # Considering market risk damage on composting rice
    # farmers may not be able to sell their compost due to low market demand
    # this can reduce the revenue from rice compost
    
    rice_compost_revenue_loss_market_risk <- rice_compost_revenue - 
      (rice_compost_revenue * portion_compost_loss_revenue)
    
    rice_compost_revenue_with_market_risk <- 
      chance_event(chance_market_risk_compost,
                   value_if = rice_compost_revenue_loss_market_risk,
                   value_if_not = rice_compost_revenue, 
                   n = n_year,
                   CV_if = CV_value,
                   CV_if_not = 10)
    
    # annual rice compost revenue after considering risk
    final_rice_compost_revenue <- vv(rice_compost_revenue_with_market_risk, 
                                     n_year, var_CV=CV_value,
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
    
    annual_cost_labor_vegetable_composting <- vv((daily_labor_cost * 15), 
                                                 #15 days for hiring labor
                                                 n_year, var_CV = CV_value,
                                                 relative_trend = inflation_rate)
    
    # Then, we calculate the annual vegetable biomass composting cost
    # Which is included for the cost of labor, compost activator, and maintenance
    vegetable_compost_cost_precal <- annual_cost_labor_vegetable_composting +
      compost_activator_cost + composter_maintenance_cost
    
    final_vegetable_compost_cost <- vv(vegetable_compost_cost_precal, 
                                       n_year, var_CV=10)
    
    # Calculate the cost with the first year establishment cost
    final_vegetable_compost_cost[1] <- final_vegetable_compost_cost[1] + first_year_composting_cost
    
    
    ## Vegetable compost benefits ####
    
    # Assuming that 30% of biomass resulting in compost
    amount_vegetable_compost <- vv((biomass_vegetables * 0.3), n_year, 
                                   var_CV = CV_value)
    
    # Annual income for composting vegetables biomass residue
    vegetables_compost_revenue <- vv((amount_vegetable_compost * 
                                        price_vegetable_compost), 
                                     n_year, var_CV=10, 
                                     relative_trend = inflation_rate)
    
    ## Chance event options on vegetable compost benefits ##
    
    # Considering market risk damage on composting vegetables
    # farmers may not be able to sell their compost due to low market demand
    # this can reduce the revenue from vegetable compost
    
    vegetable_compost_revenue_loss_market_risk <- vegetables_compost_revenue - 
      (vegetables_compost_revenue * portion_compost_loss_revenue)
    
    vegetable_compost_revenue_with_market_risk <- 
      chance_event(chance_market_risk_compost,
                   value_if = vegetable_compost_revenue_loss_market_risk,
                   value_if_not = vegetables_compost_revenue,
                   n = n_year,
                   CV_if = CV_value,
                   CV_if_not = 10)
    
    # annual vegetable compost revenue after considering risk
    final_vegetable_compost_revenue <- 
      vv(vegetable_compost_revenue_with_market_risk, 
         n_year, var_CV=CV_value,
         relative_trend = inflation_rate) 
    
    
  }else{
    final_rice_compost_cost <- rep(x = 0, times=n_year)
    final_rice_compost_revenue <- rep(x = 0, times=n_year)
    final_vegetable_compost_cost <- rep(x = 0, times=n_year)
    final_vegetable_compost_revenue <- rep(x = 0, times=n_year)
  }
  
  
  
  ## Agrotourism ####
  
  # since there is no particular program from the government for agrotourism
  # we can just consider that the chance event for agrotourism is all under
  # the 2nd scenario, which is without gov assistance.
  # so all the costs will be bear by farmers themselves
  
  
  # Probability of agrotourism 
  
  chance_agrotourism <- chance_event(chance = prob_of_agrotourism, value_if = 1, 
                                     value_if_not = 0) 
  if(chance_agrotourism == 1){
    
    ### Agrotourism for rice field cost ####
    
    # First, we need to calculate the establishment cost for agrotourism
    # by preparing some signs to be installed in the area
    # the cost would be paid only for the first year
    
    # first_year_rice_agrotourism_cost <- sign_installation_cost
    
    # Then, we calculate the annual cost of agrotourism for rice field
    # Which is included for marketing and promotion
    
    annual_rice_agrotourism_cost <- vv(marketing_and_promotion_ecotourism, 
                                       n_year, var_CV=10)
    
    # Calculate the cost with the first year establishment cost
    annual_rice_agrotourism_cost[1] <- annual_rice_agrotourism_cost[1] + 
      sign_installation_cost 
    
    # We consider that there is no risk affect cost of rice agrotourism cost
    # since the cost might be not so high
    
    # Annual income from rice field agrotourism  (based on current condition)
    rice_agrotourism_revenue <- vv(rice_ecotourism_value, n_year, var_CV=CV_value)
    
    
    ## Agrotourism for vegetable field cost ####
    
    # First, we need to calculate the establishment cost for agrotourism
    # by preparing some signs to be installed in the area
    # the cost would be paid only for the first year
    
    # Then, we calculate the annual cost of agrotourism for rice field
    # Which is included for marketing and promotion
    # annual_vegetable_agrotourism_cost <- marketing_and_promotion_ecotourism
    
    annual_vegetable_agrotourism_cost <- vv(marketing_and_promotion_ecotourism, 
                                            n_year, var_CV=10)
    
    # Calculate the cost with the first year establishment cost
    annual_vegetable_agrotourism_cost[1] <- annual_vegetable_agrotourism_cost[1] + 
      sign_installation_cost
    
    # Annual income from vegetables field agrotourism  (based on current condition)
    vegetables_agrotourism_revenue <- vv(vegetable_ecotourism_value, n_year, 
                                         var_CV=10)
  }else{
    rice_agrotourism_cost <- rep(x = 0, times=n_year)
    rice_agrotourism_revenue <- rep(x = 0, times=n_year)
    vegetable_agrotourism_cost <- rep(x = 0, times=n_year)
    vegetables_agrotourism_revenue <- rep(x = 0, times=n_year)
  }
  
  
  # Outcomes ####
  ## 1st scenario outcomes (with gov assistance) ####
  
  ### Rice outcomes #### 
  
  #### Final rice benefits
  final_rice_benefits_with_gov_assistance <- 
    final_rice_farming_revenue_with_gov_assistance + 
    final_rice_compost_revenue +
    rice_agrotourism_revenue + 
    rice_cultural_value
  
  #### Final rice costs 
  final_rice_costs_with_gov_assistance <- 
    final_rice_farming_cost_with_risk_with_gov_assistance + 
    final_rice_compost_cost +
    annual_rice_agrotourism_cost
  
  #### Final result for rice
  final_rice_result_with_gov_assistance <- 
    final_rice_benefits_with_gov_assistance - 
    final_rice_costs_with_gov_assistance
  
  
  ### Vegetables outcomes #### 
  
  #### Final benefits for vegetables
  final_vegetable_benefits_with_gov_assistance <- 
    final_vegetable_farming_revenue + final_vegetable_compost_revenue +
    final_vegetable_processed_revenue + vegetables_agrotourism_revenue + 
    vegetables_nutrition_health_benefit
  
  #### Final costs for vegetables
  final_vegetable_costs_with_gov_assistance <- 
    final_vegetable_farming_cost_with_gov_assistance + 
    final_vegetable_compost_cost +
    final_vegetable_processing_cost_with_gov_assitance + 
    annual_vegetable_agrotourism_cost
  
  #### Final result for vegetables
  final_vegetable_result_with_gov_assistance <- 
    final_vegetable_benefits_with_gov_assistance - 
    final_vegetable_costs_with_gov_assistance
  
  
  ## 2nd scenario outcomes (without gov assistance) ####
  
  ### Rice outcomes #### 
  
  #### Final rice benefits
  final_rice_benefits_without_gov_assistance <- 
    final_rice_farming_revenue_without_gov_assistance + 
    final_rice_compost_revenue +
    rice_agrotourism_revenue + rice_cultural_value
  
  #### Final rice costs
  final_rice_costs_without_gov_assistance <- 
    final_rice_farming_cost_with_risk_without_gov_assistance + 
    final_rice_compost_cost +
    annual_rice_agrotourism_cost
  
  #### Final result for rice
  final_rice_result_without_gov_assistance <- 
    final_rice_benefits_without_gov_assistance - 
    final_rice_costs_without_gov_assistance
  
  
  ### Vegetables outcomes #### 
  
  #### Final benefits for vegetables
  final_vegetable_benefits_without_gov_assistance <- 
    final_vegetable_farming_revenue + final_vegetable_compost_revenue +
    final_vegetable_processed_revenue + vegetables_agrotourism_revenue + 
    vegetables_nutrition_health_benefit
  
  #### Final costs for vegetables
  final_vegetable_costs_without_gov_assistance <- 
    final_vegetable_farming_cost_without_gov_assistance + 
    final_vegetable_compost_cost +
    final_vegetable_processing_cost_without_gov_assitance + 
    annual_vegetable_agrotourism_cost
  
  #### Final result for vegetables
  final_vegetable_result_without_gov_assistance <- 
    final_vegetable_benefits_without_gov_assistance - 
    final_vegetable_costs_without_gov_assistance
  
  
  # Calculate NPV ####
  
  ## NPV for 1st scenario (with gov assistance) ####
  
  NPV_rice_with_gov_assistance <- 
    discount(x = final_rice_result_with_gov_assistance,
             discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_vegetables_with_gov_assistance <- 
    discount(x = final_vegetable_result_with_gov_assistance,
             discount_rate = discount_rate,
             calculate_NPV = TRUE)
  
  NPV_vegetable_decision_with_gov_assistance <- 
    discount(x = (final_vegetable_result_with_gov_assistance - 
                    final_rice_result_with_gov_assistance),
             discount_rate = discount_rate,
             calculate_NPV = TRUE)
  
  ## NPV for 2nd scenario (without gov assistance) ####
  
  NPV_rice_without_gov_assistance <- 
    discount(x = final_rice_result_without_gov_assistance,
             discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_vegetables_without_gov_assistance <- 
    discount(x = final_vegetable_result_without_gov_assistance,
             discount_rate = discount_rate,
             calculate_NPV = TRUE)
  
  NPV_vegetable_decision_without_gov_assistance <- 
    discount(x = (final_vegetable_result_without_gov_assistance - 
                    final_rice_result_without_gov_assistance),
             discount_rate = discount_rate,
             calculate_NPV = TRUE)
  
  
  
  # Return list ####
  
  return(list(NPV_vegetables_decision_do_with_gov_assistance = NPV_vegetable_decision_with_gov_assistance,
              NPV_vegetables_decision_do_without_gov_assistance = NPV_vegetable_decision_without_gov_assistance,
              cashflow_vegetables_with_gov_assistance = final_vegetable_result_with_gov_assistance,
              cashflow_vegetables_without_gov_assistance = final_vegetable_result_without_gov_assistance))
}


# Monte Carlo Simulation ####

# Run the Monte Carlo simulation using the model function #
TRV_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                  model_function = transition_rice_to_vegetables,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")


# NPV distribution #####

# plot NPV distribution for rice and vegetables
NPV_vegetable_decision_boxplot <- 
  plot_distributions(mcSimulation_object = TRV_mc_simulation,
                     vars = c("NPV_vegetables_decision_do_with_gov_assistance",
                              "NPV_vegetables_decision_do_without_gov_assistance"),
                     y_axis_name = "",
                     color = c("lightgreen", "lightblue"),
                     method = 'boxplot',
                     base_size = 15)+
  labs(title = "NPV decisions")

# simple overlay
NPV_vegetable_decision <- 
  plot_distributions(mcSimulation_object = TRV_mc_simulation,
                     vars = c("NPV_vegetables_decision_do_with_gov_assistance",
                              "NPV_vegetables_decision_do_without_gov_assistance"),
                     y_axis_name = "",
                     color = c("lightgreen", "lightblue"),
                     base_size = 15)+
  labs(title = "NPV decisions")


# summary of statistics
simulation_results <- data.frame(TRV_mc_simulation$y)
selected_data <- 
  simulation_results[, c("NPV_vegetables_decision_do_with_gov_assistance",
                         "NPV_vegetables_decision_do_without_gov_assistance")]

summary_stats <- selected_data %>%
  summarise(across(everything(), list(
    Min = min,
    Q25 = ~ quantile(.x, 0.25),
    Median = median,
    Mean = mean,
    Q75 = ~ quantile(.x, 0.75),
    Max = max
  )))

str(summary_stats)

write.csv(summary_stats, "summary_statistics.csv", row.names = FALSE)



# Cashflow analysis ####

# Plot cashflow vegetables
cashflow_vegetables_with_gov_assistance <- 
  plot_cashflow(mcSimulation_object = TRV_mc_simulation,
                cashflow_var_name = "cashflow_vegetables_with_gov_assistance",
                x_axis_name = "Years with intervention",
                y_axis_name = "Annual cashflow in USD",
                color_25_75 = "lightgreen", color_5_95 = "grey",
                color_median = "blue", base_size = 15)

cashflow_vegetables_without_gov_assistance <- 
  plot_cashflow(mcSimulation_object = TRV_mc_simulation,
                cashflow_var_name = "cashflow_vegetables_with_gov_assistance",
                x_axis_name = "Years with intervention",
                y_axis_name = "Annual cashflow in USD",
                color_25_75 = "lightblue", color_5_95 = "grey",
                color_median = "blue", base_size = 15)


# Projection to Latent Structures (PLS) analysis ####

# Plot PLS with government assistance
pls_result_vegetables_with_gov_assistance <- 
  plsr.mcSimulation(object = TRV_mc_simulation,
                    resultName = names(TRV_mc_simulation$y)[1], ncomp = 1)

plot_pls_vegetables_with_gov_assistance <- 
  plot_pls(pls_result_vegetables_with_gov_assistance,
           threshold = 1.5, base_size = 12,
           pos_color = "lightgreen") +
  labs(title = "With Government Assistance", size = 6)+
  scale_y_discrete(position = "right") + # Move the y-axis to the right
  scale_x_reverse() + 
  theme(
    axis.title.y = element_text(hjust = 0.5),  # Ensure the y-axis title is centered
    axis.text.y = element_blank(),             # Hide the y-axis labels (variable names)
    axis.title.x = element_blank()             # Optionally remove x-axis title if not needed
  )


# Plot PLS without government assistance
pls_result_vegetables_without_gov_assistance <- 
  plsr.mcSimulation(object = TRV_mc_simulation,
                    resultName = names(TRV_mc_simulation$y)[2], ncomp = 1)

plot_pls_vegetables_without_gov_assistance <- 
  plot_pls(pls_result_vegetables_without_gov_assistance,
           threshold = 1.5, base_size = 12,
           pos_color = "lightblue") +
  labs(title = "Without Government Assistance", size = 5)+
  scale_y_discrete(
    position = "left", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) +
  theme(
    axis.title.y = element_text(hjust = 0.5),  # Center the y-axis title
    axis.text.y = element_text(hjust = 0.5),   # Center the y-axis labels (variable names)
    axis.title.x = element_blank())+             # Optionally remove x-axis title
  theme(legend.title=element_blank())

# Combine the plots
combined_plot_pls <- plot_pls_vegetables_with_gov_assistance + 
  plot_pls_vegetables_without_gov_assistance +
  plot_layout(ncol = 2)


# VoI analysis ####
mcSimulation_table <- data.frame(TRV_mc_simulation$x, TRV_mc_simulation$y[1:2])
evpi_TRV <- multi_EVPI(mc = mcSimulation_table, 
                       first_out_var = "NPV_vegetables_decision_do_with_gov_assistance", 
                       write_table = TRUE)

# Plot EVPI

plot_evpi_vegetable_decision  <- 
  plot_evpi(evpi_TRV,
            decision_vars = c("NPV_vegetables_decision_do_with_gov_assistance",
                              "NPV_vegetables_decision_do_without_gov_assistance"),
            new_names = c("NPV vegetables decision with government assistance",
                          "NPV vegetables decision without government assistance"),
            bar_color = c("cadetblue", "blue3"),
            base_size = 13)+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 20, face = "bold"))+
  theme(legend.position = "bottom")+
  theme(plot.title = element_text(size = 15, face = "bold"))+ # Set title
  coord_flip()
labs(x = "EVPI (USD)", y = "Variable") +
  ggtitle(c("NPV vegetables decision with and without government assistance"))



# Subset the outputs from the mcSimulation function (y) to summarize only on the variables that we want.
# names(TRV_mc_simulation$x)
mcSimulation_summary <- data.frame(TRV_mc_simulation$x[5:84],
                                   # names(TRV_mc_simulation$x)
                                   TRV_mc_simulation$y[3])

gtExtras::gt_plt_summary(mcSimulation_summary)
