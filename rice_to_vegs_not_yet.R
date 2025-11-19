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

input_estimates <- read.csv("input_rice_to_vegs_idr_new.csv", sep = ";")

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
  
  # 1. Rice #1 ####
  
  ## Scenario 1: If farmers receive assistance from the government ####
  
  ### > Rice farming costs #1 ####
  
  # Annual rice farming costs under normal conditions
  
  # Assuming rice is cultivated twice per year
  rice_farming_cost_with_gov_assistance <- 
    vv((rice_farming_input_costs_with_gov_assistance +
          rice_machinery_costs_with_gov_assistance +
          irrigation_maintenance_costs + 
          rice_labor_costs), # Normally, farmers do not pay for labor costs in cash. 
       # Instead, they share their harvest with those who help them during 
       # cultivation. Therefore, rice labor costs are estimated based on the 
       # amount of harvest that they will share with those who assist them.
       n_year, 
       var_CV=CV_value,
       relative_trend = inflation_rate)
  
  
  ## Scenario 2: if farmers don't get assistance from the government ####
  
  ### > Rice farming costs #2 ####
  
  # Annual rice farming cost under normal condition
  # Assuming rice is cultivated twice per year
  rice_farming_cost_without_gov_assistance <- 
    vv((rice_farming_input_costs + 
          rice_machinery_costs + 
          irrigation_maintenance_costs +
          rice_labor_costs), #normally farmers don't pay for labor cost in cash, instead 
       # they share their harvest to those who help them during the cultivation. 
       # So, rice labor costs is estimated based on how much the amount of harvest 
       # that they will share with those who help them.
       n_year, 
       var_CV=CV_value,
       relative_trend = inflation_rate)
  
  
  #### -> Rice risks (farming cost) #1 ####
  
  # Considering production risks (pests, diseases, heavy rainfall, water 
  # shortages, etc.), farmers need to pay higher costs for production 
  # management risks.
  
  # Event 1: Farmers need to buy more pesticides to manage pest and disease 
  # outbreaks
  
  chance_risk_rice_farming_high_input_cost <- 
    chance_event(chance = chance_production_risk, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_risk_rice_farming_high_input_cost == 1){
    
    # with government assistance
    rice_farming_input_costs_with_more_pesticides_with_gov_assistance <- 
      vv((rice_farming_input_costs_with_gov_assistance * # farming input cost under normal condition
            (1+portion_rice_farming_input_cost_for_pest_disease_management)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    # without government assistance
    rice_farming_input_costs_with_more_pesticides_without_gov_assistance <- 
      vv((rice_farming_input_costs * 
            (1+portion_rice_farming_input_cost_for_pest_disease_management)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
  }else{
    rice_farming_input_costs_with_more_pesticides_with_gov_assistance <- 
      vv(rice_farming_input_costs_with_gov_assistance,
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    rice_farming_input_costs_with_more_pesticides_without_gov_assistance <-
      vv(rice_farming_input_costs, n_year, var_CV=CV_value, 
         relative_trend = inflation_rate)
  }
  
  
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
  
  if(chance_risk_rice_farming_high_input_cost_with_bank_loan == 1){
    
    rice_farming_high_input_cost_with_bank_loan_with_gov_assistance <- 
      vv(((rice_farming_input_costs_with_more_pesticides_with_gov_assistance +
             rice_machinery_costs_with_gov_assistance + 
             rice_labor_costs) * annual_bank_interest), n_year, var_CV=CV_value,
         relative_trend = inflation_rate)
    
    rice_farming_cost_with_bank_loan_with_gov_assistance <-
      vv(((rice_farming_input_costs_with_more_pesticides_with_gov_assistance +
             rice_machinery_costs_with_gov_assistance + 
             rice_labor_costs) + 
            rice_farming_high_input_cost_with_bank_loan_with_gov_assistance),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    
  }else{
    rice_farming_high_input_cost_with_bank_loan_with_gov_assistance <- 
      rep(x = 0, times = n_year)
    rice_farming_cost_with_bank_loan_with_gov_assistance <- 
      rice_farming_input_costs_with_more_pesticides_with_gov_assistance
  }
  
  
  # Consider institutional risk to cost with government assistance
  chance_institutional_risk_rice_with_gov_assistance <- 
    chance_event(chance = chance_institutional_risk, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_institutional_risk_rice_with_gov_assistance == 1){
    
    # Farming input costs may increase due to institutional risks
    rice_farming_input_costs_with_institutional_risk <- 
      rice_farming_cost_with_bank_loan_with_gov_assistance +
      (rice_farming_cost_with_bank_loan_with_gov_assistance *
         prob_damage_institutional_risk_rice)
    
  }else{
    rice_farming_input_costs_with_institutional_risk <- 
      rice_farming_cost_with_bank_loan_with_gov_assistance
  }
  
  
  # annual rice farming cost after considering production, financial, and
  # institutional risks with government assistance
  final_rice_farming_cost_with_risk_with_gov_assistance <- vv((
    rice_farming_input_costs_with_institutional_risk +
      rice_machinery_costs_with_gov_assistance + 
      irrigation_maintenance_costs +
      rice_labor_costs), n_year, var_CV=CV_value,
    relative_trend = inflation_rate)
  
  # annual rice farming cost after considering production, financial, and
  # institutional risks without government assistance
  final_rice_farming_cost_with_risk_without_gov_assistance <- vv((
    rice_farming_input_costs_with_more_pesticides_without_gov_assistance +
      rice_machinery_costs_with_gov_assistance + 
      irrigation_maintenance_costs +
      rice_labor_costs), n_year, var_CV=CV_value,
    relative_trend = inflation_rate)
  
  
  
  ###  Rice benefits #1 ####
  
  #### Rice farming #1 ####
  
  # Annual rice farming income under normal condition
  # Assuming rice is cultivated twice per year
  rice_yield_vv <- vv(rice_yield, n_year, var_CV=CV_value)
  rice_farmgate_price_vv <- vv(rice_farmgate_price, n_year, var_CV=CV_value, 
                               relative_trend = inflation_rate)
  
  rice_farming_revenue <-  rice_yield_vv * rice_farmgate_price_vv
  
  ##### -> Rice risks (farming benefits) #1 ####
  
  # Considering the impact of production risks on rice yield,
  # assuming there are risks such as pests, diseases, heavy rainfall, storms, 
  # and water shortages, which can cause losses to rice yield. A reduction in 
  # rice capital/cost may also lead to a decrease in yield.
  
  chance_production_risk_to_rice_yield <- 
    chance_event(chance = chance_production_risk, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_production_risk_to_rice_yield == 1){  
    
    # Event 1: Rice yield reduction due to production risks.
    rice_yield_loss_with_production_risk <- 
      vv((rice_yield * 
            (prob_damage_production_risk_rice + 
               prob_damage_market_risk_rice)),
         n_year, var_CV=CV_value)
    
    rice_yield_with_production_risk <- 
      vv(rice_yield - rice_yield_loss_with_production_risk,
         n_year, var_CV=CV_value)
    
  }else{
    rice_yield_loss_with_production_risk <- rep(x = 0, times = n_year)
    rice_yield_with_production_risk <- vv(rice_yield, n_year, var_CV=CV_value)
  }
  
  
  # Considering market risks and their impact on rice prices,
  # which can cause a reduction in farmers' revenue.
  
  chance_market_risk_rice_benefits_with_gov_assistance <- 
    chance_event(chance = chance_market_risk_rice, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_market_risk_rice_benefits_with_gov_assistance == 1){    
    
    # Event 2: Rice revenue reduction due to market risks under normal conditions 
    # (without natural risks).
    
    rice_farmgate_price_with_market_risk <- 
      rice_farmgate_price_vv -
      (rice_farmgate_price_vv * prob_damage_market_risk_rice)
    
    rice_farming_revenue_with_market_risk <- 
      rice_farmgate_price_with_market_risk * rice_yield_vv
    
  }else{
    rice_farmgate_price_with_market_risk <- rep(x = 0, times = n_year)
    rice_farming_revenue_with_market_risk <- rice_farming_revenue
  }
  
  
  
  # Event 3: Rice revenue reduction due to market risks under production and 
  # financial risks.
  
  chance_market_risk_with_production_and_financial_risk_rice <- 
    chance_event(chance = chance_production_risk + chance_market_risk_rice, 
                 value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_market_risk_with_production_and_financial_risk_rice == 1){    
    
    # Annual rice farming revenue after considering all risks
    final_rice_farming_revenue_with_risks <- rice_farmgate_price_with_market_risk * 
      rice_yield_with_production_risk
    
  }else{
    # rice_farming_revenue_loss_with_market_production_financial_risk <- rep(x = 0, times = n_year)
    final_rice_farming_revenue_with_risks <- rice_farming_revenue
  }
  
  
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
  
  # rice_cultural_value_with_gov_assistance <-  
  #  rice_cultural_value - rice_farming_cost_with_gov_assistance
  # rice cultural value is subtracted from the rice farming cost
  
  # rice cultural value
  # this is intangible variable
  # that we consider that there is a value from rice cultural value itself
  # that makes farmers persist to grow rice despite of the high profit
  # from vegetable farming
  
  # so there are 4 proxies in calculating this:
  # 1) Quality / satisfaction
  # this is associated to the perception by rice farmers who thinks that by growing
  # their own rice, they can keep the quality of their rice consumption especially
  # in terms of taste. and they also feel more satisfy with their own rice
  # so this can be measured by estimating the difference between common rice 
  # price in the region and the most premium/expensive rice in Indonesia
  # let's say, the difference would be IDR 10k - 20k (USD 1-2) per kg
  # most expensive rice is IDR 35k/kg that known for its taste
  
  # rice_cultural_value_proxy_quality <- 
  #   vv(rice_cultural_value_proxy_quality_and_satisfaction * rice_yield, 
  #      n_year, var_CV=CV_value, relative_trend = inflation_rate)
  
  # 2) Local identity / pride
  # this is associated to how those rice farmers keep trying to preserving their
  # local heritage (?) from their ancestor by keep growing rice in the area
  # so this can be measured by assuming what if there is no more rice in the 
  # region so then farmers need to pay more for buying rice.
  # so we can estimating this by use the difference between the current higher
  # price in the area and the most common rice price in the area
  # or just estimating the rice price in outside region with transportation costs
  
  # rice_cultural_value_proxy_local_identity <- 
  #   vv(rice_cultural_value_proxy_local_identity * rice_yield, 
  #      n_year, var_CV=CV_value, relative_trend = inflation_rate)
  
  
  # 3) Local cultural practices
  # this is associated with the local practices that solely implemented on rice
  # farming, but not in vegetable farming.
  # a) tudang sipulung
  # rice farmers in this region typically conduct a 'gathering' to discuss about
  # the planning for rice farming in each season, including for the field 
  # preparation, planting, management, and until harvest.
  # with this practice, farmers perceive that there are more benefits for them
  # as they can be more efficiently manage their rice farming in each season
  # so that they can avoid a big issue that might be comes from pests and disease
  # so we can estimate this like by thinking there will be lower costs for crop 
  # protection, let's say maybe around 5-10% decrease.
  
  # but the
  
  # with government assistance
  
  # calculate farming costs associated with cultural value
  
  rice_tudang_sipulung_with_gov_assistance <- 
    vv((rice_cultural_value_proxy_local_cultural_practice_tudang_sipulung * 
          (rice_farming_input_costs_with_gov_assistance + 
             rice_machinery_costs_with_gov_assistance + irrigation_maintenance_costs)),
       n_year, 
       var_CV=CV_value, 
       relative_trend = inflation_rate)
  
  
  # without government assistance
  
  rice_tudang_sipulung_without_gov_assistance <- 
    vv((rice_cultural_value_proxy_local_cultural_practice_tudang_sipulung * 
          (rice_farming_input_costs + 
             rice_machinery_costs + irrigation_maintenance_costs)),
       n_year, 
       var_CV=CV_value, 
       relative_trend = inflation_rate)
  
  
  # b) gotong royong
  # rice farmers in this region also typically conduct 'communal' labor where
  # they help each other to manage and harvest their fields alternately
  # sometime, they barter their harvest or like pay the labor by the amount of 
  # harvest, which is calculated based on how much they harvest at that time
  # so we can estimate this by assuming there will be lower costs for labor,
  # let's say maybe around 10-35% decrease
  
  # both with or without government assistance
  rice_gotong_royong_value <- 
    vv((rice_cultural_value_proxy_local_cultural_practice_gotong_royong *
          rice_labor_costs), n_year, 
       var_CV=CV_value, 
       relative_trend = inflation_rate)
  
  
  
  # so the rice cultural value will be calculated as
  
  # with government assistance
  rice_cultural_value_with_gov_assistance <- 
    # rice_cultural_value_proxy_quality +
    # rice_cultural_value_proxy_local_identity +
    rice_tudang_sipulung_with_gov_assistance +
    rice_gotong_royong_value
  
  # without government assistance
  rice_cultural_value_without_gov_assistance <- 
    # rice_cultural_value_proxy_quality +
    # rice_cultural_value_proxy_local_identity +
    rice_tudang_sipulung_without_gov_assistance +
    rice_gotong_royong_value
  
  
  ## 2. Vegetable #1 ####
  
  ### Vegetable farming cost #1 ####
  
  # Annual vegetable farming costs under normal conditions
  # Assuming farmers grow vegetables with mixed cropping (6 crops per year)
  
  ## Annual vegetable costs (for all seasons throughout the year)
  vegetables_farming_cost_with_gov_assistance <- 
    vv((vegetables_farming_input_costs_with_gov_assistance + 
          vegetables_machinery_costs_with_gov_assistance + 
          vegetables_labor_costs), 
       n_year, var_CV = 10, relative_trend = inflation_rate)
  
  
  # Calculating the investment costs of growing vegetables.
  # Costs in the first year are higher than in subsequent years,
  # as farmers need to purchase more equipment and irrigation tools
  # for growing vegetables.
  vegetables_farming_cost_with_gov_assistance[1]  <- 
    vegetables_farming_cost_with_gov_assistance[1] + 
    vegetables_equipment_costs_first_year_with_gov_assistance + 
    vegetables_irrigation_costs_first_year_with_gov_assistance
  
  ### Vegetable farming cost #2 ####
  
  ## annual vegetable costs (all season throughout the year) without gov assistance
  vegetables_farming_cost_without_gov_assistance <- 
    vv((vegetables_farming_input_costs + 
          vegetables_machinery_costs + 
          vegetables_labor_costs), 
       n_year, var_CV=10, relative_trend = inflation_rate)
  
  # calculating the implementation costs of growing vegetables 
  # costs in the first year is higher than following other years
  # considering farmers need to purchase more equipment and irrigation tools
  # for growing vegetables
  vegetables_farming_cost_without_gov_assistance[1]  <- 
    vegetables_farming_cost_without_gov_assistance[1] + 
    vegetables_equipment_costs_first_year + 
    vegetables_irrigation_costs_first_year
  
  
  #### -> Vegetable risks (costs) #1 ####
  
  # Considering production risks (pests, diseases, heavy rainfall, water 
  # shortage, etc.), farmers need to pay higher costs for production management 
  # risk.
  
  # Event 1: Farmers need to buy more pesticides to overcome pest and disease 
  # outbreaks.
  
  chance_risk_vegetable_farming_high_input_cost <- 
    chance_event(chance = chance_production_risk, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_risk_vegetable_farming_high_input_cost == 1){
    
    # with government assistance
    vegetable_farming_input_costs_with_more_pesticides_with_gov_assistance <-
      vv((vegetables_farming_input_costs_with_gov_assistance *
            # farming input cost under normal condition
            (1 + portion_vegetable_farming_input_cost_for_pest_disease_management)),
         n_year, var_CV = CV_value, relative_trend = inflation_rate)
    
    # vegetable farming cost with gov assistance if need more pesticide
    # (since the farming is without gov assistance, the relevant risk to 
    # vegetable farming costs is related to production costs only )
    vegetable_farming_cost_with_high_input_with_gov_assistance <- 
      vv((vegetable_farming_input_costs_with_more_pesticides_with_gov_assistance +
            vegetables_machinery_costs_with_gov_assistance + 
            vegetables_labor_costs), 
         n_year, var_CV=10, relative_trend = inflation_rate)
    
    vegetable_farming_cost_with_high_input_with_gov_assistance[1] <- 
      vegetable_farming_cost_with_high_input_with_gov_assistance[1] + 
      vegetables_equipment_costs_first_year + 
      vegetables_irrigation_costs_first_year
    
    # without government assistance
    # final vegetable farming cost without gov assistance
    # (since the farming is without gov assistance, the relevant risk to 
    # vegetable farming costs is related to production costs only )
    vegetable_farming_input_costs_with_more_pesticides_without_gov_assistance <-
      vv((vegetables_farming_input_costs *
            # farming input cost under normal condition
            (1 + portion_vegetable_farming_input_cost_for_pest_disease_management)),
         n_year, var_CV = CV_value, relative_trend = inflation_rate)
    
    # final vegetable farming cost without gov assistance
    # (since the farming is without gov assistance, the relevant risk to 
    # vegetable farming costs is related to production costs only )
    final_vegetable_farming_cost_without_gov_assistance <- 
      vv((vegetable_farming_input_costs_with_more_pesticides_without_gov_assistance +
            vegetables_machinery_costs + 
            vegetables_labor_costs), 
         n_year, var_CV=10, relative_trend = inflation_rate)
    
    final_vegetable_farming_cost_without_gov_assistance[1] <- 
      final_vegetable_farming_cost_without_gov_assistance[1] + 
      vegetables_equipment_costs_first_year + 
      vegetables_irrigation_costs_first_year
    
    
  }else{
    vegetable_farming_input_costs_with_more_pesticides_with_gov_assistance <- 
      vv(vegetables_farming_input_costs_with_gov_assistance, , n=n_year, 
         var_CV = CV_value, relative_trend = inflation_rate)
    vegetable_farming_cost_with_high_input_with_gov_assistance <- 
      vegetables_farming_cost_with_gov_assistance
    
    vegetable_farming_input_costs_with_more_pesticides_without_gov_assistance <- 
      vv(vegetables_farming_input_costs, n=n_year, var_CV = CV_value, 
         relative_trend = inflation_rate)
    final_vegetable_farming_cost_without_gov_assistance <- 
      vegetables_farming_cost_without_gov_assistance
  }
  
  
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
  
  if(chance_risk_vegetable_farming_high_input_cost_with_bank_loan == 1){
    
    # with government assistance
    vegetable_farming_high_input_cost_with_bank_loan_with_gov_assistance <- 
      vv((vegetable_farming_input_costs_with_more_pesticides_with_gov_assistance +
            (vegetable_farming_input_costs_with_more_pesticides_with_gov_assistance +
               vegetables_machinery_costs_with_gov_assistance + 
               vegetables_labor_costs) * annual_bank_interest),
         n_year, var_CV=10, relative_trend = inflation_rate)
    
    # without government assistance # no bank loan
    # vegetable_farming_high_input_cost_without_bank_loan <- 
    #   vv(((vegetable_farming_input_costs_with_more_pesticides_without_gov_assistance +
    #          vegetables_machinery_costs + vegetables_labor_costs) * 
    #         annual_bank_interest),
    #      n_year, var_CV=10, relative_trend = inflation_rate)
    
  }else{
    vegetable_farming_high_input_cost_with_bank_loan_with_gov_assistance <- 
      vegetable_farming_input_costs_with_more_pesticides_with_gov_assistance
    # vegetable_farming_high_input_cost_without_bank_loan <- 
    #   vegetable_farming_input_costs_with_more_pesticides_without_gov_assistance
  }
  
  
  # Consider institutional risk to cost with government asssitance
  chance_institutional_vegetable_with_gov_assistance <- 
    chance_event(chance = chance_institutional_risk, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_institutional_vegetable_with_gov_assistance == 1){
    
    # Farming input costs may increase due to institutional risks
    vegetable_farming_input_costs_with_institutional_and_other_risks <- 
      vegetable_farming_high_input_cost_with_bank_loan_with_gov_assistance +
      (vegetable_farming_high_input_cost_with_bank_loan_with_gov_assistance *
         prob_damage_institutional_risk_vegetables)
    
  }else{
    vegetable_farming_input_costs_with_institutional_and_other_risks <- 
      vegetable_farming_high_input_cost_with_bank_loan_with_gov_assistance
  }
  
  
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
  
  # annual_final_vegetable_farming_cost_with_gov_assistance <- 
  #   vegetable_farming_input_costs_with_institutional_risk 
  # # * 2.5 # effective field size
  # 
  # final_vegetable_farming_cost_with_gov_assistance <- 
  #   vv(annual_final_vegetable_farming_cost_with_gov_assistance, 
  #      n_year, var_CV=CV_value, relative_trend = inflation_rate)
  
  
  # Consider if all risks happen together (?)
  chance_vegetable_costs_with_risks_with_gov_assistance <- 
    chance_event(chance = 0.01, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_vegetable_costs_with_risks_with_gov_assistance == 1){
    
    final_vegetable_farming_cost_with_risks_with_gov_assistance <- 
      vv((vegetable_farming_input_costs_with_institutional_and_other_risks +
            vegetables_machinery_costs_with_gov_assistance + 
            vegetables_labor_costs ), 
         n_year, var_CV=10, relative_trend = inflation_rate)
    
    final_vegetable_farming_cost_with_risks_with_gov_assistance[1] <- 
      final_vegetable_farming_cost_with_risks_with_gov_assistance[1] + 
      vegetables_equipment_costs_first_year_with_gov_assistance + 
      vegetables_irrigation_costs_first_year_with_gov_assistance
    
  }else{
    final_vegetable_farming_cost_with_risks_with_gov_assistance <- 
      vegetables_farming_cost_with_gov_assistance
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
  
  # since the crop yield unit is kg/ha/year, it's needed to calculate the yield
  # for per season.
  # assuming that the crops in 'Field 1' is normally can be cultivated 3 times
  # per year. So, the crop yield should be divided into 3
  
  chinese_mustard_green_yield_per_season <- vv(((chinese_mustard_green_yield/3)
                                                * 0.3), n_year, var_CV=CV_value)
  
  green_bean_yield_per_season <- vv(((green_bean_yield/3) * 0.3), 
                                    n_year, var_CV=CV_value)
  
  cabbage_yield_per_season <- vv(((cabbage_yield/3) * 0.3), 
                                 n_year, var_CV=CV_value)
  
  spring_onion_yield_per_season_field1 <- vv(((spring_onion_yield/3) * 0.6), 
                                             n_year, var_CV=CV_value)
  
  
  # calculate vegetable price with vv function
  
  chinese_mustard_green_price_vv <- vv(chinese_mustard_green_price,
                                       n_year, var_CV=40, 
                                       relative_trend = inflation_rate)
  
  green_bean_price_vv <- vv(green_bean_price,
                            n_year, var_CV=40, 
                            relative_trend = inflation_rate)
  
  cabbage_price_vv <- vv(cabbage_price,
                         n_year, var_CV=40, 
                         relative_trend = inflation_rate)
  
  spring_onion_price_vv <- vv(spring_onion_price,
                              n_year, var_CV=40, 
                              relative_trend = inflation_rate)
  
  
  # For Field 1, the proportion of the field used for the main crops is 0.3 ha, 
  # while the side crop uses 0.2 ha per season (0.6ha per year)
  # so annually, the total field size used from 'Field 1' for main crops
  
  chinese_mustard_green_revenue <- chinese_mustard_green_yield_per_season *
    chinese_mustard_green_price_vv
  # farmgate price chinese_mustard_green_price per kg at the farm gate
  
  green_bean_revenue <- green_bean_yield_per_season * green_bean_price_vv
  # farmgate price green_bean_price per kg at the farm gate
  
  cabbage_revenue <- cabbage_yield_per_season * cabbage_price_vv
  # farmgate price cabbage_price per kg at the farm gate
  
  spring_onion_revenue_1 <- spring_onion_yield_per_season_field1 * 
    spring_onion_price_vv
  # farmgate price spring_onion_price per kg at the farm gate
  
  
  # total revenue from 'Field 1'
  total_revenue_field_1 <- chinese_mustard_green_revenue + green_bean_revenue +
    cabbage_revenue + spring_onion_revenue_1
  
  
  # Calculating revenue from 'Field 2'
  
  # since the crop yield unit is kg/ha/year, it's needed to calculate the yield
  # for per season.
  # assuming that the crops in 'Field 1' is normally can be cultivated 2 times
  # per year. So, the crop yield should be divided into 2
  
  chili_yield_per_season <- vv((chili_yield/2), n_year, var_CV=CV_value)
  tomato_yield_per_season <- vv((tomato_yield/2), n_year, var_CV=CV_value)
  spring_onion_yield_per_season_field2 <- vv(((spring_onion_yield/3) * 0.2), 
                                             n_year, var_CV=CV_value)
  
  # For Field 2, the proportion of the field used for the main crops is 0.4 ha,
  # while the side crop uses 0.1 ha per season (0.2 ha per year)
  
  # chili
  chili_price_vv <- vv(chili_price, n_year, var_CV=50,
                       relative_trend = inflation_rate)
  #CV is quite high because the chili price tend to be very fluctuate
  # farmgate price chili_price per kg at the farm gate
  
  chili_revenue <- (chili_yield_per_season * chili_price_vv) * 0.4
  
  # tomato
  tomato_price_vv <- vv(tomato_price, n_year, var_CV=CV_value,
                        relative_trend = inflation_rate)
  
  tomato_revenue <- (tomato_yield_per_season * tomato_price_vv) * 0.4
  # farmgate price tomato_price per kg at the farm gate
  
  spring_onion_revenue_2 <- (spring_onion_yield_per_season_field2 *
                               spring_onion_price_vv) * 0.1
  # farmgate price spring_onion_price per kg at the farm gate
  
  # total revenue from 'Field 2'
  total_revenue_field_2 <- chili_revenue + tomato_revenue + spring_onion_revenue_2
  
  # total revenue from all vegetable fields
  # total_revenue_vegetables <- total_revenue_field_1 + total_revenue_field_2
  
  total_revenue_vegetables <- total_revenue_field_1 + total_revenue_field_2
  
  # we can just assume that the total revenue for both scenarios are the same
  # except for the income, they are different because of the costs differ 
  # depends on how much the support from government to farmers
  
  # calculate the average farmgate price of all vegetables
  # average_vegetable_farmgate_prices <- mean(chinese_mustard_green_price, 
  #                                           green_bean_price,
  #                                           cabbage_price,
  #                                           spring_onion_price,
  #                                           chili_price,
  #                                           tomato_price)
  # 
  # average_vegetable_farmgate_prices_vv <- vv(average_vegetable_farmgate_prices,
  #                                            n_year, var_CV=40)
  # 
  
  ###### ->> Vegetable risks (farming benefits) ####
  
  ## chance event on vegetable farming benefits ##
  
  # considering production, financial, and market risk damage on vegetable yield
  
  # considering production risk damage on vegetable yield
  # assuming there are some risks like pest, disease, heavy rainfall, storm, 
  # and water shortage causing some lose to yield and reduction to vegetable
  # capital/cost may also reducing yield
  
  ##### >>> Event-1 risks on vegetables yield ####
  
  ##### >>>> Event-1a production risks ####
  
  # vegetable yield reduction from production risks
  
  # calculate the yield of each vegetables with production risks
  
  chance_production_risk_vegetables_yield <- 
    chance_event(chance = chance_production_risk, value_if = 1, 
                 value_if_not = 0)
  
  if(chance_production_risk_vegetables_yield == 1)
  {
    
    ## chinese mustard green with production risk
    chinese_mustard_green_yield_loss_with_production_risk <- 
      vv((chinese_mustard_green_yield_per_season * 
            prob_damage_production_risk_vegetables),
         n_year, var_CV=CV_value)
    
    chinese_mustard_green_yield_with_production_risk <- 
      vv((chinese_mustard_green_yield_per_season - 
            chinese_mustard_green_yield_loss_with_production_risk) * 0.3, #consider size of field
         n_year, var_CV=CV_value)
    
    ## green bean with production risk
    green_bean_yield_loss_with_production_risk <- 
      vv((green_bean_yield_per_season * 
            prob_damage_production_risk_vegetables),
         n_year, var_CV=CV_value)
    
    green_bean_yield_with_production_risk <- 
      vv((green_bean_yield_per_season - 
            green_bean_yield_loss_with_production_risk) * 0.3, #consider size of field
         n_year, var_CV=CV_value)
    
    ## cabbage with production risk
    cabbage_yield_loss_with_production_risk <- 
      vv((cabbage_yield_per_season * 
            prob_damage_production_risk_vegetables),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    cabbage_yield_with_production_risk <- 
      vv((cabbage_yield_per_season - 
            cabbage_yield_loss_with_production_risk) * 0.3, #consider size of field
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    ## spring onion with production risk
    spring_onion_yield_loss_with_production_risk_1 <- 
      vv((spring_onion_yield_per_season_field1 * 
            prob_damage_production_risk_vegetables),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    spring_onion_yield_with_production_risk_1 <- 
      vv((spring_onion_yield_per_season_field1 - 
            spring_onion_yield_loss_with_production_risk_1) * 0.6, #consider size of field
         n_year, var_CV=CV_value, relative_trend = inflation_rate) 
    
    # calculate the annual total yield of vegetables for production risks
    # from Field-1
    annual_total_yield_vegetables_with_financial_risk_field1 <- 
      chinese_mustard_green_yield_with_production_risk +
      green_bean_yield_with_production_risk +
      cabbage_yield_with_production_risk +
      spring_onion_yield_with_production_risk_1 
    
    
    # Field-2
    
    ## chili with production risk
    chili_yield_loss_with_production_risk <- 
      vv((chili_yield_per_season * 
            prob_damage_production_risk_vegetables),
         n_year, var_CV=CV_value)
    
    chili_yield_with_production_risk <- 
      vv((chili_yield_per_season - 
            chili_yield_loss_with_production_risk) * 0.4, #consider size of field,
         n_year, var_CV=CV_value)
    
    ## tomato with production risk
    tomato_yield_loss_with_production_risk <- 
      vv((tomato_yield_per_season * 
            prob_damage_production_risk_vegetables),
         n_year, var_CV=CV_value)
    
    tomato_yield_with_production_risk <- 
      vv((tomato_yield_per_season - 
            tomato_yield_loss_with_production_risk) * 0.4, #consider size of field,
         n_year, var_CV=CV_value)
    
    
    ## spring onion with production risk
    spring_onion_yield_loss_with_production_risk_2 <- 
      vv((spring_onion_yield_per_season_field2 * 
            prob_damage_production_risk_vegetables),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    spring_onion_yield_with_production_risk_2 <- 
      vv((spring_onion_yield_per_season_field2 - 
            spring_onion_yield_loss_with_production_risk_2) * 0.2, #consider size of field
         n_year, var_CV=CV_value, relative_trend = inflation_rate) 
    
    
    # calculate the annual total yield of vegetables for production risks
    # from Field-2
    annual_total_yield_vegetables_with_financial_risk_field2 <- 
      chili_yield_with_production_risk +
      tomato_yield_with_production_risk +
      spring_onion_yield_with_production_risk_2
    
  }else{
    chinese_mustard_green_yield_loss_with_production_risk <- rep(x = 0, times = n_year)
    chinese_mustard_green_yield_with_production_risk <- 
      chinese_mustard_green_yield_per_season
    
    green_bean_yield_loss_with_production_risk <- rep(x = 0, times = n_year)
    green_bean_yield_with_production_risk <- green_bean_yield_per_season
    
    cabbage_yield_loss_with_production_risk <- rep(x = 0, times = n_year)
    cabbage_yield_with_production_risk <- cabbage_yield_per_season
    
    spring_onion_yield_loss_with_production_risk_1 <- rep(x = 0, times = n_year)
    spring_onion_yield_with_production_risk_1 <- spring_onion_yield_per_season_field1
    
    chili_yield_loss_with_production_risk <- rep(x = 0, times = n_year)
    chili_yield_with_production_risk <- chili_yield_per_season
    
    tomato_yield_loss_with_production_risk <- rep(x = 0, times = n_year)
    tomato_yield_with_production_risk <- tomato_yield_per_season
    
    spring_onion_yield_loss_with_production_risk_2 <- rep(x = 0, times = n_year)
    spring_onion_yield_with_production_risk_2 <- spring_onion_yield_per_season_field2
    
    annual_total_yield_vegetables_with_financial_risk_field1 <- 
      chinese_mustard_green_yield_per_season + green_bean_yield_per_season +
      cabbage_yield_per_season + spring_onion_yield_with_production_risk_1 
    
    annual_total_yield_vegetables_with_financial_risk_field2 <- 
      chili_yield_per_season + 
      tomato_yield_per_season +
      spring_onion_yield_per_season_field2
  }
  
  
  ##### >>>> Event-1b vegetable yield reduction from financial risks ####
  
  # calculate the yield of each vegetables with production risks
  
  
  chance_financial_risk_vegetables_yield <- 
    chance_event(chance = chance_financial_risk_vegetables, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_financial_risk_vegetables_yield == 1)
  {
    
    # Field 1
    
    ## chinese mustard green with financial risk
    chinese_mustard_green_yield_loss_with_financial_risk <- 
      vv((chinese_mustard_green_yield_per_season * 
            prob_damage_financial_risk_vegetables),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    chinese_mustard_green_yield_with_financial_risk <- 
      vv(chinese_mustard_green_yield_per_season - 
           chinese_mustard_green_yield_loss_with_financial_risk,
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    ## green bean with financial risk
    green_bean_yield_loss_with_financial_risk <- 
      vv((green_bean_yield_per_season * 
            prob_damage_financial_risk_vegetables),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    green_bean_yield_with_financial_risk <- 
      vv(green_bean_yield_per_season - 
           green_bean_yield_loss_with_financial_risk,
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    ## cabbage with financial risk
    cabbage_yield_loss_with_financial_risk <- 
      vv((cabbage_yield_per_season * 
            prob_damage_financial_risk_vegetables),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    cabbage_yield_with_financial_risk <- 
      vv(cabbage_yield_per_season - 
           cabbage_yield_loss_with_financial_risk,
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    ## spring onion with financial risk
    spring_onion_yield_loss_with_financial_risk_1 <- 
      vv((spring_onion_yield_per_season_field1 * 
            prob_damage_financial_risk_vegetables),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    spring_onion_yield_with_financial_risk_1 <- 
      vv(spring_onion_yield_per_season_field1 - 
           spring_onion_yield_loss_with_financial_risk_1,
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    # calculate the annual total yield of vegetables for financial risks
    # field 1
    annual_total_yield_vegetables_with_financial_risk_field1 <- 
      chinese_mustard_green_yield_with_financial_risk +
      green_bean_yield_with_financial_risk + cabbage_yield_with_financial_risk + 
      spring_onion_yield_with_financial_risk_1
    
    # Field 2
    
    ## chili with production risk
    chili_yield_loss_with_financial_risk <- 
      vv((chili_yield_per_season * 
            prob_damage_financial_risk_vegetables),
         n_year, var_CV=CV_value)
    
    chili_yield_with_financial_risk <- 
      vv((chili_yield_per_season - 
            chili_yield_loss_with_financial_risk) * 0.4, #consider size of field,
         n_year, var_CV=CV_value)
    
    ## tomato with production risk
    tomato_yield_loss_with_financial_risk <- 
      vv((tomato_yield_per_season * 
            prob_damage_financial_risk_vegetables),
         n_year, var_CV=CV_value)
    
    tomato_yield_with_financial_risk <- 
      vv((tomato_yield_per_season -
            tomato_yield_loss_with_financial_risk) * 0.4, #consider size of field,
         n_year, var_CV=CV_value)
    
    
    ## spring onion with production risk
    spring_onion_yield_loss_with_financial_risk_2 <- 
      vv((spring_onion_yield_per_season_field2 * 
            prob_damage_financial_risk_vegetables),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    spring_onion_yield_with_financial_risk_2 <- 
      vv((spring_onion_yield_per_season_field2 - 
            spring_onion_yield_loss_with_financial_risk_2) * 0.2, #consider size of field
         n_year, var_CV=CV_value, relative_trend = inflation_rate) 
    
    
    # calculate the annual total yield of vegetables for production risks
    # from Field-2
    annual_total_yield_vegetables_with_financial_risk_field2 <- 
      chili_yield_with_financial_risk +
      tomato_yield_with_financial_risk +
      spring_onion_yield_with_financial_risk_2  
    
    
    
  }else{
    chinese_mustard_green_yield_loss_with_financial_risk <- rep(x = 0, times = n_year)
    chinese_mustard_green_yield_with_financial_risk <- 
      chinese_mustard_green_yield_per_season
    
    green_bean_yield_loss_with_financial_risk <- rep(x = 0, times = n_year)
    green_bean_yield_with_financial_risk <- green_bean_yield_per_season
    
    cabbage_yield_loss_with_financial_risk <- rep(x = 0, times = n_year)
    cabbage_yield_with_financial_risk <- cabbage_yield_per_season
    
    spring_onion_yield_loss_with_financial_risk_1 <- rep(x = 0, times = n_year)
    spring_onion_yield_with_financial_risk_1 <- spring_onion_yield_per_season_field1
    
    annual_total_yield_vegetables_with_financial_risk_field1 <- 
      chinese_mustard_green_yield_per_season + green_bean_yield_per_season +
      cabbage_yield_per_season + spring_onion_yield_per_season_field1
    
    chili_yield_loss_with_financial_risk <- rep(x = 0, times = n_year)
    chili_yield_with_financial_risk <- chili_yield_per_season
    
    tomato_yield_loss_with_financial_risk <- rep(x = 0, times = n_year)
    tomato_yield_with_financial_risk <- tomato_yield_per_season
    
    spring_onion_yield_loss_with_financial_risk_2 <- rep(x = 0, times = n_year)
    spring_onion_yield_with_financial_risk_2 <- spring_onion_yield_per_season_field2
    
    annual_total_yield_vegetables_with_financial_risk_field2 <-
      chili_yield_per_season + tomato_yield_per_season + 
      spring_onion_yield_per_season_field2
    
  }
  
  ###### >>>> Event-1c vegetable yield reduction from production and financial risks ####
  
  # calculate the yield of each vegetables with production risks
  
  chance_production_financial_vegetables_yield <- 
    chance_event(chance = (1 - (chance_financial_risk_vegetables + 
                                  chance_financial_risk_vegetables)), value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_production_financial_vegetables_yield == 1)
  {
    
    ## chinese mustard green with production financial risk
    chinese_mustard_green_yield_loss_with_production_financial_risk <- 
      vv((chinese_mustard_green_yield_per_season * 
            (prob_damage_financial_risk_vegetables + 
               prob_damage_production_risk_vegetables)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    chinese_mustard_green_yield_with_production_financial_risk <- 
      vv(chinese_mustard_green_yield_per_season - 
           chinese_mustard_green_yield_loss_with_production_financial_risk,
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    ## green bean with production and financial risk
    green_bean_yield_loss_with_production_financial_risk <- 
      vv((green_bean_yield_per_season * 
            (prob_damage_financial_risk_vegetables + 
               prob_damage_production_risk_vegetables)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    green_bean_yield_with_production_financial_risk <- 
      vv(green_bean_yield_per_season - 
           green_bean_yield_loss_with_production_financial_risk,
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    ## cabbage with production and financial risk
    cabbage_yield_loss_with_production_financial_risk <- 
      vv((cabbage_yield_per_season * 
            (prob_damage_financial_risk_vegetables + 
               prob_damage_production_risk_vegetables)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    cabbage_yield_with_production_financial_risk <- 
      vv(cabbage_yield_per_season - 
           cabbage_yield_loss_with_production_financial_risk,
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    ## spring onion with production and financial risk
    spring_onion_yield_loss_with_production_financial_risk_1 <- 
      vv((spring_onion_yield_per_season_field1 * 
            (prob_damage_financial_risk_vegetables + 
               prob_damage_production_risk_vegetables)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    spring_onion_yield_with_production_financial_risk_1 <- 
      vv(spring_onion_yield_per_season_field1 - 
           spring_onion_yield_loss_with_production_financial_risk_1,
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    # calculate the annual total yield of vegetables for production and 
    # financial risks
    annual_total_yield_vegetables_with_production_financial_risk_field1 <- 
      chinese_mustard_green_yield_with_production_financial_risk +
      green_bean_yield_with_production_financial_risk + 
      cabbage_yield_with_production_financial_risk + 
      spring_onion_yield_with_production_financial_risk_1
    
    
    # Field 2
    
    ## chili with production risk
    chili_yield_loss_with_production_financial_risk <- 
      vv((chili_yield_per_season * 
            (prob_damage_financial_risk_vegetables + 
               prob_damage_production_risk_vegetables)),
         n_year, var_CV=CV_value)
    
    chili_yield_with_production_financial_risk <- 
      vv((chili_yield_per_season - 
            chili_yield_loss_with_production_financial_risk) * 0.4, #consider size of field,
         n_year, var_CV=CV_value)
    
    ## tomato with production risk
    tomato_yield_loss_with_production_financial_risk <- 
      vv((tomato_yield_per_season * 
            (prob_damage_financial_risk_vegetables + 
               prob_damage_production_risk_vegetables)),
         n_year, var_CV=CV_value)
    
    tomato_yield_with_production_financial_risk <- 
      vv((tomato_yield_per_season - 
            tomato_yield_loss_with_production_financial_risk) * 0.4, #consider size of field,
         n_year, var_CV=CV_value)
    
    
    ## spring onion with production risk
    spring_onion_yield_loss_with_production_financial_risk_2 <- 
      vv((spring_onion_yield_per_season_field2 * 
            (prob_damage_financial_risk_vegetables + 
               prob_damage_production_risk_vegetables)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    spring_onion_yield_with_production_financial_risk_2 <- 
      vv((spring_onion_yield_per_season_field2 - 
            spring_onion_yield_loss_with_production_financial_risk_2) * 0.2, #consider size of field
         n_year, var_CV=CV_value, relative_trend = inflation_rate) 
    
    
    # calculate the annual total yield of vegetables for production risks
    # from Field-2
    annual_total_yield_vegetables_with_production_financial_risk_field2 <- 
      chili_yield_with_production_financial_risk +
      tomato_yield_with_production_financial_risk +
      spring_onion_yield_with_production_financial_risk_2  
    
    
  }else{
    chinese_mustard_green_yield_loss_with_production_financial_risk <- rep(x = 0, times = n_year)
    chinese_mustard_green_yield_with_production_financial_risk <- 
      chinese_mustard_green_yield_per_season
    
    green_bean_yield_loss_with_production_financial_risk <- rep(x = 0, times = n_year)
    green_bean_yield_with_production_financial_risk <- green_bean_yield_per_season
    
    cabbage_yield_loss_with_production_financial_risk <- rep(x = 0, times = n_year)
    cabbage_yield_with_production_financial_risk <- cabbage_yield_per_season
    
    spring_onion_yield_loss_with_production_financial_risk_1 <- rep(x = 0, times = n_year)
    spring_onion_yield_with_production_financial_risk_1 <- spring_onion_yield_per_season_field1
    
    annual_total_yield_vegetables_with_production_financial_risk_field1 <- 
      chinese_mustard_green_yield_per_season + green_bean_yield_per_season +
      cabbage_yield_per_season + spring_onion_yield_per_season_field1
    
    chili_yield_loss_with_production_financial_risk <- rep(x = 0, times = n_year)
    chili_yield_with_production_financial_risk <- chili_yield_per_season
    
    tomato_yield_loss_with_production_financial_risk <- rep(x = 0, times = n_year)
    tomato_yield_with_production_financial_risk <- tomato_yield_per_season
    
    spring_onion_yield_loss_with_production_financial_risk_2 <- rep(x = 0, times = n_year)
    spring_onion_yield_with_production_financial_risk_2 <- spring_onion_yield_per_season_field2
    
    annual_total_yield_vegetables_with_production_financial_risk_field2 <-
      chili_yield_per_season + tomato_yield_per_season + spring_onion_yield_per_season_field2
    
  }
  
  ##### >>> Event-2 risks on vegetables prices ####
  
  ##### >>>> Event-2a market risk only ####
  
  ## Event-2a vegetable revenue reduction from market risk under normal condition
  ## (not include other risks e.g. production and financial)
  
  # considering market risk damage on vegetable price
  # causing reduction to farmers' revenue
  
  chance_market_risks_vegetables_price <- 
    chance_event(chance = chance_market_risk_vegetables, value_if = 1, 
                 value_if_not = 0) 
  
  if(chance_market_risks_vegetables_price == 1)
  {
    
    ## calculate vegetable revenue with market risk 
    # vegetable_revenue_with_market_risk <- # price per kg 
    #   vv((total_revenue_vegetables * (1- prob_damage_market_risk_vegetables)), 
    #      n_year, var_CV=CV_value, relative_trend = inflation_rate) # price per kg
    #total price / yr for all yield
    
    chinese_mustard_green_price_with_market_risk <- 
      vv((chinese_mustard_green_price_vv * 
            (1- prob_damage_market_risk_vegetables)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    green_bean_price_with_market_risk <- 
      vv((green_bean_price_vv * 
            (1- prob_damage_market_risk_vegetables)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    cabbage_price_with_market_risk <- 
      vv((cabbage_price_vv * 
            (1- prob_damage_market_risk_vegetables)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    spring_onion_price_with_market_risk <- 
      vv((spring_onion_price_vv * 
            (1- prob_damage_market_risk_vegetables)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    chili_price_with_market_risk <- 
      vv((chili_price_vv * 
            (1- prob_damage_market_risk_vegetables)),
         n_year, var_CV=40, # CV is high because chili is known to be the most fluctuate price
         relative_trend = inflation_rate)
    
    
    tomato_price_with_market_risk <- 
      vv((tomato_price_vv * 
            (1- prob_damage_market_risk_vegetables)),
         n_year, var_CV=CV_value, relative_trend = inflation_rate)
    
    
  }else{
    chinese_mustard_green_price_with_market_risk <- chinese_mustard_green_price_vv
    green_bean_price_with_market_risk <- green_bean_price_vv
    cabbage_price_with_market_risk <- cabbage_price_vv
    spring_onion_price_with_market_risk <- spring_onion_price_vv
    chili_price_with_market_risk <- chili_price_vv
    tomato_price_with_market_risk <- tomato_price_vv
  }
  
  
  ##### >>>>  Event-2b vegetables revenue with all risk ####
  
  ##vegetable revenue reduction from market risk under production 
  # and financial risk
  
  # chance_market_production_financial_risks_vegetables_price <- 
  #   chance_event(chance = (1 -(chance_market_risk_vegetables + 
  #                                chance_production_risk +
  #                                chance_financial_risk_vegetables)), 
  #                value_if = 1, 
  #                value_if_not = 0) 
  # 
  # if(chance_market_production_financial_risks_vegetables_price == 1)
  # {
  # 
  #   # vegetable_farming_revenue_with_all_risks <- 
  #   # vv((total_revenue_vegetables * 
  #   #       (1- (prob_damage_production_risk_vegetables +
  #   #              prob_damage_financial_risk_vegetables +
  #   #              prob_damage_market_risk_vegetables))), 
  #   #    n_year, var_CV=CV_value, relative_trend = inflation_rate)
  # 
  #   
  #   chinese_mustard_green_revenue_with_all_risks <- 
  #     vv(((chinese_mustard_green_yield_with_production_financial_risk *
  #     chinese_mustard_green_price_with_market_risk) * 
  #       (1- (prob_damage_production_risk_vegetables +
  #              prob_damage_financial_risk_vegetables +
  #              prob_damage_market_risk_vegetables))), 
  #     n_year, var_CV=CV_value, relative_trend = inflation_rate)
  #   
  #   
  #   green_bean_revenue_with_all_risks <- 
  #     vv(((green_bean_yield_with_production_financial_risk *
  #     green_bean_price_with_market_risk) *
  #       (1- (prob_damage_production_risk_vegetables +
  #              prob_damage_financial_risk_vegetables +
  #              prob_damage_market_risk_vegetables))), 
  #     n_year, var_CV=CV_value, relative_trend = inflation_rate)
  #       
  #   
  #   cabbage_revenue_with_all_risks <- 
  #     vv(((cabbage_yield_with_production_financial_risk *
  #        cabbage_price_with_market_risk) *
  #          (1- (prob_damage_production_risk_vegetables +
  #                 prob_damage_financial_risk_vegetables +
  #                 prob_damage_market_risk_vegetables))), 
  #        n_year, var_CV=CV_value, relative_trend = inflation_rate)
  #   
  #   spring_onion_revenue_with_all_risks_field1 <- 
  #     vv(((spring_onion_yield_with_production_financial_risk_1 *
  #            spring_onion_price_with_market_risk) *
  #          (1- (prob_damage_production_risk_vegetables +
  #                 prob_damage_financial_risk_vegetables +
  #                 prob_damage_market_risk_vegetables))), 
  #        n_year, var_CV=CV_value, relative_trend = inflation_rate)
  #   
  #   chili_revenue_with_all_risks <- 
  #     vv(((chili_yield_with_production_financial_risk *
  #            chili_price_with_market_risk) *
  #           (1- (prob_damage_production_risk_vegetables +
  #                  prob_damage_financial_risk_vegetables +
  #                  prob_damage_market_risk_vegetables))), 
  #        n_year, var_CV=CV_value, relative_trend = inflation_rate)
  #     
  #   
  #   tomato_revenue_with_all_risks <- 
  #     vv(((tomato_yield_with_production_financial_risk *
  #            tomato_price_with_market_risk) *
  #           (1- (prob_damage_production_risk_vegetables +
  #                  prob_damage_financial_risk_vegetables +
  #                  prob_damage_market_risk_vegetables))), 
  #        n_year, var_CV=CV_value, relative_trend = inflation_rate)
  #   
  #   spring_onion_revenue_with_all_risks_field2 <- 
  #     vv(((spring_onion_yield_with_production_financial_risk_2 *
  #            spring_onion_price_with_market_risk) *
  #           (1- (prob_damage_production_risk_vegetables +
  #                  prob_damage_financial_risk_vegetables +
  #                  prob_damage_market_risk_vegetables))), 
  #        n_year, var_CV=CV_value, relative_trend = inflation_rate)
  #   
  #   
  #   final_vegetable_farming_revenue_with_all_risks <- 
  #     chinese_mustard_green_revenue_with_all_risks +
  #     green_bean_revenue_with_all_risks +
  #     cabbage_revenue_with_all_risks +
  #     spring_onion_revenue_with_all_risks_field1 +
  #     chili_revenue_with_all_risks +
  #     tomato_revenue_with_all_risks +
  #     spring_onion_revenue_with_all_risks_field2
  #   
  # }else{
  #   chinese_mustard_green_revenue_with_all_risks <- chinese_mustard_green_revenue
  #   green_bean_revenue_with_all_risks <- green_bean_revenue
  #   cabbage_revenue_with_all_risks <- cabbage_revenue
  #   spring_onion_revenue_with_all_risks_field1 <- spring_onion_revenue_1
  #   chili_revenue_with_all_risks <- chili_revenue
  #   tomato_revenue_with_all_risks <- tomato_revenue
  #   spring_onion_revenue_with_all_risks_field2 <- spring_onion_revenue_2
  #   final_vegetable_farming_revenue_with_all_risks <- total_revenue_vegetables
  # }
  
  # final vegetable farming revenue
  
  chinese_mustard_green_revenue_with_all_risks <- 
    chinese_mustard_green_yield_with_production_financial_risk *
    chinese_mustard_green_price_with_market_risk
  
  green_bean_revenue_with_all_risks <- 
    green_bean_yield_with_production_financial_risk *
    green_bean_price_with_market_risk
  
  cabbage_revenue_with_all_risks <- 
    cabbage_yield_with_production_financial_risk *
    cabbage_price_with_market_risk
  
  spring_onion_revenue_with_all_risks_field1 <- 
    spring_onion_yield_with_production_financial_risk_1 *
    spring_onion_price_with_market_risk
  
  chili_revenue_with_all_risks <- 
    chili_yield_with_production_financial_risk *
    chili_price_with_market_risk
  
  tomato_revenue_with_all_risks <- 
    tomato_yield_with_production_financial_risk *
    tomato_price_with_market_risk
  
  spring_onion_revenue_with_all_risks_field2 <- 
    spring_onion_yield_with_production_financial_risk_2 *
    spring_onion_price_with_market_risk
  
  final_vegetable_farming_revenue <- chinese_mustard_green_revenue_with_all_risks +
    green_bean_revenue_with_all_risks +
    cabbage_revenue_with_all_risks +
    spring_onion_revenue_with_all_risks_field1 +
    chili_revenue_with_all_risks +
    tomato_revenue_with_all_risks +
    spring_onion_revenue_with_all_risks_field2
  
  
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
    
    vegetables_nutrition_health_benefit <- average_vegetables_price_market_vv *
      annual_vegs_consumption_per_person_vv
    
    # we can assume that the amount of annual spending for vegetables as benefit 
    # as if they grow vegetables then they don't need to spend those money
    # so this amount is added up to the total benefits
    
    
  }else{
    average_vegetables_price_market_vv <- rep(x = 0, times=n_year)
    annual_vegs_consumption_per_person_vv <- rep(x = 0, times=n_year)
    vegetables_nutrition_health_benefit <- rep(x = 0, times=n_year)
  }  
  
  
  ### Rice benefits #2 ####
  
  #### Rice farming #2 ####
  
  # # Annual rice farming income under normal condition
  # # Assuming rice is cultivated twice per year
  # rice_farming_revenue <- vv((rice_yield * rice_farmgate_price), n_year, 
  #                            var_CV=CV_value, relative_trend = inflation_rate) 
  # 
  
  ##### -> Rice risks (farming benefits) #2 ####
  
  ## Chance event options on rice farming benefits ##
  
  # considering production risk damage on rice yield
  # assuming there are some risks like pest, disease, heavy rainfall, storm, 
  # and water shortage causing some lose to rice yield and reduction to rice 
  # capital/cost may also reducing yield
  
  # we assume that rice farming revenue will be the same both in scenario with 
  # and without government assistance because the risks are also the same
  
  
  # 
  # chance_production_risk_rice_benefits_without_gov_assistance <- 
  #   chance_event(chance = chance_production_risk, value_if = 1, 
  #                value_if_not = 0) 
  # 
  # if(chance_production_risk_rice_benefits_without_gov_assistance == 1){  
  #   
  #   # Event 1: Rice yield reduction due to production and financial risks.
  #   rice_yield_loss_with_production_financial_risk <- vv((rice_yield * 
  #                                                           (prob_damage_production_risk_rice + prob_damage_market_risk_rice)),
  #                                                        n_year, var_CV=CV_value, relative_trend = inflation_rate)
  #   
  #   rice_yield_with_production_financial_risk <- 
  #     vv(rice_yield - rice_yield_loss_with_production_financial_risk,
  #        n_year, var_CV=CV_value, relative_trend = inflation_rate)
  #   
  # }else{
  #   rice_yield_loss_with_production_financial_risk <- rep(x = 0, times = n_year)
  #   rice_yield_with_production_financial_risk <- rice_farming_revenue
  # }
  # 
  # 
  # # Considering market risks and their impact on rice prices,
  # # which can cause a reduction in farmers' revenue.
  # 
  # chance_market_risk_rice_benefits_without_gov_assistance <- 
  #   chance_event(chance = chance_market_risk_rice, value_if = 1, 
  #                value_if_not = 0) 
  # 
  # if(chance_market_risk_rice_benefits_without_gov_assistance == 1){    
  #   
  #   # Event 2: Rice revenue reduction due to market risks under normal conditions 
  #   # (without natural risks).
  #   
  #   rice_farmgate_price_normal <- vv(rice_farmgate_price, n_year, 
  #                                    var_CV=CV_value, 
  #                                    relative_trend = inflation_rate )  
  #   
  #   rice_farmgate_price_with_market_risk <- 
  #     vv((rice_farmgate_price_normal - 
  #           (rice_farmgate_price_normal * prob_damage_market_risk_rice)),
  #        n_year, var_CV=CV_value, relative_trend = inflation_rate)
  #   
  #   rice_farming_revenue_loss_with_market_risk_normal <- 
  #     vv((rice_farmgate_price_with_market_risk * rice_yield), n=n_year,
  #        var_CV=CV_value, relative_trend = inflation_rate)
  #   
  # }else{
  #   rice_farmgate_price_with_market_risk <- rep(x = 0, times = n_year)
  #   rice_farming_revenue_loss_with_market_risk_normal <- rice_farming_revenue
  # }
  # 
  # 
  # # Event 3: Rice revenue reduction due to market risks under production and 
  # # financial risks.
  # 
  # chance_market_risk_with_production_and_financial_risk_rice <- 
  #   chance_event(chance = chance_production_risk + chance_market_risk_rice, 
  #                value_if = 1, 
  #                value_if_not = 0) 
  # 
  # if(chance_market_risk_with_production_and_financial_risk_rice == 1){    
  #   
  #   rice_farming_revenue_loss_with_market_production_financial_risk <- 
  #     rice_farmgate_price_with_market_risk * 
  #     rice_yield_with_production_financial_risk
  #   
  #   # Annual rice farming revenue after considering all risks
  #   final_rice_farming_revenue_without_gov_assistance <- 
  #     vv(rice_farming_revenue_loss_with_market_production_financial_risk,
  #        n_year, var_CV=CV_value, relative_trend = inflation_rate) 
  #   
  # }else{
  #   rice_farming_revenue_loss_with_market_production_financial_risk <- rep(x = 0, times = n_year)
  #   final_rice_farming_revenue_with_gov_assistance <- rice_farming_revenue
  # }
  
  
  
  
  
  
  
  #### Rice cultural value #2 ####
  
  # also the same for both scenarios
  
  
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
    vegetable_processing_cost_with_gov_assistance <- 
      vv((annual_cost_labor_vegetable_processing +
            maintenance_vegetable_processing_cost + 
            marketing_distribution_vegetable_processing_cost),  
         n_year, var_CV = CV_value, relative_trend = inflation_rate)
    
    # final_vegetable_processing_cost_with_gov_assistance <- 
    #   vv(vegetable_processing_cost_with_gov_assistance_precal,
    #      n_year, var_CV = CV_value, relative_trend = inflation_rate)
    
    # }else{
    #   maintenance_vegetable_processing_cost  <- rep(x = 0, times=n_year)
    #   marketing_distribution_vegetable_processing_cost <- rep(x = 0, times=n_year)
    #   annual_cost_labor_vegetable_processing <- rep(x = 0, times=n_year)
    #   vegetable_processing_cost_with_gov_assistance_precal <- rep(x = 0, times=n_year)
    #   final_vegetable_processing_cost_with_gov_assistance <- 
    #     rep(x = 0, times=n_year)
    # }
    
    
    # chance event options on vegetable processing costs #
    
    # considering financial risk (lack of capital)
    # first, we simply consider that farmers can't proceed to process vegetable
    # so they probably just simply throw away their
    
    # event-1, farmers don't have enough capital for processing vegetable
    
    
    chance_risk_farmer_have_no_capital_for_processing <-
      chance_event(chance = chance_financial_risk_vegetables, value_if = 1,
                   value_if_not = 0)
    
    if(chance_financial_risk_vegetables == 1){
      
      # with government assistance
      final_vegetable_processing_cost_with_gov_assistance <- 
        vegetable_processing_cost_with_gov_assistance +
        (vegetable_processing_cost_with_gov_assistance * annual_bank_interest)
      
      # without government assistance:
      final_vegetable_processing_cost_without_gov_assistance <- 
        vv((vegetable_processing_building_facility_cost +
              annual_cost_labor_vegetable_processing +
              vegetable_processing_equipment_procurement_cost +
              vegetable_processing_storage_equipment_procurement_cost +
              maintenance_vegetable_processing_cost + 
              marketing_distribution_vegetable_processing_cost), 
           n_year, var_CV=10, relative_trend = inflation_rate)
      
      
    }else{
      final_vegetable_processing_cost_with_gov_assistance <-
        vegetable_processing_cost_with_gov_assistance
      final_vegetable_processing_cost_without_gov_assistance <- rep(x = 0, times=n_year)
    }
    
    
    ## 2nd scenario: Cost of vegetable processing ####
    
    # For the second scenario, farmers don't get assistance from government
    # So then we can consider farmers need to request loan to bank for the 
    # capital of processing vegetable. If farmers get loan from bank, they need 
    # to pay annual interest, and this is can add up their costs
    
    # First, we need to calculate the overall cost of vegetable processing
    # without government assistance
    
    
    ## Benefit of vegetable processing ####
    
    # Assuming that around 1 to 10% of yield are low grade
    # The lower grade can be processed as sauces, paste, etc.
    # We only consider tomato and chili that can be processed as processed food
    
    # calculate annual tomato raw material (kg)
    tomato_raw_material <- vv(((tomato_yield_per_season * 
                                  portion_low_grade_vegetable_product) * 2),
                              # times 2 if we consider that tomato can be cultivated
                              # twice per year
                              n_year, var_CV=40) #CV is quite higher considering 
    # that not all farmers would do processing
    
    # calculate chili raw material
    chili_raw_material <- vv(((chili_yield_per_season * 
                                 portion_low_grade_vegetable_product) * 2), 
                             # times 2 if we consider that tomato can be cultivated
                             # twice per year
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
    
    chance_market_risk_vegetable_processing <-
      chance_event(chance = chance_market_risk_vegetable_processed, value_if = 1,
                   value_if_not = 0)
    
    if(chance_market_risk_vegetable_processing == 1){
      
      # annual vegetable processed product revenue after considering risk  
      final_vegetable_processed_revenue <- 
        vv((processed_vegetable_product_revenue - 
              (processed_vegetable_product_revenue * 
                 vegetable_processed_income_loss_market_risk)),
           n_year, var_CV=CV_value, relative_trend = inflation_rate) 
      
      
    }else{
      final_vegetable_processed_revenue <- processed_vegetable_product_revenue
      
    }
    
  }
  
  ## Compost ####
  
  # farmers may also consider to do composting from the biomass waste of 
  # their farms.
  # so all the costs will be bear by farmers themselves.
  
  # since there is no particular program from the government for composting
  # we can just consider that the chance event for composting is all under
  # the 2nd scenario, which is without gov assistance
  
  # Probability of compost 
  
  chance_compost <- chance_event(chance = prob_of_compost, value_if = 1, 
                                 value_if_not = 0) 
  if(chance_compost == 1){
    
    ### Rice compost cost ####
    
    # Normally, most rice farmers don't utilize the rice biomass residue as 
    # compost. They will just simply throw the waste away or burn it
    
    # Here, we can assume that what if the rice farmers utilize the rice biomass
    # residue into compost
    
    # Annual cost for composting rice biomass residue
    # First, we need to calculate the establishment cost for composting
    # by preparing composting facility, like compost bin and equipment
    # the cost would be paid only for the first year
    
    first_year_composting_cost <- vv(compost_bin_cost + composter_equipment_cost,
                                     n_year, var_CV = CV_value,
                                     relative_trend = inflation_rate)
    
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
    final_rice_compost_cost <- vv((annual_cost_labor_rice_composting +
                                     compost_activator_cost + composter_maintenance_cost + 
                                     manure_for_compost_cost + first_year_composting_cost),  n_year, var_CV = CV_value,
                                  relative_trend = inflation_rate)
    
    # Calculate the cost with the first year establishment cost
    final_rice_compost_cost[1] <- final_rice_compost_cost[1] + 
      annual_cost_labor_rice_composting + compost_activator_cost + 
      composter_maintenance_cost + manure_for_compost_cost 
    
    
    ## Rice compost benefits ####
    
    # Assuming that about 40 to 60% of biomass resulting in compost
    amount_rice_compost <- vv((biomass_rice * 
                                 portion_compost_result_from_biomass_waste), 
                              n_year, var_CV=CV_value, 
                              relative_trend = inflation_rate)
    
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
    final_vegetable_compost_cost <- vv((annual_cost_labor_vegetable_composting +
                                          compost_activator_cost + composter_maintenance_cost + first_year_composting_cost), 
                                       n_year, var_CV = CV_value,
                                       relative_trend = inflation_rate)
    
    # Calculate the cost with the first year establishment cost
    final_vegetable_compost_cost[1] <- final_vegetable_compost_cost[1] + 
      annual_cost_labor_vegetable_composting +
      compost_activator_cost + composter_maintenance_cost
    
    
    ## Vegetable compost benefits ####
    
    # Assuming that 40 to 60% of biomass resulting in compost
    amount_vegetable_compost <- vv((biomass_vegetables * 
                                      portion_compost_result_from_biomass_waste), 
                                   n_year, var_CV = CV_value)
    
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
    first_year_composting_cost <- rep(x = 0, times=n_year)
    annual_cost_labor_rice_composting <- rep(x = 0, times=n_year)
    final_rice_compost_cost <- rep(x = 0, times=n_year)
    amount_rice_compost <- rep(x = 0, times=n_year)
    rice_compost_revenue <- rep(x = 0, times=n_year)
    rice_compost_revenue_loss_market_risk <- rep(x = 0, times=n_year)
    rice_compost_revenue_with_market_risk <- rep(x = 0, times=n_year)
    final_rice_compost_revenue <- rep(x = 0, times=n_year)
    annual_cost_labor_vegetable_composting <- rep(x = 0, times=n_year)
    final_vegetable_compost_cost <- rep(x = 0, times=n_year)
    amount_vegetable_compost <- rep(x = 0, times=n_year)
    vegetables_compost_revenue <- rep(x = 0, times=n_year)
    vegetable_compost_revenue_loss_market_risk <- rep(x = 0, times=n_year)
    vegetable_compost_revenue_with_market_risk <- rep(x = 0, times=n_year)
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
    
    annual_vegetable_agrotourism_cost <- vv(marketing_and_promotion_ecotourism, 
                                            n_year, var_CV=10)
    
    # Calculate the cost with the first year establishment cost
    annual_vegetable_agrotourism_cost[1] <- annual_vegetable_agrotourism_cost[1] + 
      sign_installation_cost
    
    # Annual income from vegetables field agrotourism  (based on current condition)
    vegetables_agrotourism_revenue <- vv(vegetable_ecotourism_value, n_year, 
                                         var_CV=10)
  }else{
    annual_rice_agrotourism_cost <- rep(x = 0, times=n_year)
    rice_agrotourism_revenue <- rep(x = 0, times=n_year)
    annual_vegetable_agrotourism_cost <- rep(x = 0, times=n_year)
    annual_vegetable_agrotourism_cost <- rep(x = 0, times=n_year)
    rice_agrotourism_cost <- rep(x = 0, times=n_year)
    rice_agrotourism_revenue <- rep(x = 0, times=n_year)
    vegetable_agrotourism_cost <- rep(x = 0, times=n_year)
    vegetables_agrotourism_revenue <- rep(x = 0, times=n_year)
  }
  
  
  # Outcomes ####
  ## 1st scenario outcomes (with gov assistance) ####
  
  ### Rice outcomes #### 
  
  #### Final rice benefits
  # consider if rice without cultural value
  chance_rice_cultural_value <- chance_event(chance = 0.5, value_if = 1, 
                                             value_if_not = 0) 
  
  if(chance_rice_cultural_value == 1){
    
    final_rice_benefits_with_gov_assistance <- 
      final_rice_farming_revenue_with_risks + 
      final_rice_compost_revenue +
      rice_agrotourism_revenue + 
      rice_cultural_value_with_gov_assistance
    
  }else{
    final_rice_benefits_with_gov_assistance <- 
      final_rice_farming_revenue_with_risks + 
      final_rice_compost_revenue +
      rice_agrotourism_revenue
  }
  
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
    final_vegetable_farming_cost_with_risks_with_gov_assistance + 
    final_vegetable_compost_cost +
    final_vegetable_processing_cost_with_gov_assistance + 
    annual_vegetable_agrotourism_cost
  
  #### Final result for vegetables
  final_vegetable_result_with_gov_assistance <- 
    final_vegetable_benefits_with_gov_assistance - 
    final_vegetable_costs_with_gov_assistance
  
  
  ## 2nd scenario outcomes (without gov assistance) ####
  
  ### Rice outcomes #### 
  
  #### Final rice benefits
  # consider if rice without cultural value
  chance_rice_cultural_value <- chance_event(chance = 0.5, value_if = 1, 
                                             value_if_not = 0) 
  
  if(chance_rice_cultural_value == 1){
    
    final_rice_benefits_without_gov_assistance <- 
      final_rice_farming_revenue_with_risks + 
      final_rice_compost_revenue +
      rice_agrotourism_revenue + rice_cultural_value_without_gov_assistance
    
  }else{
    final_rice_benefits_without_gov_assistance <- 
      final_rice_farming_revenue_with_risks + 
      final_rice_compost_revenue +
      rice_agrotourism_revenue
  }
  
  
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
    final_vegetable_processing_cost_without_gov_assistance + 
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
              NPV_baseline_rice_with_gov_assistance = NPV_rice_without_gov_assistance,
              NPV_baseline_rice_without_gov_assistance = NPV_rice_without_gov_assistance,
              cashflow_vegetables_with_gov_assistance = final_vegetable_result_with_gov_assistance,
              cashflow_vegetables_without_gov_assistance = final_vegetable_result_without_gov_assistance,
              cashflow_rice_with_gov_assistance = final_rice_result_with_gov_assistance,
              cashflow_rice_without_gov_assistance = final_rice_result_without_gov_assistance))
}


# Monte Carlo Simulation ####

# Run the Monte Carlo simulation using the model function #
TRV_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                  model_function = transition_rice_to_vegetables,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")


# NPV distribution #####

## Plot NPV distribution for both scenarios ####

NPV_vegetable_decision_boxplot <- 
  plot_distributions(mcSimulation_object = TRV_mc_simulation,
                     vars = c("NPV_baseline_rice_with_gov_assistance",
                              "NPV_baseline_rice_without_gov_assistance",
                              "NPV_vegetables_decision_do_with_gov_assistance",
                              "NPV_vegetables_decision_do_without_gov_assistance"),
                     old_names = c("NPV_baseline_rice_with_gov_assistance",
                                   "NPV_baseline_rice_without_gov_assistance",
                                   "NPV_vegetables_decision_do_with_gov_assistance",
                                   "NPV_vegetables_decision_do_without_gov_assistance"),
                     new_names = c("NPV rice with gov assistance (baseline)",
                                   "NPV rice without gov assistance (baseline)",
                                   "NPV vegetables decision do with gov assistance",
                                   "NPV vegetables decision do without gov assistance"),
                     y_axis_name = "",
                     x_axis_name = "Outcome distribution (USD)",
                     color = c("lightgreen", "white", "yellow", "pink3"),
                     method = 'boxplot',
                     base_size = 15) +
  # scale_color_manual(values = c("lightgreen", "brown", "yellow", "pink3"), 
  #                    labels = c("", "", "", ""))+ # Hides labels
  labs(title = "NPV Vegetable Farming Decision", fill = "Decision option")+
  theme(plot.title = element_text(size = 18, face = "bold", 
                                  hjust = 0.5), axis.text.y = element_blank())+
  theme(legend.position = "below")

# Save NPV plot figure
png("NPV.png", width = 1500, height = 1500, res = 250)
plot(NPV_vegetable_decision_boxplot)
dev.off()


NPV_vegetable_and_rice <- 
  plot_distributions(mcSimulation_object = TRV_mc_simulation,
                     vars = c("NPV_baseline_rice_without_gov_assistance",
                              "NPV_vegetables_decision_do_with_gov_assistance",
                              "NPV_vegetables_decision_do_without_gov_assistance"),
                     old_names = c("NPV_baseline_rice_without_gov_assistance",
                                   "NPV_vegetables_decision_do_with_gov_assistance",
                                   "NPV_vegetables_decision_do_without_gov_assistance"),
                     new_names = c("NPV rice (baseline)",
                                   "NPV vegetables decision do with gov assistance",
                                   "NPV vegetables decision do without gov assistance"),
                     y_axis_name = "",
                     x_axis_name = "Outcome distribution (million IDR)",
                     color = c("yellow", "lightblue", "lightgreen"),
                     method = 'boxplot',
                     base_size = 15) +
  # scale_color_manual(values = c("lightgreen", "brown", "yellow", "pink3"), 
  #                    labels = c("", "", "", ""))+ # Hides labels
  labs(title = "NPV Transition to Vegetable Farming Decision", fill = "Decision option")+
  theme(plot.title = element_text(size = 15, face = "bold", 
                                  hjust = 0.5), axis.text.y = element_blank())+
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.box = "vertical")


# Save NPV plot figure
png("NPV_vegs_rice_high.png", width = 1500, height = 1500, res = 250)
plot(NPV_vegetable_and_rice)
dev.off()



NPV_vegetable_decision <- 
  plot_distributions(mcSimulation_object = TRV_mc_simulation,
                     vars = c("NPV_vegetables_decision_do_with_gov_assistance",
                              "NPV_vegetables_decision_do_without_gov_assistance"),
                     old_names = c("NPV_vegetables_decision_do_with_gov_assistance",
                                   "NPV_vegetables_decision_do_without_gov_assistance"),
                     new_names = c("NPV vegetables decision do with gov assistance",
                                   "NPV vegetables decision do without gov assistance"),
                     y_axis_name = "",
                     x_axis_name = "Outcome distribution (USD)",
                     color = c("lightgreen", "lightblue"),
                     method = 'boxplot',
                     base_size = 15) +
  # scale_color_manual(values = c("lightgreen", "brown", "yellow", "pink3"), 
  #                    labels = c("", "", "", ""))+ # Hides labels
  labs(title = "NPV Vegetable Farming Decisions", fill = "Decision option")+
  theme(plot.title = element_text(size = 18, face = "bold", 
                                  hjust = 0.5), axis.text.y = element_blank())+
  theme(legend.position = "right")

# Save NPV plot figure
png("NPV.png", width = 2500, height = 1500, res = 250)
plot(NPV_vegetable_decision_boxplot)
dev.off()



NPV_with_gov_assistance_boxplot <- 
  plot_distributions(mcSimulation_object = TRV_mc_simulation,
                     vars = c("NPV_baseline_rice_with_gov_assistance",
                              "NPV_vegetables_decision_do_with_gov_assistance"),
                     old_names = c("NPV_baseline_rice_with_gov_assistance",
                                   "NPV_vegetables_decision_do_with_gov_assistance"),
                     new_names = c("NPV rice with gov assistance (baseline)",
                                   "NPV vegetables decision with gov assistance"),
                     y_axis_name = "",
                     x_axis_name = "Outcome distribution (USD)",
                     color = c("lightgreen", "lightyellow"),
                     # method = 'boxplot',
                     base_size = 15) +
  scale_color_manual(values = c("lightgreen", "lightyellow"), 
                     labels = c("", ""))+ # Hides labels
  labs(title = "NPV Vegetable Farming Decisions", fill = "Decision option")+
  theme(plot.title = element_text(size = 18, face = "bold", 
                                  hjust = 0.5), axis.text.y = element_blank())+
  theme(legend.position = "right")

# Save NPV plot figure
png("NPV.png", width = 2500, height = 1500, res = 250)
plot(NPV_with_gov_assistance_boxplot)
dev.off()




# simple overlay
NPV_vegetable_decision_overlay <- 
  plot_distributions(mcSimulation_object = TRV_mc_simulation,
                     vars = c("NPV_vegetables_decision_do_with_gov_assistance",
                              "NPV_vegetables_decision_do_without_gov_assistance",
                              "NPV_baseline_rice_without_gov_assistance"),
                     old_names = c("NPV_vegetables_decision_do_with_gov_assistance",
                                   "NPV_vegetables_decision_do_without_gov_assistance",
                                   "NPV_baseline_rice_without_gov_assistance"),
                     new_names = c("NPV vegetables decision with gov assistance",
                                   "NPV vegetables decision do without gov assistance",
                                   "NPV baseline"),
                     y_axis_name = "",
                     color = c("lightblue", "lightgreen", "yellow"),
                     base_size = 15)+
  labs(title = "NPV Vegetable Farming Decisions")


## Summary of statistics NPV ####
simulation_results <- data.frame(TRV_mc_simulation$y)
selected_data <- 
  simulation_results[, c("NPV_baseline_rice_without_gov_assistance",
                         "NPV_vegetables_decision_do_with_gov_assistance",
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

# Save table summary NPV
write.csv(summary_stats, "summary_statistics_npv_all_new_idr_high-rice.csv", row.names = FALSE)



# Cashflow analysis ####

## Plot cashflow vegetables with government assistance ####
cashflow_vegetables_with_gov_assistance <- 
  plot_cashflow(mcSimulation_object = TRV_mc_simulation,
                cashflow_var_name = "cashflow_vegetables_with_gov_assistance",
                facet_labels = "With Government Assistance",
                x_axis_name = "Years with intervention",
                y_axis_name = "Annual cashflow (million IDR)",
                color_25_75 = "lightblue", color_5_95 = "grey",
                color_median = "blue", base_size = 15) +
  theme(axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12),
        legend.position = "bottom",
        legend.margin = margin(t = 5),
        legend.box = "horizontal", legend.text = element_text(size = 12),
        aspect.ratio = 3/3)


# Save cashflow plot figure
png("cashflow_with_idr.png", width = 2000, height = 1500, res = 250)
plot(cashflow_vegetables_with_gov_assistance)
dev.off()

## Plot cashflow vegetables with government assistance ####

#  Plot cashflow
cashflow_vegetables_without_gov_assistance <- 
  plot_cashflow(mcSimulation_object = TRV_mc_simulation,
                cashflow_var_name = "cashflow_vegetables_without_gov_assistance",
                facet_labels = "Without Government Assistance",
                x_axis_name = "Years with intervention",
                y_axis_name = "",
                color_25_75 = "lightgreen", color_5_95 = "grey",
                color_median = "blue", base_size = 15) +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.position = "bottom",
        legend.margin = margin(t = 5),
        legend.box = "horizontal", legend.text = element_text(size = 12),
        aspect.ratio = 3/3)

scale_y_discrete(
  labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
) 



# Save cashflow plot figure
png("cashflow_without.png", width = 2500, height = 1500, res = 250)
plot(cashflow_vegetables_without_gov_assistance)
dev.off()

# Combined two cashflows
combined_cashflow <- (cashflow_vegetables_with_gov_assistance | cashflow_vegetables_without_gov_assistance)


# Save combined cashflow plot figure
png("cashflow_both_idr_moderate.png", width = 3000, height = 1500, res = 250)
plot(combined_cashflow)
dev.off()


## Plot cashflow rice with government assistance ####

#  Plot cashflow
cashflow_rice_with_gov_assistance <- 
  plot_cashflow(mcSimulation_object = TRV_mc_simulation,
                cashflow_var_name = "cashflow_rice_with_gov_assistance",
                facet_labels = "Cashflow Rice with Government Assistance",
                x_axis_name = "Years with intervention",
                y_axis_name = "Annual cashflow (USD)",
                color_25_75 = "lightyellow", color_5_95 = "grey",
                color_median = "blue", base_size = 18)



# Save cashflow plot figure
png("cashflow_rice_with.png", width = 2500, height = 1500, res = 250)
plot(cashflow_vegetables_without_gov_assistance)
dev.off()

## Plot cashflow rice without government assistance ####

#  Plot cashflow
cashflow_rice_without_gov_assistance <- 
  plot_cashflow(mcSimulation_object = TRV_mc_simulation,
                cashflow_var_name = "cashflow_rice_without_gov_assistance",
                facet_labels = "Cashflow Rice without Government Assistance",
                x_axis_name = "Years with intervention",
                y_axis_name = "Annual cashflow (USD)",
                color_25_75 = "pink3", color_5_95 = "grey",
                color_median = "blue", base_size = 18)

# Save cashflow plot figure
png("cashflow_rice_without.png", width = 2500, height = 1500, res = 250)
plot(cashflow_vegetables_without_gov_assistance)
dev.off()

# combine figures

cashflow_all <- (cashflow_vegetables_with_gov_assistance | cashflow_vegetables_without_gov_assistance) /
  (cashflow_rice_with_gov_assistance | cashflow_rice_without_gov_assistance)


## Summary statistic for cashflow ####

# with gov assistance

# Assuming 'data$y' contains the simulated cashflow data
# Extract columns related to cashflows (e.g., those starting with "cashflow_vegetables_with_gov_assistance")
cashflow_columns <- TRV_mc_simulation$y[, grepl("^cashflow_vegetables", names(TRV_mc_simulation$y))]

# Compute summary statistics for each cashflow variable
cashflow_summary <- apply(cashflow_columns, 2, function(column) {
  c(
    Min = min(column, na.rm = TRUE),
    Q5 = quantile(column, 0.05, na.rm = TRUE),
    Q25 = quantile(column, 0.25, na.rm = TRUE),
    Median = median(column, na.rm = TRUE),
    Q75 = quantile(column, 0.75, na.rm = TRUE),
    Q95 = quantile(column, 0.95, na.rm = TRUE)
  )
})


str(cashflow_summary)

# Convert to a data frame for easier display
cashflow_summary_df <- as.data.frame(t(cashflow_summary))
cashflow_summary_df <- round(cashflow_summary_df, 2)  # Round values for better readability

# Add variable names for clarity
cashflow_summary_df <- cbind(Cashflow = rownames(cashflow_summary_df), cashflow_summary_df)
rownames(cashflow_summary_df) <- NULL  # Remove rownames for clean display

# Display the summary statistics
print(cashflow_summary_df)

# Save table summary cashflow
write.csv(cashflow_summary_df, "cashflow_summary_new_idr_high.csv")


# Projection to Latent Structures (PLS) analysis ####

# Plot PLS with government assistance
pls_result_vegetables_with_gov_assistance <- 
  plsr.mcSimulation(object = TRV_mc_simulation,
                    resultName = names(TRV_mc_simulation$y)[1], ncomp = 1)

# Create plot
plot_pls_vegetables_with_gov_assistance <- 
  plot_pls(pls_result_vegetables_with_gov_assistance,
           threshold = 1,
           base_size = 15,
           pos_color = "lightblue") +
  labs(title = "With Government Assistance", size = 15, 
       face = "bold", hjust = 0.5)+
  theme(plot.title = element_text(size = 18, face = "bold", 
                                  hjust = 0.5))+
  scale_y_discrete(
    position = "left", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) 


scale_y_discrete(position = "left") + # Move the y-axis to the right
  scale_x_reverse() +
  theme(
    axis.title.y = element_text(hjust = 0.5),  # Ensure the y-axis title is centered
    axis.text.y = element_blank(),             # Hide the y-axis labels (variable names)
    axis.title.x = element_blank()             # Optionally remove x-axis title if not needed
  )


# Save PLS plot figure
png("PLS_with_high.png", width = 2500, height = 1500, res = 250)
plot(plot_pls_vegetables_with_gov_assistance)
dev.off()


# Plot PLS without government assistance
pls_result_vegetables_without_gov_assistance <- 
  plsr.mcSimulation(object = TRV_mc_simulation,
                    resultName = names(TRV_mc_simulation$y)[2], ncomp = 1)
# Create plot
plot_pls_vegetables_without_gov_assistance <- 
  plot_pls(pls_result_vegetables_without_gov_assistance,
           threshold = 1, base_size = 15,
           pos_color = "lightblue") +
  labs(title = "", size = 15,
       face = "bold", hjust = 0.5)+
  theme(plot.title = element_text(size = 18, face = "bold", 
                                  hjust = 0.5))+
  scale_y_discrete(
    position = "left", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) 


scale_y_discrete(
  position = "left", # Move y-axis to the right
  labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
) +
  theme(
    axis.title.y = element_text(hjust = 0.5),  # Center the y-axis title
    axis.text.y = element_text(hjust = 0.5),   # Center the y-axis labels (variable names)
    axis.title.x = element_blank())+             # Optionally remove x-axis title
  theme(legend.title=element_blank())


# Save PLS plot figure
png("PLS_without_moderate.png", width = 2500, height = 2000, res = 250)
plot(plot_pls_vegetables_without_gov_assistance)
dev.off()


# Combine the two PLS plots
combined_plot_pls <- plot_pls_vegetables_with_gov_assistance + 
  plot_pls_vegetables_without_gov_assistance +
  plot_layout(ncol = 2)

# Save combined PLS plot figure
png("PLS_both.png", width = 3500, height = 1800, res = 250)
plot(combined_plot_pls)
dev.off()



# Plot PLS with government assistance
pls_result_rice_with_gov_assistance <- 
  plsr.mcSimulation(object = TRV_mc_simulation,
                    resultName = names(TRV_mc_simulation$y)[3], ncomp = 1)
# Create plot
plot_pls_rice_with_gov_assistance <- 
  plot_pls(pls_result_rice_with_gov_assistance,
           threshold = 1, base_size = 15,
           pos_color = "lightyellow") +
  labs(title = "Rice With Government Assistance", size = 15,
       face = "bold", hjust = 0.5)+
  theme(plot.title = element_text(size = 18, face = "bold", 
                                  hjust = 0.5))+
  scale_y_discrete(
    position = "left", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) 



# Plot PLS without government assistance
pls_result_rice_without_gov_assistance <- 
  plsr.mcSimulation(object = TRV_mc_simulation,
                    resultName = names(TRV_mc_simulation$y)[4], ncomp = 1)
# Create plot
plot_pls_rice_without_gov_assistance <- 
  plot_pls(pls_result_rice_without_gov_assistance,
           threshold = 1, base_size = 15,
           pos_color = "pink3") +
  labs(title = "Rice Without Government Assistance", size = 15,
       face = "bold", hjust = 0.5)+
  theme(plot.title = element_text(size = 18, face = "bold", 
                                  hjust = 0.5))+
  scale_y_discrete(
    position = "left", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) 


# Plot PLS without government assistance
pls_result_rice_without_gov_assistance <- 
  plsr.mcSimulation(object = TRV_mc_simulation,
                    resultName = names(TRV_mc_simulation$y)[4], ncomp = 1)
# Create plot
plot_pls_rice_without_gov_assistance <- 
  plot_pls(pls_result_rice_without_gov_assistance,
           threshold = 1, base_size = 15,
           pos_color = "pink3") +
  labs(title = "Rice Without Government Assistance", size = 15,
       face = "bold", hjust = 0.5)+
  theme(plot.title = element_text(size = 18, face = "bold", 
                                  hjust = 0.5))+
  scale_y_discrete(
    position = "left", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  )


# Plot PLS all
pls_result_all <- 
  plsr.mcSimulation(object = TRV_mc_simulation,
                    resultName = names(TRV_mc_simulation$y)[4], ncomp = 1)
# Create plot
plot_pls_all <- 
  plot_pls(pls_result_all,
           threshold = 1, base_size = 15,
           pos_color = "pink3", "lightblue") +
  labs(title = "PLS all", size = 15,
       face = "bold", hjust = 0.5)+
  theme(plot.title = element_text(size = 18, face = "bold", 
                                  hjust = 0.5))+
  scale_y_discrete(
    position = "left", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  )



# VoI analysis (EVPI) ####
mcSimulation_table <- data.frame(TRV_mc_simulation$x, TRV_mc_simulation$y[1:2])
evpi_TRV <- multi_EVPI(mc = mcSimulation_table, 
                       first_out_var = "NPV_vegetables_decision_do_with_gov_assistance", 
                       write_table = TRUE)

## Plot EVPI individually (?) ####

# EVPI with government assistance
plot_evpi_vegetable_decision_with_gov_assistance  <- 
  plot_evpi(evpi_TRV,
            decision_vars = c("NPV_vegetables_decision_do_with_gov_assistance"),
            new_names = c("With government assistance"),
            bar_color = c("lightblue"),
            base_size = 15)+
  scale_y_discrete(
    position = "left", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) 

# EVPI without government assistance
plot_evpi_vegetable_decision_without_gov_assistance  <- 
  plot_evpi(evpi_TRV,
            decision_vars = c("NPV_vegetables_decision_do_without_gov_assistance"),
            new_names = c("Without Government Assistance"),
            bar_color = c("lightblue"),
            base_size = 15) +
  scale_y_discrete(
    position = "right", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) +
  labs(x = "EVPI (USD)", y = "Variable", size = 10)+
  theme(
    legend.text = element_text(size = 12), # Adjust legend text size
    axis.title.y = element_blank(), # Optional: Remove y-axis title
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12) # Adjust y-axis label text size
  )



# Save EVPI plot figure 
png("evpi_with_moderate.png", width = 2000, height = 1500, res = 250)
plot(plot_evpi_vegetable_decision_with_gov_assistance)
dev.off()


# EVPI without government assistance
plot_evpi_vegetable_decision_without_gov_assistance  <- 
  plot_evpi(evpi_TRV,
            decision_vars = c("NPV_vegetables_decision_do_without_gov_assistance"),
            new_names = c("Without Government Assistance"),
            bar_color = c("lightblue"),
            base_size = 20)+
  scale_y_discrete(
    position = "right", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) +
  labs(x = "EVPI (USD)", y = "Variable", size = 10)+
  theme(
    legend.text = element_text(size = 12), # Adjust legend text size
    axis.title.y = element_blank(), # Optional: Remove y-axis title
    axis.text.y = element_text(size = 12) # Adjust y-axis label text size
  )

# Save EVPI plot figure   
png("evpi_without_moderate.png", width = 2000, height = 1500, res = 250)
plot(plot_evpi_vegetable_decision_without_gov_assistance)
dev.off()



# EVPI without government assistance
plot_evpi_rice_without_gov_assistance  <- 
  plot_evpi(evpi_TRV,
            decision_vars = c("NPV_baseline_rice_without_gov_assistance"),
            new_names = c("Rice Without Government Assistance"),
            bar_color = c("lightblue"),
            base_size = 20)+
  scale_y_discrete(
    position = "right", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) +
  labs(x = "EVPI (USD)", y = "Variable", size = 10)+
  theme(
    legend.text = element_text(size = 12), # Adjust legend text size
    axis.title.y = element_blank(), # Optional: Remove y-axis title
    axis.text.y = element_text(size = 12) # Adjust y-axis label text size
  )




# EVPI with government assistance
plot_evpi_rice_with_gov_assistance  <- 
  plot_evpi(evpi_TRV,
            decision_vars = c("NPV_baseline_rice_with_gov_assistance"),
            new_names = c("Rice Without Government Assistance"),
            bar_color = c("lightblue"),
            base_size = 20)+
  scale_y_discrete(
    position = "right", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) +
  labs(x = "EVPI (USD)", y = "Variable", size = 10)+
  theme(
    legend.text = element_text(size = 12), # Adjust legend text size
    axis.title.y = element_blank(), # Optional: Remove y-axis title
    axis.text.y = element_text(size = 12) # Adjust y-axis label text size
  )








# Combined two individual EVPI plot
evpi_combined <- (plot_evpi_vegetable_decision_with_gov_assistance | plot_evpi_vegetable_decision_without_gov_assistance) 


# Save combined EVPI plot figure
png("evpi_combined.png", width = 2000, height = 1500, res = 250)
plot(evpi_combined)
dev.off()


## Plot EVPI (better?) ####

# Load the first dataset
data1 <- read.csv("EVPI_table_NPV_vegetables_decision_do_with_gov_assistance.csv")
data1 <- data1 %>%
  filter(!is.na(EVPI_do) & EVPI_do != 0) %>%
  mutate(source = "With Government Assistance") # Add a column for the source

# Load the second dataset
data2 <- read.csv("EVPI_table_NPV_vegetables_decision_do_without_gov_assistance.csv")
data2 <- data2 %>%
  filter(!is.na(EVPI_do) & EVPI_do != 0) %>%
  mutate(source = "Without Government Assistance") # Add a column for the source

# Combine the two datasets
combined_data <- bind_rows(data1, data2)

# Calculate percentages within each variable
combined_data <- combined_data %>%
  group_by(variable) %>%
  mutate(percentage = (EVPI_do / sum(EVPI_do)) * 100) %>%
  ungroup()

# Replace underscores in variable names with spaces (for labels)
combined_data <- combined_data %>%
  mutate(variable = gsub("_", " ", variable),  # Replace underscores with spaces
         variable = toTitleCase(variable)) # Capitalize each word


# Create a single combined plot with percentages
EVPI_combined_plot <- 
  ggplot(combined_data, aes(x = "", y = EVPI_do, fill = source)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5), size = 4) + # Add percentage labels within stacks
  coord_flip() + # Flip for horizontal bar style
  labs(
    x = NULL,
    y = "Expected Value of Perfect Information (USD)",
    fill = "Scenario",
    title = "EVPI with Percentages for Each Variable"
  ) +
  facet_wrap(~variable, scales = "free", ncol = 1) + # Remove additional text from labels
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # Bold and italic title
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 13, face = "bold.italic", hjust = 0), # Style facet labels
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    strip.background = element_blank() # Optional: Remove background of facet titles
  )

# Save EVPI plot figure
png("evpi_combined_plot.png", width = 2000, height = 1500, res = 250)
plot(EVPI_combined_plot)
dev.off()

