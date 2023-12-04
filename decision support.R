install.packages("decisionSupport")
library(decisionSupport)


Transition_rice_to_vegetables <- function(x, varnames){

  # calculate ex-ante risks: impact the implementation of interventions ####
  intervention_NonPopInvolvEvent <-
    chance_event(intervention_NonPopInvolv, 1, 0, n = 1)
  
  
  # pre-calculation of common random draws for all intervention model runs ####
  
    
  #profits from the transition from rice to vegetables (TRV)
  TRV <- vv(TRV_no_intervention, var_CV, n_years)
  TRV_profits <- vv(profit_per_TRV, var_CV, n_years)
  
  
  #benefits of income (to be calculated from each vegetable cultivated?)
  ##for potato
  precalc_intervention_income_potato <- 
    vv(intervention_income_potato_area_ha, var_CV, n_years) *
    vv(intervention_income_potato_yield_t_ha, var_CV, n_years) *
    vv(intervention_income_potato_profit_USD_t, var_CV, n_years)
  
  ##for shallot
  precalc_intervention_income_shallot <- 
    vv(intervention_income_shallot_area_ha, var_CV, n_years) *
    vv(intervention_income_shallot_yield_t_ha, var_CV, n_years) *
    vv(intervention_income_shallot_profit_USD_t, var_CV, n_years)
  
  ##for chili
  precalc_intervention_income_chili <- 
    vv(intervention_income_chili_area_ha, var_CV, n_years) *
    vv(intervention_income_chili_yield_t_ha, var_CV, n_years) *
    vv(intervention_income_chili_profit_USD_t, var_CV, n_years)
  
  ##for carrot
  precalc_intervention_income_carrot <- 
    vv(intervention_income_carrot_area_ha, var_CV, n_years) *
    vv(intervention_income_carrot_yield_t_ha, var_CV, n_years) *
    vv(intervention_income_carrot_profit_USD_t, var_CV, n_years)
  
  
  ##for cabbage
  precalc_intervention_income_cabbage <- 
    vv(intervention_income_cabbage_area_ha, var_CV, n_years) *
    vv(intervention_income_cabbage_yield_t_ha, var_CV, n_years) *
    vv(intervention_income_cabbage_profit_USD_t, var_CV, n_years)
  
  ##income for all vegetables
  precalc_intervention_income_all <- 
    precalc_intervention_income_potato + 
    precalc_intervention_income_shallot +
    precalc_intervention_income_chili +
    precalc_intervention_income_carrot +
    precalc_intervention_income_cabbage
  
  
  #benefits of reduced water use (to be calculated from each vegetable cultivated?)
  ##for potato
  precalc_intervention_wateruse_potato <-
    vv(intervention_wateruse_potato_area_ha, var_CV, n_years) *
    vv(intervention_wateruse_potato_yield_t_ha, var_CV, n_years) *
    vv(intervention_wateruse_potato_profit_USD_t, var_CV, n_years)
  
  ##for shallot
  precalc_intervention_wateruse_shallot <-
    vv(intervention_wateruse_shallot_area_ha, var_CV, n_years) *
    vv(intervention_wateruse_shallot_yield_t_ha, var_CV, n_years) *
    vv(intervention_wateruse_shallot_profit_USD_t, var_CV, n_years)
  
  ##for chili
  precalc_intervention_wateruse_chili <-
    vv(intervention_wateruse_chili_area_ha, var_CV, n_years) *
    vv(intervention_wateruse_chili_yield_t_ha, var_CV, n_years) *
    vv(intervention_wateruse_chili_profit_USD_t, var_CV, n_years)
  
  ##for carrot
  precalc_intervention_wateruse_carrot <-
    vv(intervention_wateruse_carrot_area_ha, var_CV, n_years) *
    vv(intervention_wateruse_carrot_yield_t_ha, var_CV, n_years) *
    vv(intervention_wateruse_carrot_profit_USD_t, var_CV, n_years)
  
  ##for cabbage
  precalc_intervention_wateruse_cabbage <-
    vv(intervention_wateruse_cabbage_area_ha, var_CV, n_years) *
    vv(intervention_wateruse_cabbage_yield_t_ha, var_CV, n_years) *
    vv(intervention_wateruse_cabbage_profit_USD_t, var_CV, n_years)
  
  
  ##water use for all vegetables
  precalc_intervention_wateruse_all <- 
    precalc_intervention_wateruse_potato + 
    precalc_intervention_wateruse_shallot +
    precalc_intervention_wateruse_chili +
    precalc_intervention_wateruse_carrot +
    precalc_intervention_wateruse_cabbage
  
  
  
  #benefits of nutrition (to be calculated from each vegetables)
  ##for potato
  precalc_intervention_nutrition_potato <-
    vv(intervention_nutrition_potato_area_ha, var_CV, n_years) *
    vv(intervention_nutrition_potato_yield_t_ha, var_CV, n_years) *
    vv(intervention_nutrition_potato_profit_USD_t, var_CV, n_years)
  
  ##for shallot
  precalc_intervention_nutrition_shallot <-
    vv(intervention_nutrition_shallot_area_ha, var_CV, n_years) *
    vv(intervention_nutrition_shallot_yield_t_ha, var_CV, n_years) *
    vv(intervention_nutrition_shallot_profit_USD_t, var_CV, n_years)
  
  ##for chili
  precalc_intervention_nutrition_chili <-
    vv(intervention_nutrition_chili_area_ha, var_CV, n_years) *
    vv(intervention_nutrition_chili_yield_t_ha, var_CV, n_years) *
    vv(intervention_nutrition_chili_profit_USD_t, var_CV, n_years)
  
  ##for carrot
  precalc_intervention_nutrition_carrot <-
    vv(intervention_nutrition_carrot_area_ha, var_CV, n_years) *
    vv(intervention_nutrition_carrot_yield_t_ha, var_CV, n_years) *
    vv(intervention_nutrition_carrot_profit_USD_t, var_CV, n_years)
  
  ##for cabbage
  precalc_intervention_nutrition_cabbage <-
    vv(intervention_nutrition_cabbage_area_ha, var_CV, n_years) *
    vv(intervention_nutrition_cabbage_yield_t_ha, var_CV, n_years) *
    vv(intervention_nutrition_cabbage_profit_USD_t, var_CV, n_years)
  
  
  ##nutrition for all vegetables
  precalc_intervention_nutrition_all <- 
    precalc_intervention_nutrition_potato + 
    precalc_intervention_nutrition_shallot +
    precalc_intervention_nutrition_chili +
    precalc_intervention_nutrition_carrot +
    precalc_intervention_nutrition_cabbage
  
  
  # Intervention (?????) ####
  
  for (decision_intervention_notknownyet in c(FALSE,TRUE))
  {
    
    if (decision_intervention_notknownyet)
    {
      intervention_notknownyet <- TRUE
      intervention_notknownyet_PlanningCost <- TRUE
      intervention_notknownyet_cost <- TRUE
    } else
    {
      intervention_notknownyet <- FALSE
      intervention_notknownyet_PlanningCost <- FALSE
      intervention_notknownyet_cost <- FALSE
    }
    
    if (intervention_NonPopInvolvEvent) {
      intervention_notknownyet <- FALSE
      intervention_notknownyet_cost <- FALSE
    }
    
    
    # costs ###
    
    if (intervention_notknownyet_cost) {
      cost_intervention_notknownyet <-
        intervention_input_cost + intervention_labor_cost + intervention_machinery_cost +
        intervention_storage_cost + intervention_transportation_cost
    } else
      cost_intervention_notknownyet <- 0
    
    storage_maintenance_cost <- rep(0, n_years)
    
    if (intervention_notknownyet)
      storage_maintenance_cost <-
      storage_maintenance_cost + vv(maintenance_intervention_notknownyet, var_CV, n_years)
    
    intervention_cost <- storage_maintenance_cost
    intervention_cost[1] <-
      intervention_cost[1] + cost_intervention_notknownyet
    
    
    # Benefits from vegetable cultivation in the intervention 'notknownyet' (?)
    intervention_income_benefits <-
      as.numeric(intervention_notknownyet) * precalc_intervention_income_all
    intervention_wateruse_benefits <-
      as.numeric(intervention_notknownyet) * precalc_intervention_wateruse_all
    intervention_nutrition_benefits <-
      as.numeric(intervention_notknownyet) * precalc_intervention_nutrition_all
    
    
    # Total benefits from vegetable cultivation
    vegetable_production <-
      intervention_income_benefits +
      intervention_wateruse_benefits +
      intervention_nutrition_benefits
    
  }
  

  
  NPV_interv <-
    discount(result_interv, discount_rate, calculate_NPV = TRUE)
  
  NPV_n_interv <-
    discount(result_n_interv, discount_rate, calculate_NPV = TRUE)
  
  
  return(list(Interv_NPV = NPV_interv,
              NO_Interv_NPV = NPV_n_interv,
              NPV_decision_do = NPV_interv - NPV_n_interv,
              Cashflow_decision_do = result_interv - result_n_interv))
  
}


  
  
  
  
  