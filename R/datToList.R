datToList <- function(data){
  
  # dummy variables
  data <- dummy_cols(data, select_columns = "region")
  
  # data as list
  result <- list(
    y = data$installation_c,
    pop = data$popFage * 1e3,
    CPAnyP = as.vector(scale(data$CPAnyP)), 
    CPModP = as.vector(scale(data$CPModP)),
    CPTrad = as.vector(scale(data$CPTrad)),
    UNMP = as.vector(scale(data$UNMP)), 
    UNMModP = as.vector(scale(data$UNMModP)), 
    tfr = as.vector(scale(data$tfr)), 
    popFage = as.vector(scale((data$popFage/data$totpop))), 
    itu_internet = as.vector(scale(data$itu_internet)),
    itu_mobile = as.vector(scale(data$itu_mobile)), 
    itu_female_m = as.vector(scale(data$itu_female_m)),
    gender_in_index = as.vector(scale(data$gender_in_index)), 
    adolescent_birth =as.vector(scale(data$adolescent_birth)), 
    education_f =as.vector(scale(data$education_f)), 
    labour_f_f =as.vector(scale(data$labour_f_f)), 
    gdp_pc17 =as.vector(scale(data$gdp_pc17)),
    gdp_pc =as.vector(scale(data$gdp_pc)),
    
    
    region_Australia_New_Zealand = data$`region_Australia and New Zealand`,
    region_Central_Asia = data$`region_Central Asia`,
    region_Eastern_Asia = data$`region_Eastern Asia`,
    region_Eastern_EU = data$`region_Eastern Europe`,
    region_Latin_America_Caribbean = data$`region_Latin America and the Caribbean`,
    region_Melanesia = data$region_Melanesia,
    region_Northern_Africa = data$`region_Northern Africa`,
    region_Northern_EU = data$`region_Northern Europe`,
    #region_Polynesia = data$region_Polynesia,
    region_South_Eastern_Asia = data$`region_South-eastern Asia`,
    region_Southern_Asia = data$`region_Southern Asia`,
    region_Southern_EU = data$`region_Southern Europe`,
    region_Sub_Saharan_Africa = data$`region_Sub-Saharan Africa`,
    region_Western_Asia = data$`region_Western Asia`, 
    region_Western_EU = data$`region_Western Europe`
  )
  
  return(result)
}