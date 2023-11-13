install.packages("tidycensus")
library(tidycensus)
library(dplyr)

census_api_key("***************************",install = TRUE)

# Fetch the data for 2019 1-year ACS
data <- get_pums(variables = c("CIT", "AGEP", "REGION", "ESR", "SEX"), year = 2019, survey = "acs1" ,state = 'all' )


#Unique values for Citizenship Status
unique_citizenship <- length(unique(data$CIT))

print(paste("Unique values for Citizenship Status:", unique_citizenship))


#Total Dependency Ratio (TDR) for each region

tdr_age <- function(df, lower_age, upper_age){

  if (is.numeric(lower_age) & is.numeric(upper_age)){
    df <- df %>%
      mutate(dependant=case_when(AGEP <=lower_age | AGEP >=upper_age ~ TRUE, 
                                 AGEP <=upper_age | AGEP >=upper_age ~ FALSE))
  }

  else if (is.numeric(lower_age) & is.null(upper_age)){
    df <- df %>% mutate(dependant=if_else(AGEP <=lower_age, TRUE, FALSE))
  }
  else if (is.null(lower_age) & is.numeric(upper_age)){
    df <- df %>% mutate(dependant=if_else(AGEP >=upper_age, TRUE, FALSE))
  }
  else{
    stop("One or both of lower_age or upper_age must be a numeric or null but not both!")
  }
  return(sum(df$dependant)/sum(!df$dependant))
}

tdr <- function(df){
  return (tdr_age(df, lower_age=14, upper_age=65))
}

data %>% 
  group_by(REGION) %>% 
  summarize(tdr=tdr(cur_data())) %>%
  arrange(tdr)


#TDR Based on Employment Status

simple_tdr <- function(df){
  return(sum(df$depend)/sum(!df$depend))
}

data %>% 
  filter(!is.na(ESR)) %>%
  mutate(depend=if_else( ESR=='b' | ESR=='3' | ESR=='6', TRUE, FALSE)) %>%
  group_by(SEX) %>% 
  summarize(tdr=simple_tdr(cur_data())) %>%
  arrange(tdr)
