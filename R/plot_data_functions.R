# Functions for reading in data, transforming data, and plotting.

# Read in state level data

read_state <- function(state, index_seq = modelr::seq_range(1:200, by = 2)){
  read.csv(paste("https://api.covidtracking.com/v1/states/",state,"/daily.csv", sep = "")) %>% 
    janitor::clean_names() %>% 
    select(date, state, positive, negative, pending, total_test_results_source, total_test_results,
           hospitalized_currently, hospitalized_cumulative, in_icu_currently, in_icu_cumulative, on_ventilator_currently,
           on_ventilator_cumulative, recovered, data_quality_grade, total_tests_viral, positive_tests_viral,
           negative_tests_viral, fips) %>% 
    slice(index_seq)
    
}
  