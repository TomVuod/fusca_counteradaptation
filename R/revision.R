library(dplyr)

data(agitation_tests)
data(aggression_probing_tests)
hours <- as.character(aggression_probing_tests$hour)
no_seconds <- strsplit(hours, ":") %>% lapply(length) %>% unlist %>% {function(x) which(x==2)}()
no_hour <- strsplit(hours, ":") %>% lapply(length) %>% unlist %>% {function(x) which(x==0)}()
hours[no_seconds] <- paste0(hours[no_seconds], ":00")
aggression_probing_tests$hour <- hours
aggression_probing_tests[aggression_probing_tests$test_ID == 184&is.na(aggression_probing_tests$time_elapsed),]$time_elapsed <- c(30,60)
for(test in unique(aggression_probing_tests[no_hour,]$test_ID)){
  rows_ind <- which(aggression_probing_tests$test_ID == test)
  selected_rows <- aggression_probing_tests[rows_ind,]
  start_date <- selected_rows[which(selected_rows$time_elapsed == 10),,drop=FALSE]
  #print(paste(as.character(start_date$date), as.character(start_date$hour), sep = " "))
  start_date <- as.POSIXct(paste(as.character(start_date$date), as.character(start_date$hour), sep = " "))
  full_times <- start_date + minutes(selected_rows$time_elapsed-10)
  selected_rows$hour <- unlist(lapply(full_times, function(x) paste(hour(x), minute(x), second(x), sep = ":")))
  aggression_probing_tests[rows_ind, ] <- selected_rows
}

aggression_probing_tests$time <- as.POSIXct(paste(aggression_probing_tests$date, aggression_probing_tests$hour, sep = " "))

first_treatment_tab <- data.frame()
treatments_lag <- c()
experiments_lag <- c()
for(colony_ in unique(agitation_tests$colony)){
  colony_rows <- agitation_tests[agitation_tests$colony==colony_,,drop = FALSE]
  selected_row <- colony_rows[which.min(as.numeric(colony_rows[["time"]])),,drop = FALSE]
  next_experiment_lag <- dplyr::filter(aggression_probing_tests, colony == colony_)
  next_experiment_lag <- selected_row$time - next_experiment_lag$time
  next_experiment_lag <- next_experiment_lag[next_experiment_lag>=0]
  next_experiment_lag <- min(next_experiment_lag)
  experiments_lag <- c(experiments_lag, next_experiment_lag)
  names(experiments_lag)[length(experiments_lag)] <- as.character(selected_row$colony)
  first_treatment <- selected_row$treatment
  second_treatment_start <- colony_rows %>% filter(treatment != first_treatment) %>% pull(time) %>%
    {function(x) x[which.min(as.numeric(x))]}()
  treatments_lag <- c(treatments_lag, second_treatment_start - selected_row$time)
  if(length(second_treatment_start)>0)
  names(treatments_lag)[length(treatments_lag)] <- colony_
  first_treatment_tab <- rbind(first_treatment_tab,
                               data.frame(aggression_time = sum(colony_rows[colony_rows$treatment == first_treatment,"aggression_duration"]),
                                          treatment = first_treatment,
                                          colony = colony_,
                                          time_lag = experiments_lag))
}

summary(aov(aggression_time~treatment + time_lag, data = first_treatment_tab))



