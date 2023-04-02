library(dplyr)
library(lubridate)

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
agitation_tests <- agitation_tests[!is.na(agitation_tests$time),]
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
  second_treatment_start <- colony_rows %>% filter(treatment != first_treatment)
  second_treatment_start <- second_treatment_start$time[1]
  treatments_lag <- c(treatments_lag, second_treatment_start - selected_row$time)
  if(length(second_treatment_start)>0)
  names(treatments_lag)[length(treatments_lag)] <- colony_
  first_treatment_tab <- rbind(first_treatment_tab,
                               data.frame(aggression_time = sum(colony_rows[colony_rows$treatment == first_treatment,"aggression_duration"]),
                                          treatment = first_treatment,
                                          colony = colony_,
                                          time_lag = next_experiment_lag))
}

summary(aov(aggression_time~treatment + time_lag, data = first_treatment_tab))
res <- aov(aggression_time~treatment + time_lag, data = first_treatment_tab)

if(!require(psych)){install.packages("psych")}
if(!require(betareg)){install.packages("betareg")}
if(!require(lmtest)){install.packages("lmtest")}

if(!require(car)){install.packages("car")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(emmeans)){install.packages("emmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}

squeezing_transformation <- function(x){
  ((length(x)-1)*x+0.5)/length(x)
}
first_treatment_tab$time_prop <- first_treatment_tab$aggression_time/(5*60)

first_treatment_tab$time_prop <- squeezing_transformation(first_treatment_tab$time_prop)



library(betareg)
model.beta = betareg(time_prop~treatment + time_lag, data=first_treatment_tab)
library(lmtest)
lrtest(model.beta)
library(emmeans)

joint_tests(model.beta)

plot(model.beta)
cooksD <- cooks.distance(model.beta)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential






aggression_probing_tests <- transform_to_single_val(aggression_probing_tests)
aggression_probing_tests <- transform_to_single_val(aggression_probing_tests, column = "actual_fus_numb")
second_set <- filter(aggression_probing_tests, date > as.Date("2018-12-30"))
second_set$actual_fus_numb[is.na(second_set$actual_fus_numb)] <- 0
second_set$aggression_num[is.na(second_set$aggression_num)] <- 0
second_set <- split(second_set, second_set$test_ID)
active_fusca <- lapply(second_set, function(x) data.frame(treatment = x$treatment[1],
activity = sum((x$actual_fus_numb + x$aggression_num)>0),
test_ID = x$test_ID[1],
date = x$date[1],
colony = x$colony[1],
territory = x$territory[1]))
active_fusca <- do.call(rbind, active_fusca)
active_fusca[active_fusca$colony %in% active_fusca[active_fusca$activity==0,"colony"],]
arrange(active_fusca[active_fusca$colony %in% active_fusca[active_fusca$activity==0,"colony"],], colony)
sink("colony_activity.txt")
arrange(active_fusca[active_fusca$colony %in% active_fusca[active_fusca$activity==0,"colony"],], colony)
sink()



library(dplyr)
second_set <- filter(aggression_probing_tests, date > as.Date("2020-01-01"))
second_set
first_aggression <- filter(second_set, aggression_num != "0") %>% split(.$test_ID) %>% lapply(function(x) data.frame(colony = x$colony[1],
                                                                                                 treatment = x$treatment[1],
                                                                                                 time = min(x$time_elapsed))) %>%
    do.call(rbind, .)

# Is aggression related to worker mean head width?

# TO DO






# Include all tests in activity tests

library(lme4)
library(lmerTest)
library(DHARMa)
model_data <-transform_to_single_val(raw_data = aggression_probing_tests, column = "actual_fus_numb", mode = 4)
# treatment sanguinea_20 should be removed due to significantly lower fusca activity
model_data<-filter(model_data, territory %in% c("B","T"),
                   date > as.Date("2016-12-30"),
                   date < as.Date("2020-12-30"),
                   !is.na(temperature))
model_data$aggression_presence <- model_data$aggression_num != "0"
model_data <-transform_to_single_val(raw_data = model_data, column = "aggression_num", mode = 4)
# temperature transformation: center at zero
model_data$temperature <- model_data$temperature - min(model_data$temperature) + 1
# temperature transformation: normalize standard deviation
sd_temperature <- sd(model_data$temperature)
model_data$temperature <- model_data$temperature/sd_temperature
model_data$actual_fus_numb2 <- sqrt(model_data$actual_fus_numb + 0)
model_data$obs <- as.factor(1:nrow(model_data))
model_data <- filter(model_data, treatment != "sanguinea_20")
# fit generalized linear model; use log10 transform of the temperature
mixed.model.activity<-glmer(actual_fus_numb ~ I((temperature)) + (1|colony) + (1|obs)+territory  +
                              I(log(time_elapsed)) + I(log(aggression_num+2)),
                            data=model_data, family = "poisson")


mixed.model.activity<-lmer(actual_fus_numb2 ~ temperature + (1|colony) + territory+treatment +
                              I(log(time_elapsed)) + I(log(aggression_num+1)), data=model_data)
summary(mixed.model.activity)

set.seed(1010)
sim.resid <- simulateResiduals(mixed.model.activity, n = 1000)
plot(sim.resid)





set.seed(1001)
max_activ_data <- data.frame()
max_ac_model_list <-list()
for (i in seq_len(1E2)){
  print(i)
  model_data <-transform_to_single_val(raw_data = aggression_probing_tests, column = "actual_fus_numb", mode = 4)
  # treatment sanguinea_20 should be removed due to significantly lower fusca activity
  model_data<-filter(model_data, territory %in% c("B","T"),
                     date > as.Date("2016-12-30"),
                     date < as.Date("2020-12-30"),
                     !is.na(temperature))
  model_data$aggression_presence <- model_data$aggression_num != "0"
  model_data <-transform_to_single_val(raw_data = model_data, column = "aggression_num", mode = 4)
  # temperature transformation: center at zero
  model_data$temperature <- model_data$temperature - min(model_data$temperature) + 1
  # temperature transformation: normalize standard deviation
  sd_temperature <- sd(model_data$temperature)
  model_data$temperature <- model_data$temperature/sd_temperature
  model_data$actual_fus_numb2 <- sqrt(model_data$actual_fus_numb + 0)
  model_data$obs <- as.factor(1:nrow(model_data))
  model_data <- filter(model_data, treatment != "sanguinea_20")
  ma_model <- try(glmer(actual_fus_numb ~ I((temperature)) + (1|colony) + (1|obs)+ territory +
                          I(log(time_elapsed)) + I(log(aggression_num+2)),
                        data=model_data, family = "poisson"))
  if (class(ma_model)=="try-error"){
    print("Model error")
    next
  }
  max_ac_model_list[[length(max_ac_model_list)+1]] <- list()
  sim_residuals <-simulateResiduals(ma_model)
  diagnostic_tests <- list(testUniformity(sim_residuals, plot = FALSE)$p.value,
                           testOutliers(sim_residuals, plot = FALSE)$p.value,
                           testDispersion(sim_residuals, plot = FALSE)$p.value,
                           testQuantiles(sim_residuals, plot = FALSE)$p.value)
  names(diagnostic_tests) <- c("testUniformity", "testOutliers", "testDispersion", "testQuantiles")
  max_ac_model_list[[length(max_ac_model_list)]]$glmm_model <- ma_model
  max_ac_model_list[[length(max_ac_model_list)]]$diagnostic_tests <- diagnostic_tests
  max_ac_model_list[[length(max_ac_model_list)]]$p_val <- summary(ma_model)$coeff[,4]
}
# which model pass all diagnostic tests
max_activ_data <- data.frame()
for(i in seq_along(max_ac_model_list)){
  max_activ_data <- rbind(max_activ_data, data.frame(c(fixef(max_ac_model_list[[i]]$glmm_model),
                                                       max_ac_model_list[[i]]$diagnostic_tests)))
}
diagnostic_pass <- apply(max_activ_data, 1, function(x) all(x[5:8]>0.05))
cat(sprintf("Number of models which passed all diagnostic tests: %d\n",
            sum(diagnostic_pass, na.rm = TRUE)))


model_p_vals <- data.frame()
for(i in seq_along(max_ac_model_list)){
  model_p_vals <- rbind(model_p_vals, t(max_ac_model_list[[i]]$p_val))
}
p_vals_split <- split(model_p_vals, diagnostic_pass) %>% lapply(function(x) apply(x,2,summary))
p_vals_split$'TRUE'


summarize_test <- function(test_ID, data = aggression_probing_tests){
  test_data <- aggression_probing_tests[aggression_probing_tests$test_ID == test_ID,]
  test_data <- transform_to_single_val(test_data, mode = 4)
  data.frame(colony = test_data$colony[1],
             territory = test_data$territory[1],
             aggression_num = max(na.omit(test_data$aggression_num)),
             treatment = test_data$treatment[1])
}


summarized_data <- data.frame()
for (test_ID in unique(aggression_probing_tests$test_ID)){
  summarized_data <- rbind(summarized_data, summarize_test(test_ID))
}

data(head_width)

summarized_data <- left_join(head_width, summarized_data)

summarized_data$aggression_presence <- summarized_data$aggression_num > 0
library(lme4)
library(purrr)
library(lmerTest)
library(DHARMa)

logistic_regress<-glmer(aggression_presence~head_width+territory+treatment+ (1|colony),
                        data=summarized_data, family="binomial")

logistic_regress<-glm(aggression_presence~head_width+territory*treatment,
                        data=summarized_data, family="binomial")

logistic_regress<-glm(aggression_presence~head_width+territory+treatment,
                      data=summarized_data, family="binomial")
summary(logistic_regress)
sim_residuals <-simulateResiduals(logistic_regress)
plot(sim_residuals)





# include aggression presence
library(lme4)
library(lmerTest)
library(DHARMa)
model_data <-transform_to_single_val(raw_data = aggression_probing_tests, column = "actual_fus_numb")
# treatment sanguinea_20 should be removed due to significantly lower fusca activity
model_data<-filter(model_data, territory %in% c("B","T"),
                   date > as.Date("2016-12-30"),
                   date < as.Date("2020-12-30"),
                   !is.na(temperature),
                   treatment!="sanguinea_20")
aggression_records <- filter(model_data, aggression_num > 0)
aggression_tests <- unique(aggression_records$test_ID)
new_dataset<-data.frame()
for(i in 1:nrow(model_data)){
  Test = model_data$test_ID[i]
  interval = model_data$time_elapsed[i]
  # include experiments with no aggression
  if (!(Test %in% aggression_records$test_ID)){
    new_dataset <- rbind(new_dataset, model_data[i,])
    next
  }
  # include experiment part before aggression occurred
  reference_set <- filter(aggression_records, test_ID == Test)
  if (all(interval < reference_set$time))
    new_dataset<-rbind(new_dataset, model_data[i,])
}
new_dataset$aggression_presence <- new_dataset$test_ID %in% aggression_tests
model_data <- new_dataset
# temperature transformation: center at zero
model_data$temperature <- model_data$temperature - min(model_data$temperature) + 1
# temperature transformation: normalize standard deviation
sd_temperature <- sd(model_data$temperature)
model_data$temperature <- model_data$temperature/sd_temperature
# fit generalized linear model; use log10 transform of the temperature
mixed.model.activity<-glmer(actual_fus_numb ~ temperature + (1|colony) + territory +
                              I(log10(time_elapsed)) + aggression_presence, data=model_data, family="poisson")
summary(mixed.model.activity)
