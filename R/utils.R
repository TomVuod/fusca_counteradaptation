# transform ranges to single values in the specified column
# mode 1: minimal number
# mode 2: maximal number
# mode 3: mean number
# mode 4: random sampling
transform_to_sigle_val<-function(data=long_tests_results,column="aggression_num",mode=1){
  fun_list=list(min,max,mean,select_from_range)
  data[,column]<-mapply(function(x) fun_list[[mode]](get_extremes(x)),data[,column])
  data[,column]<-as.numeric(as.character(data[,column]))
  data
}

# select random value from a given range
select_from_range<-function(x){
  x_len=length(x)
  if(x_len==1) return(x)
  if(x_len!=2) stop(sprintf("Vaules vector should be of length one or two but is %s", x_len))
  sample(x[1]:x[2],1)
}


# return a data frame row with summarized aggression index for a given test
# (maximal number of aggressive ants during selected period);
# if range instead of single value is given - minimum value is taken
# to do: random sampling from range
# to do: account for colonies tested twice
process_test_data<-function(data, duration=60, test_id=NA, ...){
  if(!is.na(test_id)){
    data <- filter(data, test_ID==test_id)
  }
  territory_treat <- data$territory[1]
  colony <- data$colony[1]
  test_id <- data$test_ID[1]
  treatment <- NA
  if ("treatment" %in% colnames(data)) treatment <- data$treatment[1]
  ant_number <- hitherto_number(test_id, time_limit = duration, data=data, ...)
  data.frame(ant_number = ant_number, territory = territory_treat,
             test_ID = test_id, colony=colony, treatment = treatment)
}


# take the maximum number of ants observed up to defined time point
hitherto_number<-function(test_id=NA, time_limit=10, column="aggression_num",
                          data=aggression_probing_tests, ...){
  if(!is.na(test_id))
    filter(data,test_ID==test_id,time_elapsed <= time_limit) %>%
    pull(!!sym(column)) -> vals
  else
    filter(data, time_elapsed <= time_limit) %>%
    pull(!!sym(column)) -> vals
  max(na.omit(vals))
}

# perform the chi-square test for the number of colonies in which
# the number of aggressive F. fusca was greater then threshold value (defaults to 0);
# the tested factor is location of F. fusca colony relative to red wood ant territory
chi_sq_test<-function(upper_limit_date="2020-12-31", lower_limit_date="2017-01-01",
                      treatment_="sanguinea_1", duration=60, threshold_number=0,
                      data=long_tests_results,...){
  filtered_data <- filter(data,date < as.Date(upper_limit_date),
                        date > as.Date(lower_limit_date), treatment==treatment_)
  filtered_data <- transform_to_sigle_val(data=filtered_data,...)
  aggression_presence<-data.frame()
  for(test_id in unique(filtered_data$test_ID)){
    aggression_presence <- rbind(aggression_presence,
                               process_test_data(test_id = test_id, data = filtered_data,
                                                 duration = duration,...))

  }
  test_data <- data.frame()
  aggression_presence<-aggression_presence[is.finite(aggression_presence$ant_number),]

  aggression_presence[,"ant_number"] <- aggression_presence[,"ant_number"] > threshold_number
  test_data <- table(aggression_presence[,1:2])[,c("B","T")]
  if(nrow(test_data) == 1) stop("All or none data records pass threshold")
  rownames(tst_data)<-c("threshold number not passed", "threshold number passed")
  print(test_data)
  chisq.test(test_data)
}
