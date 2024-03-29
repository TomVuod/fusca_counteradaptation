#' Change data frame by transforming ranges to single values in the specified column
#' @param raw_data A data frame with source data for the analysis
#' @param column A character indicating column name with variable to be transformed
#' @param mode A numeric indicating how the range will be mapped to single variable;
#' 1 - minimum, 2 - maximum, 3 - mean, 4 - random.
#' @returns A data frame with transformed variable
#' @export
transform_to_single_val <- function(raw_data = NULL, column="aggression_num", mode="min"){
  if(is.null(raw_data)) {
    data("aggression_probing_tests", envir = environment())
    raw_data <- aggression_probing_tests
  }
  raw_data[,column] <- mapply(function(x) get(mode)(get_extremes(x)),raw_data[,column])
  raw_data[,column] <- as.numeric(as.character(raw_data[,column]))
  raw_data
}

# transform character string representing range to numeric variable
get_extremes<-function(x){
  as.numeric(stringi::stri_split(x,fixed="-")[[1]])
}

# select random value from a given range
random_select_from_range<-function(x){
  x_len=length(x)
  if(x_len==1) return(x)
  if(x_len!=2) stop(sprintf("Vaules vector should be of length one or two but is %s", x_len))
  sample(x[1]:x[2],1)
}

#' Return a data frame row with summarized aggression index for a given test
#' (maximal number of aggressive ants during selected period);
#' if range instead of single value is given - minimum value is taken
#' @param data A data frame with results of the behavioural tests
#' @param duration A numeric indicting the period form the test beginning to be
#' taken into consideration
#' @param test_id A numric indicating test ID
#' @param ... Additional arguments passed to `hitherto_number` function
#' @returns A data frame with summarized aggression index
#' @export
process_test_data <- function(data, duration = 60, test_id = NA, ...){
  if(!is.na(test_id)){
    data <- filter(data, .data$test_ID==test_id)
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
hitherto_number <- function(test_id = NA, time_limit = 10, column = "aggression_num",
                          data = aggression_probing_tests, ...){
  if(!is.na(test_id))
    filter(data, .data$test_ID == test_id, .data$time_elapsed <= time_limit) %>%
    pull(!!sym(column)) -> vals
  else
    filter(data, .data$time_elapsed <= time_limit) %>%
    pull(!!sym(column)) -> vals
  max(stats::na.omit(vals))
}

#' Perform the chi-square test for the number of colonies in which
#' the number of aggressive F. fusca was greater then threshold value (defaults to 0);
#' the tested factor is location of F. fusca colony relative to red wood ant territory
#'
#' @param upper_limit_date A character to be coerced to class 'Date' indicating
#' upper bound to filter experiments by completion date
#' @param lower_limit_date A character to be coerced to class 'Date' indicating
#' lower bound to filter experiments by completion date
#' @param treatment_ A character indicating treatment variant to be chosen for
#' statistical comparison
#' @param duration A numeric indicating the number of minutes from the experiment begin
#' to be considered when determining aggression occurrence.
#' @param threshold_number A numeric indicating the minimum number of ants being
#' aggressive above which the test is classified as showing aggression
#' @param raw_data A data frame with source data for the analysis
#' @param ... Other arguments passed to `transform_to_single_val` function
#' @returns Output from stats::chisq.test and medians of the compared variables
#' @importFrom magrittr "%>%"
#' @export
chi_sq_test <- function(upper_limit_date = "2020-12-31", lower_limit_date = "2017-01-01",
                      treatment_ = "sanguinea_1", duration = 60, threshold_number = 0,
                      raw_data = NULL, ...){
  if(is.null(raw_data)) {
    data("aggression_probing_tests", envir = environment())
    raw_data <- aggression_probing_tests
  }
  filtered_data <- filter(raw_data, .data$date < as.Date(upper_limit_date),
                        .data$date > as.Date(lower_limit_date), .data$treatment==treatment_)
  filtered_data <- transform_to_single_val(raw_data = filtered_data,...)
  aggression_presence<-data.frame()
  for(test_id in unique(filtered_data$test_ID)){
    aggression_presence <- rbind(aggression_presence,
                               process_test_data(test_id = test_id, data = filtered_data,
                                                 duration = duration,...))
  }
  test_data <- data.frame()
  aggression_presence <- aggression_presence[is.finite(aggression_presence$ant_number), ]
  aggression_presence[,"ant_number"] <- aggression_presence[,"ant_number"] > threshold_number
  test_data <- table(aggression_presence[,1:2])[,c("B","T")]
  if(nrow(test_data) == 1) stop("All or none data records pass threshold")
  rownames(test_data) <-c ("threshold number not passed", "threshold number passed")
  print(test_data)
  stats::chisq.test(test_data)
}

#' Summarize data from measurement of the head width of F. fusca foragers
#'
#' @param headwidth A data frame with the results of head width measurement
#' @returns A data frame with head width summarized over colonies
#' @export
headwidth_summary <- function(headwidth){
  hw_summary<- split(headwidth, headwidth$colony)
  hw_summary <- lapply(hw_summary, function(x){
    data.frame(mean=mean(x$head_width),
               median=stats::median(x$head_width),
               min=min(x$head_width),
               max=max(x$head_width),
               sd=stats::sd(x$head_width),
               n=nrow(x))})
  hw_summary <- do.call(rbind, hw_summary)
  hw_summary <- cbind(hw_summary, data.frame(colony=rownames(hw_summary)))
  rownames(hw_summary) <- seq_len(nrow(hw_summary))
  hw_summary
}

#' @import ggplot2 DHARMa lmerTest lme4 dplyr
NULL
