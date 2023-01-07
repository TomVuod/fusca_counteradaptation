#' Generate figure 3
#' @export
#' @importFrom rlang .data
#' @importFrom utils data
generate_fig3 <- function(){
  data("aggression_probing_tests", envir = environment())
  # extract IDs of the test in which there was no aggression at all
  no_aggression_tests <- aggression_probing_tests %>%
    transform_to_single_val() %>%
    split(.$test_ID) %>%
    purrr::map(process_test_data) %>%
    do.call(rbind, .) %>%
    filter(.data$ant_number == 0) %>%
    select(.data$test_ID) %>%
    unlist()
  theme_set(theme_bw())
  fig3_data <- aggression_probing_tests %>%
    filter(.data$date > as.Date("2019-01-01")) %>%
    mutate(mean_activity = transform_to_single_val(., column = "actual_fus_numb", mode = 3)$actual_fus_numb,
           min_activity = transform_to_single_val(., column = "actual_fus_numb", mode = 1)$actual_fus_numb,
           max_activity = transform_to_single_val(., column = "actual_fus_numb", mode = 2)$actual_fus_numb) %>%
    mutate(aggression_presence = !(.data$test_ID %in% no_aggression_tests)) %>%
    group_by(.data$territory, .data$aggression_presence, .data$treatment, .data$colony, .data$date) %>%
    summarise(mean_activity = round(max(stats::na.omit(.data$mean_activity))),
              min_activity = round(max(stats::na.omit(.data$min_activity))),
              max_activity = round(max(stats::na.omit(.data$max_activity))))

  fig3_data$aggression_presence <- as.factor(fig3_data$aggression_presence)
  fig3_data$treatment <- forcats::fct_relevel(fig3_data$treatment, c("sanguinea_1", "sanguinea_20"))
  set.seed(10)
  ggplot(fig3_data, aes(y = .data$mean_activity,
                                  shape = .data$treatment,
                                  color = .data$aggression_presence,
                                  ymax = .data$max_activity,
                                  ymin = .data$min_activity)) +
    geom_pointrange(aes(x = as.factor(.data$territory)), cex = 0.5,
                    position = position_jitter(width = 0.25), alpha = 0.7) +
    scale_x_discrete(labels = c("Outside", "Inside")) +
    scale_shape_manual(name = "Number of \nF. sanguinea ants",
                       values = c(15, 17), labels = c("1", "20")) +
    scale_color_manual(name = "Aggression",
                       values = c(3, 2), labels = c("absent", "present")) +
    ylab("Maximal number of the F. fusca ants")+
    xlab("Colony location relative to F. rufa territory")+
    labs(color = "Aggression", shape = "Aggression")
}

#' Generate figure 4
#' @export
generate_fig4 <- function(){
  data("agitation_tests", envir = environment())
  agitation_tests_summary <- group_by(agitation_tests, .data$colony, .data$treatment) %>%
    summarise(aggression_duration = sum(.data$aggression_duration)) %>%
    left_join(distinct(select(agitation_tests, .data$colony, .data$territory)))
  theme_set(theme_bw())
  ggplot(agitation_tests_summary, aes(y = .data$aggression_duration)) +
    geom_boxplot(aes(x = as.factor(.data$territory))) +
    scale_x_discrete(labels = c("Outside", "Inside")) +
    ylab("Aggression duration [sec]") +
    xlab("Colony location relative to F. rufa territory") +
    facet_wrap(~.data$treatment)
}

