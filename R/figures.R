#' Generate figure 3
#' @export
generate_fig3 <- function(){
  load("aggression_probing_tests", package = "fusca.defence")
  # extract IDs of the test in which there was no aggression at all
  no_aggression_tests <- aggression_probing_tests %>%
    transform_to_sigle_val() %>%
    split(.$test_ID) %>%
    purrr::map(process_test_data) %>%
    do.call(rbind, .) %>%
    filter(ant_number == 0) %>%
    select(test_ID) %>%
    unlist()
  fig3_data <- aggression_probing_tests %>%
    filter(date > as.Date("2019-01-01")) %>%
    mutate(mean_activity = transform_to_sigle_val(.,column = "actual_fus_numb", mode = 3)$actual_fus_numb,
           min_activity = transform_to_sigle_val(.,column = "actual_fus_numb", mode = 1)$actual_fus_numb,
           max_activity = transform_to_sigle_val(.,column = "actual_fus_numb", mode = 2)$actual_fus_numb) %>%
    mutate(aggression_presence = !(test_ID %in% no_aggression_tests)) %>%
    group_by(territory, aggression_presence, treatment, colony, date) %>%
    summarise(mean_activity = round(max(na.omit(mean_activity))),
              min_activity = round(max(na.omit(min_activity))),
              max_activity = round(max(na.omit(max_activity))))

  fig3_data$aggression_presence <- as.factor(fig3_data$aggression_presence)
  fig3_data$treatment <- forcats::fct_relevel(fig3_data$treatment, c("sanguinea_1", "sanguinea_20"))
  ggplot(Fig_2_data, aes(y = mean_activity,
                                  shape = treatment,
                                  size = exp_order,
                                  color = aggression_presence,
                                  ymax = max_activity,
                                  ymin = min_activity)) +
    geom_pointrange(aes(x = as.factor(territory)), cex = 0.5,
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
  load("agitation_tests", package = "fusca.defence")
  agitation_tests_summary <- group_by(agitation_tests, colony, treatment) %>%
    summarise(aggression_duration=sum(aggression_duration)) %>%
    left_join(distinct(select(agitation_tests_summary, colony, territory)))
  ggplot(agitation_tests, aes(y = aggression_duration)) +
    geom_boxplot(aes(x = as.factor(territory))) +
    scale_x_discrete(labels = c("Outside", "Inside")) +
    scale_y_log10() +
    ylab("Aggression duration [sec]") +
    xlab("Colony location relative to F. rufa territory") +
    facet_wrap(~species)
}

