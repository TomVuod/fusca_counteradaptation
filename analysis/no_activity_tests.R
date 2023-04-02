aggression_probing_tests <- transform_to_single_val(aggression_probing_tests, column = "actual_fus_numb",mode = 2)
aggression_probing_tests <- transform_to_single_val(aggression_probing_tests, mode = 2)

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
active_fusca[active_fusca$activity==0,]

sink("colony_activity.txt")
arrange(active_fusca[active_fusca$colony %in% active_fusca[active_fusca$activity==0,"colony"],], colony)
sink()

id_remove <- active_fusca[active_fusca$activity==0,"test_ID"]
data("aggression_probing_tests")
id_remove <- c(154, 162, 169)
aggression_probing_tests <- aggression_probing_tests[!(aggression_probing_tests$test_ID %in% id_remove),]


