#' A data frame with the results of behavioural tests in which one or 20 alien ants were placed
#' into the arena around F. fusca nest
#'
#' @format The data frame consists of the following columns:
#'   \describe{
#'     \item{test_ID}{Test ID}
#'     \item{colony}{ID of F. fusca colony}
#'     \item{treatment}{Species and number of workers of alien species introduced into the arena}
#'     \item{territory}{Variable indicating location of F. fusca colony relative to red wood ant
#'     territory; T - onto, B - beyond, E - near the edge (from outside)}
#'     \item{time_elapsed}{Time elapsed from the beginning of experiment}
#'     \item{initial_num}{The number of F. fusca ants on the arena at the beginning of test}
#'     \item{actual_fus_numb}{The number of F. fusca ants on the arena at a given time point; only
#'     ants not involved in aggression were included}
#'     \item{aggression_num}{The number of F. fusca ants involved in aggression}
#'     \item{hour}{Timing of the test start}
#'     \item{presence}{A logical vector indicating whther an itroduced ant was still present (visible) during arena
#'     inspection}
#'     \item{date}{Date of the test}
#'     \item{temperature}{Ambient temperature}
#'     \item{alien_colony_ID}{ID of the colony from which alien ants introduced into arena were collected}
#'     }
"aggression_probing_tests"


#' A data frame with the regults of the behavioural tests in which alien ant was places
#' into the arena with numerous already agitated F. fusca ants
#'
#' @format The data frame consists of the following columns:
#'   \describe{
#'     \item{colony}{Colony ID}
#'     \item{date}{Date of the test}
#'     \item{treatment}{Species and number of workers of alien species introduced into the arena}
#'     \item{start}{Time of the test start}
#'     \item{aggression_start}{Experiment start time}
#'     \item{aggression_end}{Experiment end time}
#'     \item{actual_fus_numb}{The number of F. fusca ants on the arena at a given time point; only
#'     ants not involved in aggression were included}
#'     \item{biting_ants_number}{The number of F. fusca ants biting an alien ant on experiment completion}
#'     \item{territory}{Variable indicating location of F. fusca colony relative to red wood ant
#'     territory; T - onto, B - beyond}
#'     \item{aggression_duration}{Duration of alien ant being bitten by F. fusca (in sec.)}
#'     \item{time}{Time and date of experiment start}
#'     \item{alien_colony_ID}{ID of the colony from which alien ants introduced into arena were collected}
#'     }
"agitation_tests"
#' A data frame with the results of measurement of head width of F. fusca foragers
#'  \describe{
#'    \item{colony}{Colony ID}
#'    \item{head_width}{Head width of a single worker in micrometers}
#'  }
"head_width"
