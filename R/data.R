#' A data frame with the regults of behavioural tests in which one or 20 alien ants were placed
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
#'     \item{date}{Test date}
#'     \item{temperature}{Ambient temperature}
#'     \item{alien_colony_ID}{ID of the colony from which alien ants introduced into arena were collected}
#'     }
"aggression_probing_tests"
