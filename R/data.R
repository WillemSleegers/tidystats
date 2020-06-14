#' A many labs replication of Lorge & Curtiss (1936)
#'
#' Data of multiple replication studies of Lorge & Curtiss (1936) from the Many 
#' Labs project (Klein et al., 2014).
#'
#' @format A data frame with 6343 rows and 15 columns:
#' \describe{
#'   \item{ID}{participant number}
#'   \item{source}{attributed source of the quote: Washington or Bin 
#'     Laden}
#'   \item{response}{evaluation of the quote on a 9-point Likert 
#'     scale, with 1 indicating disagreement and 9 indicating agreement}
#'   \item{age}{participant's age}
#'   \item{sex}{participant's sex}
#'   \item{citizenship}{participant's citizenship}
#'   \item{race}{participant's race}
#'   \item{major}{participant's major}
#'   \item{native_language}{participant's native language}
#'   \item{referrer}{location of where the study was conducated}
#'   \item{compensation}{how the participant was compensated for their 
#'     participation}
#'   \item{recruitment}{how the participant was recruited}
#'   \item{separated_or_not}{description of how the study was administered in 
#'     terms of participant isolation}
#'   \item{us_or_international}{whether the study was conducted in the US or 
#'    outside of the US (international)}
#'   \item{lab_or_online}{whether the study was conducted in the lab or online}   
#' }
#' 
#' @details Lorge and Curtiss (1936) examined how a quotation is perceived when 
#' it is attributed to a liked or disliked individual. The quotation of interest 
#' was, ‘‘I hold it that a little rebellion, now and then, is a good thing, and 
#' as necessary in the political world as storms are in the physical world.’’ 
#' In one condition the quotation was attributed to Thomas Jefferson, a liked 
#' individual, and in the other it was attributed to Vladimir Lenin, a disliked 
#' individual. More agreement was observed when the quotation was attributed to 
#' Jefferson than Lenin. In the replication studies, the quotation was 
#' attributed to either George Washington, the liked individual, or Osama Bin 
#' Laden, the disliked individual.
#' 
#' @references 
#' Lorge, I., & Curtiss, C. C. (1936). Prestige, suggestion, and attitudes. 
#' The Journal of Social Psychology, 7, 386-402. 
#' \url{https://doi.org/10.1080/00224545.1936.9919891}
#' 
#' Klein, R.A. et al. (2014) Investigating Variation in Replicability: A "Many 
#' Labs" Replication Project. Social Psychology, 45(3), 142-152. 
#' \url{https://dx.doi.org/10.1027/1864-9335/a000178}
#' 
"quote_source"

#' Data of a replication study of C.R. Cox, J. Arndt, T. Pyszczynski, 
#' J. Greenberg, A. Abdollahi, S. Solomon (2008, JPSP, 94(4), Exp. 6)
#'
#' @description This is the data of a replication study performed by J. Wissink,
#' G. Hoogendoorn, H. Brohmer, M. Verschoor, J. Krijnen, and M. Zeelenberg as 
#' part of the Reproducibility Project: Psychology. The target result of this 
#' replication was the finding in Experiment 6 of Cox et al. (2008) that 
#' participants who scored low on avoidance but high on anxiety demonstrated an 
#' increased relative preference for a parent after mortality salience as 
#' opposed to dental pain, b = -32.04, SE = 14.47, t = -2.22, p = .03.
#'
#' @format A data frame with 200 rows and 67 variables.
#' \describe{
#'  \item{ID}{Participant identifier}
#'  \item{sex}{The participant's sex}
#'  \item{age}{The participant's age}
#'  \item{condition}{The experimental condition: mortality salience or dental pain}
#'  \item{avoidance}{Attachment avoidance score as assessed with the Relationship Scales Questionnaire}
#'  \item{anxiety}{Attachment anxiety score as assessed with the Relationship Scales Questionnaire}
#'  \item{affect_positive}{Sum of positive PANAS items}
#'  \item{affect_negative}{Sum of negative PANAS items}
#'  \item{call_parent}{Minutes allocated (out of 100) to call a parent}
#'  \item{call_siblings}{Minutes allocated (out of 100) to call a sibling}
#'  \item{call_partner}{Minutes allocated (out of 100) to call a romantic partner}
#'  \item{call_friend}{Minutes allocated (out of 100) to call a close friend}
#'  }
#'
#' @details For more information on the Reproducibility Project: Psychology, 
#' see \url{https://osf.io/ezcuj/}. The individual scores on the PANAS and RSQ 
#' items are not included in this dataset.
#'
#' @references
#' Cox, C.R., Arndt, J., Pyszczynski, T., Greenberg, J., Abdollahi, A., & 
#' Solomon, S. (2008) Terror management and adults’ attachment to their parents:
#' The safe haven remains. Journal of Personality and Social Psychology, 94(4), 
#' 696-717, https://dx.doi.org/10.1037/0022-3514.94.4.696)
#'
#' @source
#' \url{https://osf.io/5tbxf/}
#'
"cox"