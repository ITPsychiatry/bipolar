# Constants
BD_LABELS <- c("mania", "depression", "mixed", "euthymia")

#' Transforms column into label with healthy-unhealthy values
#'
#' @param dataset data.frame
#' @param label_name character; the name of a column storing state values (mania, depression etc.)
#'
#' @importFrom dplyr %>% mutate case_when
#'
#' @return data.frame
#' @export
#'
#' @examples
#' visits <- get_sample_psychiatric_data()
#' transform_label_healthy_unhealthy(visits, "hamd_ymrs")
transform_label_healthy_unhealthy <- function(dataset, label_name) {
  dataset <- dataset %>% mutate(
    label = case_when(
      dataset[[label_name]] == 'mania' ~ "unhealthy",
      dataset[[label_name]] == 'depression' ~ "unhealthy",
      dataset[[label_name]] == 'mixed' ~ "unhealthy",
      dataset[[label_name]] == 'euthymia' ~ "healthy"
    )
  )
  return(dataset)
}

#' Transforms column with states into custom label
#'
#' @param dataset data.frame
#' @param label_name character
#'
#' @return data.frame
#' @export
#' @examples
#' visits <- get_sample_psychiatric_data()
#' transform_label_custom(visits, "hamd_ymrs")
transform_label_custom <- function(dataset, label_name) {
  dataset["label"] = dataset[label_name]
  return(dataset)
}

#' Transform column into cgi label
#'
#' @param dataset data.frame
#' @param label_name character
#'
#' @importFrom dplyr %>% distinct pull
#'
#' @return dataset
#' @export
#'
#' @examples
#' visits <- get_sample_psychiatric_data()
#' transform_label_cgi(visits, "hamd_ymrs")
transform_label_cgi <- function(dataset, label_name) {
  tryCatch({
    dataset_labels <- pull(dataset, label_name) %>% unique()
    if (any(dataset_labels %in% BD_LABELS)) {
      dataset["label"] <- dataset[label_name]
    }
    return(dataset)
  }, error = function(err) {
    warning(sprintf("An error occured while transforming labels into CGI. %s", err))
    return(NA)
  })
}



#' Transform column into hamilton/young label
#'
#' @param dataset data.frame
#' @param label_name character
#'
#' @return dataset
#' @export
#'
#' @examples
#' visits <- get_sample_psychiatric_data()
#' transform_label_hy(visits, "hamd_suma")
transform_label_hy <- function(dataset, label_name) {
  tryCatch({
    if(is.numeric(dataset[,label_name])){
      dataset["label"] <- dataset[label_name]
    }
    return(dataset)
  }, error = function(err) {
    warning(sprintf("An error occured while transforming labels into Hamilton/Young %s", err))
    return(NA)
  })
}

#' Transform column into multilabels hamilton and young
#'
#' @param dataset data.frame
#'
#' @importFrom dplyr %>%
#'
#' @return dataset
#' @export
#'
#' @examples
#' visits <- get_sample_psychiatric_data()
#' transform_label_symptoms(visits )
transform_label_symptoms <- function(dataset) {
  symptoms_columns <- c('hamd_anxiety','hamd_fearMentalSymptoms','hamd_inhibition', 'hamd_workAndInterests',
                        'hamd_criticism', 'hamd_depressingMood', 'hamd_feelingGuilty', 'yms_appearance',
                        'yms_formalThoughtsDisorder', 'yms_thoughtsDisorder', 'yms_view', 'yms_increasedActivity',
                        'yms_speech', 'yms_elevatedMood', 'yms_destructiveBehavior' , 'yms_irritability',
                        'hamd_earlyAwakening', 'hamd_intermittentSleep', 'hamd_sleepDisorder', 'yms_sleep',
                        'hamd_fearSomaticSymptoms', 'hamd_genericSomaticSymptoms', 'hamd_hypochondria',
                        'hamd_suicidalTendencies')
  tryCatch({
    if( all(symptoms_columns  %in% colnames(dataset))){
      dataset <- dataset %>%
       # rowwise() %>%
        mutate(
          label_anxiety =  hamd_anxiety + hamd_fearMentalSymptoms ,
          label_decreased_activity =  hamd_inhibition + hamd_workAndInterests ,
          label_decreased_mood =  hamd_criticism + hamd_depressingMood + hamd_feelingGuilty ,
          label_disorganization =  yms_appearance+ yms_formalThoughtsDisorder + yms_thoughtsDisorder+ yms_view ,
          label_elevated_activity =  yms_increasedActivity + yms_speech ,
          label_elevated_mood = yms_elevatedMood ,
          label_irritability =  yms_destructiveBehavior  + yms_irritability ,
          label_sleep_disorder =  hamd_earlyAwakening+ hamd_intermittentSleep+ hamd_sleepDisorder+ yms_sleep ,
          label_somatisation =  hamd_fearSomaticSymptoms+ hamd_genericSomaticSymptoms+ hamd_hypochondria ,
          label_suicide =  hamd_suicidalTendencies
        )
    }
    return(dataset)
  }, error = function(err) {
    warning(sprintf("An error occured while transforming labels into symptoms %s", err))
    return(NA)
  })
}
