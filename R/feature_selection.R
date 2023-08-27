#' Calculate top n features using weighted feature selection
#'
#' @param dataset data.frame (dataframe contains columns with acoustic features, assigned labels - as 'label' and  confidence path as 'confidence')
#' @param number_of_features integer
#' @param target_type character c('mania', 'depression', 'all')
#' @param start_column character (starting column from range of required columns, ex: 'pcm_LOGenergy_sma' )
#' @param end_column character (ending column from range of required columns, ex: 'pcm_LOGenergy_sma' 'pcm_fftMag_mfcc_12_')
#'
#' @return list
#' @export
#' @importFrom dplyr %>% filter select
#' @import xgboost
#' @examples
#' \dontrun{
#' weighted_FS(dataset = model_df , number_of_features = 10, target_type = 'mania')
#' }
weighted_FS <-
  function(dataset,
           number_of_features,
           target_type ,
           start_column,
           end_column) {
    stopifnot(is.numeric(number_of_features))
    stopifnot(number_of_features < ncol(dataset))
    stopifnot(target_type %in% c('all', 'mania', 'depression'))
    stopifnot("confidence" %in% colnames(dataset))
    stopifnot("label" %in% colnames(dataset))

    if (target_type == 'mania') {
      dataset <- dataset %>%
        filter(label %in% c('mania' , 'euthymia'))

      dataset <- dataset %>%
        mutate(label = case_when(label == 'mania' ~ 1,
                                 label == 'euthymia' ~ 0))

    } else if (target_type == 'depression') {
      dataset <-
        dataset %>% filter(label %in% c('depression' , 'euthymia'))
      dataset <- dataset %>%
        mutate(label = case_when(label == 'depression' ~ 1,
                                 label == 'euthymia' ~ 0))

    } else{
      dataset <-
        dataset %>% filter(label %in% c('depression' , 'euthymia', 'mixed', 'mania'))
      dataset <- dataset %>%
        mutate(
          label = case_when(
            label == 'depression' ~ 1,
            label == 'euthymia' ~ 0,
            label == 'mania' ~ 2 ,
            label == 'mixed' ~ 3 ,
          )
        )
    }

    # Split the data into features and target variable

    features <-
      dataset %>% select({{ start_column }}:{{ end_column }})   #select('pcm_LOGenergy_sma':'pcm_fftMag_mfcc_12_')  # select acoustic columns
    target <- select(dataset, label)
    weights <- select(dataset, confidence)
    target <- as.numeric(target$label)

    data_with_weights <- cbind(features, weights)


    if (target_type == 'all') {
      params <- list(objective = "multi:softmax",
                     num_class = 4,
                     eval_metric = "mlogloss")
    } else{
      params <- list(objective = "binary:logistic",
                     eval_metric = "logloss")
    }

    model <-
      xgboost(
        data = as.matrix(data_with_weights[, -weights$confidence]),
        label = target,
        weight = data_with_weights$confidence ,
        nrounds = 50,
        params = params
      )

    selected_features <- NULL
    while (length(selected_features) < number_of_features) {
      importance <- xgb.importance(model = model)
      most_important_feature <-
        importance$Feature[which.max(importance$Gain)]
      selected_features <-
        c(selected_features, most_important_feature)
      data_with_weights <-
        data_with_weights[, !names(data_with_weights) %in% most_important_feature]
      dtrain <-
        xgb.DMatrix(as.matrix(data_with_weights[, -weights$confidence]),
                    label = target,
                    weight = data_with_weights$confidence)
      model <- xgb.train(params, data = dtrain, nrounds = 50)
    }


    return(selected_features)
  }
