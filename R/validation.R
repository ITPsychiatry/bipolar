#' Calculate F1 score for binary and multiclass problem
#'
#' @param true_y factor containing true labels
#' @param predicted_y factor containing predicted labels
#' @param mode character specifying binary or multiclass label problem.
#' @param average specifies how to summarize overall F1.
#' * `macro` will take unweighted mean of F1 scores for each class,
#' * any other value will exclude overall summary from the returned data frame.
#'
#' @return Dataframe with F1 results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' phases <- c("euthymia", "depression", "mania")
#' yt <- factor(sample(x = phases, replace = TRUE, size = 100), levels = phases)
#' yp <- factor(sample(x = phases, replace = TRUE, size = 100), levels = phases)
#' calculate_f1_metrics(yt, yp)
#' calculate_f1_metrics(yt, yp, average = FALSE)
#' }
calculate_f1_metrics <- function(true_y, predicted_y, mode = "multiclass", average = "macro") {
  stopifnot("Both arguments should be of class `factor`" =
              ("factor" %in% class(true_y)) && ("factor" %in% class(predicted_y)))
  stopifnot(mode %in% c("binary", "multiclass"))

  results_raw <- caret::confusionMatrix(
    table(true_y, predicted_y),
    mode = "everything"
  )

  if (mode == "binary") {
    results <- data.frame(Class = results_raw$positive,
                          F1 = results_raw$byClass[["F1"]])
  } else {
    results_rownames <- as.data.frame(results_raw$byClass)
    row.names(results_rownames) <- gsub("Class: ", "", rownames(results_rownames))

    results <- tibble::rownames_to_column(results_rownames, "Class")
    results <- select(results, Class, F1)

    if (average == "macro") {
      results <- rbind(results,
                       data.frame(Class = "avg_macro", F1 = mean(results$F1)))
    }
  }

  return(results)
}


#' Calculate metric(s) for multiclass problem
#'
#' TODO: to be merged with `calculate_f1_metrics`, currently not done
#' since this requires complex regression tests and because this function
#' does not handle binary label problems.
#'
#' @param true_y factor containing true labels
#' @param predicted_y factor containing predicted labels
#' @param average specifies how to summarize overall F1.
#' * `macro` will take unweighted mean of F1 scores for each class,
#' * any other value will exclude overall summary from the returned data frame.
#' @param metric string or character vector with names of selected metrics
#' matching the names from caret::confusionMatrix output.
#'
#' @return Dataframe with metric results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' phases <- c("euthymia", "depression", "mania")
#' yt <- factor(sample(x = phases, replace = TRUE, size = 100), levels = phases)
#' yp <- factor(sample(x = phases, replace = TRUE, size = 100), levels = phases)
#' calculate_metrics(yt, yp)
#' calculate_metrics(yt, yp, metric = c("Precision", "F1"))
#' }
calculate_metrics <- function(true_y, predicted_y, average = "macro", metric = "F1") {
  stopifnot("Both arguments should be of class `factor`" =
              ("factor" %in% class(true_y)) && ("factor" %in% class(predicted_y)))

  results_raw <- caret::confusionMatrix(
    table(true_y, predicted_y),
    mode = "everything"
  )

  results_rownames <- as.data.frame(results_raw$byClass)
  row.names(results_rownames) <- gsub("Class: ", "", rownames(results_rownames))

  results <- tibble::rownames_to_column(results_rownames, "Class")
  results <- results[, c("Class", metric)]

  if (average == "macro") {
    results <- rbind(results,
                     c("Class" = "avg_macro",
                       apply(select(results, -Class), 2, function(x) mean(x, na.rm = TRUE)))
    )
  }

  return(results)
}


#' Creates explicitly cross-validation train/test splits indices for observations.
#'
#' @param i index of a fold considered when invoking this function.
#' @param folds list of length K. Each ith element of the list contains a numeric
#' vector denoting indices of observations assigned to the ith fold.
#'
#' @return List with two named elements `train` and `test`, each containing
#' vector with indices of observations that are categorized as *training data*
#' or *test data*.
#'
#' @examples
#' \dontrun{
#' folds_info <- list(`0`=c(1, 6, 7, 19, 20), `1`=c(4, 5, 8, 9, 11),
#'                    `2`=c(2, 3, 12, 13, 14), `3`=c(10, 15, 16, 17, 18))
#' divide_train_test(1, folds_info)
#' }
divide_train_test <- function(i, folds) {
  all_indices <- unlist(folds)
  test_indices <- folds[[i]]

  list(
    train = setdiff(all_indices, test_indices),
    test = test_indices
  )
}


#' Splits partially supervised data into a list of train/test indices for each fold
#' used as a test set.
#'
#' It can either include *all unsupervised* data in each *training set*,
#' or exclude it from each training set.
#' This behavior enables fair comparison of semi-supervised methods and
#' purely supervised methods on the same train/test splits of *labeled data*.
#'
#' Note that `splitter` invoked on the same `y` and `folds` with different
#' `partial` settings will invoke different indices for observations from
#' different folds since excluding labeled data rearranges the range of the
#' possible indices.
#' In the examples section we show that these two sets of indicies result
#' in the same splitting of the observations into respective folds.
#'
#' @param y vector containing labels (unsupervised data are represented by `NA`)
#' @param obs_folds vector containing division of observations into folds
#' in a format returned by `caret::createFolds(..., list=FALSE)`.
#' Note that fold for unsupervised observations is represented by `0`s.
#'
#' @param partial boolean value.
#' If `partial = TRUE`, include the `0` fold containing unlabeled observations
#' in each training set in the resulting train/test splits of the labeled data.
#' If `partial = FALSE`, simply exclude the unsupervised observations
#' and - correspondingly - the `0` fold.
#'
#' @return list of length K, where K is the number of folds.
#' Each element of the list is a list with two named elements `train` and `test`
#' that include vectors with indices of observations treated as training data
#' (all data but kth fold) and test data (data from kth fold).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' grouping <- c(0, 2, 2, 1, 1, 0, 0, 1, 1, 3, 1, 2, 2, 2, 3, 3, 3, 3, 0, 0)
#' ys <- c(NA, "y", "n", "y", "n", NA, NA, "y", "n", "y",
#'         "n", "y", "y", "y", "y", "n", "n", "n", NA, NA)
#' splitter_cv(ys, grouping, TRUE)
#' splitter_cv(ys, grouping, FALSE)
#'
#' df <- data.frame(grouping=grouping, id=1:20)
#' splits_partial <- splitter_cv(y = ys, obs_folds = df$grouping, partial=TRUE)
#' df[splits_partial[[2]]$test, ]
#' splits_not_partial <- splitter_cv(y = ys, obs_folds = df$grouping, partial = FALSE)
#' df[!is.na(ys), ][splits_not_partial[[2]]$test, ]
#' }
splitter_cv <- function(y, obs_folds, partial) {
  if (partial) {
    folds_ids <- split(seq(along = y), obs_folds)
    splits <- lapply(seq_along(folds_ids)[-1], divide_train_test, folds_ids)
    names(splits) <- paste0("OnHoldFold", seq_along(folds_ids)[-1] - 1)
  } else {
    folds_ids <- split(seq(along = na.omit(y)), obs_folds[obs_folds != 0])
    splits <- lapply(seq_along(folds_ids), divide_train_test, folds_ids)
    names(splits) <- paste0("OnHoldFold", seq_along(folds_ids))
  }

  return(splits)
}


#' The function to create train/test split in scenario-visit-out (SVO)
#' scenario.
#'
#' @param .data final dataframe for modeling purposes
#' @param visit_out a character indicator, either "random" or "last"
#' @param min_visits numeric, how many visits at least
#' patients must have had to be considered for inclusion in the test set
#'
#' @return a list of two lists named `train` and `test`, each list
#' including integers being indices of observations chosen to be in the
#' respective train/test split
#'
#' @export
#'
#' @examples
#' \dontrun{
#' names <- c("patient_id", "visit_date", "label", "variable")
#'
#' df_example <- rbind(
#'   setNames(data.frame(1, NA, NA, rnorm(20)), names),
#'   setNames(data.frame(2, "2021-03-17", "depression", rnorm(7)), names),
#'   setNames(data.frame(2, "2021-04-18", "depression", rnorm(6)), names),
#'   setNames(data.frame(2, NA, NA, rnorm(24)), names),
#'   setNames(data.frame(3, "2022-10-26", "euthymia", rnorm(11)), names),
#'   setNames(data.frame(3, "2022-11-27", "mixed", rnorm(9)), names),
#'   setNames(data.frame(3, "2022-12-28", "mixed", rnorm(5)), names),
#'   setNames(data.frame(3, NA, NA, rnorm(30)), names)
#' )
#'
#' splits_random <- bipolar::splitter_visits(df_example, "random")
#' df_example[splits_random[[1]]$train, ]
#' df_example[splits_random[[1]]$test, ]
#' }
splitter_visits <-
function(.data, visit_out = c("last", "random"), min_visits = 1) {
  visit_out <- match.arg(visit_out)

  data_grouped <- dplyr::count(.data, patient_id, visit_date, label)

  data_patients_with_enough_visits <-
    data_grouped %>%
    filter(!is.na(label)) %>%
    group_by(patient_id) %>%
    filter(n_distinct(visit_date, label) >= min_visits)

  data_visit_for_holdout_chosen <-
    data_patients_with_enough_visits %>%
    group_by(patient_id) %>%
    arrange(visit_date) %>%
    {if (visit_out == "last")
      dplyr::slice(., n())
      else dplyr::slice_sample(., n = 1)
    } %>%
    mutate(test_flag = TRUE) %>%
    select(patient_id, visit_date, test_flag)

  data_to_split <- dplyr::left_join(
    x = .data,
    y = data_visit_for_holdout_chosen,
    by = c("patient_id" = "patient_id",
           "visit_date" = "visit_date")
  )

  test_idx = which(data_to_split$test_flag == TRUE)

  output <- list(list(test = test_idx,
                      train = setdiff(seq_len(nrow(data_to_split)), test_idx)))
}
