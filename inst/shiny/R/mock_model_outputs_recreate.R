if (FALSE) {
  mo_grid <- expand.grid(
    c(
      "N-fold cross validation",
      "Leave one visit out",
      "Leave one patient out"
    ),
    c("SSFCM", "RF", "XGBoost", "SVM")
  ) |> dplyr::arrange(Var1)


  mo_grid$Labeling <- "100%"
  colnames(mo_grid)[1:2] <- c("Validation Setting", "Method")

  ssfcm_rows <- dplyr::tibble(
    `Validation Setting` = rep("N-fold cross validation", 3),
    Method = rep("SSFCM", 3),
    Labeling = c("100%", "50%", "...")
  )

  mo_grid <- dplyr::bind_rows(ssfcm_rows,
                              mo_grid)
  mo_grid$Hyperparameters <- ""
  mo_grid$F1 <- runif(n = nrow(mo_grid))
  mo_grid$Precision <- runif(n = nrow(mo_grid))
  mo_grid$Recall <- runif(n = nrow(mo_grid))
  mo_grid$`Other Parameters` <- ""
  mo_grid

  saveRDS(mo_grid,
          file = file.path("inst", "shiny", "www", "model_outputs_mock.RDS"))
}
