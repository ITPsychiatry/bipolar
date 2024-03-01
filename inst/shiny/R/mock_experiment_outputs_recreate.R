if (FALSE) {
  patients_id =  c(528, 9813)
  acoustic_params = c(
    'f0_sma',
    'jitterlocal_sma',
    'pcm_fftMag_mfcc_2_',
    'pcm_rmsenergy_sma',
    'audSpec_Rfilt_sma_compare_3_'
  )
  model_name <- c('ssfcm_2patients')
  hyperparams <- list(
    alfa = 1.5,
    beta = 5.5,
    gamma = 10,
    single = FALSE,
    type = 'boost'
  )
  dt_output = data.frame(readRDS("inst/sample_data/validation_df2.rds"))
  confusion_matrix = caret::confusionMatrix(dt_output$predict, dt_output$true,
                                            mode = "everything")

  output = list(
    patients_id = patients_id,
    acoustic_params = acoustic_params,
    model_name = model_name,
    hyperparams = hyperparams,
    dt_output = dt_output,
    confusion_matrix = confusion_matrix
  )

  saveRDS(output, file = "./inst/shiny/www/experiment_outputs_mock.RDS")
}
