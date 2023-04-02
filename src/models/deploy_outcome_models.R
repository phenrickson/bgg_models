# deploy outcome models trained previously

# refit outcome models to train plus additional year
# averageweight
averageweight_fit = 
        averageweight_train_fit %>%
        fit(bind_rows(train_imputed,
                      valid_imputed) %>%
                    filter(yearpublished <= end_train_year+1) %>%
                    filter(numweights > 5) %>%
                    filter(usersrated >=25))

# average
average_fit = 
        average_train_fit %>%
        fit(bind_rows(train_imputed,
                      valid_imputed) %>%
                    filter(yearpublished <= end_train_year+1) %>%
                    filter(usersrated >=25))

# usersrated
usersrated_fit = 
        usersrated_train_fit %>%
        fit(bind_rows(train_imputed,
                      valid_imputed) %>%
                    filter(yearpublished <= end_train_year+1) %>%
                    filter(usersrated >=25))


# pin workflows ----------------------------------------------------------


# pin workflow to models board
# averageweight
averageweight_fit  %>%
        pin_write(board = models_board,
                  name = 'averageweight_fit',
                  description = paste('model trained to predict averageweight; trained through', end_train_year+1))

# average
average_fit  %>%
        pin_write(board = models_board,
                  name = 'average_fit',
                  description = paste('model trained to predict average; trained through', end_train_year+1))

# average
usersrated_fit  %>%
        pin_write(board = models_board,
                  name = 'usersrated_fit',
                  description = paste('model trained to predict usersrated; trained through', end_train_year+1))


# pin vetiver -----------------------------------------------------------------



# averageweight -----------------------------------------------------------


# make vetiver version
averageweight_vetiver =
        vetiver::vetiver_model(model = averageweight_fit,
                               model_name = "averageweight_vetiver",
                               description = paste("model trained to predict averageweight; trained through",
                                                   end_train_year+ 1))

# test that it works
testthat::test_that("vetiver model does not error due to package",
                    testthat::expect_no_error(
                            averageweight_vetiver %>%
                                    predict(train_imputed %>%
                                                    sample_n(10)))
)

# pin
vetiver::vetiver_pin_write(board = deployed_board,
                           averageweight_vetiver)



# average -----------------------------------------------------------

# make vetiver version
average_vetiver =
        vetiver::vetiver_model(model = average_fit,
                               model_name = "average_vetiver",
                               description = paste("model trained to predict average; trained through",
                                                   end_train_year+ 1))


# test that it works
testthat::test_that("vetiver model does not error due to package",
                    testthat::expect_no_error(
                            average_vetiver %>%
                                    predict(train_imputed %>%
                                                    sample_n(10)))
)

# pin
vetiver::vetiver_pin_write(board = deployed_board,
                           average_vetiver)



# usersrated -----------------------------------------------------------

# make vetiver version
usersrated_vetiver =
        vetiver::vetiver_model(model = usersrated_fit,
                               model_name = "usersrated_vetiver",
                               description = paste("model trained to predict usersrated; trained through",
                                                   end_train_year+ 1))


# test that it works
testthat::test_that("vetiver model does not error due to package",
                    testthat::expect_no_error(
                            usersrated_vetiver %>%
                                    predict(train_imputed %>%
                                                    sample_n(10)))
)

# pin
vetiver::vetiver_pin_write(board = deployed_board,
                           usersrated_vetiver)

