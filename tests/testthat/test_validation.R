context("validate_data() function")

library(testthat)
library(logitr)

# Helper function to create test data based on yogurt dataset
create_test_data <- function() {
  data(yogurt, package = "logitr")
  return(yogurt)
}

test_that("validate_data works with clean data", {
  data <- create_test_data()

  # Should pass validation
  result <- validate_data(
    data,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "feat", "brand")
  )

  expect_true(result$valid)
  expect_length(result$errors, 0)
  expect_s3_class(result, "logitr_validation")
  expect_true(result$summary$total_observations > 0)
  expect_true(result$summary$total_alternatives > 0)
})

test_that("validate_data catches missing required columns", {
  data <- create_test_data()

  # Remove outcome column
  data_no_outcome <- data[, !names(data) %in% "choice"]
  result <- validate_data(
    data_no_outcome,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl("Missing required columns.*choice", result$errors)))

  # Remove obsID column
  data_no_obsid <- data[, !names(data) %in% "obsID"]
  result <- validate_data(
    data_no_obsid,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl("Missing required columns.*obsID", result$errors)))

  # Missing parameter column
  result <- validate_data(
    data,
    outcome = "choice",
    obsID = "obsID",
    pars = "nonexistent_column"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl(
    "Missing required columns.*nonexistent_column",
    result$errors
  )))
})

test_that("validate_data catches outcome variable issues", {
  data <- create_test_data()

  # Test invalid outcome values (not 0/1)
  data_bad_outcome <- data
  data_bad_outcome$choice[1:5] <- c(2, 3, -1, 0.5, 99)

  result <- validate_data(
    data_bad_outcome,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl("invalid values.*2.*3.*-1.*0.5.*99", result$errors)))

  # Test character outcome variable
  data_char_outcome <- data
  data_char_outcome$choice <- as.character(data_char_outcome$choice)

  result <- validate_data(
    data_char_outcome,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl("must be numeric.*or logical", result$errors)))

  # Test missing outcome values
  data_na_outcome <- data
  data_na_outcome$choice[c(1, 10, 50)] <- NA

  result <- validate_data(
    data_na_outcome,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl("missing values in rows.*1.*10.*50", result$errors)))
})

test_that("validate_data catches multiple choices per observation", {
  data <- create_test_data()

  # Create multiple choices for obsID 1 (rows 1-4 in original data)
  data_multiple_choices <- data
  data_multiple_choices$choice[1:2] <- 1 # Both alternatives 1 and 2 chosen

  result <- validate_data(
    data_multiple_choices,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl("Multiple choices.*found in obsID", result$errors)))
  # Check that diagnostics are stored
  expect_true(!is.null(result$diagnostics$multiple_choice_details))
})

test_that("validate_data catches observations with no choices", {
  data <- create_test_data()

  # Remove choice from obsID 1 (make all choices 0)
  data_no_choices <- data
  obsid_1_rows <- which(data_no_choices$obsID == 1)
  data_no_choices$choice[obsid_1_rows] <- 0

  result <- validate_data(
    data_no_choices,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl("No choices.*found in obsID", result$errors)))
  # Check that diagnostics are stored
  expect_true(!is.null(result$diagnostics$no_choice_details))
})

test_that("validate_data catches non-contiguous obsID blocks", {
  data <- create_test_data()

  # Create non-contiguous obsID pattern
  data_noncontiguous <- data

  # Take first 12 rows (3 observations of 4 alternatives each)
  data_noncontiguous <- data_noncontiguous[1:12, ]

  # Change obsID pattern to create non-contiguous blocks
  # Original: 1,1,1,1, 2,2,2,2, 3,3,3,3
  # Modified: 1,1, 2,2, 1,1, 2,2, 3,3,3,3 (obsID 1 and 2 appear twice)
  data_noncontiguous$obsID <- c(1, 1, 2, 2, 1, 1, 2, 2, 3, 3, 3, 3)

  result <- validate_data(
    data_noncontiguous,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl("non-contiguous blocks", result$errors)))
  # Check that diagnostics are stored
  expect_true(!is.null(result$diagnostics$noncontiguous_details))
})

test_that("validate_data handles missing obsID values", {
  data <- create_test_data()

  # Add missing obsID values
  data_na_obsid <- data
  data_na_obsid$obsID[c(5, 15, 25)] <- NA

  result <- validate_data(
    data_na_obsid,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl(
    "ObsID has missing values in rows.*5.*15.*25",
    result$errors
  )))
})

test_that("validate_data handles parameter column issues", {
  data <- create_test_data()

  # Test missing values in parameter columns
  data_na_pars <- data
  data_na_pars$price[c(1, 10, 50)] <- NA
  data_na_pars$feat[c(2, 20)] <- NA

  result <- validate_data(
    data_na_pars,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "feat", "brand")
  )

  expect_true(result$valid) # Should still be valid, but with warnings
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl(
    "' price '.*missing values.*1.*10.*50",
    result$warnings
  )))
  expect_true(any(grepl("' feat '.*missing values.*2.*20", result$warnings)))

  # Check that parameter info is stored
  expect_true(!is.null(result$summary$parameter_info))
  expect_true("price" %in% names(result$summary$parameter_info))
  expect_true(result$summary$parameter_info$price$na_count == 3)
})

test_that("validate_data validates scalePar correctly", {
  data <- create_test_data()

  # Test non-numeric scalePar
  data_char_scale <- data
  data_char_scale$price <- as.character(data_char_scale$price)

  result <- validate_data(
    data_char_scale,
    outcome = "choice",
    obsID = "obsID",
    scalePar = "price"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl("Scale parameter.*must be numeric", result$errors)))

  # Test missing values in scalePar
  data_na_scale <- data
  data_na_scale$price[c(1, 10)] <- NA

  result <- validate_data(
    data_na_scale,
    outcome = "choice",
    obsID = "obsID",
    scalePar = "price"
  )

  expect_true(result$valid) # Valid but with warnings
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl(
    "Scale parameter.*missing values.*1.*10",
    result$warnings
  )))

  # Check that scalePar info is stored
  expect_true(!is.null(result$summary$scalePar_info))
  expect_equal(result$summary$scalePar_info$na_count, 2)
})

test_that("validate_data validates panel structure", {
  data <- create_test_data()

  # Test missing panelID values
  data_na_panel <- data
  data_na_panel$id[c(1, 50, 100)] <- NA

  result <- validate_data(
    data_na_panel,
    outcome = "choice",
    obsID = "obsID",
    panelID = "id"
  )

  expect_true(result$valid) # Valid but with warnings
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl(
    "Panel ID.*missing values.*1.*50.*100",
    result$warnings
  )))

  # Test panel summary statistics are computed
  result <- validate_data(
    data,
    outcome = "choice",
    obsID = "obsID",
    panelID = "id"
  )

  expect_true(!is.null(result$summary$individuals))
  expect_true(!is.null(result$summary$obs_per_individual))
  expect_s3_class(result, "logitr_validation")
})

test_that("validate_data handles empty or invalid data frames", {
  # Test empty data frame
  empty_data <- data.frame()

  result <- validate_data(
    empty_data,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl("empty", result$errors)))

  # Test non-data.frame input
  result <- validate_data(
    list(a = 1),
    outcome = "choice",
    obsID = "obsID"
  )

  expect_false(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_true(any(grepl("must be a data.frame", result$errors)))
})

test_that("validate_data handles logical outcome variables", {
  data <- create_test_data()

  # Convert to logical
  data_logical <- data
  data_logical$choice <- as.logical(data_logical$choice)

  result <- validate_data(
    data_logical,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_true(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_length(result$errors, 0)
})

test_that("validate_data provides correct summary statistics", {
  data <- create_test_data()

  result <- validate_data(
    data,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "feat", "brand")
  )

  # Check summary statistics make sense
  expect_s3_class(result, "logitr_validation")
  expect_true(result$summary$total_observations > 0)
  expect_true(
    result$summary$total_alternatives > result$summary$total_observations
  )
  expect_true(result$summary$valid_choices == result$summary$total_observations)
  expect_true(!is.null(result$summary$alternatives_per_obs))
  expect_true(!is.null(result$summary$parameter_info))
})

test_that("validate_data handles factor variables correctly", {
  data <- create_test_data()

  # Ensure brand is factor
  data$brand <- as.factor(data$brand)

  result <- validate_data(
    data,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "feat", "brand")
  )

  expect_true(result$valid)
  expect_s3_class(result, "logitr_validation")
  expect_length(result$errors, 0)

  # Check that factor info is properly stored
  expect_equal(result$summary$parameter_info$brand$type, "factor")
  expect_true(result$summary$parameter_info$brand$n_levels > 1)
})

test_that("validate_data handles mixed data type scenarios", {
  data <- create_test_data()

  # Mix of issues: some missing values, but otherwise valid structure
  data_mixed <- data
  data_mixed$price[c(1, 100)] <- NA
  data_mixed$brand[50] <- NA

  result <- validate_data(
    data_mixed,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "feat", "brand")
  )

  expect_true(result$valid) # Should be valid despite warnings
  expect_s3_class(result, "logitr_validation")
  expect_true(length(result$warnings) > 0) # Should have warnings about missing values
  expect_length(result$errors, 0) # No errors
})

test_that("validate_data handles edge cases with obsID gaps", {
  data <- create_test_data()[1:20, ] # Take first 20 rows (5 observations)

  # Create gaps in obsID sequence (1,2,5,7,10) - this should be OK
  data_gaps <- data
  unique_obs <- unique(data_gaps$obsID)
  new_ids <- c(1, 2, 12, 7, 10)

  for (i in seq_along(unique_obs)) {
    data_gaps$obsID[data_gaps$obsID == unique_obs[i]] <- new_ids[i]
  }

  result <- validate_data(
    data_gaps,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_true(result$valid) # Gaps in sequence should be OK
  expect_s3_class(result, "logitr_validation")
  expect_length(result$errors, 0)
})

test_that("validate_data returns appropriate structures", {
  data <- create_test_data()

  # Test that we can capture the returned object
  result <- validate_data(
    data,
    outcome = "choice",
    obsID = "obsID"
  )

  expect_true(is.list(result))
  expect_s3_class(result, "logitr_validation")
  expect_true(all(
    c("valid", "errors", "warnings", "summary", "diagnostics", "data_info") %in%
      names(result)
  ))
})

test_that("print method works correctly", {
  data <- create_test_data()

  # Test print method with valid data
  result <- validate_data(
    data,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "feat", "brand")
  )

  expect_output(print(result), "LOGITR DATA VALIDATION")
  expect_output(print(result), "Data Overview")
  expect_output(print(result), "VALIDATION RESULTS")
  expect_output(print(result), "Data validation PASSED")

  # Test print method with errors
  data_bad <- data
  data_bad$choice[1:2] <- 1 # Multiple choices
  result_bad <- validate_data(data_bad, outcome = "choice", obsID = "obsID")

  expect_output(print(result_bad), "ERRORS found")
  expect_output(print(result_bad), "Multiple choices")
  expect_output(print(result_bad), "Detailed locations")
})
