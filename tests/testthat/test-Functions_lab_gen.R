test_that("sex_to_latex works", {
  expect_all_equal(c(v_sex_to_latex(c("f","F","female","Female","Fem"))),"\\smallfemale")
  expect_all_equal(c(v_sex_to_latex(c("m","M","male","Male","mal"))),"\\smallmale")
})

test_that("print_header runs without error", {
  
  outfile <- file.path(tempdir(), "test_labels.tex")
  
  expect_no_error(
    print_header(
      file_out = outfile
    )
  )
  expect_true(file.exists(outfile))
})

test_that("print_line runs without error", {
  
  outfile <- file.path(tempdir(), "test_labels.tex")
  ind_list <- mosquito_collection
  print_info <- print_parameters
  
  expect_no_error(
    for (num in 1:nrow(ind_list)) {
      print_line(file_out = outfile , ind_list = ind_list, print_info = print_info, line_n = num)
    }
  )
  expect_true(file.exists(outfile))
})

test_that("print_bottom runs without error", {
  
  outfile <- file.path(tempdir(), "test_labels.tex")
  
  expect_no_error(
    print_bottom(
      file_out = outfile
    )
  )
  expect_true(file.exists(outfile))
})

test_that("create_pdf runs without error", {
  
  outfile <- file.path(tempdir(), "test_labels.tex")
  ind_list <- mosquito_collection
  print_info <- print_parameters
  
  expect_no_error(
    create_pdf(
      file_out = outfile,
      ind_list = ind_list,
      print_info = print_info,
      compile = FALSE
    )
  )
  expect_true(file.exists(outfile))
})

