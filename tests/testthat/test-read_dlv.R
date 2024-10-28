test_that("Read DLV check", {
  # read hta301_ae01t_1 to compare
  test_path <- testthat::test_path("testdata/dlv_hta301_ae01t_1.rds")
  test_data <- readRDS(test_path)
  res_data1 <- read_dlv(pgm_name = 'hta301_ae01t_v',
                        pgm_side = 'qc', output_name = '_1',
                        dlv_full_path = "inst/extdata/dlv_read_only.xlsx")
  expect_identical(test_data, res_data)

  # read hta301_ae01t_2 to compare
  test_path2 <- testthat::test_path("testdata/dlv_hta301_ae01t_2.rds")
  test_data2 <- readRDS(test_path2)
  res_data2 <- read_dlv(pgm_name = 'hta301_ae01t_v',
                        pgm_side = 'qc', output_name = '_2',
                        dlv_full_path = "inst/extdata/dlv_read_only.xlsx")
  expect_identical(test_data2, res_data2)
})
