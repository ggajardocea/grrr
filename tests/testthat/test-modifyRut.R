test_that("modifyRut() pasa a numérico un rut", {
  x1 <- "17994104-k"
  y1 <- 17994104
  x2 <- "17994104k"
  y2 <- 17994104
  expect_identical(modifyRut(x1), y1)
  expect_identical(modifyRut(x2), y2)
})
