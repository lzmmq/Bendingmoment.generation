test_data_10 <- round(BendingMoment(1,100,50,2,0.10)$M)
test_data_15 <- round(BendingMoment(1,100,50,2,0.15)$M)

test_that("test if f function is correct", {
  expect_equal(test_data_10, c(0,  600, 1523,  993))
}
)

test_that("test if f function is correct", {
  expect_equal(test_data_15, c(0, 505, 1470, 1045))
}
)