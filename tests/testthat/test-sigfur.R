test_that("subiterative convergence works", {
  input_rkgs1 <- matrix(c(1,2,4,3,2,3,1,4,1,3,2,4), ncol=3)
  input_rkgs2 <- matrix(c(3, 2, 5, 4,1,2,3,1,5,4,5,1,3,4,2,1,2,4,5,3), ncol = 4)
  expect_equal(sigfur(input_rkgs1, c(2:3), 10, c(2:3), 1), list(ConsensusRanking = c(1,3,2,4), KemenyDistance = 6, tau = 2/3))
  expect_equal(sigfur(input_rkgs1, 2, 50, 3, 1), list(ConsensusRanking = c(1,3,2,4), KemenyDistance = 6, tau = 2/3))
  expect_equal(sigfur(input_rkgs2, c(3:4), 10, c(2:3), 1), list(ConsensusRanking = c(2,1,4,5,3), KemenyDistance = 22, tau = 0.45))
  expect_equal(sigfur(input_rkgs2, 2, 100, 3, 2), list(ConsensusRanking = c(2,1,4,5,3), KemenyDistance = 22, tau = 0.45))
})

test_that("error is raised for incomplete or incorrect inputs", {
  input_rkgs1 <- matrix(c(1,2,4,3,2,3,1,4,1,3,2,4), ncol=3)
  expect_error(sigfur(input_rkgs1, -1, 10, c(2:3), 1))
  expect_error(sigfur(input_rkgs1, c(3:6), 10, c(2:3), 1))
  expect_error(sigfur(input_rkgs1, c(2:3), 0, c(2:3), 1))
  expect_error(sigfur(input_rkgs1, c(2:3), -1, c(2:3), 1))
  expect_error(sigfur(input_rkgs1, c(2:3), 10, -1, 1))
  expect_error(sigfur(input_rkgs1, c(2:3), 10, 6, 1))
  expect_error(sigfur(input_rkgs1, c(2:3), 10, c(2:3), -1))
})

