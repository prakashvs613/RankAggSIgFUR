test_that("subiterative convergence works", {
  input_rkgs1 <- matrix(c(1,2,4,3,2,3,1,4,1,3,2,4), ncol=3)
  input_rkgs2 <- matrix(c(3, 2, 5, 4,1,2,3,1,5,4,5,1,3,4,2,1,2,4,5,3), ncol = 4)
  expect_equal(fur(input_rkgs1, 2, 1), list(ConsensusRanking = c(1,3,2,4), KemenyDistance = 6, tau = 2/3))
  expect_equal(fur(input_rkgs1, c(2:3),1), list(ConsensusRanking = c(1,3,2,4), KemenyDistance = 6, tau = 2/3))
  expect_equal(fur(input_rkgs2, c(3:4),1), list(ConsensusRanking = c(2,1,4,5,3), KemenyDistance = 22, tau = 0.45))
  expect_equal(fur(input_rkgs2, c(2:3),2), list(ConsensusRanking = c(3,1,4,5,2), KemenyDistance = 22, tau = 0.45))
})

test_that("error is raised for incomplete or incorrect inputs", {
  input_rkgs1 <- matrix(c(1,2,4,3,2,3,1,4,1,3,2,4), ncol=3)
  expect_error(fur(input_rkgs1,2,-1))
  expect_error(fur(input_rkgs1,-1,2))
  expect_error(fur(input_rkgs1,6,2))
  expect_error(fur(input_rkgs1,c(2:6),2))
})
