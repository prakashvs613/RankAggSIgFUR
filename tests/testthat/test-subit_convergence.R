test_that("subiterative convergence works", {
  input_rkgs1 <- matrix(c(1,2,4,3,2,3,1,4,1,3,2,4), ncol=3)
  input_rkgs2 <- matrix(c(3, 2, 5, 4,1,2,3,1,5,4,5,1,3,4,2,1,2,4,5,3), ncol = 4)
  expect_equal(subit_convergence(2, c(1,3,2,4), input_rkgs1), list(ConsensusRanking = c(1,3,2,4), KemenyDistance = 6, tau = 2/3))
  expect_equal(subit_convergence(3, c(1,3,2,4), input_rkgs1), list(ConsensusRanking = c(1,3,2,4), KemenyDistance = 6, tau = 2/3))
  expect_equal(subit_convergence(3, c(1,2,3,4,5), input_rkgs2), list(ConsensusRanking = c(2,1,4,5,3), KemenyDistance = 22, tau = 0.45))
  expect_equal(subit_convergence(4, c(1,2,3,4,5), input_rkgs2), list(ConsensusRanking = c(1,2,4,5,3), KemenyDistance = 22, tau = 0.45))
})

test_that("warning is raised for eta=1", {
  input_rkgs1 <- matrix(c(1,2,4,3,2,3,1,4,1,3,2,4), ncol=3)
  input_rkgs2 <- matrix(c(3, 2, 5, 4,1,2,3,1,5,4,5,1,3,4,2,1,2,4,5,3), ncol = 4)
  expect_warning(subit_convergence(1, c(1,3,2,4), input_rkgs1),"Caution: eta=1 has no effect")
  expect_warning(subit_convergence(1, c(4,3,2,1), input_rkgs1),"Caution: eta=1 has no effect")
  expect_warning(subit_convergence(1, c(1,3,2,4,5), input_rkgs2),"Caution: eta=1 has no effect")
  expect_warning(subit_convergence(1, c(5,4,3,2,1), input_rkgs2),"Caution: eta=1 has no effect")
})

test_that("error is raised for incomplete or incorrect inputs", {
  input_rkgs1 <- matrix(c(1,2,4,3,2,3,1,4,1,3,2,4), ncol=3)
  expect_error(subit_convergence(-1,c(1:4),input_rkgs1))
  expect_error(subit_convergence(2,c(1:3),input_rkgs1))
  expect_error(subit_convergence(5,c(1:4),input_rkgs1))
  expect_error(subit_convergence(2,c(1:4),input_rkgs1[1:2,1:2]))
})
