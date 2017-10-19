library(APfun)
library(raster)

context("Tests for AProunder")

### INPUT VALUE ALREADY ROUNDED

test_that("AProunder: input values intersect with input interval and do not need to be rounded", {

  expect_equal(AProunder(31.5, interval = 31.5, direction = "closest", snap = 0), 31.5)
  expect_equal(AProunder(31.5, interval = 31.5, direction = "up", snap = 0),      31.5)
  expect_equal(AProunder(31.5, interval = 31.5, direction = "down", snap = 0),    31.5)

  expect_equal(AProunder(25.9, interval = 12.55, direction = "closest", snap = -200), 25.9)
  expect_equal(AProunder(25.9, interval = 12.55, direction = "up", snap = -200),      25.9)
  expect_equal(AProunder(25.9, interval = 12.55, direction = "down", snap = -200),    25.9)
})

test_that("AProunder: interval of 1 with no snapping (closest)", {

  expect_equal(AProunder(0.5, interval = 1, direction = "closest", snap = 0),  1)
  expect_equal(AProunder(0.49, interval = 1, direction = "closest", snap = 0), 0)
  expect_equal(AProunder(1.55, interval = 1, direction = "closest", snap = 0), 2)

  expect_equal(AProunder(-0.5, interval = 1, direction = "closest", snap = 0),  -1)
  expect_equal(AProunder(-0.49, interval = 1, direction = "closest", snap = 0), 0)
  expect_equal(AProunder(-1.55, interval = 1, direction = "closest", snap = 0), -2)
})

test_that("AProunder: interval of 0.1 with no snapping (closest)", {

  expect_equal(AProunder(1.85, interval = 0.1, direction = "closest", snap = 0), 1.9)
  expect_equal(AProunder(1.84, interval = 0.1, direction = "closest", snap = 0), 1.8)
  expect_equal(AProunder(1.54, interval = 0.1, direction = "closest", snap = 0), 1.5)
  expect_equal(AProunder(1.55, interval = 0.1, direction = "closest", snap = 0), 1.6)

  expect_equal(AProunder(1.85, interval = 0.1, direction = "closest", snap = 0), 1.9)
  expect_equal(AProunder(1.84, interval = 0.1, direction = "closest", snap = 0), 1.8)
  expect_equal(AProunder(1.54, interval = 0.1, direction = "closest", snap = 0), 1.5)
  expect_equal(AProunder(1.55, interval = 0.1, direction = "closest", snap = 0), 1.6)
})

test_that("AProunder: interval of 10 with no snapping (multiple directions)", {

  expect_equal(AProunder(5,  interval = 10, direction = "up",      snap = 0), 10)
  expect_equal(AProunder(5,  interval = 10, direction = "down",    snap = 0), 0)
  expect_equal(AProunder(5,  interval = 10, direction = "closest", snap = 0), 10)

  expect_equal(AProunder(-5, interval = 10, direction = "up",      snap = 0), 0)
  expect_equal(AProunder(-5, interval = 10, direction = "down",    snap = 0), -10)
  expect_equal(AProunder(-5, interval = 10, direction = "closest", snap = 0), -10)
})

test_that("AProunder: interval of 10 with snapping (multiple directions)", {

  expect_equal(AProunder(5,  interval = 10, direction = "up",      snap = 0.5), 10.5)
  expect_equal(AProunder(5,  interval = 10, direction = "down",    snap = 0.5), 0.5)
  expect_equal(AProunder(5,  interval = 10, direction = "closest", snap = 0.5), 0.5)

  expect_equal(AProunder(-5, interval = 10, direction = "up",      snap = 0.5), 0.5)
  expect_equal(AProunder(-5, interval = 10, direction = "down",    snap = 0.5), -9.5)
  expect_equal(AProunder(-5, interval = 10, direction = "closest", snap = 0.5), -9.5)
})

test_that("AProunder: interval of 1000 with snapping (multiple directions)", {

  expect_equal(AProunder(5,  interval = 1000, direction = "up",      snap = 0.5), 1000.5)
  expect_equal(AProunder(5,  interval = 1000, direction = "down",    snap = 0.5), 0.5)
  expect_equal(AProunder(5,  interval = 1000, direction = "closest", snap = 0.5), 0.5)

  expect_equal(AProunder(-5, interval = 1000, direction = "up",      snap = -10000.5), -0.5)
  expect_equal(AProunder(-5, interval = 1000, direction = "down",    snap = -10000.5), -1000.5)
  expect_equal(AProunder(-5, interval = 1000, direction = "closest", snap = -10000.5), -0.5)
})

test_that("AProunder: interval of 100 with snapping and uneven values (multiple directions)", {

  expect_equal(AProunder(454, interval = 100, direction = "closest",  snap = 562), 462)
  expect_equal(AProunder(454, interval = 100, direction = "up",       snap = 562), 462)
  expect_equal(AProunder(454, interval = 100, direction = "down",     snap = 562), 362)

  expect_equal(AProunder(-454, interval = 100, direction = "closest", snap = 562), -438)
  expect_equal(AProunder(-454, interval = 100, direction = "up",      snap = 562), -438)
  expect_equal(AProunder(-454, interval = 100, direction = "down",    snap = 562), -538)
})

test_that("AP rounder: 'Extent' object with interval of 1 and default snap of 0", {

  testExt1 <- extent( -5.9, 0.5,  -2.5, 3.1)

  expect_equal(AProunder(testExt1, interval = 1, direction = "closest", snap = 0), extent(-6, 1, -3, 3))
  expect_equal(AProunder(testExt1, interval = 1, direction = "in",      snap = 0), extent(-5, 0, -2, 3))
  expect_equal(AProunder(testExt1, interval = 1, direction = "out",     snap = 0), extent(-6, 1, -3, 4))

})

test_that("AP rounder: 'Extent' object with interval of 0.5 and default snap of 0", {

  testExt2 <- extent(-10.2, 4.9, -7.72, 6.6)

  expect_equal(AProunder(testExt2, interval = 0.5, direction = "closest", snap = 0), extent(-10.0, 5.0, -7.5, 6.5))
  expect_equal(AProunder(testExt2, interval = 0.5, direction = "in",      snap = 0), extent(-10.0, 4.5, -7.5, 6.5))
  expect_equal(AProunder(testExt2, interval = 0.5, direction = "out",     snap = 0), extent(-10.5, 5.0, -8.0, 7.0))

})

test_that("AP rounder: 'Extent' object with interval of 0.4, snap of 0.3, and all negative extent coordinates", {

  testExt3 <- extent(-100.2, -42.1, -71.72, -69.6)

  expect_equal(AProunder(testExt3, interval = 0.4, direction = "closest", snap = 0.3), extent(-100.1, -42.1, -71.7, -69.7))
  expect_equal(AProunder(testExt3, interval = 0.4, direction = "in",      snap = 0.3), extent(-100.1, -42.1, -71.7, -69.7))
  expect_equal(AProunder(testExt3, interval = 0.4, direction = "out",     snap = 0.3), extent(-100.5, -42.1, -72.1, -69.3))

})
