testthat::test_that("Correct classes", {
  testthat::expect_true(inherits(nifutheme::scale_color_nifu(), 
                                 what=class(ggplot2::scale_color_discrete())))
  testthat::expect_true(inherits(nifutheme::scale_fill_nifu(), 
                                 what=class(ggplot2::scale_color_discrete())))
  testthat::expect_true(inherits(nifutheme::theme_nifu(), 
                                 what=class(ggplot2::theme_classic())))
  testthat::expect_equal(nifutheme::nifu_cols(), 
                         c("#C84957",
                           "#404040",
                           "#EDE2D2",
                           "#E8AE59",
                           "#2D8E9F",
                           "#DBD2E0"))
})
