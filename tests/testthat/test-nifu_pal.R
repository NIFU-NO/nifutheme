testthat::test_that("Correct classes", {
  testthat::expect_true(inherits(nifutheme::scale_color_nifu(), 
                                 what=class(ggplot2::scale_color_discrete())))
  testthat::expect_true(inherits(nifutheme::scale_fill_nifu(), 
                                 what=class(ggplot2::scale_color_discrete())))
  testthat::expect_true(inherits(nifutheme::theme_nifu(), 
                                 what=class(ggplot2::theme_classic())))
  testthat::expect_true(inherits(nifutheme::nifu_theme(), 
                                 what=class(list())))
  testthat::expect_equal(nifutheme::nifu_cols(), 
                         c("#C84957",
                           "#404040",
                           "#EDE2D2",
                           "#E8AE59",
                           "#2D8E9F",
                           "#DBD2E0"))
  plot <- 
    ggplot2::ggplot(data=mtcars,
                    mapping = ggplot2::aes(x=mpg, 
                                           y=hp, 
                                           fill=as.factor(cyl),
                                           colour = as.factor(cyl))) +
    ggplot2::geom_point() + 
    nifutheme::nifu_theme(base_size = 14)
  # testthat::expect_snapshot(x = plot)
})
