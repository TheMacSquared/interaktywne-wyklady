testthat::test_that("most ggplot→ECharts zachowuje granice przedziałów i osie kategorii", {
  env <- new.env(parent=globalenv())
  sys.source(file.path(stat_root,"R","palette.R"),env)
  sys.source(file.path(stat_root,"R","lc_ggplot_echarts.R"),env)
  df <- data.frame(center=10,lo=8.125,hi=11.875,y=1)
  p <- ggplot2::ggplot(df,ggplot2::aes(y=y)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin=lo,xmax=hi),orientation="y",width=.2) +
    ggplot2::geom_point(ggplot2::aes(x=center)) +
    ggplot2::annotate("rect",xmin=10,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=.1)
  spec <- env$lc_ec_plot_spec(p)
  lines <- Filter(function(e) e$type=="line",spec$panels[[1]]$elements)
  testthat::expect_equal(lines[[1]]$coords[,1],c(8.125,11.875))
  testthat::expect_equal(lines[[2]]$coords[,1],rep(8.125,2))
  testthat::expect_equal(lines[[3]]$coords[,1],rep(11.875,2))
  testthat::expect_true(all(is.finite(unlist(lapply(spec$panels[[1]]$elements,`[[`,"coords")))))
  bar <- ggplot2::ggplot(data.frame(x=c("NIE","TAK"),y=c(20,30)),ggplot2::aes(x,y)) + ggplot2::geom_col()
  axes <- env$lc_ec_plot_spec(bar)$panels[[1]]$x
  testthat::expect_equal(unlist(axes$breaks),c(1,2))
  testthat::expect_equal(unlist(axes$labels),c("NIE","TAK"))
  empty <- ggplot2::ggplot() + ggplot2::scale_y_continuous(breaks=c(1,2),labels=c("A","B")) +
    ggplot2::coord_cartesian(xlim=c(0,1),ylim=c(.4,2.6))
  testthat::expect_equal(unlist(env$lc_ec_plot_spec(empty)$panels[[1]]$y$labels),c("A","B"))
  paired <- patchwork::wrap_plots(p,p,ncol=1,heights=c(2,1))
  panels <- env$lc_ec_plot_spec(paired)$panels
  testthat::expect_length(panels,2)
  testthat::expect_equal(panels[[1]]$box,c(0,0,1,2/3))
  testthat::expect_equal(panels[[2]]$box,c(0,2/3,1,1/3))
})

testthat::test_that("most ggplot→ECharts rysuje pudełka w obu orientacjach i kafelki", {
  env <- new.env(parent=globalenv())
  sys.source(file.path(stat_root,"R","palette.R"),env)
  sys.source(file.path(stat_root,"R","lc_ggplot_echarts.R"),env)
  d <- data.frame(v=c(1,2,3,4,5,10),g="a")
  st <- grDevices::boxplot.stats(d$v)$stats
  shapes <- function(p) env$lc_ec_plot_spec(p)$panels[[1]]$elements
  # Poziomo: pudełko od Q1 do Q3 na osi x, mediana pionową kreską.
  h <- shapes(ggplot2::ggplot(d,ggplot2::aes(v,g))+ggplot2::geom_boxplot(outlier.shape=NA))
  testthat::expect_equal(vapply(h,`[[`,"",'type'),c("rect","line","line","line"))
  testthat::expect_equal(h[[1]]$coords[c(1,3)],c(2.25,4.75))
  testthat::expect_equal(h[[2]]$coords[,1],rep(3.5,2))
  # Pionowo: te same wartości na osi y.
  v <- shapes(ggplot2::ggplot(d,ggplot2::aes(g,v))+ggplot2::geom_boxplot(outlier.shape=NA))
  testthat::expect_equal(v[[1]]$coords[c(2,4)],c(2.25,4.75))
  testthat::expect_equal(v[[2]]$coords[,2],rep(3.5,2))
  # Kafelki: prostokąty o granicach komórek i wypełnieniu ze skali.
  t <- shapes(ggplot2::ggplot(data.frame(a=c("x","y"),b="u",n=1:2),ggplot2::aes(a,b,fill=n))+ggplot2::geom_tile())
  testthat::expect_equal(t[[1]]$coords,c(.5,.5,1.5,1.5))
  testthat::expect_false(identical(t[[1]]$fill,t[[2]]$fill))
})
