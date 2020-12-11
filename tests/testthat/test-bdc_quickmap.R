Context ("quick map")

polygon.x =rnorm(100, -0,50)
polygon.y =rnorm(100, 20, 30)
group = rnorm(100, 50, 50)
df<-data.frame(group, polygon.x, polygon.y)


test_that("quick_map", {
  bdc_quickmap(df, lon= polygon.x, lat= polygon.y, column_to_map=group)
})

