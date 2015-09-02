context("auto-introspection")

test_that("Automatic Introspection works as intended.", {
  
  ll = list( a=1:3, b=2 )
  
  ll %>% a %>% expect_equal(1:3)
  ll %>% b %>% expect_equal(2)
  
  ll %>% a %>% length %>% expect_equal(3)
  
})
