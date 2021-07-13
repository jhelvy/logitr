context("Data encoding")
library(logitr)

diamonds_test <- subset(ggplot2::diamonds,
    cut %in% c("Ideal", "Good", "Fair") & color %in% c("D", "E", "F"))

test_that("recodeData correctly encodes single, continuous variables", {
  x <- recodeData(yogurt, "price", NULL)
  expect_equal(colnames(x$X), "price")
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, "feat", NULL)
  expect_equal(colnames(x$X), "feat")
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, "price", c(price = "n"))
  expect_equal(colnames(x$X), "price")
  expect_equal(x$randPars, c(price = "n"))
})

test_that("recodeData correctly encodes single, categorical variables", {
  brand <- c(brandhiland = "n", brandweight = "n", brandyoplait = "n")
  brand_weight <- c(brandhiland = "n", brandyoplait = "n", branddannon = "n")
  yogurt_weight <- yogurt
  yogurt_weight$brand <- factor(yogurt_weight$brand, levels = c(
    "weight", "hiland", "yoplait", "dannon"))

  x <- recodeData(yogurt, "brand", NULL)
  expect_equal(colnames(x$X), names(brand))
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, "brand", c(brand = "n"))
  expect_equal(colnames(x$X), names(brand))
  expect_equal(x$randPars, brand)

  # Switch factor order
  x <- recodeData(yogurt_weight, "brand", NULL)
  expect_equal(colnames(x$X), names(brand_weight))
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt_weight, "brand", c(brand = "n"))
  expect_equal(colnames(x$X), names(brand_weight))
  expect_equal(x$randPars, brand_weight)
})

test_that("recodeData correctly encodes continuous + categorical variables", {
  names_price_first <- c("price", "brandhiland", "brandweight", "brandyoplait")
  names_brand_first <- c("brandhiland", "brandweight", "brandyoplait", "price")
  brand <- c(brandhiland = "n", brandweight = "n", brandyoplait = "n")
  price_brand <- c(
    price = "n", brandhiland = "n", brandweight = "n", brandyoplait = "n")

  x <- recodeData(yogurt, c("price", "brand"), NULL)
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("brand", "price"), NULL)
  expect_equal(colnames(x$X), names_brand_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("price", "brand"), c(brand = "n"))
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, brand)

  x <- recodeData(yogurt, c("brand", "price"), c(brand = "n", price = "n"))
  expect_equal(colnames(x$X), names_brand_first)
  expect_equal(x$randPars, price_brand)

  x <- recodeData(yogurt, c("price", "brand"), c(brand = "n", price = "n"))
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, price_brand)

  x <- recodeData(yogurt, c("price", "brand"), c(price = "n", brand = "n"))
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, price_brand)
})

test_that("recodeData correctly encodes interactions between two continuous variables", {
  names_price_first <- c("price", "feat", "price:feat")
  names_feat_first <- c("feat", "price", "feat:price")
  feat_price <- c(feat = "n", price = "n")
  price_feat <- c(price = "n", feat = "n")

  x <- recodeData(yogurt, c("price", "feat", "price*feat"), NULL)
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("feat", "price", "price*feat"), NULL)
  expect_equal(colnames(x$X), names_feat_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("price", "price*feat"), NULL)
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("feat", "price*feat"), NULL)
  expect_equal(colnames(x$X), names_feat_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("price", "feat*price"), NULL)
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("price*feat"), NULL)
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("feat*price"), NULL)
  expect_equal(colnames(x$X), names_feat_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(
    yogurt, c("price", "feat", "price*feat"), c(feat = "n", price = "n"))
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, feat_price)

  x <- recodeData(
    yogurt, c("feat", "price", "price*feat"), c(feat = "n", price = "n"))
  expect_equal(colnames(x$X), names_feat_first)
  expect_equal(x$randPars, feat_price)

  x <- recodeData(
    yogurt, c("feat", "price", "price*feat"), c(price = "n", feat = "n"))
  expect_equal(colnames(x$X), names_feat_first)
  expect_equal(x$randPars, price_feat)
})

test_that("recodeData correctly encodes interactions between one continuous variable and one categorical variable", {
  names_price_first <- c(
    "price", "brandhiland", "brandweight", "brandyoplait",
    "price:brandhiland", "price:brandweight", "price:brandyoplait")
  names_brand_first <- c(
    "brandhiland", "brandweight", "brandyoplait", "price",
    "brandhiland:price", "brandweight:price", "brandyoplait:price")
  brand <- c(brandhiland = "n", brandweight = "n", brandyoplait = "n")
  price_brand <- c(
    price = "n", brandhiland = "n", brandweight = "n", brandyoplait = "n")

  x <- recodeData(yogurt, c("price", "brand", "price*brand"), NULL)
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("price", "price*brand"), NULL)
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("price", "brand*price"), NULL)
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("price*brand"), NULL)
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("brand", "price", "price*brand"), NULL)
  expect_equal(colnames(x$X), names_brand_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("brand", "price*brand"), NULL)
  expect_equal(colnames(x$X), names_brand_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(yogurt, c("brand*price"), NULL)
  expect_equal(colnames(x$X), names_brand_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(
    yogurt, c("price", "brand", "price*brand"), c(brand = "n", price = "n"))
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, price_brand)

  x <- recodeData(
    yogurt, c("brand", "price", "price*brand"), c(brand = "n", price = "n"))
  expect_equal(colnames(x$X), names_brand_first)
  expect_equal(x$randPars, price_brand)

  x <- recodeData(
    yogurt, c("price", "brand", "price*brand"), c(price = "n", brand = "n"))
  expect_equal(colnames(x$X), names_price_first)
  expect_equal(x$randPars, price_brand)

})

test_that("recodeData correctly encodes interactions between two categorical variables", {
  names_color_first <- c(
    "colorE", "colorF", "cutGood", "cutIdeal", "colorE:cutGood",
    "colorF:cutGood", "colorE:cutIdeal", "colorF:cutIdeal")
  names_cut_first <- c(
    "cutGood", "cutIdeal", "colorE", "colorF", "cutGood:colorE",
    "cutIdeal:colorE", "cutGood:colorF", "cutIdeal:colorF")
  cut_color <- c(cutGood = "n", cutIdeal = "n", colorE = "n", colorF = "n")
  color_cut <- c(colorE = "n", colorF = "n", cutGood = "n", cutIdeal = "n")

  x <- recodeData(diamonds_test, c("cut", "color", "cut*color"), NULL)
  expect_equal(colnames(x$X), names_cut_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(diamonds_test, c("cut", "color", "color*cut"), NULL)
  expect_equal(colnames(x$X), names_cut_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(diamonds_test, c("cut", "cut*color"), NULL)
  expect_equal(colnames(x$X), names_cut_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(diamonds_test, c("cut*color"), NULL)
  expect_equal(colnames(x$X), names_cut_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(diamonds_test, c("color", "cut", "cut*color"), NULL)
  expect_equal(colnames(x$X), names_color_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(diamonds_test, c("color", "cut", "color*cut"), NULL)
  expect_equal(colnames(x$X), names_color_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(diamonds_test, c("color", "cut*color"), NULL)
  expect_equal(colnames(x$X), names_color_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(diamonds_test, c("color*cut"), NULL)
  expect_equal(colnames(x$X), names_color_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(
    diamonds_test, c("cut", "color", "cut*color"), c(cut = "n", color = "n"))
  expect_equal(colnames(x$X), names_cut_first)
  expect_equal(x$randPars, cut_color)

  x <- recodeData(
    diamonds_test, c("cut", "color", "cut*color"), c(color = "n", cut = "n"))
  expect_equal(colnames(x$X), names_cut_first)
  expect_equal(x$randPars, cut_color)

  x <- recodeData(
    diamonds_test, c("color", "cut", "cut*color"), c(cut = "n", color = "n"))
  expect_equal(colnames(x$X), names_color_first)
  expect_equal(x$randPars, color_cut)

  x <- recodeData(
    diamonds_test, c("color", "cut", "cut*color"), c(color = "n", cut = "n"))
  expect_equal(colnames(x$X), names_color_first)
  expect_equal(x$randPars, color_cut)

})
