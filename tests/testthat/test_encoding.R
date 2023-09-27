context("Data encoding")
library(logitr)

iris$cat <- rep(c('catA', 'catB', 'catC'), nrow(iris))[1:nrow(iris)]
iris$species <- iris$Species

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
  names_species_first <- c(
    "speciesversicolor", "speciesvirginica", "catcatB", "catcatC",
    "speciesversicolor:catcatB", "speciesvirginica:catcatB",
    "speciesversicolor:catcatC", "speciesvirginica:catcatC"
  )
  names_cat_first <- c(
    "catcatB", "catcatC", "speciesversicolor", "speciesvirginica",
    "catcatB:speciesversicolor", "catcatC:speciesversicolor",
    "catcatB:speciesvirginica", "catcatC:speciesvirginica"
  )
  species_cat <- c(
    speciesversicolor = "n", speciesvirginica = "n",
    catcatB = "n", catcatC = "n"
  )
  cat_species <- c(
    catcatB = "n", catcatC = "n",
    speciesversicolor = "n", speciesvirginica = "n"
  )

  x <- recodeData(iris, c("species", "cat", "species*cat"), NULL)
  expect_equal(colnames(x$X), names_species_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(iris, c("cat", "species", "cat*species"), NULL)
  expect_equal(colnames(x$X), names_cat_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(iris, c("cat", "cat*species"), NULL)
  expect_equal(colnames(x$X), names_cat_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(iris, c("cat*species"), NULL)
  expect_equal(colnames(x$X), names_cat_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(iris, c("species", "cat", "cat*species"), NULL)
  expect_equal(colnames(x$X), names_species_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(iris, c("species", "cat", "species*cat"), NULL)
  expect_equal(colnames(x$X), names_species_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(iris, c("species", "cat*species"), NULL)
  expect_equal(colnames(x$X), names_species_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(iris, c("species*cat"), NULL)
  expect_equal(colnames(x$X), names_species_first)
  expect_equal(x$randPars, NULL)

  x <- recodeData(iris,
    c("cat", "species", "cat*species"),
    c(cat = "n", species = "n")
  )
  expect_equal(colnames(x$X), names_cat_first)
  expect_equal(x$randPars, cat_species)

  x <- recodeData(iris,
    c("cat", "species", "cat*species"),
    c(species = "n", cat = "n")
  )
  expect_equal(colnames(x$X), names_cat_first)
  expect_equal(x$randPars, cat_species)

  x <- recodeData(iris,
    c("species", "cat", "cat*species"),
    c(cat = "n", species = "n")
  )
  expect_equal(colnames(x$X), names_species_first)
  expect_equal(x$randPars, species_cat)

  x <- recodeData(iris,
    c("species", "cat", "cat*species"),
    c(species = "n", cat = "n")
  )
  expect_equal(colnames(x$X), names_species_first)
  expect_equal(x$randPars, species_cat)

})
