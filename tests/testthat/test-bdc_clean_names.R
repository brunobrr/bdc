test_that("test with function example", {
  scientificName = c(
    "Fridericia bahiensis (Schauer ex. DC.) L.G.Lohmann",
    "Peltophorum dubium (Spreng.) Taub. (Griseb.) Barneby",
    "Gymnanthes edwalliana (Pax & K.Hoffm.) Laurenio-Melo & M.F.Sales",
    "LEGUMINOSAE Senna aff. organensis (Glaz. ex Harms) H.S.Irwin & Barneby")
  
  expect_equal(class(bdc_clean_names(sci_names = scientificName)), "data.frame") 
})
