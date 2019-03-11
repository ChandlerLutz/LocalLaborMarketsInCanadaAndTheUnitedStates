r.files <- c(
  list.files("Rprograms5/10-Plots/", full.names = TRUE),
  list.files("Rprograms5/20-Bartik_China_Regressions/", full.names = TRUE),
  list.files("Rprograms5/30-DFL/", full.names = TRUE)
)
r.files <- r.files[grepl(".R$", r.files)]

lapply(r.files, function(f) {
  print(f)
  source(f, chdir = TRUE)
})

