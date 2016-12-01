if (!"data.wd" %in% ls()) source("check.wd.R")
setwd(data.wd)

npo.name <- rjson::fromJSON(file="../npos.json")
npos <- t(sapply(npo.name, function(l) {
  c(sprintf("npo%04d", l$nid), l$title)
}))
npos <- as.data.frame(npos)
colnames(npos) <- c("npo.id", "npo.name")