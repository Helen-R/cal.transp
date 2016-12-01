data.wd <- "~/data/smartdonor/fields/"
setwd(data.wd)
fls <- list.files()
fls <- gsub(".json", "", fls)
fls <- fls[fls %in% npos[, "npo.id"]]
# prepare the data frame for data merging
#   F1 F2 F3 F4 F5 F6 ...
# 1 NA NA NA NA NA NA ...
d0 <- data.frame(t(1:nrow(df)))
colnames(d0) <- df[, "col.id"]
d0[1, ] <- NA
cn <- 1
for (x in fls) {
  if (cn %% 1000 == 0) {
    print(x)
    flush.console()
  }
  
  j <- rjson::fromJSON(file = sprintf("%s.json", x))
  
  d <- unlist(j)
  nm <- names(d)
  d <- data.frame(t(as.data.frame(d)))
  colnames(d) <- nm
  
  cn <- cn + 1
}

x2 <- lapply(fls, function(f){
  j <- rjson::fromJSON(file = sprintf("%s.json", f))
  d <- unlist(j)
  nm <- names(d)
  d <- data.frame(t(as.data.frame(d)))
  colnames(d) <- nm
  d
})


for(cn in 1:length(x2)){
  if (cn==1) {
    d0 <- x2[cn]
  } else {
    d0 <- join(d0, x2[cn])
  }  
}
