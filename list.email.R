library(rjson)
library(plyr)
if (system("hostname")=="skyrim3") {
  prj.wd <- "/home/helen/cal.transp"
} else {
  prj.wd <- "/home/helen/function/cal.transp"
}
if(!grepl("cal.transp", getwd())) setwd(prj.wd)
# if(! "d3" %in% ls()) source("cal.transp.R")

fls <- list.files("../../data/smartdonor/fields", full.names = T)

setwd("/home/helen/data/smartdonor")
npo.name <- rjson::fromJSON(file="npos.json")
npos <- t(sapply(npo.name, function(l) {
  c(sprintf("npo%04d", l$nid), l$title)
}))
npos <- as.data.frame(npos)
colnames(npos) <- c("npo.id", "npo.name")

strip <- function(j, cnm) {
  ifelse(length(j[[cnm]])!=0, unlist(j[[cnm]]), "")
}

st <- Sys.time()
tar <- lapply(fls, function (x) {
  nm <- gsub(".json", "", basename(x))
  j <- fromJSON(file = x)
  
  email <- strip(j, "F31")
  tel   <- strip(j, "F27")
  data.frame(npo.id=nm, email=email, tel=tel, stringsAsFactors = F)
})
tar <- ldply(tar, rbind)
tar <- join(npos, tar)

setwd(prj.wd)
write.csv(tmp, "npo_mail_list.csv", row.names=F)
print(Sys.time() - st)


# for (x in fls) {
#   print(nm)
#   flush.console()
#   
#   nm <- gsub(".json", "", basename(x))
#   j <- fromJSON(file = x)
#   
#   email <- strip(j, "F31")
#   tel   <- strip(j, "F27")
# }