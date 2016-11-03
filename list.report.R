library(rjson)
library(data.table)
# List the financial reports of npo (if any)
#
# Input:
### 1: colnames.json (need to by update?)
###    need to be pre-processed, delete the first n rows
### 2: npodatas/npoxxxx/fields.json
#
# Output:
### 1: 

if(! "d3" %in% ls()) source("cal.transp.R")

d3 <- cbind(d3, F34 = d0[, "F34"])
# npo with financial reports
fls <- rownames(d3[d3[, "F34"]>0, ])
data.wd <- "/home/_obsolete/kj/www/smartdonor/cache/npodatas/"
if (getwd() != data.wd) {
  prj.wd <- getwd()
  setwd(data.wd)
}


# for (x in fls) {
st <- Sys.time()
tar <- lapply(fls, function (x) {
  cn <- which(x %in% fls)
  if (cn %% 50 == 0) { # counter for console printing by 10 (check [line 38, 46] out)
    print(x)
    flush.console()
  }
  
  j <- fromJSON(file = sprintf("%s/fields.json", x))
  yr <- sapply(j$F34, "[[", "period")
  data.frame(npo.id=x, year=yr, stringsAsFactors = F)
})
tar <- ldply(tar, rbind)
tar <- join(tar, npos)
tar <- tar[, c("npo.id", "npo.name", "yr")]
print(Sys.time() - st)

add <- "http://smartdonor.tw/npo.php?npo="
tar$link.add <- as.integer(substr(tar$npo.id, 4, 7))
tar$input <- ""
tar$review <- ""
tar <- data.table(tar)
tar[, n.yr:=.N, by=npo.id]

tmp <- tar[, .(n.npo=.N), by=yr]
setorder(tmp, yr)

if(getwd()!=prj.wd) setwd(prj.wd)
write.csv(tar, "npo_report_list.csv", row.names = F)
