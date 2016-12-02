# Calculate the transparency by the concept of idf (inverse document frequency)
#
# Input:
### 1: colnames.json 
###    df: F1=基本資料, F2=服務概況, ...
###    need to manually check if it is outdated
###    might need to be pre-processed, delete the first n rows
### 2: npos.json
###    visible npo list
### 2: ..../npoxxxx.json
#
# Output:
### 1: transp.json --> for the website
### 2: transp.all.csv --> all data for this call, onlyfor reference

# Calculate the transparency by the concept of idf (inverse document frequency)
# reference: "https://zh.wikipedia.org/wiki/TF-IDF"
# 說明：
# (文件 document = 組織, term = 資料項目)
# (wiki 的例子中有 log10 也有 ln，嘗試之後 ln 的 weigth 差距會太大，因此選 log10 較為合適)
# !!!!!!!!!!!!!!!!注意!!!!!!!!!!!!!!!!
# 有手動調整過項目的 weight (肉眼判斷這個項目是否重要)
# 計算方式：
# D = 總家數
# d = 有此項目的家數
# weight = log10 ( D / d )
# 滿分 total = sum ( weight ) = sum ( weight * [1, 1, 1, ...即所有項目都有資料] )
# 
# 該組織獲得分數：
# 原始分數 raw.tp = sum ( weight * 該組織每個項目的有無（0 / 1）) 
# 百分比分數 p.tp = raw.tp / total

if (! "rjson" %in% installed.packages()[,"Package"]) install.packages("rjson")
library(rjson)

# settings:
## whether the colnames (F1=基本資料, F2=服務概況, ...) should be saved
col.update <- F
##  whether the calculated transparency should be saved
save <- T
# set the project directory (for Rscript calling)
# prj.wd <- "/home/helen/cal.transp"
# data.wd <- "/home/brianpan/www/smartdonor_production/cache"

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if (!"prj.wd" %in% ls()) source("check.wd.R")

# get the colnames (F1=基本資料, F2=服務概況, ...)
# prj.wd: "~/function/cal.transp"
# setwd(prj.wd)
if (col.update == T) {
  # get all column names refering to "Fxx"
  f <- rjson::fromJSON(file=file.path(prj.wd, "colnames.json"))
  # process json
  f0 <- lapply(f, rbind)
  # get column 1 ("Fxx")
  f1 <- paste0("F", sapply(f0, "[[", 1))
  # get column 6 (column names)
  f2 <- sapply(f0, "[[", 6)
  # set up the mapping table of column names to column id
  df <- data.frame(col.id=f1, col.nm=f2, stringsAsFactors = F)
  # get "Fxx" of "董事長" & "理事長"
  nm <- df$col.id[grep("事長", df$col.nm)]
  
  write.csv(df, "colnames.csv", row.names = F)
  print("colnames.csv (=Fxx) updated")
  flush.console()
} else {
  df <- read.csv(file.path(prj.wd, "colnames.csv"), stringsAsFactors = F)
  nm <- df$col.id[grep("事長", df$col.nm)]
  print("colnames.csv (=Fxx) read in")
}


# data.wd <- "~/data/smartdonor/fields/"
# setwd(data.wd)
# get the visible npo list
npo.name <- rjson::fromJSON(file=file.path(data.wd, "npos.json"))
npos <- t(sapply(npo.name, function (l) {
  c(sprintf("npo%04d", l$nid), l$title)
}))
npos <- as.data.frame(npos)
colnames(npos) <- c("npo.id", "npo.name")


# get json file list ("fields/npoxxxx.json")
# or get json file list ("npoxxxx/fields.json")
if (system("hostname", intern = T)=="skyrim3") {
  fls <- list.dirs(file.path(data.wd, "npodatas"))
} else {
  fls <- list.files(file.path(data.wd, "fields"))
  fls <- gsub(".json", "", fls)
}
fls1 <- basename(fls)
fls <- fls[which(fls1 %in% npos[, "npo.id"])]

# prepare the data frame for data merging
#   F1 F2 F3 F4 F5 F6 ...
# 1 NA NA NA NA NA NA ...
d0 <- data.frame(t(1:nrow(df)))
colnames(d0) <- df[, "col.id"]
d0[1, ] <- NA

# for merging, and organize the order of columns
st <- Sys.time()
cn <- 0    # counter for console printing by 10 (check [line 38, 46] out)
for (x in fls) {
  if (cn %% 1000 == 0) {
    print(sprintf("%sth %s", cn, basename(x)))
    flush.console()
  }
  
  if (system("hostname", intern = T)=="skyrim3") {
    j <- rjson::fromJSON(file = file.path(x, "fields.json"))
  } else {
    j <- rjson::fromJSON(file = file.path(data.wd, "fields", paste0(x, ".json")))
  }
  
  d <-sapply(j, length)
  o <- order(names(d))
  d <- d[o]
  
  d0[cn + 1, names(d)] <-  d
  cn <- cn + 1
}
print(Sys.time()-st)
# merge columns of "董事長" & "理事長"
d0[, nm[1]] <- d0[, nm[1]] + d0[, nm[2]]


# setup the matrix
## clear matrix to binary format
d <- ifelse(d0 > 0, 1, 0)
## remove unnecessary column "理事長"(F13), "附屬設施"(F17), "出版品"(F23), "郵政劃撥"(F32),
## "捐款住址"(F33), "透明度"(F37)...
### reason:
###   理事長：合併到董事長
###   附屬設施 & 出版品：不一定每家都有
###   郵政劃撥 & 捐款住址：不影響透明度（？）
rm <- c(1:5, 17, 23, 32, 33, 37)
d <- d[, -which(colnames(d) %in% c(nm[2], sprintf("F%s", rm)))]
# replace the column name from "董事長" to "董事長/理事長"
df$col.nm[which(df$col.id==nm[1])] <- "董事長/理事長"
df <- df[-which(df[, "col.id"] %in% c(nm[2], sprintf("F%s", rm))), ]

# set column names of the matrix
colnames(d) <- df$col.nm[match(colnames(d), df$col.id)]

# calculate # of items
# number of npo
d1 <- colSums(d)
# idf calcuation from "tf-idf"
idf <- log10(nrow(d) / d1)
# construct data frame
# number of npo, inverse document frequency
d2 <- data.frame(n.npo=d1, idf=idf)
stopifnot(identical(df[df$col.nm %in% row.names(d2),"col.nm"], row.names(d2)))
df <- cbind(df[df$col.nm %in% row.names(d2),], d2)
# View(df[order(df$n.npo),])

# calculate 100% rating
total <- sum(idf, na.rm=T)
# calculate absolute ratings of each npo
tp <- rowSums(d %*% idf, na.rm=T) # calculate by idf as weight

# order the columns by decreasing idf
ord <- order(idf, decreasing = T)
idf <- idf[ord]
d <- d[, ord]

d <- as.data.frame(d)
d3 <- cbind(n.item=rowSums(d), d)
d3 <- cbind(raw.tp=tp, d3) # transparancy
d3 <- cbind(p.tp=round(tp * 100 / total, 2), d3) # tp tranformed to mode 100/100
rownames(d3) <- fls

d3 <- cbind(npo.name=npos[match(rownames(d3), npos[, "npo.id"]), "npo.name"], d3)
# View(d3)

print(sprintf("Total # document = %s", length(fls)))
flush.console()
print(sprintf("Max ratings = %s", total))
flush.console()

# check and choose proper cut
hist(d3[, "p.tp"], br=100, xlab="Npo transparency", main="Histogram of Transparency")
c1 <- quantile(d3[, "p.tp"], c(0.33, 0.66))[1]
c2 <- quantile(d3[, "p.tp"], c(0.33, 0.66))[2]
offset <- 3
text(c1+offset, sum(d3$p.tp==c1), sum(d3$p.tp==c1))
text(c2+offset, sum(d3$p.tp==c2), sum(d3$p.tp==c2))
plot(d3$p.tp, d3$n.item, cex=0.5, pch=1, xlab="Transparency in percentage", ylab="Number of items completed")
abline(a=0, b=26/100, col="red")

# plot.ecdf(d3[, "p.tp"], pch=".")

# table(cut(d3[, "p.tp"], c(0, 40, 80, 100), labels=3:1))
# table(cut(d3[, "p.tp"], c(0, 60, 80, 100), labels=3:1))
# quantile(d3[, "p.tp"], c(0.33, 0.66))
table(cut(d3[, "p.tp"], c(0, quantile(d3[, "p.tp"], c(0.33, 0.66)), 100), labels=3:1))

# output files
if (save == T) {
  # # output the transparency rank (3, 2, 1) for every npo
  #xxxx rank <- cut(d3[, "p.tp"], c(0, quantile(d3[, "p.tp"], c(0.33, 0.66)), 100), labels=3:1)
  #xxxx writeLines(sprintf("{%s}", paste(fls, rank, sep=":", collapse=",")), "transp.json")
  # output the transparency grades (out of 100)
  writeLines(sprintf("{%s}", paste(fls, d3[, "p.tp"], sep=":", collapse=",")), "transp.json")
  # save raw data for reference
  write.csv(d3, "transp.all.csv", fileEncoding = "utf-8")
  # save necessary data  
  write.table(d3[order(d3[, "p.tp"], decreasing = T), c("npo.name", "p.tp", "raw.tp", "n.item")], "transp.csv", row.names = T, fileEncoding="utf-8")
  print("File saved")
  flush.console()
}
