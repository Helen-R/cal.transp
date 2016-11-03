library(rjson)
# Calculate the transparency by the concept of idf (from tf-idf)
#
# Input:
### 1: colnames.json (need to by update?)
###    need to be pre-processed, delete the first n rows
### 2: npodatas/npoxxxx/fields.json
#
# Output:
### 1: transp.json --> for the website
### 2: transp.all.csv --> all data for this call, for reference

save <- F
prj.wd <- "~/cal.transp/"
if (getwd()!= prj.wd) setwd(prj.wd)
# get all column names refering to "Fxx"
f <- rjson::fromJSON(file="colnames.json")
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


data.wd <- "/home/_obsolete/kj/www/smartdonor/cache/npodatas/"
setwd(data.wd)

npo.name <- rjson::fromJSON(file="../npos.json")
npos <- t(sapply(npo.name, function(l) {
  c(sprintf("npo%04d", l$nid), l$title)
}))
npos <- as.data.frame(npos)
colnames(npos) <- c("npo.id", "npo.name")

fls <- list.files()
fls <- fls[fls %in% npos[, "npo.id"]]
# read all npo data
st <- Sys.time()
l <- lapply(fls, function(x) {
  j <- fromJSON(file = sprintf("%s/fields.json", x))
  d2 <- sapply(j, length)
})
print(Sys.time() - st)


# prepare the data frame for data merging
d0 <- data.frame(t(1:nrow(df)))
colnames(d0) <- df[, "col.id"]
d0[1, ] <- NA

st <- Sys.time()
cn <- 0    # counter for console printing by 10 (check [line 38, 46] out)
for (x in fls) {
  if (cn %% 1000 == 0) {
    print(x)
    flush.console()
  }
  
  j <- fromJSON(file = sprintf("%s/fields.json", x))
  
  d <-sapply(j, length)
  o <- order(names(d))
  d <- d[o]

  d0[cn + 1, names(d)] <-  d
  cn <- cn + 1
}
print(cn)
print(Sys.time()-st)
# merge columns of "董事長" & "理事長"
d0[, nm[1]] <- d0[, nm[1]] + d0[, nm[2]]


# setup the matrix
# clear matrix to binary format
d <- ifelse(d0 > 0, 1, 0)

# remove unnecessary column "理事長"(F13), "附屬設施"(F17), "出版品"(F23), "郵政劃撥"(F32),
# "捐款住址"(F33), "透明度"(F37)...
rm <- c(1:5, 17, 23, 32, 33, 37)
d <- d[, -which(colnames(d) %in% c(nm[2], sprintf("F%s", rm)))]

# replace the column name from "董事長" to "董事長/理事長"
df$col.nm[which(df$col.id==nm[1])] <- "董事長/理事長"
df <- df[-which(df[, "col.id"] %in% c(nm[2], sprintf("F%s", rm))), ]

# set column names of the matrix
colnames(d) <- df$col.nm[match(colnames(d), df$col.id)]

# calculate # of items
d1 <- colSums(d)
# d1[order(d1)]
# idf calcuation from "tf-idf"
idf <- log10(nrow(d) / d1)

d2 <- data.frame(n.npo=d1, idf=idf)
df <- cbind(df, d2)
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
print(sprintf("Max ratings = %s", total))

# # check and choose proper cut 
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
setwd(prj.wd)
if (save ==T) {
  # output the transparency rank (3, 2, 1) for every npo
  writeLines(sprintf("{%s}", paste(fls, cut(d3[, "p.tp"], c(0, quantile(d3[, "p.tp"], c(0.33, 0.66)), 100), labels=3:1), sep=":", collapse=",")), "transp.json")
  # save raw data for reference
  write.csv(d3, "transp.all.csv", fileEncoding = "utf-8")
  # save necessary data  
  write.table(d3[order(d3[, "p.tp"], decreasing = T), c("npo.name", "p.tp", "raw.tp", "n.item")], "transp.csv", row.names = T, fileEncoding="utf-8")
}

# # inspect the data
# setwd(data.wd)
# 
# ddd <- as.data.frame(d0)
# dd <- fls[ddd$F34>0]
# 
# 
# 
# x <- list.dirs()
# x <- x[grep("reports", x)]
