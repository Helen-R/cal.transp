### one time thing----
source("~/auxiliary/slackme.R")
source("~/auxiliary/auxiliary.R")
lib("RJSONIO")

## copy files
st <- Sys.time()
# # only copy from skyrim, no update on skyrim
# system("Rscript ~/function/cal.transp/copy.data.R")
# update skyrim then copy from it
system("Rscript ~/function/cal.transp/copy.data.R T")
slackme("copy done", st)

# st <- Sys.time()
# source("copy.data.R")
# slackme("copy done", st)


#### 問題：
#### 如果抓出來的list沒有內容怎麼辦? df return NA, 不存檔
#### c('content', 'status', 'message', 'parent', 'map', 'visible)
#### 'map' 也還沒有處理
#### 數字框的平均高度是否一致（有特別小或特別大的要注意）
#### 大項目和子項目是否都有數字框
#### 檢查長寬比

## read file
if (system("hostname", intern=T)=="skyrim3") {
  prj.wd <- "~/cal.transp/"
} else { # mmnet-dl
  prj.wd <- "~/function/cal.transp/"
}
source(sprintf("%s/check.wd.R", prj.wd), chdir=T)
setwd(paste(data.wd, "reports", sep="/"))
fls <- list.files(pattern=".json")
print(sprintf("In total %d reports created", length(fls)))
head(fls)

# # check one single file
# j <- fromJSON(fls[1])
# j <- j[["fields"]]
# length(j)
# names(j)

# get json file list
# input path:
#   "~/data/smartdonor/reports"
# output path:
#   "~/data/smartdonor/reports"

library(plyr)
source(paste(prj.wd, "npo_json2csv.R", sep="/"))
# k <- ldply(lapply(fls, json2csv))

# convert json to csv
#   json2csv(f, save)
st <- Sys.time()
# # test
# tmp <- json2csv(fls[34])
k <- ldply(lapply(fls, json2csv, T))
slackme("convert reports to csv done", st)



# generate reviews of the reports
# report list
r.ls <- read.csv(paste(prj.wd, "npo_report_list.csv", sep="/"), stringsAsFactors = F)
setorder(r.ls, npo.id, -yr)
r.ls <- rbind(r.ls[r.ls$npo.id=="npo1421",], r.ls[r.ls$npo.id!="npo1421",])
r.ls <- r.ls[, c("npo.id", "yr")]
k <- k[k$npo.id!="npo0085",]
k$yr <- as.integer(k$yr)
data <- left_join(r.ls, k)
data <- data[, c("npo.id", "yr", "review", "note")]
setwd(prj.wd)
write.csv(data, sprintf("npo_report_review_%s.csv", 
                        strftime(Sys.time(), "%Y-%m-%d_%H-%M")), row.names = F)
setwd(paste0(data.wd, "/reports/"))


# View(df)
# data
# 為何無此項目有的是NA有的是0?
# 哪些NA是沒問題的?
# 是不是visible=FALSE都應該刪除or忽略? No


# standard item list
# get levels (standard items)
# ???standard.item.list???

# # output file name
# # f.out <- "bug_list/buggy_reports.csv"
# f.out <- "buggy_reports.csv"
# # all files
# fls <- list.files("~/data/smartdonor/reports", pattern=".csv", full.names = T)
# csv <- lapply(fls, function(x) {
#   csv <- read.csv(x, stringsAsFactors = F)
# 
#   # 檢查是否有內容 (行數)
#   if (nrow(csv)==0) {
#     write.table(gsub(".csv", "", basename(x)), 
#                 f.out, append=T, col.names=F, row.names=F)
#     file.remove(x)
#   } else if (ncol(csv) < 8) {# 檢查欄位數
#       write.table(paste0("buggy.ncol_", gsub(".csv", "", basename(x))), 
#                   f.out, append=T, col.names=F, row.names=F)
#   }
#   
#   
#     if (length(buggy.id)!=0) {
#       csv <- select(csv, -message)
#       # add [npo_xxxx] column
#       csv$npo <- gsub("_reports.csv", "", basename(x))
#       buggy.csv <- csv[buggy.id, ]
#       write.table(buggy.csv, "bug_list/buggy.txt", row.names=F, append=T, col.names=F, sep=",")
#       
#       # clean csv
#       csv <- csv[-buggy.id, ]
#     }
#     na.i <- which(is.na(csv$parent_item))
#     csv$parent_item[na.i] <- csv$names[na.i]
#     csv$names[na.i] <- NA
#     colnames(csv)[which(colnames(csv)=="parent_item")] <- "l3"
#     colnames(csv)[which(colnames(csv)=="names")] <- "l4"
#     
#     csv.l3 <- csv[which(!csv$l3%in%df.lv$l2),]
#     csv.l3 <- left_join(csv.l3, df.lv)
#     
#     csv.l2 <- csv[which(csv$l3%in%df.lv$l2),]
#     colnames(csv.l2)[which(colnames(csv.l2)=="l3")] <- "l2"
#     csv.l2$l3 <- as.character(NA)
#     csv.l2 <- left_join(csv.l2, df.lv)
#     
#     csv.l2 <- csv.l2[, c("l1", "l2", "l3", "l4", "status", "content")]
#     csv.l3 <- csv.l3[, c("l1", "l2", "l3", "l4", "status", "content")]
#     
#     tmp <- rbind(csv.l2, csv.l3)
#     csv <- full_join(df.lv, tmp)
#     
#     csv$npo <- gsub("reports/", "", strsplit(x, "_")[[1]][2])
#     csv <- left_join(csv, df)
#   }
#   write.csv(csv, x, row.names=F)
#   csv
