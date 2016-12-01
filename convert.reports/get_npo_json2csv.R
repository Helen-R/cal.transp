library(rjson)
library(plyr)
library(dplyr)
library(data.table)

# get json file list
# in.dir <- "npodatas"
in.dir <- "/home/_obsolete/kj/www/smartdonor/cache/npodatas"
fdls <- list.dirs(in.dir, recursive=T)
fdls <- fdls[grep("reports", fdls)]
jsls <- sapply(fdls, function(fd){
    jsls <- list.files(fd, pattern="json")
    if (length(jsls) > 0) {
        idx <- sapply(strsplit(jsls, "\\."), length)
        js <- jsls[which(idx==2)]
        if (length(js)==1) {
            js
        } else {
            print(paste0("buggy.mt2files:", fd))
            write.table(paste0("buggy.mt2files:", fd), "bug_list/buggy_reports.csv",
                        row.names=F, col.names=F, append=T)
        }
    }
})
jsls <- unlist(jsls)
nm <- names(jsls)
tm <- sapply(strsplit(nm, "/"), "[[", 2)
write.csv(data.frame(npo=tm, jsls=jsls), "data/npodatas/jsls.csv", row.names=F)

# convert json to csv
json2csv <- function(x) {
    c <- fromJSON(file=paste(names(jsls)[x], jsls[x], sep="/"))
    
    amount <- data.frame(
        amount=unlist(lapply(c$fields, "[[", "content"))
    )
    amount$names <- row.names(amount)
    
    pnames <- data.frame(
        pnames=unlist(lapply(c$fields, "[[", "parent"))
    )
    pnames$names <- row.names(pnames)
    
    status <- data.frame(
        status=unlist(lapply(c$fields, "[[", "status"))
    )
    status$names <- row.names(status)
    
    message <- data.frame(
        message=unlist(lapply(c$fields, "[[", "message"))
    )
    message$names <- row.names(message)
    
    amount <- full_join(amount, message, by="names")
    amount <- full_join(status, amount, by="names")
    amount <- full_join(pnames, amount, by="names")
    
    print(names(jsls)[x])
    flush.console()
    
    n1 <- strsplit(names(jsls)[x], "/")
    dir.out <- "data/npo_reports"
    dir.create(dir.out, showWarnings = F)
    f.out <- paste0(dir.out,"/" , n1[[1]][2], "_reports.csv")
    write.csv(amount, f.out, row.names = F)
    print(f.out)
    flush.console()
}
# json2csv(17)
# k <- sapply(1:length(jsls), json2csv)
# k <- sapply(1:2, json2csv)




# 
# # !!!! don't know why json file can't be read in
# x <- fromJSON(file="npodatas/npo0028/reports/7ac91406460eeb27ee5e6349473c0c78.json")
# 
# 
# 
# 
# 
# # get default colnames
# t <- fromJSON(file="bug_list/report.json")
# v <- vector()
# for (i in 1:length(t)) {
#     v <- c(v, names(t[[i]]$items), unlist(t[[i]]$items))
# }
# 
# 
# # join all reports
# fls <- list.files("data/npo_reports", full.names = T)
# csv <- lapply(fls, function(x) {
#     csv <- read.csv(x, stringsAsFactors = F)
#     if (nrow(csv)>0) {
#         csv$npo <- gsub("_reports.csv", "", basename(x))
#     } else {
#         csv <- gsub("_reports.csv", "", basename(x))
#     }
#     csv
# })
# long.csv <- data.frame()
# buggy.rept <- list()
# for (i in 1:length(csv)) {
#     if (is.null(nrow(csv[[i]]))) {
#         buggy.rept <- c(buggy.rept, csv[[i]])
#     } else if (ncol(csv[[i]])==6) {
#         long.csv <- rbind(long.csv, csv[[i]])
#     } else {
#         long.csv <- join(long.csv, csv[[i]])
#         buggy.rept <- c(buggy.rept, paste0("buggy.ncol_", unique(csv[[i]]$npo)))
#     }
# }
# # dir "reports" exists, but no data inside, or incorrect ncol
# buggy.rept <- unlist(buggy.rept)
# 
# 
# 
# # buggy things:
# buggy.df <- data.frame()
# 
# # 1 status=="$" but amount==NA
# buggy.idx <- 
#     buggy.df <- rbind(buggy.df, long.csv[buggy.idx,])
# long.csv <- long.csv[-buggy.idx,]
# #xxx write.csv(buggy.amnt, "bug_list/buggy_amnt.txt", row.names=F)
# 
# sts.opt <- c("$", "包含子項目欄位", "無此資訊", "已包含於其他欄位")
# # abnormalities
# buggy.id <- which(
#     (long.csv$status=="$"&is.na(long.csv$amount))|
#         (!long.csv$status %in% sts.opt)|
#         (long.csv$status=="無此資訊"&!is.na(long.csv$amount))|
#         (long.csv$status=="已包含於其他欄位"&!is.na(long.csv$amount))|
#         (long.csv$status=="已包含於其他欄位"&!is.na(long.csv$pnames))|
#         (long.csv$status=="包含子項目欄位"&!is.na(long.csv$pnames))|
#         (is.na(long.csv$pnames) & !(long.csv$names) %in% v)
# )
# buggy <- long.csv[buggy.id,]
# write.csv(buggy, "bug_list/buggy.txt", row.names=F)
# 
# long.csv <- long.csv[-buggy.id,]
# 
# # not in v & not NA
# i1 <- which((!long.csv$pnames %in% v)&(!is.na(long.csv$pnames)))
# # pnames is NA & names not in v
# i2 <- which(!long.csv$names[which(is.na(long.csv$pnames))] %in% v)
# 
# tmp <- filter(long.csv, !is.na(pnames)) # %>%
# # mutate(names=paste(pnames, names, sep="-"))
# tmp1 <- long.csv[which(is.na(long.csv$pnames)),]
# tmp1$pnames <- tmp1$names
# tmp1$names <- NA
# tmp2 <- rbind(tmp, tmp1)
# tmp2$pnames <- factor(tmp2$pnames, levels = v)
# setorder(tmp2, npo, pnames)
# tmp2 <- select(tmp2, -message)
# write.csv(tmp2, "data/npo_reports/npo_financial_reports.csv", row.names=F)
# 
# 
# 
# 
# View(tmp2[!is.na(tmp2$names),])
# s <- table(tmp2$names[!is.na(tmp2$names)])
# s[s>1]
# s[s==1]
# length(s[grep("銀行|存", names(s))])
# 
# unique(tmp2$names)
# t <- s[order(s)]
# 
# 
# write.csv(s, "s.csv", row.names=F)

# 
# library(RJSONIO)
# x <- fromJSON(content="3a64fc4df24e7b184ff691973b968dff.json")
# y <- read.csv("data/npo_reports/npo_reports_all.csv")
# z <- toJSON(head(y))
# 
# write.csv(y, "test.csv", row.names=F)


# # get pdf file list
# # in.dir <- "npodatas"
# in.dir <- "/home/_obsolete/kj/www/smartdonor/cache/npodatas"
# fnls <- list.files(in.dir, recursive = T)
# fnls <- fnls[grep("twnpos", fnls)]
