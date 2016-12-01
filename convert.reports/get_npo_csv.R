library(rjson)
library(plyr)
library(dplyr)
library(data.table)

source("get_npo_json2csv.R")
k <- sapply(1:length(jsls), json2csv)

options(stringsAsFactors=F)

# get default colnames
t <- fromJSON(file="bug_list/report.json")
# get default level names
df.lv <- data.frame()
for (i in 1:length(t)) {
    df <- data.frame()
    for(j in names(t[[i]]$items)) {
        if (length(t[[i]]$items[[j]])>0) {
            df <- rbind(df, data.frame(l2=j, l3=t[[i]]$items[[j]]))
        }
        df <- rbind(df, data.frame(l2=j, l3=NA))
    }
    df$l1 <- t[[i]]$title
    df.lv <- rbind(df.lv, df)
}
rm(df)
df.lv <- df.lv[, c("l1", "l2", "l3")]
write.csv(df.lv, "data/npo_reports_lvl.csv", row.names=F)
# get all possible level names
v <- c(as.character(unique(df.lv$l2)), as.character(unique(df.lv$l3)))

df.name <- fread("data/npo_finance_year.csv")
setnames(df.name, "filename", "jsls")
df.name <- df.name[, .(title, year, jsls)]
jsls <- read.csv("data/jsls.csv")
jsls$jsls <- gsub(".json", "", jsls$jsls)
jsls <- data.table(jsls)
df <- right_join(df.name, jsls)
df.cat <- read.csv("data/npo_categories.csv")
df.cat <- df.cat[-which(sapply(df.cat$id, nchar)>4),]
df.cat$id <- sprintf("npo%04d", as.integer(df.cat$id))
# dt1 <- left_join(dt, jsls)
# dt1 <- left_join(dt1, df.name)
# dt <- select(dt1, -jsls)
# setorder(dt, npo)


# status options
sts.opt <- c("$", "包含子項目欄位", "無此資訊", "已包含於其他欄位")
# output file name
of <- "bug_list/buggy_reports.csv"
# all files
fls <- list.files("data/npo_reports", pattern="_reports.csv", full.names = T)
csv <- lapply(fls, function(x) {
    csv <- read.csv(x, stringsAsFactors = F)
    print(x)
    flush.console()
    if (nrow(csv)==0) {
        write.table(gsub("_reports.csv", "", basename(x)), 
                    of, append=T, col.names=F, row.names=F)
        file.remove(x)
    } else if (nrow(csv) > 0) {
        if (ncol(csv) < 5) {
            csv$pnames <- NA
            write.table(paste0("buggy.ncol_", gsub("_reports.csv", "", basename(x))), 
                        of, append=T, col.names=F, row.names=F)
        }
        csv[csv$status=="無此資訊"&!is.na(csv$amount), "amount"] <- NA
        buggy.id <- which(
                (csv$status=="$"&is.na(csv$amount))|
                (!csv$status %in% sts.opt)|
                # (csv$status=="無此資訊"&!is.na(csv$amount))|
                (csv$status=="已包含於其他欄位"&!is.na(csv$amount))|
                (csv$status=="已包含於其他欄位"&!is.na(csv$pnames))|
                (csv$status=="包含子項目欄位"&!is.na(csv$pnames))|
                (is.na(csv$pnames) & !(csv$names) %in% v)
        )
        if (length(buggy.id)!=0) {
            csv <- select(csv, -message)
            # add [npo_xxxx] column
            csv$npo <- gsub("_reports.csv", "", basename(x))
            buggy.csv <- csv[buggy.id, ]
            write.table(buggy.csv, "bug_list/buggy.txt", row.names=F, append=T, col.names=F, sep=",")
            
            # clean csv
            csv <- csv[-buggy.id, ]
        }
        na.i <- which(is.na(csv$pnames))
        csv$pnames[na.i] <- csv$names[na.i]
        csv$names[na.i] <- NA
        colnames(csv)[which(colnames(csv)=="pnames")] <- "l3"
        colnames(csv)[which(colnames(csv)=="names")] <- "l4"
        
        csv.l3 <- csv[which(!csv$l3%in%df.lv$l2),]
        csv.l3 <- left_join(csv.l3, df.lv)
        
        csv.l2 <- csv[which(csv$l3%in%df.lv$l2),]
        colnames(csv.l2)[which(colnames(csv.l2)=="l3")] <- "l2"
        csv.l2$l3 <- as.character(NA)
        csv.l2 <- left_join(csv.l2, df.lv)
        
        csv.l2 <- csv.l2[, c("l1", "l2", "l3", "l4", "status", "amount")]
        csv.l3 <- csv.l3[, c("l1", "l2", "l3", "l4", "status", "amount")]
        
        tmp <- rbind(csv.l2, csv.l3)
        csv <- full_join(df.lv, tmp)
        
        csv$npo <- gsub("reports/", "", strsplit(x, "_")[[1]][2])
        csv <- left_join(csv, df)
    }
    write.csv(csv, x, row.names=F)
    csv
})


df <- data.frame()
# for(f in fls) {
for (i in 1:length(fls)) {
    df <- rbind(df, csv[[i]])
}

write.csv(df, "data/npo_reports_all.csv", row.names=F)

dt <- data.table(df)

# test <- fromJSON(file="npodatas/npo0329/reports/548467ead375be9db353f0f97ca2c519.json")
# test1 <- fromJSON(file="npodatas/npo0024/reports/cd17b8fe2875cc41c94bff6f63c431b8.json")
