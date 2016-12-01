library(data.table)
library(dplyr)

df.lv <- read.csv("data/npo_reports/npo_reports_lvl.csv")
dt <- fread("data/npo_reports/npo_reports_all.csv")

# df.name <- fread("npodatas/npo_finance_year.csv")
# setnames(df.name, "filename", "jsls")
# df.name <- df.name[, .(title, year, jsls)]
# jsls <- read.csv("npodatas/jsls.csv")
# jsls$jsls <- gsub(".json", "", jsls$jsls)
# jsls <- data.table(jsls)
# dt1 <- left_join(dt, jsls)
# dt1 <- left_join(dt1, df.name)
# dt <- select(dt1, -jsls)
# setorder(dt, npo)
# write.csv(dt, "data/npo_reports/npo_reports_all.csv", row.names=F)

# number of npo: 125
length(unique(dt$npo))

# number of lv 4: 1386
length(unique(dt$l4))

# number of items of each npo:
hist(table(dt$npo))
table(cut(table(dt$npo), seq(40, 140, 10), labels=4:13))

dt[, item.qty:=.N, by=npo]
View(dt[item.qty>70])

dt1 <- dt[!is.na(l4)]
l4.count <- table(dt1$l4)
l4.count <- l4.count[order(l4.count, decreasing = T)]
dt1[, l4.count:=.N, by=l4]
setorder(dt1, -l4.count, l4, -amount)
write.csv(dt1, "data/npo_reports/npo_finan_summaryByL4.csv", row.names=F)


dt2 <- group_by(dt, npo, l2) %>%
    summarize(ttl.amount=sum(amount, na.rm=T))


dt[, complete:=sum(is.na(status)), by=npo]
View(dt[complete==0])



x <- unique(df.cat$category)
x <- x[!x%in%c("老人福利", "身心障礙福利", "綜合性服務", "社區規劃(營造)", 
               "環境保護", "消費者保護", "文化藝術", "國際合作交流", "動物保護", 
               "兒童青少年福利", "健康醫療", "婦女福利", "家庭福利", "教育與科學", 
               "政府單位", "性別平等", "心理衛生", "人權和平")]
