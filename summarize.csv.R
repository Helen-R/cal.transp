source("~/auxiliary/slackme.R")

if (system("hostname", intern=T)=="skyrim3") {
  prj.wd <- "~/cal.transp/"
} else { # mmnet-dl
  prj.wd <- "~/function/cal.transp/"
}
source(sprintf("%s/check.wd.R", prj.wd), chdir=T)


# get standard item level
item.lvl <- read.csv(paste0(prj.wd, "/npo_reports_lvl.csv"), stringsAsFactors = F)
na.idx <- is.na(item.lvl$l3)
standard.item.list <- c(item.lvl$l2[na.idx], item.lvl$l3[!na.idx])
l1 <- unique(item.lvl$l1)
t1 <- item.lvl$l2[is.na(item.lvl$l3) & item.lvl$l1 %in% l1[1]]
t2 <- item.lvl$l2[is.na(item.lvl$l3) & item.lvl$l1 %in% l1[2]]
t3 <- item.lvl$l2[is.na(item.lvl$l3) & item.lvl$l1 %in% l1[3]]
t4 <- item.lvl$l2[is.na(item.lvl$l3) & item.lvl$l1 %in% l1[4]]
t11 <- item.lvl$l3[!is.na(item.lvl$l3) & item.lvl$l1 %in% l1[1]]
t22 <- item.lvl$l3[!is.na(item.lvl$l3) & item.lvl$l1 %in% l1[2]]
t2 <- c(t2, t22[20])
t22 <- t22[-20]
t33 <- item.lvl$l3[!is.na(item.lvl$l3) & item.lvl$l1 %in% l1[3]]
t3 <- c(t3, t33[5])
t33 <- t33[-5]
t44 <- item.lvl$l3[!is.na(item.lvl$l3) & item.lvl$l1 %in% l1[4]]
t4 <- c(t4, t44[c(1, 4, 7)])
t44 <- t44[-c(1, 4, 7)]



setwd(paste(data.wd, "reports", sep="/"))
# read in files
fls <- list.files(pattern=".csv")

df <- lapply(fls[tmp], function(f) {
  df <- read.csv(f)
  f <- unlist(strsplit(f, "\\."))
  # f: list([npoXXXX], [year], "csv")
  df$npo.id <- f[1]
  df$year <- f[2]
  df
})
df <- ldply(df, rbind)
df <- df[, c("npo.id", "year", "item", "width", "height", "content", "status", 
             "message", "parent_item")]

if (sum(df$width<=0|df$height<=0, na.rm = T) > 0) df1 <- df[-which(df$width<=0|df$height<=0),]


# status options
status.option <- c("$", "包含子項目欄位", "無此資訊", "已包含於其他欄位")
# check message
tmp <- df[!is.na(df$message)&df$status%in%status.option[4],]


cols <- c("資產", "負債", "淨值", "收入", "支出", "總計")
amount <- df[df$item %in% cols, c("npo.id", "year", "item", "content")]
amount <- spread(amount, key=item, value=content)
amount <- amount[, c("npo.id", "year", cols)]



four.sheets <- ldply(lapply(fls, function(f) {
  df <- read.csv(f)
  f <- unlist(strsplit(f, "\\."))
  standard.df <- df[df$item %in% standard.item.list, ]
  
  # 忽略沒有該表的資訊  
  idx.table.no.info <- NULL
  for (i in 1:4) {
    # 如果整張表都是無此資訊
    if (sum(standard.df$status[standard.df$item %in% get(sprintf("t%d%d", i, i))] == status.option[3])==length(get(sprintf("t%d%d", i, i)))) {      
      idx.table.no.info <- c(idx.table.no.info, i)
      standard.df <- standard.df[!standard.df$item %in% c(get(sprintf("t%d%d", i, i)), get(sprintf("t%d", i))),]
    }
  }
  if (!is.null(idx.table.no.info)) {
    # which table has info
    idx <- c(1:4)[-idx.table.no.info]
    # to convert to order 10 (10^n)
    idx <- c(4:1)[idx]
    idx <- sprintf("%04d", sum(10^(idx - 1)))
  } else {
    idx <- "1111"
  }
  # f: list([npoXXXX], [year], "csv")
  a <- table(standard.df$status) %>% as.matrix() %>% t()
  b <- table(df$status) %>% as.matrix() %>% t()
  colnames(b) <- paste0("all.", colnames(b))
  df <- data.frame(npo.id=f[1], year=f[2], four.sheets=idx, n.sheets=4 - length(idx.table.no.info), a)
  df <- cbind(df, b)
}), rbind)


tmp <- as.integer(row.names(four.sheets[!is.na(four.sheets$all.流動負債)|!is.na(four.sheets$all.0000)|!is.na(four.sheets$all.辦公費用),]))
df[df$status==c("流動負債", "辦公費用", "0000"),]
