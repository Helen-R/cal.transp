library(rjson)
library(dplyr)
library(data.table)
options(stringsAsFactors = F)

# status options
status.option <- c("$", "包含子項目欄位", "無此資訊", "已包含於其他欄位")
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

# function call example:
# extract.json(c=npoxxxx.[hash_key].json, col="content")
# check.report(csv=data.frame)
# json2csv(f=npoxxxx.[hash_key].json, save=T)

extract.json <- function (fields, column) {
  # extract json data by certain list specified by list name
  # input: 
  #   [c] content
  #     file name
  #     ex. npoxxxx.[hash_key].json
  #   [col] column
  #     list name (will be colnames in output)
  # output: 
  #   [n x 2] data frame
  #   column 1 [data]:content of specific list name
  #   column 2 [item]:financial item name
  if (column %in% "page") {
    js.map <- lapply(fields, "[[", "map")
    # extract the page of the rectangle annotation
    # "pageContainer1-sheet"
    page <- unlist(lapply(js.map, "[[", column))
    page <- gsub("pageContainer", "", page)
    page <- gsub("-sheet", "", page)
    df <- data.frame(data=page, stringsAsFactors = F)
  } else if (column %in% c("width", "height")) {
    js.map <- lapply(fields, "[[", "map")
    rect <- lapply(js.map, "[[", "rect")
    data <- unlist(lapply(rect, "[[", column))
    df <- data.frame(data=data, stringsAsFactors = F)
  } else {
    df <- data.frame(data=unlist(lapply(fields, "[[", column)), 
                     stringsAsFactors = F)
  }
  if (length(df)!=0) {
    df$names <- row.names(df)
  }
  df
}



check.report <- function (df, illegal) {
  # check buggy issues of report and return the # of buggy items
  # input: 
  #   [n x 7] data frame generated from npoxxxx.[hash_key].json by extract.json()
  #     ex. df[, "content", "item", "status", "message", "parent", "page", "height"]
  # output: 
  #   how many buggy items to be tracked (named int array)
  #     incomplete 未輸入完成項目 
  #     child.no.amount 子項目 (有parent name) 沒有金額
  #     unbalance 資產負債表不平衡 
  #     irregular.status "status" 出現不應存在的選項
  #     irregular.item 沒有 parent name (父項目) 但這個 item 不在父項目標準清單中
  #     it.should.be.na 已包含於其他欄位 或 無此資訊 卻有 content (有[非 NA 金額])
  #     there.should.be.child 包含子項目欄位 必須要有子項目 (檢查parent name)
  #     there.should.be.message 已包含於其他欄位 但是沒有 "message"
  
  ### todo:
  ### 多少為無此資訊 (比例不可以過高--> 檢查, 全部都是無此資訊可能也有問題)
  ### 框畫太高或太窄，必須檢查
  
  
  ## 要check的項目：
  note <- NULL
  standard.df <- df[df$item %in% standard.item.list, ]
  n.standard.item <- nrow(standard.df)
  
  #### [$, 包含子項目 的項目] 為 NA
  boo <- is.na(standard.df$content) & standard.df$status %in% status.option[1:2]
  #### [$, 包含子項目 的項目] 為 NA 的數量 ( <= 尚未輸入 )
  incomplete <- sum(boo)
  
  # 忽略沒有該表的資訊
  item.ignore <- NULL
  idx.table.no.info <- NULL
  for (i in 1:4) {
    # 如果整張表都是無此資訊
    if (sum(standard.df$status[standard.df$item %in% get(sprintf("t%d%d", i, i))] == status.option[3])==length(get(sprintf("t%d%d", i, i)))) {
      item.ignore <- c(item.ignore, c(get(sprintf("t%d", i)), get(sprintf("t%d%d", i, i))))
      idx.table.no.info <- c(idx.table.no.info, i)
    }
  }
  
  # 有表 (非無此資訊)、有金額
  if (is.null(item.ignore)) {
    boo1 <- (!is.na(standard.df$content)) & standard.df$status %in% status.option[1:2]
  } else {
    boo1 <- (!standard.df$item %in% item.ignore) & (!is.na(standard.df$content)) & standard.df$status %in% status.option[1:2]
  }
  #### [$, 包含子項目 的項目 非NA金額] == 0 的數量 (不能太多等於 0)
  boo2 <- standard.df$content[boo1]==0
  zero.amount <- sum(boo2, na.rm = T)
  
  #### 已輸入的數量
  n.standard.input <- n.standard.item - (zero.amount + incomplete)
  
  
  ## 檢查
  to.check <- 0
  
  # # 缺某張表
  # if (length(idx.table.no.info) > 0) {
  #   # to.check <- to.check + 1000
  #   note <- c(note, paste(sprintf("無%s", l1[idx.table.no.info]), collapse = ", "))
  # }
  
  ### 標準項目不足 57
  if (n.standard.item != 57) {
    to.check <- to.check + 100
    note <- c(note, sprintf("標準項目共 %d 項", n.standard.item))
  }
  ### 檢查是否太多項為 0 (多少項?? 3, 待統整後可再確認一次)
  if (zero.amount > 3) {
    to.check <- to.check + 10
    note <- c(note, sprintf("標準項目有 %d 項為 0", zero.amount))
  }
  ### 輸入太少項
  if (n.standard.input < 6) {
    to.check <- to.check + 1
    note <- c(note, sprintf("僅輸入了 %d 項", n.standard.input))
  }
  
  ### 未輸入完成項目
  if (n.standard.input == 0) {
    incomplete <- n.standard.input
    note <- c(note, "尚未開始輸入")
  } else if (incomplete > 3) {
    buggy <- standard.df$item[boo][1:3]
    note <- c(note, sprintf("多項未輸入完成: %s...", paste(buggy, collapse=", ")))
  } else if (incomplete > 0) {
    buggy <- standard.df$item[boo]
    note <- c(note, sprintf("%d 項未輸入完成: %s", incomplete, paste(buggy, collapse=", ")))
  }
  
  ### 資產負債表不平衡
  unbalance <- length(illegal)
  if (length(unbalance) > 0) {
    msg <- ifelse(illegal %in% "balance", "資產負債未平衡", illegal)
    note <- c(note, msg)
  }
  
  # 以下為檢查程式的bug
  # ### 子項目 (有parent name) 沒有金額
  # child.no.amount <- sum(!is.na(df$parent_item) & df$status != status.option[1])
  
  # # "status" 出現不應存在的選項
  # irregular.status <- sum(!df$status %in% status.option)
  
  # # 沒有 parent name (父項目) 但這個 item 不在父項目標準清單中
  # irregular.item <- sum(is.na(df$parent_item) & !(df$item) %in% standard.item.list)
  
  # # 已包含於其他欄位 或 無此資訊 卻有 content (有[非 NA 金額])
  # it.should.be.na <- sum(df$status %in% status.option[3:4] & !is.na(df$content))
  
  
  
  # 包含子項目欄位 卻沒有子項目 (檢查該父項目是否有子項目)
  # condtions:
  # c1: "包含子項目欄位"的項目
  c1 <- df$status %in% status.option[2]
  # c2: 子項目
  c2 <- !is.na(df$parent_item)
  if (sum(c1) > 0) { # 有子項目的父項目
    parent.item <- df$item[c1]
    if (sum(c2) > 0) {
      child.item <- df[c2,]
  # c3: 非 NA 子項目 
  # 子項目金額 NA --> 有問題 (暫先忽略) 
      c3 <- !is.na(child.item$content)
      if (sum(c3) > 0) {
        # 的父項目名稱 parent name of non-NA child item
        sub.item <- child.item$parent_item[c3]
        boo <- !parent.item %in% sub.item
        there.should.be.child <- sum(boo)
        if (there.should.be.child > 0) {# 缺子項目的父項目
          buggy <- parent.item[boo]
          if (there.should.be.child > 1) {
            note <- c(note, sprintf("%d 項缺少子項目: %s", there.should.be.child, paste(buggy, collapse = ", ")))
          } else {
            note <- c(note, sprintf("應有子項目: %s", paste(buggy, collapse = ", ")))
          }
        }
      }
      # error: 子項目沒有內容 
      if (sum(!c3) > 0) {
        error.child <- paste(child.item[!c3, c("parent_item", "item")], collapse = "-")
        note <- c(note, sprintf("%d 子項無金額: %s", sum(!c3), error.child))
      }
      
    } else {# 沒子項目 --> 有問題 (有應包含子項目的父項目但是沒有子項目)
      buggy <- parent.item
      if (sum(c1) > 1) {
        note <- c(note, sprintf("%d 項缺少子項目: %s", sum(!c3), paste(buggy, collapse = ", ")))
      } else {
        note <- c(note, sprintf("應有子項目: %s", paste(buggy, collapse = ", ")))
      }
    }
  } else {# 整篇沒有子項目
    there.should.be.child <- 0 # NA?
  }
      
  # 已包含於其他欄位 但是沒有 "message"
  boo <- df$status %in% status.option[4] & nchar(as.character(df$message)) < 1
  there.should.be.message <- sum(boo)
  if (there.should.be.message > 0) {
    buggy <- df$item[boo]
    note <- c(note, sprintf("%d 項應標示包含於哪個欄位: %s", there.should.be.message, paste(buggy, collapse = ", ")))
  }  
  
  
  
  # 檢查畫框
  # 1 是否有框
  # 2 neg.rect: width or height <= 0
  # 3 rect.too.long: aspect ratio < 1
  # 4 rect.too.wide: mean aspect ratio > 8
  
  # 是否有框
  boo <- sum(is.na(df$width))==nrow(df) | sum(is.na(df$height))==nrow(df)
  no.rect <- 0
  neg.rect <- 0
  rect.too.long <- 0
  rect.too.wide <- 0
  
  if (boo) { # no rect at all
    no.rect <- 1
    note <- c(note, "需補畫標示框")
  } else { # 有畫框 --> 檢查框的長寬
    boo <- df$width <= 0 | df$height <= 0
    neg.rect <- sum(boo, na.rm = T)
    if ( neg.rect > 0) {
      buggy <- df$item[which(boo)]
      if (length(buggy <= 3)) {
        note <- c(note, sprintf("標示框長寬是負的或是 0: %s", paste0(buggy, collapse = ", ")))
      } else {
        note <- c(note, sprintf("%d 項標示框長寬是負的或是 0: %s...", paste0(buggy[1:3], collapse = ", ")))
      }
      df <- df[!boo,]
    }
  
    asp.ratio <- df$width / df$height
    boo <- asp.ratio < 1
    rect.too.long <- sum(boo, na.rm = T)
    if (rect.too.long > 0) {
      buggy <- df$item[which(boo)]
      if (length(buggy <= 3)) {
        note <- c(note, sprintf("標示框不可標示多個數字: %s", paste0(buggy, collapse = ", ")))
      } else {
        note <- c(note, sprintf("%d 項標示框標示了多個數字: %s...", paste0(buggy[1:3], collapse = ", ")))
      }
    }
    
    mean.asp.ratio <- mean(asp.ratio, na.rm = T)
    rect.too.wide <- mean.asp.ratio > 8
    if (rect.too.wide) {
      note <- c(note, "標示框寬度都太寬，可能需要修正。")
    }
  }
  
  # bug summary
  result <- c(to.check, incomplete, unbalance, there.should.be.child, 
              there.should.be.message, no.rect, neg.rect, rect.too.long, rect.too.wide)
  names(result) <- c("to.check", "incomplete", "unbalance", "there.should.be.child", 
                     "there.should.be.message", "no.rect", "neg.rect", "rect.too.long", "rect.too.wide")
  result <- list(result, note)
  # result <- c(incomplete, child.no.amount, unbalance, 
  #             irregular.status, there.should.be.child, 
  #             there.should.be.message, it.should.be.na, 
  #             irregular.item)
  # names(result) <- c("incomplete", "child.no.amount", "unbalance", 
  #                    "irregular.status", "there.should.be.child", 
  #                    "there.should.be.message", "it.should.be.na", 
  #                    "irregular.item")
  result
}



json2csv <- function(f, save=F) {
  # convert json to data frame and save into csv format
  # input:
  #   [f]
  #     file name
  #     ex. npoxxxx.[hash_key].json (no folder)
  #   [save]
  #     save the df to csv format or not
  #     if FALSE, return the data frame
  #     if TRUE, return output file name????
  
  ## read in json file
  js <- rjson::fromJSON(file=f)
  fields <- js$fields
  illegal <- js$illegal
  ## convert json format to data frame
  for (column in c("content", "status", "message", "parent", "page", "width", "height")) {
    ## extract column by column
    df1 <- extract.json(fields, column)
    
    if (column == "parent") {
      column <- "parent_item"
    }
    
    # set column names if there is any content
    if (nrow(df1)!=0) {
      ## rename one of the columns
      colnames(df1) <- c(column, "item")
      
      ## join all four content
      if (column == "content") {
        df <- df1 # first df, no need to join
      } else {
        df <- full_join(df, df1, by="item")
      }
    } else if (column=="content") { 
      ## or if the whole table is NA
      df <- NA
      break
    } else {
      
      ## if it's not content(must have content), then fill with NA
      df[column] <- NA
    }
  }
  
  # js.map <- lapply(fields, "[[", "map")
  # # get rect visible information
  # visible <- unlist(lapply(js.map, "[[", "visible"))
  # names(visible)[!visible]
  
  buggy <- check.report(df, illegal)
  # [[1]] # of buggy item
  # [[2]] buggy note
  
  # todo
  # 加上 level 1-4
  
  # split file name
  # from npoxxxx.[hash_key].json --> npoxxxx.[year].csv
  fs <- unlist(strsplit(f, "\\."))
  # f.out[1] ==> npoxxxx
  #      |---1--|----2-----|-3--|
  npo.id <- c(fs[1])
  yr <- ifelse(is.null(js$year), "none", js$year) 
  
  # set output file name
  f.out <- paste(c(npo.id, yr, "csv"), collapse=".")
  
  
  if (save & !is.null(nrow(df))) {
    write.csv(df, f.out, row.names = F)
  }
  
  # what to return?? data frame or result of checking?
  # return(df)
  return(data.frame(npo.id=npo.id, yr=yr,
                    review=ifelse(sum(buggy[[1]][-which(names(buggy[[1]])=="to.check")])!=0, "x", 
                                  ifelse(buggy[[1]]["to.check"]>0, "check", "v")),
                    note=paste(buggy[[2]], collapse = "; ")))
  # if(is.null(df$parent_item)) {
  #   cat(sprintf("%s %d na.parent over %d row %s \n", f.out, sum(is.null(df$parent_item)), nrow(df), f))
  #   flush.console()
  #   return(df)
  # } else {
  #   NA
  # }
}

# problem history:
# 1 wrong terms (illegal "/") (eceptions, manually modify)
#   (Error in rjson::fromJSON(file = f) : incomplete list - missing...)
# 2 whole table is NA (return NA)
