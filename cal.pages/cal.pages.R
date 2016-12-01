library(RJSONIO)
library(plyr)

j <- RJSONIO::fromJSON("finance_pages.json")
x <- unlist(j)
y <- data.frame(npo.id=names(x), n.page=x, stringsAsFactors = F)
x <- gsub(".", "_", y$npo.id, fixed=T)
x <- strsplit(x, "_")
df <- ldply(x, rbind)
colnames(df) <- c("npo.id", "type", "year")
df$n.page <- y$n.page
df <- df[grepl("report", df$type),]
df <- df[, c("npo.id", "year", "n.page")]

write.csv(df, "npo_pdf_pages.csv", row.names=F)
