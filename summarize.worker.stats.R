# worker stats
if (system("hostname", intern=T)=="skyrim3") {
  prj.wd <- "~/cal.transp/"
} else { # mmnet-dl
  prj.wd <- "~/function/cal.transp/"
}
setwd(prj.wd)
w <- read.csv("npo_worker_review_stats.csv")
w <- w[, 1:6]
w$input <- tolower(w$input)
library(data.table)
library(tidyr)
dt <- data.table(w)
setnames(dt, "X.plan.", "plan")
setorder(dt, input, review)
dt[, `:=` (input=tolower(input), plan=tolower(plan))]

stat <- dt[, .N, by=.(input, review)] %>%
  spread(key=review, value=N, fill=0)
set(stat, i=which(stat$input==""), j=1L, "none")
setnames(stat, colnames(stat), c("input", "new", "x.pdf", "check", "v", "x"))
stat <- mutate(stat, done=rowSums(stat[, 4:6, with=F]))
# add column "plan"
s2 <- dt[, .N, by=plan]
setnames(s2, colnames(s2), c("input", "plan"))
set(s2, i=which(s2$input==""), j=1L, "none")

s3 <- merge(stat, s2, by="input")
s3 <- data.table(s3)
s4 <- s3[, .(input, check, v, x, done, plan)]
s3 <- rbind(data.frame(s3), c("total", colSums(s3[, -1, with=F])))
View(s3)
s4 <- s4[!input %in% c("none", "x", "rex")]
s4 <- data.frame(s4)
# Rex 沒做完韋汝補做的有 6 份
# Rocio 原來做了 40 份
s4[which(s4$input=="韋汝"), "plan"] <- as.integer(s4[which(s4$input=="韋汝"), "plan"]) + 6
s4[which(s4$input=="rocio"), "plan"] <- as.integer(s4[which(s4$input=="rocio"), "plan"]) - 40
s4 <- rbind(s4, c("total", colSums(s4[, -1])))
View(s4)
