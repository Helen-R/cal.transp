data.wd <- "/home/_obsolete/kj/www/smartdonor/cache/npodatas"
if (getwd() != data.wd) setwd(data.wd)

# ref: [Smd_nporeports\get_npo_json2csv.R]
in.dir <- data.wd
fls <- list.files(in.dir, recursive=T)
fls <- fls[grep("reports.*.json", fls)]
idx <- sapply(strsplit(basename(fls), "\\."), length)
fls <- fls[which(idx==2)]
tols <- substr(fls, 1, 7)
to.dir <- "~/cal.transp/reports/"
if (!dir.exists(to.dir)) dir.create(to.dir)
r <- file.copy(from=js, to=paste0(to.dir, tols, ".json"))
# check if results are correct
stopifnot(r==length(fls))