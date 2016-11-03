data.wd <- "/home/_obsolete/kj/www/smartdonor/cache/npodatas/"
setwd(data.wd)

# copy fields
# ref: [cal.transp.R]
fls <- list.files()
fls <- fls[fls %in% npos[, "npo.id"]]
to.dir <- "~/cal.transp/fields/"
if (!dir.exists(to.dir)) dir.create(to.dir)
# file.copy(from=sprintf("%s/fields.json", fls[1:3]), to=paste0(to.dir, fls[1:3], ".json"))
r <- file.copy(from=sprintf("%s/fields.json", fls), to=paste0(to.dir, fls, ".json"))
# check if results are correct
stopifnot(r==length(fls))

# copy reports
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


# copy npo.json (list of npo name)
file.copy(from = "../npos.json", to = "~/cal.transp/")

if (system("hostname")!="skyrim3") {
  system("sshpass -f /home/helen/auxiliary/pw.txt scp helen@skyrim3:/home/brianpan/www/smartdonor_production/cache/npos.json ~/data/smartdonor/")
}