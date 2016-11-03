data.wd <- "/home/_obsolete/kj/www/smartdonor/cache/npodatas/"
setwd(data.wd)

# ref: [cal.transp.R]
fls <- list.files()
fls <- fls[fls %in% npos[, "npo.id"]]
to.dir <- "~/cal.transp/fields/"
if (!dir.exists(to.dir)) dir.create(to.dir)
# file.copy(from=sprintf("%s/fields.json", fls[1:3]), to=paste0(to.dir, fls[1:3], ".json"))
r <- file.copy(from=sprintf("%s/fields.json", fls), to=paste0(to.dir, fls, ".json"))
# check if results are correct
stopifnot(r==length(fls))