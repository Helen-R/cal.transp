# copy data for further analysis

# data----
# 1 fields.json
# 2 reports
# 3 npos.json
# 4 finance.json

# working directory----
# sk3:
#   data.wd: "/home/brianpan/www/smartdonor_production/cache/npodatas"
#   prj.wd: "~/cal.transp/"
# others:
#   data.wd: "~/data/smartdonor/"
#   prj.wd: "~/function/cal.transp/"

# PS. because we need to change the file name
#     the steps are: sk3(www) -> sk3(helen) -> others(helen)

# example----
#   sk3:
#     system("Rscript ~/cal.transp/copy.data.R")
#     source("~/cal.transp/copy.data.R")
#   others:
#     [update skyrim data]
#     system("Rscript ~/function/cal.transp/copy.data.R T")
#     [only copy skyrim data, no need to update]
#     system("Rscript ~/function/cal.transp/copy.data.R")
#     source("~/function/cal.transp/copy.data.R")
hostname <- system("hostname", intern=T)
if (hostname=="skyrim3") {
  prj.wd <- "~/cal.transp/"
} else { # mmnet-dl
  prj.wd <- "~/function/cal.transp/"
}

if (!"data.wd" %in% ls()) source(sprintf("%s/check.wd.R", prj.wd), chdir=T)

a <- commandArgs(trailingOnly = T)
if (length(a)!=0) {
  copy <- as.logical(a)
} else {
  copy <- F
}


if (hostname=="skyrim3") {
  # 1 copy fields (npo data)
  # ref: [cal.transp.R]
  
  ## get the npo list (leave only available ones)
  if (!"nops" %in% ls()) {
    setwd(prj.wd)
    source("npos.R")
    setwd(data.wd)
  }
  
  ## list all files in npodatas
  fls <- list.files()
  ## leave only the necessary files
  fls <- fls[fls %in% npos[, "npo.id"]]
  ## set (and create) folder
  to.dir <- paste0(prj.wd, "/fields/")
  if (!dir.exists(to.dir)) dir.create(to.dir)
  ## copy files
  r1 <- file.copy(from=sprintf("%s/%s/fields.json", data.wd, fls),
                  to=paste0(to.dir, fls, ".json"),
                  overwrite = T)
  ## check if results are correct
  print(sprintf("In total %s field.json will be copied", sum(r1)))
  flush.console()
  stopifnot(sum(r1) == length(fls))
   
  
  # 2 copy reports
  # ref: [Smd_nporeports\get_npo_json2csv.R]
  ## get file list
  fls <- list.files(data.wd, recursive=T)
  ## keep only reports
  fls <- fls[grep("reports.*.json", fls)]
  ## get rid of history files
  ## length == 3: [hashed key].[timestamp].json (history files)
  ## length == 2: [hashed key].json (to keep)
  f <- basename(fls)
  f <- strsplit(f, "\\.")
  idx <- sapply(f, length)
  fls <- fls[which(idx==2)]
  
  ## get npoxxxx (for renaming)
  tols <- gsub("/reports/", "\\.", fls)
  
  to.dir <- paste0(prj.wd, "/reports/")
  unlink(to.dir, recursive = T) # delete the whole dir
  if (!dir.exists(to.dir)) dir.create(to.dir)
  
  # delete old files
  old <- list.files(to.dir)
  del <- old[!old %in% to]
  if (length(del)){
    cat(sprintf("%s \n", del))
    file.remove(del)
  }
  
  ## copy reports
  to <- paste0(to.dir, tols)
  r2 <- file.copy(from= sprintf("%s/%s", data.wd, fls), to=to, overwrite = T)
  ## check if results are correct
  print(sprintf("In total %s reports will be copied", sum(r2)))
  flush.console()
  stopifnot(sum(r2) == length(fls))
  
  # 3 copy npo.json (list of npo name)
  file.copy(from = "../npos.json", to = prj.wd, overwrite = T)
  
  # 4 copy finance.json (# of pages in each finance report)
  file.copy(from = "../finance.json", to = prj.wd, overwrite = T)
  
  setwd(prj.wd)
} else { # mmnet-dl
  # 0 first do copying on skyrim3
  if (copy) {
    system("sshpass -f /home/helen/auxiliary/pw.txt scp ~/function/cal.transp/copy.data.R helen@skyrim3:~/cal.transp")
    system("sshpass -f /home/helen/auxiliary/pw.txt ssh helen@skyrim3 'Rscript' ~/cal.transp/copy.data.R T")
    print("sk3 copying completed")
    flush.console()
  }
  
  # 1 copy fields
  to.dir <- "~/data/smartdonor/fields/"
  if (!dir.exists(to.dir)) dir.create(to.dir)
  system("sshpass -f /home/helen/auxiliary/pw.txt scp -r helen@skyrim3:~/cal.transp/fields/*.json ~/data/smartdonor/fields/")
  print(sprintf("fields.json sk3 to %s copied", hostname))
  flush.console()
  
  # 2 copy reports
  to.dir <- "~/data/smartdonor/reports/"
  if (!dir.exists(to.dir)) dir.create(to.dir)
  system("sshpass -f /home/helen/auxiliary/pw.txt scp -r helen@skyrim3:~/cal.transp/reports/*.json ~/data/smartdonor/reports/")
  print(sprintf("reports sk3 to %s copied", hostname))
  flush.console()
  
  # 3 copy npo.json (list of npo name)
  system("sshpass -f /home/helen/auxiliary/pw.txt scp helen@skyrim3:~/cal.transp/npos.json ~/data/smartdonor/")
  print(sprintf("npo.json sk3 to %s copied", hostname))
  flush.console()
  
  # 4 copy finance.json (# of pages in each finance report)
  system("sshpass -f /home/helen/auxiliary/pw.txt scp helen@skyrim3:~/cal.transp/finance.json ~/data/smartdonor/")
  print(sprintf("finanical.json (# of pages) sk3 to %s copied", hostname))
  flush.console()
}