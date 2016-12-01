if (system("hostname", intern=T)=="skyrim3") {
  prj.wd <- "~/cal.transp"
  # data.wd <- "/home/_obsolete/kj/www/smartdonor/cache/npodatas"
  data.wd <- "/home/brianpan/www/smartdonor_production/cache/npodatas"
} else { # mmnet-dl
  prj.wd <- "~/function/cal.transp"
  data.wd <- "~/data/smartdonor"
}
