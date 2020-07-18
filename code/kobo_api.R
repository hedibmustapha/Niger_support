library(readxl)
library(httr)
library(jsonlite)
source_url("https://raw.githubusercontent.com/ppsapkota/kobohr_apitoolbox/master/R/r_func_ps_kobo_utils.R")

kobo_server_url<-"https://kobo.humanitarianresponse.info/"
kc_server_url<-"https://kc.humanitarianresponse.info/"

u <-"djibokobo"
pw <-"Zbarzbar"  
asset_uid <- "awQKkpnqz4hHEnxbhJu4XZ"

type <- "xls"
lang <- "xml"
fields_from_all_versions <- "false"
hierarchy_in_labels <- "false"
group_sep = "."
d_exports<-kobohr_create_export(type=type,
                                lang=lang,
                                fields_from_all_versions=fields_from_all_versions,
                                hierarchy_in_labels=hierarchy_in_labels,
                                group_sep=group_sep,
                                asset_uid=asset_uid,
                                u,
                                pw)
d_exports<-as.data.frame(d_exports)

result<-httr::GET (url=paste0(as.character(d_exports$url),"?format=json"),
                   # body=d,
                   authenticate(u,pw),
                   progress()
)

result_file<-httr::GET (url=jsonlite::fromJSON(rawToChar(result$content))$result,
                        # body=d,
                        authenticate(u,pw),
                        progress()
)

repeat{
  GET(result_file$url, write_disk(tf <- tempfile(fileext = ".xlsx")),
      authenticate(u,pw))
  if(length(content(GET(result_file$url, write_disk(tf <- tempfile(fileext = ".xlsx")),
                        authenticate(u,pw))))>0){
    break
  }
}


data <- read_excel(tf,sheet = "NER2002_MSNA_diffa")
hh_membres_loop <- read_excel(tf,sheet = "hh_membres_loop")
enfant_sep_loop <- read_excel(tf,sheet = "enfant_sep_loop")
nbre_pers_decedes_loop <- read_excel(tf,sheet = "repeat_nbre_pers_decedes")
nbre_pers_difficulte_loop <- read_excel(tf,sheet = "repeat_nbre_pers_difficulte")

write.csv(data, "./temp/main.csv", row.names = F)
write.csv(hh_membres_loop, file = "./temp/hh_membres_loop.csv", row.names = F)
write.csv(enfant_sep_loop, file = "./temp/enfant_sep_loop.csv", row.names = F)
write.csv(nbre_pers_decedes_loop, file = "./temp/nbre_pers_decedes_loop.csv", row.names = F)
write.csv(nbre_pers_difficulte_loop, file = "./temp/nbre_pers_difficulte_loop.csv", row.names = F)

data <- read.csv("./temp/main.csv", stringsAsFactors = F)
hh_membres_loop <- read.csv("./temp/hh_membres_loop.csv", stringsAsFactors = F)
enfant_sep_loop <- read.csv("./temp/enfant_sep_loop.csv", stringsAsFactors = F)
nbre_pers_decedes_loop <- read.csv("./temp/nbre_pers_decedes_loop.csv", stringsAsFactors = F)
nbre_pers_difficulte_loop <- read.csv("./temp/nbre_pers_difficulte_loop.csv", stringsAsFactors = F)
