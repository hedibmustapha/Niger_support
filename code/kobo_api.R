library(readxl)
library(httr)
library(jsonlite)
source_url("https://raw.githubusercontent.com/ppsapkota/kobohr_apitoolbox/master/R/r_func_ps_kobo_utils.R")

kobo_server_url<-"https://kobo.humanitarianresponse.info/"
kc_server_url<-"https://kc.humanitarianresponse.info/"

u <-"xffdjibokobox"
pw <-"xfffZbarzbarx"  
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


data <- readxl::read_excel(tf, 1) %>% type_convert()
hh_membres_loop <- read_excel(tf,sheet = "hh_membres_loop") %>% type_convert()
enfant_sep_loop <- read_excel(tf,sheet = "enfant_sep_loop") %>% type_convert()
nbre_pers_decedes_loop <- read_excel(tf,sheet = "repeat_nbre_pers_decedes") %>% type_convert()
nbre_pers_difficulte_loop <- read_excel(tf,sheet = "repeat_nbre_pers_difficulte") %>% type_convert()