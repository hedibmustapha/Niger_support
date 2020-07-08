library(stringr)
library(composr)

`%!in%` = Negate(`%in%`)

enum_id <- vector()
uuid <- vector()
date <- vector()
start <- vector()
nom_site <- vector()
nom_question <- vector()
issue <- vector()

data <- read.csv("./input/data.csv", 
                 na.strings = c(""," ","n/a","NA",NA),
                 stringsAsFactors = F)

hh_membres_loop <- read.csv("./input/hh_membres_loop.csv", 
                 na.strings = c(""," ","n/a","NA",NA),
                 stringsAsFactors = F)

enfant_sep_loop <- read.csv("./input/enfant_sep_loop.csv", 
                            na.strings = c(""," ","n/a","NA",NA),
                            stringsAsFactors = F)

nbre_pers_decedes_loop <- read.csv("./input/nbre_pers_decedes_loop.csv", 
                            na.strings = c(""," ","n/a","NA",NA),
                            stringsAsFactors = F)


hh_membres_loop <- hh_membres_loop %>% mutate(
  age_hh_cat = case_when(
    between(age_hh,0,12) ~ "moins_13_ans",
    between(age_hh,13,17) ~ "de_13_ans_a_17_ans",
    between(age_hh,18,64) ~ "de_18_ans_a_64_ans",
    TRUE ~ "de_65_ans_a_plus"
  )
)

for(i in 1:nrow(data)){
  
  temp <- hh_membres_loop %>% filter(X_parent_index == i)
  
  if(data[["sexe_chef_meg"]][i] %in% temp$sexe_hh & 
     data[["age_chef_meg"]][i] %!in% temp$age_hh_cat &
     !is.na(data[["sexe_chef_meg"]][i])){
    
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "age_chef_meg")
    issue <- c(issue, "age chef menage n est pris en compte dans la composition du ménage")
  }else if(data[["sexe_chef_meg"]][i] %!in% temp$sexe_hh & 
           data[["age_chef_meg"]][i] %in% temp$age_hh_cat &
           !is.na(data[["sexe_chef_meg"]][i])){
    
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "sexe_chef_meg")
    issue <- c(issue, "sexe chef menage n est pris en compte dans la composition du ménage")
  }
  
}

for(i in 1:nrow(data)){
  
temp <- hh_membres_loop %>% filter(X_parent_index == i)

if(data[["sexe_enqueteur"]][i] %in% temp$sexe_hh & 
  data[["age_enquete"]][i] %!in% temp$age_hh){
  
  enum_id <- c(enum_id, data[["global_enum_id"]][i])
  uuid <- c(uuid,data[["X_uuid"]][i])
  date <- c(date, data[["today"]][i])
  start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
               map_chr(2))
  nom_site <- c(nom_site, data[["liste_enquete"]][i])
  nom_question <- c(nom_question, "age_enquete")
  issue <- c(issue, "age enquete n est pris en compte dans la composition du ménage")
} else if(data[["sexe_enqueteur"]][i] %!in% temp$sexe_hh & 
          data[["age_enquete"]][i] %in% temp$age_hh){
  
  enum_id <- c(enum_id, data[["global_enum_id"]][i])
  uuid <- c(uuid,data[["X_uuid"]][i])
  date <- c(date, data[["today"]][i])
  start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
               map_chr(2))
  nom_site <- c(nom_site, data[["liste_enquete"]][i])
  nom_question <- c(nom_question, "sexe_enqueteur")
  issue <- c(issue, "sexe enquete n est pris en compte dans la composition du ménage")
}

}

for(i in 1:nrow(data)){

  if(data[["chef_menage"]][i] == "Non" & 
     data[["age_enquete"]][i] == data[["age_enquete"]][i] &
     data[["sexe_enqueteur"]][i] == data[["sexe_chef_meg"]][i]){
    
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "chef_menage")
    issue <- c(issue, "Le profil démographique de l'enquêté correspond au profil démographique du chef de ménage mais il dit ne pas être le chef de ménage")
  }
  
}


for(i in 1:nrow(enfant_sep_loop)){
  
  temp <- hh_membres_loop %>% filter(X_parent_index == enfant_sep_loop[["X_parent_index"]][i])
  
  if(enfant_sep_loop[["sexe_sep"]][i] %in% temp$sexe_hh & 
     enfant_sep_loop[["age_sep"]][i] %!in% temp$age_hh){
    
    enum_id <- c(enum_id, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,enfant_sep_loop[["X_index"]][i])
    date <- c(date, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "age_sep")
    issue <- c(issue, "age enfants séparés ne correspondent à personne dans la composition du ménage")
  }else if(enfant_sep_loop[["sexe_sep"]][i] %!in% temp$sexe_hh & 
           enfant_sep_loop[["age_sep"]][i] %in% temp$age_hh){
    
    enum_id <- c(enum_id, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,enfant_sep_loop[["X_index"]][i])
    date <- c(date, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "sexe_sep")
    issue <- c(issue, "sexe enfants séparés ne correspondent à personne dans la composition du ménage")
  }
  
}


for(i in 1:nrow(data)){
  if(data$enfant_sep_plus[i] == "oui_compte" &
     data$nbre_accueil[i] == 0){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "nbre_accueil")
    issue <- c(issue, "Enfants séparés signalé mais 0 comptabilisé")
  }
}

for(i in 1:nrow(data)){
  if(data$distance_acces_cs[i] == "pas_infra" &
     data$barriere_service_sante[i] == "pas_barriere"){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "barriere_service_sante")
    issue <- c(issue, "Il n'y a pas d'infrastructure de santé mais le ménage ne signale pas de barrières à l'accès à la santé")
  }
}

for(i in 1:nrow(nbre_pers_decedes_loop)){
  if(nbre_pers_decedes_loop$sexe_pers_decede[i] == "homme" &
     nbre_pers_decedes_loop$cause_deces[i] == "accouchement"){
    enum_id <- c(enum_id, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,nbre_pers_decedes_loop[["X_index"]][i])
    date <- c(date, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "cause_deces")
    issue <- c(issue, "Décès suite à un accouchement attribué à un homme")
  }
}

for(i in 1:nrow(nbre_pers_decedes_loop)){
  if(nbre_pers_decedes_loop$age_pers_decede[i] < 60 &
     nbre_pers_decedes_loop$cause_deces[i] == "viellesse"){
    enum_id <- c(enum_id, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,nbre_pers_decedes_loop[["X_index"]][i])
    date <- c(date, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "cause_deces")
    issue <- c(issue, "Décès suite à un accouchement attribué à un homme")
  }
}


for(i in 1:nrow(data)){
  if(data$statut_deplacement[i] == "population_non_deplacee" &
     data$prio_besoins.reunification_familiale[i]==1){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "prio_besoins.reunification_familiale")
    issue <- c(issue, "Un ménage non déplacé indique avior besoin de réunification familiale")
  }
}

for(i in 1:nrow(data)){
  if(data$source_nourriture[i] %in% c("assistance", "nourriture_travail_ong") &
     (data$aide_oui_non[i]== "non" | sm_selected(data$aide_recue_type[i], none = c("nourriture")))){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "aide_oui_non")
    issue <- c(issue, "Le ménage dit ne pas avoir reçu d'assistance alimentaire ce qui contredit sa source de nourriture")
  }
}

for(i in 1:nrow(data)){
  if(data$source_boire[i] =="eau_camion" &
     (data$aide_oui_non[i]== "non" | sm_selected(data$aide_recue_type[i], none = c("eau")))){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "aide_oui_non")
    issue <- c(issue, "Le ménage dit ne pas avoir reçu d'assistance en eau ce qui contredit sa source d'eau de boisson")
  }
}

for(i in 1:nrow(data)){
  if(sm_selected(data$eau_besoins[i], any = c("pas_assez")) &
     (sm_selected(data$prio_besoins[i], none = c("besoin_eha")))){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "prio_besoins")
    issue <- c(issue, "Le ménage n'a pas assez d'eau pour boire mais ne demande pas d'assistance dans ce secteur")
  }
}

for(i in 1:nrow(data)){
  if(data$femme_enceinte_allaitante[i] == 0 &
     data$nut_aide[i] %in% c("depistage_domicile_femme","conseil_nut_femme","pec_femme")){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "nut_aide")
    issue <- c(issue, "Le ménage demande une assistance pour femme enceinte sans avoir de femme enciente dans le ménage")
  }
}

for(i in 1:nrow(data)){
  if(data$sum_agegrp_0_4_plus[i] == 0 &
     data$nut_aide[i] %in% c("conseil_nut_enfant","depistage_domicile_enfant","pec_enfant")){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "nut_aide")
    issue <- c(issue, "Le ménage de mande une assistance pour jeunes enfants sans avoir de jeunes enfants dans le ménage")
  }
}


for(i in 1:nrow(data)){
  if(data$occupation_abris[i] == "proprietaire" &
     data$abna_aide[i] == "argent_loyer" & !is.na(data$abna_aide[i])){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "abna_aide")
    issue <- c(issue, "Le ménage a besoin d'argent pour payer son loyer alors qu'il est propriétaire")
  }
}

sm_autre <- names(data)[grep(".[.]autre", names(data))]
sm_nsp <- names(data)[grep(".[.](nsp|ne_sait_pas|je_ne_sais_pas)", names(data))]
sm_prefere_pas <- names(data)[grep(".[.](prefere_pas|ne_souhaite_pas_repondre|je_prefere_ne_pas_repondre)", names(data))]
nb_autre <- 0
nb_nsp <- 0
nb_prefere_pas <- 0

for(i in 1:nrow(data)){
  for(j in 1:length(data)){
    if(!is.na(data[i,j]) & data[i,j] %in% c("nsp","ne_sait_pas","je_ne_sais_pas")) nb_prefere_pas <- nb_prefere_pas + 1
  }
  for(z in 1:length(sm_prefere_pas)){
    if(data[i,sm_prefere_pas[z]] == 1 & !is.na(data[i,sm_prefere_pas[z]])) nb_prefere_pas = nb_prefere_pas + 1
  }
  if(nb_prefere_pas > 4){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "")
    issue <- c(issue, "Nombre important de réponses Je préfère ne pas répondre par enquêteur")
  }
  nb_prefere_pas <- 0
}

for(i in 1:nrow(data)){
  for(j in 1:length(data)){
    if(!is.na(data[i,j]) & data[i,j] %in% c("nsp","ne_sait_pas","je_ne_sais_pas")) nb_nsp <- nb_nsp + 1
  }
  for(z in 1:length(sm_nsp)){
    if(data[i,sm_nsp[z]] == 1 & !is.na(data[i,sm_nsp[z]])) nb_nsp = nb_nsp + 1
  }
  if(nb_nsp > 4){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "")
    issue <- c(issue, "Nombre important de réponses je ne sais pas par enquêteur")
  }
  nb_nsp <- 0
}

for(i in 1:nrow(data)){
  for(j in 1:length(data)){
    if(!is.na(data[i,j]) & data[i,j] =="autre"){nb_autre <- nb_autre + 1}
  }
  for(z in 1:length(sm_autre)){
    if(data[i,sm_autre[z]] == 1 & !is.na(data[i,sm_autre[z]])) nb_autre = nb_autre + 1
  }
  if(nb_autre > 4){
    enum_id <- c(enum_id, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date <- c(date, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "")
    issue <- c(issue, "Nombre important de réponses autre par enquêteur")
  }
  nb_autre <- 0
}



