library(stringr)
library(composr)

`%!in%` = Negate(`%in%`)

id_enqueteur <- vector()
uuid <- vector()
date_enquete <- vector()
start <- vector()
nom_site <- vector()
nom_question <- vector()
probleme <- vector()

nb_autre <- 0
nb_nsp <- 0
nb_prefere_pas <- 0

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

sm_autre <- names(data)[grep(".[.]autre", names(data))]
sm_nsp <- names(data)[grep(".[.](nsp|ne_sait_pas|je_ne_sais_pas)", names(data))]
sm_prefere_pas <- names(data)[grep(".[.](prefere_pas|ne_souhaite_pas_repondre|je_prefere_ne_pas_repondre)", names(data))]


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
    
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "age_chef_meg")
    probleme <- c(probleme, "age chef menage n est pas pris en compte dans la composition du ménage")
  }else if(data[["sexe_chef_meg"]][i] %!in% temp$sexe_hh & 
           data[["age_chef_meg"]][i] %in% temp$age_hh_cat &
           !is.na(data[["sexe_chef_meg"]][i])){
    
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "sexe_chef_meg")
    probleme <- c(probleme, "sexe chef menage n est pas pris en compte dans la composition du ménage")
  }
  
}

for(i in 1:nrow(data)){
  
temp <- hh_membres_loop %>% filter(X_parent_index == i)

if(data[["sexe_enqueteur"]][i] %in% temp$sexe_hh & 
  data[["age_enquete"]][i] %!in% temp$age_hh){
  
  id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
  uuid <- c(uuid,data[["X_uuid"]][i])
  date_enquete <- c(date_enquete, data[["today"]][i])
  start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
               map_chr(2))
  nom_site <- c(nom_site, data[["liste_enquete"]][i])
  nom_question <- c(nom_question, "age_enquete")
  probleme <- c(probleme, "age enquete n est pas pris en compte dans la composition du ménage")
} else if(data[["sexe_enqueteur"]][i] %!in% temp$sexe_hh & 
          data[["age_enquete"]][i] %in% temp$age_hh){
  
  id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
  uuid <- c(uuid,data[["X_uuid"]][i])
  date_enquete <- c(date_enquete, data[["today"]][i])
  start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
               map_chr(2))
  nom_site <- c(nom_site, data[["liste_enquete"]][i])
  nom_question <- c(nom_question, "sexe_enqueteur")
  probleme <- c(probleme, "sexe enquete n est pas pris en compte dans la composition du ménage")
}

}

for(i in 1:nrow(data)){

  if(data[["chef_menage"]][i] == "Non" & 
     data[["age_enquete"]][i] == data[["age_enquete"]][i] &
     data[["sexe_enqueteur"]][i] == data[["sexe_chef_meg"]][i]){
    
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "chef_menage")
    probleme <- c(probleme, "Le profil démographique de l'enquêté correspond au profil démographique du chef de ménage mais il dit ne pas être le chef de ménage")
  }
  
}


for(i in 1:nrow(enfant_sep_loop)){
  
  temp <- hh_membres_loop %>% filter(X_parent_index == enfant_sep_loop[["X_parent_index"]][i])
  
  if(enfant_sep_loop[["sexe_sep"]][i] %in% temp$sexe_hh & 
     enfant_sep_loop[["age_sep"]][i] %!in% temp$age_hh){
    
    id_enqueteur <- c(id_enqueteur, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,enfant_sep_loop[["X_index"]][i])
    date_enquete <- c(date_enquete, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "age_sep")
    probleme <- c(probleme, "age enfants séparés ne correspondent à personne dans la composition du ménage")
  }else if(enfant_sep_loop[["sexe_sep"]][i] %!in% temp$sexe_hh & 
           enfant_sep_loop[["age_sep"]][i] %in% temp$age_hh){
    
    id_enqueteur <- c(id_enqueteur, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,enfant_sep_loop[["X_index"]][i])
    date_enquete <- c(date_enquete, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "sexe_sep")
    probleme <- c(probleme, "sexe enfants séparés ne correspondent à personne dans la composition du ménage")
  }
  
}


for(i in 1:nrow(data)){
  if(data$enfant_sep_plus[i] == "oui_compte" &
     data$nbre_accueil[i] == 0){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "nbre_accueil")
    probleme <- c(probleme, "Enfants séparés signalé mais 0 comptabilisé")
  }
}

for(i in 1:nrow(data)){
  if(data$distance_acces_cs[i] == "pas_infra" &
     data$barriere_service_sante[i] == "pas_barriere"){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "barriere_service_sante")
    probleme <- c(probleme, "Il n'y a pas d'infrastructure de santé mais le ménage ne signale pas de barrières à l'accès à la santé")
  }
}

for(i in 1:nrow(nbre_pers_decedes_loop)){
  if(nbre_pers_decedes_loop$sexe_pers_decede[i] == "homme" &
     nbre_pers_decedes_loop$cause_deces[i] == "accouchement"){
    id_enqueteur <- c(id_enqueteur, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,nbre_pers_decedes_loop[["X_index"]][i])
    date_enquete <- c(date_enquete, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "cause_deces")
    probleme <- c(probleme, "Décès suite à un accouchement attribué à un homme")
  }
}

for(i in 1:nrow(nbre_pers_decedes_loop)){
  if(nbre_pers_decedes_loop$age_pers_decede[i] < 60 &
     nbre_pers_decedes_loop$cause_deces[i] == "viellesse"){
    id_enqueteur <- c(id_enqueteur, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,nbre_pers_decedes_loop[["X_index"]][i])
    date_enquete <- c(date_enquete, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "cause_deces")
    probleme <- c(probleme, "Décès suite à un accouchement attribué à un homme")
  }
}


for(i in 1:nrow(data)){
  if(data$statut_deplacement[i] == "population_non_deplacee" &
     data$prio_besoins.reunification_familiale[i]==1){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "prio_besoins.reunification_familiale")
    probleme <- c(probleme, "Un ménage non déplacé indique avior besoin de réunification familiale")
  }
}

for(i in 1:nrow(data)){
  if(data$source_nourriture[i] %in% c("assistance", "nourriture_travail_ong") &
     (data$aide_oui_non[i]== "non" | sm_selected(data$aide_recue_type[i], none = c("nourriture")))){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "aide_oui_non")
    probleme <- c(probleme, "Le ménage dit ne pas avoir reçu d'assistance alimentaire ce qui contredit sa source de nourriture")
  }
}

for(i in 1:nrow(data)){
  if(data$source_boire[i] =="eau_camion" &
     (data$aide_oui_non[i]== "non" | sm_selected(data$aide_recue_type[i], none = c("eau")))){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "aide_oui_non")
    probleme <- c(probleme, "Le ménage dit ne pas avoir reçu d'assistance en eau ce qui contredit sa source d'eau de boisson")
  }
}

for(i in 1:nrow(data)){
  if(sm_selected(data$eau_besoins[i], any = c("pas_assez")) &
     (sm_selected(data$prio_besoins[i], none = c("besoin_eha")))){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "prio_besoins")
    probleme <- c(probleme, "Le ménage n'a pas assez d'eau pour boire mais ne demande pas d'assistance dans ce secteur")
  }
}

for(i in 1:nrow(data)){
  if(data$femme_enceinte_allaitante[i] == 0 &
     data$nut_aide[i] %in% c("depistage_domicile_femme","conseil_nut_femme","pec_femme")){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "nut_aide")
    probleme <- c(probleme, "Le ménage demande une assistance pour femme enceinte sans avoir de femme enciente dans le ménage")
  }
}

for(i in 1:nrow(data)){
  if(data$sum_agegrp_0_4_plus[i] == 0 &
     data$nut_aide[i] %in% c("conseil_nut_enfant","depistage_domicile_enfant","pec_enfant")){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "nut_aide")
    probleme <- c(probleme, "Le ménage de mande une assistance pour jeunes enfants sans avoir de jeunes enfants dans le ménage")
  }
}


for(i in 1:nrow(data)){
  if(data$occupation_abris[i] == "proprietaire" &
     data$abna_aide[i] == "argent_loyer" & !is.na(data$abna_aide[i])){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "abna_aide")
    probleme <- c(probleme, "Le ménage a besoin d'argent pour payer son loyer alors qu'il est propriétaire")
  }
}

for(i in 1:nrow(data)){
  if(sm_selected(data$strat_subsistance[i], any = c("retrait_ecole")) &
     data$primaire_garcon[i] == data$sum_agegrp_6_12_hommes[i] &
     data$primaire_fille[i] == data$sum_agegrp_6_12_femmes[i] &
     data$secondaire_garcon[i] == data$sum_agegrp_13_17_hommes[i] &
     data$secondaire_fille[i] == data$sum_agegrp_13_17_femmes[i]){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "strat_subsistance")
    probleme <- c(probleme, "Ménage indique avoir retiré les enfants de l'école alors que tous les enfants sont scolarisés")
  }
}

for(i in 1:nrow(data)){
  if(data$source_revenu[i] == "mendicite" &
     sm_selected(data$strat_subsistance[i], any = c("mendicite"))){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "strat_subsistance")
    probleme <- c(probleme, "Le ménage indique mendicité comme source revenue mais pas dons start_subsistance")
  }
}

for(i in 1:nrow(data)){
  if(data$source_boire[i] == "borne_fontaine" &
     data$source_distance[i] == "acces_menage"){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "source_boire")
    probleme <- c(probleme, "Incohérence entre le type de source d'eau et la distance à la source d'eau")
  }
}

for(i in 1:nrow(data)){
  if(data$source_distance[i] == "acces_menage" &
     sm_selected(data$probleme_appro_eau_type[i], any = c("distance"))){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "probleme_appro_eau_type")
    probleme <- c(probleme, "Incohérence entre la proximité du point d'eau et les problèmes d'approvisionnement (distance)")
  }
}

for(i in 1:nrow(data)){
  if(data$source_distance[i] == "acces_menage" &
     sm_selected(data$probleme_appro_eau_type[i], any = c("acces_dangereux"))){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "probleme_appro_eau_type")
    probleme <- c(probleme, "Incohérence entre la proximité du point d'eau et les problèmes d'approvisionnement(acces_dangereux)")
  }
}

for(i in 1:nrow(data)){
  if(data$infra_latrine[i] == "aucun" &
     data$partage_latrine[i] == "oui"){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "partage_latrine")
    probleme <- c(probleme, "Pratique de défécation à l'air libre mais indique partage des latrines")
  }
}

for(i in 1:nrow(data)){
  if(data$occupation_abris[i] == "proprietaire" &
     data$risque_expulsion[i] == "oui" &
     !is.na(data$occupation_abris[i]) & !is.na(data$risque_expulsion[i])){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "risque_expulsion")
    probleme <- c(probleme, "Incohérence entre la modalité d'occupation de l'abri et le sentiment d'être à risque d'expulsion")
  }
}

for(i in 1:nrow(data)){
  if(data$abris_type[i] == "batiment_public" &
     data$occupation_abris[i] == "proprietaire"){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "occupation_abris")
    probleme <- c(probleme, "Ménage indique être propriétaire de l'abri mais réside dans un bâtiment public")
  }
}

for(i in 1:nrow(data)){
  if(data$tel_type[i] == "aucun" &
     data$mode_info[i] %in% c("telephone","sms")){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "mode_info")
    probleme <- c(probleme, "Le mode d'information préféré nécessite un téléphone alors que le ménage n'en a pas")
  }
}

for(i in 1:nrow(data)){
  if(data$dechet_solide[i] == "parfois_visible" &
     data$rongeur[i] == "parfois_visible" &
     data$animaux_morts[i] == "parfois_visible" &
     data$eau_stagnante[i] == "parfois_visible" &
     data$excrement[i] == "parfois_visible"){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "dechet_solide/rongeur/animaux_morts/eau_stagnante/excrement")
    probleme <- c(probleme, "Tous les types de déchets sont toujours parfois visibles au cours des derniers 7 jours")
  }
}

for(i in 1:nrow(data)){
  if(data$dispo_lavage_main[i] %in% c("non","oui_sans_eau_sans_savon") &
     data$savon_menage[i] %in% c("oui_montre", "oui_pas_montre")){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "savon_menage")
    probleme <- c(probleme, "Incohérence entre la disponibilité d'un dispositf de lavage des mains avec du savon et la possession de savon par le ménage")
  }
}

for(i in 1:nrow(data)){
  for(j in 1:length(data)){
    if(!is.na(data[i,j]) & data[i,j] %in% c("nsp","ne_sait_pas","je_ne_sais_pas")) nb_prefere_pas <- nb_prefere_pas + 1
  }
  for(z in 1:length(sm_prefere_pas)){
    if(data[i,sm_prefere_pas[z]] == 1 & !is.na(data[i,sm_prefere_pas[z]])) nb_prefere_pas = nb_prefere_pas + 1
  }
  if(nb_prefere_pas > 4){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "")
    probleme <- c(probleme, "Nombre important de réponses Je préfère ne pas répondre par enquêteur")
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
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "")
    probleme <- c(probleme, "Nombre important de réponses je ne sais pas par enquêteur")
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
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "")
    probleme <- c(probleme, "Nombre important de réponses autre par enquêteur")
  }
  nb_autre <- 0
}

cleaning_log <- data.frame(id_enqueteur,uuid,date_enquete,start,nom_site,nom_question,probleme)
write.csv(cleaning_log, "./output/cleaning_log.csv")
