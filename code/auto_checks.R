library(stringr)
library(composr)
library(lubridate)
library(dplyr)
library(purrr)
source("./code/functions.R")

time_diff <- vector()
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
i <- 1

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

data$starttime = ymd_hms(data[["start"]])
data$endtime = ymd_hms(data[["end"]])


list_data_byenum <- data %>% 
  split(.$global_enum_id) %>%
  map(~.[order(.$start),])

results <- map(list_data_byenum, survey_tonext_loop,
               i,"starttime", "endtime",
               "mins","X_uuid"," | ", time_diff, uuid, id_enqueteur, 
               "global_enum_id", date_enquete, "today", start, "start", nom_site, "liste_enquete", nom_question)

results <- results[sapply(results, function(x) dim(x)[1]) > 0] %>% bind_rows()

results <- results %>% mutate(
  probleme = case_when(
    between(diff, -5, 0) ~ "temps entre deux enqu�tes successives court",
    diff >0 ~ "l'enquete a commenc� avant la fin de l'enquete precedente",
    TRUE ~ "Rien a signal�"
  )
) %>% select(-diff) %>% filter(probleme == "temps entre deux enqu�tes successives court")


hh_membres_loop <- hh_membres_loop %>% mutate(
  age_hh_cat = case_when(
    between(age_hh,0,12) ~ "moins_13_ans",
    between(age_hh,13,17) ~ "de_13_ans_a_17_ans",
    between(age_hh,18,64) ~ "de_18_ans_a_64_ans",
    TRUE ~ "de_65_ans_a_plus"
  )
)

for(i in 1:nrow(data)){
  
  temp <- hh_membres_loop %>% filter(X_submission__uuid  == data[["X_uuid"]][i])
  
  if((data[["sexe_chef_meg"]][i] %!in% temp$sexe_hh | 
     data[["age_chef_meg"]][i] %!in% temp$age_hh_cat) &
     !is.na(data[["sexe_chef_meg"]][i])){
    
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "age_chef_meg")
    probleme <- c(probleme, "age ou sex du chef menage n est pas pris en compte dans la composition du m�nage")
  }
}

for(i in 1:nrow(data)){
  
temp <- hh_membres_loop %>% filter(X_submission__uuid  == data[["X_uuid"]][i])

if(data[["sexe_enqueteur"]][i] %!in% temp$sexe_hh | 
  data[["age_enquete"]][i] %!in% temp$age_hh){
  
  id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
  uuid <- c(uuid,data[["X_uuid"]][i])
  date_enquete <- c(date_enquete, data[["today"]][i])
  start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
               map_chr(2))
  nom_site <- c(nom_site, data[["liste_enquete"]][i])
  nom_question <- c(nom_question, "age_enquete")
  probleme <- c(probleme, "age ou sex enquete n est pas pris en compte dans la composition du m�nage")
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
    probleme <- c(probleme, "Le profil d�mographique de l'enqu�t� correspond au profil d�mographique du chef de m�nage mais il dit ne pas �tre le chef de m�nage")
  }
  
}


for(i in 1:nrow(enfant_sep_loop)){
  
  temp <- hh_membres_loop %>% filter(X_parent_index == enfant_sep_loop[["X_parent_index"]][i])
  
  if(enfant_sep_loop[["sexe_sep"]][i] %in% temp$sexe_hh & 
     enfant_sep_loop[["age_sep"]][i] %!in% temp$age_hh){
    
    id_enqueteur <- c(id_enqueteur, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,enfant_sep_loop[["X_submission__uuid"]][i])
    date_enquete <- c(date_enquete, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "age_sep")
    probleme <- c(probleme, "age enfants s�par�s ne correspondent � personne dans la composition du m�nage")
  }else if(enfant_sep_loop[["sexe_sep"]][i] %!in% temp$sexe_hh & 
           enfant_sep_loop[["age_sep"]][i] %in% temp$age_hh){
    
    id_enqueteur <- c(id_enqueteur, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,enfant_sep_loop[["X_submission__uuid"]][i])
    date_enquete <- c(date_enquete, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == enfant_sep_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "sexe_sep")
    probleme <- c(probleme, "sexe enfants s�par�s ne correspondent � personne dans la composition du m�nage")
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
    probleme <- c(probleme, "Enfants s�par�s signal� mais 0 comptabilis�")
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
    probleme <- c(probleme, "Il n'y a pas d'infrastructure de sant� mais le m�nage ne signale pas de barri�res � l'acc�s � la sant�")
  }
}

for(i in 1:nrow(nbre_pers_decedes_loop)){
  if(nbre_pers_decedes_loop$sexe_pers_decede[i] == "homme" &
     nbre_pers_decedes_loop$cause_deces[i] == "accouchement"){
    id_enqueteur <- c(id_enqueteur, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,nbre_pers_decedes_loop[["X_submission__uuid"]][i])
    date_enquete <- c(date_enquete, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "cause_deces")
    probleme <- c(probleme, "D�c�s suite � un accouchement attribu� � un homme")
  }
}

for(i in 1:nrow(nbre_pers_decedes_loop)){
  if(nbre_pers_decedes_loop$age_pers_decede[i] < 60 &
     nbre_pers_decedes_loop$cause_deces[i] == "viellesse"){
    id_enqueteur <- c(id_enqueteur, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,nbre_pers_decedes_loop[["X_submission__uuid"]][i])
    date_enquete <- c(date_enquete, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == nbre_pers_decedes_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "cause_deces")
    probleme <- c(probleme, "D�c�s suite � un accouchement attribu� � un homme")
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
    probleme <- c(probleme, "Un m�nage non d�plac� indique avior besoin de r�unification familiale")
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
    probleme <- c(probleme, "Le m�nage dit ne pas avoir re�u d'assistance alimentaire ce qui contredit sa source de nourriture")
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
    probleme <- c(probleme, "Le m�nage dit ne pas avoir re�u d'assistance en eau ce qui contredit sa source d'eau de boisson")
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
    probleme <- c(probleme, "Le m�nage n'a pas assez d'eau pour boire mais ne demande pas d'assistance dans ce secteur")
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
    probleme <- c(probleme, "Le m�nage demande une assistance pour femme enceinte sans avoir de femme enciente dans le m�nage")
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
    probleme <- c(probleme, "Le m�nage de mande une assistance pour jeunes enfants sans avoir de jeunes enfants dans le m�nage")
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
    probleme <- c(probleme, "Le m�nage a besoin d'argent pour payer son loyer alors qu'il est propri�taire")
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
    probleme <- c(probleme, "M�nage indique avoir retir� les enfants de l'�cole alors que tous les enfants sont scolaris�s")
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
    probleme <- c(probleme, "Le m�nage indique mendicit� comme source revenue mais pas dons start_subsistance")
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
    probleme <- c(probleme, "Incoh�rence entre le type de source d'eau et la distance � la source d'eau")
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
    probleme <- c(probleme, "Incoh�rence entre la proximit� du point d'eau et les probl�mes d'approvisionnement (distance)")
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
    probleme <- c(probleme, "Incoh�rence entre la proximit� du point d'eau et les probl�mes d'approvisionnement(acces_dangereux)")
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
    probleme <- c(probleme, "Pratique de d�f�cation � l'air libre mais indique partage des latrines")
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
    probleme <- c(probleme, "Incoh�rence entre la modalit� d'occupation de l'abri et le sentiment d'�tre � risque d'expulsion")
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
    probleme <- c(probleme, "M�nage indique �tre propri�taire de l'abri mais r�side dans un b�timent public")
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
    probleme <- c(probleme, "Le mode d'information pr�f�r� n�cessite un t�l�phone alors que le m�nage n'en a pas")
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
    probleme <- c(probleme, "Tous les types de d�chets sont toujours parfois visibles au cours des derniers 7 jours")
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
    probleme <- c(probleme, "Incoh�rence entre la disponibilit� d'un dispositf de lavage des mains avec du savon et la possession de savon par le m�nage")
  }
}

for(i in 1:nrow(data)){
  if(difftime(as.POSIXct(data[["endtime"]][i]), 
              as.POSIXct(data[["starttime"]][i]), 
              units = "mins") <= 20){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["start"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "nbre_accueil")
    probleme <- c(probleme, "duree d enquete courte")
  }
}

for(i in 1:nrow(data)){
  if(difftime(as.POSIXct(data[["endtime"]][i]), 
              as.POSIXct(data[["starttime"]][i]), 
              units = "mins") > 90){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["start"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "nbre_accueil")
    probleme <- c(probleme, "duree d enquete longue")
  }
}


for(i in 1:nrow(hh_membres_loop)){
  if(as.character(hh_membres_loop[i,c("vaccination_carte","vaccin_rougeole",
                                      "vaccin_bcg","vaccin_dtc3")]) %in% c("oui_avec_carte","non")){
    id_enqueteur <- c(id_enqueteur, data[data$X_index == hh_membres_loop[["X_parent_index"]][i],"global_enum_id"])
    uuid <- c(uuid,hh_membres_loop[["X_submission__uuid"]][i])
    date_enquete <- c(date_enquete, data[data$X_index == hh_membres_loop[["X_parent_index"]][i],"today"])
    start <- c(start, str_split(ymd_hms(data[data$X_index == hh_membres_loop[["X_parent_index"]][i], "start"])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[data$X_index == hh_membres_loop[["X_parent_index"]][i], "liste_enquete"])
    nom_question <- c(nom_question, "vaccination_carte")
    probleme <- c(probleme, "la carte de vaccination de l'enfant a �t� montr� pour certaines questions mais pas d'autres")
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
    probleme <- c(probleme, "Nombre important de r�ponses Je pr�f�re ne pas r�pondre par enqu�teur")
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
    probleme <- c(probleme, "Nombre important de r�ponses je ne sais pas par enqu�teur")
  }
  nb_nsp <- 0
}


for(i in 1:nrow(data)){
  for(j in 1:length(data)){
    if(!is.na(data[i,j]) & data[i,j] %in% c("autre")) nb_autre <- nb_autre + 1
  }
  for(z in 1:length(sm_autre)){
    if(data[i,sm_autre[z]] == 1 & !is.na(data[i,sm_autre[z]])) nb_autre <- nb_autre + 1
  }
  if(nb_autre > 4){
    id_enqueteur <- c(id_enqueteur, data[["global_enum_id"]][i])
    uuid <- c(uuid,data[["X_uuid"]][i])
    date_enquete <- c(date_enquete, data[["today"]][i])
    start <- c(start, str_split(ymd_hms(data[["start"]][i])," ") %>% 
                 map_chr(2))
    nom_site <- c(nom_site, data[["liste_enquete"]][i])
    nom_question <- c(nom_question, "")
    probleme <- c(probleme, "Nombre important de r�ponses autre par enqu�teur")
  }
  nb_autre <- 0
}

cleaning_log <- data.frame(id_enqueteur,uuid,date_enquete,start,nom_site,nom_question,probleme)
cleaning_log <- rbind(results,cleaning_log)
write.csv(cleaning_log, "./output/cleaning_log.csv")
