`%!in%` = Negate(`%in%`)

# new_data: c'est l object qui va recevoir que les nouvelles enquetes
# downloaded_data: les donnnes telecharge de KOBO (toutes les donnnes du coup: anciennes/nouvelles)
# clean_data: les donnes qu'on a deja traité
# NB vous pouvez changez les noms des object c'ets juste un example ici


# code pour la donnee principale
new_data <- downloaded_data %>% filter(X_uuid %!in% clean_data[["X_uuid"]])

# code pour les loops
new_data_loop <- downloaded_data_loop %>% filter(X_submission__uuid %!in% clean_data[["X_uuid"]])
