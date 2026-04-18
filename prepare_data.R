# In this file, write the R-code necessary to load your original data file
# (e.g., an SPSS, Excel, or SAS-file), and convert it to a data.frame. Then,
# use the function open_data(your_data_frame) or closed_data(your_data_frame)
# to store the data.

library(worcs)
library(foreign)


df <- foreign::read.spss("dataset_id_dataset_vanscheppingen.sav", to.data.frame = TRUE)

# df[c("ParticipantID", "V12_A1", "V12_A2")] <- NULL
# Fix names of reverse coded
names(df)[grep("_r$", names(df))] <- gsub("_r$", "", names(df)[grep("_r$", names(df))])
# Delete text fields
df[grep("_TEXT", names(df), fixed = TRUE)] <- NULL
df[grep("_A\\d", names(df))] <- NULL
df[grep("^V\\d+_1$", names(df))] <- NULL
df[grep("^Reasongynaec", names(df))] <- NULL


# Read data dictionary
shts <- readxl::excel_sheets("Copy of Brabant Study Data Selection_Predictors of New Parents RQ_MvS_cj.xlsx")[-1]
dict <- do.call(rbind, lapply(shts, function(sht){
  print(sht)
  dict <- readxl::read_xlsx("Copy of Brabant Study Data Selection_Predictors of New Parents RQ_MvS_cj.xlsx", sheet = sht, skip = 1)
  tmz <- c("OBS", "12", "20", "28", "8wPP", "6mPP", "1yPP",
           "1y6mPP", "2yPP", "2y6mPP", "3yPP", "3y6mPP", "4yPP", "4y6mPP",
           "5yPP")
  requested <- dict[, tmz[which(tmz %in% names(dict))]]
  requested <- sapply(requested, function(j){
    out <- rep(FALSE, length(j))
    out[which(as.logical(j))] <- TRUE
    out
  })
  vars <- matrix(rep(dict[[3]], ncol(requested)), ncol = ncol(requested))
  out <- expand.grid(variable = dict[[3]], time = colnames(requested))
  if(grepl("father", sht, ignore.case = TRUE)){
    out$variable <- paste0(out$variable, "_F_")
  }
  result <- data.frame(sheet = rep(sht, sum(requested)), out[requested, ], name = matrix(apply(out, 1, paste, collapse = "_"), ncol = ncol(requested))[requested])
  result$exists <- result$name %in% names(df)

  result$items <- vector("list", length = nrow(result))
  result$items[result$exists] <- result$name[result$exists]

  for(s in which(!result$exists)){
    itms <- grep(paste0("^", result$variable[s], "_.+?_", result$time[s], "$"), names(df), value = TRUE)
    itms <- itms[!grepl("_TOT_", itms, fixed = TRUE)]
    if(grepl("father", sht, ignore.case = TRUE)){
      itms <- itms[grep("_F_", itms, fixed = TRUE)]
    } else {
      itms <- itms[!grepl("_F_", itms, fixed = TRUE)]
    }
    if(length(itms) > 0){
      result$exists[s] <- TRUE
      result$items[[s]] <- itms
    }
  }
  result
}))

dict <- dict[dict$exists, ]
df <- df[, names(df) %in% unlist(dict$items)]
unlist(dict$items)[!unlist(dict$items) %in% names(df)]

desc <- descriptives(df)
# for(n in desc$name[desc$type == "character"]){cat(n, "\n\n"); print(head(df[[n]], n = 10))}
# dput(desc$name[desc$type == "character"])
# for(n in "OVG_Complicaties_na_bevalling_welke_OBS"){cat(n, "\n\n"); print(table(df[[n]]))}

# Dates
df$Dateofbirth_baby_8wPP <- as.Date(paste0(df$Dateofbirth_baby_8wPP, "-01"), format = "%Y-%m-%d")

# Numeric
df$Absence_workdays_12 <- as.numeric(gsub("^(\\d{1,}).*$", "\\1", df$Absence_workdays_12))
nums <- c("Kidsathome_8wPP", "Absence_workfreq_12", "Maternityleave_12", "Verwijzing_zwangerschap_OBS", "Complicaties_na_bevalling_OBS", "Kind_gewicht_percentiel_OBS")
df[nums] <- lapply(df[nums], as.numeric)


ints <- c("Kidsathome_8wPP", "Absence_workfreq_12", "Maternityleave_12", "Verwijzing_zwangerschap_OBS", "Complicaties_na_bevalling_OBS")
df[ints] <- lapply(df[ints], as.integer)

# Delete
# These are mostly text variables, or variables where the data is in inconsistent formats (minutes, days, weeks combined), or very sparse data (e.g., country where someone migrated from). Deleting
delte <- unique(c("Agekids_8wPP", "Profession_12",  "Unpaidprofession_12",  "Healthbaby_8wPP", "Complicaties_zwangerschap_welke_OBS", "Verwijzing_zwangerschap_reden_OBS", "OVG_overige_complicaties_OBS","Dayshospital_6mPP","Dayshospital_6mPP", "Migration_2_1y6mPP", "Migration_4_1y6mPP", "Migration_6_1y6mPP", "Infopregcourse_28", "OVG_vroeggeboorte_reden_OBS", "HG_pre_existente_aandoening_en_welke_OBS", "OVG_Complicaties_na_bevalling_welke_OBS", "Duedate_12", "Durationcrying_8wPP", "Prevdelivery_time_12",
                  "Complicaties_na_bevalling_welke_OBS",
                    "Baringsuitkomst_OBS",
                    "Cryhourencoding_8wPP",
                    "Neurodiverse_4_5_1y6mPP",
                    "Neurodiverse_4_4_1y6mPP",
                    "Neurodiverse_4_3_1y6mPP",
                    "Depressionfam_unsure_8wPP",
                    "Anxietyfam_unsure_8wPP",
                    "Doctorencoding_8wPP",
                    "Specialconsultencoding_8wPP",
                    "Checkupresults_8wPP",
                    "Sickencoding_8wPP",
                    "Followupprobencoding_8wPP",
                    "Complaintsdelivencoding_8wPP",
                    "Deliveryasplanned_8wPP", "Neurodiverse_2_1_1y6mPP", "Neurodiverse_2_2_1y6mPP", "Neurodiverse_2_3_1y6mPP",
                  "Neurodiverse_2_4_1y6mPP", "Neurodiverse_2_5_1y6mPP", "Neurodiverse_4_1_1y6mPP",
                  "Neurodiverse_4_2_1y6mPP", "Migration_1_1y6mPP", "Migration_3_1y6mPP", "Migration_5_1y6mPP",
"Migration_Status_1y6mPP", "Depressionfam_mother_8wPP", "Depressionfam_father_8wPP", "Depressionfam_sister_8wPP",
"Depressionfam_brother_8wPP", "Anxietyfam_mother_8wPP", "Anxietyfam_father_8wPP",
"Anxietyfam_sister_8wPP", "Anxietyfam_brother_8wPP"))

df[delte] <- NULL
for(d in delte){
  dict <- dict[!sapply(dict$items, function(x) d %in% x), ]
}

desc <- descriptives(df)

# Factor levels
fct <- desc$name[desc$type == "factor"]
# Get vars with identical levels
df_fact <- data.frame(name = fct)
df_fact$levels <- lapply(fct, function(x){levels(df[[x]])})
df_fact$levels_alphabetical <- lapply(df_fact$levels, sort)
df_fact$num <- sapply(df_fact$levels, length)

# Fix yes/no
df_fact$yesno <- sapply(df_fact$levels, function(x){any(grepl("\\b(no|nee)\\b", x, ignore.case = T)) & any(grepl("\\b(yes|ja)\\b", x, ignore.case = T))})
df_fact$yesno <- df_fact$yesno & df_fact$num < 4
df_fact$newlevels <- df_fact$levels
df_fact$newlevels[df_fact$yesno] <- lapply(df_fact$newlevels[df_fact$yesno], function(l){
  l[grepl("(no|nee)", l, ignore.case = T)] <- "No"
  l[grepl("(yes|ja)", l, ignore.case = T)] <- "Yes"
  l[grepl("(nvt|niet van toepassing)", l, ignore.case = T)] <- "n/a"
  l
})
relevel_these <- df_fact$name[df_fact$yesno]
for(v in relevel_these){
  levels(df[[v]]) <- df_fact$newlevels[[match(v, df_fact$name)]]
  df[[v]][!df[[v]] %in% c("No", "Yes", "n/a")] <- NA
  df[[v]] <- droplevels(df[[v]])
}

df_fact <- df_fact[!df_fact$yesno, ]
# levels(df$Problemsprev_F_28) <- c("n/a", "No", rep("Yes", length(levels(df$Problemsprev_F_28))-2L))

# df$Problemsprev_F_28 <- droplevels(df$Problemsprev_F_28)
levels(df$Treatment_12) <- c("n/a", "No", rep("Yes", length(levels(df$Treatment_12))-2L))
df$Treatment_12 <- droplevels(df$Treatment_12)
levels(df$Rupture_8wPP) <- c("No", rep("Yes", length(levels(df$Rupture_8wPP))-1L))
df$Rupture_8wPP <- droplevels(df$Rupture_8wPP)

df_fact <- df_fact[!df_fact$name %in% c("Problemsprev_F_28", "Treatment_12", "Rupture_8wPP"), ]

# Fix ordinal
df_fact$ord <- sapply(df_fact$levels, function(x){sum(grepl("\\b(important|less|minder|much|af en toe|elke|eens|altijd|helemaal|zo veel|heel|beetje|completely|fairly|vaak|nooit|soms|little|disagree|agree|false|true|neutral|nauwelijks|erg|never|rarely|occasionally|sometimes|always|not at all|moderate|often|of the time|a lot|a bit|extremely|extreem|every day|\\d days|2|3|4|7)\\b", x, ignore.case = T)) > 2})

df_fact$ord[df_fact$name %in% c("Education_12", "Education_F_28", "EDS_6_F_28")] <- TRUE
df[df_fact$name[df_fact$ord]] <- lapply(df[df_fact$name[df_fact$ord]], ordered)

df_fact <- df_fact[!df_fact$ord, ]

desc <- descriptives(df)

df$Delivery_8wPP[df$Delivery_8wPP == "0"] <- NA
levels(df$Delivery_8wPP) <- c("0", "Spontaneous", "Medication",
                              "Intervention",
                              "Intervention", "Medication",
                              "Intervention",
                              "Intervention",
                              "Intervention", "Intervention")
df$Delivery_8wPP <- droplevels(df$Delivery_8wPP)

levels(df$Spinalpuncture_8wPP) <- c("No", "Yes", "Yes", "Too late")
df$Spinalpuncture_8wPP <- droplevels(df$Spinalpuncture_8wPP)

levels(df$Painmanagementencoding_8wPP) <- c("No", rep("Yes", (length(levels(df$Painmanagementencoding_8wPP))-1L)))
df$Painmanagementencoding_8wPP <- droplevels(df$Painmanagementencoding_8wPP)

levels(df$Problemspreg_8wPP) <- c("No", rep("Yes", (length(levels(df$Problemspreg_8wPP))-1L)))
df$Problemspreg_8wPP <- droplevels(df$Problemspreg_8wPP)

levels(df$Problemsprevdeliv_12) <- c("N/A", "No", rep("Yes", (length(levels(df$Problemsprevdeliv_12))-2L)))
df$Problemsprevdeliv_12 <- droplevels(df$Problemsprevdeliv_12)

levels(df$Lifeeventrate_5yPP) <- c("Positief", "Negatief", "Both")

vs <- grep("^Pregnancycourse_", names(df), value = TRUE)
for(v in vs){
  levels(df[[v]]) <- c("No", "Yes")
}

df$Population_12 <- factor(df$Population_12 == "DUTCH", levels = c("TRUE", "FALSE"), labels = c("Dutch", "Other"))


levels(df$Maritalstatus_1yPP) <- c("Relationship","Relationship",         "Relationship", "Other", "Other",                   "Other")
levels(df$Maritalstatus_1y6mPP) <- c("Relationship","Relationship",         "Relationship", "Other", "Other",                   "Other")

vs <- grep("Maritalstatus_", names(df), value = TRUE)
for(v in setdiff(vs, c("Maritalstatus_1y6mPP", "Maritalstatus_1yPP"))){
  levels(df[[v]]) <- c("Relationship",         "Relationship", "Other", "Other", "Other")
}
for(v in vs){
  df[[v]] <- droplevels(df[[v]])
}

desc <- descriptives(df)
dict$items2 <- lapply(dict$items, function(v){
  v <- v[v %in% names(df)]
})

# Get raw sample size
dict$n <- NA
for(v in dict$name){
  r = match(v, dict$name)
  it <- dict$items[[r]]
  if(length(it) == 1){
    dict$n[r] <- desc$n[match(v, desc$name)]
  } else {
    dict$n[r] <- sum(rowSums(!is.na(df[ , it, drop = FALSE])) > 0)
    # paste0(range(desc$n[match(it, desc$name)], na.rm = TRUE), collapse = " - ")
  }
}

# Drop variables with < 10% of cases
# dict$name[dict$n < round(.1*nrow(df))]
# names(drop_these) <- dict$name
# drop_these <- drop_these / nrow(df)
# drop_these[drop_these < .05]
# drop_these <- dict$name[drop_these < round(.1*nrow(df))]

open_data(dict, filename = "dict.RData", save_expression = saveRDS(dict, "dict.RData"), load_expression = readRDS("dict.RData"), codebook = NULL, value_labels = NULL)

open_data(desc, codebook = NULL, value_labels = NULL)

closed_data(df, synthetic = FALSE)
