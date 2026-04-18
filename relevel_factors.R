Delete
Complicaties_na_bevalling_welke_OBS
Baringsuitkomst_OBS
Cryhourencoding_8wPP
Neurodiverse_4_5_1y6mPP
Neurodiverse_4_4_1y6mPP
Neurodiverse_4_3_1y6mPP
Depressionfam_unsure_8wPP
Anxietyfam_unsure_8wPP
Doctorencoding_8wPP
Specialconsultencoding_8wPP
Checkupresults_8wPP
Sickencoding_8wPP
Followupprobencoding_8wPP
Complaintsdelivencoding_8wPP
Deliveryasplanned_8wPP

Ord:
  Education_12
Education_F_28
EDS_6_F_28

Yesno nvt
Problemsprev_F_28
Treatment_12
Rupture_8wPP

df$Delivery_8wPP[df$Delivery_8wPP == "0"] <- NA
levels(df$Delivery_8wPP) <- c("0", "Spontaneous", "Medication",
                              "Intervention",
                              "Intervention", "Medication",
                              "Intervention",
                              "Intervention",
                              "Intervention", "Intervention")
levels(df$Spinalpuncture_8wPP) <- c("No", "Yes", "Yes", "Too late")

levels(df$Painmanagementencoding_8wPP) <- c("No", rep("Yes", (length(levels(df$Painmanagementencoding_8wPP))-1L))

                                            levels(df$Problemspreg_8wPP) <- c("No", rep("Yes", (length(levels(df$Problemspreg_8wPP))-1L))

                                                                              levels(df$Problemsprevdeliv_12) <- c("N/A", "No", rep("Yes", (length(levels(df$Problemsprevdeliv_12))-2L))





                                                                                                                   Relabel
                                                                                                                   levels(df$Lifeeventrate_5yPP) <- c("Positief", "Negatief", "Both"  )

                                                                                                                   All Pregnancycourse_ variables: <- c("No", "Yes")

                                                                                                                   All Neurodiverse_ variables should be used for mixture model

                                                                                                                   Migration_Status_1y6mPP

                                                                                                                   levels(df$Population_12) <- c("Dutch", , "Other",               "Other", "Other", "Other", "Other", "Other", "Other")

                                                                                                                   levels(df$Maritalstatus_1yPP) <- c("Relationship","Relationship",         "Relationship", "Other", "Other"                   "Other")
                                                                                                                   levels(df$Maritalstatus_1y6mPP) <- c("Relationship","Relationship",         "Relationship", "Other", "Other"                   "Other")

                                                                                                                   all other marital status:
                                                                                                                     levels() <- c("Relationship",         "Relationship", "Other", "Other"                   "Other")
