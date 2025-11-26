#==============================================================================#
# India Downscaling ###
# Post-Stratification Save Function ####

# INPUTS:
# projections = estimates from poststratification function
# pred_total_demo = estimates from poststratification function broken down by demographics
# pred_area_demo = estimates from poststratification function broken down by geographies and demographics
# column_name = name of the column in the df with the geographic level ("state_code" or "state_dist_code")
# level = geographic level currently being run ("state" or "district")
# var = name of the variable currently being run (i.e., "flood" or "drought")
# time = current year
# outdir - output directory

# OUTPUTS:
# R object with area-level projections
# CSV file with area-level projections
# CSV files with area-level projections broken down by demographics

# Last modified: November 4, 2025, EG
#==============================================================================#
# LOGS ####
#==============================================================================#
todaysdate <- Sys.Date()
logfile <- paste0(outputpath,"logs/save_functions_",todaysdate,"_log.txt")
sink(logfile, append=TRUE, split=TRUE)

#==============================================================================#
## POSTSTRAT SAVE FUNCTION ####
#==============================================================================#
poststrat_save <- function(projections, pred_total_demo, pred_area_demo, column_name, level, var, time, outdir){
  
  areapred <- projections[[2]]
  #### 1. Sum at national level for difference plots ####
  national_n                                <- sum(areapred$n, na.rm=TRUE)
  national_cellpred_n                       <- sum(areapred$cellpred_n, na.rm=TRUE)
  national_prop                             <- national_cellpred_n / national_n
  national_per                              <- national_prop*100
  national_pred_prop_upper                  <- sum(areapred$pred_prop_upper, na.rm=TRUE)
  national_pred_prop_lower                  <- sum(areapred$pred_prop_lower, na.rm=TRUE)
  national_moe                              <- (national_pred_prop_upper - national_pred_prop_lower)/2
  areapred$national_per                     <- national_per
  areapred$pred_prop[areapred$pred_prop==0] <- national_prop
  
  #### 2. Create national dataframe ####
  natpred <- cbind("us",national_n,national_cellpred_n,national_prop,national_per,national_pred_prop_upper,national_pred_prop_lower,national_moe)
  colnames(natpred) <- c("country","n","cellpred_n","pred_prop","pred_per","pred_prop_upper","pred_prop_lower","moe")
  
  #### 3. Calculate difference from national average for each area ####
  areapred$pred_per_diff <- areapred$pred_per - national_per
  
  for(i in 1:length(pred_area_demo)){
    pred_area_demo[[i]]$temp <- gsub("[0-9]._", "",pred_area_demo[[i]]$Group.2)
    for(j in 1:length(unique(pred_total_demo[[i]]$Group.2))){
      predvar <- pred_total_demo[[i]]$Group.2[j]
      pred_area_demo[[i]]$pred_per_diff[pred_area_demo[[i]]$temp==predvar] <- pred_area_demo[[i]]$pred_per[pred_area_demo[[i]]$temp==predvar] - pred_total_demo[[i]]$pred_per[pred_total_demo[[i]]$Group.2==predvar]
    }
    pred_area_demo[[i]]$temp <- NULL
  }
  
  #### 4. Save geography files ####
  if(dir.exists(paste0(outdir,level,"/", var, "/"))==FALSE){
    dir.create(paste0(outdir,level,"/", var, "/"), recursive=TRUE)
  }
  write.csv(areapred, file=paste0(outdir,level,"/", var, "/", var, "_", level, "_", time, "_table.csv"), row.names = FALSE)
  save(areapred, file=paste0(outdir,level,"/", var, "/",var,"_", level, "_", time, ".Rda"))
  
  #### 5. Save country files ####
  if(level=="state"){
    if(dir.exists(paste0(outdir,"country/", var, "/"))==FALSE){
      dir.create(paste0(outdir,"country/", var, "/"), recursive=TRUE)
    }
    write.csv(natpred, file=paste0(outdir,"country/", var, "/", var, "_country_", time, "_table.csv"), row.names = FALSE)
    save(natpred, file=paste0(outdir,"country/", var, "/",var,"_country_", time, ".Rda"))
  }
  
  #### 6. Save demographic breakdowns ####
  for(i in 1:length(pred_area_demo)){
    demo <- names(pred_total_demo)[i]
    write.csv(pred_area_demo[[i]], file=paste0(outdir,level,"/",var,"/",var,"_",level,"_",demo,"_",time,"_table.csv"), row.names = FALSE)
  }
  for(i in 1:length(pred_total_demo)){
    demo <- names(pred_total_demo)[i]
    write.csv(pred_total_demo[[i]], file=paste0(outdir,level,"/",var,"/",var,"_country_",demo,"_",time,"_table.csv"), row.names = FALSE)
  }
  
  return(areapred)
}

#==============================================================================#
# END OF FUNCTION ####
#==============================================================================#
