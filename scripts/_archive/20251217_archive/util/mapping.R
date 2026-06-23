#==============================================================================#
# MAPPING FUNCTION: map.ypccc
#==============================================================================#
# PURPOSE: Create maps for YCOM and other models, reports and climate notes

# INPUT PARAMETERS (Required): 
#       df = Dataframe including one column to plot and one or more columns to merge on.
#       shape = Shapefile including spatial geometry and one or more columns to merge on.
#       plotvar = Variable to plot. Must be the name of one column in the df.
#       data_col = Vector of one or more columns in the df to merge on.
#       shape_col = Vector of one or more columns in the shapefile to merge on.
#       yearlab = Year of the data being displayed. 
#       outputpaths = Vector of one or more file paths where plot should be output. 

# INPUT PARAMETERS (Optional): 
#       legendlab = Text description of the data being displayed. Default is NULL. If NULL, legendlab is the name of the plotvar. 
#       ptitle = Plot title. Default is NULL.
#       psubtitle = Plot subtitle. Default is NULL. 
#       filenames = Vector of one or more additional filenames that the plot should be output with. Default is NULL. 
#       source = Attribution source included in the footer of the plot. Default is NULL. 
#       ftext = Text included in the footer of the plot. Default is NULL. 
#       ptext = Binary option to determine if text labels should be added onto the map. Default is FALSE.
#       breaks = Manually set the data breaks. Default is NULL, meaning function will use parameters to automatically set breaks. 
#       n_breaks = Number of breaks. Default is 20.
#       break_type = Method to set breaks. Options are "equal", "stddev", "highvals", and "lowvals". Default is "equal". 
#       palette = Manually set the color palette. Default is NULL, meaning function will automatically set color palette. 
#       symb = Symbol to follow labels in legend. Default is "".
#       mapproj = Projection for the map. Options are "albers", "lambert", "wgs84", and "nad83". Default is "wgs84".
#       font_base = Base font size on which other sizes are modified. Default is 11pt.
#       leg_mod = Modifier to base font size for legend text. Default is 0pt. 
#       capt_mod = Modifier to base font size for caption text. Default is 2pt. 
#       subt_mod = Modifier to base font size for subtitle text. Default is 5pt. 
#       title_mod = Modifier to base font size for title text. Default is 8pt. 
#       title_pos = Title position. Can be 0 for "left", 0.5 for "center", or 1 for "right. Default is 0.5.
#       title_face = Typeface for title. Default is "bold".
#       subt_pos = Subtitle position. Can be 0 for "left", 0.5 for "center", or 1 for "right. Default is 0.5.
#       twidth = Title width. Default is 60.
#       fwidth = Footer width. Default is 100.
#       leg_pos = Legend position. Can be "top", "bottom", "left", or "right". Default is "right". 
#       leg_dir = Legend direction. Can be "vertical", or "horizontal". Default is "vertical".
#       leg_just = Legend justification. Can be "left", "right", or "center". Default is "left".
#       leg_tpos = Legend title position. Can be "top", "bottom", "left", or "right". Default is "top".
#       lab_pos = Legend label position. Can be "top", "bottom", "left", or "right". Default is "bottom". 
#       linewidth = Width of borderlines. Default is 0. 
#       bordercol = Color of the geographic borders. Default is "white". 
#       shift = Option to shift Alaska and Hawaii below contiguous US if they are on map. Default is FALSE. 
#       state_fips = Name of column that has state fips codes if shift option is TRUE. Default is NULL. 
#       margins = Numeric constant for plot margins. Default is 0.25. 
#       width = PDF width. Dault is 11.5cm.
#       height = PDF height. Default is 9cm. 

# OUTPUT: Map output as PNG to one or more locations with one or more
#         file names. 

# DATE CREATED: July 30, 2024 - EG (R version 4.4.1)
# LAST MODIFIED: July 30, 2024 - EG (R version 4.4.1)
#==============================================================================#
# MAPPING FUNCTION ####
#==============================================================================#
#----------------------------------------------------------------------------#
## 0. Set function's required inputs and their default values ####
#----------------------------------------------------------------------------#
map.ypccc <- function(df, shape, plotvar, data_col, shape_col, yearlab, outputpaths,
                      legendlab=NULL, ptitle=NULL, psubtitle=NULL, filenames=NULL, source=NULL, 
                      ftext=NULL, ptext=FALSE, breaks=NULL, palette=NULL, n_breaks=20, break_type="stddev", symb="",
                      mapproj="wgs84", font_base=11, capt_mod=2, leg_mod=0, 
                      subt_mod=5, title_mod=8, title_pos=0.5, title_face="bold", 
                      subt_pos=0.5, twidth=60,fwidth=100,leg_pos="right", 
                      leg_dir="vertical", leg_just="left",leg_tpos="top",
                      lab_pos="bottom",linewidth=0, bordercol="white",
                      shift=FALSE,state_fips=NULL, margins=0.25, width=11, height=8.5){
  
  options(scipen = 999)
  
  # Load libraries
  library(ggplot2)
  library(ggpubr)
  library(ggtext)
  library(scales)
  library(viridis)
  library(tidyverse)
  
  # Load colors
  #source("plot_colors.R", local = TRUE)
  #source("India-IGUIDE/scripts/util/plot_colors.R", local = TRUE)
  source(here::here("India-IGUIDE/scripts/util", "plot_colors.R"))
  
  # Create function to set breaks
  rnorm2 <- function(n,mean,sd){mean+sd*scale(rnorm(n))}
  
  #-----------------------------------------------------------------------------#
  # 1. Set Parameters
  #-----------------------------------------------------------------------------#
  # Find mean and standard deviation
  mean <- mean(df[,plotvar], na.rm=TRUE)
  sd <- sd(df[,plotvar], na.rm=TRUE)
  
  if(is.null(breaks)){
    # Set breaks minimum
    if(min(df[,plotvar])<0){
      start <- min(df[,plotvar])
    }else{
      start <- 0
    }
    
    if(max(df[,plotvar])<10 & max(df[,plotvar])>1){
      round_n <- 1
    } else if(max(df[,plotvar])<=1 & max(df[,plotvar])>0.01){
      round_n <- 2
    } else if(max(df[,plotvar])<=0.01){
      round_n <- 5
    } else{
      round_n <- 0
    }
    
    # Set breaks
    if(break_type=="equal"){
      breaks  <- sort(round(seq(start,(max(df[,plotvar])+1),((max(df[,plotvar])+1)/n_breaks)),round_n))
    }else if(break_type=="stddev"){
      breaks <- c(mean)
      for(s in 1:(n_breaks/2)){
        if(mean+(sd*s)<max(df[,plotvar])){
          breaks <- c(breaks,(mean+(sd*s)))
        }
        if(mean-(sd*s)>min(df[,plotvar])){
          breaks <- c(breaks,(mean-(sd*s)))
        }
        if(s==(n_breaks/2)){
          if(max(breaks)<(max(df[,plotvar]))){
            breaks <- c(breaks,(max(df[,plotvar])+1))
          }
          if(min(breaks)>(min(df[,plotvar]))){
            breaks <- c(breaks,(min(df[,plotvar])-1))
          }
        }
      }
      breaks <- sort(round(breaks,round_n))
    }else if(break_type=="highvals"){
      breaks <- seq(start,(mean+(sd*2)),((mean+(sd*2))/(n_breaks-1)))
      breaks <- c(breaks,(max(df[,plotvar])+1))
      breaks  <- sort(round(breaks,round_n))
    }else if(break_type=="lowvals"){
      breaks <- seq((mean-(sd*2)),(max(df[,plotvar])+1),((mean-(sd*2))/(n_breaks-1)))
      breaks <- c(breaks,(min(df[,plotvar])-1))
      breaks  <- sort(round(breaks,round_n))
    }else{
      stop("Update method for setting breaks in the mapping function.")
    }
  }
  
  # Set plot palette
  if(is.null(palette)){
    palette <- viridis(length(breaks))
  }
  
  # Set title
  title_text  <- paste0(lapply(strwrap(paste0(ptitle,", ",yearlab), width=twidth, simplify=FALSE), paste, collapse="\n"),"\n")
  
  # Set legend title
  if(is.null(legendlab)){
    legendlab <- plotvar
  }
  
  # Set file names
  filename1    <- paste0(plotvar,"_",yearlab,"_map")
  filenames    <- c(filename1, filenames)
  
  # Set numeric plot parameters
  font_leg       <- font_base+leg_mod
  font_capt      <- font_base+capt_mod
  font_subt      <- font_base+subt_mod
  font_title     <- font_base+title_mod
  
  # Set footer text
  if(!is.null(ftext)&!is.null(source)){
    source_form  <- paste0("<br><br><p style=\"color:#b4b4b4\"><em>Source:</em> ",
                           paste0(lapply(strwrap(source,width=(fwidth-5),simplify=FALSE), paste, 
                                         collapse="<br>"),collapse = "<br>"),"</p>")
    footer_text <- paste0(paste0(lapply(strwrap(ptexts,width=fwidth,simplify=FALSE), paste, 
                                        collapse="<br>"),collapse = "<br>"),"<br>",ftext,source_form)
  }else if(!is.null(ftext)&is.null(source)){
    footer_text <- paste0(paste0(lapply(strwrap(ftext,width=fwidth,simplify=FALSE), paste, 
                                        collapse="<br>"),collapse = "<br>"))
  }else if(!is.null(source)&is.null(ftext)){
    footer_text  <- paste0("<br><br><p style=\"color:#b4b4b4\"><em>Source:</em> ",
                           paste0(lapply(strwrap(source,width=(fwidth-5),simplify=FALSE), paste, 
                                         collapse="<br>"),collapse = "<br>"),"</p>")
  }else{
    footer_text <- NULL
  }
  
  #-----------------------------------------------------------------------------#
  # 2. Set Bins and Levels
  #-----------------------------------------------------------------------------#
  # Set bins
  df$bin <- cut(df[,plotvar], breaks=breaks)
  
  # Set legend rows
  leg_nrow <- ceiling(length(unique(df$bin))/2)
  
  
  # Format levels and labels for non-overlapping bins and round to nearest integer
  lev  <- gsub("\\(|\\)","",gsub("\\]$", "",levels(df$bin)))
  lev  <- as.numeric(unlist(strsplit(lev,",")))
  lev  <- prettyNum(lev, big.mark = ",", scientific = FALSE)
  levo <- as.character(lev[c(TRUE,FALSE)])
  leve <- as.character(lev[c(FALSE,TRUE)])
  lev  <- paste0(levo," to ",leve,symb," ")
  
  # Reassign bin levels 
  levels(df$bin) <- lev
  df$bin <- droplevels(df$bin)
  
  # Set simple bins without labels
  if(symb=="%"){
    df$bin2 <- cut(df[,plotvar], breaks=breaks)
    levels(df$bin2) <- breaks[1:(length(breaks)-1)]
  }else{
    df$bin2 <- df$bin
  }
  
  rm(levo, leve)
  #-----------------------------------------------------------------------------#
  # 3. Merge DF and Shapefile
  #-----------------------------------------------------------------------------#
  # Merge df and shapefile
  merge <- base::merge(df, shape, by.x=data_col, by.y=shape_col)
  merge <- sf::st_as_sf(merge)
  
  # Remove duplicated column names
  if(any(endsWith(colnames(merge),"\\.y"))){
    merge <- merge %>%
      dplyr::select(!ends_with("\\.y"))
    colnames(merge) <- gsub("\\.x$","",colnames(merge))
  }

  #-----------------------------------------------------------------------------#
  # 4. Set Spatial Projection
  #-----------------------------------------------------------------------------#
  # Identify map projection
  if(mapproj=="albers"){ #9822
    epsg <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  }else if(mapproj=="lambert"){ #102004
    epsg <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
  }else if(mapproj=="wgs84"){ #4326
    epsg <- "+proj=longlat +datum=WGS84 +no_defs"
  }else if(mapproj=="nad83"){ #4269
    epsg <- "+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs"
  }else{
   stop("Map projection not provided. Update projection options or change map projection parameter.") 
  }
  
  # Transform data
  data_sf <- st_transform(merge,crs=epsg)
  
  # If map is of the US, Shift Alaska and Hawaii to below Contiguous US
  if(shift==TRUE){
    data_sf <- tigris::shift_geometry(data_sf, geoid_column=state_fips, position = "below")
  }
  
  # Subset data
  data_sf <- data_sf %>%
    dplyr::select(!!sym(plotvar),bin,bin2,geometry)
  
  #-----------------------------------------------------------------------------#
  # 5. Add text label
  #-----------------------------------------------------------------------------#
  # Find centroids for placing text (rounded percents)
  sf_use_s2(FALSE)
  df3 <- sf::st_centroid(data_sf)
  df3 <- data.frame(sf::st_coordinates(df3))
  df3 <- df3 %>% 
    dplyr::select(X, Y)
  
  # Add columns from original dataset
  df3$plotvar <- merge[,plotvar][[1]]
  df3$geoname <- merge[,data_col[1]][[1]]
  df3$label <- round(df3$plotvar,0)
  #-----------------------------------------------------------------------------#
  # 5. Create Map
  #-----------------------------------------------------------------------------#
  map <- ggplot(data_sf) +
    
    # Create map
    geom_sf(aes(fill = bin2), color = bordercol, linewidth = linewidth) +
    
    # Set projection
    coord_sf(crs = epsg) +
    
    # Fill colors
    scale_fill_manual(breaks=levels(data_sf$bin2), limits=levels(data_sf$bin2), 
                      values=palette) +
    
    # Define legend
    guides(fill = guide_legend(title=legendlab,label.position=lab_pos,nrow=leg_nrow)) + 
    
    # Set title
    {if(!is.null(ptitle))ggtitle(label=title_text)}+
    
    # Add text
    {if(ptext==TRUE)geom_text(data=df3, aes(X,Y,label=label), size=numsize[j], color=ifelse((as.numeric(df3$plotvar)<(sort(breaks)[12])), "white", "black"))}+
    
    # Set subtitle
    {if(!is.null(psubtitle))labs(subtitle=paste0(strwrap(psubtitle, width=twidth), collapse="\n"))}+
    
    # Set footer
    {if(!is.null(footer_text))labs(caption=footer_text)}+
    
    #-----------------------------------------------------------------------------#
    # 6. Format Map
    #-----------------------------------------------------------------------------#
    theme_void() +
    
    # Format title
    {if(!is.null(ptitle))theme(plot.title = element_text(size=font_title, face=title_face, hjust=title_pos))}+
    
    # Format subtitle
    {if(!is.null(psubtitle))theme(plot.subtitle = element_text(size=font_subt, hjust=subt_pos))}+
    
    # Set legend position and text
    theme(legend.title.position = leg_tpos, 
          legend.position = leg_pos,
          legend.direction = leg_dir,
          legend.justification = leg_just,
          legend.text = element_text(size=font_leg), 
          
          # Remove gridlines and borders
          panel.grid=element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(), 
          
          # Remove axis labels
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          
          # Set footer size
          plot.caption=element_markdown(size=font_capt, hjust=0, vjust=0.5),
            
          # Set plot margin
          plot.margin=margin(margins,margins,margins,margins, "in"))

  #-----------------------------------------------------------------------------#
  # 7. Save Map
  #-----------------------------------------------------------------------------#
  # Loop over output paths
  for(o in 1:length(outputpaths)){
    outputpath <- outputpaths[o]
    
    # Create output directory if it doesn't already exist
    if(dir.exists(outputpath)==FALSE){
      dir.create(outputpath, recursive=TRUE)
    }
    
    # Write files
    for(f in 1:length(filenames)){
      filename <- filenames[f]
      
      pdf(paste0(outputpath,filename,".pdf"),width=width,height=height)
      print(map)
      dev.off()
    }
  }
}
#==============================================================================#
# END OF FUNCTION ####
#==============================================================================#
