#==============================================================================#
# PLOT COLOR STANDARDS: 
#==============================================================================#
# PURPOSE: identify colors used in CCAM and other YPCCC reports. 

# DATE CREATED: July 16, 2024 - EG (R version 4.4.1)
# LAST MODIFIED: July 16, 2024 - EG (R version 4.4.1)
#==============================================================================#
# PLOT COLOR STANDARDS ####
#==============================================================================#
#------------------------------------------------------------------------------#
### Color Palettes ###
#------------------------------------------------------------------------------#
# Greens: 
lightgreen      <-"#5ec2a8"
logogreen       <-"#19a883" # Main green color for charts
darkergreen     <-"#168167"
darkestgreen    <-"#0e5141"

# Yellows:
lightmarigold   <-"#f4ca78"
marigold        <-"#eeac2c"
darkermarigold  <-"#c38d25"
darkestmarigold <-"#6d4f14"

# Grays:
lightestgray    <-"#d9d9d9"
lightgray       <-"#a6a6a6"
midgray         <-"#7f7f7f"
darkgray        <-"#595959"
almostblack     <-"#262626"

#------------------------------------------------------------------------------#
### Politics ###
#------------------------------------------------------------------------------#
# Democrats 
totaldem        <-"#245697" # Total Democrats
libdem          <-"#144787" # Liberal Democrats
moddem          <-"#7E9ABD" # Moderate/Conservative Democrats
demtransp       <-"#4f81bd" # Democrats (transparent)
libdemlight     <-"#bbcbe4" # Rank Plots
moddemlight     <-"#c0d3e2" # Rank Plots

# Republicans
totalrep        <-"#b00000" # Total Republicans
conrep          <-"#962919" # Conservative Republicans
modrep          <-"#F08393" # Liberal/Moderate Republicans
reptrans        <-"#950000" # Republicans (transparent)
modreplight     <-"#dbbabc" # Rank Plots
conreplight     <-"#db9695" # Rank Plots

# Independent / Other
ind             <-"#969696" # Independents
indtransp       <-"#bfbfbf" # Indenepents (transparent)
allreg          <-"#262626" # All Registered Voters
fullsample      <-"#000000" # Full sample
allreglight     <-"#e9edf3" # Rank Plots

#------------------------------------------------------------------------------#
### Six Americas Colors ###
#------------------------------------------------------------------------------#
alarmed 	      <-"#1F4C5B" 	# Alarmed
concerned     	<-"#418C74" 	# Concerned
cautious 	      <-"#C8904D" 	# Cautious
disengaged    	<-"#767171" 	# Disengaged
doubtful 	      <-"#9E5050" 	# Doubtful
dismissive 	    <-"#61425D" 	# Dismissive

#------------------------------------------------------------------------------#
### YCOM Palette ###
#------------------------------------------------------------------------------#
ycom_palette <- rev(c("95"="#450847","90"="#6e123d","85"="#921c33",
                      "80"="#bf272a","75"="#cb612e","70"="#ed914c",
                      "65"="#f1ab62","60"="#f4c579","55"="#f7d990",
                      "50"="#fdeca7","45"="#e1ebf6","40"="#becee3",
                      "35"="#9eb1d0","30"="#8098bd","25"="#667eac",
                      "20"="#4e669a","15"="#395188","10"="#283e75",
                      "5"="#1a2b65","0"="#0c1b54"))

ycom_diffs   <- rev(c("#00441b","#1b7837","#5aae61","#a6daa0","#d8f0d2",
                      "#e7d4e8","#c1a5cf","#9970aa","#762a83","#40004b"))

#------------------------------------------------------------------------------#
### Other Colors ###
#------------------------------------------------------------------------------#
deeplilac	      <-"#A569BD"
cornsilkdark 	  <-"#8f8c7b" 	
cornsilk3 	    <-"#cdc8b1" 	

#------------------------------------------------------------------------------#
### Colors By Corresponding Text Color ###
#------------------------------------------------------------------------------#
white_text <- c("#168167", "#0e5141", "#6d4f14", "#7f7f7f", "#595959", "#262626", 
                "#245697", "#144787", "#b00000", "#962919", "#950000", "#262626", 
                "#000000")
black_text <- c("#5ec2a8", "#19a883", "#f4ca78", "#eeac2c", "#c38d25", "#d9d9d9", 
                "#a6a6a6", "#7E9ABD", "#4f81bd", "#F08393", "#969696", "#bfbfbf")
#==============================================================================#
# END OF FILE ####
#==============================================================================#
