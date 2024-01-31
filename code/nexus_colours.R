##### Colour Palettes IPBES Nexus Assessment ######

## Purpose ##
# define colour palettes to be used throughout the IPBES nexus assessment
# Nexus colour Settings ########################################################

### 1. Primary nexus colours ####
nx_col_prim <- c("#B65719", "#4A928F", "#C6D68A", "#4D2D71", "#696B5F", "#FFFFFF") 


### 2. Total range of nexus colours ####
nx_col <- c("#799336","#A0AF67","#C6D68A",     
            "#196C71","#4A928F","#A7C6C5",     
            "#4D2D71","#8B78A1","#BAB0C9",    
            "#791E32",
            "#B65719","#C3773E",
            "#D9AA80","#696B5F","#ACABA4","#FFFFFF") 


### 3. Colour-blind friendly combinations of nexus colours ####
nx_col_blind <- c("#4D2D71", "#8B78A1", "#BAB0C9", "#B65719", "#C6D68A", "#791E32", "#4A928F", "#FFFFFF"
                  ,"#C3773E" ,"#799336"   ### chose ONE of the previous two !
                  ,"#D9AA80" ,"#A0AF67"   ### chose ONE of the previous two !
                  ,"#196C71" ,"#686B5F"   ### chose ONE of the previous two !
                  ,"#A7C6C5" ,"#ACABA4"   ### chose ONE of the previous two !
                  ,"#D5B41F", "#EDD018", "#F9E855") # 3 Shades of yellow could be added


### 4. Colour use for countries ####
# colouring of IPBES Regions according to IPBES Nexus colour palette 
nx_col_region <- c("Africa" = "#B65719", 
                   "Americas" = "#799336",
                   "Asia and the Pacific" = "#4A928F", 
                   "Europe and Central Asia" = "#791E32",
                   "Arctic / Antarctic" = "#FFFFFF")

# colouring for income level groups:
# as the IPBES Nexus colour palette does not specify the colouring of different income level groups 
# the following colour codes are set according to the colouring of income level groups in the 
nx_col_income <- c("World" = "#696B5F", 
                   "High income" = "#B65719", 
                   "Upper middle income" = "#8B78A1", 
                   "Lower middle income" = "#799336", 
                   "Low income" = "#C6D68A")


### 5. Colour use for nexus elements
nx_col_elements <- c("Climate" = "#BAB0C9",
                    "Food" = "#B65719",
                    "Biodiversity" = "#C6D68A",
                    "Health" = "#791E32", 
                    "Water" = "#4A928F")


### 6. Background colour #### 
# C0 M0 Y0 K7 (Background)
nx_back_col <- c("#F2F2F2")
