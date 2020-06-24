### Risk tool instructions ###

### Environmental distance risk calculation component remains the same as currently being done in 1st generation tool.  Code for this calcuation is provided in the 'app.R' file that is set up for Shiny.
### Envt distance is calculated as Euclidean distance between source and recipient ports given the standardized data. 
### Risk categories for environmental distance values have been updated, as follows: 

envt_distances_risk[which(distances<=1.246)]=5
envt_distances_risk[which(distances<=2.113&distances>1.246)]=4
envt_distances_risk[which(distances<=3.078&distances>2.113)]=3
envt_distances_risk[which(distances<=4.304&distances>3.078)]=2
envt_distances_risk[which(distances>4.304)]=1

#### Addition of propagule pressure risk 
### Ballast age (in days) and discharge volume are taken from the ballast water reporting form

pred_discharges_ZP=tankdata$DischargeVolume*(exp(10.572-0.196*as.numeric(tankdata$ballastage)))
pred_discharges_diatom=tankdata$DischargeVolume*(exp(10.483-0.131*as.numeric(tankdata$ballastage)))
pred_discharges_dino=tankdata$DischargeVolume*(exp(5.838-0.185*as.numeric(tankdata$ballastage)))

total_discharges=pred_discharges_ZP+(pred_discharges_diatom+pred_discharges_dino)*1000

PP_risk[which(total_discharges)>=18725960445]=5
PP_risk[which(total_discharges>=6743283148&total_discharges<18725960445)]=4
PP_risk[which(total_discharges>=2899026543&total_discharges<6743283148)]=3
PP_risk[which(total_discharges>=949725836&total_discharges<2899026543)]=2
PP_risk[which(total_discharges)<949725836]=1

### For each ship, determine the minimum risk value from the environmental distance risk and the PP risk 

minimum_risk=pmin(distances_risk,discharges_risk)  

## If minimum risk value = 1, tank is assigned "Very low risk".  
## If minimum risk value = 2, tank is assigned "Low risk".  
## If minimum risk value = 3, tank is assigned "Moderate risk".  
## If minimum risk value = 4, tank is assigned "High risk".  
## If minimum risk value = 5, tank is assigned "Very high risk.  

## Calculate differential risk based on distances risk and discharges risk. 

differential_risk=NULL
for (i in 1:length(distances_risk)) {
  if (!is.na(distances_risk[i]) & !is.na(discharges_risk[i])) {
    if (distances_risk[i]==1 & discharges_risk[i]==1) {
      differential_risk[i]=1
    } else if (distances_risk[i]==1 & discharges_risk[i]==2) {
      differential_risk[i]=2
    } else if (distances_risk[i]==1 & discharges_risk[i]==3) {
      differential_risk[i]=3
    } else if (distances_risk[i]==1 & discharges_risk[i]==4) {
      differential_risk[i]=4
    } else if (distances_risk[i]==1 & discharges_risk[i]==5) {
      differential_risk[i]=5
    } else if (distances_risk[i]==2 & discharges_risk[i]==2) {
      differential_risk[i]=6
    } else if (distances_risk[i]==2 & discharges_risk[i]==3) {
      differential_risk[i]=7
    } else if (distances_risk[i]==2 & discharges_risk[i]==4) {
      differential_risk[i]=8
    } else if (distances_risk[i]==2 & discharges_risk[i]==5) {
      differential_risk[i]=9
    } else if (distances_risk[i]==3 & discharges_risk[i]==3) {
      differential_risk[i]=10
    } else if (distances_risk[i]==3 & discharges_risk[i]==4) {
      differential_risk[i]=11
    } else if (distances_risk[i]==3 & discharges_risk[i]==5) {
      differential_risk[i]=12
    } else if (distances_risk[i]==4 & discharges_risk[i]==4) {
      differential_risk[i]=13
    } else if (distances_risk[i]==4 & discharges_risk[i]==5) {
      differential_risk[i]=14
    } else if (distances_risk[i]==5 & discharges_risk[i]==5) {
      differential_risk[i]=15
    } else if (discharges_risk[i]==1 & distances_risk[i]==2) {
      differential_risk[i]=2
    } else if (discharges_risk[i]==1 & distances_risk[i]==3) {
      differential_risk[i]=3
    } else if (discharges_risk[i]==1 & distances_risk[i]==4) {
      differential_risk[i]=4
    } else if (discharges_risk[i]==1 & distances_risk[i]==5) {
      differential_risk[i]=5
    } else if (discharges_risk[i]==2 & distances_risk[i]==3) {
      differential_risk[i]=7
    } else if (discharges_risk[i]==2 & distances_risk[i]==4) {
      differential_risk[i]=8
    } else if (discharges_risk[i]==2 & distances_risk[i]==5) {
      differential_risk[i]=9
    } else if (discharges_risk[i]==3 & distances_risk[i]==4) {
      differential_risk[i]=11
    } else if (discharges_risk[i]==3 & distances_risk[i]==5) {
      differential_risk[i]=12
    } else if (discharges_risk[i]==4 & distances_risk[i]==5) {
      differential_risk[i]=14
    } 
    
  } else (discharges_risk[i]=NA) }

### Report 4 columns of results: 

### Column 1: Categorical value for Environmental match:
# "Very high" if envt_distances_risk=5
# "High" if envt_distances_risk=4
# "Moderate" if envt_distances_risk=3
# "Low" if envt_distances_risk=2
# "Very low" if envt_distances_risk=1

### Column 2: Categorical value for Propagule pressure:
# "Very high" if PP_risk=5
# "High" if PP_risk=4
# "Moderate" if PP_risk=3
# "Low" if PP_risk=2
# "Very low" if PP_risk=1

### Column 3: Overall risk:
## If minimum_risk value = 1, tank is assigned "Very low risk".  
## If minimum_risk value = 2, tank is assigned "Low risk".  
## If minimum_risk value = 3, tank is assigned "Moderate risk".  
## If minimum_risk value = 4, tank is assigned "High risk".  
## If minimum_risk value = 5, tank is assigned "Very high risk.  

### Column 4: Differential risk Value 
## Value from differential_risk
## Top value is 15 

# If output orders results by risk, tanks should be displayed in order of decreasing differential risk.

