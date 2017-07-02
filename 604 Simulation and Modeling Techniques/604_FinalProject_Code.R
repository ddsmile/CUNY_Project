# Load packages
library(igraph)
library(statnet)
library(ggplot2)

# Download facebook_combined.txt from http://snap.stanford.edu/data/egonets-Facebook.html
# Read in facebook_combined_net0_with0.txt
dat_0  <- read.table(file.choose(), header=FALSE)
# Put the edges into a matrix
el_0 <- as.matrix(dat_0, ncol=2)  # edgelist

# Build adjacency matrix
# Create igraph graph
g_0 <- graph.data.frame(el_0, directed=FALSE)
# Generate adjacency matrix using igraph package
mat_0 <- get.adjacency(g_0, sparse=FALSE)

# Load the attributes file "Net0_FullFeats_with0.csv"
atts_0<-read.csv(file.choose(),head=TRUE,sep=",",stringsAsFactors=FALSE)

# Create a network object using statnet suite
net_0<-network(mat_0, vertex.attr=atts_0, vertex.attrnames=colnames(atts_0),
               directed=F, hyper=F, loops=F, multiple=F, bipartite=F)

# plot the network
plot(net_0)

# Build ergm by edge term only (Bernoulli model)
bmodel_0 <- ergm(net_0~edges) #Estimate the model
# Print out summary
summary(bmodel_0)

# ==========================
#   Summary of model fit
# ==========================
#   
#   Formula:   net_0 ~ edges
# 
# Iterations:  6 out of 20 
# 
# Monte Carlo MLE Results:
#   Estimate Std. Error MCMC % p-value    
# edges -2.99908    0.01914      0  <1e-04 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Null Deviance: 83702  on 60378  degrees of freedom
# Residual Deviance: 23063  on 60377  degrees of freedom
# 
# AIC: 23065    BIC: 23074    (Smaller is better.) 

  
# Build the model keeping all variables
bmodelFull_0 <- ergm(net_0~edges+nodefactor("birthday_1")
                     +nodefactor("birthday_2")
                     +nodefactor("birthday_3")
                     +nodefactor("birthday_4")
                     +nodefactor("birthday_5")
                     +nodefactor("birthday_6")
                     +nodefactor("birthday_7")
                     +nodefactor("birthday_8")
                     +nodefactor("education_classes_1")
                     +nodefactor("education_classes_2")
                     +nodefactor("education_classes_3")
                     +nodefactor("education_classes_4")
                     +nodefactor("education_classes_5")
                     +nodefactor("education_concentration_1")
                     +nodefactor("education_concentration_2")
                     +nodefactor("education_concentration_3")
                     +nodefactor("education_concentration_4")
                     +nodefactor("education_concentration_5")
                     +nodefactor("education_concentration_6")
                     +nodefactor("education_concentration_7")
                     +nodefactor("education_degree_1")
                     +nodefactor("education_degree_2")
                     +nodefactor("education_degree_3")
                     +nodefactor("education_degree_4")
                     +nodefactor("education_school_1")
                     +nodefactor("education_school_2")
                     +nodefactor("education_school_3")
                     +nodefactor("education_school_4")
                     +nodefactor("education_school_5")
                     +nodefactor("education_school_6")
                     +nodefactor("education_school_7")
                     +nodefactor("education_school_8")
                     +nodefactor("education_school_9")
                     +nodefactor("education_school_10")
                     +nodefactor("education_school_11")
                     +nodefactor("education_school_12")
                     +nodefactor("education_school_13")
                     +nodefactor("education_school_14")
                     +nodefactor("education_school_15")
                     +nodefactor("education_school_16")
                     +nodefactor("education_school_17")
                     +nodefactor("education_school_18")
                     +nodefactor("education_school_19")
                     +nodefactor("education_school_20")
                     +nodefactor("education_school_21")
                     +nodefactor("education_school_22")
                     +nodefactor("education_school_23")
                     +nodefactor("education_school_24")
                     +nodefactor("education_school_25")
                     +nodefactor("education_school_26")
                     +nodefactor("education_school_27")
                     +nodefactor("education_school_28")
                     +nodefactor("education_school_29")
                     +nodefactor("education_type_1")
                     +nodefactor("education_type_2")
                     +nodefactor("education_type_3")
                     +nodefactor("education_with_1")
                     +nodefactor("education_year_1")
                     +nodefactor("education_year_2")
                     +nodefactor("education_year_3")
                     +nodefactor("education_year_4")
                     +nodefactor("education_year_5")
                     +nodefactor("education_year_6")
                     +nodefactor("education_year_7")
                     +nodefactor("education_year_8")
                     +nodefactor("education_year_9")
                     +nodefactor("education_year_10")
                     +nodefactor("education_year_11")
                     +nodefactor("education_year_12")
                     +nodefactor("education_year_13")
                     +nodefactor("education_year_14")
                     +nodefactor("education_year_15")
                     +nodefactor("education_year_16")
                     +nodefactor("first_name_1")
                     +nodefactor("first_name_2")
                     +nodefactor("first_name_3")
                     +nodefactor("first_name_4")
                     +nodefactor("gender_1")
                     +nodefactor("gender_2")
                     +nodefactor("hometown_1")
                     +nodefactor("hometown_2")
                     +nodefactor("hometown_3")
                     +nodefactor("hometown_4")
                     +nodefactor("hometown_5")
                     +nodefactor("hometown_6")
                     +nodefactor("hometown_7")
                     +nodefactor("hometown_8")
                     +nodefactor("hometown_9")
                     +nodefactor("hometown_10")
                     +nodefactor("hometown_11")
                     +nodefactor("languages_1")
                     +nodefactor("languages_2")
                     +nodefactor("languages_3")
                     +nodefactor("languages_4")
                     +nodefactor("languages_5")
                     +nodefactor("languages_6")
                     +nodefactor("languages_7")
                     +nodefactor("languages_8")
                     +nodefactor("languages_9")
                     +nodefactor("languages_10")
                     +nodefactor("languages_11")
                     +nodefactor("languages_12")
                     +nodefactor("languages_13")
                     +nodefactor("languages_14")
                     +nodefactor("last_name_1")
                     +nodefactor("last_name_2")
                     +nodefactor("last_name_3")
                     +nodefactor("last_name_4")
                     +nodefactor("last_name_5")
                     +nodefactor("last_name_6")
                     +nodefactor("last_name_7")
                     +nodefactor("last_name_8")
                     +nodefactor("last_name_9")
                     +nodefactor("last_name_10")
                     +nodefactor("last_name_11")
                     +nodefactor("last_name_12")
                     +nodefactor("last_name_13")
                     +nodefactor("last_name_14")
                     +nodefactor("last_name_15")
                     +nodefactor("last_name_16")
                     +nodefactor("last_name_17")
                     +nodefactor("last_name_18")
                     +nodefactor("last_name_19")
                     +nodefactor("last_name_20")
                     +nodefactor("last_name_21")
                     +nodefactor("locale_1")
                     +nodefactor("locale_2")
                     +nodefactor("locale_3")
                     +nodefactor("location_1")
                     +nodefactor("location_2")
                     +nodefactor("location_3")
                     +nodefactor("location_4")
                     +nodefactor("location_5")
                     +nodefactor("location_6")
                     +nodefactor("location_7")
                     +nodefactor("location_8")
                     +nodefactor("location_9")
                     +nodefactor("location_10")
                     +nodefactor("location_11")
                     +nodefactor("location_12")
                     +nodefactor("work_employer_1")
                     +nodefactor("work_employer_2")
                     +nodefactor("work_employer_3")
                     +nodefactor("work_employer_4")
                     +nodefactor("work_employer_5")
                     +nodefactor("work_employer_6")
                     +nodefactor("work_employer_7")
                     +nodefactor("work_employer_8")
                     +nodefactor("work_employer_9")
                     +nodefactor("work_employer_10")
                     +nodefactor("work_employer_11")
                     +nodefactor("work_employer_12")
                     +nodefactor("work_employer_13")
                     +nodefactor("work_employer_14")
                     +nodefactor("work_employer_15")
                     +nodefactor("work_employer_16")
                     +nodefactor("work_employer_17")
                     +nodefactor("work_employer_18")
                     +nodefactor("work_employer_19")
                     +nodefactor("work_employer_20")
                     +nodefactor("work_end_date_1")
                     +nodefactor("work_end_date_2")
                     +nodefactor("work_end_date_3")
                     +nodefactor("work_end_date_4")
                     +nodefactor("work_end_date_5")
                     +nodefactor("work_end_date_6")
                     +nodefactor("work_end_date_7")
                     +nodefactor("work_end_date_8")
                     +nodefactor("work_end_date_9")
                     +nodefactor("work_end_date_10")
                     +nodefactor("work_end_date_11")
                     +nodefactor("work_end_date_12")
                     +nodefactor("work_end_date_13")
                     +nodefactor("work_end_date_14")
                     +nodefactor("work_end_date_15")
                     +nodefactor("work_end_date_16")
                     +nodefactor("work_location_1")
                     +nodefactor("work_location_2")
                     +nodefactor("work_location_3")
                     +nodefactor("work_location_4")
                     +nodefactor("work_location_5")
                     +nodefactor("work_location_6")
                     +nodefactor("work_location_7")
                     +nodefactor("work_location_8")
                     +nodefactor("work_location_9")
                     +nodefactor("work_location_10")
                     +nodefactor("work_location_11")
                     +nodefactor("work_location_12")
                     +nodefactor("work_position_1")
                     +nodefactor("work_position_2")
                     +nodefactor("work_position_3")
                     +nodefactor("work_position_4")
                     +nodefactor("work_position_5")
                     +nodefactor("work_position_6")
                     +nodefactor("work_position_7")
                     +nodefactor("work_position_8")
                     +nodefactor("work_position_9")
                     +nodefactor("work_position_10")
                     +nodefactor("work_position_11")
                     +nodefactor("work_position_12")
                     +nodefactor("work_position_13")
                     +nodefactor("work_start_date_1")
                     +nodefactor("work_start_date_2")
                     +nodefactor("work_start_date_3")
                     +nodefactor("work_start_date_4")
                     +nodefactor("work_start_date_5")
                     +nodefactor("work_start_date_6")
                     +nodefactor("work_start_date_7")
                     +nodefactor("work_start_date_8")
                     +nodefactor("work_start_date_9")
                     +nodefactor("work_start_date_10")
                     +nodefactor("work_start_date_11")
                     +nodefactor("work_start_date_12")
                     +nodefactor("work_start_date_13")
                     +nodefactor("work_start_date_14")
                     +nodefactor("work_start_date_15")
                     +nodefactor("work_start_date_16")
                     +nodefactor("work_start_date_17")
                     +nodefactor("work_start_date_18")
                     +nodefactor("work_start_date_19")
                     +nodefactor("work_start_date_20")
                     +nodefactor("work_start_date_21")
                     +nodefactor("work_start_date_22")
                     +nodefactor("work_with_1"))
summary(bmodelFull_0)

# ==========================
#   Summary of model fit
# ==========================
#   
#   Formula:   net_0 ~ edges + nodefactor("birthday_1") + nodefactor("birthday_2") + 
#   nodefactor("birthday_3") + nodefactor("birthday_4") + nodefactor("birthday_5") + 
#   nodefactor("birthday_6") + nodefactor("birthday_7") + nodefactor("birthday_8") + 
#   nodefactor("education_classes_1") + nodefactor("education_classes_2") + 
#   nodefactor("education_classes_3") + nodefactor("education_classes_4") + 
#   nodefactor("education_classes_5") + nodefactor("education_concentration_1") + 
#   nodefactor("education_concentration_2") + nodefactor("education_concentration_3") + 
#   nodefactor("education_concentration_4") + nodefactor("education_concentration_5") + 
#   nodefactor("education_concentration_6") + nodefactor("education_concentration_7") + 
#   nodefactor("education_degree_1") + nodefactor("education_degree_2") + 
#   nodefactor("education_degree_3") + nodefactor("education_degree_4") + 
#   nodefactor("education_school_1") + nodefactor("education_school_2") + 
#   nodefactor("education_school_3") + nodefactor("education_school_4") + 
#   nodefactor("education_school_5") + nodefactor("education_school_6") + 
#   nodefactor("education_school_7") + nodefactor("education_school_8") + 
#   nodefactor("education_school_9") + nodefactor("education_school_10") + 
#   nodefactor("education_school_11") + nodefactor("education_school_12") + 
#   nodefactor("education_school_13") + nodefactor("education_school_14") + 
#   nodefactor("education_school_15") + nodefactor("education_school_16") + 
#   nodefactor("education_school_17") + nodefactor("education_school_18") + 
#   nodefactor("education_school_19") + nodefactor("education_school_20") + 
#   nodefactor("education_school_21") + nodefactor("education_school_22") + 
#   nodefactor("education_school_23") + nodefactor("education_school_24") + 
#   nodefactor("education_school_25") + nodefactor("education_school_26") + 
#   nodefactor("education_school_27") + nodefactor("education_school_28") + 
#   nodefactor("education_school_29") + nodefactor("education_type_1") + 
#   nodefactor("education_type_2") + nodefactor("education_type_3") + 
#   nodefactor("education_with_1") + nodefactor("education_year_1") + 
#   nodefactor("education_year_2") + nodefactor("education_year_3") + 
#   nodefactor("education_year_4") + nodefactor("education_year_5") + 
#   nodefactor("education_year_6") + nodefactor("education_year_7") + 
#   nodefactor("education_year_8") + nodefactor("education_year_9") + 
#   nodefactor("education_year_10") + nodefactor("education_year_11") + 
#   nodefactor("education_year_12") + nodefactor("education_year_13") + 
#   nodefactor("education_year_14") + nodefactor("education_year_15") + 
#   nodefactor("education_year_16") + nodefactor("first_name_1") + 
#   nodefactor("first_name_2") + nodefactor("first_name_3") + 
#   nodefactor("first_name_4") + nodefactor("gender_1") + nodefactor("gender_2") + 
#   nodefactor("hometown_1") + nodefactor("hometown_2") + nodefactor("hometown_3") + 
#   nodefactor("hometown_4") + nodefactor("hometown_5") + nodefactor("hometown_6") + 
#   nodefactor("hometown_7") + nodefactor("hometown_8") + nodefactor("hometown_9") + 
#   nodefactor("hometown_10") + nodefactor("hometown_11") + nodefactor("languages_1") + 
#   nodefactor("languages_2") + nodefactor("languages_3") + nodefactor("languages_4") + 
#   nodefactor("languages_5") + nodefactor("languages_6") + nodefactor("languages_7") + 
#   nodefactor("languages_8") + nodefactor("languages_9") + nodefactor("languages_10") + 
#   nodefactor("languages_11") + nodefactor("languages_12") + 
#   nodefactor("languages_13") + nodefactor("languages_14") + 
#   nodefactor("last_name_1") + nodefactor("last_name_2") + nodefactor("last_name_3") + 
#   nodefactor("last_name_4") + nodefactor("last_name_5") + nodefactor("last_name_6") + 
#   nodefactor("last_name_7") + nodefactor("last_name_8") + nodefactor("last_name_9") + 
#   nodefactor("last_name_10") + nodefactor("last_name_11") + 
#   nodefactor("last_name_12") + nodefactor("last_name_13") + 
#   nodefactor("last_name_14") + nodefactor("last_name_15") + 
#   nodefactor("last_name_16") + nodefactor("last_name_17") + 
#   nodefactor("last_name_18") + nodefactor("last_name_19") + 
#   nodefactor("last_name_20") + nodefactor("last_name_21") + 
#   nodefactor("locale_1") + nodefactor("locale_2") + nodefactor("locale_3") + 
#   nodefactor("location_1") + nodefactor("location_2") + nodefactor("location_3") + 
#   nodefactor("location_4") + nodefactor("location_5") + nodefactor("location_6") + 
#   nodefactor("location_7") + nodefactor("location_8") + nodefactor("location_9") + 
#   nodefactor("location_10") + nodefactor("location_11") + nodefactor("location_12") + 
#   nodefactor("work_employer_1") + nodefactor("work_employer_2") + 
#   nodefactor("work_employer_3") + nodefactor("work_employer_4") + 
#   nodefactor("work_employer_5") + nodefactor("work_employer_6") + 
#   nodefactor("work_employer_7") + nodefactor("work_employer_8") + 
#   nodefactor("work_employer_9") + nodefactor("work_employer_10") + 
#   nodefactor("work_employer_11") + nodefactor("work_employer_12") + 
#   nodefactor("work_employer_13") + nodefactor("work_employer_14") + 
#   nodefactor("work_employer_15") + nodefactor("work_employer_16") + 
#   nodefactor("work_employer_17") + nodefactor("work_employer_18") + 
#   nodefactor("work_employer_19") + nodefactor("work_employer_20") + 
#   nodefactor("work_end_date_1") + nodefactor("work_end_date_2") + 
#   nodefactor("work_end_date_3") + nodefactor("work_end_date_4") + 
#   nodefactor("work_end_date_5") + nodefactor("work_end_date_6") + 
#   nodefactor("work_end_date_7") + nodefactor("work_end_date_8") + 
#   nodefactor("work_end_date_9") + nodefactor("work_end_date_10") + 
#   nodefactor("work_end_date_11") + nodefactor("work_end_date_12") + 
#   nodefactor("work_end_date_13") + nodefactor("work_end_date_14") + 
#   nodefactor("work_end_date_15") + nodefactor("work_end_date_16") + 
#   nodefactor("work_location_1") + nodefactor("work_location_2") + 
#   nodefactor("work_location_3") + nodefactor("work_location_4") + 
#   nodefactor("work_location_5") + nodefactor("work_location_6") + 
#   nodefactor("work_location_7") + nodefactor("work_location_8") + 
#   nodefactor("work_location_9") + nodefactor("work_location_10") + 
#   nodefactor("work_location_11") + nodefactor("work_location_12") + 
#   nodefactor("work_position_1") + nodefactor("work_position_2") + 
#   nodefactor("work_position_3") + nodefactor("work_position_4") + 
#   nodefactor("work_position_5") + nodefactor("work_position_6") + 
#   nodefactor("work_position_7") + nodefactor("work_position_8") + 
#   nodefactor("work_position_9") + nodefactor("work_position_10") + 
#   nodefactor("work_position_11") + nodefactor("work_position_12") + 
#   nodefactor("work_position_13") + nodefactor("work_start_date_1") + 
#   nodefactor("work_start_date_2") + nodefactor("work_start_date_3") + 
#   nodefactor("work_start_date_4") + nodefactor("work_start_date_5") + 
#   nodefactor("work_start_date_6") + nodefactor("work_start_date_7") + 
#   nodefactor("work_start_date_8") + nodefactor("work_start_date_9") + 
#   nodefactor("work_start_date_10") + nodefactor("work_start_date_11") + 
#   nodefactor("work_start_date_12") + nodefactor("work_start_date_13") + 
#   nodefactor("work_start_date_14") + nodefactor("work_start_date_15") + 
#   nodefactor("work_start_date_16") + nodefactor("work_start_date_17") + 
#   nodefactor("work_start_date_18") + nodefactor("work_start_date_19") + 
#   nodefactor("work_start_date_20") + nodefactor("work_start_date_21") + 
#   nodefactor("work_start_date_22") + nodefactor("work_with_1")
# 
# Iterations:  10 out of 20 
# 
# Monte Carlo MLE Results:
#   Estimate Std. Error MCMC %  p-value    
# edges                                   -1.398604   0.625501      0 0.025357 *  
#   nodefactor.birthday_1.1                  1.698371   0.427450      0  < 1e-04 ***
#   nodefactor.birthday_2.1                 -1.005589   0.307004      0 0.001055 ** 
#   nodefactor.birthday_3.1                 -0.177394   0.323915      0 0.583929    
#   nodefactor.birthday_4.1                 -0.682253   0.319527      0 0.032749 *  
#   nodefactor.birthday_5.1                  0.672717   0.277125      0 0.015207 *  
#   nodefactor.birthday_6.1                  1.264543   0.318215      0  < 1e-04 ***
#   nodefactor.birthday_7.1                 -0.198562   0.178036      0 0.264730    
#   nodefactor.birthday_8.1                 -0.247869   0.147355      0 0.092550 .  
#   nodefactor.education_classes_1.1         0.279895   1.005213      0 0.780673    
#   nodefactor.education_classes_2.1         1.321599   0.622711      0 0.033814 *  
#   nodefactor.education_classes_3.1         2.530319   2.293905      0 0.270005    
#   nodefactor.education_classes_4.1        -2.612631   0.824685      0 0.001535 ** 
#   nodefactor.education_classes_5.1        -3.432133   1.740265      0 0.048593 *  
#   nodefactor.education_concentration_1.1   0.381457   0.265072      0 0.150136    
#   nodefactor.education_concentration_2.1  -0.079112   0.143834      0 0.582308    
#   nodefactor.education_concentration_3.1   0.939750   0.496094      0 0.058190 .  
#   nodefactor.education_concentration_4.1   0.018516   0.256782      0 0.942516    
#   nodefactor.education_concentration_5.1   3.047619   0.234982      0  < 1e-04 ***
#   nodefactor.education_concentration_6.1   4.001432   0.585620      0  < 1e-04 ***
#   nodefactor.education_concentration_7.1  -0.352966   0.482050      0 0.464039    
#   nodefactor.education_degree_1.1         -1.481530   0.710676      0 0.037103 *  
#   nodefactor.education_degree_2.1          0.757620   0.423356      0 0.073530 .  
#   nodefactor.education_degree_3.1          1.081270   0.305886      0 0.000408 ***
#   nodefactor.education_degree_4.1         -1.739554   1.224979      0 0.155593    
#   nodefactor.education_school_1.1          1.271167   0.396533      0 0.001348 ** 
#   nodefactor.education_school_2.1         -1.395717   0.563882      0 0.013319 *  
#   nodefactor.education_school_3.1         -1.630787   0.483740      0 0.000749 ***
#   nodefactor.education_school_4.1          1.939262   0.691500      0 0.005042 ** 
#   nodefactor.education_school_5.1          2.426577   0.466140      0  < 1e-04 ***
#   nodefactor.education_school_6.1          1.113504   0.491971      0 0.023618 *  
#   nodefactor.education_school_7.1         -0.075286   0.359467      0 0.834108    
#   nodefactor.education_school_8.1         -0.999653   0.814927      0 0.219948    
#   nodefactor.education_school_9.1         -0.262855   0.464596      0 0.571552    
#   nodefactor.education_school_10.1        -2.916581   0.704185      0  < 1e-04 ***
#   nodefactor.education_school_11.1         1.628592   0.520625      0 0.001760 ** 
#   nodefactor.education_school_12.1         0.373976   0.231504      0 0.106226    
#   nodefactor.education_school_13.1        -1.461257   0.581602      0 0.011992 *  
#   nodefactor.education_school_14.1         1.346127   0.542043      0 0.013015 *  
#   nodefactor.education_school_15.1         1.215685   0.208332      0  < 1e-04 ***
#   nodefactor.education_school_16.1        -0.952238   0.275577      0 0.000550 ***
#   nodefactor.education_school_17.1        -0.001245   0.408009      0 0.997566    
#   nodefactor.education_school_18.1        -2.863479   1.816638      0 0.114973    
#   nodefactor.education_school_19.1         1.867631   1.596557      0 0.242091    
#   nodefactor.education_school_20.1        -0.050457   0.273320      0 0.853538    
#   nodefactor.education_school_21.1         0.995880   0.325167      0 0.002195 ** 
#   nodefactor.education_school_22.1         1.265029   0.332436      0 0.000142 ***
#   nodefactor.education_school_23.1        -1.371568   0.993112      0 0.167259    
#   nodefactor.education_school_24.1        -0.479997   0.271994      0 0.077614 .  
#   nodefactor.education_school_25.1         2.759740   1.024234      0 0.007053 ** 
#   nodefactor.education_school_26.1         0.596729   0.309053      0 0.053509 .  
#   nodefactor.education_school_27.1        -0.249634   0.112331      0 0.026266 *  
#   nodefactor.education_school_28.1        -6.749633   1.659751      0  < 1e-04 ***
#   nodefactor.education_school_29.1         0.719137   0.276235      0 0.009234 ** 
#   nodefactor.education_type_1.1           -0.160812   0.100378      0 0.109147    
#   nodefactor.education_type_2.1           -0.088524   0.126028      0 0.482423    
#   nodefactor.education_type_3.1           -0.572544   0.107158      0  < 1e-04 ***
#   nodefactor.education_with_1.1            0.914562   0.262504      0 0.000494 ***
#   nodefactor.education_year_1.1            0.174518   0.874860      0 0.841887    
#   nodefactor.education_year_2.1            0.134236   0.290918      0 0.644497    
#   nodefactor.education_year_3.1            0.220656   0.161960      0 0.173073    
#   nodefactor.education_year_4.1           -0.087448   0.236302      0 0.711332    
#   nodefactor.education_year_5.1           -0.164789   0.213133      0 0.439421    
#   nodefactor.education_year_6.1           -0.326672   0.331839      0 0.324909    
#   nodefactor.education_year_7.1           -0.011219   0.179194      0 0.950078    
#   nodefactor.education_year_8.1           -0.772748   0.179879      0  < 1e-04 ***
#   nodefactor.education_year_9.1           -0.404309   0.159382      0 0.011192 *  
#   nodefactor.education_year_10.1          -0.556612   0.161618      0 0.000574 ***
#   nodefactor.education_year_11.1          -0.885097   0.350507      0 0.011566 *  
#   nodefactor.education_year_12.1          -0.738622   0.204376      0 0.000302 ***
#   nodefactor.education_year_13.1           1.178687   0.156236      0  < 1e-04 ***
#   nodefactor.education_year_14.1          -1.490122   0.460131      0 0.001202 ** 
#   nodefactor.education_year_15.1           0.221199   0.321947      0 0.492044    
#   nodefactor.education_year_16.1           0.394358   0.191905      0 0.039887 *  
#   nodefactor.first_name_1.1                0.554855   0.214116      0 0.009562 ** 
#   nodefactor.first_name_2.1               -1.531218   0.523744      0 0.003461 ** 
#   nodefactor.first_name_3.1               -0.913687   0.685423      0 0.182528    
#   nodefactor.first_name_4.1                1.038455   0.466816      0 0.026115 *  
#   nodefactor.gender_1.1                   -0.560861   0.172039      0 0.001114 ** 
#   nodefactor.gender_2.1                   -0.358265   0.172580      0 0.037904 *  
#   nodefactor.hometown_1.1                  4.427306   1.304553      0 0.000690 ***
#   nodefactor.hometown_2.1                 -1.247196   0.420352      0 0.003008 ** 
#   nodefactor.hometown_3.1                  0.019370   0.208424      0 0.925956    
#   nodefactor.hometown_4.1                 -1.762992   0.793593      0 0.026319 *  
#   nodefactor.hometown_5.1                  0.152762   0.623659      0 0.806500    
#   nodefactor.hometown_6.1                  0.734912   0.471100      0 0.118767    
#   nodefactor.hometown_7.1                  3.065907   0.989318      0 0.001943 ** 
#   nodefactor.hometown_8.1                 -1.286201   1.078372      0 0.232982    
#   nodefactor.hometown_9.1                 -1.272365   0.491965      0 0.009704 ** 
#   nodefactor.hometown_10.1                -1.146345   0.734204      0 0.118448    
#   nodefactor.hometown_11.1                 1.972095   0.611850      0 0.001268 ** 
#   nodefactor.languages_1.1                 0.066206   0.386344      0 0.863938    
#   nodefactor.languages_2.1                -1.943627   0.378245      0  < 1e-04 ***
#   nodefactor.languages_3.1                 0.896265   0.168303      0  < 1e-04 ***
#   nodefactor.languages_4.1                 0.361984   0.401270      0 0.367010    
#   nodefactor.languages_5.1                -3.836377   0.847909      0  < 1e-04 ***
#   nodefactor.languages_6.1                -1.387717   0.496364      0 0.005179 ** 
#   nodefactor.languages_7.1                -0.429940   0.397846      0 0.279848    
#   nodefactor.languages_8.1                -1.787592   0.823578      0 0.029971 *  
#   nodefactor.languages_9.1                -1.158765   0.332361      0 0.000490 ***
#   nodefactor.languages_10.1               -1.624762   0.485320      0 0.000815 ***
#   nodefactor.languages_11.1                0.310147   0.410333      0 0.449747    
#   nodefactor.languages_12.1                0.893971   0.276538      0 0.001227 ** 
#   nodefactor.languages_13.1               -3.164526   1.092499      0 0.003774 ** 
#   nodefactor.languages_14.1               -1.593612   1.051093      0 0.129487    
#   nodefactor.last_name_1.1                -0.339550   0.326757      0 0.298738    
#   nodefactor.last_name_2.1                -0.810892   1.638866      0 0.620751    
#   nodefactor.last_name_3.1                 5.169421   1.041024      0  < 1e-04 ***
#   nodefactor.last_name_4.1                       NA   0.000000      0       NA    
#   nodefactor.last_name_5.1                -1.114636   0.230547      0  < 1e-04 ***
#   nodefactor.last_name_6.1                -0.623603   0.467476      0 0.182216    
#   nodefactor.last_name_7.1                -0.695260   0.391994      0 0.076126 .  
#   nodefactor.last_name_8.1                 1.642402   0.403628      0  < 1e-04 ***
#   nodefactor.last_name_9.1                -1.516944   0.461225      0 0.001006 ** 
#   nodefactor.last_name_10.1               -0.253002   0.290983      0 0.384593    
#   nodefactor.last_name_11.1               -2.250075   1.351922      0 0.096047 .  
#   nodefactor.last_name_12.1               -1.511431   0.359273      0  < 1e-04 ***
#   nodefactor.last_name_13.1               -1.384426   0.395825      0 0.000470 ***
#   nodefactor.last_name_14.1               -0.478218   0.290548      0 0.099787 .  
#   nodefactor.last_name_15.1               -0.866740   0.303556      0 0.004301 ** 
#   nodefactor.last_name_16.1                0.298449   0.163656      0 0.068212 .  
#   nodefactor.last_name_17.1                0.320094   0.267574      0 0.231590    
#   nodefactor.last_name_18.1                      NA   0.000000      0       NA    
#   nodefactor.last_name_19.1               -1.592895   0.690450      0 0.021056 *  
#   nodefactor.last_name_20.1               -0.625387   0.252765      0 0.013357 *  
#   nodefactor.last_name_21.1                1.868059   0.312967      0  < 1e-04 ***
#   nodefactor.locale_1.1                   -0.464897   0.352593      0 0.187339    
#   nodefactor.locale_2.1                   -0.233359   0.287199      0 0.416488    
#   nodefactor.locale_3.1                   -0.261181   0.267503      0 0.328887    
#   nodefactor.location_1.1                 -1.295274   0.452024      0 0.004165 ** 
#   nodefactor.location_2.1                 -0.318645   0.216791      0 0.141615    
#   nodefactor.location_3.1                 -0.965485   0.482148      0 0.045239 *  
#   nodefactor.location_4.1                  2.915978   2.021807      0 0.149233    
#   nodefactor.location_5.1                  0.131057   0.135817      0 0.334571    
#   nodefactor.location_6.1                 -3.080843   0.487557      0  < 1e-04 ***
#   nodefactor.location_7.1                  0.687244   0.128325      0  < 1e-04 ***
#   nodefactor.location_8.1                  1.901688   1.126159      0 0.091292 .  
#   nodefactor.location_9.1                 -1.564709   0.316227      0  < 1e-04 ***
#   nodefactor.location_10.1                 1.328119   0.517636      0 0.010298 *  
#   nodefactor.location_11.1                 0.316928   0.125049      0 0.011265 *  
#   nodefactor.location_12.1                -1.517023   0.324701      0  < 1e-04 ***
#   nodefactor.work_employer_1.1            -6.570558   1.351201      0  < 1e-04 ***
#   nodefactor.work_employer_2.1            -0.492701   0.372204      0 0.185595    
#   nodefactor.work_employer_3.1            -1.251084   0.889783      0 0.159713    
#   nodefactor.work_employer_4.1            -1.603944   1.036421      0 0.121729    
#   nodefactor.work_employer_5.1             1.190211   0.993253      0 0.230807    
#   nodefactor.work_employer_6.1             0.494858   0.177362      0 0.005271 ** 
#   nodefactor.work_employer_7.1             0.571679   0.444202      0 0.198106    
#   nodefactor.work_employer_8.1             2.817601   0.725658      0 0.000103 ***
#   nodefactor.work_employer_9.1           -14.092997   2.111660      0  < 1e-04 ***
#   nodefactor.work_employer_10.1           -0.098468   0.253337      0 0.697512    
#   nodefactor.work_employer_11.1            1.202397   0.748191      0 0.108044    
#   nodefactor.work_employer_12.1            2.523251   1.259713      0 0.045178 *  
#   nodefactor.work_employer_13.1           -1.973959   0.302792      0  < 1e-04 ***
#   nodefactor.work_employer_14.1            0.021177   0.588082      0 0.971274    
#   nodefactor.work_employer_15.1          -10.568515   2.244058      0  < 1e-04 ***
#   nodefactor.work_employer_16.1           -4.474347   1.016427      0  < 1e-04 ***
#   nodefactor.work_employer_17.1            0.318552   0.562176      0 0.570960    
#   nodefactor.work_employer_18.1            0.051986   1.774533      0 0.976629    
#   nodefactor.work_employer_19.1           -0.249497   0.294614      0 0.397077    
#   nodefactor.work_employer_20.1           -3.714359   1.192981      0 0.001850 ** 
#   nodefactor.work_end_date_1.1            -0.015658   0.149119      0 0.916374    
#   nodefactor.work_end_date_2.1            -1.083937   0.485798      0 0.025668 *  
#   nodefactor.work_end_date_3.1            -0.261185   1.268947      0 0.836926    
#   nodefactor.work_end_date_4.1             1.666210   1.349786      0 0.217049    
#   nodefactor.work_end_date_5.1             0.265600   0.596059      0 0.655893    
#   nodefactor.work_end_date_6.1             1.769889   0.454729      0  < 1e-04 ***
#   nodefactor.work_end_date_7.1             1.632084   0.546840      0 0.002841 ** 
#   nodefactor.work_end_date_8.1             0.700539   0.427418      0 0.101218    
#   nodefactor.work_end_date_9.1            -0.144012   0.484686      0 0.766373    
#   nodefactor.work_end_date_10.1            0.146725   0.353351      0 0.677970    
#   nodefactor.work_end_date_11.1            1.194975   0.919832      0 0.193906    
#   nodefactor.work_end_date_12.1            0.163355   0.288822      0 0.571674    
#   nodefactor.work_end_date_13.1            1.955393   0.568602      0 0.000584 ***
#   nodefactor.work_end_date_14.1            1.309742   0.369531      0 0.000394 ***
#   nodefactor.work_end_date_15.1            2.070272   0.642009      0 0.001262 ** 
#   nodefactor.work_end_date_16.1            0.652074   1.075696      0 0.544392    
#   nodefactor.work_location_1.1             1.751299   0.374618      0  < 1e-04 ***
#   nodefactor.work_location_2.1            -1.552402   0.631758      0 0.014002 *  
#   nodefactor.work_location_3.1             1.654706   0.614807      0 0.007117 ** 
#   nodefactor.work_location_4.1            -0.228748   0.398439      0 0.565895    
#   nodefactor.work_location_5.1            -0.698750   0.710902      0 0.325657    
#   nodefactor.work_location_6.1             1.409080   0.474038      0 0.002955 ** 
#   nodefactor.work_location_7.1             1.709984   0.648324      0 0.008353 ** 
#   nodefactor.work_location_8.1             0.223395   0.622481      0 0.719687    
#   nodefactor.work_location_9.1             3.300978   0.736201      0  < 1e-04 ***
#   nodefactor.work_location_10.1           -0.425908   0.316654      0 0.178623    
#   nodefactor.work_location_11.1            1.916912   1.266255      0 0.130071    
#   nodefactor.work_location_12.1            1.153806   0.457053      0 0.011591 *  
#   nodefactor.work_position_1.1             4.400935   1.059445      0  < 1e-04 ***
#   nodefactor.work_position_2.1             0.870393   0.745364      0 0.242915    
#   nodefactor.work_position_3.1            -0.756338   0.931043      0 0.416590    
#   nodefactor.work_position_4.1             1.585876   0.368568      0  < 1e-04 ***
#   nodefactor.work_position_5.1            -0.099394   0.403730      0 0.805537    
#   nodefactor.work_position_6.1            -1.763818   0.975389      0 0.070561 .  
#   nodefactor.work_position_7.1             4.252814   1.384814      0 0.002134 ** 
#   nodefactor.work_position_8.1             4.415551   1.684558      0 0.008764 ** 
#   nodefactor.work_position_9.1             0.927483   0.815476      0 0.255396    
#   nodefactor.work_position_10.1           -0.811425   0.705296      0 0.249953    
#   nodefactor.work_position_11.1           -2.633303   1.698181      0 0.120988    
#   nodefactor.work_position_12.1            0.196791   0.379982      0 0.604533    
#   nodefactor.work_position_13.1            0.134272   0.327893      0 0.682175    
#   nodefactor.work_start_date_1.1           0.128610   0.171170      0 0.452442    
#   nodefactor.work_start_date_2.1           0.637895   0.571760      0 0.264568    
#   nodefactor.work_start_date_3.1          -1.846605   0.752548      0 0.014138 *  
#   nodefactor.work_start_date_4.1          -2.112779   0.797908      0 0.008102 ** 
#   nodefactor.work_start_date_5.1           2.428654   0.374852      0  < 1e-04 ***
#   nodefactor.work_start_date_6.1           0.915212   0.696885      0 0.189091    
#   nodefactor.work_start_date_7.1           0.039360   0.428174      0 0.926758    
#   nodefactor.work_start_date_8.1          -0.316063   0.464767      0 0.496478    
#   nodefactor.work_start_date_9.1           1.065741   0.560006      0 0.057034 .  
#   nodefactor.work_start_date_10.1         -0.743906   0.297010      0 0.012260 *  
#   nodefactor.work_start_date_11.1          0.742178   0.373340      0 0.046823 *  
#   nodefactor.work_start_date_12.1         -0.710528   0.577239      0 0.218362    
#   nodefactor.work_start_date_13.1          2.681394   0.493735      0  < 1e-04 ***
#   nodefactor.work_start_date_14.1         -1.150566   0.352129      0 0.001086 ** 
#   nodefactor.work_start_date_15.1          1.942077   0.349184      0  < 1e-04 ***
#   nodefactor.work_start_date_16.1         -1.203903   0.319636      0 0.000166 ***
#   nodefactor.work_start_date_17.1         -1.901416   0.400724      0  < 1e-04 ***
#   nodefactor.work_start_date_18.1          1.769731   0.680564      0 0.009314 ** 
#   nodefactor.work_start_date_19.1         -1.254866   0.353886      0 0.000391 ***
#   nodefactor.work_start_date_20.1          1.093776   0.386097      0 0.004614 ** 
#   nodefactor.work_start_date_21.1         -1.566666   0.670977      0 0.019552 *  
#   nodefactor.work_start_date_22.1         -4.399261   0.798616      0  < 1e-04 ***
#   nodefactor.work_with_1.1                       NA   0.000000      0       NA    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Null Deviance: 83702  on 60378  degrees of freedom
# Residual Deviance: 17713  on 60153  degrees of freedom
# 
# AIC: 18163    BIC: 20190    (Smaller is better.)


# Build the model keeping only significant variables
bmodelRefined_0 <- ergm(net_0~edges+nodefactor("birthday_1")
                        +nodefactor("birthday_2")
                        +nodefactor("birthday_4")
                        +nodefactor("birthday_5")
                        +nodefactor("birthday_6")
                        +nodefactor("education_classes_2")
                        +nodefactor("education_classes_5")
                        +nodefactor("education_concentration_3")
                        +nodefactor("education_concentration_5")
                        +nodefactor("education_concentration_6")
                        +nodefactor("education_degree_1")
                        +nodefactor("education_degree_2")
                        +nodefactor("education_degree_3")
                        +nodefactor("education_school_1")
                        +nodefactor("education_school_2")
                        +nodefactor("education_school_3")
                        +nodefactor("education_school_4")
                        +nodefactor("education_school_5")
                        +nodefactor("education_school_6")
                        +nodefactor("education_school_10")
                        +nodefactor("education_school_11")
                        +nodefactor("education_school_13")
                        +nodefactor("education_school_14")
                        +nodefactor("education_school_15")
                        +nodefactor("education_school_16")
                        +nodefactor("education_school_21")
                        +nodefactor("education_school_22")
                        +nodefactor("education_school_25")
                        +nodefactor("education_school_27")
                        +nodefactor("education_school_28")
                        +nodefactor("education_school_29")
                        +nodefactor("education_type_3")
                        +nodefactor("education_with_1")
                        +nodefactor("education_year_8")
                        +nodefactor("education_year_9")
                        +nodefactor("education_year_10")
                        +nodefactor("education_year_11")
                        +nodefactor("education_year_12")
                        +nodefactor("education_year_13")
                        +nodefactor("education_year_14")
                        +nodefactor("education_year_16")
                        +nodefactor("first_name_1")
                        +nodefactor("first_name_2")
                        +nodefactor("first_name_4")
                        +nodefactor("gender_1")
                        +nodefactor("hometown_1")
                        +nodefactor("hometown_2")
                        +nodefactor("hometown_4")
                        +nodefactor("hometown_7")
                        +nodefactor("hometown_9")
                        +nodefactor("hometown_11")
                        +nodefactor("languages_2")
                        +nodefactor("languages_3")
                        +nodefactor("languages_5")
                        +nodefactor("languages_6")
                        +nodefactor("languages_8")
                        +nodefactor("languages_9")
                        +nodefactor("languages_10")
                        +nodefactor("languages_12")
                        +nodefactor("languages_13")
                        +nodefactor("last_name_3")
                        +nodefactor("last_name_5")
                        +nodefactor("last_name_7")
                        +nodefactor("last_name_8")
                        +nodefactor("last_name_9")
                        +nodefactor("last_name_11")
                        +nodefactor("last_name_12")
                        +nodefactor("last_name_13")
                        +nodefactor("last_name_14")
                        +nodefactor("last_name_15")
                        +nodefactor("last_name_16")
                        +nodefactor("last_name_19")
                        +nodefactor("last_name_20")
                        +nodefactor("last_name_21")
                        +nodefactor("location_1")
                        +nodefactor("location_6")
                        +nodefactor("location_7")
                        +nodefactor("location_8")
                        +nodefactor("location_9")
                        +nodefactor("location_10")
                        +nodefactor("location_11")
                        +nodefactor("location_12")
                        +nodefactor("work_employer_1")
                        +nodefactor("work_employer_6")
                        +nodefactor("work_employer_8")
                        +nodefactor("work_employer_9")
                        +nodefactor("work_employer_12")
                        +nodefactor("work_employer_13")
                        +nodefactor("work_employer_15")
                        +nodefactor("work_employer_16")
                        +nodefactor("work_employer_20")
                        +nodefactor("work_end_date_2")
                        +nodefactor("work_end_date_6")
                        +nodefactor("work_end_date_7")
                        +nodefactor("work_end_date_13")
                        +nodefactor("work_end_date_14")
                        +nodefactor("work_end_date_15")
                        +nodefactor("work_location_1")
                        +nodefactor("work_location_2")
                        +nodefactor("work_location_3")
                        +nodefactor("work_location_6")
                        +nodefactor("work_location_7")
                        +nodefactor("work_location_9")
                        +nodefactor("work_location_12")
                        +nodefactor("work_position_1")
                        +nodefactor("work_position_4")
                        +nodefactor("work_position_6")
                        +nodefactor("work_position_7")
                        +nodefactor("work_position_8")
                        +nodefactor("work_start_date_3")
                        +nodefactor("work_start_date_4")
                        +nodefactor("work_start_date_5")
                        +nodefactor("work_start_date_9")
                        +nodefactor("work_start_date_10")
                        +nodefactor("work_start_date_11")
                        +nodefactor("work_start_date_13")
                        +nodefactor("work_start_date_14")
                        +nodefactor("work_start_date_15")
                        +nodefactor("work_start_date_16")
                        +nodefactor("work_start_date_17")
                        +nodefactor("work_start_date_18")
                        +nodefactor("work_start_date_19")
                        +nodefactor("work_start_date_20")
                        +nodefactor("work_start_date_21")
                        +nodefactor("work_start_date_22"))
summary(bmodelRefined_0)


# ==========================
#   Summary of model fit
# ==========================
#   
#   Formula:   net_0 ~ edges + nodefactor("birthday_1") + nodefactor("birthday_2") + 
#   nodefactor("birthday_4") + nodefactor("birthday_5") + nodefactor("birthday_6") + 
#   nodefactor("education_classes_2") + nodefactor("education_classes_5") + 
#   nodefactor("education_concentration_3") + nodefactor("education_concentration_5") + 
#   nodefactor("education_concentration_6") + nodefactor("education_degree_1") + 
#   nodefactor("education_degree_2") + nodefactor("education_degree_3") + 
#   nodefactor("education_school_1") + nodefactor("education_school_2") + 
#   nodefactor("education_school_3") + nodefactor("education_school_4") + 
#   nodefactor("education_school_5") + nodefactor("education_school_6") + 
#   nodefactor("education_school_10") + nodefactor("education_school_11") + 
#   nodefactor("education_school_13") + nodefactor("education_school_14") + 
#   nodefactor("education_school_15") + nodefactor("education_school_16") + 
#   nodefactor("education_school_21") + nodefactor("education_school_22") + 
#   nodefactor("education_school_25") + nodefactor("education_school_27") + 
#   nodefactor("education_school_28") + nodefactor("education_school_29") + 
#   nodefactor("education_type_3") + nodefactor("education_with_1") + 
#   nodefactor("education_year_8") + nodefactor("education_year_9") + 
#   nodefactor("education_year_10") + nodefactor("education_year_11") + 
#   nodefactor("education_year_12") + nodefactor("education_year_13") + 
#   nodefactor("education_year_14") + nodefactor("education_year_16") + 
#   nodefactor("first_name_1") + nodefactor("first_name_2") + 
#   nodefactor("first_name_4") + nodefactor("gender_1") + nodefactor("hometown_1") + 
#   nodefactor("hometown_2") + nodefactor("hometown_4") + nodefactor("hometown_7") + 
#   nodefactor("hometown_9") + nodefactor("hometown_11") + nodefactor("languages_2") + 
#   nodefactor("languages_3") + nodefactor("languages_5") + nodefactor("languages_6") + 
#   nodefactor("languages_8") + nodefactor("languages_9") + nodefactor("languages_10") + 
#   nodefactor("languages_12") + nodefactor("languages_13") + 
#   nodefactor("last_name_3") + nodefactor("last_name_5") + nodefactor("last_name_7") + 
#   nodefactor("last_name_8") + nodefactor("last_name_9") + nodefactor("last_name_11") + 
#   nodefactor("last_name_12") + nodefactor("last_name_13") + 
#   nodefactor("last_name_14") + nodefactor("last_name_15") + 
#   nodefactor("last_name_16") + nodefactor("last_name_19") + 
#   nodefactor("last_name_20") + nodefactor("last_name_21") + 
#   nodefactor("location_1") + nodefactor("location_6") + nodefactor("location_7") + 
#   nodefactor("location_8") + nodefactor("location_9") + nodefactor("location_10") + 
#   nodefactor("location_11") + nodefactor("location_12") + nodefactor("work_employer_1") + 
#   nodefactor("work_employer_6") + nodefactor("work_employer_8") + 
#   nodefactor("work_employer_9") + nodefactor("work_employer_12") + 
#   nodefactor("work_employer_13") + nodefactor("work_employer_15") + 
#   nodefactor("work_employer_16") + nodefactor("work_employer_20") + 
#   nodefactor("work_end_date_2") + nodefactor("work_end_date_6") + 
#   nodefactor("work_end_date_7") + nodefactor("work_end_date_13") + 
#   nodefactor("work_end_date_14") + nodefactor("work_end_date_15") + 
#   nodefactor("work_location_1") + nodefactor("work_location_2") + 
#   nodefactor("work_location_3") + nodefactor("work_location_6") + 
#   nodefactor("work_location_7") + nodefactor("work_location_9") + 
#   nodefactor("work_location_12") + nodefactor("work_position_1") + 
#   nodefactor("work_position_4") + nodefactor("work_position_6") + 
#   nodefactor("work_position_7") + nodefactor("work_position_8") + 
#   nodefactor("work_start_date_3") + nodefactor("work_start_date_4") + 
#   nodefactor("work_start_date_5") + nodefactor("work_start_date_9") + 
#   nodefactor("work_start_date_10") + nodefactor("work_start_date_11") + 
#   nodefactor("work_start_date_13") + nodefactor("work_start_date_14") + 
#   nodefactor("work_start_date_15") + nodefactor("work_start_date_16") + 
#   nodefactor("work_start_date_17") + nodefactor("work_start_date_18") + 
#   nodefactor("work_start_date_19") + nodefactor("work_start_date_20") + 
#   nodefactor("work_start_date_21") + nodefactor("work_start_date_22")
# 
# Iterations:  8 out of 20 
# 
# Monte Carlo MLE Results:
#   Estimate Std. Error MCMC %  p-value    
# edges                                  -2.72107    0.06137      0  < 1e-04 ***
#   nodefactor.birthday_1.1                 0.96869    0.25733      0 0.000167 ***
#   nodefactor.birthday_2.1                -0.84038    0.16769      0  < 1e-04 ***
#   nodefactor.birthday_4.1                -0.43829    0.13916      0 0.001636 ** 
#   nodefactor.birthday_5.1                 0.25043    0.13761      0 0.068788 .  
#   nodefactor.birthday_6.1                 0.72117    0.11444      0  < 1e-04 ***
#   nodefactor.education_classes_2.1        1.35512    0.33915      0  < 1e-04 ***
#   nodefactor.education_classes_5.1       -3.43480    0.74253      0  < 1e-04 ***
#   nodefactor.education_concentration_3.1  0.78378    0.33551      0 0.019489 *  
#   nodefactor.education_concentration_5.1  2.99026    0.17555      0  < 1e-04 ***
#   nodefactor.education_concentration_6.1  2.18545    0.24987      0  < 1e-04 ***
#   nodefactor.education_degree_1.1        -0.45434    0.23688      0 0.055116 .  
#   nodefactor.education_degree_2.1         0.52644    0.19694      0 0.007517 ** 
#   nodefactor.education_degree_3.1         0.71326    0.13161      0  < 1e-04 ***
#   nodefactor.education_school_1.1         0.79136    0.20041      0  < 1e-04 ***
#   nodefactor.education_school_2.1        -1.63420    0.27172      0  < 1e-04 ***
#   nodefactor.education_school_3.1        -1.78098    0.39857      0  < 1e-04 ***
#   nodefactor.education_school_4.1         2.05745    0.19301      0  < 1e-04 ***
#   nodefactor.education_school_5.1         1.82990    0.23270      0  < 1e-04 ***
#   nodefactor.education_school_6.1         1.36092    0.32057      0  < 1e-04 ***
#   nodefactor.education_school_10.1       -1.73041    0.27479      0  < 1e-04 ***
#   nodefactor.education_school_11.1        0.59182    0.31866      0 0.063283 .  
#   nodefactor.education_school_13.1       -1.29422    0.38039      0 0.000668 ***
#   nodefactor.education_school_14.1        0.93189    0.24665      0 0.000158 ***
#   nodefactor.education_school_15.1        0.97389    0.15408      0  < 1e-04 ***
#   nodefactor.education_school_16.1       -0.63870    0.15462      0  < 1e-04 ***
#   nodefactor.education_school_21.1        1.04207    0.14704      0  < 1e-04 ***
#   nodefactor.education_school_22.1        0.85431    0.17730      0  < 1e-04 ***
#   nodefactor.education_school_25.1        2.81842    0.45013      0  < 1e-04 ***
#   nodefactor.education_school_27.1       -0.39013    0.05840      0  < 1e-04 ***
#   nodefactor.education_school_28.1       -5.24268    0.83081      0  < 1e-04 ***
#   nodefactor.education_school_29.1        0.91250    0.13768      0  < 1e-04 ***
#   nodefactor.education_type_3.1          -0.63678    0.05503      0  < 1e-04 ***
#   nodefactor.education_with_1.1           1.24129    0.17110      0  < 1e-04 ***
#   nodefactor.education_year_8.1          -0.58944    0.09655      0  < 1e-04 ***
#   nodefactor.education_year_9.1          -0.24472    0.07691      0 0.001465 ** 
#   nodefactor.education_year_10.1         -0.48380    0.08867      0  < 1e-04 ***
#   nodefactor.education_year_11.1         -0.68485    0.16253      0  < 1e-04 ***
#   nodefactor.education_year_12.1         -0.79322    0.17357      0  < 1e-04 ***
#   nodefactor.education_year_13.1          0.99498    0.09770      0  < 1e-04 ***
#   nodefactor.education_year_14.1         -1.61267    0.28102      0  < 1e-04 ***
#   nodefactor.education_year_16.1          0.48603    0.11043      0  < 1e-04 ***
#   nodefactor.first_name_1.1               0.72437    0.17842      0  < 1e-04 ***
#   nodefactor.first_name_2.1              -0.79944    0.36498      0 0.028502 *  
#   nodefactor.first_name_4.1               1.23292    0.29255      0  < 1e-04 ***
#   nodefactor.gender_1.1                  -0.18430    0.04162      0  < 1e-04 ***
#   nodefactor.hometown_1.1                 3.95449    0.67291      0  < 1e-04 ***
#   nodefactor.hometown_2.1                -0.70703    0.20901      0 0.000718 ***
#   nodefactor.hometown_4.1                -2.36567    0.25749      0  < 1e-04 ***
#   nodefactor.hometown_7.1                 1.51322    0.32753      0  < 1e-04 ***
#   nodefactor.hometown_9.1                -0.88882    0.21465      0  < 1e-04 ***
#   nodefactor.hometown_11.1                1.22890    0.17830      0  < 1e-04 ***
#   nodefactor.languages_2.1               -1.51838    0.17462      0  < 1e-04 ***
#   nodefactor.languages_3.1                0.87129    0.06781      0  < 1e-04 ***
#   nodefactor.languages_5.1               -1.87465    0.30408      0  < 1e-04 ***
#   nodefactor.languages_6.1               -0.85436    0.27383      0 0.001809 ** 
#   nodefactor.languages_8.1               -1.59845    0.36877      0  < 1e-04 ***
#   nodefactor.languages_9.1               -0.83061    0.17380      0  < 1e-04 ***
#   nodefactor.languages_10.1              -2.15662    0.26147      0  < 1e-04 ***
#   nodefactor.languages_12.1               0.88942    0.12588      0  < 1e-04 ***
#   nodefactor.languages_13.1              -3.60488    0.61820      0  < 1e-04 ***
#   nodefactor.last_name_3.1                2.88676    0.42104      0  < 1e-04 ***
#   nodefactor.last_name_5.1               -1.07871    0.21260      0  < 1e-04 ***
#   nodefactor.last_name_7.1               -0.40940    0.19936      0 0.040019 *  
#   nodefactor.last_name_8.1                1.26496    0.26449      0  < 1e-04 ***
#   nodefactor.last_name_9.1               -1.10715    0.22379      0  < 1e-04 ***
#   nodefactor.last_name_11.1              -2.13971    1.00070      0 0.032504 *  
#   nodefactor.last_name_12.1              -1.47322    0.35854      0  < 1e-04 ***
#   nodefactor.last_name_13.1              -1.05097    0.27510      0 0.000133 ***
#   nodefactor.last_name_14.1              -0.92959    0.23657      0  < 1e-04 ***
#   nodefactor.last_name_15.1              -0.83238    0.21005      0  < 1e-04 ***
#   nodefactor.last_name_16.1               0.33209    0.16218      0 0.040598 *  
#   nodefactor.last_name_19.1              -2.39089    0.37794      0  < 1e-04 ***
#   nodefactor.last_name_20.1              -0.59834    0.25101      0 0.017143 *  
#   nodefactor.last_name_21.1               1.37173    0.19823      0  < 1e-04 ***
#   nodefactor.location_1.1                -2.04564    0.21343      0  < 1e-04 ***
#   nodefactor.location_6.1                -1.74854    0.21457      0  < 1e-04 ***
#   nodefactor.location_7.1                 0.58706    0.11223      0  < 1e-04 ***
#   nodefactor.location_8.1                 2.24024    1.05224      0 0.033257 *  
#   nodefactor.location_9.1                -1.34003    0.20320      0  < 1e-04 ***
#   nodefactor.location_10.1                0.77779    0.18631      0  < 1e-04 ***
#   nodefactor.location_11.1                0.44448    0.07314      0  < 1e-04 ***
#   nodefactor.location_12.1               -1.31519    0.18936      0  < 1e-04 ***
#   nodefactor.work_employer_1.1           -3.71629    0.46501      0  < 1e-04 ***
#   nodefactor.work_employer_6.1            0.56939    0.10764      0  < 1e-04 ***
#   nodefactor.work_employer_8.1            2.53862    0.25481      0  < 1e-04 ***
#   nodefactor.work_employer_9.1           -7.35254    0.96150      0  < 1e-04 ***
#   nodefactor.work_employer_12.1           2.19625    0.44783      0  < 1e-04 ***
#   nodefactor.work_employer_13.1          -1.58365    0.16281      0  < 1e-04 ***
#   nodefactor.work_employer_15.1          -6.21345    0.52330      0  < 1e-04 ***
#   nodefactor.work_employer_16.1          -3.08992    0.42990      0  < 1e-04 ***
#   nodefactor.work_employer_20.1          -1.58436    0.45823      0 0.000545 ***
#   nodefactor.work_end_date_2.1           -0.44673    0.15270      0 0.003440 ** 
#   nodefactor.work_end_date_6.1            1.36682    0.14562      0  < 1e-04 ***
#   nodefactor.work_end_date_7.1            0.65124    0.17723      0 0.000239 ***
#   nodefactor.work_end_date_13.1           1.19588    0.22674      0  < 1e-04 ***
#   nodefactor.work_end_date_14.1           0.91052    0.12649      0  < 1e-04 ***
#   nodefactor.work_end_date_15.1           1.61416    0.23272      0  < 1e-04 ***
#   nodefactor.work_location_1.1            1.34772    0.16293      0  < 1e-04 ***
#   nodefactor.work_location_2.1           -1.66018    0.36200      0  < 1e-04 ***
#   nodefactor.work_location_3.1            1.47085    0.27509      0  < 1e-04 ***
#   nodefactor.work_location_6.1            0.90059    0.15029      0  < 1e-04 ***
#   nodefactor.work_location_7.1            1.72494    0.24348      0  < 1e-04 ***
#   nodefactor.work_location_9.1            1.68734    0.29950      0  < 1e-04 ***
#   nodefactor.work_location_12.1           1.09836    0.20312      0  < 1e-04 ***
#   nodefactor.work_position_1.1            2.38911    0.33178      0  < 1e-04 ***
#   nodefactor.work_position_4.1            0.44622    0.13179      0 0.000710 ***
#   nodefactor.work_position_6.1           -2.07769    0.21996      0  < 1e-04 ***
#   nodefactor.work_position_7.1            3.17755    0.37374      0  < 1e-04 ***
#   nodefactor.work_position_8.1            2.02659    0.66175      0 0.002196 ** 
#   nodefactor.work_start_date_3.1         -1.16200    0.34750      0 0.000827 ***
#   nodefactor.work_start_date_4.1         -1.55439    0.24607      0  < 1e-04 ***
#   nodefactor.work_start_date_5.1          1.88209    0.14469      0  < 1e-04 ***
#   nodefactor.work_start_date_9.1          0.37812    0.15862      0 0.017137 *  
#   nodefactor.work_start_date_10.1        -0.57219    0.10561      0  < 1e-04 ***
#   nodefactor.work_start_date_11.1         1.07777    0.11908      0  < 1e-04 ***
#   nodefactor.work_start_date_13.1         1.86438    0.18444      0  < 1e-04 ***
#   nodefactor.work_start_date_14.1        -1.37039    0.19516      0  < 1e-04 ***
#   nodefactor.work_start_date_15.1         1.43166    0.16234      0  < 1e-04 ***
#   nodefactor.work_start_date_16.1        -0.77499    0.12044      0  < 1e-04 ***
#   nodefactor.work_start_date_17.1        -1.31180    0.14782      0  < 1e-04 ***
#   nodefactor.work_start_date_18.1         1.11781    0.19014      0  < 1e-04 ***
#   nodefactor.work_start_date_19.1        -0.98862    0.15730      0  < 1e-04 ***
#   nodefactor.work_start_date_20.1         0.94774    0.14516      0  < 1e-04 ***
#   nodefactor.work_start_date_21.1        -1.88400    0.45399      0  < 1e-04 ***
#   nodefactor.work_start_date_22.1        -2.66121    0.44425      0  < 1e-04 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Null Deviance: 83702  on 60378  degrees of freedom
# Residual Deviance: 17998  on 60252  degrees of freedom
# 
# AIC: 18250    BIC: 19385    (Smaller is better.) 

# Goodness of fit for basic model
bmodel_0.gof <- gof(bmodel_0)
summary(bmodel_0.gof) 
plot(bmodel_0.gof)

# Goodness of fit for model containing all features
bmodelFull_0.gof <- gof(bmodelFull_0)
summary(bmodelFull_0.gof) 
plot(bmodelFull_0.gof) 

# Goodness of fit for model containing significant features only
bmodelRefined_0.gof <- gof(bmodelRefined_0)
summary(bmodelRefined_0.gof) 
plot(bmodelRefined_0.gof) 

# Find average path length of original data
average.path.length(g_0) # command in igraph package. g is igraph graph
# [1] 1.952532

# Simulate
set.seed(2017)

model.0.sim <- simulate(bmodelRefined_0, nsim=50)  # simulate to generate 50 networks

# Plot the structures of original network together with simulated network [10] and [50]
par(mfrow=c(1,3))
plot(net_0)
plot(model.0.sim[[10]])
plot(model.0.sim[[50]])

# Show features of original network together with simulated network [10] and [50]
summary(net_0)$gal
summary(model.0.sim[[10]])$gal
summary(model.0.sim[[50]])$gal

# Function for finding the average path length for give simulated network
find.apl <- function(sim){
  adj <- as.sociomatrix(sim) # Create adjacency matrix from simulated network
  g <- graph.adjacency(adj, mode="undirected") # Create igraph network
  return(average.path.length(g))
}

find.apl(model.0.sim[[10]])
find.apl(model.0.sim[[50]])

# Find apl of simulated network and create a list
sim.apl <- unlist(lapply(model.0.sim, find.apl))
# hist(sim.apl)

# Calculate the mean and standard deviation
(apl.mean <- mean(sim.apl))
(apl.sd <- sd(sim.apl))

# Plot the histogram of distribution of apl
apl.df <- data.frame(apl = sim.apl)
ggplot(apl.df, aes(x=apl)) + 
  geom_histogram(binwidth = 0.0003, color="darkgreen", fill="white") + 
  geom_vline(xintercept = apl.mean, color="blue") +
  theme_classic() +
  # ggtitle("simulated average path length") +
  xlab("average path length")
