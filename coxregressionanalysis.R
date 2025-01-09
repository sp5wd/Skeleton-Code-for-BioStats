#This section will outline the basics for a cox regression analysis. See Survival Analysis for 
# how cox regression is utilized specifcally in the context of survial.

###############################################################################
# Data Elements
###############################################################################

# data_set <- this will be the name of our data set
# StartDate <- Date of First Treatment
# EndDate <- Date of Last Treatment
# censor_time <- calculated time 



###############################################################################
# Creating "censor_time"
###############################################################################

# Define your start date
data_set$start_date <- as.Date(data_set$dateOfDiagnosis_dt,format = "%Y-%m-%d")
head(lang$start_date)


# Create End date
data_set <- data_set %>% mutate(end_date = if_else(is.na(DeathDate),
                                                   DateLastContact, DeathDate))

### If Date Variable is not in Date format, you can use:
data_set$DeathDate <-strptime(data_set$DeathDate,format="%m/%d/%Y")
unique(data_set$DeathDate) #use to check if conversion worked


# Calculate the difference in months = censor_time
data_set$censor_time <- interval(data_set$start_date, data_set$end_date) %/% months(1) #option 1
data_set$censor_time <- as.numeric(difftime(date_set$end_date, data_set$start_date, units = "days")) / 30.44 #option 2




###############################################################################
# Unadjusted Cox PH
###############################################################################

#cox phazards unadjusted

#numeric conversions
mast_lnpos$death <- as.numeric(mast_lnpos$death)
mast_lnpos$censor_months <- as.numeric(mast_lnpos$censor_months)

#creating survival object
survobject2 <- Surv(time = mast_lnpos$censor_months, event = mast_lnpos$death)


#unadjusted surv number_of_positive_nodes
z3 <- coxph(survobject2 ~  number_of_positive_nodes, data = mast_lnpos)
summary(z3)

#forest plot for full model
ggforest(z3, data =mast_lnpos, fontsize = 0.4)


#fully adjusted cox ph
z4 <- coxph(survobject2 ~ raceth + insurance_status + Grade + HISTOLOGY_CAT + age + number_of_positive_nodes + path_n, data = mast_lnpos)
summary(z4)

#forest plot for full model
ggforest(z4, data =mast_lnpos, fontsize = 0.4)



#KM for number of positive nodes
unique(mast_lnpos$number_of_positive_nodes)
ggsurvplot(nodespositivekm2, conf.int = FALSE, pval= TRUE, risk.table = FALSE,
           title ="Overall Survival",
           legend.labs = c("1-3 Lymph Nodes", "4+ Lymph Nodess"),
           ggtheme=theme_classic2(base_size=16, base_family = "sans") + theme(title = element_text(face = "bold")),
           font.family = "sans",
           legend = c(.3,.2), break.time.by = 10,
           legend.title = "Number of Postive Nodes",
           censor.shape = 3, censor.size=0.1, size = 1,
           color = "strata",
           linetype = "strata",
           xlab = "Months since Diagnosis",
           ylab = "Survival Probability",
           xlim = c(0,80),
           risk.table.pos="out",
           risk.table.col="black",
           fontsize = 7,
           risk.table.y.text = FALSE,
           tables.theme = theme_cleantable())