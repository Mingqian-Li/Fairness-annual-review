#load data

day <- 0
dayend <- Inf
source(paste0(codePath,"Extract_information_per_date.R"))

data <- finaldata %>% select(t1d,
                             persist_conf_gad_all_time,
                             persist_conf_ia2a_all_time,
                             persist_conf_miaa_all_time,
                             multiple_autoantibody_all_time,
                             any_autoantibody_all_time,
                             probio,
                             country_cd,
                             fdr,
                             grs1,
                             race_ethnicity,
                             HLA_Category,
                             sex,
                             maternal,
                             c_section,
                             siblingsDiabetesType,
                             FatherDiabetesType,
                             MotherDiabetesType) %>% 
  mutate(GADA = persist_conf_gad_all_time,
         IA2A = persist_conf_ia2a_all_time,
         MIAA = persist_conf_miaa_all_time,
         multiple_autoantibody = multiple_autoantibody_all_time,
         any_autoantibody = any_autoantibody_all_time)
variable_AB <- c("persist_conf_gad_all_time","persist_conf_ia2a_all_time","persist_conf_miaa_all_time","multi_persist_ab_all_time","any_autoantibody_all_time")


test1 <- with(data, table(GADA,t1d))
test1 <- as.matrix(test1)
names1 <- names(attributes(test1)$dimnames)[1]
level1 <- c("no","yes")

test2 <- with(data, table(IA2A,t1d))
test2 <- as.matrix(test2)
names2 <- names(attributes(test2)$dimnames)[1]
level2 <- c("no","yes")

test3 <- with(data, table(MIAA,t1d))
test3 <- as.matrix(test3)
names3 <- names(attributes(test3)$dimnames)[1]
level3 <- c("no","yes")

test4 <- with(data, table(multiple_autoantibody,t1d))
test4 <- as.matrix(test4)
names4 <- "multiple autoantibodies"
level4 <- c("no","yes")

test5 <- with(data, table(any_autoantibody,t1d))
test5 <- as.matrix(test5)
names5 <- "any autoantibody"
level5 <- c("no","yes")

table_autoantibody <- rbind(test1,test2,test3,test4,test5)
table_autoantibody <- as.data.frame(table_autoantibody)
variable_name <- rep(c(names1,names2,names3,names4,names5),each = 2)
variable_states <- c(level1,level2,level3,level4,level5)
final_table_autoantibody <- cbind(variable_name,variable_states,table_autoantibody)


######################## others than ab ###################
# probio

# probio_t <- with(data, table(probio,t1d))
# probio_t <- as.matrix(probio_t)
# names_probio <- names(attributes(probio_t)$dimnames)[1]
# level_probio <- c("no","yes")

# country_cd
country_cd_t <- with(data, table(country_cd,t1d))
country_cd_t <- as.matrix(country_cd_t)
names_country_cd <- "Country"
level_country <- c("USA","Finland","Germany","Sweden")

# fdr
fdr_t <- with(data, table(fdr,t1d))
fdr_t <- as.matrix(fdr_t)
names_fdr <- "First degree relative with T1D"
level_fdr <- c("no","yes")

# Siblings T1D
siblingsDiabetesType_t <- with(data, table(siblingsDiabetesType,t1d))
siblingsDiabetesType_t <- as.matrix(siblingsDiabetesType_t)
names_siblingsDiabetesType <- "Siblings T1D"
level_siblingsDiabetesType <- c("no","yes")

# Mother T1D
MotherDiabetesType_t <- with(data, table(MotherDiabetesType,t1d))
MotherDiabetesType_t <- as.matrix(MotherDiabetesType_t)
names_MotherDiabetesType <- "Mother T1D"
level_MotherDiabetesType <- c("no","yes")

# Father T1D
FatherDiabetesType_t <- with(data, table(FatherDiabetesType,t1d))
FatherDiabetesType_t <- as.matrix(FatherDiabetesType_t)
names_FatherDiabetesType <- "Father T1D"
level_FatherDiabetesType <- c("no","yes")


# race_ethnicity

race_ethnicity_t <- with(data, table(race_ethnicity,t1d))
race_ethnicity_t <- as.matrix(race_ethnicity_t)
names_race_ethnicity <- "race"
level_race_ethnicity <- c("Hispanic","White Non-Hispanic","African Americans","All Other Races","Missing")

# HLA_Category

HLA_Category_t <- with(data, table(HLA_Category,t1d))
HLA_Category_t <- as.matrix(HLA_Category_t)
names_HLA_Category <- "HLA genotype"
level_HLA_Category <- c("other","DR4/DR3","DR4/DR4","DR4/DR8","DR3/DR3")

# sex

sex_t <- with(data, table(sex,t1d))
sex_t <- as.matrix(sex_t)
names_sex <- "Sex"
level_sex <- c("Female","Male")

# maternal

maternal_t <- with(data, table(maternal,t1d))
maternal_t <- as.matrix(maternal_t)
names_maternal <- "maternal autoantibody"
level_maternal <- c("no","yes")

# Caesarean section

c_section_t <- with(data, table(c_section,t1d))
c_section_t <- as.matrix(c_section_t)
names_c_section <- "Caesarean section"
level_c_section <- c("no","yes")


#########################################################
table_other <- rbind(country_cd_t,fdr_t,MotherDiabetesType_t,FatherDiabetesType_t,siblingsDiabetesType_t,HLA_Category_t,sex_t,c_section_t)
table_other <- as.data.frame(table_other)
variable_name <- rep(c(names_country_cd,names_fdr,names_MotherDiabetesType,names_FatherDiabetesType,names_siblingsDiabetesType,names_HLA_Category,names_sex,names_c_section),times = c(4,2,2,2,2,5,2,2))
variable_states <- c(level_country,level_fdr,level_MotherDiabetesType,level_FatherDiabetesType,level_siblingsDiabetesType,level_HLA_Category,level_sex,level_c_section)
final_table_other <- cbind(variable_name,variable_states,table_other)



################## autoantibody ################
myft <- regulartable(
  final_table_autoantibody, 
  col_keys = c("variable_name","variable_states", "0", "1"))
myft
myft <- theme_vanilla(myft)
myft
myft <- merge_v(myft, j = c("variable_name") )
myft
myft <- set_header_labels( myft, variable_name = "variable", variable_states = "","0" = "non T1D", "1" = "T1D")
myft

doc <- read_docx()
doc <- body_add_flextable(doc, value = myft)
# print(doc, target = "tables.docx")

##################### non autoantibody
final_table_other <- data.frame(final_table_other)
myft <- regulartable(
  final_table_other, 
  col_keys = c("variable_name","variable_states", "X0", "X1"))
myft
myft <- theme_vanilla(myft)
myft
myft <- merge_v(myft, j = c("variable_name") )
myft
myft <- set_header_labels( myft, variable_name = "variable", variable_states = "","X0" = paste0("non T1D (",table(data$t1d)[1],")"), "X1" = paste0("T1D (",table(data$t1d)[2],")"))
myft
myft <- autofit(myft)
myft <- align( myft, align = "center", part = "all" )
myft
doc <- read_docx()
doc <- body_add_flextable(doc, value = myft)
print(doc, target = paste0(pathfigures_tables,"tableS1.docx"))
