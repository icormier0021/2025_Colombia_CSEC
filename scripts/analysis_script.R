#VACs Analysis Script- Stephens et al. (2025)
#Written By: Isaac Cormier
#Date: 2025/05/28

pkg_helper <- function(pkg_name) {
        if (!requireNamespace(pkg_name, quietly = TRUE)) {
                install.packages(pkg_name)
        }
        library(pkg_name, character.only = TRUE)
}

# Load required libraries
packages <- c("survey", "haven", "dplyr", "labelled", "tidyr", "stringr", "writexl", "officer", "rmarkdown")
invisible(lapply(packages, pkg_helper))

# --- 1. Load SPSS data file ---
nationalVACS <- read_sav("Colombia VACS SPSS data.sav")
#priorityVACS <- read_sav("Colombia VACS priority data.sav")
#NOTE- Until we hear back from the VACs team, I am going to filter out priority for now and just use the national sample
data <- nationalVACS
        
# --- 2. Mutate data types prior to recoding ---
data <- data %>%
        mutate(
                DEPT = as.numeric(DEPT),
                MUNI = as.numeric(MUNI),
                PSU  = as.numeric(PSU),
                Wght_final = as.numeric(Wght_final)
        )

# --- 3. Recode variables ---
recode_01 <- function(x) {  #helper functions for recoding variables
        dplyr::case_when(
                x %in% c(98, 99) ~ NA_real_,
                x == 1 ~ 1,
                x == 2 ~ 0,
                TRUE ~ NA_real_
        )
}

recode_copy <- function(x) {
        case_when(
                x %in% c(98, 99) ~ NA_real_,
                TRUE ~ as.numeric(x)
        )
}
reverse_3 <- function(x) {
        case_when(
                x %in% c(98, 99) ~ NA_real_,
                x == 1 ~ 3,
                x == 2 ~ 2,
                x == 3 ~ 1,
                TRUE ~ NA_real_
        )
}
reverse_4 <- function(x) {
        case_when(
                x %in% c(98, 99) ~ NA_real_,
                x == 1 ~ 4,
                x == 2 ~ 3,
                x == 3 ~ 2,
                x == 4 ~ 1,
                TRUE ~ NA_real_
        )
}
reverse_5 <- function(x) {
        case_when(
                x %in% c(98, 99) ~ NA_real_,
                x == 1 ~ 5,
                x == 2 ~ 4,
                x == 3 ~ 3,
                x == 4 ~ 2,
                x == 5 ~ 1,
                TRUE ~ NA_real_
        )
}

data <- data %>% 
        select(-starts_with("Parenphys"),
               -starts_with("Nonparenphys"))

data <- data %>%
        mutate(
                Sharedhous  = recode_01(H6),
                Telehom = recode_01(H7H),
                Govsupp = recode_01(H18),
                NGOsupp = recode_01(H19),
                Victimreg = recode_01(H19A),
                Parentdie = recode_01(H55A),
                childouthome = recode_01(H56),
                childstreet = recode_01(H58),
                
                Victimregtype = recode_copy(H19B),
                Moneyworry = reverse_5(H20),
                Sex = as.factor(Sex),
                Age = recode_copy(Q2),
                Eth = factor(
                        case_when(
                                Q2A %in% c(98, 99) ~ NA_real_, 
                                Q2A == 7 ~ 0,
                                TRUE ~ as.numeric(Q2A)
                                ),
                        levels = 1:6,
                        labels = c(
                        "INDIGENOUS",
                        "GYPSY (ROMA)",
                        "RAIZAL OF THE ARCHIPELAGO",
                        "PALENQUERO OF SAN BASILIO",
                        "BLACK/MULATO/AFROCOLOMBIAN OR AFRODESCENDANT",
                        "MESTIZO")),
                School = recode_01(Q3),
                Foodins = recode_01(Q7AA),
                Emp = recode_01(Q13),
                Momaliv = recode_01(Q19),
                Dadaliv = recode_01(Q29),
                Partner = recode_01(Q58),
                Safetyvio = recode_01(Q70),
                Safetywar = recode_01(Q71),
                Bullyvic = recode_01(Q306),
                
                Momaway = case_when(Q21 %in% c(98, 99) ~ NA_real_, Q21 == 11 ~ 0, Q21 >= 1 & Q21 <= 10 ~ 1),
                Dadaway = case_when(Q31 %in% c(98, 99) ~ NA_real_, Q31 == 11 ~ 0, Q31 >= 1 & Q31 <= 10 ~ 1),
                
                Friend = reverse_4(Q7),
                Momclose = reverse_4(Q25),
                Dadclose = reverse_4(Q35),
                
                Matins1 = recode_01(Q7AB),
                Matins3 = recode_01(Q7AD),
                Matins2 = recode_01(Q7AC),
                
                Parmonit1 = reverse_3(Q36A),
                Parmonit2 = reverse_3(Q36B),
                Parmonit3 = reverse_3(Q36C),
                Parmonit4 = reverse_3(Q36D),
                Parmonit5 = reverse_3(Q36E),
                
                sexorient = recode_copy(Q59),
                WitparentIPV = case_when(Q80 %in% c(98, 99) ~ NA_real_, Q80 == 1 ~0, Q80 == 2 ~1, Q80 == 3 ~1),
                Witsib = case_when(Q82 %in% c(98, 99) ~ NA_real_, Q82 == 1 ~0, Q82 == 2 ~1, Q82 == 3 ~1),
                Witcomvio = case_when(Q84 %in% c(98, 99) ~ NA_real_, Q84 == 1 ~0, Q84 == 2 ~1, Q84 == 3 ~1),
                Witconflict = case_when(Q86 %in% c(98, 99) ~ NA_real_, Q86 == 1 ~0, Q86 == 2 ~1, Q86 == 3 ~1) ,
                
                Secact = recode_01(Q407),
                Firstwanted = recode_01(Q409),
                CSEC = recode_01(Q500),
                Sexabuse = recode_01(Q600),
                Rapeattempt1 = recode_01(Q700A),
                Rapeattempt2 = recode_01(Q700B),
                Rapecomplete1 = recode_01(Q800A),
                Rapecomplete2 = recode_01(Q800B),
                
                Drugs = recode_01(Q1205),
                Selfharm = recode_01(Q1207),
                SI = recode_01(Q1208),
                Suiattempt = recode_01(Q1209),
                STI = recode_01(Q1210),
                
                Alcohol = case_when(Q1200 %in% c(98, 99) ~ NA_real_, Q1200 == 97 ~ 0, TRUE ~ 1),
                
                IPVphys1 = recode_01(Q100A),
                IPVphys2 = recode_01(Q100B),
                IPVphys3 = recode_01(Q100C),
                IPVphys4 = recode_01(Q100D),
                Peerphys1 = recode_01(Q116A),
                Peerphys2 = recode_01(Q116B),
                Peerphys3 = recode_01(Q116C),
                Peerphys4 = recode_01(Q116D),
                Parentphys1 = recode_01(Q128A),
                Parentphys2 = recode_01(Q128B),
                Parentphys3 = recode_01(Q128C),
                Parentphys4 = recode_01(Q128D),
                Nonparentphys1 = recode_01(Q142A),
                Nonparentphys2 = recode_01(Q142B),
                Nonparentphys3 = recode_01(Q142C),
                Nonparentphys4 = recode_01(Q142D),
                ParentEmotabuse1 = recode_01(Q300A),
                ParentEmotabuse2 = recode_01(Q300B),
                ParentEmotabuse3 = recode_01(Q300C),
                ParentEmotabuse4 = recode_01(Q300D),
                IPVEmot1 = recode_01(Q305A),
                IPVEmot2 = recode_01(Q305B),
                IPVEmot3 = recode_01(Q305C),
                IPVEmot4 = recode_01(Q305D),
                IPVEmot5 = recode_01(Q305E),
                IPVEmot6 = recode_01(Q305F),
                
                Physcorp = case_when(Q400B %in% c(98, 99) ~ NA_real_, Q400B == 2 ~ 0, Q400B >= 1 ~ 1),
                
                Sexinit = recode_copy(Q408),
                Sexpar = recode_copy(Q417),
                CSECvicage = recode_copy(Q504),
                CSECageperp = recode_copy(Q505),
                CSECtype = recode(
                        Q503,
                        `A` = "Male Friend/Neighbor",
                        `B` = "Male Teacher",
                        `C` = "Male Community Leader",
                        `D` = "Male Religious Leader",
                        `E` = "Male Employer",
                        `F` = "Boyfriend/Ex-Boyfriend/Romantic Partner/Ex-Romantic Partner",
                        `G` = "Male classmate/schoolmate",
                        `H` = "Male Police/Soldier/Security Person",
                        `I` = "Male relative",
                        `J` = "Male tourist or non-national",
                        `K` = "Male stranger",
                        `L` = "Male criminal gang member",
                        `M` = "Male I met on the internet",
                        `N` = "Female Friend/Neighbor",
                        `O` = "Female Teachers",
                        `P` = "Female community leader",
                        `Q` = "Female religious leader",
                        `R` = "Female employer",
                        `S` = "Girlfriend/Ex-Girlfriend/Romantic Partner/Ex-Romantic Partner ",
                        `T` = "Female Classmate/Schoolmate",
                        `U` = "Female Police/Soilder/Security Person",
                        `V` = "Female Relative",
                        `W` = "Female Tourist or Non-National",
                        `X` = "Female Stranger",
                        `1` = "Female Criminal Gang Member",
                        `2` = "Female I met on the internet",
                        `Y` = "Don't Know",
                        `Z` = "Declined",
                ),
        
                CSECtourist = case_when(Q503 %in% c("J", "W") ~ 1, Q503 %in% c("98", "99") ~ NA_real_, TRUE ~ 0),
                
                MHA = reverse_5(Q1206A),
                MHB = reverse_5(Q1206B),
                MHC = reverse_5(Q1206C),
                MHD = reverse_5(Q1206D),
                MHE = reverse_5(Q1206E),
                MHF = reverse_5(Q1206F)
        )

data <- set_variable_labels(data,
                            Sharedhous = "Live in shared housing",
                            Telehom = "Internet in home",
                            Govsupp = "Government financial support",
                            NGOsupp = "NGO or other community organization support",
                            Victimreg = "Recognized on national victim registry",
                            Parentdie = "Parent has died due to violence",
                            childouthome = "Child under 18 lived outside of home",
                            childstreet = "Child less than 18 lived on the street",
                            Victimregtype = "Type of conflict victim",
                            Moneyworry = "Respondent or family worried about money over past year",
                            Age = "Age of respondent",
                            Eth = "Ethnicity of respondent",
                            School = "Ever attended school",
                            Foodins = "Food insecurity",
                            Emp = "Employment",
                            Momaliv = "Mother alive",
                            Dadaliv = "Dad living",
                            Partner = "Ever had a romantic partner",
                            Safetyvio = "Feel safe from violence",
                            Safetywar = "Feel safe from war",
                            Bullyvic = "Bullying victim",
                            Momaway = "Biological mother ever lived far away",
                            Dadaway = "Biological father ever lived far away",
                            Friend = "Closeness to friends",
                            Momclose = "Bio mom close",
                            Dadclose = "Bio dad close",
                            Matins1 = "Material insecurity 7AB",
                            Matins2 = "Material insecurity 2",
                            Matins3 = "Material insecurity AD",
                            Parmonit1 = "Parental monitoring item A",
                            Parmonit2 = "Parental monitoring item B",
                            Parmonit3 = "Parental monitoring item C",
                            Parmonit4 = "Parental monitoring item D",
                            Parmonit5 = "Parental monitoring item E",
                            sexorient = "Sexual orientation",
                            WitparentIPV = "Witness parental IPV",
                            Witsib = "Witnessing sibling abuse",
                            Witcomvio = "Witness violence in community",
                            Witconflict = "Witness armed conflict",
                            Secact = "Ever had sex",
                            Firstwanted = "First sexual experience wanted",
                            CSEC = "Ever involved in transactional sex",
                            Sexabuse = "Ever been sexually touched when not wanted",
                            Rapeattempt1 = "Rape attempt 1",
                            Rapeattempt2 = "Rape attempt 2",
                            Rapecomplete1 = "Completed rape 1",
                            Rapecomplete2 = "Completed rape 2",
                            Drugs = "Used drugs in 30 days",
                            Selfharm = "Self harmed",
                            SI = "Wanted to be dead",
                            Suiattempt = "Suicide attempt",
                            STI = "Ever had STI",
                            Alcohol = "Ever drank alcohol",
                            IPVphys1 = "Intimate partner physical violence 1",
                            IPVphys2 = "Intimate partner physical violence 2",
                            IPVphys3 = "Intimate partner physical violence 3",
                            IPVphys4 = "Intimate partner physical violence 4",
                            Peerphys1 = "Peer physical violence 1",
                            Peerphys2 = "Peer physical violence 2",
                            Peerphys3 = "Peer physical violence 3",
                            Peerphys4 = "Peer physical violence 4",
                            Parentphys1 = "Parent, adult caregiver and other adult relative physical violence 1",
                            Parentphys2 = "Parent, adult caregiver and other adult relative physical violence 2",
                            Parentphys3 = "Parent, adult caregiver and other adult relative physical violence 3",
                            Parentphys4 = "Parent, adult caregiver and other adult relative physical violence 4",
                            Nonparentphys1 = "Adult in community/neighborhood physical violence 1",
                            Nonparentphys2 = "Adult in community/neighborhood physical violence 2",
                            Nonparentphys3 = "Adult in community/neighborhood physical violence 3",
                            Nonparentphys4 = "Adult in community/neighborhood physical violence 4",
                            ParentEmotabuse1 = "Parent, adult caregiver and other adult relative emotional violence 1",
                            ParentEmotabuse2 = "Parent, adult caregiver and other adult relative emotional violence 2",
                            ParentEmotabuse3 = "Parent, adult caregiver and other adult relative emotional violence 3",
                            ParentEmotabuse4 = "Parent, adult caregiver and other adult relative emotional violence 4",
                            IPVEmot1 = "Intimate partner emotional violence 1",
                            IPVEmot2 = "Intimate partner emotional violence 2",
                            IPVEmot3 = "Intimate partner emotional violence 3",
                            IPVEmot4 = "Intimate partner emotional violence 4",
                            IPVEmot5 = "Intimate partner emotional violence 5",
                            IPVEmot6 = "Intimate partner emotional violence 6",
                            
                            MHA = "Mental health: Nervousness",
                            MHB = "Mental health: Hopeless/anxious",
                            MHC = "Mental health: Restless",
                            MHD = "Mental health: Extreme Sadness",
                            MHE = "Mental health: Increased Effort",
                            MHF = "Mental health: Worthlessness",
                            CSECtype = "Identity of the CSEC perpetrator",
                            CSECtourist = "Was CSEC perpetrator a tourist",
                            Physcorp = "Physical corporal punishment parent",
                            Sexinit = "Age at sexual initiation",
                            Sexpar = "Number of sex partners",
                            CSECvicage = "Age of CSEC victimization first",
                            CSECageperp = "Age of CSEC perpetrator")

# --- 4.Compute the following variables from scales or collapses variables ---
data <- data %>% 
        mutate(Foodandmatins = rowSums(across(matches("^Matins|^Foodins")), na.rm = TRUE),
               Monit = rowSums(across(starts_with("Parmonit")), na.rm = TRUE),
               IPVphystot= rowSums(across(starts_with("IPVphys")), na.rm = TRUE),
               Peerphystotal= rowSums(across(starts_with("Peerphys")), na.rm = TRUE),
               Parentphy= rowSums(across(starts_with("Parentphys")), na.rm = TRUE),
               Nonparentphys= rowSums(across(starts_with("Nonparentphys")), na.rm = TRUE),
               Parentemotabuse= rowSums(across(starts_with("ParentEmotabuse")), na.rm = TRUE),
               IPVEmotional= rowSums(across(starts_with("IPVEmot")), na.rm = TRUE),
               MHsxs = rowSums(across(starts_with("MH")), na.rm = TRUE),
               Witanyvio = rowSums(across(c("WitparentIPV", "Witsib", "Witcomvio", "Witconflict")), na.rm = TRUE),
               Physviolence = rowSums(across(c("IPVphystot", "Peerphystotal", "Parentphy", "Nonparentphys")), na.rm = TRUE),
               Sexabusebroad = rowSums(across(matches("^Sexabuse|^Rapeattempt|^Rapecomplete")), na.rm = TRUE),
               Sexabusenarrow = rowSums(across(matches("^Sexabuse|^Rapecomplete")), na.rm = TRUE),
               IPVtotal = rowSums(across(c("IPVphystot", "IPVEmotional")), na.rm = TRUE)
               )
data <-  data %>% 
        mutate(across(
                c(IPVphystot, Peerphystotal, Parentphy, IPVEmotional,Parentemotabuse, 
                  Nonparentphys, MHsxs, Witanyvio, Physviolence, Sexabusebroad, Sexabusenarrow, IPVtotal),
                ~ case_when(
                        is.na(.) ~ NA_real_,
                        . == 0   ~ 0,
                        . >= 1   ~ 1
                )
        )) 
data <- set_variable_labels(data,
                            Foodandmatins = "All material and food insecurity",
                            Monit = "All parental monitoring",
                            IPVphystot = "All intimate partner physical violence" ,
                            Peerphystotal = "All peer physical violence" ,
                            Parentphy = "All parent, adult caregiver, and other adult relative physical violence",
                            Nonparentphys = "All adult in community/neighborhood physical violence" ,
                            Parentemotabuse = "All parent, adult caregiver and other adult relative emotional violence",
                            IPVEmotional = "All intimate partner emotional violence",
                            MHsxs = "All mental health concerns",
                            Witanyvio = "Witnessed any abuse, violence, or conflict",
                            Physviolence = "Experienced any physical violence",
                            Sexabusebroad = "Reported sexual abuse including attempted rape",
                            Sexabusenarrow = "Reported sexual abuse excluding attempted rape",
                            IPVtotal = "Experienced any physical or emotional violence"
                            )

vars_to_keep <- c(
        "Foodandmatins",
        "Monit",
        "IPVphystot",
        "Peerphystotal",
        "Parentphy",
        "Nonparentphys",
        "Parentemotabuse",
        "IPVEmotional",
        "MHsxs",
        "Witanyvio",
        "Physviolence",
        "Sexabusebroad",
        "Sexabusenarrow",
        "IPVtotal",
        "Sharedhous",
        "Telehom",
        "Govsupp",
        "NGOsupp",
        "Victimreg",
        "Parentdie",
        "childouthome",
        "childstreet",
        "Victimregtype",
        "Moneyworry",
        "Age",
        "Eth",
        "School",
        "Foodins",
        "Emp",
        "Momaliv",
        "Dadaliv",
        "Partner",
        "Safetyvio",
        "Safetywar",
        "Bullyvic",
        "Momaway",
        "Dadaway",
        "Friend",
        "Momclose",
        "Dadclose",
        "Matins1",
        "Matins2",
        "Matins3",
        "Parmonit1",
        "Parmonit2",
        "Parmonit3",
        "Parmonit4",
        "Parmonit5",
        "sexorient",
        "WitparentIPV",
        "Witsib",
        "Witcomvio",
        "Witconflict",
        "Secact",
        "Firstwanted",
        "CSEC",
        "Sexabuse",
        "Rapeattempt1",
        "Rapeattempt2",
        "Rapecomplete1",
        "Rapecomplete2",
        "Drugs",
        "Selfharm",
        "SI",
        "Suiattempt",
        "STI",
        "Alcohol",
        "IPVphys1",
        "IPVphys2",
        "IPVphys3",
        "IPVphys4",
        "Peerphys1",
        "Peerphys2",
        "Peerphys3",
        "Peerphys4",
        "Parentphys1",
        "Parentphys2",
        "Parentphys3",
        "Parentphys4",
        "Nonparentphys1",
        "Nonparentphys2",
        "Nonparentphys3",
        "Nonparentphys4",
        "ParentEmotabuse1",
        "ParentEmotabuse2",
        "ParentEmotabuse3",
        "ParentEmotabuse4",
        "IPVEmot1",
        "IPVEmot2",
        "IPVEmot3",
        "IPVEmot4",
        "IPVEmot5",
        "IPVEmot6",
        "MHA",
        "MHB",
        "MHC",
        "MHD",
        "MHE",
        "MHF",
        "CSECtype",
        "CSECtourist",
        "Physcorp",
        "Sexinit",
        "Sexpar",
        "CSECvicage",
        "CSECageperp",
        "Sex",
        "DEPT",
        "MUNI",
        "PSU",
        "USM",
        "Frame",
        "HH",
        "VACS_ID",
        "Wght_final"
)

data <- data %>% 
        select(all_of(vars_to_keep))

save_variable_types(data)
# --- 5. Create analysis filters ---
data <- data %>% 
        mutate(
                Filter_CSECendorsed = case_when(CSECvicage == NA_real_ ~0, CSECvicage == 0 ~0, CSECvicage >= 18 & CSECvicage <= 24 ~0, CSECvicage >= 13 & CSECvicage <= 17 ~1),
                age_Filter = case_when(Age >= 13 & Age <= 17 ~1, TRUE ~0),
                Full_filter = if_else(age_Filter == 1 | Filter_CSECendorsed == 1, 1, 0),
                Answer_CSEC = case_when(CSEC == NA_real_ ~0, CSEC >= 0 & CSEC <= 1 ~1),
                Expand_Filter = (Full_filter * 1) + (Answer_CSEC * 2),
                test_Filter = )

data <- set_variable_labels(data,
                            Filter_CSECendorsed = 'Person was less than 18 when transactional sex occured',
                            age_Filter = 'under 18',
                            Answer_CSEC = 'Answered question about CSEC')

#Set variables to factors if categorical
cont_vars <- c(
        "Age",         # exact age
        "Sexinit",     # age at first sex
        "Sexpar",      # number of partners
        "CSECvicage",  # age at first transactional sex
        "CSECageperp"  # age of perpetrator
)

# — 1. grab *all* of the newly created analysis variables — 
#    (just paste their names here in one big character vector)
all_analysis_vars <- c(
        "Sharedhous","Telehom","Govsupp","NGOsupp","Victimreg","Parentdie",
        "childouthome","Victimregtype","Moneyworry","Sex","Eth",
        "School","Foodins","Emp","Momaliv","Dadaliv","Partner","Safetyvio",
        "Safetywar","Bullyvic","Momaway","Dadaway","Friend","Momclose","Dadclose",
        "Matins1","Matins2","Matins3",
        "Parmonit1","Parmonit2","Parmonit3","Parmonit4","Parmonit5",
        "sexorient","WitparentIPV","Witsib","Witcomvio","Witconflict",
        "Secact","Firstwanted","CSEC","Sexabuse","Rapeattempt1","Rapeattempt2",
        "Rapecomplete1","Rapecomplete2","Drugs","Selfharm","SI","Suiattempt",
        "STI","Alcohol",
        "IPVphys1","IPVphys2","IPVphys3","IPVphys4",
        "Peerphys1","Peerphys2","Peerphys3","Peerphys4",
        "Parentphys1","Parentphys2","Parentphys3","Parentphys4",
        "Nonparentphys1","Nonparentphys2","Nonparentphys3","Nonparentphys4",
        "ParentEmotabuse1","ParentEmotabuse2","ParentEmotabuse3","ParentEmotabuse4",
        "IPVEmot1","IPVEmot2","IPVEmot3","IPVEmot4","IPVEmot5","IPVEmot6",
        "Physcorp","CSECtourist","CSECtype",
        "MHA","MHB","MHC","MHD","MHE","MHF",
        "Foodandmatins","Monit","IPVphystot","Peerphystotal","Parentphy",
        "Nonparentphys","Parentemotabuse","IPVEmotional","MHsxs",
        "Witanyvio","Physviolence","Sexabusebroad","Sexabusenarrow","IPVtotal"
)

# — 2. derive the *categorical* set by removing the continuous ones — 
factor_vars <- setdiff(all_analysis_vars, cont_vars)

# — 3. coerce them to factors in your data frame (so the design picks them up) — 
data <- data %>%
        mutate(across(all_of(factor_vars), ~ factor(.)))


# --- 5. Define survey design
vacs_design <- svydesign(
        id = ~PSU,  #Tells R that the sample was selected in clusters
        strata = ~DEPT,  #Tells R that the departments should be treated as distinct
        weights = ~Wght_final, #Final sampling weight
        data = data,
        nest = TRUE  # Tells R that the PSUs are nested inside strata (i.e., each PSU cluster belongs to only one stratum)
)

# --- 6. Review sample characteristics
# Overall counts
total_n    <- nrow(data)
num_vars   <- ncol(data)
num_q_vars <- sum(startsWith(names(data), "Q"))


# Sex distribution
sex_total <- data %>%
        count(Sex, name = "N") %>%
        mutate(Percent = round(100 * N / sum(N), 1))

# Age summary by Frame and Sex
age_summary <- data %>%
        group_by(Sex) %>%
        summarise(
                mean_age = mean(Age, na.rm=TRUE),
                sd_age   = sd(Age,   na.rm=TRUE),
                n        = n(),
                Percent  = round(100 * n / sum(n), 1),
                .groups  = "drop"
        )

# Print results
global_info <- sprintf(
        "Total participants: %d\nTotal vars: %d (Q-variables: %d)",
        total_n, num_vars, num_q_vars
)
cat(global_info, "\n\n")

cat("Sex breakdown (total sample):\n"); print(sex_total); cat("\n")
cat("Age summary by Frame & Sex:\n"); print(age_summary); cat("\n")


# --- 7. CSEC analysis restricted to those under 18 or those who reported CSEC when less than 18
vars    <- c("CSEC", "CSECtype", "CSECtourist", "CSECvicage", "CSECageperp")
design0 <- vacs_design                           # full national design
design1 <- subset(vacs_design, Full_filter == 1) # filtered design

for (v in vars) {
        cat("========================================================\n")
        cat("Variable:", v, "\n")
        # for CSECtype, coerce to factor so svymean treats it as a set of indicators
        if (v == "CSECtype") {
                d0 <- update(design0, CSECtype = factor(CSECtype))
                d1 <- update(design1, CSECtype = factor(CSECtype))
        } else {
                d0 <- design0
                d1 <- design1
        }
        
        # —— Full national sample ——
        prev0 <- svymean( as.formula(paste0("~", v)), design = d0, na.rm = TRUE)
        var0  <- svyvar(  as.formula(paste0("~", v)), design = d0, na.rm = TRUE)
        ci0   <- confint(prev0)
        de0   <- deff(prev0)
        tot0  <- svytotal(as.formula(paste0("~", v)), design = d0, na.rm = TRUE)
        
        cat("**Full national sample**\n")
        if (v == "CSECtype") {
                print(prev0) 
                cat("Design‐based variance of each level:\n"); print(var0)
                cat("95% CIs by level:\n");                print(ci0)
                cat("Design effects by level:\n");        print(de0)
                cat("Weighted total counts by level:\n"); print(tot0)
        } else {
                cat(sprintf("Point estimate       : %.4f\n", coef(prev0)))
                cat(sprintf("Standard error       : %.4f\n", SE(prev0)))
                cat(sprintf("Population variance  : %.4f\n",   as.numeric(var0)))
                cat(sprintf("95%% CI               : [%.4f, %.4f]\n",
                            ci0[1], ci0[2]))
                cat(sprintf("Design effect (DEFF) : %.2f\n", de0))
                cat(sprintf("Weighted total       : %.0f\n\n", coef(tot0)))
        }
        
        # —— Filtered subset ——
        prev1 <- svymean( as.formula(paste0("~", v)), design = d1, na.rm = TRUE)
        var1  <- svyvar(  as.formula(paste0("~", v)), design = d1, na.rm = TRUE)
        ci1   <- confint(prev1)
        de1   <- deff(prev1)
        tot1  <- svytotal(as.formula(paste0("~", v)), design = d1, na.rm = TRUE)
        
        cat("**Filtered (Full_filter==1)**\n")
        if (v == "CSECtype") {
                print(prev1)
                cat("Design‐based variance of each level:\n"); print(var1)
                cat("95% CIs by level:\n");                print(ci1)
                cat("Design effects by level:\n");        print(de1)
                cat("Weighted total counts by level:\n"); print(tot1)
        } else {
                cat(sprintf("Point estimate       : %.4f\n", coef(prev1)))
                cat(sprintf("Standard error       : %.4f\n", SE(prev1)))
                cat(sprintf("Population variance  : %.4f\n",   as.numeric(var1)))
                cat(sprintf("95%% CI               : [%.4f, %.4f]\n",
                            ci1[1], ci1[2]))
                cat(sprintf("Design effect (DEFF) : %.2f\n", de1))
                cat(sprintf("Weighted total       : %.0f\n\n", coef(tot1)))
        }
}
cat("========================================================\n")


vars <- c(
        "Sex","Age","Eth","sexorient","School","Emp","Partner","Telehom","Govsupp","NGOsupp",
        "Foodandmatins","childouthome","childstreet","Moneyworry","Momclose","Dadclose","Monit",
        "Victimreg","Safetyvio","Safetywar","Physviolence","IPVphystot","Physcorp","Parentphy",
        "Sexabusebroad","Sexabusenarrow","Witanyvio","WitparentIPV","Witconflict","IPVtotal",
        "Secact","Firstwanted","Sexinit","Sexpar","STI","Drugs","Selfharm","SI","Suiattempt",
        "Alcohol","MHsxs", "CSEC", "CSECtype", "CSECtourist", "CSECvicage", "CSECageperp"
)
class_list <- sapply(vacs_design$variables, class)
design0 <- vacs_design                           # full national design
design1 <- subset(vacs_design, Full_filter == 1) # filtered design

#Save output to word document
doc <- read_docx() %>%
        body_add_par("VACS Survey Summary", style = "heading 1")

for(v in vars){
        out <- capture.output({
                cat("================================================================================\n")
                cat("Variable:", v, "\n\n")
                
                # if character/factor, build a named list for update(..., var = factor(var))
                if(class_list[[v]] %in% c("character","factor")){
                        args0 <- c(list(design0),
                                   setNames(
                                           list(as.call(list(as.name("factor"), as.name(v)))),
                                           v
                                   )
                        )
                        d0 <- do.call(update, args0)
                        
                        args1 <- c(list(design1),
                                   setNames(
                                           list(as.call(list(as.name("factor"), as.name(v)))),
                                           v
                                   )
                        )
                        d1 <- do.call(update, args1)
                } else {
                        d0 <- design0
                        d1 <- design1
                }
                
                # —— Full national sample ——
                prev0 <- svymean( as.formula(paste0("~",v)), design = d0, na.rm = TRUE)
                var0  <- svyvar(  as.formula(paste0("~",v)), design = d0, na.rm = TRUE)
                ci0   <- confint(prev0)
                de0   <- deff(prev0)
                tot0  <- svytotal(as.formula(paste0("~",v)), design = d0, na.rm = TRUE)
                
                cat("**Full national sample**\n")
                if(class_list[[v]] %in% c("character","factor")){
                        print(prev0)
                        cat("Design‐based variance (levels):\n"); print(var0)
                        cat("95% CIs (levels):\n");           print(ci0)
                        cat("Design effects (levels):\n");   print(de0)
                        cat("Weighted totals (levels):\n");  print(tot0)
                } else {
                        cat(sprintf("  Mean/proportion        : %.4f\n", coef(prev0)))
                        cat(sprintf("  SE                     : %.4f\n", SE(prev0)))
                        cat(sprintf("  Variance               : %.4f\n", as.numeric(var0)))
                        cat(sprintf("  95%% CI                 : [%.4f, %.4f]\n", ci0[1], ci0[2]))
                        cat(sprintf("  Design effect (DEFF)   : %.2f\n", de0))
                        cat(sprintf("  Weighted total (cases) : %.0f\n\n", coef(tot0)))
                }
                
                # —— Filtered subset ——
                prev1 <- svymean( as.formula(paste0("~",v)), design = d1, na.rm = TRUE)
                var1  <- svyvar(  as.formula(paste0("~",v)), design = d1, na.rm = TRUE)
                ci1   <- confint(prev1)
                de1   <- deff(prev1)
                tot1  <- svytotal(as.formula(paste0("~",v)), design = d1, na.rm = TRUE)
                
                cat("**Filtered (Full_filter==1)**\n")
                if(class_list[[v]] %in% c("character","factor")){
                        print(prev1)
                        cat("Design‐based variance (levels):\n"); print(var1)
                        cat("95% CIs (levels):\n");           print(ci1)
                        cat("Design effects (levels):\n");   print(de1)
                        cat("Weighted totals (levels):\n");  print(tot1)
                } else {
                        cat(sprintf("  Mean/proportion        : %.4f\n", coef(prev1)))
                        cat(sprintf("  SE                     : %.4f\n", SE(prev1)))
                        cat(sprintf("  Variance               : %.4f\n", as.numeric(var1)))
                        cat(sprintf("  95%% CI                 : [%.4f, %.4f]\n", ci1[1], ci1[2]))
                        cat(sprintf("  Design effect (DEFF)   : %.2f\n", de1))
                        cat(sprintf("  Weighted total (cases) : %.0f\n\n", coef(tot1)))
                }
        })
        doc <- body_add_par(doc, paste0("Variable:", v), style = "heading 2")
        for(line in out){
                doc <- body_add_par(doc, line, style = "Normal")
                }
        }

print(doc, target = "VACS_Survey_Summary2.docx")



one_level <- sapply(factor_vars, function(v){
        x <- design0$variables[[v]]
        length(unique(na.omit(x)))
})
# Show you which ones have fewer than 2 levels:
one_level[ one_level < 2 ]

doc <- read_docx()

for(v in factor_vars){
        fmla <- as.formula(paste0("~", v))
        
        out <- capture.output({
                cat("\n", strrep("=", 60), "\n")
                cat("Variable:", v, "\n\n")
                
                for(scope in c("Full national sample" = "0",
                               "Filtered (Full_filter == 1)" = "1")){
                        des <- if(scope=="0") design0 else design1
                        
                        cat("∘", names(scope), "\n")
                        
                        # 1) proportions + SE
                        m <- svymean(fmla, design = des, na.rm=TRUE)
                        print(m)
                        
                        # 2) confints
                        ci <- confint(m)
                        cat("  – 95% CI:\n"); print(ci)
                        
                        # 3) DEFF
                        de <- deff(m)
                        cat("  – DEFF:\n"); print(de)
                        
                        # 4) weighted totals
                        tot <- svytotal(fmla, design = des, na.rm=TRUE)
                        cat("  – Weighted totals (svytotal):\n"); print(tot)
                        
                        # 5) svytable
                        tbl <- svytable(fmla, design = des)
                        cat("  – Weighted table (svytable):\n"); print(tbl)
                        
                        # 6) proportions from table
                        pr <- prop.table(tbl)
                        cat("  – Proportions (prop.table):\n"); print(pr)
                        
                        cat("\n")
                }
        })
        
        # append to Word doc
        doc <- body_add_par(doc, paste0("Variable: ", v), style = "heading 2")
        for(line in out){
                doc <- body_add_par(doc, line, style = "Normal")
        }
}

print(doc, target = "VACS_categorical_summary2.docx")

doc <- read_docx()
for(v in factor_vars){
        out <- capture.output({
        cat("\n", strrep("=", 60), "\n")
        cat("Variable:", v, "\n\n")
        fmla <- reformulate(v)    # builds ~v
        
        # full national
        prev0 <- svymean( fmla, design=design0, na.rm=TRUE )
        ci0   <- confint(prev0)
        de0   <- deff(prev0)
        tot0  <- svytotal(fmla, design=design0, na.rm=TRUE)
        
        cat("Full national sample:\n")
        print(prev0)             # proportions + SE
        cat("  – 95% CI:\n");     print(ci0)
        cat("  – DEFF:\n");       print(de0)
        cat("  – Weighted totals:\n"); print(tot0)
        
        # filtered subset
        prev1 <- svymean( fmla, design=design1, na.rm=TRUE )
        ci1   <- confint(prev1)
        de1   <- deff(prev1)
        tot1  <- svytotal(fmla, design=design1, na.rm=TRUE)
        
        cat("\n Filtered (Full_filter == 1):\n")
        print(prev1)
        cat("  – 95% CI:\n");     print(ci1)
        cat("  – DEFF:\n");       print(de1)
        cat("  – Weighted totals:\n"); print(tot1)
        })
doc <- body_add_par(doc, paste0("Variable:", v), style = "heading 2")
        for(line in out){
                doc <- body_add_par(doc, line, style = "Normal")
        }
}

print(doc, target = "VACS_Survey_categorical_Summary2.docx")
# — 8. Bivariate analyses with survey‐corrected ORs — 
design1 <- subset(vacs_design, Full_filter == 1)

# 1) first set of categorical predictors
vars1 <- c("Eth", "sexorient", "School", "Emp", "Partner",
           "Telehom", "Govsupp", "NGOsupp", "childouthome", "childstreet",
           "Victimreg", "Safetyvio", "Safetywar")

for (v in vars1) {
        cat("\n---- CSEC ×", v, "(filtered subset) ----\n")
        
        # build and print weighted 2×K table
        tab_fmla <- as.formula(paste0("~ CSEC + ", v))
        tab      <- svytable(tab_fmla, design1)
        print(tab)
        
        # row‐ and column‐percentages
        cat("Row %:\n"); print(prop.table(tab, 1))
        cat("Col %:\n"); print(prop.table(tab, 2))
        
        # Rao–Scott χ² (if ≥2 columns)
        if (ncol(tab) >= 2) {
                chi <- try(svychisq(tab_fmla, design1, statistic="Chisq"), silent=TRUE)
                if (!inherits(chi, "try-error")) {
                        print(chi)
                        if (all(dim(tab)==c(2,2))) {
                                phi <- sqrt(as.numeric(chi$statistic) / sum(tab))
                                cat(sprintf(" φ (approx) = %.3f\n", phi))
                        }
                } else {
                        cat("→ Rao–Scott χ² failed; skipping.\n")
                }
        } else {
                cat("→ Only one column; skipping χ².\n")
        }
        
        # survey‐glm OR — *only* if exactly 2 levels (non‐NA) in predictor
        vals <- design1$variables[[v]]
        nlev <- length(unique(na.omit(vals)))
        if (nlev == 2) {
                fit <- try(
                        svyglm(
                                as.formula(paste0("CSEC ~ ", v)),
                                design    = design1,
                                family    = quasibinomial(),
                                na.action = na.omit
                        ), silent=TRUE
                )
                if (!inherits(fit, "try-error")) {
                        beta    <- coef(fit)[2]
                        ci_beta <- confint(fit)[2, ]
                        OR      <- exp(beta)
                        CI      <- exp(ci_beta)
                        cat(sprintf(
                                " Survey‐adjusted OR = %.2f; 95%% CI [%.2f, %.2f]\n",
                                OR, CI[1], CI[2]
                        ))
                } else {
                        cat("→ svyglm failed; skipping OR.\n")
                }
        } else {
                cat("→ Predictor", v, "has", nlev, "levels; skipping OR (only 2‐level preds).\n")
        }
}


# 2) continuous outcomes by CSEC with survey t‐test
cont_vars <- c("Age", "Foodandmatins", "Moneyworry",
               "Momclose", "Dadclose", "Monit",
               "Sexinit", "Sexpar", "MHsxs")

for (y in cont_vars) {
        cat("\n----", y, "by CSEC (filtered subset) ----\n")
        
        # first check that y is numeric in the design
        vec <- design1$variables[[y]]
        if (!is.numeric(vec)) {
                cat("→ Variable", y, "is not numeric (is", class(vec)[1], "); skipping t-test.\n")
                next
        }
        
        # run the survey t-test
        tt <- svyttest(as.formula(paste0(y, " ~ CSEC")),
                       design    = design1,
                       na.action = na.omit)
        print(tt)
        
        # extract difference in means, t, CI
        diff_mean <- as.numeric(tt$estimate)    # difference in means
        tval      <- as.numeric(tt$statistic)   # t statistic
        se        <- diff_mean / tval           # SE = estimate / t
        ci        <- tt$conf.int                 # 95% CI
        
        cat(sprintf(
                "Δ mean = %.3f (SE = %.3f); 95%% CI [%.3f, %.3f]\n",
                diff_mean, se, ci[1], ci[2]
        ))
}


# 3) second set of categorical predictors
vars2 <- c("Physviolence", "IPVphystot", "Physcorp", "Parentphy",
           "Sexabusebroad", "Sexabusenarrow", "Witanyvio",
           "WitparentIPV", "Witconflict", "IPVtotal",
           "Secact", "Firstwanted", "STI", "Drugs",
           "Selfharm", "SI")

for (v in vars2) {
        cat("\n---- CSEC ×", v, "(filtered subset) ----\n")
        
        tab_fmla <- as.formula(paste0("~ CSEC + ", v))
        tab      <- svytable(tab_fmla, design1)
        print(tab)
        cat("Row %:\n"); print(prop.table(tab, 1))
        cat("Col %:\n"); print(prop.table(tab, 2))
        
        if (ncol(tab) >= 2) {
                chi <- try(svychisq(tab_fmla, design1, statistic="Chisq"), silent=TRUE)
                if (!inherits(chi, "try-error")) {
                        print(chi)
                        if (all(dim(tab)==c(2,2))) {
                                phi <- sqrt(as.numeric(chi$statistic) / sum(tab))
                                cat(sprintf(" φ (approx) = %.3f\n", phi))
                        }
                } else {
                        cat("→ Rao–Scott χ² failed; skipping.\n")
                }
        } else {
                cat("→ Only one column; skipping χ².\n")
        }
        
        if (all(dim(tab)==c(2,2))) {
                fit <- try(
                        svyglm(
                                as.formula(paste0("CSEC ~ ", v)),
                                design    = design1,
                                family    = quasibinomial(),
                                na.action = na.omit
                        ), silent=TRUE
                )
                if (!inherits(fit, "try-error")) {
                        beta    <- coef(fit)[2]
                        ci_beta <- confint(fit)[2, ]
                        OR      <- exp(beta)
                        CI      <- exp(ci_beta)
                        cat(sprintf(
                                " Survey‐adjusted OR = %.2f; 95%% CI [%.2f, %.2f]\n",
                                OR, CI[1], CI[2]
                        ))
                } else {
                        cat("→ svyglm failed; skipping OR.\n")
                }
        } else {
                cat("→ Table is not 2×2; skipping OR.\n")
        }
}



#------------------------------------------------
## 3a) CSEC (binary → proportion, mean, SE, SD, min, max)
csec_m <- svymean(~ CSEC, design = d_filt, na.rm = TRUE)
csec_v <- svyvar (~ CSEC, design = d_filt, na.rm = TRUE)
csec_q <- svyquantile(~ CSEC, design = d_filt, quantiles = c(0,1), na.rm = TRUE, ci = FALSE)

# Inspect:
coef(csec_m)                # mean (proportion)
SE(csec_m)                  # SE
sqrt(coef(csec_v))          # SD
csec_q                       # min & max (should be 0 and 1)

## 3b) CSECtype (character → treat as factor → frequency + proportion)
# convert to factor on the fly
d_filt2 <- update(d_filt, CSECtype = factor(CSECtype))

tab_type <- svytable(~ CSECtype, design = d_filt2)
prop_type <- prop.table(tab_type)

# Inspect:
tab_type   # weighted counts per level
prop_type  # weighted proportions per level

## 3c) CSECtourist (binary → proportion etc.)
ct_m <- svymean(~ CSECtourist, design = d_filt, na.rm = TRUE)
ct_v <- svyvar (~ CSECtourist, design = d_filt, na.rm = TRUE)
ct_q <- svyquantile(~ CSECtourist, design = d_filt, quantiles = c(0,1), na.rm = TRUE, ci = FALSE)

coef(ct_m); SE(ct_m); sqrt(coef(ct_v)); ct_q

## 3d) CSECvicage (numeric → mean, SE, SD, min, max)
cv_m <- svymean(~ CSECvicage, design = d_filt, na.rm = TRUE)
cv_v <- svyvar (~ CSECvicage, design = d_filt, na.rm = TRUE)
cv_q <- svyquantile(~ CSECvicage, design = d_filt, quantiles = c(0,1), na.rm = TRUE, ci = FALSE)

coef(cv_m); SE(cv_m); sqrt(diag(cv_v)); cv_q

## 3e) CSECageperp (numeric → mean, SE, SD, min, max)
cp_m <- svymean(~ CSECageperp, design = d_filt, na.rm = TRUE)
cp_v <- svyvar (~ CSECageperp, design = d_filt, na.rm = TRUE)
cp_q <- svyquantile(~ CSECageperp, design = d_filt, quantiles = c(0,1), na.rm = TRUE, ci = FALSE)

coef(cp_m); SE(cp_m); sqrt(diag(cp_v)); cp_q

# — End of SPSS block with FILTER BY filter_$ —

# 4) “FILTER OFF” → go back to the full design still carried in `vacs_design`

## 4a) Repeat the same stats on the full sample:

# CSEC
csec_m_full <- svymean(~ CSEC, design = vacs_design, na.rm = TRUE)
csec_v_full <- svyvar (~ CSEC, design = vacs_design, na.rm = TRUE)
csec_q_full <- svyquantile(~ CSEC, design = vacs_design, quantiles = c(0,1), na.rm = TRUE, ci = FALSE)

coef(csec_m_full); SE(csec_m_full); sqrt(coef(csec_v_full)); csec_q_full

# CSECtype
vacs_designb <- update(vacs_design, CSECtype = factor(CSECtype))
tab_type_full <- svytable(~ CSECtype, design = vacs_designb)
prop_type_full <- prop.table(tab_type_full)
tab_type_full; prop_type_full

# CSECtourist
ct_m_full <- svymean(~ CSECtourist, design = vacs_design, na.rm = TRUE)
ct_v_full <- svyvar (~ CSECtourist, design = vacs_design, na.rm = TRUE)
ct_q_full <- svyquantile(~ CSECtourist, design = vacs_design, quantiles = c(0,1), na.rm = TRUE, ci = FALSE)

coef(ct_m_full); SE(ct_m_full); sqrt(coef(ct_v_full)); ct_q_full

# CSECvicage
cv_m_full <- svymean(~ CSECvicage, design = vacs_design, na.rm = TRUE)
cv_v_full <- svyvar (~ CSECvicage, design = vacs_design, na.rm = TRUE)
cv_q_full <- svyquantile(~ CSECvicage, design = vacs_design, quantiles = c(0,1), na.rm = TRUE, ci = FALSE)

coef(cv_m_full); SE(cv_m_full); sqrt(diag(cv_v_full)); cv_q_full

# CSECageperp
cp_m_full <- svymean(~ CSECageperp, design = vacs_design, na.rm = TRUE)
cp_v_full <- svyvar (~ CSECageperp, design = vacs_design, na.rm = TRUE)
cp_q_full <- svyquantile(~ CSECageperp, design = vacs_design, quantiles = c(0,1), na.rm = TRUE, ci = FALSE)

coef(cp_m_full); SE(cp_m_full); sqrt(diag(cp_v_full)); cp_q_full


summary_df <- tibble::tibble(
        Measure = c("CSEC (filtered)", 
                    "CSECtourist",
                    "CSECvicage",
                    "CSECageperp"),
        Mean    = c(coef(csec_m)[1],
                    coef(ct_m)[1],
                    coef(cv_m)[1],
                    coef(cp_m)[1]),
        SE      = c(SE(csec_m)[1],
                    SE(ct_m)[1],
                    SE(cv_m)[1],
                    SE(cp_m)[1]),
        SD      = c(sqrt(as.numeric(coef(csec_v))),
                    sqrt(as.numeric(coef(ct_v))),
                    sqrt(as.numeric(coef(cv_v))),
                    sqrt(as.numeric(coef(cp_v)))),
        Min     = c(csec_q$CSEC[1],
                    ct_q$CSECtourist[1],
                    cv_q$CSECvicage[1],
                    cp_q$CSECageperp[1]),
        Max     = c(csec_q$CSEC[2],
                    ct_q$CSECtourist[2],
                    cv_q$CSECvicage[2],
                    cp_q$CSECageperp[2])
) %>%
        mutate(
                Mean = sprintf("%.2f%%", 100 * Mean),
                SE   = sprintf("%.2f%%", 100 * SE)
        )

ft1 <- flextable(summary_df)
ft1 <- set_header_labels(ft1,
                         Measure = "Variable",
                         Mean    = "Mean / %", 
                         SE      = "SE / %",
                         SD      = "SD",
                         Min     = "Min",
                         Max     = "Max")
ft1 <- autofit(ft1)

# 2) Build the CSECtype frequency table
csec_type_df <- as.data.frame(tab_type) %>%
        rename(Count = Freq) %>%
        mutate(
                Proportion = prop_type[CSECtype],
                Proportion = sprintf("%.2f%%", 100 * Proportion)
        )

ft2 <- flextable(csec_type_df)
ft2 <- set_header_labels(ft2,
                         CSECtype   = "Perpetrator Type",
                         Count      = "Weighted Count",
                         Proportion = "Weighted %")
ft2 <- autofit(ft2)

# 3) Assemble the Word document
doc <- read_docx() %>%
        body_add_par("VACS CSEC Summary", style = "heading 1") %>%
        body_add_par("**Table 1.** Key summary statistics for selected CSEC variables (filtered sample)", style = "Normal") %>%
        body_add_flextable(ft1) %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("**Table 2.** Distribution of CSECtype (perpetrator category) among filtered cases", style = "Normal") %>%
        body_add_flextable(ft2) %>%
        body_add_par("", style = "Normal")

# 4) Save to disk
print(doc, target = "VACS_CSEC_Full_Summary.docx")


# 3. Simple tabulations (frequencies) ----------------------------------------

# SPSS FREQUENCIES VARIABLES=CSEC ... /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
svymean(~CSEC + CSECtype + CSECtourist + CSECvicage + CSECageperp, design = des, na.rm=TRUE)
svyvar (~CSEC + CSECtype + CSECtourist + CSECvicage + CSECageperp, design = des, na.rm=TRUE)
# To get min/max, you'll need to compute them manually:
svyby(~CSECvicage, ~1, svyquantile, design=des, quantiles=c(0,1), na.rm=TRUE)

# 4. Applying the filter “Full_filter == 1” -----------------------------------

d_sub <- subset(des, Full_filter == 1)

# then re-run the same frequencies on d_sub:
svymean(~CSEC + CSECtype + CSECtourist + CSECvicage + CSECageperp, design = d_sub, na.rm=TRUE)

# 5. Bivariate associations --------------------------------------------------

# 5a. Univariate descriptives for a long list of vars
vars <- c("Sex","Age","Eth","Sexorientfinal","School","Emp","Partner",
          "Telehom","Govsupp","NGOsupp","Foodandmatins","childouthome",
          "childstreet","Moneyworry","Momclose","Dadclose","Monit","Victimreg",
          "Safetyvio","Safetywar","Physviolence","IPVphystot","Physcorp",
          "Parentphy","Sexabusebroad","Sexabusenarrow","Witanyvio",
          "WitparentIPV","Witconflict","IPVtotal","Secact","Firstwanted",
          "Sexinit","Sexpar","STI","Drugs","Selfharm","SI","Suiattempt",
          "Alcohol","MHsxs")

svymean(as.formula(paste("~", paste(vars, collapse="+"))), design = d_sub, na.rm=TRUE)
# for higher moments:
svyvar (as.formula(paste("~", paste(vars, collapse="+"))), design = d_sub, na.rm=TRUE)

# 5b. Cross‐tabs with Chi‐square
# SPSS CROSSTABS /TABLES=CSEC BY Eth ...
svytable(~ CSEC + Eth, design = d_sub)              # counts
svychisq(~ CSEC + Eth, design = d_sub, statistic="Chisq")  # Chi-square test

# repeat for each pairing you need, e.g.
svychisq(~ CSEC + Sexorientfinal, design=d_sub)

# 5c. T‐tests for continuous vars by CSEC
# SPSS T-TEST GROUPS=CSEC(0 1) /VARIABLES=Age Foodandmatins ...
cont_vars <- c("Age","Foodandmatins","Moneyworry","Momclose","Dadclose",
               "Monit","Sexinit","Sexpar","MHsxs")

for(v in cont_vars) {
        cat("===", v, "===\n")
        print(svyttest(as.formula(paste(v, "~ CSEC")), design = d_sub))
}

# 5d. More cross‐tabs
other_cat_vars <- c("Physviolence","IPVphystot","Physcorp","Parentphy",
                    "Sexabusebroad","Sexabusenarrow","Witanyvio",
                    "WitparentIPV","Witconflict","IPVtotal","Secact",
                    "Firstwanted","STI","Drugs","Selfharm","SI")

for(v in other_cat_vars) {
        cat("=== CSEC x", v, "===\n")
        print(svychisq(as.formula(paste("~ CSEC +", v)), design = d_sub))
}





length(vacs_design$strata)
length(vacs_design2$strata)
strata1 <- unique(vacs_design$strata)
strata2 <- unique(vacs_design2$strata)

all.equal(strata1, strata2)

svymean(~CSEC, vacs_design, na.rm = TRUE)
svymean(~CSEC, vacs_design2, na.rm = TRUE)

svyby(~CSEC, ~Frame, vacs_design, svymean, na.rm = TRUE)
svyby(~CSEC, ~Frame, vacs_design2, svymean, na.rm = TRUE)

table(interaction(combinedVACS$Frame, combinedVACS$DEPT))
table(combinedVACS$Frame, combinedVACS$DEPT)


#Specify the subpopulation- Persons who were either between age 13 and 17, inclusive, or reported CSEC before age 17
sub_design <- subset(vacs_design, Full_filter == 1)
sub_design <- update(sub_design, CSEC_numeric = as.numeric(as.character(CSEC)))
svytable(~CSEC, sub_design)
svymean(~as.factor(CSEC), sub_design, na.rm = TRUE)
confint(svymean(~CSEC, sub_design, na.rm = TRUE))
svytable(~CSECtourist, sub_design)
svytable(~CSECvicage, sub_design)
svytable(~CSECageperp, sub_design)



# 3. Estimate weighted prevalence of CSEC and SECTT by gender (sex).
csec_by_gender <- svyby(~CSEC_flag, ~Sex, design=subdesign, svymean, na.rm=TRUE)
sectt_by_gender <- svyby(~SECTT_flag, ~Sex, design=subdesign, svymean, na.rm=TRUE)
print(csec_by_gender)   # Proportion of subpopulation with CSEC experience by gender (with SEs)
print(sectt_by_gender)  # Proportion of subpopulation with SECTT experience by gender (with SEs)
# The results give the weighted prevalence (as a proportion between 0 and 1) for males and females, along with standard errors. 
# You can multiply by 100 to express as percentage:
print(csec_by_gender * 100)

# 4. Chi-square tests (design-adjusted) for association between SECTT and various factors.
# Define a helper to perform design-based chi-square test:
survey_chisq <- function(var) {
        formula <- as.formula(paste("~", var, "+ SECTT_flag"))
        tbl <- svytable(formula, design=subdesign)
        chi <- svychisq(formula, design=subdesign, statistic="adjWald")
        list(Table=tbl, RaoScott_CHISQ=chi$statistic[[1]], df=chi$parameter, p.value=chi$p.value)
}

# Demographic/household variables vs SECTT:
survey_chisq("AgeGroup")        # Age group (13–17 vs 18–24)
survey_chisq("Q2A")            # Ethnicity categories
survey_chisq("School_now")     # Education (current attendance)
survey_chisq("Food_insec")     # Food insecurity
survey_chisq("Employed")       # Employment status
survey_chisq("Migrant")        # Migration status (respondent moved)
survey_chisq("Parent_migr")    # Family disruption by parent migration
survey_chisq("In_relationship")# Current relationship status
survey_chisq("Safe_comm")      # Perception of community safety

# Violence exposure variables vs SECTT:
survey_chisq("SexAbuse_contact")   # Contact sexual abuse
survey_chisq("SexAbuse_noncontact")# Non-contact sexual abuse
survey_chisq("IPV_any")           # Intimate partner violence
survey_chisq("Peer_violence")     # Peer violence
survey_chisq("Adult_physical")    # Physical violence by parent/other adult
survey_chisq("Corporal_pun")      # Corporal punishment
survey_chisq("Emotional_viol")    # Emotional violence

# Sexual risk behaviors & mental health vs SECTT:
survey_chisq("Early_sex")       # Early sexual debut (<15)
# For pregnancy, restrict to females in subdesign:
female_subdesign <- subset(subdesign, Sex == 2)
svychisq(~Ever_preg + SECTT_flag, design=female_subdesign, statistic="adjWald")
survey_chisq("Multi_partner")   # Multiple sex partners
survey_chisq("STI_any")         # Any STI
survey_chisq("High_distress")   # High psychological distress
survey_chisq("Self_harm")       # Self-harm
survey_chisq("Suicidal_idea")   # Suicidal ideation
survey_chisq("Suicide_attempt") # Suicide attempt