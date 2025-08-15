library(fs)
#knitr::opts_knit$set(root.dir = fs::path_dir(knitr::current_input()))

options(repos = c(CRAN = "https://cloud.r-project.org"))
knitr::opts_chunk$set(echo = TRUE)

packages <- c("survey", "haven", "dplyr", "labelled", "tidyr", "DT", "skimr", "ggplot2", "knitr", "kableExtra", "svyweight", "purrr", "srvyr", "tibble", "here", "naniar")

if(!require("pacman"))install.packages("pacman")

pacman::p_load(char = packages, character.only = TRUE)

VACSData <- read_sav(here::here("rawdata", "Colombia_VACS_national_data.sav"))

class(VACSData$Sex)
help(class)

test <- VACSData %>% 
        mutate(
                test = factor(Sex, levels = c(1,2), labels = c("Male", "Female")
                ))
        )

class(test$test)


df <- VACSData %>%
        select(-starts_with("Parenphys"),
               -starts_with("Nonparenphys")) %>%
        mutate(
                Sex = factor(Sex, levels = c(1,2), labels = c("Male", "Female")),
                SharedHouse = case_when(
                        H6 == 1                        ~1,
                        H5 %in% c(9, 98, 99) | H6 == 2 ~0,
                        H6 %in% c(98,99) | is.na(H6)   ~NA_real_),
                InternetHome = recode_bin(H7H),
                CellHome = recode_bin(H7E),
                FinanceSupport = case_when(
                        H18 == 1 | H19 == 1  ~1,
                        H18 == 2 & H19 == 2  ~0,
                        H18 %in% c(98,99) | is.na(H18) & H19 != 1 ~NA_real_,
                        H19 %in% c(98,99) | is.na(H19) & H18 != 1 ~NA_real_),
                VictimReg = recode_bin(H19A),
                MoneyWorry = reverse_5(H20),
                ParentAbs = case_when(
                        H54 %in% c(1,2,3,4)              ~1,
                        is.na(H54) & Q2 > 17 | H54 == 5  ~0,
                        H54 %in% c(98,99) | is.na(H54)   ~NA_real_
                ),
                ChildOutHome = case_when(
                        H56 == 1                        ~1,
                        H56 == 2 | is.na(H56) & Q2 > 17 ~0,
                        H56 %in% c(98,99) | is.na(H56)  ~NA_real_
                ),
                ChildStreet = case_when(
                        H58 == 1                        ~1,
                        H58 == 2 | is.na(H58) & Q2 > 17 ~0,
                        H58 %in% c(98,99) | is.na(H58)  ~NA_real_
                ),
                Age = Q2,
                Ethnicity = factor(
                        case_when(
                                Q2A %in% c(98, 99) | is.na(Q2A) ~ NA_real_,
                                TRUE ~ as.numeric(Q2A)
                        ),
                        levels = 1:7,
                        labels = ethLabels
                ),
                School = recode_bin(Q3),
                CloseFriend = reverse_4(Q7),
                Employed = recode_bin(Q13),
                ParMonitFreeTime = case_when(
                        Q36A == 97  ~3
                )
                
        ) %>% 
        select(
                VACS_ID,
                Sex,
                USM,
                HH,
                MUNI,
                PSU,
                DEPT,
                Wght_final,
                SharedHouse,
                InternetHome,
                CellHome,
                FinanceSupport,
                VictimReg,
                MoneyWorry,
                ParentAbs,
                ChildOutHome,
                ChildStreet,
                Age,
                Ethnicity,
                School,
                CloseFriend,
                Employed,
                
                
                
        )

testVar <- function(x, df){
        print(class(df[[x]]))
        table(df[[x]], useNA="always")
}
