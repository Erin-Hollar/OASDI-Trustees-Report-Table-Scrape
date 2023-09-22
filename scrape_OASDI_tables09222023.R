  #-----
  # project: Generate Table for OASDI Trustees Report Scraping
  # author: EBH
  # date: 09/21/2023
  #-----
  
  #install packages
  library(dplyr)
  library(rvest)
  library(stringr)
  library(janitor)
  
  
  # Generate Main Table
  # Main table will contain all identifying information for each table
  # Includes Columns: Year, Section, Subsection, HTML Bit, Table Number
  #Year: 2014-2022
  #For each year, sections II through VI
  #For each section, subsections A through H
  #For each subsection, table numbers 1 through 10
  
  Year_list <- 2014:2022
  section_list <- c("II","III","IV","V","VI")
  subsection_list <- c("A","B","C","D","E","F","G","H")
  tablenum <- 1:10
  
  
  # Create empty data frame to store the main table
  main_table <- data.frame()
  
  # Generate row for each year, section, subsection, and table number and add to the main table
  for (year in Year_list) {
    for (section in section_list) {
      for (subsection in subsection_list) {
        section_data <- data.frame(
          Year = year,
          Section = section, 
          Subsection = subsection,
          Table_Number = 1:10
        )
         main_table <- bind_rows(main_table, section_data)
      }
    }
  }
  
  # Add html bit columns filtered for the section and subsection and manually assign the unique end of the html for each table
  # html_bit refers to the end of the url 
  #II.B
  main_table <- main_table %>%
    filter(Section == "II" & Subsection == "B") %>% 
    mutate(html_bit = "cyoper.html#96807") %>%
    bind_rows(main_table %>% filter(!(Section == "II" & Subsection == "B")))
  #II.C
  main_table <- main_table %>%
    filter(Section == "II" & Subsection == "C") %>%
    mutate(html_bit = "assump.html#101021") %>%
    bind_rows(main_table %>% filter(!(Section == "II" & Subsection == "C")))
  #II.D
  main_table <- main_table %>%
    filter(Section == "II" & Subsection == "D") %>%
    mutate(html_bit = "project.html#125512") %>%
    bind_rows(main_table %>% filter(!(Section == "II" & Subsection == "D")))
  #III.A
  main_table <- main_table %>%
    filter(Section == "III" & Subsection == "A") %>%
    mutate(html_bit = "cyoper.html#193204") %>%
    bind_rows(main_table %>% filter(!(Section == "III" & Subsection == "A")))
  #IV.A
  main_table <- main_table %>%
    filter(Section == "IV" & Subsection == "A") %>%
    mutate(html_bit = "SRest.html#506116") %>%
    bind_rows(main_table %>% filter(!(Section == "IV" & Subsection == "A")))
  #IV.B
  main_table <- main_table %>%
    filter(Section == "IV" & Subsection == "B") %>%
    mutate(html_bit = "LRest.html#462733") %>%
    bind_rows(main_table %>% filter(!(Section == "IV" & Subsection == "B")))
  #V.A
  main_table <- main_table %>%
    filter(Section == "V" & Subsection == "A") %>%
    mutate(html_bit = "demo.html#308505") %>%
    bind_rows(main_table %>% filter(!(Section == "V" & Subsection == "A")))
  #V.B
  main_table <- main_table %>%
    filter(Section == "V" & Subsection == "B") %>%
    mutate(html_bit = "econ.html#292722") %>%
    bind_rows(main_table %>% filter(!(Section == "V" & Subsection == "B")))
  #V.C
  main_table <- main_table %>%
    filter(Section == "V" & Subsection == "C") %>%
    mutate(html_bit = "prog.html#1047210") %>%
    bind_rows(main_table %>% filter(!(Section == "V" & Subsection == "C")))
  #VI.A
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "A") %>%
    mutate(html_bit = "cyoper_hist.html#2829247") %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "A")))
  #VI.B
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "B") %>%
    mutate(html_bit = "LRact_bal.html#103557") %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "B")))
  #VI.C
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "C") %>%
    mutate(html_bit = "SRfyproj.html#317167") %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "C")))
  #VI.D
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "D") %>%
    mutate(html_bit = "LRsens.html#111775") %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "D")))
  #VI.E
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "E") %>%
    mutate(html_bit = "stoch.html#130300") %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "E")))
  #VI.F
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "F") %>%
    mutate(html_bit = "infinite.html#1000194") %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "F")))
  #VI.G1
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "G" & (Table_Number == 1 | Table_Number == 2| Table_Number == 3)) %>%
    mutate(html_bit = "payroll.html#92759") %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "G" & (Table_Number == 1 | Table_Number == 2| Table_Number == 3))))
  #VI.G2
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "G" & (Table_Number == 4 | Table_Number == 5)) %>%
    mutate(html_bit = "GDP.html#200732") %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "G" & (Table_Number == 4 | Table_Number == 5))))
              
  #VI.G3
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "G" & (Table_Number == 6 | Table_Number == 7| Table_Number == 8| Table_Number == 9| Table_Number == 10)) %>%
    mutate(html_bit = "dollars.html#1829137") %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "G" & (Table_Number == 6 | Table_Number == 7| Table_Number == 8| Table_Number == 9| Table_Number == 10))))
  
  #VI.H
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "H") %>%
    mutate(html_bit = "OASIforDI.html#124088") %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "H")))
  
  
  #Add title column; this will be referenced when scraping the title. It refers to the index of the graph on its corresponding link
  #i.e. If it is the 1st graph on its webpage -> Title_number = [1]
  main_table <- main_table %>%
    filter(!(Section == "VI" & Subsection == "G")) %>%
    mutate(Title_Number = Table_Number) %>%
    bind_rows(main_table %>% filter(Section == "VI" & Subsection == "G"))
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "G" & (Table_Number == 1 | Table_Number == 2| Table_Number == 3)) %>%
    mutate(Title_Number = Table_Number) %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "G" & (Table_Number == 1 | Table_Number == 2| Table_Number == 3))))
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "G" & (Table_Number == 4 | Table_Number == 5 )) %>%
    mutate(Title_Number = Table_Number - 3) %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "G" & (Table_Number == 4 | Table_Number == 5 ))))
  main_table <- main_table %>%
    filter(Section == "VI" & Subsection == "G" & (Table_Number == 6 | Table_Number == 7 | Table_Number == 8 | Table_Number == 9 | Table_Number == 10)) %>%
    mutate(Title_Number = Table_Number - 5) %>%
    bind_rows(main_table %>% filter(!(Section == "VI" & Subsection == "G" & (Table_Number == 6 | Table_Number == 7 | Table_Number == 8 | Table_Number == 9 | Table_Number == 10))))

  
  #Add full html bit column, referencing the other columns to combine the year, section, subsection, and html_bit
  #Filter of VI.G b/c it has different links
  
  html_added1 <- main_table%>%  
    filter(!(Section == "VI" & Subsection == "G")) %>% #generate full html for all tables except VI.G's; VI.G's contain an additional number_"OASDHI_" in the html
    mutate(html = paste0("https://www.ssa.gov/OACT/TR/", Year ,"/", Section ,"_", Subsection ,"_", html_bit))
    
  #Now add full links for VI.G
  html_added2 <- main_table%>% 
    filter(Section == "VI" & Subsection == "G" & (Table_Number == 1 | Table_Number == 2 | Table_Number == 3)) %>%
    mutate(html = paste0("https://www.ssa.gov/OACT/TR/", Year ,"/", Section ,"_", Subsection, "1_OASDHI_", html_bit))
  html_added3 <- main_table%>% 
    filter(Section == "VI" & Subsection == "G" & (Table_Number == 4 | Table_Number == 5)) %>%
    mutate(html = paste0("https://www.ssa.gov/OACT/TR/", Year ,"/", Section ,"_", Subsection, "2_OASDHI_", html_bit))
  html_added4 <- main_table%>% 
    filter(Section == "VI" & Subsection == "G" & (Table_Number == 6 | Table_Number == 7 | Table_Number == 8| Table_Number == 9| Table_Number == 10)) %>%
    mutate(html = paste0("https://www.ssa.gov/OACT/TR/", Year ,"/", Section ,"_", Subsection, "3_OASDHI_", html_bit))
  
  
  #Combine and add to main table
  main_table <- bind_rows(html_added1,html_added2,html_added3,html_added4)
  
  
  #----------------------------------------------------------------------------------------------------------------------
  #Begin assigning table names
  
  #Create function that takes the existing information in each row, adds a row with the table name, and adds that section make into the larger data frame
  add_table_name <- function(data, section, subsection, table_range, year_range, table_name1) {
    filtered_data <- data %>%
      
      filter(Section == section & Subsection == subsection & Table_Number %in% table_range & Year %in% year_range) %>%
      mutate(table_name = table_name1)
    
    processed_table <- bind_rows(filtered_data, data %>% filter(!(Section == section & Subsection == subsection & Table_Number %in% table_range & Year %in% year_range)))
    # return (processed_table)
  }
  
  # Manually assign each table name to the corresponding row in the table
  # Skip to 435 to find when the tables are generated
  
  # II
  # II.B1
  main_table <- add_table_name(main_table, "II", "B", 1, 2014:2022, "OperationsSummary")
  # II.B2
  main_table <- add_table_name(main_table, "II", "B", 2, 2014:2022, "TaxRates")
  # II.C1
  main_table <- add_table_name(main_table, "II", "C", 1, 2014:2022, "UltimateKeyAssumps")
  # II.D1, 2019:2022
  main_table <- add_table_name(main_table, "II", "D", 1, 2019:2022, "MaxTFRatios_ExhausDates")
  # II.D1, 2014:2018
  main_table <- add_table_name(main_table, "II", "D", 1, 2014:2018, "MaxTFRatios_0026ExhausDates")
  # II.D2
  main_table <- add_table_name(main_table, "II", "D", 2, 2014:2022, "LR_Change_in_Actuarial_Balance")
  
  # III
  # III.A1, 2019-2022
  main_table <- add_table_name(main_table, "III", "A", 1, 2019:2022, "Operations_OASI")
  # III.A2, 2019-2022
  main_table <- add_table_name(main_table, "III", "A", 2, 2019:2022, "Operations_DI")
  # III.A3, 2019-2022
  main_table <- add_table_name(main_table, "III", "A", 3, 2019:2022, "Operations_OASDI")
  # III.A1-3, 2014-2018
  main_table <- add_table_name(main_table, "III", "A", 1:3, 2014:2018, "Operations")
  # III.A4, 2019:2022
  main_table <- add_table_name(main_table, "III", "A", 4, 2019:2022, "Comp_Actual_Est")
  # III.A4, 2014:2018
  main_table <- add_table_name(main_table, "III", "A", 4, 2014:2018, "Comp_002fActual_002fEst")
  # III.A5
  main_table <- add_table_name(main_table, "III", "A", 5, 2014:2022, "DistBenPaymts")
  # III.A6, 2019:2022
  main_table <- add_table_name(main_table, "III", "A", 6, 2019:2022, "AdminExpAsPercentage")
  # III.A6, 2014:2018
  main_table <- add_table_name(main_table, "III", "A", 6, 2014:2018, "Adm_Exp__0025_Contr_Inc_002fBen_Paymts")
  # III.A7
  main_table <- add_table_name(main_table, "III", "A", 7, 2014:2022, "Investment_Transactions")
  
  
  #IV
  # IV.A1, 2019-2022
  main_table <- add_table_name(main_table, "IV", "A", 1, 2019:2022, "OperationsCY_OASI")
  # IV.A2, 2019-2022
  main_table <- add_table_name(main_table, "IV", "A", 2, 2019:2022, "OperationsCY_DI")
  # IV.A3, 2019-2022
  main_table <- add_table_name(main_table, "IV", "A", 3, 2019:2022, "OperationsCY_OASDI")
  # IV.A1-3, 2014-2018
  main_table <- add_table_name(main_table, "IV", "A", 1:3, 2014:2018, "Operations_002cCY")
  # IV.A4, 2019-2022
  main_table <- add_table_name(main_table, "IV", "A", 4, 2019:2022, "ReasonsForChange")
  # IV.A4, 2014-2018
  main_table <- add_table_name(main_table, "IV", "A", 4, 2014:2018, "SR_Change_in_Cost")
  # IV.B1
  main_table <- add_table_name(main_table, "IV", "B", 1, 2014:2022, "Annual_Income_and_Cost_Rates")
  # IV.B2, 2019-2022
  main_table <- add_table_name(main_table, "IV", "B", 2, 2019:2022, "ComponentsAnnIncomeRatesCY")
  # IV.B2, 2015-2018
  main_table <- add_table_name(main_table, "IV", "B", 2, 2015:2018, "OASDHI_TF_in_Dollars")
  # IV.B2, 2014
  main_table <- add_table_name(main_table, "IV", "B", 2, 2014, "Comparison_of_Workers_and_Benies")
  # IV.B3, 2019-2022
  main_table <- add_table_name(main_table, "IV", "B", 3, 2019:2022, "CoveredWorkersAndBenies")
  # IV.B3, 2014-2018
  main_table <- add_table_name(main_table, "IV", "B", 3, 2014:2018, "Comparison_of_Workers_and_Benies")
  # IV.B4
  main_table <- add_table_name(main_table, "IV", "B", 4, 2014:2022, "LR_Trust_Fund_Ratios")
  # IV.B5, 2019-2022
  main_table <- add_table_name(main_table, "IV", "B", 5, 2019:2022, "ComponentsSummIncAndCostRates")
  # IV.B5, 2014-2018
  main_table <- add_table_name(main_table, "IV", "B", 5, 2014:2018, "ComponentSummInc_0026CostRate")
  # IV.B6, 2019-2022
  main_table <- add_table_name(main_table, "IV", "B", 6, 2019:2022, "Components75YrActBal")
  # IV.B6, 2014-2018
  main_table <- add_table_name(main_table, "IV", "B", 6, 2014:2018, "Open-Group_Unfunded_Obligations")
  # IV.B7, 2019-2022
  main_table <- add_table_name(main_table, "IV", "B", 7, 2019:2022, "ReasonsForChange75YrActBal")
  # IV.B7, 2014-2018
  main_table <- add_table_name(main_table, "IV", "B", 7, 2014:2018, "LR_Change_in_Actuarial_Balance")
  
  #V
  # V.A1, 2019-2022
  main_table <- add_table_name(main_table, "V", "A", 1, 2019:2022, "FertilityMortalityAssumptions")
  # V.A2, 2019-2022
  main_table <- add_table_name(main_table, "V", "A", 2, 2019:2022, "ImmigrationAssumptions")
  # V.A1-2, 2014-2018
  main_table <- add_table_name(main_table, "V", "A", 1:2, 2014:2018, "Principal_Demographic_Assumptions")
  # V.A3
  main_table <- add_table_name(main_table, "V", "A", 3, 2014:2022, "Population")
  # V.A4
  main_table <- add_table_name(main_table, "V", "A", 4, 2014:2022, "PeriodLifeExpenctancies") #Crazy typo on SSAs behalf!
  # V.A5
  main_table <- add_table_name(main_table, "V", "A", 5, 2014:2022, "CohortLifeExpectancies")
  # V.B1
  main_table <- add_table_name(main_table, "V", "B", 1, 2014:2022, "Econ_Assump")
  # V.B2
  main_table <- add_table_name(main_table, "V", "B", 2, 2014:2022, "AddEconFactNew")
  # V.C1, 2019-2022
  main_table <- add_table_name(main_table, "V", "C", 1, 2019:2022, "COLA_AWI_CBB_RET")
  # V.C1, 2014-2018
  main_table <- add_table_name(main_table, "V", "C", 1, 2014:2018, "COLA_002fAWI_002fC_0026BBases_002fRET")
  # V.C2
  main_table <- add_table_name(main_table, "V", "C", 2, 2014:2022, "Bend_Points_and_QCs")
  # V.C3, 2019-2022
  main_table <- add_table_name(main_table, "V", "C", 3, 2019:2022, "LegislatedChanges_NRA_DRC")
  # V.C3, 2014-2018
  main_table <- add_table_name(main_table, "V", "C", 3, 2014:2018, "LegChg_002fNRA_0026DRC")
  # V.C4
  main_table <- add_table_name(main_table, "V", "C", 4, 2014:2022, "OASI_Beneficiaries")
  # V.C5
  main_table <- add_table_name(main_table, "V", "C", 5, 2014:2022, "DI_Beneficiaries")
  # V.C6, 2019-2022
  main_table <- add_table_name(main_table, "V", "C", 6, 2019:2022, "ContributionBenefitBase_and_PayrollTaxRates")
  # V.C6, 2014-2018
  main_table <- add_table_name(main_table, "V", "C", 6, 2014:2018, "Wage_Base_and_Tax_Rates")
  # V.C7, 2019-2022
  main_table <- add_table_name(main_table, "V", "C", 7, 2019:2022, "AnnScheduledBenefitAmts")
  # V.C7, 2014-2018
  main_table <- add_table_name(main_table, "V", "C", 7, 2014:2018, "BenefitAmounts-c")
  
  #VI
  # VI.A1 2019:2022
  main_table <- add_table_name(main_table, "VI", "A", 1, 2019:2022, "HistoricalOperations_OASI")
  # VI.A2 2019:2022
  main_table <- add_table_name(main_table, "VI", "A", 2, 2019:2022, "HistoricalOperations_DI")
  # VI.A3 2019:2022
  main_table <- add_table_name(main_table, "VI", "A", 3, 2019:2022, "HistoricalOperations_OASDI")
  # VI.A1-3 2014:2018
  main_table <- add_table_name(main_table, "VI", "A", 1:3, 2014:2018, "HistoricalOperations")
  # VI.A4 2019:2022
  main_table <- add_table_name(main_table, "VI", "A", 4, 2019:2022, "OASITrustFundAssets")
  # VI.A5 2019:2022
  main_table <- add_table_name(main_table, "VI", "A", 5, 2019:2022, "DITrustFundAssets")
  # VI.A4-5 2014:2018
  main_table <- add_table_name(main_table, "VI", "A", 4:5, 2014:2018, "Assets")
  # VI.B1
  main_table <- add_table_name(main_table, "VI", "B", 1, 2014:2022, "HistoricalActBals")
  # VI.C1 2019:2022
  main_table <- add_table_name(main_table, "VI", "C", 1, 2019:2022, "OperationsOASI")
  # VI.C2 2019:2022
  main_table <- add_table_name(main_table, "VI", "C", 2, 2019:2022, "OperationsDI")
  # VI.C3 2019:2022
  main_table <- add_table_name(main_table, "VI", "C", 3, 2019:2022, "OperationsOASDI")
  # VI.C1-3 2014:2018
  main_table <- add_table_name(main_table, "VI", "C", 1:3, 2014:2018, "Operations")
  # VI.C4 2019:2022
  main_table <- add_table_name(main_table, "VI", "C", 4, 2019:2022, "HistAndSR_OperationsOASI")
  # VI.C5 2019:2022
  main_table <- add_table_name(main_table, "VI", "C", 5, 2019:2022, "HistAndSR_OperationsDI")
  # VI.C6 2019:2022
  main_table <- add_table_name(main_table, "VI", "C", 6, 2019:2022, "HistAndSR_OperationsOASDI")
  # VI.C4-6 2014:2018
  main_table <- add_table_name(main_table, "VI", "C", 4:6, 2014:2018, "FY_002cHist_002cOper")
  
  # VI.D1 2019:2022
  main_table <- add_table_name(main_table, "VI", "D", 1, 2019:2022, "SensitivityFertility")
  # VI.D2 2019:2022
  main_table <- add_table_name(main_table, "VI", "D", 2, 2019:2022, "SensitivityDeathRate")
  # VI.D3 2019:2022
  main_table <- add_table_name(main_table, "VI", "D", 3, 2019:2022, "SensitivityImmigration")
  # VI.D4 2019:2022
  main_table <- add_table_name(main_table, "VI", "D", 4, 2019:2022, "SensitivityWage")
  # VI.D5 2019:2022
  main_table <- add_table_name(main_table, "VI", "D", 5, 2019:2022, "SensitivityCPI")
  # VI.D6 2019:2022
  main_table <- add_table_name(main_table, "VI", "D", 6, 2019:2022, "SensitivityInterest")
  # VI.D7 2019:2022
  main_table <- add_table_name(main_table, "VI", "D", 7, 2019:2022, "SensitivityTaxableRatio")
  # VI.D8 2019:2022
  main_table <- add_table_name(main_table, "VI", "D", 8, 2019:2022, "SensitivityDIIncidence")
  # VI.D9 2019:2022
  main_table <- add_table_name(main_table, "VI", "D", 9, 2019:2022, "SensitivityDITermination")
  # VI.D1-9 2014:2018
  main_table <- add_table_name(main_table, "VI", "D", 1:9, 2014:2018, "Sensitivity")
  # VI.E1 2014:2022
  main_table <- add_table_name(main_table, "VI", "E", 1, 2014:2022, "Stochastic")
  # VI.F1 2019:2022
  main_table <- add_table_name(main_table, "VI", "F", 1, 2019:2022, "UnfundedObligations")
  # VI.F1 2014:2018
  main_table <- add_table_name(main_table, "VI", "F", 1, 2014:2018, "Unfunded_Obligations")
  # VI.F2 2019:2022
  main_table <- add_table_name(main_table, "VI", "F", 2, 2019:2022, "PresentValues")
  # VI.F2 2014:2018
  main_table <- add_table_name(main_table, "VI", "F", 2, 2014:2018, "Present_Value_of_Benefits")
  
  # VI.G1 2019:2022
  main_table <- add_table_name(main_table, "VI", "G", 1, 2019:2022, "PayrollTaxRates")
  # VI.G1 2014:2018
  main_table <- add_table_name(main_table, "VI", "G", 1, 2014:2018, "TaxRates_0025Payroll")
  # VI.G2 2019:2022
  main_table <- add_table_name(main_table, "VI", "G", 2, 2019:2022, "OASDI_and_HI_Rates")
  # VI.G2 2014:2018
  main_table <- add_table_name(main_table, "VI", "G", 2, 2014:2018, "AnnualOASDHIRates_0025Payroll")
  # VI.G3 2014:2022
  main_table <- add_table_name(main_table, "VI", "G", 3, 2014:2022, "Val_Period_OASDHI_Rates")
  # VI.G4 2019:2022
  main_table <- add_table_name(main_table, "VI", "G", 4, 2019:2022, "OASDHI_IncomeCostBal")
  # VI.G4 2014:2018
  main_table <- add_table_name(main_table, "VI", "G", 4, 2014:2018, "Inc__0026_Outgo_as__0025_of_GDP")
  # VI.G5 2019:2022
  main_table <- add_table_name(main_table, "VI", "G", 5, 2019:2022, "RatioTaxablePayrollToGDP")
  # VI.G5 2014:2018
  main_table <- add_table_name(main_table, "VI", "G", 5, 2014:2018, "Payroll_as__0025_of_GDP")
  # VI.G6 2014:2022
  main_table <- add_table_name(main_table, "VI", "G", 6, 2014:2022, "Economic_Variables")
  # VI.G7 2014:2022
  main_table <- add_table_name(main_table, "VI", "G", 7, 2014:2022, "TF_in_Constant_Dollars")
  # VI.G8 2014:2022
  main_table <- add_table_name(main_table, "VI", "G", 8, 2014:2022, "TF_in_Current_Dollars")
  # VI.G9 2015:2022
  main_table <- add_table_name(main_table, "VI", "G", 9, 2015:2022, "OASDHI_TF_in_CPI-Indexed_Dollars")
  # VI.G9 2014
  main_table <- add_table_name(main_table, "VI", "G", 9, 2014, "OASDHI_TF_in_Dollars") 
  #2014 only has 9 tables; The table that is #10 in 2015-2022 is #9 in 2014
  # VI.G10, 2014
  main_table <- add_table_name(main_table, "VI", "G", 10, 2014, "OASDHI_TF_in_Dollars")
  # VI.G10, 2015:2022
  main_table <- add_table_name(main_table, "VI", "G", 10, 2015:2022, "OASDHI_TF_in_Current_Dollars")
  # VI.H1 2019:2022
  main_table <- add_table_name(main_table, "VI", "H", 1, 2019:2022, "OASI_DI_Disbursements")
  # VI.H2 2019:2022
  main_table <- add_table_name(main_table, "VI", "H", 2, 2019:2022, "OASDI_DI_Disbursements")
  #VI.H1-2 2014:2018
  main_table <- add_table_name(main_table, "VI", "H", 1:2, 2014:2018, "OASDI_Disburs")
  
  
  main_table_final <- main_table %>% filter (!is.na(table_name))#Remove observations that do not have a table name; these do not exist of OASDI page
  main_table_final <- main_table_final[nrow(main_table_final):1, ] #reverse order of observations, for sorting preferences
  
#---------------------------------------------------------------------------------------------------------------------------------------
  
  #create empty nested list structure
  rawdata <- list()
  for (i in 1:nrow(main_table_final)) {
    section <- as.character(main_table_final$Section[i])
    subsection <- as.character(main_table_final$Subsection[i])
    number <- as.character(main_table_final$Table_Number[i])
    year <- as.character(main_table_final$Year[i])
    html <- main_table_final$html[i]
    table_name <- str_c("table." , main_table_final$table_name[i])
    title_num <- main_table_final$Title_Number[i]
    
   # Check if the 'section' exists in the rawdata list & create if not
   if (!(section %in% names(rawdata))) {
    rawdata[[section]] <- list()
   }
   # Check if the 'subsection' exists in the rawdata list & create if not
   if (!(subsection %in% names(rawdata[[section]]))) {
    rawdata[[section]][[subsection]] <- list()
   }
   # Check if the 'table number' exists in the rawdata list & create if not
   if (!(number %in% names(rawdata[[section]][[subsection]]))) {
    rawdata[[section]][[subsection]][[number]] <- list()
   }
    # Check if the 'year' exists in the rawdata list & create if not
    if (!(year %in% names(rawdata[[section]][[subsection]][[number]]))) {
    rawdata[[section]][[subsection]][[number]][[year]] <- list()
   }

  # fil in nested list structure by pulling html tables
  link_list <- xml2:: read_html(html) 
  body <- link_list %>%
    html_nodes(table_name)%>%
    html_table %>% .[[1]]
  title <- link_list %>% html_nodes("div.TableTitle") %>%  #pull table title and assign it to title object
     html_text() %>%
     .[title_num]

  rawdata[[section]][[subsection]][[number]][[year]][[title]] <- body # insert title as the name of the inner-most nested list object & assign the body/table info to that object
  
  }
  
  


