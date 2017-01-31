featureEngineer <- function(RawData)
{
    BsmtFullBathPresent <- as.factor(RawData$BsmtFullBath)
    BsmtFullBathPresent <- recode_factor(BsmtFullBathPresent,
                                         '0' = "NotPresent", '1' = "Present", 
                                         '2' = "Present", '3' = "Present")
    RawData$BsmtFullBath <- NULL
    RawData <- data.frame(RawData, BsmtFullBathPresent)
    
    
    RawData$FullBath <- recode_factor(as.factor(RawData$FullBath), '0' = "OneORLess",
                                      '1' = "OneORLess", .default = "TwoOrMore")
    
    
    HalfBathPresent <- as.factor(RawData$HalfBath)
    HalfBathPresent <- recode_factor(HalfBathPresent, '0' = "NotPresent", 
                                     '1' = "Present", '2' = "Present")
    RawData$HalfBath <- NULL
    RawData <- data.frame(RawData, HalfBathPresent)
    rm(HalfBathPresent)
    
    
    RawData$BedroomAbvGr <- recode_factor(as.factor(RawData$BedroomAbvGr), 
                                          '0' = "OneOrLess", '1' = "OneOrLess", 
                                          '2' = "Two", '3' = "Three", '4' = "FourOrMore", 
                                          '5' = "FourOrMore", '6' = "FourOrMore", 
                                          '8' = "FourOrMore")
    
    
    RawData$TotRmsAbvGrd <- recode_factor(as.factor(RawData$TotRmsAbvGrd), 
                                          '2' = "FourOrLess", '3' = "FourOrLess",
                                          '4' = "FourOrLess", '5' = "Five", 
                                          '6' = "Six", '7' = "Seven", '8' = "Eight", 
                                          .default = "NineOrMore")
    
    
    RawData$Fireplaces <- recode_factor(as.factor(RawData$Fireplaces), '0' = "Zero",
                                        '1' = "One", .default = "TwoOrMore")
    
    
    RawData$GarageCars <- recode_factor(as.factor(RawData$GarageCars), '0' = "None",
                                        '1' = "One", '2' = "Two",
                                        .default = "ThreeOrMore")
    
    
    RawData$HouseStyle <- recode_factor(RawData$HouseStyle, "1.5Unf" = "1To1.5Unf",
                                        "1Story" = "1To1.5Unf", "1.5Fin" = "1.5Fin",
                                        "2Story" = "2To2.5", "2.5Fin" = "2To2.5",
                                        "2.5Unf" = "2To2.5", "SFoyer" = "Others",
                                        "SLvl" = "Others")
    
    
    RawData$LotShape <- recode_factor(RawData$LotShape, "Reg" = "Reg", "IR1" = "IR",
                                      "IR2" = "IR", "IR3" = "IR")
    
    
    RawData$LotConfig <- recode_factor(RawData$LotConfig, "Inside" = "Inside",
                                       "Corner" = "Corner", CulDSac = "CulDSac",
                                       "FR2" = "Others", "FR3" = "Others")
    
    RawData$OverallQual <- recode_factor(RawData$OverallQual, 
                                         "1" = "4OrLess", "2" = "4OrLess",
                                         "3" = "4OrLess", "4" = "4OrLess", "5" = "5",
                                         "6" = "6", "7" = "7", "8" = "8OrMore", 
                                         "9" = "8OrMore")
    
    RawData$OverallCond <- recode_factor(RawData$OverallCond, "1" = "4OrLess",
                                         "2" = "4OrLess", "3" = "4OrLess", "4" = "4OrLess",
                                         "5" = "5", "6" = "6", "7" = "7", "8" = "8OrMore",
                                         "9" = "8OrMore")
    
    
    RawData$Exterior1st <- recode_factor(RawData$Exterior1st, "HdBoard" = "HdBoard",
                                         "MetalSd" = "MetalSd", "Plywood" = "Plywood",
                                         "VinylSd" = "VinylSd", "Wd Sdng" = "Wd_Sdng",
                                         .default = "Others")
    
    
    RawData$Exterior2nd <- recode_factor(RawData$Exterior2nd, "HdBoard" = "HdBoard",
                                         "MetalSd" = "MetalSd", "Plywood" = "Plywood",
                                         "VinylSd" = "VinylSd", "Wd Sdng" = "Wd_Sdng",
                                         .default = "Others")
    
    
    RawData$Foundation <- recode_factor(RawData$Foundation, "CBlock" = "CBlock", 
                                        "PConc" = "PConc", .default = "Others")
    
    
    RawData$BsmtQual[is.na(RawData$BsmtQual)] = "Fa"
    RawData$BsmtQual <- recode_factor(RawData$BsmtQual, "Ex" = "Ex", "Gd" = "Gd",
                                      "TA" = "TA", "Fa" = "FaOrNA")
    
    
    RawData$HeatingQC <- recode_factor(RawData$HeatingQC, "Ex" = "Ex", "Gd" = "Gd",
                                       "TA" = "TA_Fa_Po", "Fa" = "TA_Fa_Po", 
                                       "Po" = "TA_Fa_Po")
    
    RawData$KitchenQual <- recode_factor(RawData$KitchenQual, "Ex" = "Ex",
                                         "Gd" = "Gd", "TA" = "TA_Fa", "Fa" = "TA_Fa")
    
    
    RawData$FireplaceQu <- recode_factor(RawData$FireplaceQu, "Ex" = "Ex_Gd", 
                                         "Gd" = "Ex_Gd", "Fa" = "TA_Fa_Po",
                                         "TA" = "TA_Fa_Po", "Po" = "TA_Fa_Po")
    
    
    RawData$GarageType <- recode_factor(RawData$GarageType, "Attchd" = "Attchd",
                                        "Detchd" = "Detchd", .default = "Others")
    
    
    RawData$SaleCondition <- recode_factor(RawData$SaleCondition, "Normal" = "Normal",
                                           "Partial" = "Partial", .default = "Others")
    
    
    RawData$MSSubClass <- recode_factor(RawData$MSSubClass, "20" = "Newer_1946",
                                        "60" = "Newer_1946", "120" = "Newer_1946",
                                        "160" = "Newer_1946", "30" = "Older_1945",
                                        "70" = "Older_1945", "40" = "All_Ages", 
                                        "45" = "All_Ages", "50" = "All_Ages",
                                        "75" = "All_Ages", "90" = "All_Ages", 
                                        "150" = "All_Ages", "190" = "All_Ages",
                                        "80" = "Others", "85" = "Others",
                                        "180" = "Others")
    
    RawData
}
