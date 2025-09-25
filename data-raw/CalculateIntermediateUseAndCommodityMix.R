## Generate Intermediate Use and Commodity mix tables for nowcasting

## 1. Load/Build objects
# Set paths
modelspecs_dir <- "tests/modelspecs"
modelspecs_path <- file.path(modelspecs_dir)


# 1.1 Build the model objects required for this file to run.
# Model specs: PRO price, 2017 schema, ECON only, Detail level
m <- "USEEIOv2.2-PRO-ECONONLY-17"
cfg <- paste0(file.path(modelspecs_path,m),".yml")
Make_Use_2017 <- buildIOModel(m, configpaths = cfg)
#saveRDS(Make_Use_2017, "data/USEEIOv2.2-PRO-ECONONLY-17.rds")

# The 2017 SUT model used in this script can be built with the following commands
# Model specs: BAS price, 2017 schema, ECON only, Detail level

SUT_2017 <-  initializeModel(m, cfg)
SUT_2017$specs$BasePriceType = "BAS"
SUT_2017 <- loadIOData(SUT_2017, cfg)
SUT_2017 <- loadDemandVectors(SUT_2017)
# function calls below part of buildIOModel() but don't work for BAS price type
#SUT_2017 <- buildEconomicMatrices(SUT_2017)
#SUT_2017 <- buildPriceMatrices(SUT_2017)

#Adding supply table back in 
schema <- getSchemaCode(SUT_2017$specs)
SUT_2017$Supply <- get(paste(na.omit(c(SUT_2017$specs$BaseIOLevel, "Supply", SUT_2017$specs$IOYear, schema)), collapse = "_"))

#saveRDS(SUT_2017, "data/SUT_2017.rds")

# Set target years
target_years <- c(2018:2023)

# BEA PCE Index, 2018-2023, as obtained from Table 2.3.4. Price Indexes for Personal Consumption Expenditures by Major Type of Product.
# 2017 = 1
BEA_PCE_Index <- c(1.02047,1.03509,1.04641,1.08972,1.16111,1.20491)

## 2. Calculate Intermediate Use

# Initialize list to store output tables
intermediate_Use_ls <- list()

# Add 2017 benchmark for printing to excel purposes
intermediate_Use_ls[["2017_Inter_U_PRO"]] <- Make_Use_2017$UseTransactions
intermediate_Use_ls[["2017_Inter_U_PRO"]] <- cbind(row.names(intermediate_Use_ls[["2017_Inter_U_PRO"]]), 
                                                   intermediate_Use_ls[["2017_Inter_U_PRO"]])  

for(year in target_years){
  print(year)
  
  inter_U_name <- paste0(as.character(year),"_Inter_U_PRO")
  
  # 2.1: Inflate the inputs to target year using Rho
  intermediate_Use_ls[[inter_U_name]] <- Make_Use_2017$U # Store original U in list for modification
  
  
  
  # There are no values in Rho for VA, so we calculate a weighted average Rho value for the VA rows for the current year
  # by using the commodity output values as weights, multiplying the Rho values by the weights, 
  # diving by the sum of the weights, and averaging the result 
  Rho_year_index = which(colnames(Make_Use_2017$Rho) %in% year)
  weighted_av_Rho = mean((Make_Use_2017$Rho[,Rho_year_index] * Make_Use_2017$q)/Make_Use_2017$q)
  
  # Comparing weighted_av_Rho to using BEA PCE Index
  print(paste0("Weighted average Rho vs. BEA PCE Index for ", year,": ",weighted_av_Rho,", ",1/BEA_PCE_Index[(year - 2017)]))
  
  Rho_with_VA <- append(Make_Use_2017$Rho[,Rho_year_index], rep(weighted_av_Rho,dim(Make_Use_2017$ValueAddedMeta)[1]))
  names(Rho_with_VA) <- rownames(Make_Use_2017$U)
  
  # Multiply U by Rho_with_VA vector
  intermediate_Use_ls[[inter_U_name]] <- intermediate_Use_ls[[inter_U_name]] * as.vector(Rho_with_VA)
  
  # 2.2: Calculate new ratios for each column, including value added, by dividing elements of intermediate use by its colsums
  # Note that we are summing over columns of Make_Use_2017$U to include VA and FD 
  # as these are not present in the intermediate matrices, and x and q no longer represent the industry and commodity 
  # output sums after inflation with Rho
  # Also note that:
  # A) colSums(Make_Use_2017$U) == Make_Use_2017$IndustryOutput == Make_Use_2017$x, 
  #    except for the FD columns which are missing in the latter 2
  # B) rowSums(Make_Use_2017$U) == Make_Use_2017$CommodityOutput == Make_Use_2017$q,
  #    except for the VA rows which are missing in the latter 2
  
  # Sweep should result in the same values as normalizeIOTransactions
  # intermediate_Use_ls[[inter_U_name]] <- sweep(intermediate_Use_ls[[inter_U_name]], 2,
  #                                              colSums(intermediate_Use_ls[[inter_U_name]]), FUN = '/') 
  
  
  intermediate_Use_ls[[inter_U_name]] <- useeior:::normalizeIOTransactions( 
    intermediate_Use_ls[[inter_U_name]],
    colSums(intermediate_Use_ls[[inter_U_name]]))
  
  # Validation: check if column sums equal to 1
  industryoutputfractions <- colSums(intermediate_Use_ls[[inter_U_name]])
  tolerance <- 0.005
  for (s in industryoutputfractions) {
    if (abs(1-s)>tolerance) {
      stop("Error in intermediate use")
    }
  }
  
  
  
  # 2.3: Multiply the ratios by gross industry output of current year (multiply columns by vector)
  
  # Drop value added and final demand from intermediate U matrix
  numCommodities <- dim(Make_Use_2017$Commodities)[1]
  numIndustries <- dim(Make_Use_2017$Industries)[1]
  
  intermediate_Use_ls[[inter_U_name]] <- intermediate_Use_ls[[inter_U_name]][1:numCommodities, 1:numIndustries]
  
  # Multiply columns by diagonalizing the vector
  ind_year_index = which(colnames(Make_Use_2017$MultiYearIndustryOutput) %in% year)
  
  intermediate_Use_ls[[inter_U_name]] <-  intermediate_Use_ls[[inter_U_name]] %*% 
    diag(Make_Use_2017$MultiYearIndustryOutput[,ind_year_index])
  
  #Convert to DF for printing
  intermediate_Use_ls[[inter_U_name]] <-  data.frame(intermediate_Use_ls[[inter_U_name]])
  colnames(intermediate_Use_ls[[inter_U_name]]) <- Make_Use_2017$Industries$Code_Loc
  rownames(intermediate_Use_ls[[inter_U_name]]) <- Make_Use_2017$Commodities$Code_Loc
  
  # To make sure the rows are printed to excel
  intermediate_Use_ls[[inter_U_name]] <- cbind(row.names(intermediate_Use_ls[[inter_U_name]]), intermediate_Use_ls[[inter_U_name]])                                        
  
}


## 3. Calculate commodity totals from SUT commodity mix (normalized make)

# Note that SUT_2017$MakeTransactions is equal to as.data.frame(t(SUT_2017$Supply[1:402, 1:402])*1e6)). I.e., 
#  all.equal(SUT_2017$MakeTransactions, as.data.frame(t(SUT_2017$Supply[1:402, 1:402])*1e6))
# > [1] "Names: 402 string mismatches"                                 "Attributes: < Component “row.names”: 402 string mismatches >"



# Changes from step 2:
# intermediate_Use_ls -> com_mix_ls
# inter_U_name  -> com_mix_name
# Make_Use_2017$U  -> SUT_2017$MakeTransactions 

# Initialize list to store output tables
com_mix_ls <- list()

# Add 2017 benchmark for printing to excel purposes
com_mix_ls[["2017_Supply_BAS"]] <- data.frame(t(SUT_2017$MakeTransactions))
com_mix_ls[["2017_Supply_BAS"]] <- cbind(row.names(com_mix_ls[["2017_Supply_BAS"]]), 
                                         com_mix_ls[["2017_Supply_BAS"]]) 

comOutput_ls <- list()

for(year in target_years){
  print(year)
  
  com_mix_name <- paste0(as.character(year),"_Supply_BAS")
  
  # 3.1: Inflate the inputs to target year using Rho
  com_mix_ls[[com_mix_name]] <- t(SUT_2017$MakeTransactions) # Store transposed Make Table in list for modification
  
  # Multiply Supply by Rho. Since there are no VA rows here no need to adjust for that.
  Rho_year_index = which(colnames(Make_Use_2017$Rho) %in% year) # Rho not available in SUT_2017
  com_mix_ls[[com_mix_name]] <- com_mix_ls[[com_mix_name]] * as.vector(Make_Use_2017$Rho[,Rho_year_index]) # This is right, manually inspected
  
  
  # 3.2: Calculate new ratios for each column, including value added, by dividing elements of the matrix by its colsums
  # Note that we are summing over the columns of the inflated com mix table.
  # Also note that we already transposed it so can't call the generateCommodityMixFunction (linked) without further changes
  # https://github.com/USEPA/useeior/blob/develop/R/IOFunctions.R#L74
  # However that function is copied and pasted below, without the transposition
  
  ## start copy of generate Commodity mix function 
  com_mix_ls[[com_mix_name]] <- useeior:::normalizeIOTransactions( 
    com_mix_ls[[com_mix_name]],
    colSums(com_mix_ls[[com_mix_name]]))
  
  # Validation: check if column sums equal to 1
  industryoutputfractions <- colSums(com_mix_ls[[com_mix_name]])
  tolerance <- 0.005
  for (s in industryoutputfractions) {
    if (abs(1-s)>tolerance) {
      stop("Error in commoditymix")
    }
  }
  
  ## end copy of generate Commodity mix function
  
  # 3.3: Multiply the ratios by gross industry output of current year (multiply columns by vector)
  
  # No need to Drop value added and final demand from table
  # Multiply columns by diagonalizing the vector
  ind_year_index = which(colnames(Make_Use_2017$MultiYearIndustryOutput) %in% year)
  
  com_mix_ls[[com_mix_name]] <-  com_mix_ls[[com_mix_name]] %*%
    diag(Make_Use_2017$MultiYearIndustryOutput[,ind_year_index])
  
  #Convert to DF for printing
  com_mix_ls[[com_mix_name]] <-  data.frame(com_mix_ls[[com_mix_name]])
  colnames(com_mix_ls[[com_mix_name]]) <- Make_Use_2017$Industries$Code_Loc
  rownames(com_mix_ls[[com_mix_name]]) <- Make_Use_2017$Commodities$Code_Loc
  
  # Sum across the rows rather than the columns to obtain commodity totals because we transposed the Make table
  comOutput_ls[[paste0(as.character(year),"_ComOutput_S_BAS")]] <- rowSums(com_mix_ls[[com_mix_name]])
  
  
  # To make sure the rows are printed to excel
  com_mix_ls[[com_mix_name]] <- cbind(row.names(com_mix_ls[[com_mix_name]]), com_mix_ls[[com_mix_name]])
  
  
}


## 4. Print
intermediate_Use_print_path <- file.path("data/intermediate_Use.xlsx")
commodity_mix_print_path <- file.path("data/com_mix.xlsx")

writexl::write_xlsx(intermediate_Use_ls, intermediate_Use_print_path, format_headers = FALSE)
writexl::write_xlsx(com_mix_ls, commodity_mix_print_path, format_headers = FALSE)




#source("data-raw/BEAData.R")
# 
# ls <- list("url" = "USEEIO",
#            "date_accessed" = "",
#            "date_last_modified" = "")
# 
# dir <- file.path(rappdirs::user_data_dir(), "USEEIO-input")
# dir.create(dir, showWarnings = FALSE)
# 
# for(yr in c(2018:2023)) {
#   name <- "U_out"
#   df <- read.csv(file.path(dir, paste0(name, "_", yr, ".csv")))
#   rownames(df) <- df[, 1]
#   df <- df[, -1]
#   names(df) <- gsub("^X", "", names(df))
#   writeFile(df = df, year = yr,
#             name = paste0("Detail_Use_", yr, "_PRO_BeforeRedef"), ls = ls,
#             schema_year = 2017)
# 
#   name <- "U_imports_out"
#   df <- read.csv(file.path(dir, paste0(name, "_", yr, ".csv")))
#   rownames(df) <- df[, 1]
#   df <- df[, -1]
#   names(df) <- gsub("^X", "", names(df))
#   writeFile(df = df, year = yr,
#             name = paste0("Detail_Import_", yr, "_BeforeRedef"), ls = ls,
#             schema_year = 2017)
#   
#   name <- "V_out"
#   df <- read.csv(file.path(dir, paste0(name, "_", yr, ".csv")))
#   rownames(df) <- df[, 1]
#   df <- df[, -1]
#   names(df) <- gsub("^X", "", names(df))
#   df <- t(df) ## TRANSPOSE!
#   writeFile(df = df, year = yr,
#             name = paste0("Detail_Make_", yr, "_BeforeRedef"), ls = ls,
#             schema_year = 2017)
# }





