# Adapted from https://gitlab.com/karthikdurvasula/nsf-wordacceptability-workingfolder

library(tidyverse)
library(xlsx)
library(rvest)
library(jsonlite)
library(stringr)


#Data munger
DataMunger = function(data=dataOriginal,TestStart=40,TestStimuliCount=60,inputFileName){
  #Removing the first 39 lines, as they are unnecessary for analysis
  
  dataCleaned = data[-c(1:(TestStart-1)),] %>% 
    filter(trial_type != "html-keyboard-response")
  
  # Identify demographic vs test responses based on content pattern
  dataCleaned = dataCleaned %>%
    mutate(isDemographic = grepl("P0_Q1", response, fixed = TRUE)) %>%
    mutate(stimulusNumber = cumsum(!isDemographic & !is.na(response) & response != "")) %>%
    # For demographic rows, use a special code
    mutate(CodingForStimulus = ifelse(isDemographic, 999, 
                                     ifelse(!is.na(response) & response != "", 
                                            ceiling(stimulusNumber/2), NA)))
  
  #Response data - exclude demographic responses
  dataResponses = dataCleaned %>% 
    filter(!isDemographic) %>%  # Exclude demographic data
    filter(!is.na(rt)) %>% 
    select(rt:response,CodingForStimulus) %>% 
    mutate(responseType=ifelse(grepl("TestRecording",stimulus),"ResponsePlaySound","ResponseActual")) %>% 
    mutate(stimulus = gsub("TestRecordings/","",stimulus)) %>% 
    #Fill works only if the cell has NA, so converting blank to NA
    mutate(stimulus = ifelse(stimulus=="",NA,gsub(".wav","",stimulus))) %>% 
    #Copies value from previous cell to the NA cell
    fill(stimulus) %>%
    filter(responseType=="ResponseActual") %>% 
    # Extract rating from JSON format like {"P0_Q0":5}
    # Only extract if it's a simple numeric rating, not demographic data
    mutate(Rating = ifelse(grepl("P0_Q1", response, fixed = TRUE), 
                          NA,  # If it contains P0_Q1, it's demographic data - set to NA
                          gsub(".*\"P0_Q0\":(\\d+).*", "\\1", response))) %>%
    # Convert Rating to numeric - but DON'T filter out failed extractions yet
    mutate(Rating = as.numeric(Rating)) %>%
    select(-response)  # Remove original response column
  
  
  
  #Demographic data - only get actual demographic responses
  dataDemographic = dataCleaned %>% 
    filter(isDemographic) %>%  # Only demographic data
    mutate(ResponseCleaned=gsub("\"","",response)) %>% 
    # ResponseCleaned=gsub("{","",ResponseCleaned)) %>% 
    # separate(response,sep=",",into=as.character(1:7))
    separate(response,sep=",",into=c("Age","Gender","AmericanEnglishNative","OtherLanguages","AgeOfOtherL2Onset","DescribeOtherL2Experience"), extra = "merge") %>% 
    #Cleaning up the demographic info
    mutate(Age=gsub("\\{\"P0_Q0\":","",Age),
           Age=as.numeric(gsub("\"","",Age))) %>% 
    mutate(Gender = gsub("\"P0_Q1\":","",Gender),
           Gender=gsub("\"","",Gender)) %>% 
    mutate(AmericanEnglishNative=gsub("\"P0_Q2\":","",AmericanEnglishNative),
           AmericanEnglishNative=gsub("\"","",AmericanEnglishNative)) %>% 
    mutate(OtherLanguages = gsub("\"P0_Q3\":","",OtherLanguages),
           OtherLanguages=gsub("\"","",OtherLanguages)) %>%
    mutate(AgeOfOtherL2Onset=gsub("\"P0_Q4\":","",AgeOfOtherL2Onset),
           AgeOfOtherL2Onset=gsub("\"","",AgeOfOtherL2Onset)) %>% 
    mutate(DescribeOtherL2Experience = gsub("\"P0_Q5\":","",DescribeOtherL2Experience),
           DescribeOtherL2Experience=gsub("\"","",DescribeOtherL2Experience),
           DescribeOtherL2Experience=gsub("}","",DescribeOtherL2Experience)) %>% 
    select(Age:DescribeOtherL2Experience)
  
  #Filename/Subject
  dataFileName = data.frame(Subject=rep(gsub(".csv","",inputFileName),nrow(dataResponses)))
  
  # Repeat demographic data for each response row
  dataDemographicRepeated = dataDemographic[rep(1, nrow(dataResponses)), ]
  
  #returning modified dataframe
  cbind(dataFileName,dataResponses,dataDemographicRepeated)
}

# dataOriginal=read.csv("Results/Korean-3a/Originals/Converted to reasonable CSVs_K3a_L1/K_L1_001.csv")
# cleanedData=DataMunger(data=data)

#***************
#Reading in files

#Directory and files
inputDirectoryData = "/Users/foresthallee/Documents/School/CUNY/jobs/nsf-wordacceptability-workingfolder-main/Results/Experiment-1a-English/Originals/Original_Data_Exp1a_"
List = c("L1","L2")
outputDirectory = "/Users/foresthallee/Documents/School/CUNY/jobs/nsf-wordacceptability-workingfolder-main/Results/Experiment-1a-English/"
outputFileName = "English-1a_"

#iterating through each list folder
for(list in List){
  filenamesList=list.files(path=paste0(inputDirectoryData,list),pattern="*.csv")  

    
  #iterating through each file in the list
  fullDataForList = NULL
  # list="L1"
  for(fileName in filenamesList){
    #Actual data
    # fileName="English2-20241007-005.csv"
    dataOriginal=read.csv(paste0(inputDirectoryData,list,"/",fileName))
    
    fullDataForList = rbind(fullDataForList,DataMunger(data=dataOriginal,inputFileName=fileName))
  }

  #Final data.frame
  # inputFilename = str_replace_all(inputFilename,".csv","")
  # write.csv(fullDataForList,paste0(outputDirectory,outputFileName,list,".csv"),row.names=F,fileEncoding = "UTF-8")
  write_excel_csv(fullDataForList,paste0(outputDirectory,outputFileName,list,".csv"))
}



# #***************
# #*#Trying to automate reading in the HTML file
# library(rvest)
# url = 'Results/Korean-3a/Original_Data_K3a_L1/K_L1_001.html'
# site = read_html(url)
# text = html_text(html_nodes(site, 'jspsych-data-display')) # comma separate
# 
# library(htmltools)
# X=includeHTML(url)
# 
# # library(xml2)
# # X=read_html(x = "Results/Korean-3a/Data_K3a_L1/K_L1_001.html")
# # 
# library(rvest)
# scraping_wiki <- read_html(url)
# Y=scraping_wiki %>%
#   html_nodes("pre") %>%
#   html_text() #%>%
#   str_split("\"\"\"")
#   
# read.csv(url)
#   
# # 
# # 
# # library(xlsx)
# # write.xlsx(Y,"Results/Korean-3a/Data_K3a_L1/K_L1_001_trialautomated.xlsx")

#***************


