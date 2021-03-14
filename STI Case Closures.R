library(RSelenium)
library(magrittr)
library(purrr)
library(stringr)
library(keyring)
library(methods)
library(tibble)
library(readr)
library(dplyr)

#key_set("idph_username") #set your IDPH web portal username -- only needs to be done once per computer
#key_set("idph_portal")  #set your IDPH web portal password -- only needs to be done once per computer

#Note: IE is preferred browser for INEDSS but requires special drivers for Selenium. 
#Chrome has issues with switching tabs so script will only work with the Firefox browser.

#Load all supporting functions
source('STI Case Closure Functions.R')
devtools::source_url("https://github.com/hsteinberg/ccdph-functions/blob/master/general-use-rselenium-functions.R?raw=TRUE")
devtools::source_url("https://github.com/hsteinberg/ccdph-functions/blob/master/inedss-rselenium-functions.R?raw=TRUE")

#Set length of time before cases without treatment will be auto-closed (in days)
autoCloseTimeDelay <- 90

#Set time lag before cases will be subject to processing by the script
processingDelay <- 14

#If running to close all cases at the end of the year, set endofYear to TRUE, otherwise should be FALSE
endofYear <- FALSE

#Set year when running for end of year case closure
if (endofYear) { closingYear <- "20" } else { closingYear <- NA}

#Create empty file to store cases that can't be closed yet
#Note: if script run over multiple days, will need to concatenate files
errors <- data.frame(StateCaseNumber = character(), 
                     EventDate = as.Date(character()), 
                     Disease = character(),
                     Reason = character(),
                     OrderingFacilityName = character(),
                     OrderingFacilityAddress = character(),
                     OrderingFacilityPhone = character(),
                     OrderingProviderName = character(),
                     OrderingProviderPhone = character()
                     )  

if (endofYear) {
  error_path <- paste0('sti-exceptions/', Sys.Date(), "_sti-exceptions_end-of-year.csv")
} else {
  error_path <- paste0('sti-exceptions/', Sys.Date(), "_sti-exceptions.csv") 
}
  
write_csv(errors, error_path)


#Import accepted treatment lists
ct_rx <- read_csv('accepted_ct_rx.csv')
gc_rx <- read_csv('accepted_gc_rx.csv')

#=================LOGGING INTO THE PORTAL AND NAVIGATING TO LAB PROVIDER=================#

#Open selenium session
start_server()

#Log in to INEDSS
login_inedss()

#Clicking into STIs (CSS not stable so must search to ID row)
stiRow <- find_child_element("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(2)", "tbody > tr", "STD Section")

click(paste0("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(", stiRow, ") > td:nth-child(1) > a:nth-child(2)"))



#Initializing n_child to start with first case 
tr_n_child_Val <- 8

#Initializing counters
totalClosed <- 0
totalLeftOpen <- 0



#=================BEGIN LOOP TO PROCESS CASES=================#

repeat {
  
  #Increment n_child until case encountered 
  nextCase <- try(rD$findElement(using = "css", value = paste0("table.indessTable:nth-child(2) > tbody:nth-child(1) > tr:nth-child(", tr_n_child_Val, ") > td:nth-child(2) > a:nth-child(1)")))
  
  while(class(nextCase) == "try-error" & tr_n_child_Val < 301) {
    
    tr_n_child_Val <- tr_n_child_Val + 1
    nextCase <- try(rD$findElement(using = "css", value = paste0("table.indessTable:nth-child(2) > tbody:nth-child(1) > tr:nth-child(", tr_n_child_Val, ") > td:nth-child(2) > a:nth-child(1)")))
    
  }
  
  #If not encountered, send error and stop script
  if (tr_n_child_Val >= 301) {
    
    stop("No case to process")  
    
  }
  
  #Otherwise, click into nextCase
  nextCase$clickElement()
  
  #Give case page time to load
  #isPageLoaded(".pageDesc")
  wait_page("Case Summary")
  
  #Store event date and state case number and disease for future use
  eventDate <- get_text("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(2)") %>%
    lubridate::mdy()
  stateCaseNumber <- get_text("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(6) > td:nth-child(2)")
  disease <- get_text("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1)")
  #Get first two digits of state case number (used for end of year closures)
  scnPrefix<- substr(stateCaseNumber,1,2)
  
  #Exit out if 1) case is too new or 2) event date is missing or 3) if running for end of year and case in current year 
  if ((endofYear == T & scnPrefix != closingYear) | 
      (endofYear == F & (Sys.Date() - eventDate) < processingDelay)| 
      (endofYear == F & is.na(eventDate))) {
    
    click(name.is("cancel"))
    
    #Increment cases worked counter
    totalLeftOpen <- totalLeftOpen + 1
    
    #Determine next row to work
    tr_n_child_Val <- ifelse(totalLeftOpen %% 25 == 0, 8, tr_n_child_Val + 1)
    
    
  } else {
    
    #Determine whether case is subject to new or old processing rules
    if (endofYear) {
      processingType <- "old"
    } else {
      processingType <- ifelse((Sys.Date() - eventDate) > autoCloseTimeDelay, "old", "new")
    }
    
    #Click into All Case Details
    click_link("View/Edit All Case Details")
    
    #Give page time to load
    isPageLoaded(".fullPageDescription")
    
    #Store initial invalid conditions
    invalidConditions <- try(rD$findElement("css", "td[bgcolor = \"#FCFCD0\"]")$findChildElements("css", "center") %>%
                               map_chr(., function(x) x$getElementText()[[1]]))
    
    #Check for major name error before processing case
    if (class(invalidConditions) != "try-error") {
      nameError <- grepl("First Name|Last Name", invalidConditions)
    } else {
      nameError <- FALSE
    }
    
    #If no name error, process other sections
    if (nameError == FALSE) {
      
      #Processing demographics
      demographicsResults <- demographicsProcessing()
      
      #Give page time to load
      isPageLoaded(".fullPageDescription")
      
      #Get provider info
      #Process lab prior to diagnosis in case specimen collection date needed
      provider = labProcessing() 
      
      #Processing diagnosis (no potential CCDPH errors; return contains info possibly needed in Treatment section)
      testOrderFac <- diagnosisProcessing()
      
      #Give page time to load
      isPageLoaded(".fullPageDescription")
      
      #Processing treatment
      treatmentResults <- treatmentProcessing(disease)
      
      #Give page time to load
      isPageLoaded(".fullPageDescription")
      
      #Creating CCDPH error string
      CCDPHErrors <- paste(demographicsResults, treatmentResults, collapse = " ")
      
      #Give page time to load
      isPageLoaded(".fullPageDescription")
      
      #Re-saving invalid Conditions
      invalidConditions <- try(rD$findElement("css", "td[bgcolor = \"#FCFCD0\"]")$findChildElements("css", "center") %>%
                                 map_chr(., function(x) x$getElementText()[[1]]))
      
      #Create final error list
      if (class(invalidConditions) != "try-error") {
        
        invalidConditions <- paste(invalidConditions, CCDPHErrors, collapse = " ")
        
      } else {
        
        invalidConditions <- CCDPHErrors
        
      }
      
    } #processing if closure
  
    #Clean up string
    invalidConditions <- invalidConditions %>%
      str_remove(pattern = "The following invalid conditions were found:\n\n") %>% 
      str_remove(pattern = "Race, Sex, and Ethnicity cannot be blank.") %>%
      str_remove(pattern = "Disposition is required to close the case.") %>%
      str_squish()
    
    #Take final actions
    if (nchar(invalidConditions) > 1) { #invalid conditions exist, case will not be closed
      
      #Give page time to load
      isPageLoaded(".fullPageDescription")

      #write invalid conditions to file
      caseResults <- data.frame(case = stateCaseNumber, date = eventDate, disease = disease, errors = invalidConditions, stringsAsFactors = FALSE) %>%
        bind_cols(provider)
      write_csv(caseResults, error_path, append = T)
      
      #Increment cases worked counter
      totalLeftOpen <- totalLeftOpen + 1
      
      #close out to main page
      click("#closetop")
      ifVisiblethenClick(name.is("cancel"))
      
      #Determine next row to work
      tr_n_child_Val <- ifelse(totalLeftOpen %% 25 == 0, 8, tr_n_child_Val + 1)

    } else {

      #close out to main page
      click("#closetop")
      
      #click complete investigation
      ifVisiblethenClick("fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(2) > a:nth-child(1)")
      wait_page("Complete Investigation")
      
      #Check for final case validation issues
      caseValidation <- get_text("#completion")
      
      #If errors will prevent closing the case, exit out and store in errors list
      if (grepl("A Chlamydia case already exists for this person", caseValidation)) {
        
        #write invalid conditions to file
        caseResults <- data.frame(case = stateCaseNumber, date = eventDate, disease = disease, errors = caseValidation, stringsAsFactors = FALSE) %>%
          bind_cols(provider)
        write_csv(caseResults, error_path, append = T)
        
        #exit from complete case
        click(name.is("cancel"))
        
        #Give page time to load
        wait_page("Case Summary")
        
        #Increment cases worked counter
        totalLeftOpen <- totalLeftOpen + 1
        
        #close out to main page
        ifVisiblethenClick(name.is("cancel"))
        
        #Determine next row to work
        tr_n_child_Val <- ifelse(totalLeftOpen %% 25 == 0, 8, tr_n_child_Val + 1)
        
      } else {
        
        #Mark disposition as completed
        completedChild <- map_chr(rD$findElement("css","#dis")$findChildElements("css", "option"), function(x) x$getElementText()[[1]]) %>%
          grepl("^Completed$", .) %>%
          which(. == TRUE)
        click(paste0("#dis > option:nth-child(", completedChild,")"))
        
        #Mark case status confirmed
        confirmedChild <- map_chr(rD$findElement("css","#case")$findChildElements("css", "option"), function(x) x$getElementText()[[1]]) %>%
          grepl("Confirmed", .) %>%
          which(. == TRUE)
        click(paste0("#case > option:nth-child(", confirmedChild,")"))
        
        #Add script closure comment to log
        enter_text("#comment", "Administratively closed.")
        
        #Click send to IDPH
        click(name.is("save"))
        
        #Accept alert
        acceptAlertwithWait()
        
        #Increment cases worked counter
        totalClosed <- totalClosed + 1
        
      } #if/else from completion screen

    } #if/else all closing actions
    
  }  #if/else closure for case too new to process or not
  
  
  #Give main page time to load
  wait_page("My Cases")
  
  #Determining page to work
  pageCount <- floor(totalLeftOpen / 25) + 1
  
  if (pageCount > 0) {
    
    #Click to go page being worked
    ifVisiblethenClick(paste0("td.position > select:nth-child(2) > option:nth-child(", pageCount,")"))
    
    #Give page time to load
    isPageLoaded(".pageDesc")
    
  }
  

}


#=================FINAL CLEAN UP ACTIONS=================#

#Stop server
stop_server()

#Save processing stats -- not useful until script is more stable
# if (endofYear == F) {  #not relevant for end of year
#   scriptStats <- data.frame(Date = Sys.Date(), totalLeft = totalLeftOpen, totalClosed = totalClosed)
#   write_csv(scriptStats, "Processing Statistics.csv", append = T)
# }



