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

#Set working directory
setwd("S:/Enhanced Surveillance/General CD/Automated STI Case Closure")

#Load all supporting functions
source('STI Case Closure Functions.R')

#Set length of time before cases without treatment will be auto-closed (in days)
autoCloseTimeDelay <- 90

#Set time lag before cases will be subject to processing by the script
processingDelay <- 14

#File path to errors list -- IMPORTANT: IF RUNNING SCRIPT ON ENTIRE CASE LOAD ROUTINELY, WILL PROBABLY WANT ERROR CSV FROM PREVIOUS RUN TO BE DELETED OR EMPTIED FIRST
errorCSV <- "S:/Enhanced Surveillance/General CD/Automated STI Case Closure/STI Exceptions.csv"

#Import accepted treatment lists
ct_rx <- read_csv('accepted_ct_rx.csv')
gc_rx <- read_csv('accepted_gc_rx.csv')

#=================LOGGING INTO THE PORTAL AND NAVIGATING TO LAB PROVIDER=================#

#Open selenium session
remDr <- rsDriver(browser = "firefox")

#Extract the client for navigation
rD <- remDr[['client']]

#Navigating to log-in page
rD$navigate("https://dph.partner.illinois.gov/my.policy")

#Pause to give page time to load
Sys.sleep(5)

#Check for cookies error
login_error <- try(rD$findElement("css", "#newSessionDIV > a:nth-child(1)"))
if (class(login_error) != "try-error") {login_error$clickElement()}

#Pause to give page time to load
Sys.sleep(5)

#Clicking link to access log-in screen
rD$findElement("css", ".interaction_table_text_cell > a:nth-child(1)")$clickElement()

#Pausing execution to give time to log in and load page
Sys.sleep(5)

#Enter credentials and log in
rD$findElement(using = "css", value = "#input_1")$sendKeysToElement(list(key_get("idph_username"), key = "tab", key_get("idph_portal")))
rD$findElement("css", "input[value = \"Logon\"]")$clickElement()

#Pausing execution to give time to log in and load page
Sys.sleep(10)

#Mousing over applications button
rD$findElement(using = "xpath", value = '//*[@id="zz6_RootAspMenu"]/li/ul/li[1]/a/span/span')$mouseMoveToLocation()

#Finding production apps button
rD$findElement(using = "xpath", value = '//*[@id="zz6_RootAspMenu"]/li/ul/li[1]/a')$clickElement()

#Finding INEDSS buttons  -- IMPORTANT: XPATH WILL BE DIFFERENT DEPENDING ON APPS USER HAS
ifVisiblethenClick('//*[@id="column"]/table[5]/tbody/tr/td[2]/a', selectorType = "xpath") 

#Pausing execution to give time to load page
Sys.sleep(10)

#Switching focus to INEDSS tab   
windows <- rD$getWindowHandles()   
rD$switchToWindow(windows[[2]])

#Clicking login button
rD$findElement(using = "css", value = "input[name = \"login\"]")$clickElement()

#Pausing execution to give time to load page
Sys.sleep(5)

#Clicking into STIs (CSS not stable so must search to ID row)
dashBoardItems <- rD$findElement(using = "css", value ="#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(2)")$findChildElements(using = "css", "tbody > tr") %>%
  map_chr(., function(x) x$getElementText()[[1]])
stiRow <- which(grepl("STD Section", dashBoardItems))
rD$findElement("css", paste0("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(", stiRow, ") > td:nth-child(1) > a:nth-child(2)"))$clickElement()

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
  isPageLoaded(".pageDesc")
  
  #Store event date and state case number and disease for future use
  eventDate <- rD$findElement("css", "#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(2)")$getElementText()[[1]] %>%
    lubridate::mdy()
  stateCaseNumber <- rD$findElement("css", "#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(6) > td:nth-child(2)")$getElementText()[[1]]
  disease <- rD$findElement("css", "#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1)")$getElementText()[[1]]
  
  #Exit out if case is too new or event date is missing 
  if ((Sys.Date() - eventDate) < processingDelay | is.na(eventDate)) {
    
    rD$findElement("css", "input[name = \"cancel\"]")$clickElement()
    
    #Increment cases worked counter
    totalLeftOpen <- totalLeftOpen + 1
    
    #Determine next row to work
    tr_n_child_Val <- ifelse(totalLeftOpen %% 25 == 0, 8, tr_n_child_Val + 1)
    
    
  } else {
    
    #Determine whether case is subject to new or old processing rules
    processingType <- ifelse((Sys.Date() - eventDate) > autoCloseTimeDelay, "old", "new")
    
    #Click into All Case Details
    rD$findElement("css", "fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > a:nth-child(1)")$clickElement()
    
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
      
      #Processing diagnosis (no potential CCDPH errors so return contains info possibly needed in Treatment section)
      testOrderFac <- diagnosisProcessing()
      
      #Give page time to load
      isPageLoaded(".fullPageDescription")
      
      #Processing treatment
      treatmentResults <- treatmentProcessing(disease)
      
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

      #write invalid conditions to file
      caseResults <- data.frame(case = stateCaseNumber, date = eventDate, disease = disease, errors = invalidConditions, stringsAsFactors = FALSE)
      write_csv(caseResults, errorCSV, append = T)
      
      #Increment cases worked counter
      totalLeftOpen <- totalLeftOpen + 1
      
      #close out to main page
      rD$findElement("css", "#closetop")$clickElement()
      ifVisiblethenClick("input[name = \"cancel\"]")
      
      #Determine next row to work
      tr_n_child_Val <- ifelse(totalLeftOpen %% 25 == 0, 8, tr_n_child_Val + 1)

    } else {

      #close out to main page
      rD$findElement("css", "#closetop")$clickElement()
      
      #click complete investigation
      ifVisiblethenClick("fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(2) > a:nth-child(1)")
      
      #Mark disposition as completed
      completedChild <- map_chr(rD$findElement("css","#dis")$findChildElements("css", "option"), function(x) x$getElementText()[[1]]) %>%
        grepl("^Completed$", .) %>%
        which(. == TRUE)
      rD$findElement("css", paste0("#dis > option:nth-child(", completedChild,")"))$clickElement()
      
      #Mark case status confirmed
      confirmedChild <- map_chr(rD$findElement("css","#case")$findChildElements("css", "option"), function(x) x$getElementText()[[1]]) %>%
        grepl("Confirmed", .) %>%
        which(. == TRUE)
      rD$findElement("css", paste0("#case > option:nth-child(", confirmedChild,")"))$clickElement()
      
      #Add script closure comment to log
      rD$findElement("css", "#comment")$sendKeysToElement(list("Administratively closed."))
      
      #Click send to IDPH
      rD$findElement("css", "input[name = \"save\"]")$clickElement()
      
      #Accept alert
      acceptAlertwithWait()
      
      #Increment cases worked counter
      totalClosed <- totalClosed + 1

    }
    
  }  #if/else closure for case too new to process or not
  
  
  #Give main page time to load
  isPageLoaded(".pageDesc")
  
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
remDr$server$stop() 

#Deduplicate error CSV  (might not be necessary if working through entire case load on each script run)
errors <- read_csv(errorCSV) %>%
  distinct()

#Resave final errors CSV
write_csv(errors, errorCSV)

#Save processing stats
scriptStats <- data.frame(Date = Sys.Date(), totalLeft = totalLeftOpen, totalClosed = totalClosed)
write_csv(scriptStats, "Processing Statistics.csv", append = T)

