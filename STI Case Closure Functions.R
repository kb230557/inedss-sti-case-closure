

#=========================GENERAL USE FUNCTIONS=========================#

#General use functions now loaded from Github



#=========================PERSON SUMMARY PAGE=========================#

#Previously had selectCase() function (see archived scripts) here but no longer necessary now that script works through the case links, rather than the person links 


#=========================DEMOGRAPHICS=========================#

#function to process demographics
demographicsProcessing <- function() {
  
  #Click into demographics
  click_link("Demographic")
  
  #Give page time to load
  isPageLoaded(".pageDesc")
  
  
  #Create holder for errors
  demographicsErrors <- vector("character", 6)
  
  
  #Storing gender info
  isGenderMissing <- length(rD$findElement("css", "#currentGender")$findChildElements("css", "option[selected=\"\"]")) == 0
  if(isGenderMissing == FALSE) {
    
    isGenderUnknown <- grepl("Unknown", rD$findElement("css", "#currentGender")$findChildElements("css", "option[selected=\"\"]")[[1]]$getElementText()[[1]])
    
  } else {
    
    isGenderUnknown <- FALSE
    
  }
  
  #Storing sex info
  isSexMissing <- length(rD$findElement("css", "#sex")$findChildElements("css", "option[selected=\"\"]")) == 0
  if(isSexMissing == FALSE) {
    
    isSexUnknown <- grepl("Unknown", rD$findElement("css", "#sex")$findChildElements("css", "option[selected=\"\"]")[[1]]$getElementText()[[1]])
    
  } else {
    
    isSexUnknown <- FALSE
    
  }
  
  #Generate CCDPH error if sex/gender Unknown (but not missing) 
  if ((isSexUnknown & isGenderMissing) | (isGenderUnknown & isSexMissing) | (isGenderUnknown & isSexUnknown)) {
    
    demographicsErrors[1] <- "Sex/gender is entered as Unknown."
    
  }
  
  #Generate CCDPH error if sex/gender is missing) 
  if (isGenderMissing & isSexMissing) {
    
    demographicsErrors[2] <- "Sex/gender is blank."
    
  }
  
  
  #Checking race
  currentRace <- rD$findElement("css", "#selectedRace")$findChildElements("css", "option") %>%
    map_chr(., function(x) x$getElementText()[[1]]) 
  isRaceUnknown <- grepl("Unknown", currentRace) 
  isRaceMissing <- !grepl("[[:alpha:]]", currentRace) 
  
  #Processing race
  if(isRaceMissing & processingType == "old") {
    
    #Find unknown option
    unknownRaceChild <- map_chr(rD$findElement("css", "#availableRace")$findChildElements("css", "option"), function(x) x$getElementText()[[1]]) %>%
      grepl("Unknown", .) %>%
      which(. == TRUE)
    
    #click Unknown and Add
    click(paste0("#availableRace > option:nth-child(", unknownRaceChild,")"))
    click("fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(3) > p:nth-child(1) > input:nth-child(1)")
  
  } else if (isRaceUnknown & processingType == "new"){
    
    #Generate CCDPH error
    demographicsErrors[3] <- "Race is entered as Unknown."
    
  } else if (isRaceMissing & processingType == "new"){
    
    #Generate CCDPH error
    demographicsErrors[4] <- "Race is blank."
    
  }

  
  #Checking ethnicity
  isEthnicityMissing <- length(rD$findElement("css", "#ethnic")$findChildElements("css", "option[selected=\"\"]")) == 0
  if(isEthnicityMissing == FALSE) {
    
    isEthnicityUnknown <- grepl("Unknown", rD$findElement("css", "#ethnic")$findChildElements("css", "option[selected=\"\"]")[[1]]$getElementText()[[1]])
    
  } else {
    
    isEthnicityUnknown <- FALSE
    
  }
  
  #Processing ethnicity
  if(isEthnicityMissing & processingType == "old") {
    
    #Find unknown and click
    unknownEthnicityChild <- map_chr(rD$findElement("css", "#ethnic")$findChildElements("css", "option"), function(x) x$getElementText()[[1]]) %>%
      grepl("Unknown", .) %>%
      which(. == TRUE)
    click(paste0("#ethnic > option:nth-child(", unknownEthnicityChild,")"))
    
  } else if (isEthnicityUnknown & processingType == "new") {
    
    #Generate CCDPH error
    demographicsErrors[5] <- "Ethnicity is entered as Unknown."
    
  } else if (isEthnicityMissing & processingType == "new") {
    
    #Generate CCDPH error
    demographicsErrors[6] <- "Ethnicity is blank."
    
  } 
  
  
  #click save
  click(name.is("save"))
  
  #Check to see if still on page and invalid conditions popped up
  pageTitle <- try(rD$findElement("css", ".pageDesc"))
  pageInvalidConditions <- try(rD$findElement("css", "td[bgcolor = \"#FCFCD0\"]"))
  
  #If still on page and invalid conditions present, cancel out (throws off the script)
  if (class(pageTitle) != "try-error" & class(pageInvalidConditions) != "try-error") {
    
    click(name.is("cancel"))
    
    return(NA)
    
  }
  
  #Give next page time to load
  Sys.sleep(1)
  
  #See if the validate address page comes up
  validateAddress <- try(rD$findElement("css", "#enterAdd"))
  
  if (class(validateAddress) != "try-error") {
    
    #accept default validated address
    click(name.is("save"))
    
  }
  
  
  #Return result
  return(paste(demographicsErrors, collapse = " "))

  
}




#=========================DIAGNOSIS=========================#
diagnosisProcessing <- function() {
  
  #click into diagnosis
  click_link("Diagnosis")
  
  #Give page time to load
  isPageLoaded(".pageDesc")
  
  #Checking test ordering facility
  isTestDropDownEmpty <- length(rD$findElement("css", "#testOrderingFacility")$findChildElements("css", "option[selected=\"\"]"))
  isTestTextBoxEmpty <- nchar(get_text("#STDTRMTOTHTESTORDFAC"))
  
  if (isTestDropDownEmpty == 0 & isTestTextBoxEmpty == 0) {
    
    #Gather comment info
    diagnosisComment <- get_text("#STDPROVPARTNOTIFYCOM")
    
    #if comment contains test ordering info, copy into test ordering text box
    if(grepl("Test Ordering Info", diagnosisComment)) {
      
      enter_text("#STDTRMTOTHTESTORDFAC", diagnosisComment)
      
    } 
    
  } #processing test ordering if closure
  
  
  #If not entering if above, test ordering facility is complete, exit with no error
  click("#caseSummary")
  
  #Check for page-specific invalid conditions
  pageTitle <- try(rD$findElement("css", ".pageDesc"))
  pageInvalidConditions <- try(rD$findElement("css", "td[bgcolor = \"#FCFCD0\"]"))
  
  #If still on page and invalid conditions present, cancel out (throws off the script)
  if (class(pageTitle) != "try-error" & class(pageInvalidConditions) != "try-error") {
    
    click("#cancel")
    
  }
  
  
  #Returning test ordering in case needed for treatment
  return(ifelse(exists("diagnosisComment"), diagnosisComment, NA))
  
}



#=========================TREATMENT=========================#
treatmentProcessing <- function(ctorgc) {
  
  #Click into treatment
  click_link("Treatments")

  #Give page time to load
  isPageLoaded(".pageDesc")
  
  #Setting correct rx list (script should fail if it encounters an STI besides CT or GC)
  if (ctorgc == 'Chlamydia') { rx_list <- ct_rx$rx} 
    else if (ctorgc == 'Gonorrhea') {rx_list <- gc_rx$rx }
  
  #Creating holder for error
  treatmentError <- vector("character", 1)
  
  #Initializing treatment vectors
  treatmentSelected <- vector("logical", 3)
  treatmentAdequate <- vector("logical", 3)
  
  #loop through treatments to assess info
  for (i in 1:3) {
    
    #set element to work
    id <- paste0("#treatment", i)
    
    #is treatment missing  
    treatmentSelected[i] <- length(rD$findElement("css", id)$findChildElements("css", "option[selected=\"\"]")) == 1
    
    #if not missing, is it adequate
    if (treatmentSelected[i]) {
      
      treatmentAdequate[i] <- rD$findElement("css",id)$findChildElements("css", "option[selected=\"\"]")[[1]]$getElementText()[[1]] %>%
        grepl(., rx_list) %>%
        any()
      
    } else {
      
      treatmentAdequate[i] <- FALSE
      
    }
    
    
  }

  
  #if case is new and has no adequate treament entered, exit with errors
  if(processingType == "new" & any(treatmentSelected) == TRUE & any(treatmentAdequate) == FALSE) {
    
    #Save CCDPH error
    treatmentError <- "Treatment inadequate or needs verification."
    
  }
  

  
  #if treatment entered, make sure treating provider entered
  if ((any(treatmentSelected) & processingType == "old") | (any(treatmentAdequate) & processingType == "new")) {
    
    #is treating provider available
    isTreatDropDownEmpty <- length(rD$findElement("css", "#treatingFacility")$findChildElements("css", "option[selected=\"\"]")) == 0
    isTreatTextBoxEmpty <- nchar(str_squish(get_text("#STDTRMTOTHTREATFAC"))) < 2
    
    #if not, attempt to click same as testing and copy diagnosis comment to box as back up
    if (isTreatDropDownEmpty == TRUE & isTreatTextBoxEmpty == TRUE) {
      
      #Attempt to click box
      click("#sameAsTof")
      
      #Make sure text box enabled
      #Click search
      click("#searchTreatingFacility")
      #Search random string
      enter_text("#name","zzzz")
      #Click search then cancel
      click(name.is("search"))
      click(name.is("cancel"))
      
      #Copy diagnosis provider to text box if available
      enter_text("#STDTRMTOTHTREATFAC", testOrderFac)
      
    }
    
  }
  
  #if case is old and no treatment info, select no treatment and exit with no errors
  if(processingType == "old" & any(treatmentSelected) == FALSE) {
    
    #Find no treatment and click
    noRxChild <- map_chr(rD$findElement("css", "#treatment1")$findChildElements("css", "option"), function(x) x$getElementText()[[1]]) %>%
      grepl("No Treatment", .) %>%
      which(. == TRUE)
    click(paste0("#treatment1 > option:nth-child(", noRxChild,")"))
    
    #Add comment
    enter_text("#STDTRMTCOMMENT", "Treatment unknown.")
    
  }
  

  
  #Exit out of page
  click("#caseSummary")
  
  #Check for page-specific invalid conditions
  pageTitle <- try(rD$findElement("css", ".pageDesc"))
  pageInvalidConditions <- try(rD$findElement("css", "td[bgcolor = \"#FCFCD0\"]"))
  
  #If still on page and invalid conditions present, cancel out (throws off the script)
  if (class(pageTitle) != "try-error" & class(pageInvalidConditions) != "try-error") {
    
    rD$findElement("css", "#cancel")$clickElement()
    
  }
  
  #return errors
  return(treatmentError)
  
}  
  

#=========================LABORATORY=========================#

labProcessing <- function() {
  
  #Click in Lab Tests section
  click_link("Laboratory Tests")
  
  #Click to expand specimen info
  click("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(7) > td:nth-child(7) > a:nth-child(1)")
  
  
  
  facilityName <- get_text("input#orderingFacilityName")
  
  
  
  
}

