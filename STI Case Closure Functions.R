

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
  Sys.sleep(2)
  
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
  #isPageLoaded(".pageDesc")
  isPageLoaded("#STDTRMTOTHTESTORDFAC")
  
  #Checking test ordering facility
  isTestDropDownEmpty <- length(rD$findElement("css", "#testOrderingFacility")$findChildElements("css", "option[selected=\"\"]"))
  isTestTextBoxEmpty <- nchar(get_text("#STDTRMTOTHTESTORDFAC"))
  
  #If provider info available in comments and test ordering provider is empty, move comments to text field
  if (isTestDropDownEmpty == 0 & isTestTextBoxEmpty == 0) {
    
    #Gather comment info
    diagnosisComment <- get_text("#STDPROVPARTNOTIFYCOM")
    
    #if comment contains test ordering info, copy into test ordering text box
    if(grepl("Test Ordering Info", diagnosisComment)) {
      
      #must search first to enable box
      click("#searchTestOrderingFacility")
      #Search random string
      enter_text("#name","zzzz")
      #Click search then cancel
      click(name.is("search"))
      click(name.is("cancel"))
      wait_page("Diagnosis")
      isPageLoaded("#STDTRMTOTHTESTORDFAC")
      
      #when box enabled, enter comment
      enter_text("#STDTRMTOTHTESTORDFAC", diagnosisComment)
      
    } 
    
  } #processing test ordering if closure
  
  
  #If specimen collection date is empty but available from lab section, enter date
  if (grepl("Specimen Collection Date for a positive lab test is required.", invalidConditions)) {
    
    #Store specimen collection date from lab section in pieces
    specimenCollection <- unlist(str_split(pull(provider, SpecimenCollectionDate), "/"))
    
    #Enter specimen collection date if available
    if (length(specimenCollection) > 1) {
      enter_text_na(name.is("specColDatemth"), c(specimenCollection[[1]], specimenCollection[[2]], specimenCollection[[3]]))
    }

  }

  #Exit section
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
  #Note:seeing a lot of failures here, not sure why isPageLoaded and wait_page aren't preventing them - best guess is top of page is loading before bottom so adding second isPageLoaded for lower element
  isPageLoaded(".pageDesc")
  #wait_page("Treatments")
  isPageLoaded("#STDTRMTOTHTREATFAC")
  
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
      wait_page("STD Facility Search")
      #Search random string
      enter_text("#name","zzzz")
      #Click search then cancel
      click(name.is("search"))
      click(name.is("cancel"))
      isPageLoaded("#STDTRMTOTHTREATFAC")
      
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

#helper function to extract fields from all of the details under lab result
extractLabText = function(field, lab_text = lab){
  gsub(paste0("^.*",field,":"), "", lab_text) %>%
    gsub("\\n.*$|^ ", "", .) %>%
    gsub("  ", " ", .)
  
}

#Get ordering provider info
labProcessing <- function() {
  
  #Click in Lab Tests section
  #click_link("Laboratory Tests")
  
  #Click to expand specimen info
  #click("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(7) > td:nth-child(7) > a:nth-child(1)")
  
  #facilityName <- get_text("input#orderingFacilityName")
  #facilityName <- get_text("#orderingFacilityName")
  
  #code above to go into Laboratory Test not working, instead, expand.
  
  #expand Laboratory Tests
  click("#divft-4Anchor")
  
  #Expand first test
  plus_buttons = rD$findElements(using = "css", value = "img[src$= \".PNG\"]")
  plus_lab = plus_buttons[[5]]
  plus_lab$clickElement()
  
  Sys.sleep(2)
  
  #Get lab info
  lab = get_text("#divft-4")
  #print('here')
  #print(lab)
  
  #fields to extract
  fields = c("Specimen Collection Date", "Ordering Facility Name", "Ordering Facility Address",
             "Ordering Facility Phone", "Ordering Provider Name", 
             "Ordering Provider Phone")
  
  #extract data from specified fields
  ordering_facility = sapply(fields, extractLabText, lab_text = lab)
  names(ordering_facility) %<>% gsub(" ", "", .)
  
  #address is two lines, have to extract differently
  ordering_facility["OrderingFacilityAddress"] = str_extract(lab,
                                                             pattern = "Ordering Facility Address: .*\\n.*\\n") %>%
                                                  gsub("Ordering Facility Address: ", "", .) %>%
                                                  gsub("\\n", ", ", .) %>%
                                                  gsub("\\, $", "", .) %>%
                                                  gsub("  ", " ", .)
  
  #make data frame for easy binding
  ordering_facility = data.frame(ordering_facility, stringsAsFactors = F) %>%
    t() %>%
    as.data.frame()
  
  #un-expand Laboratory Tests
  click("#divft-4Anchor")
  
  #return data
  return(ordering_facility)
  
  
}

