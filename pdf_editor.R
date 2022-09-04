## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##   Shiny App for automatically putting info into clinical PDFs              ##
##   Written by Daniel Mulder, Aug 2022                                       ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Script overview
# - Function of Script: takes blank pdf templates of clinical forms (prepared w Adobe Acrobat)
#     then puts clinical data into template and saves a new file with patient name/record number
#     as part of filename, this makes heavy use of the staplr package functionality
#     - All the input required is the patent record number and to select the req
#     - Some reqs come in packages for common multi-req workups (like IBD work up or liver dz work up)
# - Structure of Script:
#     - Load required packages
#     - Set filepaths to use throughout
#     - make lists of req templates to use
#     - load in my clinical databases
#     - UI
#       - sidepanel is patient demographics
#       - mainpanel is the req selection (a second list of available reqs appears when req type is selected)
#       - Button at bottom saves req to output folder
#     - Server
#       - load demographics function
#       - huge function for when save_req button is pressed
#         - clean up the demographic information so it can be used to put into pdf forms
#         - many blocks of code that are similar but differ slightly to update the pdfs w the patient info
#           (the first two sections are the multi-pdf packages then followed by code blocks to update individual pdfs)
#     
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

library(qpdf)
library(pdftools)
library(staplr) # this is the package w the functionality I needed, built on/w the first two

library(shiny)  # for interactive web application framework
library(tidyverse)  # for basic data organization
library(glue)  # for gluing together text
library(lubridate)  # for creating/parsing date objects
library(officer)  # for saving notes to office formats
library(readxl)  # for working with xlsx files
library(bslib)  # bootstrap themes
library(DT) # for datatables (for vlookup tabe to create interactive table)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# To Do:
# - Integrate into meddocr

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# set filepaths
billing_db_path <- "path/to/file.xlsx"
referral_db_path <- "path/to/file.csv"
templates_path <- "path/to/folder"
output_path <- "path/to/folder"


# possible req types list
req_type <- c("Bloodwork",
              "Stool Studies",
              "Imaging",
              "IBD Workup Pkg",
              "Liver Workup Pkg")

bloodwork_reqs <- c("Abdo_Pain_Bloodwork Requisition",
                    "Celiac_Monitoring_Bloodwork Requisition",
                    "IBD_serology_Public Health Ontario",
                    "Liver_Basic_Labs_Bloodwork Requisition",
                    "Liver_serology_Public Health Ontario",
                    "Liver_Workup_Bloodwork Requisition",
                    "MOH IBD Monitoring Req",
                    "MOH lab Req",
                    "Possible_IBD_Bloodwork Requisition")

ibd_workup_pkg <- c("AUS_inflammation_Imaging Req",
                    "IBD_serology_Public Health Ontario",
                    "Possible_IBD_Bloodwork Requisition",
                    "Stool HDH req",
                    "Stool_Public Health Ontario")

liver_workup_pkg <- c("AUS_liver_Imaging Req",
                      "Liver_serology_Public Health Ontario",
                      "Liver_Workup_Bloodwork Requisition")
                    
imaging_reqs <- c("AUS_inflammation_Imaging Req",
                  "AUS_liver_Imaging Req",
                  "MRE Req",
                  "Upper_GI_Series_Imaging Req")

stool_studies_reqs <- c("just fcal HDH Requisition",
                        "Stool HDH req",
                        "Stool_Public Health Ontario")


# load in the patient demographics
encounter_columns <- c("cr_number",
                       "date_of_birth",
                       "sex",
                       "my_math",
                       "service_date",
                       "admit_date",
                       "diagnosis_code",
                       "billing_diagnosis",
                       "fees",
                       "units",
                       "location",
                       "referring_provider")

billing_database <- read_excel(billing_db_path,
                               col_types = c("text", "date", "text", "text", "text", "date", "date", "text",
                                             "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text"))

referral_database <- read_csv(referral_db_path,
                              col_types = list(
                                name = col_character(),
                                date_of_birth = col_date(),
                                sex = col_character(),
                                cr = col_character(),
                                referring_provider = col_character()))

clinical_database <- bind_rows(billing_database, referral_database)
rm(billing_database, referral_database)



shinyApp(
  
  # UI Function ####
  
  ui <- fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "cyborg"),
    
    # Sidebar layout with a input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(width = 4,
                   
                   # Sidebar Inputs: ----
                   # demographic and visit information that will be loaded into note and encounter spreadsheet without having to retype info
                   titlePanel("Patient Data:"),
                   numericInput("cr_number", "Record Number:", value = 1234567, min = 0, max = 999999999),
                   # load demographic data if patient already known to me
                   actionButton("load_demographics", "Load demographics (for known patients)", icon("download")),
                   br(),
                   br(),
                   textInput("patient_name", "Patient name (First Last):", value = ""),
                   dateInput("dob", "Date of Birth (YYYY-MM-DD):", value = "2020-01-01")
                   ),
    # Mainpanel for selecting req to fill out and save ----
    mainPanel(width = 8,
              br(),
              titlePanel("Requisition:"),
              selectizeInput("req_type", "Req Type", choices = req_type, selected = NULL,
                             options = list(
                               placeholder = 'Please select an option below',
                               onInitialize = I('function() { this.setValue(""); }')
                             )),
              br(),
              br(),
              conditionalPanel(
                  condition = ("input.req_type == 'Bloodwork'"),
                  selectizeInput("req_selection_bloodwork",
                              "Bloodwork Req Selection",
                              choices = bloodwork_reqs,
                              selected = NULL,
                              options = list(
                                placeholder = 'Please select an option below',
                                onInitialize = I('function() { this.setValue(""); }')
                              ))
                ),
              conditionalPanel(
                condition = ("input.req_type == 'Stool Studies'"),
                selectizeInput("req_selection_stool",
                            "Stool Req Selection",
                            choices = stool_studies_reqs,
                            selected = NULL,
                            options = list(
                              placeholder = 'Please select an option below',
                              onInitialize = I('function() { this.setValue(""); }')
                            ))
                ),
              conditionalPanel(
                condition = ("input.req_type == 'Imaging'"),
                selectizeInput("req_selection_imaging",
                            "Imaging Req Selection",
                            choices = imaging_reqs,
                            selected = NULL,
                            options = list(
                              placeholder = 'Please select an option below',
                              onInitialize = I('function() { this.setValue(""); }')
                            ))
              ),
              br(),
              br(),
              actionButton("save_req", "Save Req", icon("download"))
    )
    )),

server <- function(input, output, session) {
  
  # Load the patient info (just using cr to look up name and dob here)
  loadDemographics <- reactive({
    if (input$cr_number %in% clinical_database$cr) {
      is_known_patient <<- TRUE
      print("Matching record found, loading prior demographic information...")
      known_patient <<- filter(clinical_database, clinical_database$cr == input$cr_number)
      known_patient_cr <<- as.character(known_patient[[5]])
      known_patient_name <<- known_patient[[1]][1]
      known_patient_dob <<- known_patient[[2]][1]
      known_patient_sex <<- known_patient[[3]][1]
      known_patient_referring_provider <<- known_patient[[13]][1]
      known_patient_billing_diagnosis <<- known_patient[[9]][1]
      print(paste0(known_patient))
      }
    })
      
  observeEvent(input$load_demographics, {
    loadDemographics()
    updateTextInput(session, "patient_name",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_name)
                      })
    updateTextInput(session, "dob",
                    value =
                      if (is_known_patient == TRUE) {
                        paste(known_patient_dob)
                      })
  })

  observeEvent(input$save_req, {
    saveReq()
    })
  
  saveReq <- reactive({
    
    req_selection <- if (input$req_type == "Bloodwork") {
      input$req_selection_bloodwork
    } else if (input$req_type == "Stool Studies") {
      input$req_selection_stool
    } else if (input$req_type == "Imaging") {
      input$req_selection_imaging
    }
    
    # filename object
    file_name <- paste0(input$patient_name, "_", input$cr_number, "_", req_selection, ".pdf")
    
    # extract first and last names
    first_name <- word(input$patient_name, 1)
    patient_name_vector <- unlist(str_split(input$patient_name, pattern = " "))
    last_name <- ifelse(
      length(patient_name_vector) <3,
      paste(patient_name_vector[2]),
      paste(patient_name_vector[-1], collapse = " ")
    )
    inverted_patient_name <- paste0(last_name, ", ", first_name)
    dob <- known_patient_dob
    cr <- known_patient_cr
    
    # ----
    
    # editing multiple reqs to include patient name, cr and date of birth for IBD Workup Pkg
    # this utilizes the staplr package function to fill in a fill-able pdf
    if (input$req_type == "IBD Workup Pkg") {
      
      imaging <- paste0(templates_path, "IBD Workup Pkg/AUS_inflammation_Imaging Req.pdf")
      imaging_fields <- get_fields(imaging)
      imaging_fields[["patient_name"]]$value <- inverted_patient_name
      imaging_fields[["cr"]]$value <- cr
      imaging_fields[["dob"]]$value <- dob
      set_fields(input_filepath = imaging,
                 output_filepath = paste0(output_path, input$patient_name, "_", input$cr_number, "_AUS_inflammation_Imaging Req.pdf"),
                 fields = imaging_fields)
      
      pub_health <- paste0(templates_path, "IBD_serology_Public Health Ontario.pdf")
      pub_health_fields <- get_fields(pub_health)
      pub_health_fields[["First Name per OHIP card"]]$value <- first_name
      pub_health_fields[["Patients Last Name per OHIP card"]]$value <- last_name
      pub_health_fields[["Date of Birth"]]$value <- dob
      pub_health_fields[["Test(s) Requested"]]$value <- "Epstein Barr Virus (EBV) - EBV VCA IgG/EA/EBNA
Epstein Barr Virus (EBV) - EBV VCA IgM
Cytomegalovirus (CMV) Culture/Early Antigen
Cytomegalovirus (CMV) IgG Immune status
Cytomegalovirus (CMV) IgG/IgM Diagnosis 
Varicella - Zoster (Chicken Pox) IgG Immune Status
Hepatitis B surface antigen (HBsAg)
Hepatitis B surface antibody (antiHBs)"
      set_fields(input_filepath = pub_health,
                 output_filepath = paste0(output_path, input$patient_name, "_", input$cr_number, "_IBD_serology_Public Health Ontario.pdf"),
                 fields = pub_health_fields)
      
      bloodwork <- paste0(templates_path, "IBD Workup Pkg/Possible_IBD_Bloodwork Requisition.pdf")
      bloodwork_fields <- get_fields(bloodwork)
      bloodwork_fields[["patient_name"]]$value <- inverted_patient_name
      bloodwork_fields[["cr"]]$value <- cr
      bloodwork_fields[["dob"]]$value <- dob
      set_fields(input_filepath = bloodwork,
                 output_filepath = paste0(output_path, input$patient_name, "_", input$cr_number, "_Possible_IBD_Bloodwork Requisition.pdf"),
                 fields = bloodwork_fields)
      
      stool <- paste0(templates_path, "IBD Workup Pkg/Stool HDH req.pdf")
      stool_fields <- get_fields(stool)
      stool_fields[["patient_name"]]$value <- inverted_patient_name
      stool_fields[["cr"]]$value <- cr
      stool_fields[["dob"]]$value <- dob
      set_fields(input_filepath = stool,
                 output_filepath = paste0(output_path, input$patient_name, "_", input$cr_number, "_Stool HDH req.pdf"),
                 fields = stool_fields)
      
      pub_health_stool <- paste0(templates_path, "IBD Workup Pkg/Stool_Public Health Ontario.pdf")
      pub_health_stool_fields <- get_fields(pub_health_stool)
      pub_health_stool_fields[["First Name per OHIP card"]]$value <- first_name
      pub_health_stool_fields[["Patients Last Name per OHIP card"]]$value <- last_name
      pub_health_stool_fields[["Date of Birth"]]$value <- dob
      pub_health_stool_fields[["Test(s) Requested"]]$value <- "Bacterial Culture and Sensitivity
Stool parasites
Clostridium difficile toxin"
      set_fields(input_filepath = pub_health_stool,
                 output_filepath = paste0(output_path, input$patient_name, "_", input$cr_number, "_Stool_Public Health Ontario.pdf"),
                 fields = pub_health_stool_fields)
      
      
    # ----
    # editing multiple reqs to include patient name, cr and date of birth for Liver Workup Pkg
    # this utilizes the staplr package function to fill in a fill-able pdf
    } else if (input$req_type == "Liver Workup Pkg") {
      
      imaging <- paste0(templates_path, "Liver Workup Pkg/AUS_liver_Imaging Req.pdf")
      imaging_fields <- get_fields(imaging)
      imaging_fields[["patient_name"]]$value <- inverted_patient_name
      imaging_fields[["cr"]]$value <- cr
      imaging_fields[["dob"]]$value <- dob
      set_fields(input_filepath = imaging,
                 output_filepath = paste0(output_path, input$patient_name, "_", input$cr_number, "_AUS_liver_Imaging Req.pdf"),
                 fields = imaging_fields)
      
      pub_health <- paste0(templates_path, "bloodwork/Liver_serology_Public Health Ontario.pdf")
      pub_health_fields <- get_fields(pub_health)
      pub_health_fields[["First Name per OHIP card"]]$value <- first_name
      pub_health_fields[["Patients Last Name per OHIP card"]]$value <- last_name
      pub_health_fields[["Date of Birth"]]$value <- dob
      pub_health_fields[["Test(s) Requested"]]$value <- "Epstein Barr Virus (EBV) - EBV VCA IgG/EA/EBNA
Epstein Barr Virus (EBV) - EBV VCA IgM
Cytomegalovirus (CMV) Culture/Early Antigen
Cytomegalovirus (CMV) IgG Immune status
Cytomegalovirus (CMV) IgG/IgM Diagnosis 
Varicella - Zoster (Chicken Pox) IgG Immune Status
Hepatitis B surface antigen (HBsAg)
Hepatitis B surface antibody (antiHBs)"
      set_fields(input_filepath = pub_health,
                 output_filepath = paste0(output_path, input$patient_name, "_", input$cr_number, "_Liver_serology_Public Health Ontario.pdf"),
                 fields = pub_health_fields)
      
      bloodwork <- paste0(templates_path, "bloodwork/Liver_Workup_Bloodwork Requisition.pdf")
      bloodwork_fields <- get_fields(bloodwork)
      bloodwork_fields[["patient_name"]]$value <- inverted_patient_name
      bloodwork_fields[["cr"]]$value <- cr
      bloodwork_fields[["dob"]]$value <- dob
      set_fields(input_filepath = bloodwork,
                 output_filepath = paste0(output_path, input$patient_name, "_", input$cr_number, "_Liver_Workup_Bloodwork Requisition.pdf"),
                 fields = bloodwork_fields)
    
      
    # ----
    # editing public health lab req to include patient name and date of birth
    # this utilizes the staplr package function to fill in a fill-able pdf
    } else if (req_selection %in% c("IBD_serology_Public Health Ontario")) {
      pub_health <- paste0(templates_path, "bloodwork/", req_selection, ".pdf")
      pub_health_fields <- get_fields(pub_health)
      pub_health_fields[["First Name per OHIP card"]]$value <- first_name
      pub_health_fields[["Patients Last Name per OHIP card"]]$value <- last_name
      pub_health_fields[["Date of Birth"]]$value <- dob
      pub_health_fields[["Test(s) Requested"]]$value <- "Epstein Barr Virus (EBV) - EBV VCA IgG/EA/EBNA
Epstein Barr Virus (EBV) - EBV VCA IgM
Cytomegalovirus (CMV) Culture/Early Antigen
Cytomegalovirus (CMV) IgG Immune status
Cytomegalovirus (CMV) IgG/IgM Diagnosis 
Varicella - Zoster (Chicken Pox) IgG Immune Status
Hepatitis B surface antigen (HBsAg)
Hepatitis B surface antibody (antiHBs)"
      set_fields(input_filepath = pub_health,
                 output_filepath = paste0(output_path, file_name),
                 fields = pub_health_fields)
      
    } else if (req_selection %in% c("Liver_serology_Public Health Ontario")) {
      pub_health <- paste0(templates_path, "bloodwork/", req_selection, ".pdf")
      pub_health_fields <- get_fields(pub_health)
      pub_health_fields[["First Name per OHIP card"]]$value <- first_name
      pub_health_fields[["Patients Last Name per OHIP card"]]$value <- last_name
      pub_health_fields[["Date of Birth"]]$value <- dob
      pub_health_fields[["Test(s) Requested"]]$value <- "Epstein Barr Virus (EBV) - EBV VCA IgG/EA/EBNA
Epstein Barr Virus (EBV) - EBV VCA IgM
Cytomegalovirus (CMV) Culture/Early Antigen
Cytomegalovirus (CMV) IgG Immune status
Cytomegalovirus (CMV) IgG/IgM Diagnosis 
Varicella - Zoster (Chicken Pox) IgG Immune Status
Hepatitis B surface antigen (HBsAg)
Hepatitis B surface antibody (antiHBs)"
      set_fields(input_filepath = pub_health,
                 output_filepath = paste0(output_path, file_name),
                 fields = pub_health_fields)
    
    
    # editing MOH lab req to include patient name and date of birth
    # this utilizes the staplr package function to fill in a fill-able pdf
    } else if (req_selection %in% c("MOH IBD Monitoring Req",
                                   "MOH lab Req")) {
      moh <- paste0(templates_path, "bloodwork/", req_selection, ".pdf")
      moh_fields <- get_fields(moh)
      moh_fields[["First Name per OHIP card"]]$value <- first_name
      moh_fields[["Patients Last Name per OHIP card"]]$value <- last_name
      moh_fields[["Date of Birth"]]$value <- dob
      print(moh_fields)
      set_fields(input_filepath = moh,
                 output_filepath = paste0(output_path, file_name),
                 fields = moh_fields)
    
      
    # editing imaging reqs to include patient name and date of birth
    # this utilizes the staplr package function to fill in a fill-able pdf
    } else if (req_selection %in% c("AUS_inflammation_Imaging Req",
                                          "AUS_liver_Imaging Req",
                                          "MRE Req",
                                          "Upper_GI_Series_Imaging Req")) {
      imaging <- paste0(templates_path, "imaging/", req_selection, ".pdf")
      imaging_fields <- get_fields(imaging)
      imaging_fields[["patient_name"]]$value <- inverted_patient_name
      imaging_fields[["cr"]]$value <- cr
      imaging_fields[["dob"]]$value <- dob
      set_fields(input_filepath = imaging,
                 output_filepath = paste0(output_path, file_name),
                 fields = imaging_fields)

    
    # editing HDH stool reqs to include patient name and date of birth
    # this utilizes the staplr package function to fill in a fill-able pdf
    } else if (req_selection %in% c("just fcal HDH Requisition",
                                    "Stool HDH req")) {
      stool <- paste0(templates_path, "stool_studies/", req_selection, ".pdf")
      stool_fields <- get_fields(stool)
      stool_fields[["patient_name"]]$value <- inverted_patient_name
      stool_fields[["cr"]]$value <- cr
      stool_fields[["dob"]]$value <- dob
      set_fields(input_filepath = stool,
                 output_filepath = paste0(output_path, file_name),
                 fields = stool_fields)

    
    # editing public health lab req to include patient name and date of birth
    # this utilizes the staplr package function to fill in a fill-able pdf
    } else if (req_selection %in% c("Stool_Public Health Ontario")) {
      pub_health_stool <- paste0(templates_path, "stool_studies/", req_selection, ".pdf")
      pub_health_stool_fields <- get_fields(pub_health_stool)
      pub_health_stool_fields[["First Name per OHIP card"]]$value <- first_name
      pub_health_stool_fields[["Patients Last Name per OHIP card"]]$value <- last_name
      pub_health_stool_fields[["Date of Birth"]]$value <- dob
      pub_health_stool_fields[["Test(s) Requested"]]$value <- "Bacterial Culture and Sensitivity
Stool parasites
Clostridium difficile toxin"
      set_fields(input_filepath = pub_health_stool,
                 output_filepath = paste0(output_path, file_name),
                 fields = pub_health_stool_fields)
    
    
    # editing HDH lab req to include patient name and date of birth
    # this utilizes the staplr package function to fill in a fill-able pdf
    } else if(req_selection %in% c("Abdo_Pain_Bloodwork Requisition",
                                   "Celiac_Monitoring_Bloodwork Requisition",
                                   "Liver_Basic_Labs_Bloodwork Requisition",
                                   "Liver_Workup_Bloodwork Requisition",
                                   "Possible_IBD_Bloodwork Requisition")) {
      bloodwork <- paste0(templates_path, "bloodwork/", req_selection, ".pdf")
      bloodwork_fields <- get_fields(bloodwork)
      bloodwork_fields[["patient_name"]]$value <- inverted_patient_name
      bloodwork_fields[["cr"]]$value <- cr
      bloodwork_fields[["dob"]]$value <- dob
      set_fields(input_filepath = bloodwork,
                 output_filepath = paste0(output_path, file_name),
                 fields = bloodwork_fields)
      }
    })
})


shinyApp(ui, server)
