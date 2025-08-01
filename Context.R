## Context embedded into Server
## By George Addo Opoku-Pare


### Infographic----
d_infographic <- tabItem(
  tabName = "infographic",
  h1("Key points infographic"),
  tags$br(),
  fluidRow(column(
    width = 12,
    tags$div(
      style = "text-align: center;",
      tags$img(src = "Infographic.png",
               style = "width:80%; height:auto;"
      )))))
### Incidence across Scotland----

d_Incidence_across_Scotland <- tabItem(
  tabName = "Incidence-across-Scotland",
  h1("Incidence Across Scotland"),
  h3("Annual Multiple Sclerosis (MS) incidence rate per 100,000 population, broken down by year and NHS board of diagnosis"), # Chart title
  fluidRow(column(width = 4, 
                  selectInput("Incidence_across_Scotland_Dropdown",
                              "Please select the NHS board you would like to view:",
                              choices = unique(sort(incidence_numbers$Hblabel, decreasing = TRUE)), # Drop down menu selection
                              multiple = FALSE))),
  fluidRow(column(12,
                  plotlyOutput("Incidence_across_Scotland_chart", height=500))), # Generate chart 
  
  br(), # line break
  p(tags$b("Please note that there is a data table at the bottom of the page, scroll down to view.")),
  h2("Incidence across Scotland"),br(),
  ### SMSR writer's text:
  p("There were 470 patients diagnosed with MS in the year 2024, which is equivalent to 8.5 people newly diagnosed with MS
    per 100,000 people in the Scottish population. There is considerable geographic variation in incidence rates of MS
    diagnosis across Scotland with the lowest incidence being found in the Western Isles with a rate of 6.9 per 100,000
    between 2020 and 2024 whilst the highest incidence continues to be in Orkney with a rate of 19.94 per 100,000 over
    the same time period. The lowest incidence on the Scottish mainland is in Dumfries and Galloway where a rate of 7.52
    per 100,000 is reported over the relevant 5-year period. The highest mainland incidence is in Central Scotland where
    NHS Forth Valley have reported 11.94 cases per 100,000."),
  p("The high rate in Orkney is unlikely to be purely due to latitudinal factors as the incidence in the Shetland islands
    is substantially lower at 9.56 per 100,000 which is more in keeping with the rest of mainland Scotland. The reason
    for this peak incidence in Orkney is debatable and may be due to genetic factors, vitamin D exposure or patterns of
    viral infection. There may also be non-biological reasons for these apparent differences. Further work would be
    required to explain this better given the limitations of data available within the SMSR."),
  p("Non-biological factors may also influence the incidence of multiple sclerosis in a region. These factors could include
    level of local access to neurological services in terms of waiting lists both for clinic appointments and for
    investigations such as MRI. Regions with better access to such resources may have a higher incidence of multiple
    sclerosis although further work would be required to test this hypothesis."),
  p("Notably, in September 2024 the proposed revisions to the McDonald Criteria for the diagnosis of Multiple Sclerosis
    were presented at the European Committee for Treatment and Research in Multiple Sclerosis (ECTRIMS) Congress. These
    new diagnostic criteria, which we expect to be formally published in the near future, are intended to make the
    diagnosis of multiple sclerosis easier and facilitate earlier diagnosis. The adoption of these diagnostic criteria
    will almost certainly increase the incidence rate of MS in Scotland, likely impacting patient pathways and clinical
    services."),
  p("The epidemiological data from the SMSR is available to help the Scottish Government prospectively plan clinical services.  
    Currently, comprehensive prevalence data for MS in Scotland is not available, however, we can refer to the MS Society’s estimate 
    which suggests that there are over 17,000 individuals with the condition in Scotland. 
    It is imperative to monitor the incidence of MS in Scotland in order to ascertain if there is an increase over the following years, 
    as this will have implications on NHS resources."),
  fluidRow(column(12, h3(textOutput("Incidence_across_Scotland_table_title")))), # table title
  fluidRow(column(12, h5(textOutput("Incidence_across_Scotland_table_subtitle")))), # table sub title
  fluidRow(column(12,downloadButton("download_Incidence_across_Scot", "Download"))),
  br(),
  fluidRow(column(12,
                  DT::dataTableOutput("Incidence_across_Scotland_table"))) # table output based on selected year
)

### Governance----
d_SNAP_Governance <- tabItem(
  tabName = "SNAP-Governance",
  h1("SNAP Governance"),
  p("The national clinical standard used by the SMSR to measure the delivery of care and support for those diagnosed with MS in Scotland is:"),
  p(tags$b("Contact with a MS specialist nurse within ten working days of diagnosis.")),
  #SMSR writer's text:
  p("The SMSR introduced funnel charts in 2019 to examine the variation in compliance with the national clinical standard at NHS board level
    against the national compliance rate."),
  p("The data for 2024 show that all the NHS boards were able to achieve the national clinical standard (93.3% patients contacted by a MS
    specialist nurse within 10 working days from day of their diagnosis), with all data points falling within the funnel limits, indicating
    that all NHS boards are within the expected range of variability. Encouragingly many NHS boards achieved, or were close to, 100%."),
  br(),
  h3("Funnel chart for compliance rate with national clinical standard - calendar year 2024"), # Chart title
  #fluidRow(column(12,h3(textOutput("SNAP_Governance_chart_title")))), # chart title
  fluidRow(column(12,
                  plotlyOutput("SNAP_Governance_chart", height=500))), # Generate chart 
  
  br(), br(),  # spacing between plot and text
  
  # Static text below the graph
  #Steve's commentary
  p("The performance of individual NHS boards against the national clinical standard for MS specialist nurses contacting patients within 10 working days of diagnosis is illustrated in this funnel plot chart.",
    "The funnel chart examines the variation in compliance of the national standard at NHS board level against the national compliance rate."),
  p("As patient numbers continue to increase, correspondingly so does the caseload per MS specialist nurse and this can cause some challenges for clinical teams. Contact with a MS specialist nurse is crucial
  after diagnosis, in guiding, commencing DMT and navigating the patient journey in managing MS symptoms and living with MS."),
  p("An explanation of the SNAP governance process can be read at this ", 
    a("link.",href = "https://publichealthscotland.scot/resources-and-tools/health-strategy-and-outcomes/scottish-national-audit-programme-snap/overview-of-the-scottish-national-audit-programme-snap/what-is-the-scottish-national-audit-programme/our-programme-governance/",
      target = "_blank",
      .noWS = "outside")),
  p("Being an outlier may be explained by differences in patient pathways, referral processes, resources, standards of care or data quality."))


### National Clinical Standard-----
d_National_Clinical_Standard <- tabItem(
  tabName = "National-Clinical-Standard",
  fluidRow(column(12,h1("National Clinical Standard"))),
  fluidRow(column(12,h3("Percentage of patients with a new diagnosis of MS contacted by MS specialist nurse within 10 working days of confirmed diagnosis"))), # Chart title
  fluidRow(column(12,h3(textOutput("National_Clinical_Standard_chart_title")))), # chart title
  fluidRow(column(12,
                  plotlyOutput("National_Clinical_Standard_chart", height=500))), # Generate chart 
  p(tags$b("Please note that there is a data table at the bottom of the page, scroll down to view.")),
  # Static text below the graph
  br(), #line break
  p("This chart displays the percentage of patients included in the SMSR who met the national clinical standard of receiving contact
  from an MS specialist nurse within ten working days of their diagnosis. These data are provided for patients diagnosed and included in
  the SMSR since its inception in 2010. Achievement of the standard has steadily improved over time, suggesting that Scottish services
  have been able to progressively offer faster post-diagnostic support to MS patients since the SMSR was launched."),
  p("Data from 2010 onwards shows that there is significant and steady improvement in the number of patients being contacted within the
    standard. In 2010, only 50% of patients met the standard, whereas by 2024, this figure had risen to 93.3%. The growing patient
    population continues to present challenges, as it proportionally increases the caseload managed by each MS specialist nurse.
    Contact with a MS specialist nurse is crucial after diagnosis, in guiding, commencing DMTs and navigating the patient journey
    in managing MS symptoms and living with MS."),
  fluidRow(column(12,h3(textOutput("National_Clinical_Standard_table_title")))),# table title
  fluidRow(column(12,h5("Note: Patients with an incomplete/missing date of confirmed diagnosis/date of birth were excluded from these data.
  Therefore total number of patients may differ from those in the incidence data"))), # table sub title
  fluidRow(column(12,downloadButton("download_Nat_Clin_Stdrd", "Download"))),
  br(),
  fluidRow(column(12,
                  DT::dataTableOutput("National_Clinical_Standard_table"))) # table
)


### Context ----


### Incidence Map -----
d_incid_Map <- tabItem(
  tabName = "Incidence-Map",
  h1("Incidence Map"),
  h3("Average annual incidence of newly diagnosed people with MS per 100,000 population, 2020-2024"), # Chart title
  p("Please use zoom and click on the icons to see the rates, please also note that there is text and a table under
  the map that may be of interest"),
  br(),
  # leaflet Output
  fluidRow(column(12,
                  leafletOutput("Incidence_Map_Chart", height=550)) # Generate chart 
           
  ),
  ##Our commentary:
  h5(paste0("The 2020-2025 mean incidence rate for Scotland is ",Scotland_Incidence_box[1,2],
            " newly diagnosed MS patients per 100,000 population.")),
  br(), # spacing between plot and text
  
  # Static text below the graph
  #Steve's and our commentary (on how to use map):
  p("This chart shows the incidence rate of those newly diagnosed with Multiple Sclerosis (MS) by 100,000 population in their NHS board of diagnosis, over the time period shown. 
Clicking on the individual NHS board icons will show the average annual incidence over the time range."),
  #caveats added by us:
  p("Notes on methodology:"),
  #text to address Matthew's
  p("Please note that the rates in this publication are not adjusted for the underlying age or sex structure of the population and this should be taken into account when 
    comparing for different areas."),
  p("NHS health board population figures from the National Records of Scotland (NRS) were used in calculating the average (mean) multiple sclerosis rates. For 2010-2023,
    the mid-year populations estimated for the 30th of June each year, from the National Records of Scotland (NRS), were used. For 2024, the Projected Population of 
    Scotland: 2022-based, published Jan 2025, from the NRS, were used."),
  br(),
  fluidRow(column(12,h3("Average annual incidence of newly diagnosed people with MS per 100,000 population, 2020-2024"))), 
  # table title
  fluidRow(column(12,downloadButton("download_map_incidence_table", "Download"))),
  br(),
  fluidRow(column(12,
                  DT::dataTableOutput("map_incidence_table")))
)

### Incidence Rate: Age and Gender ----

d_incid_age_sex <- tabItem(
  tabName = "incidence-age-and-sex", # tab name
  h1("Incidence Rate: Age & Sex"), # page title
  h3("Average annual age-specific incidence of people diagnosed with MS in Scotland per 100,000 population, by sex, 2010-2024"),
  br(), # line break
  fluidRow(column(4, 
                  selectInput("Incid_age_sex_Dropdown", # dropdown menu name
                              "Please select which view you would like to see:", # dropdown menu text
                              choices = c("Age" = "Age",
                                          "Sex and year of birth" = "Sex and year of birth"), # dropdown menu selection
                              multiple = FALSE))),
  
  fluidRow(column(12,
                  plotlyOutput("Incid_age_sex_chart", height=500))), # chart
  br(), #line break
  #Steve's comment
  p("This chart shows the rate of MS diagnosis for all age groups (per 100,000 population) and sex and year of birth 
      (number), since the SMSR began in 2010, until the current reporting year."),
  #SMSR writer's text.
  p("For all people, MS is most likely to be diagnosed in Scotland between the ages of 30 and 39 with the highest
      overall incidence rate between the ages of 30 and 34. Incidence of multiple sclerosis is higher in women than
      in men, across a wide range of ages. In women, the peak incidence rate is 28.3 cases per 100,000 between 30 and
      34 compared to men who have a later peak incidence rate of 12.67 per 100,000 between the ages of 40 and 44.  
      The reasons for these sex differences in peak incidence and peak age at diagnosis remain unclear. The mean
      age at diagnosis remains 41 with a median of 40.  The inclusion of paediatric cases has not altered this.  
      Scotland continues to report a higher average age of MS diagnosis compared to the rest of the world. Interestingly 29.6% of 
      those diagnosed with multiple sclerosis after the age of 60 have relapsing remitting multiple sclerosis."),
  p(HTML(paste("Since its inception, the SMSR has held data on 5092 women and 2233 men with sex data for 8 individuals not recorded.
      This gives a ratio of approximately 2.28 women to 1 man diagnosed with MS. This is roughly comparable to data 
      reported by the Swedish MS register",
               tags$sup("10"),
               " in 2014, although slightly higher sex ratios are often observed. It is often suggested that the Female: Male
      ratio for MS diagnosis is increasing, but this is difficult to confirm as men tend to have a later peak incidence
      of diagnosis."))),
  p("Notes on methodology:"),
  p("Average annual incidence within each age group was calculated using the mean mid-year gender population
    estimates for 2010-2023 published by the NRS.",
    "Average annual incidence within each age group used for 2024 was calculated using the mean mid-year gender population
    projections 2022-based, published Jan 2025, from the NRS.",
    "Cases for which age at diagnosis could not be calculated were excluded from the data presented."),
  br(), #line break
  fluidRow(column(12,h3(textOutput("Incid_age_sex_table_title")))), # table title
  fluidRow(column(12,downloadButton("download_age_sex_Table", "Download"))),
  br(),
  fluidRow(column(12,
                  DT::dataTableOutput("Incid_age_sex_table"))) # table
  
)

### Deprivation----
d_deprivation <- tabItem(
  tabName = "Deprivation",
  h1("Deprivation"),
  h3("MS registration rate by Scottish Index of Multiple Deprivation (SIMD) quintiles"),
  
  fluidRow(
    column(4, 
           selectInput("Deprivation_HB_Dropdown",
                       "Please select the NHS board you would like to view:",
                       choices = unique(sort(deprivation$hb, decreasing = TRUE)), 
                       multiple = FALSE)),
    column(5, 
           selectInput("Deprivation_year_Dropdown",
                       "Please select the calendar year:",
                       choices = unique(sort(deprivation$diag_year, decreasing = TRUE)), 
                       multiple = FALSE))
  ),
  
  fluidRow(column(12,
                  plotlyOutput("Deprivation_chart", height=500))), # Generate chart 
  br(), #line break
  p("This chart shows the rate of MS diagnoses per 100,000 NHS board population by Scottish
    Index of Multiple Deprivation (SIMD) quintile. You can read more on SIMD and SIMD quintiles ",
    a("here.",href = "https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/",
      target = "_blank",
      .noWS = "outside")),
  p("If no MS diagnoses are recorded within a particular quintile, the corresponding bar in the chart will appear empty."),
  p("The pattern of socioeconomic gradients seen in new multiple sclerosis (MS) diagnoses across Scotland exhibit
    inconsistency over time. Data from 2024 reveal a socioeconomic gradient, with less deprived population sub-groups
    generally receiving more MS diagnoses than their more deprived counterparts. However, this pattern shows significant
    variation by health board and year, often due to lower numbers of MS registrations, and sometimes none, within particular deprivation quintiles."),
  p("To address these issues, future reports will present these data in five-year bands. This approach aims to provide a more accurate and comprehensive 
    understanding of the socioeconomic factors influencing MS diagnoses across Scotland. By adopting a longitudinal perspective, we aim to identify
    persistent trends and variations, thereby offering valuable insights for healthcare planning and resource allocation."),
  br(),
  fluidRow(column(12,h3("MS registration rate by Scottish Index of Multiple Deprivation (SIMD) quintiles"))),
  fluidRow(column(12,downloadButton("download_deprivation_table", "Download"))),
  br(),
  fluidRow(column(12,
                  DT::dataTableOutput("deprivation_table")))
)


### Investigations----
d_investigations <- tabItem(
  tabName = "Investigations",
  h1("Investigations"),
  h3("Investigations split by NHS board (of diagnosis) and year of diagnosis"), # Chart title
  
  fluidRow(column(4, 
                  selectInput("Investigations_Year_Dropdown",
                              "Please select which year you would like to view:",
                              choices = unique(sort(investigations$year, decreasing = TRUE)), # Drop down menu selection
                              multiple = FALSE)),
           column(width = 5, 
                  selectInput("Investigations_Dropdown",
                              "Please select which type of investigation(s) you would like to view:",
                              choices = unique(sort(investigations$Ind, decreasing = TRUE)), # Drop down menu selection
                              multiple = FALSE))),
  fluidRow(column(12,
                  plotlyOutput("Investigations_chart", height=500))), # Generate chart 
  p(tags$b("Please note that there is a data table at the bottom of the page, scroll down to view.")),
  br(), #line break
  #Steve's descriptive commnetary:
  p("Investigations to diagnose MS are commonly: spinal MRI (SPMRI) and brain MRI (BRAINMRI), lumbar puncture (LUMPUNC)
  and the detection of oligoclonal bands (OLIBANDS) in the cerebrospinal fluid (CSF). 
This chart shows a percentage of each test reported by health board of diagnosis, available by year of diagnosis.   
The all Scotland average percentage is included for each diagnostic investigation in each year."),
  #SMSR report writer's text
  p("Most NHS boards reported adequate access to investigations to confirm the diagnosis of multiple sclerosis. A
      comprehensive diagnostic evaluation for MS include MRI of the brain and cervico-thoracic spinal cord which were
      routinely performed in most centres. Having a baseline MRI of the brain and cervico-thoracic spine may facilitate
      better monitoring of the disease’s progression over time. These neuroimaging techniques are essential tools to
      diagnose MS and to confirm dissemination in place and time, and also to monitor disease activity. However, there
      is a necessity to standardise the MRI sequences and protocols at the time of diagnosis across all NHS boards. The
      new 2024 MS diagnostic criteria (unpublished at the time of this review) will incorporate new MRI sequences and
      radiological signs including the analysis of paramagnetic rim lesions (PRLs) and the central venous sign (CVS),
      that will help at the time of diagnosis. For this reason, these new techniques should be incorporated into
      future diagnostic evaluations."),
  p("The NHS boards also reported performing lumbar punctures to analyse the presence of unmatched/ exclusive
      oligoclonal bands (OCBs) in the cerebrospinal fluid (CSF) on average in around 70-80% of MS patients. It is
      estimated that around 85%-90% of people with MS have exclusive or isolated OCBs in the CSF at the time of diagnosis.
      The analysis of OCBs is commonly used as a substitute for “dissemination in time” criteria in people presenting with
      a first clinical demyelination event and demyelination lesions on the baseline MRI. The detection of isolated OCBs
      makes the diagnosis of MS more likely in a person with a suggestive clinical picture and presence of demyelinating
      lesions on MRI."),
  br(), #line break
  fluidRow(column(12,h3(textOutput("Investigations_table_title")))), # table title
  fluidRow(column(12,downloadButton("download_investigations_table", "Download"))),
  br(),
  fluidRow(column(12,
                  DT::dataTableOutput("Investigations_table"))) # table
)

### Mortality-----
d_mortality <- tabItem(
  tabName = "Mortality",
  h1("Mortality"),
  h3("Number of death registrations in Scotland mentioning multiple sclerosis by calendar year"), # Chart title
  
  fluidRow(column(12,h3(textOutput("Mortality_chart_title")))), # chart title
  fluidRow(column(12,
                  plotlyOutput("Mortality_chart", height=500))), # Generate chart 
  #br(), # spacing between plot and text
  p(tags$b("Please note that there is a data table at the bottom of the page, scroll down to view.")),
  br(),
  # Static text below the graph
  #Steve's commentary
  p("We cannot calculate a mortality rate as we do not have a confirmed prevalence of MS in Scotland, the SMSR holds only data on those diagnosed
      with MS since 2010 when the registry began."), 
  p("This chart is produced by examining how many death certificates have mentioned MS and how many individuals in the
    SMSR have died. It is important to note that these figures may well be an underestimate as it is likely that some death
      certificates issued for people with MS who have died, may not mention the diagnosis of MS."), 
  #SMSR Writer's text:
  p("We have reported an estimate of how many people with MS died in each year, based on SMSR and death certificate data."),
  p("As the precise number of people with MS in Scotland is not known, the presented figures do not take into account the
      overall population of people with MS living in Scotland. It is possible, as discussed under the ‘time to diagnosis section’
      that wider issues relevant to MS diagnosis (such as access to neurology clinics or to MRI scanners) may have the effect of 
      increasing the number of known MS cases."),
  p("Our data show that in the year 2024, 313 deaths of people with MS were identified. There has been an apparent year-on-year decrease in the
      number of deaths per annum identified since 2021, though this should be viewed within the context discussed above which makes it impossible
      to report if death rates are truly decreasing."),
  p("Notes on methodology:"),
  p("The data used in this analysis have been derived through Community Health Index (CHI) linkage between the SMSR and National Records Scotland
      (NRS) death records, and by selecting death records for patients with MS listed on their death certificate using the International
      Classification of Disease-10 (ICD10) code ‘G35’. The number of deaths therefore includes the deaths of patients who are not listed on SMSR."),
  p("Average annual incidence within each age group was calculated using the mean mid-year gender population estimates for 2010-2023 published by
      the NRS. Average annual incidence within each age group used for 2024 was calculated using the mean mid-year gender population projections
      2022-based, published Jan 2025, from the NRS."),
  br(),
  fluidRow(column(12,h3("Number of Deaths registrations in Scotland mentioning MS"))),
  fluidRow(column(12,downloadButton("download_deaths_table", "Download"))),
  br(),
  fluidRow(column(12,DT::dataTableOutput("Deaths_table")))
)


d_mortalityCause <- tabItem(
  tabName = "MortalityCauses",
  h1("Mortality"),
  h3("Most commonly listed causes of death alongside multiple sclerosis in 5 year groups"), # Chart title
  fluidRow(column(4, 
                  selectInput("MS_Deaths_Cause_Dropdown", # dropdown menu name
                              "Please select which time period you would like to see:", # dropdown menu text
                              choices = sort(unique(Top_five_causes$TimePeriod),decreasing = TRUE),
                              #choices = unique(Top_five_causes$TimePeriod), # dropdown menu selection
                              multiple = FALSE))),
  
  fluidRow(column(12,
                  plotlyOutput("Mortality_cause_chart", height=500))), # Generate chart 
  p(tags$b("Please note that there is a data table at the bottom of the page, scroll down to view.")),
  br(),
  #Steve's commentary
  p("When someone with MS dies in Scotland, the causes of death that are most commonly recorded alongside the diagnosis of MS, over the past 5 years,
     are shown in this chart."),
  p("Although we do not know to what extent these data accurately reflect the cause of death, as opposed to a contributing condition, these data
       would be compatible with the majority of deaths in people with MS being caused by infection (pneumonia or other types of infection)
       or aspiration."),
  p("Swallowing problems (dysphagia) are frequently seen in people with MS, particularly in the more advanced stages of MS. Aspiration
       (when food, drink or saliva reaches the lungs, causing inflammation or infection) can sometimes be fatal. While attentive care can
       reduce the risk of aspiration, it cannot cannot remove the risk entirely."),
  #SMSR writer's text:
  p("Causes of death listed alongside MS over a 5 year period from 2020-2024 included ‘pneumonitis due to inhalation of food and vomit’
       (commonly referred to as aspiration pneumonia) (281 deaths), ‘pneumonia unspecified’ (188 deaths), sepsis due to unspecified organism
       (149 deaths) and COVID-19 (where the infection was confirmed) (200 deaths). There is substantial clinical overlap between these stated 
       causes of death, for instance an aspiration pneumonia might be recorded either as ‘pneumonitis due to inhalation of food and vomit’, 
       or ‘pneumonia’, and in turn could cause sepsis. Interpretation of the number of most commonly listed causes of death is therefore
       challenging. The data do however reflect that aspiration pneumonia, lung infection and infection more generally, are commonly recorded
       as causes of death in people with multiple sclerosis."),
  p("The number of deaths associated with laboratory-confirmed Covid-19 infection was 65 in the year first recorded (2020), decreasing
      year on year to 14 in the year 2024. These trends are in keeping with decreasing mortality directly linked to Covid-19 infection in
      Scotland as a whole.  Further Scottish COVID-19 data can be viewed at the following PHS ", 
    a("website.",href = "https://scotland.shinyapps.io/phs-respiratory-covid-19/",
      target = "_blank",
      .noWS = "outside")),
  p("Notes on methodology:"),
  p("The data used in this analysis has been derived through CHI linkage between the SMSR and NRS death records, and by selecting death
      records for patients with MS listed on their death certificate using the ICD-10 code 'G35'. The number of deaths therefore includes
      the deaths of patients who are not listed on SMSR."),
  br(),
  fluidRow(column(12,h3("Most commonly listed causes of death alongside MS"))),
  fluidRow(column(12,downloadButton("download_death_causes_table", "Download"))),
  br(),
  fluidRow(column(12,DT::dataTableOutput("death_causes_table")))
)

### Weeks from confirmed diagnosis to first contact----
d_confirmedDiagnosis <- tabItem(
  tabName = "Weeks-from-confirmed-diagnosis-to-first-contact",
  h1("Weeks from confirmed diagnosis to first contact"),
  h3("Percentage of people newly diagnosed with MS in Scotland, by number of weeks from confirmed diagnosis to first contact with a MS specialist nurse"), # Chart title
  
  fluidRow(column(width = 4, 
                  selectInput("Weeks_from_Diagnosis_Year_Dropdown",
                              "Please select which year you would like to view:",
                              choices = unique(sort(diag2cont$year, decreasing = TRUE)), # Drop down menu selection
                              multiple = FALSE))),
  fluidRow(column(12,
                  plotlyOutput("Weeks_from_Diagnosis_chart", height=500))), # Generate chart 
  p(tags$b("Please note that there is a data table at the bottom of the page, scroll down to view.")),
  br(), #line break
  # SMSR writer's text
  p("Once the diagnosis of MS is made by a specialist neurologist, the MS specialist nurses play a crucial role in
      supporting patients to understand their diagnosis, how it will be managed and in their ongoing journey of living
      with MS. For some patients, the initial diagnosis can be bewildering and frightening and some may not have access
      to appropriate information. Different patients will require different levels of support in their personal, social,
      and occupational lives. Patients will often experience various emotions of denial, anger, frustration, and anxiety.
      Early contact with a MS specialist nurse is crucial in supporting and guiding patients through this difficult time.
      Early contact also helps facilitate treatment initiation. This national clinical standard is useful to assess how
      services and pathways in the different NHS boards across Scotland vary."),
  #caveats
  p("Notes on methodology:"),
  p("These data exclude patients that have refused contact with a MS specialist nurse and cases for which we hold a
      missing year of diagnosis over the period 2010-2024."),
  br(), #line break
  fluidRow(column(12,h3(textOutput("Weeks_from_Diagnosis_table_title")))), # table title
  fluidRow(column(12,downloadButton("download_weeksfromDiag_table", "Download"))),
  br(),
  fluidRow(column(12,
                  DT::dataTableOutput("Weeks_from_Diagnosis_table"))) # table
)

### Months from primary referral to confirmed diagnosis ----
d_primrefDiag <- tabItem(
  tabName = "Months-from-primary-referral-to-confirmed-diagnosis",
  h1("Months from primary referral to confirmed diagnosis"),
  h3("Percentage of people newly diagnosed with MS in Scotland, 
       by number of months from primary referral to confirmed diagnosis"), # Chart title
  (column(width = 4, 
          selectInput("primref_Diag_Year_Dropdown",
                      "Please select which year you would like to view:",
                      choices = (sort(unique(primref2diag$year, decreasing = TRUE))), # Drop down menu selection
                      selected = 2024,
                      multiple = FALSE))),
  fluidRow(column(12,h3(textOutput("primref_Diag_chart_title")))), # chart title
  fluidRow(column(12,
                  plotlyOutput("primref_Diag_Chart", height=500))), # Generate chart 
  p(tags$b("Please note that there is a data table at the bottom of the page, scroll down to view.")),
  br(), #line break
  
  #SMSR Writer's text:
  p("It can be seen from this chart that a significant proportion of people with MS are still waiting more than six 
      months from the primary referral to a confirmed diagnosis. Concerns should be raised regarding the group of 
      people that have experienced a delay of more than one year from time of referral to diagnosis. Several factors
      are thought to be contributing to delays in the diagnosis MS. These include:",
    tags$ul(
      tags$li("limited access to neurologists"), 
      tags$li("low rate of neurology and MS specialists per head of population"), 
      tags$li("neurology clinic waiting times"),
      tags$li("delayed access to diagnostic tests (e.g., MRI of brain and spine, and lumbar puncture)")
    )),
  p("In addition, other clinical factors may have a significant role here, including the fact that progressive
      forms of MS (in absence of relapses) and late onset MS, may be more difficult to diagnose quickly. Data are
      not currently available to understand if this delay in the diagnosis is more pronounced in relapsing/ remitting
      or progressive forms of MS."),
  p(HTML(paste("The SMSR did not capture specific data about the time from primary referral to first neurological consultation,
      and neither from the first neurological assessment to the confirmation of the diagnosis of MS. Despite this,
      there is a window of opportunity to reduce these specific waiting times. A delay in the diagnosis is associated 
      with a significant risk of disability progression.",tags$sup("11")))),
  p("This finding demonstrates an opportunity for NHS Scotland and local NHS boards to improve MS outcomes by
      reducing time to diagnosis. This might be achieved through; increasing awareness of MS symptoms and presentations,
      improving access to neurology services, MS specialists, and MS teams, and improving waiting times for diagnostic
      tests."),
  p("Delayed diagnosis is linked with delayed start to appropriate DMT to control disease activity and as a
      consequence, treatment naïve individuals or those with undertreated MS, may be at increased risk of disability
      progression."),
  #caveats
  p("Notes on methodology:"),
  p("Primary referral data collection started in 2019.",
    "These data exclude patients that have refused contact with a MS specialist nurse and cases for which we hold a missing year of diagnosis over the period 2010-2024.",
    "The ‘missing’ group includes some patients with date of primary referral not recorded as the patient was already under the care of neurology."),
  br(), #line break
  fluidRow(column(12,h3(textOutput("primref_Diag_table_title")))), # table title
  fluidRow(column(12,downloadButton("download_primref_Diag_table", "Download"))),
  br(),
  fluidRow(column(12,
                  DT::dataTableOutput("primref_Diag_table"))) # table
)

### DMT----

d_results_DMT <- tabItem(
  tabName = "results-DMT",
  h1("Disease modifying treatments (DMTs)"), # page title
  # dynamic chart title
  fluidRow(column(12,h3(textOutput("Chart_DMT_title")))),
  fluidRow(column(4, 
                  selectInput("Results_DMT_Dropdown", # dropdown menu name
                              "Please select which view you would like to see:", # dropdown menu text
                              choices = c("DMTs discussed at the time of diagnosis" = "DMTs discussed at the time of diagnosis",
                                          "DMTs by type of MS" = "DMTs by type of MS"), # dropdown menu selection
                              multiple = FALSE))),
  
  #chart
  fluidRow(column(12,
                  plotlyOutput("Results_DMT_chart", height=500))), # chart
  p(tags$b("Please note that there is a data table at the bottom of the page, scroll down to view.")),
  br(), #line break
  #add dymamic text description
  uiOutput("DMT_Text"),
  br(),
  fluidRow(column(12,h3(textOutput("Results_DMT_title")))), # table title
  # Button
  fluidRow(column(12,downloadButton("download_DMT_Table", "Download"))),
  br(),
  fluidRow(column(12,
                  DT::dataTableOutput("Results_DMT_table"))) # table
)


### Quality Improvement - Research -----
# Anne Rowling Regenerative Neurology Clinic

d_anne_rowling <- tabItem(
  tabName = "Anne-Rowling-Regenerative-Neurology-Clinic",
  h2("Anne Rowling Regenerative Neurology Clinic"),
  tags$iframe(
    src = "Anne Rowling RNC.pdf#zoom=100",
    style = "height:90vh; width:100%; border:none; margin-left: -5%;",
    frameborder = "0",
    scrolling = "yes"
  )
)

d_revive_ms <- tabItem(
  tabName = "Revive-MS-Support",
  h2("Revive MS Support"),
  tags$iframe(
    src = "Revive MS Support.pdf#zoom=100",
    style = "height:90vh; width:100%; border:none; margin-left: -5%;",
    frameborder = "0",
    scrolling = "yes"
  )
)

### Conclusion----
### Acknowlegements -----
d_acknowledgements <- tabItem(
  tabName = "Acknowledgements",
  h1("Acknowledgements"),   
  br(),
  fluidRow(column(12,
                  tableOutput("acknowledgements_table")))
)

### Contact -----
d_contact<- tabItem(
  tabName = "Contact",
  h1("Contacts"), 
  br(),
  p(tags$b("Niall MacDougall, "), 
    "Chair, Scottish Multiple Sclerosis Register, ", 
    a("phs.scottishmsregister@phs.scot", 
      href = "mailto:phs.scottishmsregister@phs.scot")),
  
  br(),
  p(tags$b("Stephen Marjoribanks, "),
    "National clinical co-ordinator/ senior nurse, ", 
    "Scottish National Audit Programme, Public Health Scotland, ", a("phs.scottishmsregister@phs.scot", 
                                                                     href = "mailto:phs.scottishmsregister@phs.scot")),
  br(),
  p(tags$b("Rita Sá Nogueira, "),
    "Senior Information Analyst, ", 
    "Scottish National Audit Programme, Public Health Scotland, ", 
    a("phs.scottishmsregister@phs.scot", 
      href = "mailto:phs.scottishmsregister@phs.scot")),
  br(),
  p(tags$b("George Addo Opoku-Pare, "),
    "Information Analyst, ", 
    "Scottish National Audit Programme, Public Health Scotland, ", 
    a("phs.scottishmsregister@phs.scot", 
      href = "mailto:phs.scottishmsregister@phs.scot")),
  br(),
  h2("Media Media enquiries"),
  p("If you have a media enquiry relating to this publication, please contact the ",
    a("Communications and Engagement team.", 
      href = "https://publichealthscotland.scot/contact-us/media-enquiries/",
      target = "_blank",
      .noWS = "outside"),
    p(
      a("phs.comms@phs.scot",
        href = "mailto:phs.comms@phs.scot")))
)

### Glossary-----
d_glossary <- tabItem(
  tabName = "Glossary",
  h1("Glossary"),   
  br(),
  fluidRow(column(12,
                  DT::dataTableOutput("Glossary_table")))
)

### Data Sources ----
d_dataSources <- tabItem(
  tabName = "Data-sources",
  h1("Data sources"),
  br(),
  p("Scottish Multiple Sclerosis Register"),
  br(),
  p("Scottish Morbidity Records/ General Records Office."),
  br(),
  p("The Scottish Index of Multiple Deprivation, Scottish Government")
)