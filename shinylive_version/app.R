library(shiny)
library(shinydashboard)
#library(shinyalert)
#library(shinyBS)
#library(rmarkdown)
library(DT)
library(jsonlite)

#In-App Datasets

input_values <- data.frame(rbind(
  c(element = "1A",	
    name = "1A_1",	
    desc = "Study type, objective, and scope",
    tooltip = "State the type and scope of the study, including specific study objectives, hypotheses, and/or questions intended to be addressed by the study. State also how often this method/study is intended to be used – specifically, is it meant for ad hoc (i.e., emergency, one-off) analysis or for routine analyses?",	
    type = "textarea",	
    value = "enter text",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "For example, study types/goals might include hypothesis generation, sample exploration or chemical discovery, investigation of a specific research question, or investigation of questions with legal or other regulatory implications. For more examples, go to Table 1.1. Frequency of method use will affect the extent of method optimization, which will be planned further in Section 2B.4. "
    ),
  c(element = "1A",	
    name = "1A_2",	
    desc = "Analysis approach",
    tooltip = "State whether the study approach will include non-targeted, suspect screening, targeted (quantitative) analysis, and/or qNTA. Will any complementary data sets (i.e., from non-MS instrumentation) be used and/or generated during the study?",	
    type = "textarea",	
    value = "State whether the study approach will include non-targeted, suspect screening, targeted (quantitative) analysis, and/or qNTA. Will any complementary data sets (i.e., from non-MS instrumentation) be used and/or generated during the study?",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "For example, study types/goals might include hypothesis generation, sample exploration or chemical discovery, investigation of a specific research question, or investigation of questions with legal or other regulatory implications. For more examples, go to Table 1.1. Frequency of method use will affect the extent of method optimization, which will be planned further in Section 2B.4. "
  ),
  c(element = "1A",	
    name = "1A_3",	
    desc = "Chemical space",
    tooltip = "State the intended chemical space for the study, and potential limitations.",	
    type = "textarea",	
    value = "State the intended chemical space for the study, and potential limitations.",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Approaches for evaluating the chemical space of the analytical method and the planned data analysis approaches remain limited. The ChemSpaceTool remains under development, but future updates to the SPT will enable exporting information provided in the SPT for import to the ChemSpaceTool. For more information see Ref Content."
  ),
  c(element = "1A",	
    name = "1A_4",	
    desc = "Target audience",
    tooltip = "State the intended audience of the results of the study, and how the study results will be communicated (e.g., internal report, peer-reviewed publication, presentation, etc.). ",	
    type = "textarea",	
    value = "State the intended audience of the results of the study, and how the study results will be communicated (e.g., internal report, peer-reviewed publication, presentation, etc.). ",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Consider how different audiences may need the results to be communicated. Different communication platforms may also impact the ways in which data are generated, analyzed, and presented."
  ),
  c(element = "2A",	
    name = "2A_1",	
    desc = "Sample type",
    tooltip = "Describe the samples to be collected, including the physical state (liquid, gas, solid) and types (e.g., soil, sediment, groundwater).",	
    type = "input table",	
    value = "Sample Name; Physical State; Type; Relevant Collection Procedure",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Describe the samples to be collected, including the physical state (liquid, gas, solid) and types (e.g., soil, sediment, groundwater)."
  ),
  c(element = "2A",	
    name = "2A_2",	
    desc = "Sample collection",
    tooltip = "Describe the procedures for collecting the samples described above. Include details such as frequency of collection, sampling method (e.g., use of grab, composite, passive samples, or a combination thereof; how/if samples will be composited), the sampling equipment, the containers/materials that will be used to collect the samples, and any other relevant details.",	
    type = "input table",	
    value = "Collection Procedure Name;Sampling Method;Sampling Equipment;Containers/Materials;Sampling Frequency;Other Details",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Describe the procedures for collecting the samples described above. Include details such as frequency of collection, sampling method (e.g., use of grab, composite, passive samples, or a combination thereof; how/if samples will be composited), the sampling equipment, the containers/materials that will be used to collect the samples, and any other relevant details."
  ),
  c(element = "2A",	
    name = "2A_3",	
    desc = "Sample quantity & replicates",
    tooltip = "Describe the sample quantities (i.e., amount per sample and the total number of samples) and the use of replicate samples (e.g., from the same sample location, time, phenotype, or other variable). Will the same sample be analyzed multiple times (e.g., analytical/technical replicate)?",	
    type = "input table",	
    value = "Collection Procedure Name;Sampling Method;Sampling Equipment;Containers/Materials;Sampling Frequency;Other Details",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Describe the sample quantities (i.e., amount per sample and the total number of samples) and the use of replicate samples (e.g., from the same sample location, time, phenotype, or other variable). Will the same sample be analyzed multiple times (e.g., analytical/technical replicate)? NOTE: Consider whether additional quantity of sample will need to be collected for QA/QC samples that are defined in Standards, Calibrants, Replicates, Blanks, and QC Spikes & Samples (Section 3)."
  ),
  c(element = "2A",	
    name = "2A_4",	
    desc = "Sampling metadata",
    tooltip = "Describe how you will record, store, and manage information regarding the sample description, collection, handling, and storage.",	
    type = "textarea",	
    value = "Describe how you will record, store, and manage information regarding the sample description, collection, handling, and storage.",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Describe how you will record, store, and manage information regarding the sample description, collection, handling, and storage."
  ),
  c(element = "2A",	
    name = "2A_5",	
    desc = "Overall sampling design",
    tooltip = "Reconsider the sampling plan described above and evaluate whether it will address the intended study objectives.  State any necessary special considerations, including any key and/or confounding variables that might impact your study conclusions.",	
    type = "textarea",	
    value = "Reconsider the sampling plan described above and evaluate whether it will address the intended study objectives.  State any necessary special considerations, including any key and/or confounding variables that might impact your study conclusions.",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Reconsider the sampling plan described above and evaluate whether it will address the intended study objectives.  State any necessary special considerations, including any key and/or confounding variables that might impact your study conclusions."
  ),
  c(element = "2B",	
    name = "2B_1",	
    desc = "Citation for sample preparation method (optional)",
    tooltip = "State URL or DOI for method citation, if your sample preparation method is based on a previously reported study or reference method.",	
    type = "text",	
    value = "State URL or DOI for method citation, if your sample preparation method is based on a previously reported study or reference method. ",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State URL or DOI for method citation, if your sample preparation method is based on a previously reported study or reference method."
  ),
  c(element = "2B",	
    name = "2B_2",	
    desc = "Sample preparation method",
    tooltip = "Whether the preparation method is novel or is a modified version of the above-cited method, state all steps between an unmodified sample and producing a final sample extract that is ready for instrumental analysis. Specify the details corresponding to each method step. ",	
    type = "input table",	
    value = "Sample Preparation Method Step;Equipment/materials used;Solvents or other chemicals used; duration",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Whether the preparation method is novel or is a modified version of the above-cited method, state all steps between an unmodified sample and producing a final sample extract that is ready for instrumental analysis. Specify the details corresponding to each method step.Example preparation method steps: sample extraction method (liquid-liquid extraction, solid phase extraction, etc.), enrichment/concentration, reconstitution, filtering, centrifugation, derivatization, addition of reference standards, etc. Example method details: solvents or other chemicals used, duration of each step, equipment settings, cartridge/filter type, etc."
  ),
  c(element = "2B",	
    name = "2B_3",	
    desc = "Sample & sample extract hold time and preservation",
    tooltip = "State the length of time for holding the sample and/or the sample extract, including any preservation protocols or conditions used to ensure sample and extract stability.",	
    type = "input table",	
    value = "Stage of Sample Prep Method;Sample or Extract;Hold Time & Conditions;Preservation Method",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State the length of time for holding the sample and/or the sample extract, including any preservation protocols or conditions used to ensure sample and extract stability."
  ),
  c(element = "2B",	
    name = "2B_4",	
    desc = "Sample preparation method evaluation and/or optimization",
    tooltip = "Were any of the sample preparation protocols, hold times, or preservation methods previously evaluated to determine impacts on recovery/stability of the chemicals of interest in the study? Will any part of the sample preparation protocols, hold times, or preservation methods need to be optimized to achieve the goals of the study? Establish an experimental plan for parameter optimization. ",	
    type = "input table",	
    value = "Stage of Sample Prep Method;Previous Evaluation or Needed Optimization;Evaluation Results/Optimization Plan;Expected impact on recovery/stability of the chemical of interest",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Were any of the sample preparation protocols, hold times, or preservation methods previously evaluated to determine impacts on recovery/stability of the chemicals of interest in the study? Will any part of the sample preparation protocols, hold times, or preservation methods need to be optimized to achieve the goals of the study? Establish an experimental plan for parameter optimization. These questions are meant to inform your understanding of the overall observable chemical space in the study. All method choices will inherently bias the observable chemical space in some way."
  ),
  c(element = "3A",	
    name = "3A_1",	
    desc = "Analytical Standards (Unlabeled & Isotopically Labeled)",
    tooltip = "",	
    type = "input table",	
    value = "Compound Name;Chemical Identifier;Supplier;Stock Solution;Concentration in Stock Solution [units];Concentration in Sample [units]; Concentration in Final Extract[units];Paired Isotopically Labeled Standard (if used)",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "In the Table of Analytical Standards (Table 3.1) and Table of Stock Solutions (Table 3.2), document information about all analytical standards and stock solutions that will be used in the study. Note that columns with an asterisk (*) in the header name are not essential at the study planning stage, but include details that must eventually be collated for use in a final report and may be useful during study planning (e.g., selecting a chemical supplier based on availability and price). "
  ),
  c(element = "3A",	
    name = "3A_2",	
    desc = "Stock Solutions",
    tooltip = "",	
    type = "input table",	
    value = "Stock Solution Name; Stock Solution Solvent;Stock Solution Preparation Method;Volume of Spike[units];Spike Timing;Samples to which Stock Solution Spike will be added",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "In the Table of Analytical Standards (Table 3.1) and Table of Stock Solutions (Table 3.2), document information about all analytical standards and stock solutions that will be used in the study. Note that columns with an asterisk (*) in the header name are not essential at the study planning stage, but include details that must eventually be collated for use in a final report and may be useful during study planning (e.g., selecting a chemical supplier based on availability and price). "
  ),
  c(element = "3A",	
    name = "3A_3",	
    desc = "Standards, Calibrants, Sample Replicates, Blanks, QC Spikes, and QC Samples",
    tooltip = "",	
    type = "input table",	
    value = "Standard, Calibrant, Replicate, Blank, QC Sample or QC Spike Name;Intended Use;Sample Creation Method;Sample Creation Frequency;Sample Analysis Frequency;Performance Metric(s);Performance Calculation(s);Performance Criteria",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "In the Table of Analytical Standards (Table 3.1) and Table of Stock Solutions (Table 3.2), document information about all analytical standards and stock solutions that will be used in the study. Note that columns with an asterisk (*) in the header name are not essential at the study planning stage, but include details that must eventually be collated for use in a final report and may be useful during study planning (e.g., selecting a chemical supplier based on availability and price). "
  ),
  c(element = "4A",	
    name = "4A_1",	
    desc = "Randomization",
    tooltip = "State whether the samples will be fully or partially randomly ordered in the analytical sequence. State how will sample randomization be achieved. If any samples will be consistently analyzed at the same position(s) within an analytical batch (e.g., calibrants, QC samples, etc.), specify which samples and their position in the analytical sequence. ",	
    type = "vert table",	
    value = "Full vs. Partial Randomization;Method to achieve randomization;For any samples analyzed at a certain position within the analytical batch, specify which samples & position in sequence",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State whether the samples will be fully or partially randomly ordered in the analytical sequence. State how will sample randomization be achieved. If any samples will be consistently analyzed at the same position(s) within an analytical batch (e.g., calibrants, QC samples, etc.), specify which samples and their position in the analytical sequence. "
  ),
  c(element = "4A",	
    name = "4A_2",	
    desc = "Single vs Multiple Batch",
    tooltip = "State whether the study samples will be analyzed in a single analytical batch or multiple analytical batches. Include the number of samples per batch.",
    type = "vert table",	
    value = "Single vs. Multiple Batch;Number of samples per batch",	
    restrict = NA,	
    multiple = FALSE,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State whether the study samples will be analyzed in a single analytical batch or multiple analytical batches. Include the number of samples per batch."
  ),
  c(element = "4A",	
    name = "4A_3",	
    desc = "Multi-batch comparability and sample overlap",
    tooltip = "If multiple analytical batches are used, state what will be done to enable understanding and determining between-batch effects. Include which QC samples will be analyzed in all analytical batches to support comparisons, and what evaluation approaches will be used to conduct those comparisons. ",
    type = "vert table",	
    value = "General method to enable understanding/determining between-batch effects;QC samples to be analyzed in all analytical batches;Evaluation approaches to conduct comparisons across analytical batches",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "If multiple analytical batches are used, state what will be done to enable understanding and determining between-batch effects. Include which QC samples will be analyzed in all analytical batches to support comparisons, and what evaluation approaches will be used to conduct those comparisons."
  ),
  c(element = "4B",	
    name = "4B_1",	
    desc = "Citation for chromatography method",
    tooltip = "If your sample introduction + chromatography method(s) are based on a previously reported study or reference method, enter the prior method citation as a URL or DOI. State whether any parameters will be optimized prior to or during the study.",
    type = "vert table",	
    value = "Citation;Parameter optimization",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "If your sample introduction + chromatography method(s) are based on a previously reported study or reference method, enter the prior method citation as a URL or DOI. State whether any parameters will be optimized prior to or during the study."
  ),
  c(element = "4B",	
    name = "4B_2",	
    desc = "Chromatography instrument",
    tooltip = "State the instrument(s) manufacturer and model(s) that will be used for chromatography, including all modules. Also state the acquisition software and version.",
    type = "vert table",	
    value = "Instrument manufacturer;Instrument model(s);Acquisition software & version",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State the instrument(s) manufacturer and model(s) that will be used for chromatography, including all modules. Also state the acquisition software and version. "
  ),
  c(element = "4B",	
    name = "4B_3",	
    desc = "Sample introduction parameters",
    tooltip = "If the sample introduction method is novel or is a modified version of the above cited method, describe the sample introduction parameters.",
    type = "textarea",	
    value = "If the sample introduction method is novel or is a modified version of the above cited method, describe the sample introduction parameters.",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "For a list of parameters to include, go to Table 2.1 at: https://nontargetedanalysis.org/reference-content/methods/data-acquisition/#chromatography"
  ),
  c(element = "4B",	
    name = "4B_4",	
    desc = "Chromatography parameters",
    tooltip = "If the chromatography method is novel or is a modified version of the above cited method, describe the chromatography parameters.  ",
    type = "textarea",	
    value = "If the chromatography method is novel or is a modified version of the above cited method, describe the chromatography parameters.",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "For a list of parameters to include, go to Table 2.1 at: https://nontargetedanalysis.org/reference-content/methods/data-acquisition/#chromatography"
  ),
  c(element = "4C",	
    name = "4C_1",	
    desc = "Citation for mass spectrometry method",
    tooltip = "If your mass spectrometry method is based on a previously reported study or reference method, enter the prior method citation as a URL or DOI. ",
    type = "vert table",	
    value = "Citation;Parameter optimization",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "If your mass spectrometry method is based on a previously reported study or reference method, enter the prior method citation as a URL or DOI. "
  ),
  c(element = "4C",	
    name = "4C_2",	
    desc = "Mass spectrometry instrument",
    tooltip = "State the instrument(s) manufacturer and model(s) that will be used for mass spectrometry. Also state the acquisition software and version. ",
    type = "vert table",	
    value = "Instrument manufacturer;Instrument model(s);Acquisition software & version",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State the instrument(s) manufacturer and model(s) that will be used for mass spectrometry. Also state the acquisition software and version. "
  ),
  c(element = "4C",	
    name = "4C_3",	
    desc = "Mass spectrometry parameters",
    tooltip = "If the mass spectrometry method is novel or is a modified version of the above cited method, describe the mass spectrometry parameters. State whether any parameters will be optimized prior to or during the study. Include how the mass spectral data are generated (MS, MS2, AIF, DDA, DIA, SWATH, etc.).",
    type = "textarea",	
    value = "If the mass spectrometry method is novel or is a modified version of the above cited method, describe the mass spectrometry parameters. State whether any parameters will be optimized prior to or during the study. Include how the mass spectral data are generated (MS, MS2, AIF, DDA, DIA, SWATH, etc.).",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "For a full list of parameters, go to Table 2.3 at: https://nontargetedanalysis.org/reference-content/methods/data-acquisition/#mass-spec "
  ),
  c(element = "4C",	
    name = "4C_4",	
    desc = "Mass spectrometer calibration protocol",
    tooltip = "Describe any specific protocols for the calibration of the mass spectrometer m/z accuracy and resolution. State the frequency of instrument calibration and what calibration solution and/or lock-mass solution is used.",
    type = "vert table",	
    value = "Calibration protocol(s);Calibration frequency;Calibration and/or lock mass solution",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "For a full list of parameters, go to Table 2.3 at: https://nontargetedanalysis.org/reference-content/methods/data-acquisition/#mass-spec "
  ),
  c(element = "4C",	
    name = "4C_5",	
    desc = "Mass spectrometer calibration limits",
    tooltip = "State the expected (acceptable) m/z accuracy (in ppm) and resolution of the mass spectrometer. ",
    type = "vert table",	
    value = "Acceptable m/z accuracy (ppm);Acceptable m/z resolution",	
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State the expected (acceptable) m/z accuracy (in ppm) and resolution of the mass spectrometer. "
  ),
  c(element = "4D",	
    name = "4D_1",	
    desc = "System Suitability Protocols and Criteria",
    tooltip = "System Suitability Protocols and Criteria",
    type = "input table",	
    value = "System Suitability Protocol;System Suitability Evaluation Frequency;System Suitability Metric;System Suitability Calculation;System Suitability Criteria",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "System Suitability Protocol: Name the specific protocol that will be used for system suitability, including any chemical standards or QC samples that will be used for this purpose (refer to Section 3). System Suitability Frequency: State when how frequently the protocol will be performed. For example, at the beginning of each X-hour period, before/after each analytical sequence, etc. System Suitability Metric: Describe the specific metric that will be measured. System Suitability Calculation: Enter the equation or describe the calculation algorithm for calculating the metric. System Suitability Criteria: State the acceptable criteria (as a value or an acceptable range) for determining that the instrumental system is suitable for analysis.  "
  ),
  c(element = "5A",	
    name = "5A_1",	
    desc = "Software and/or code selection",
    tooltip = "State what software and/or code will be used (creator, version) and for which analyses. Define the version(s) used in the study, whether the software is open-source (external or in-house) or proprietary, and if portions of existing software code will be modified for the analyses in this study.  ",
    type = "vert table",	
    value = "Software used (version);Code/language used;Development environment software used",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State what software and/or code will be used (creator, version) and for which analyses. Define the version(s) used in the study, whether the software is open-source (external or in-house) or proprietary, and if portions of existing software code will be modified for the analyses in this study."
  ),
  c(element = "5A",	
    name = "5A_2",	
    desc = "Manual processing or review",
    tooltip = "State whether a user will perform manual processing to reduce, analyze, or review the raw data and/or the outputs from an automated analysis.",
    type = "vert table",	
    value = "Types of manual processing and review performed;Frequency of manual review;Comparison and flagging of manual processing and review to automated analysis",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State whether a user will perform manual processing to reduce, analyze, or review the raw data and/or the outputs from an automated analysis. "
  ),
  c(element = "5A",	
    name = "5A_3",	
    desc = "New software/code",
    tooltip = "State whether any new software, code, algorithms, packages, or scripts will be developed during this study. If yes, state how will it be developed, version-controlled, and provided for public use.",
    type = "vert table",	
    value = "Types of software/code developed;Code and coding language(s) used;Development environment software(s) used;Version tracking and updates;Availability of developed software/code",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State whether any new software, code, algorithms, packages, or scripts will be developed during this study. If yes, state how will it be developed, version-controlled, and provided for public use."
  ),
  c(element = "5B",	
    name = "5B_1",	
    desc = "Define sample groups for data processing",
    tooltip = "State whether and how samples will be grouped for data processing. ",
    type = "vert table",	
    value = "Number of groups;Group breakdown",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State whether and how samples will be grouped for data processing."
  ),
  c(element = "5B",	
    name = "5B_2",	
    desc = "Data conversion",
    tooltip = "State whether and how the data will be converted to a different format (from the raw data files) for processing/analysis. Define the settings for all parameters for data conversion.",
    type = "vert table",	
    value = "Raw file formats;File formats after conversion;Parameters used for file conversion",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State whether and how the data will be converted to a different format (from the raw data files) for processing/analysis. Define the settings for all parameters for data conversion. "
  ),
  c(element = "5B",	
    name = "5B_3",	
    desc = "Data extraction",
    tooltip = "State the workflow steps that will be used to extract the raw data and produce a list of detected features. Define the settings for all steps.",
    type = "input table",	
    value = "Workflow step for extraction;Parameter for extraction;Related QA/QC samples;Steps for comparison of QA/QC samples",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State the workflow steps that will be used to extract the raw data and produce a list of detected features. Define the settings for all steps. For a representative list of steps, go to Table 3.2 at: https://nontargetedanalysis.org/reference-content/methods/data-processing-and-analysis/#data-processing"
  ),
  c(element = "5B",	
    name = "5B_4",	
    desc = "Data reduction",
    tooltip = "State the workflow steps that will be used to reduce the processed data. State if any steps require comparisons to blanks and/or QC samples, including which blanks/QC samples will be used. Define the settings for all steps.",
    type = "input table",	
    value = "Workflow step for reduction;Parameter for reduction;Related QA/QC samples;Steps for comparison of QA/QC samples",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State the workflow steps that will be used to reduce the processed data. State if any steps require comparisons to blanks and/or QC samples, including which blanks/QC samples will be used. Define the settings for all steps. For a representative list of steps, go to Table 3.2 at: https://nontargetedanalysis.org/reference-content/methods/data-processing-and-analysis/#data-processing"
  ),
  c(element = "5B",	
    name = "5B_5",	
    desc = "Data normalization",
    tooltip = "State the workflow steps that will be used to normalize the reduced data. State if blank and/or QC samples will be used for normalization, including which blanks/QC samples will be used. Define the settings for all steps.",
    type = "input table",	
    value = "Workflow step for normalization;Parameter for normalization;Related QA/QC samples;Steps for comparison of QA/QC samples",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State the workflow steps that will be used to normalize the reduced data. State if blank and/or QC samples will be used for normalization, including which blanks/QC samples will be used. Define the settings for all steps."
  ),
  c(element = "5C",	
    name = "5C_1",	
    desc = "Statistical & Chemometric Analysis Workflow",
    tooltip = "Table of Statistical & Chemometric Analyses & Planned Outputs",
    type = "input table",	
    value = "Statistical or Chemometric Analysis Method;Statistical or Chemometric Analysis Method Goals;Samples Used & Sample Grouping for Statistical or Chemometric Analysis;Statistical or Chemometric Analysis Method Assumptions & Thresholds;Statistical or Chemometric Analysis Equations or Algorithms Used	Statistical or Chemometric Analysis Output(s);Statistical or Chemometric Analysis Output Reporting",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Statistical or Chemometric Analysis Method: Describe the method that will be used, including any relevant citation to published methods.•	Statistical or Chemometric Analysis Method Goals: State the specific goal of each method. State whether the approaches been validated for the intended use in this study by peer-reviewed literature.•	Samples Used & Sample Grouping for Statistical or Chemometric Analysis: State how samples be grouped for statistical/chemometric analyses. Include whether QC samples and/or blanks will be used in the statistical/chemometric approach.•	Statistical or Chemometric Analysis Method Assumptions & Thresholds: Define all assumptions and settings/thresholds for the analysis.•	Statistical or Chemometric Analysis Equations or Algorithms Used: State the equations or algorithms for the statistical or chemometric analysis, including whether they are publicly available.•	Statistical or Chemometric Analysis Output(s): State the planned outputs of the statistical or chemometric analysis, including any visuals or plots.•	Statistical or Chemometric Analysis Output Reporting: State how the planned outputs of the statistical or chemometric analysis (both numerical and visual outputs) will be reported."
  ),
  c(element = "5D",	
    name = "5D_1",	
    desc = "Annotation / identification workflow",
    tooltip = "Describe the general workflow for chemicals to be annotated or identified in the samples. Include any parameters or thresholds that must be used for this workflow. Include any compound class-specific approaches that will be used to aide in annotation (e.g. mass defect, molecular networking, spectral similarity).",
    type = "input table",	
    value = "Workflow steps for annotation and identification	Parameters/thresholds for workflow steps;Compound class-specific approaches;Parameters for compound class-specific approaches",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Describe the general workflow for chemicals to be annotated or identified in the samples. Include any parameters or thresholds that must be used for this workflow. Include any compound class-specific approaches that will be used to aide in annotation (e.g. mass defect, molecular networking, spectral similarity)." 
  ),
  c(element = "5D",	
    name = "5D_2",	
    desc = "Library/database for annotation / identification",
    tooltip = "State whether a mass spectral library or database will be used to annotate or identify chemicals in your samples. If yes, describe the library or database. Include the type of data (e.g., chemical formulas, MS2 spectra, etc.), the source of the data (and its public availability), how the data was collected/curated, and the quality of the data. When available, record any existing toxicological information for chemical class characterization for compounds present in the library or database. ",
    type = "input table",	
    value = "Mass spectral library and/or database used:;Number of compounds in library/database:;Compound classes in library/database:;Type of data within library/database:;Information on curation/generation of library/database:;Relevant metadata (toxicological, chemical class characterization) on compounds:",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State whether a mass spectral library or database will be used to annotate or identify chemicals in your samples. If yes, describe the library or database. Include the type of data (e.g., chemical formulas, MS2 spectra, etc.), the source of the data (and its public availability), how the data was collected/curated, and the quality of the data. When available, record any existing toxicological information for chemical class characterization for compounds present in the library or database." 
  ),
  c(element = "5D",	
    name = "5D_3",	
    desc = "Annotation/identification confidence",
    tooltip = "Describe the schema for categorizing or communicating the confidence of annotation or identification of chemicals in the sample. State any thresholds for acceptable annotation/identification, and state how evidence of annotations/identifications will be communicated. If using a previously stated confidence schema (e.g., the Schymanski scale), just enter the citation as a URL or DOI. If a previously stated scale will be used, include any modifications for your study.",
    type = "vert table",	
    value = "Schema for communication;Modifications to existing schema;Relevant citations for schema;Evidence used for annotations/identifications;Outputs and their format to communicate annotations/identifications & their confidence schema",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Describe the schema for categorizing or communicating the confidence of annotation or identification of chemicals in the sample. State any thresholds for acceptable annotation/identification, and state how evidence of annotations/identifications will be communicated. If using a previously stated confidence schema (e.g., the Schymanski scale), just enter the citation as a URL or DOI. If a previously stated scale will be used, include any modifications for your study." 
  ),
  c(element = "5E",	
    name = "5E_1",	
    desc = "qNTA Workflow",
    tooltip = "Describe the general workflow for quantitative NTA. State any parameters or thresholds and their values.",
    type = "input table",	
    value = "Workflow step for qNTA;Parameter and threshold for qNTA;Related QA/QC samples;Steps for comparison of QA/QC samples",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Describe the general workflow for quantitative NTA. State any parameters or thresholds and their values. Ensure that any methods for peak area normalization or other data treatment prior to quantitation are stated in Data Processing Workflow (Section 5B)." 
  ),
  c(element = "5E",	
    name = "5E_2",	
    desc = "qNTA calibrant and calibration curve selection",
    tooltip = "State the chemical used as calibrants and how they were selected. For calibrants, state the phase calibration curves will be prepared in (e.g. matrix-matched or solvent phase). If prepared in sample matrix, state if a pooled sample matrix will be used. State the number of calibrant points used and how the concentration range of the calibration curve was selected. ",
    type = "vert table",	
    value = "Calibrants used;Procedure used to select calibrants;Solvent phase of qNTA calibrants;Number of points in calibration curve;Determination of range in calibration curve",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State the chemical used as calibrants and how they were selected. For calibrants, state the phase calibration curves will be prepared in (e.g. matrix-matched or solvent phase). If prepared in sample matrix, state if a pooled sample matrix will be used. State the number of calibrant points used and how the concentration range of the calibration curve was selected. " 
  ),
  c(element = "5E",	
    name = "5E_3",	
    desc = "qNTA modeling approach",
    tooltip = "State the methods and approaches used for modeling qNTA data to generate calibration curves for calibrants and for estimating concentrations in features. Define equations and algorithms used, validation information for the methods, and specific criteria (e.g. accuracy, reproducibility, precision).",
    type = "vert table",	
    value = "Methods used for modeling data;Generation of calibration curves;Concentration estimations for features;Equations and algorithms used;Validation information",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State the methods and approaches used for modeling qNTA data to generate calibration curves for calibrants and for estimating concentrations in features. Define equations and algorithms used, validation information for the methods, and specific criteria (e.g. accuracy, reproducibility, precision)." 
  ),
  c(element = "5E",	
    name = "5E_4",	
    desc = "qNTA uncertainty analysis",
    tooltip = "State how uncertainty of the qNTA approach will be determined, and the expected bounds of the results. Define any approaches for comparing estimated concentrations with known/measured concentrations for calibrants.",
    type = "vert table",	
    value = "Approach for uncertainty estimation;Expected bounds of uncertainty approach;Comparison of estimates to known concentrations for calibrants",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State how uncertainty of the qNTA approach will be determined, and the expected bounds of the results. Define any approaches for comparing estimated concentrations with known/measured concentrations for calibrants. " 
  ),
  c(element = "5E",	
    name = "5E_5",	
    desc = "Concurrent vs. retrospective analysis",
    tooltip = "State if qNTA will be performed concurrently with the NTA workflow or if quantitation will occur retrospectively (or both)? If retrospective quantitation will occur, state methods used to ensure the data is usable for quantitation.",
    type = "vert table",	
    value = "Concurrent or retrospective qNTA;Quality assurance for retrospective qNTA",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State if qNTA will be performed concurrently with the NTA workflow or if quantitation will occur retrospectively (or both)? If retrospective quantitation will occur, state methods used to ensure the data is usable for quantitation." 
  ),
  c(element = "5F",	
    name = "5F_1",	
    desc = "Data types",
    tooltip = "State the types of data that will be produced during this study.",
    type = "vert table",	
    value = "Data type;File Format",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State the types of data that will be produced during this study." 
  ),
  c(element = "5F",	
    name = "5F_2",	
    desc = "Data chain of custody",
    tooltip = "State the protocols for the control and maintenance of the data. Include specific processes, such as manual record-keeping or use of an automated tracking system (such as LIMS) to track changes to the data itself.",
    type = "vert table",	
    value = "Protocols for control of data;Data controls (passwords and limited access);Process for change tracking",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State the protocols for the control and maintenance of the data. Include specific processes, such as manual record-keeping or use of an automated tracking system (such as LIMS) to track changes to the data itself." 
  ),
  c(element = "5F",	
    name = "5F_3",	
    desc = "Data storage",
    tooltip = "State how the data will be stored including file types (vendor/proprietary formats and/or standard file formats).",
    type = "vert table",	
    value = "File types;Location of stored data",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State how the data will be stored including file types (vendor/proprietary formats and/or standard file formats)." 
  ),
  c(element = "5F",	
    name = "5F_4",	
    desc = "Data backup",
    tooltip = "State how the data will be protected and backed up. Include specific protocols, such as back-up frequency and type.",
    type = "vert table",	
    value = "Backup of stored data;Backup frequency",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State how the data will be protected and backed up. Include specific protocols, such as back-up frequency and type." 
  ),
  c(element = "5F",	
    name = "5F_5",	
    desc = "Data sharing",
    tooltip = "Describe the how the data will be shared and version-controlled. Include the types of data that will be shareable, any restrictions on who can access the data, and how other researchers can request access to the data.",
    type = "vert table",	
    value = "Methods and platforms for sharing data;Version tracking for shared data;Limiting & requesting access to data",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Describe the how the data will be shared and version-controlled. Include the types of data that will be shareable, any restrictions on who can access the data, and how other researchers can request access to the data." 
  ),
  c(element = "6A",	
    name = "6A_1",	
    desc = "QA/QC",
    tooltip = "Table of QA/QC .",
    type = "input table",	
    value = "Sub-Category;QA/QC Aspect;Performance Metric;Performance Calculation;Performance Criteria",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "•	Sub-Category: State the component of the study for which performance will be evaluated, such as sample preparation, data acquisition, data processing, annotation & identification, statistical & chemometric analysis, or qNTA.•	Aspect: State the QA/QC aspect that will be evaluated (e.g., accuracy, precision, limit of detection, quantitation, or identification (LOD/LOQ/LOI)).•	Performance Metric: State the performance metric(s) that will be used to evaluate the chosen QA/QC aspect.•	Performance Calculation: State the calculation(s) used to for each selected metric.•	Performance Criteria: State the accepted criteria for each performance metric." 
  ),
  c(element = "6B",	
    name = "6B_1",	
    desc = "QA/QC Deviations - Data Inclusion",
    tooltip = "State how data will be reported if QA/QC metrics did not meet the established criteria. This can include whether samples will be excluded from analyses and/or reporting.",
    type = "vert table",	
    value = "Reporting of QA/QC metrics which do not meet criteria;Exclusion of samples that do not meet criteria",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "State how data will be reported if QA/QC metrics did not meet the established criteria. This can include whether samples will be excluded from analyses and/or reporting." 
  ),
  c(element = "6B",	
    name = "6B_2",	
    desc = "QA/QC Deviations - Data Flagging",
    tooltip = "Describe the procedures for identifying or flagging specific results and/or samples for which QA/QC metrics did not meet the established criteria.",
    type = "vert table",	
    value = "Procedures for identifying results/samples that do not meet criteria;Manual review of results that do and do not meet criteria",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Describe the procedures for identifying or flagging specific results and/or samples for which QA/QC metrics did not meet the established criteria." 
  ),
  c(element = "6B",	
    name = "6B_3",	
    desc = "QA/QC Deviations - Corrective Action",
    tooltip = "Describe any corrective actions that will be performed if QA/QC metrics do not meet established criteria.",
    type = "textarea",	
    value = "Describe any corrective actions that will be performed if QA/QC metrics do not meet established criteria.",
    restrict = NA,	
    multiple = NA,	
    example1 = "tbd",	
    example2 = "tbd",	
    example3 = "tbd",	
    example4 = "Describe any corrective actions that will be performed if QA/QC metrics do not meet established criteria." 
  )
)
)


element_values <- data.frame(rbind(
  c(	"1A",	"Study Objective and Scope",	"Prior to the designing other NTA study aspects, it is important for non-targeted analysis researchers to first examine their study’s objectives and scope. Specific decisions regarding the study objectives and scope will impact sampling design, controls, and performance metrics (among other aspects). For more information, go to: https://nontargetedanalysis.org/reference-content/methods/study-design/"	),
  c(	"2A",	"Sample Information",	"Describe the samples and sampling plan (including the sample types, sampling methods, use of replicates, and availability of sample material for blanks and QC samples), establish a plan for storing sampling metadata, and review the overall sampling design. For more information, go to: https://nontargetedanalysis.org/reference-content/methods/study-design/#sample-info-and-prep"	),
  c(	"2B",	"Sample Preparation",	"Define all sample preparation method parameters for the study. Describe any need for parameter optimization to ensure the selected method is appropriate for the chemicals of interest in the study. For more information, go to: https://nontargetedanalysis.org/reference-content/methods/study-design/#sample-info-and-prep "	),
  c(	"3A",	"Standards, Calibrants, Replicates, Blanks, QC Spikes, & QC Samples",	"Detail all analytical standards, calibrants, replication, blanks, QC spikes, and QC samples that will be used in the study. The tables below guide a researcher stepwise through the relevant information. For more information, go to: https://nontargetedanalysis.org/reference-content/methods/study-design/#sample-info-and-prep and https://nontargetedanalysis.org/reference-content/methods/study-design/#qc-spikes-and-samples."	),
  c(	"4A",	"Analytical Sequence and Batches",	"Determine the structure of the analytical sequence including randomization, quality control frequency, and sample replication. Identify if the study will use a single analytical batch or multiple batches, and how data from multiple batches will be combined. For more information, go to: https://nontargetedanalysis.org/reference-content/methods/data-acquisition/#analytical-sequence "	),
  c(	"4B",	"Chromatography",	"During study design, it is important to define all sample introduction and chromatography method parameters for the study and identify any parameters that may need to be adjusted prior to the study. For more information, go to: https://nontargetedanalysis.org/reference-content/methods/data-acquisition/#chromatography  "	),
  c(	"4C",	"Mass Spectrometry",	"During study design, it is important to define all mass spectrometry method parameters for the study and identify any parameters that may need to be adjusted prior to the study. For more information, go to: https://nontargetedanalysis.org/reference-content/methods/data-acquisition/#mass-spec"	),
  c(	"4D",	"System Suitability",	"System suitability covers a set of materials and procedures intended to verify the performance of an instrumental method. In layman’s terms, system suitability is a check to see if the system is working. It is important to include system suitability checks prior to, and potentially during and after, an analytical sequence to verify that the instrument will produce high quality data."	),
  c(	"5A",	"Software for Data Processing",	"For most non-targeted analysis workflows, one or more software or code-based tools are used to perform processing, statistical/chemometric analyses, annotation/identification of chemicals, and quantitative NTA. For more information, go to: https://nontargetedanalysis.org/reference-content/methods/data-processing-and-analysis/#data-processing "	),
  c(	"5B",	"Data Processing Workflow",	"Describe the data conversion, extraction, reduction, and normalization steps used to isolate features of interest prior to further data analysis. For more information, go to: https://nontargetedanalysis.org/reference-content/methods/data-processing-and-analysis/#data-processing."	),
  c(	"5C",	"Statistical & Chemometric Analysis Workflow",	"Determine the statistical or chemometric analyses and associated parameters. For more information, go to: https://nontargetedanalysis.org/reference-content/methods/data-processing-and-analysis/#statistical-analysis "	),
  c(	"5D",	"Annotation & Identification Workflow",	"Annotation and identification workflows are essential to translating analytical data into meaningful elemental formulas, chemical classes, and/or chemical structures and identifications. Describe the protocols by which features are annotated or identified after data processing. For more information, go to: https://nontargetedanalysis.org/reference-content/methods/data-processing-and-analysis/#annotation-and-id "	),
  c(	"5E",	"qNTA  Workflow",	"Quantitative non-targeted analysis (qNTA) is an emerging area of NTA studies for which unknown chemicals are not only identified, but estimates for quantity (i.e., concentration) in the sample are provided. This can be done concurrent with the study or in the future (retrospectively). To produce data that can be analyzed quantitatively, there are specific considerations that should be made beforehand."	),
  c(	"5F",	"Data Management Plan",	"PProper management of the raw, processed, and analyzed data for this study will enable researchers to adequately use and share the data, as well as enable the use of the data in future (retrospective) analyses. It is important to define types of data that will be generated and how data will be collected, stored, and shared."	),
  c(	"6A",	"QA/QC  ",	"Quality assurance and quality control is essential to evaluate the method performance and to ensure study results data are trustworthy and appropriate for use in decision-making. For more information, go to: https://nontargetedanalysis.org/reference-content/results/qa-qc-metrics/ and https://nontargetedanalysis.org/reference-content/methods/data-processing-and-analysis/#statistical-analysis. "),
  c("6B", "Procedures for QA/QC Deviations", "There may be occurrences where the QA/QC protocols described above will result in a sample, sequence, or multiple sequences failing to meet one or more researcher-defined criteria. It is important to understand what will happen to address such events and how the data that exists outside of accepted QA/QC criteria will be handled.")
))

colnames(element_values) <- c("element", "name", "desc")

#In-App Specific Functions
dynamic_input_fn <- function(name, desc, type, value, restrict = NULL, width = "100%", multiple = FALSE) {
  returnfn <- NULL
  if (type == "text") {
    returnfn <- textInput(inputId = name, label = desc, value = value, width = width)
  }
  if (type == "textarea") {
    returnfn <- textAreaInput(inputId = name, label = desc, value = value, width = width)
  }
  if (type == "numeric") {
    if (!is.na(restrict)) {
      restricts <- as.numeric(unlist(strsplit(restrict, split = ":")))
      returnfn <- numericInput(inputId = name, label = desc, value = value, min = restricts[1], max = restricts[2], step = restricts[3], width = width)
    }
    if (is.na(restrict)) {
      returnfn <- numericInput(inputId = name, label = desc, value = value, width = width)
    }
  }
  if (type == "list") {
    returnfn <- selectizeInput(inputId = name, label = desc, choices = unlist(strsplit(value, split = ";")), width = width, multiple = as.logical(multiple))
  }
  if (type == "list create") {
    returnfn <- selectizeInput(inputId = name, 
                               label = desc, 
                               choices = unlist(strsplit(value, split = ";")), 
                               width = width, 
                               multiple = as.logical(multiple),
                               options = list(create = TRUE))
  }
  if (type == "list matrix") {
    val <- unlist(strsplit(value, split = "!"))
    val <- val[1]
    returnfn <- list(
      selectizeInput(inputId = name,
                     label = desc,
                     choices = unlist(strsplit(val, split = ";")), 
                     width = width, 
                     multiple = as.logical(multiple),
                     options = list(create = TRUE)),
      DT::DTOutput(outputId = paste0("table_",name))
    )
  }
  if (type == "input table") {
    # default row num
    returnfn <- list(
      h4(desc),
      DT::DTOutput(outputId = paste0("table_", name))
    )
  }
  if (type == "vert table") {
    # default row num
    returnfn <- list(
      h4(desc),
      DT::DTOutput(outputId = paste0("table_", name))
    )
  }
  if (type == "modal table") {
    returnfn <- list(
      p(desc),
      actionButton(paste0("add_", name), "Add Value"),
      actionButton(paste0("remove_", name), "Remove Value"),
      DT::DTOutput(outputId = paste0("table_", name)),
      br()
    )
  }
  returnfn
}

dynamic_modal <- function(title, name, inputs, helps, ok = "Add value", size = "xl") {
  inputs <- unlist(strsplit(inputs, split = ";"))
  helps <- unlist(strsplit(helps, split = ";"))
  modalDialog(
    title = title,
    size = size,
    easyClose = TRUE,
    lapply(1:length(inputs), function(x)
      fluidRow(
        textInput(inputId = paste0("modal_", name, inputs[x]), inputs[x]), p(helps[x])
      )),
    footer = tagList(actionButton(paste0("modal_ok_", name), ok), modalButton("Cancel"))
  )
}

# Define header
header <- dashboardHeader(title = "NTA-SPT")

# Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("About", tabName = "about", selected = TRUE, icon = icon("question", lib = "font-awesome")),
              menuItem("Examples", tabName = "examples", icon = icon("id-card", lib = "font-awesome")),
              menuItem("Background and Objectives", tabName = "background", icon = icon("scroll", lib = "font-awesome")),
              menuItem("Sample Information", tabName = "samples", icon = icon("glass-water", lib = "font-awesome")),
              menuItem("Standards and Controls", tabName = "standards",icon = icon("flask", lib = "font-awesome")),
              menuItem("Data Acquisition", tabName = "method", icon = icon("microscope", lib = "font-awesome")),
              menuItem("Data Processing", tabName = "dataproc", icon = icon("computer", lib = "font-awesome")),
              menuItem("QA/QC", tabName = "qaqc", icon = icon("list-check", lib = "font-awesome")),
              menuItem("Export SMRT", tabName = "export", icon = icon("download", lib = "font-awesome")),
              selectInput("examples",label = "Load", choices = c("Additional Information", "1) Contaminated food", "2) Polluted river", "3) Human exposure"), selected = NULL),
              actionButton("user_guide", label = "Launch User Guide"),
              actionButton("browser", label = "Browser"),
              htmlOutput("progress")
  )
)

# Define body

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "about",
            p("Background: The Study Planning Tool (SPT) is intended as a guide for researchers who are developing studies that will use non-targeted analysis (NTA). The SPT was developed by members of the Best Practices for Non-Targeted Analysis (BP4NTA) working group. The SPT is part of a broader suite of tools and resources, including the Study Reporting Tool (SRT) and online Reference Content, that are structured based on the chronology and key components of an NTA study. We anticipate that the SPT will also eventually connect to tools that are currently under development, such as the ChemSpaceTool, including the ability to export information directly from the SPT into other platforms."),
            p("Purpose: The SPT is intended for use during the planning stages of a study as a resource for defining key parameters, methods, and metrics to be used in the study. Importantly, a completed SPT can be used to facilitate subsequent development of a quality assurance plan and ultimately enable clear and thorough reporting. The SPT can also be useful to identify potential gaps in study plans and to ensure aspects of quality assurance and quality control are addressed in each stage of a study."),
            p("Structure of SPT: The SPT is broken into sections, each covering a different stage of study planning. Each section contains a brief description of the section and its relation to NTA study planning. Sections also contain table(s) which give specific aspects of the study that should be considered. To facilitate user input, specific items that should be recorded are listed and reference content that define and expand on these aspects are given. Each aspect is then linked to a table where user input is recorded.")
    ),
    tabItem(tabName = "export",
            #fluidRow(downloadButton(outputId = "sop_export", label = "Download data in a SOP format", icon = icon("file-download", verify_fa = FALSE))),
            #fluidRow(downloadButton(outputId = "plan_export", label = "Download data in a Study Plan format", icon = icon("file-download", verify_fa = FALSE))),
            fluidRow(downloadButton(outputId = "raw_export", label = "Download raw data as a JSON", icon = icon("file-download", verify_fa = FALSE)))
    ),
    tabItem(tabName = "examples",
            h2("Example Study Designs"),
            h4("We have provided 3 different examples for studies and provided details in each section about that study. You can populate the examples by selecting the drop-down menu on the left."),
            fluidRow(box(title = "Example 1 - Contaminated food",
                         p("You are a scientist at a state regulatory lab (environmental/public health) that has experience in measuring contaminants in food and agricultural products via non-targeted analysis. You need to design a generalizable study to be ready for the following example: The local police department has a case of a single person getting sick from eating at a local farm-to-table restaurant, the only difference between the victim's meal and others was that they ate mashed potatoes with their meal. To discern the possibility that the mashed potatoes were the culprit, the police officers bring 10 g of sample in a sealed plastic bag to you to test for any possible chemical contaminants.")
            )), 
            fluidRow(box(title = "Example 2 - Polluted water",
                         p("Evaluate potential contamination from a chemical manufacturing facility located on a river")
            )), 
            fluidRow(box(title = "Example 3 - Human exposure",
                         p("A epidemiologist wants to conduct an exploratory analysis that helps identify unsuspected chemical exposures in a population with a particular disease.")
            ))
    ),
    tabItem(tabName = "background",
            fluidRow(
              #element info
              {
                  j <- 1 #set this manually
                  el_num <- element_values$element[j]
                  el_name <- element_values$name[j]
                  el_desc <- element_values$desc[j]
                  inputs <- input_values[which(input_values$element == el_num),]
                  fns <- list()
                  tt <- list()
                  for (i in 1:nrow(inputs)) {
                    fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                    fns <- list(fns, fn)
                    # if (!is.na(inputs$tooltip[i])) {
                    #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                    #   fns <- list(fns, tt)
                    # }
                  }
                  do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
                },
              #exampleinfo
              {
                j <- 1 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
              )
    ),
    tabItem(tabName = "samples",
            fluidRow(
              #element info
              {
                j <- 2 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 2 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            ),
            fluidRow(
              #element info
              {
                j <- 3 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 3 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            )
    ),
    tabItem(tabName = "standards",
            fluidRow(
              #element info
              {
                j <- 4 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 4 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            )
    ),
    tabItem(tabName = "method",
            fluidRow(
              #element info
              {
                j <- 5 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 5 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            ),
            fluidRow(
              #element info
              {
                j <- 6 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 6 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            ),
            fluidRow(
              #element info
              {
                j <- 7 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 7 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            )
    ),
    tabItem(tabName = "dataproc",
            fluidRow(
              #element info
              {
                j <- 9 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 9 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            ),
            fluidRow(
              #element info
              {
                j <- 10 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 10 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            ),
            fluidRow(
              #element info
              {
                j <- 11 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 11 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            ),
            fluidRow(
              #element info
              {
                j <- 12 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 12 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            ),
            fluidRow(
              #element info
              {
                j <- 13 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 13 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            ),
            fluidRow(
              #element info
              {
                j <- 14 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 14 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            )
    ),
    tabItem(tabName = "qaqc",
            fluidRow(
              #element info
              {
                j <- 15 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 15 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            ),
            fluidRow(
              #element info
              {
                j <- 16 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                tt <- list()
                for (i in 1:nrow(inputs)) {
                  fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
                  fns <- list(fns, fn)
                  # if (!is.na(inputs$tooltip[i])) {
                  #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
                  #   fns <- list(fns, tt)
                  # }
                }
                do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
              },
              #exampleinfo
              {
                j <- 16 #set this manually
                el_num <- element_values$element[j]
                el_name <- element_values$name[j]
                el_desc <- element_values$desc[j]
                inputs <- input_values[which(input_values$element == el_num),]
                fns <- list()
                fns <- lapply(1:nrow(inputs), function(i) {
                  p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[["example4"]][i])))
                })
                do.call(box, list(title = paste0("Examples"), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
              }
            )
    )
  )
)

# create UI


ui <- dashboardPage( header, sidebar, body, skin = "blue")

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  reacts <- reactiveValues(examplechoice = 1, score = 0, input_scores = data.frame(inputs = input_values$name, values = rep(0, nrow(input_values))))
  #because this needs to be reactive and manually made right now
  output_tables <- reactiveValues()
  
  #Warning message isn't supported by shinylive
  #shinyalert("Welcome!", "This is currently in development and should not be used for any reason other than that.", type = "info")
  
  
  observeEvent(input$examples, {
    reacts$examplechoice <- switch(input$examples,
                                   "1) Contaminated food" = 1, "2) Polluted river" = 2, "3) Human exposure" = 3, "Additional Information" = 4
    )
  })
  
  output$raw_export <- downloadHandler(
    filename = function() {
      paste0("SPTrawdata_", Sys.Date(), ".R")
    },
    content = function(file) {
      exportobj <- output_tables$table_2A_1
      dump('exportobj', file)
    }
  )
  
  observeEvent(input$browser, {browser()})
  
  ## List Matrix tables
  
  
  #2A_1
  output$table_2A_1 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_2A_1)) {
      val <- input_values$value[which(input_values$name == "2A_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_2A_1 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_2A_1) <- inputs
    }
      output_tables$table_2A_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_2A_1_cell_edit, {
    row  <- input$table_2A_1_cell_edit$row
    clmn <- input$table_2A_1_cell_edit$col
    output_tables$table_2A_1[row, clmn] <- input$table_2A_1_cell_edit$value
  })

  #2A_2
  output$table_2A_2 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_2A_2)) {
      val <- input_values$value[which(input_values$name == "2A_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_2A_2 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_2A_2) <- inputs
    }
    output_tables$table_2A_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_2A_2_cell_edit, {
    row  <- input$table_2A_2_cell_edit$row
    clmn <- input$table_2A_2_cell_edit$col
    output_tables$table_2A_2[row, clmn] <- input$table_2A_2_cell_edit$value
  })
  
    #2A_3
  output$table_2A_3 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_2A_3)) {
      val <- input_values$value[which(input_values$name == "2A_3")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_2A_3 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_2A_3) <- inputs
    }
    output_tables$table_2A_3
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_2A_3_cell_edit, {
    row  <- input$table_2A_3_cell_edit$row
    clmn <- input$table_2A_3_cell_edit$col
    output_tables$table_2A_3[row, clmn] <- input$table_2A_3_cell_edit$value
  })
  
  #2B_2
  output$table_2B_2 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_2B_2)) {
      val <- input_values$value[which(input_values$name == "2B_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_2B_2 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_2B_2) <- inputs
    }
    output_tables$table_2B_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_2B_2_cell_edit, {
    row  <- input$table_2B_2_cell_edit$row
    clmn <- input$table_2B_2_cell_edit$col
    output_tables$table_2B_2[row, clmn] <- input$table_2B_2_cell_edit$value
  })
  
  
  #2B_4
  output$table_2B_4 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_2B_4)) {
      val <- input_values$value[which(input_values$name == "2B_4")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_2B_4 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_2B_4) <- inputs
    }
    output_tables$table_2B_4
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_2B_4_cell_edit, {
    row  <- input$table_2B_4_cell_edit$row
    clmn <- input$table_2B_4_cell_edit$col
    output_tables$table_2B_4[row, clmn] <- input$table_2B_4_cell_edit$value
  })
  
  #3A_1
  output$table_3A_1 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_3A_1)) {
      val <- input_values$value[which(input_values$name == "3A_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_3A_1 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_3A_1) <- inputs
    }
    output_tables$table_3A_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_3A_1_cell_edit, {
    row  <- input$table_3A_1_cell_edit$row
    clmn <- input$table_3A_1_cell_edit$col
    output_tables$table_3A_1[row, clmn] <- input$table_3A_1_cell_edit$value
  })
  
  #3A_2
  output$table_3A_2 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_3A_2)) {
      val <- input_values$value[which(input_values$name == "3A_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_3A_2 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_3A_2) <- inputs
    }
    output_tables$table_3A_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_3A_2_cell_edit, {
    row  <- input$table_3A_2_cell_edit$row
    clmn <- input$table_3A_2_cell_edit$col
    output_tables$table_3A_2[row, clmn] <- input$table_3A_2_cell_edit$value
  })
  
  
  #3A_3
  output$table_3A_3 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_3A_3)) {
      val <- input_values$value[which(input_values$name == "3A_3")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_3A_3 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_3A_3) <- inputs
    }
    output_tables$table_3A_3
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_3A_3_cell_edit, {
    row  <- input$table_3A_3_cell_edit$row
    clmn <- input$table_3A_3_cell_edit$col
    output_tables$table_3A_3[row, clmn] <- input$table_3A_3_cell_edit$value
  })
  
  #4D_1
  output$table_4D_1 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_4D_1)) {
      val <- input_values$value[which(input_values$name == "4D_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_4D_1 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_4D_1) <- inputs
    }
    output_tables$table_4D_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_4D_1_cell_edit, {
    row  <- input$table_4D_1_cell_edit$row
    clmn <- input$table_4D_1_cell_edit$col
    output_tables$table_4D_1[row, clmn] <- input$table_4D_1_cell_edit$value
  })
  
  #5B_3
  output$table_5B_3 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_5B_3)) {
      val <- input_values$value[which(input_values$name == "5B_3")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5B_3 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_5B_3) <- inputs
    }
    output_tables$table_5B_3
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_5B_3_cell_edit, {
    row  <- input$table_5B_3_cell_edit$row
    clmn <- input$table_5B_3_cell_edit$col
    output_tables$table_5B_3[row, clmn] <- input$table_5B_3_cell_edit$value
  })
  
  #5B_4
  output$table_5B_4 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_5B_4)) {
      val <- input_values$value[which(input_values$name == "5B_4")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5B_4 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_5B_4) <- inputs
    }
    output_tables$table_5B_4
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_5B_4_cell_edit, {
    row  <- input$table_5B_4_cell_edit$row
    clmn <- input$table_5B_4_cell_edit$col
    output_tables$table_5B_4[row, clmn] <- input$table_5B_4_cell_edit$value
  })
  
  
  #5B_5
  output$table_5B_5 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_5B_5)) {
      val <- input_values$value[which(input_values$name == "5B_5")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5B_5 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_5B_5) <- inputs
    }
    output_tables$table_5B_5
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_5B_5_cell_edit, {
    row  <- input$table_5B_5_cell_edit$row
    clmn <- input$table_5B_5_cell_edit$col
    output_tables$table_5B_5[row, clmn] <- input$table_5B_5_cell_edit$value
  })
  
  #5C_1
  output$table_5C_1 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_5C_1)) {
      val <- input_values$value[which(input_values$name == "5C_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5C_1 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_5C_1) <- inputs
    }
    output_tables$table_5C_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_5C_1_cell_edit, {
    row  <- input$table_5C_1_cell_edit$row
    clmn <- input$table_5C_1_cell_edit$col
    output_tables$table_5C_1[row, clmn] <- input$table_5C_1_cell_edit$value
  })
  
  #5D_1
  output$table_5D_1 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_5D_1)) {
      val <- input_values$value[which(input_values$name == "5D_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5D_1 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_5D_1) <- inputs
    }
    output_tables$table_5D_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_5D_1_cell_edit, {
    row  <- input$table_5D_1_cell_edit$row
    clmn <- input$table_5D_1_cell_edit$col
    output_tables$table_5D_1[row, clmn] <- input$table_5D_1_cell_edit$value
  })
  
  #5D_2
  output$table_5D_2 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_5D_2)) {
      val <- input_values$value[which(input_values$name == "5D_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5D_2 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_5D_2) <- inputs
    }
    output_tables$table_5D_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_5D_2_cell_edit, {
    row  <- input$table_5D_2_cell_edit$row
    clmn <- input$table_5D_2_cell_edit$col
    output_tables$table_5D_2[row, clmn] <- input$table_5D_2_cell_edit$value
  })
  
  #5E_1
  output$table_5E_1 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_5E_1)) {
      val <- input_values$value[which(input_values$name == "5E_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5E_1 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_5E_1) <- inputs
    }
    output_tables$table_5E_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_5E_1_cell_edit, {
    row  <- input$table_5E_1_cell_edit$row
    clmn <- input$table_5E_1_cell_edit$col
    output_tables$table_5E_1[row, clmn] <- input$table_5E_1_cell_edit$value
  })
  
  #6A_1
  output$table_6A_1 <- renderDT({
    #default row number
    rownumber = 10
    if (is.null(output_tables$table_6A_1)) {
      val <- input_values$value[which(input_values$name == "6A_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_6A_1 <- data.frame(matrix("", nrow = rownumber, ncol = length(inputs)))
      colnames(output_tables$table_6A_1) <- inputs
    }
    output_tables$table_6A_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_6A_1_cell_edit, {
    row  <- input$table_6A_1_cell_edit$row
    clmn <- input$table_6A_1_cell_edit$col
    output_tables$table_6A_1[row, clmn] <- input$table_6A_1_cell_edit$value
  })
  
  #4A_1 vert table
  output$table_4A_1 <- renderDT({
    #default row number
    if (is.null(output_tables$table_4A_1)) {
      val <- input_values$value[which(input_values$name == "4A_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_4A_1 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_4A_1) <- inputs
      colnames(output_tables$table_4A_1) <- "Value"
    }
    output_tables$table_4A_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_4A_1_cell_edit, {
    row  <- input$table_4A_1_cell_edit$row
    clmn <- input$table_4A_1_cell_edit$col
    output_tables$table_4A_1[row, clmn] <- input$table_4A_1_cell_edit$value
  })
  
  #4A_2 vert table
  output$table_4A_2 <- renderDT({
    #default row number
    if (is.null(output_tables$table_4A_2)) {
      val <- input_values$value[which(input_values$name == "4A_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_4A_2 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_4A_2) <- inputs
      colnames(output_tables$table_4A_2) <- "Value"
    }
    output_tables$table_4A_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_4A_2_cell_edit, {
    row  <- input$table_4A_2_cell_edit$row
    clmn <- input$table_4A_2_cell_edit$col
    output_tables$table_4A_2[row, clmn] <- input$table_4A_2_cell_edit$value
  })
  
  #4A_3 vert table
  output$table_4A_3 <- renderDT({
    #default row number
    if (is.null(output_tables$table_4A_3)) {
      val <- input_values$value[which(input_values$name == "4A_3")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_4A_3 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_4A_3) <- inputs
      colnames(output_tables$table_4A_3) <- "Value"
    }
    output_tables$table_4A_3
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_4A_3_cell_edit, {
    row  <- input$table_4A_3_cell_edit$row
    clmn <- input$table_4A_3_cell_edit$col
    output_tables$table_4A_3[row, clmn] <- input$table_4A_3_cell_edit$value
  })
  
  #4B_1 vert table
  output$table_4B_1 <- renderDT({
    #default row number
    if (is.null(output_tables$table_4B_1)) {
      val <- input_values$value[which(input_values$name == "4B_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_4B_1 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_4B_1) <- inputs
      colnames(output_tables$table_4B_1) <- "Value"
    }
    output_tables$table_4B_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_4B_1_cell_edit, {
    row  <- input$table_4B_1_cell_edit$row
    clmn <- input$table_4B_1_cell_edit$col
    output_tables$table_4B_1[row, clmn] <- input$table_4B_1_cell_edit$value
  })
  
  #4B_2 vert table
  output$table_4B_2 <- renderDT({
    #default row number
    if (is.null(output_tables$table_4B_2)) {
      val <- input_values$value[which(input_values$name == "4B_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_4B_2 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_4B_2) <- inputs
      colnames(output_tables$table_4B_2) <- "Value"
    }
    output_tables$table_4B_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_4B_2_cell_edit, {
    row  <- input$table_4B_2_cell_edit$row
    clmn <- input$table_4B_2_cell_edit$col
    output_tables$table_4B_2[row, clmn] <- input$table_4B_2_cell_edit$value
  })
  
  #4C_1 vert table
  output$table_4C_1 <- renderDT({
    #default row number
    if (is.null(output_tables$table_4C_1)) {
      val <- input_values$value[which(input_values$name == "4C_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_4C_1 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_4C_1) <- inputs
      colnames(output_tables$table_4C_1) <- "Value"
    }
    output_tables$table_4C_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_4C_1_cell_edit, {
    row  <- input$table_4C_1_cell_edit$row
    clmn <- input$table_4C_1_cell_edit$col
    output_tables$table_4C_1[row, clmn] <- input$table_4C_1_cell_edit$value
  })
  
  #4C_2 vert table
  output$table_4C_2 <- renderDT({
    #default row number
    if (is.null(output_tables$table_4C_2)) {
      val <- input_values$value[which(input_values$name == "4C_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_4C_2 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_4C_2) <- inputs
      colnames(output_tables$table_4C_2) <- "Value"
    }
    output_tables$table_4C_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_4C_2_cell_edit, {
    row  <- input$table_4C_2_cell_edit$row
    clmn <- input$table_4C_2_cell_edit$col
    output_tables$table_4C_2[row, clmn] <- input$table_4C_2_cell_edit$value
  })
  
  #4C_4 vert table
  output$table_4C_4 <- renderDT({
    #default row number
    if (is.null(output_tables$table_4C_4)) {
      val <- input_values$value[which(input_values$name == "4C_4")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_4C_4 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_4C_4) <- inputs
      colnames(output_tables$table_4C_4) <- "Value"
    }
    output_tables$table_4C_4
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_4C_4_cell_edit, {
    row  <- input$table_4C_4_cell_edit$row
    clmn <- input$table_4C_4_cell_edit$col
    output_tables$table_4C_4[row, clmn] <- input$table_4C_4_cell_edit$value
  })
  
  #4C_5 vert table
  output$table_4C_5 <- renderDT({
    #default row number
    if (is.null(output_tables$table_4C_5)) {
      val <- input_values$value[which(input_values$name == "4C_5")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_4C_5 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_4C_5) <- inputs
      colnames(output_tables$table_4C_5) <- "Value"
    }
    output_tables$table_4C_5
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_4C_5_cell_edit, {
    row  <- input$table_4C_5_cell_edit$row
    clmn <- input$table_4C_5_cell_edit$col
    output_tables$table_4C_5[row, clmn] <- input$table_4C_5_cell_edit$value
  })
  
  #5A_1 vert table
  output$table_5A_1 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5A_1)) {
      val <- input_values$value[which(input_values$name == "5A_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5A_1 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5A_1) <- inputs
      colnames(output_tables$table_5A_1) <- "Value"
    }
    output_tables$table_5A_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5A_1_cell_edit, {
    row  <- input$table_5A_1_cell_edit$row
    clmn <- input$table_5A_1_cell_edit$col
    output_tables$table_5A_1[row, clmn] <- input$table_5A_1_cell_edit$value
  })
  
  #5A_2 vert table
  output$table_5A_2 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5A_2)) {
      val <- input_values$value[which(input_values$name == "5A_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5A_2 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5A_2) <- inputs
      colnames(output_tables$table_5A_2) <- "Value"
    }
    output_tables$table_5A_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5A_2_cell_edit, {
    row  <- input$table_5A_2_cell_edit$row
    clmn <- input$table_5A_2_cell_edit$col
    output_tables$table_5A_2[row, clmn] <- input$table_5A_2_cell_edit$value
  })
  
  #5A_3 vert table
  output$table_5A_3 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5A_3)) {
      val <- input_values$value[which(input_values$name == "5A_3")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5A_3 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5A_3) <- inputs
      colnames(output_tables$table_5A_3) <- "Value"
    }
    output_tables$table_5A_3
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5A_3_cell_edit, {
    row  <- input$table_5A_3_cell_edit$row
    clmn <- input$table_5A_3_cell_edit$col
    output_tables$table_5A_3[row, clmn] <- input$table_5A_3_cell_edit$value
  })
  
  #5B_1 vert table
  output$table_5B_1 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5B_1)) {
      val <- input_values$value[which(input_values$name == "5B_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5B_1 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5B_1) <- inputs
      colnames(output_tables$table_5B_1) <- "Value"
    }
    output_tables$table_5B_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5B_1_cell_edit, {
    row  <- input$table_5B_1_cell_edit$row
    clmn <- input$table_5B_1_cell_edit$col
    output_tables$table_5B_1[row, clmn] <- input$table_5B_1_cell_edit$value
  })
  
  #5B_2 vert table
  output$table_5B_2 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5B_2)) {
      val <- input_values$value[which(input_values$name == "5B_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5B_2 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5B_2) <- inputs
      colnames(output_tables$table_5B_2) <- "Value"
    }
    output_tables$table_5B_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5B_2_cell_edit, {
    row  <- input$table_5B_2_cell_edit$row
    clmn <- input$table_5B_2_cell_edit$col
    output_tables$table_5B_2[row, clmn] <- input$table_5B_2_cell_edit$value
  })
  
  #5B_3 vert table
  output$table_5B_3 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5B_3)) {
      val <- input_values$value[which(input_values$name == "5B_3")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5B_3 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5B_3) <- inputs
      colnames(output_tables$table_5B_3) <- "Value"
    }
    output_tables$table_5B_3
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5B_3_cell_edit, {
    row  <- input$table_5B_3_cell_edit$row
    clmn <- input$table_5B_3_cell_edit$col
    output_tables$table_5B_3[row, clmn] <- input$table_5B_3_cell_edit$value
  })
  
  #5E_2 vert table
  output$table_5E_2 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5E_2)) {
      val <- input_values$value[which(input_values$name == "5E_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5E_2 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5E_2) <- inputs
      colnames(output_tables$table_5E_2) <- "Value"
    }
    output_tables$table_5E_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5E_2_cell_edit, {
    row  <- input$table_5E_2_cell_edit$row
    clmn <- input$table_5E_2_cell_edit$col
    output_tables$table_5E_2[row, clmn] <- input$table_5E_2_cell_edit$value
  })
  
  #5E_3 vert table
  output$table_5E_3 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5E_3)) {
      val <- input_values$value[which(input_values$name == "5E_3")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5E_3 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5E_3) <- inputs
      colnames(output_tables$table_5E_3) <- "Value"
    }
    output_tables$table_5E_3
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5E_3_cell_edit, {
    row  <- input$table_5E_3_cell_edit$row
    clmn <- input$table_5E_3_cell_edit$col
    output_tables$table_5E_3[row, clmn] <- input$table_5E_3_cell_edit$value
  })
  
  #5E_4 vert table
  output$table_5E_4 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5E_4)) {
      val <- input_values$value[which(input_values$name == "5E_4")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5E_4 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5E_4) <- inputs
      colnames(output_tables$table_5E_4) <- "Value"
    }
    output_tables$table_5E_4
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5E_4_cell_edit, {
    row  <- input$table_5E_4_cell_edit$row
    clmn <- input$table_5E_4_cell_edit$col
    output_tables$table_5E_4[row, clmn] <- input$table_5E_4_cell_edit$value
  })
  
  #5E_5 vert table
  output$table_5E_5 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5E_5)) {
      val <- input_values$value[which(input_values$name == "5E_5")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5E_5 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5E_5) <- inputs
      colnames(output_tables$table_5E_5) <- "Value"
    }
    output_tables$table_5E_5
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5E_5_cell_edit, {
    row  <- input$table_5E_5_cell_edit$row
    clmn <- input$table_5E_5_cell_edit$col
    output_tables$table_5E_5[row, clmn] <- input$table_5E_5_cell_edit$value
  })
  
  #5F_1 vert table
  output$table_5F_1 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5F_1)) {
      val <- input_values$value[which(input_values$name == "5F_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5F_1 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5F_1) <- inputs
      colnames(output_tables$table_5F_1) <- "Value"
    }
    output_tables$table_5F_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5F_1_cell_edit, {
    row  <- input$table_5F_1_cell_edit$row
    clmn <- input$table_5F_1_cell_edit$col
    output_tables$table_5F_1[row, clmn] <- input$table_5F_1_cell_edit$value
  })
  
  #5F_2 vert table
  output$table_5F_2 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5F_2)) {
      val <- input_values$value[which(input_values$name == "5F_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5F_2 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5F_2) <- inputs
      colnames(output_tables$table_5F_2) <- "Value"
    }
    output_tables$table_5F_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5F_2_cell_edit, {
    row  <- input$table_5F_2_cell_edit$row
    clmn <- input$table_5F_2_cell_edit$col
    output_tables$table_5F_2[row, clmn] <- input$table_5F_2_cell_edit$value
  })
  
  #5F_3 vert table
  output$table_5F_3 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5F_3)) {
      val <- input_values$value[which(input_values$name == "5F_3")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5F_3 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5F_3) <- inputs
      colnames(output_tables$table_5F_3) <- "Value"
    }
    output_tables$table_5F_3
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5F_3_cell_edit, {
    row  <- input$table_5F_3_cell_edit$row
    clmn <- input$table_5F_3_cell_edit$col
    output_tables$table_5F_3[row, clmn] <- input$table_5F_3_cell_edit$value
  })
  
  #5F_4 vert table
  output$table_5F_4 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5F_4)) {
      val <- input_values$value[which(input_values$name == "5F_4")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5F_4 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5F_4) <- inputs
      colnames(output_tables$table_5F_4) <- "Value"
    }
    output_tables$table_5F_4
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5F_4_cell_edit, {
    row  <- input$table_5F_4_cell_edit$row
    clmn <- input$table_5F_4_cell_edit$col
    output_tables$table_5F_4[row, clmn] <- input$table_5F_4_cell_edit$value
  })
  
  #5F_5 vert table
  output$table_5F_5 <- renderDT({
    #default row number
    if (is.null(output_tables$table_5F_5)) {
      val <- input_values$value[which(input_values$name == "5F_5")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_5F_5 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_5F_5) <- inputs
      colnames(output_tables$table_5F_5) <- "Value"
    }
    output_tables$table_5F_5
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_5F_5_cell_edit, {
    row  <- input$table_5F_5_cell_edit$row
    clmn <- input$table_5F_5_cell_edit$col
    output_tables$table_5F_5[row, clmn] <- input$table_5F_5_cell_edit$value
  })
  
  #6B_1 vert table
  output$table_6B_1 <- renderDT({
    #default row number
    if (is.null(output_tables$table_6B_1)) {
      val <- input_values$value[which(input_values$name == "6B_1")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_6B_1 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_6B_1) <- inputs
      colnames(output_tables$table_6B_1) <- "Value"
    }
    output_tables$table_6B_1
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_6B_1_cell_edit, {
    row  <- input$table_6B_1_cell_edit$row
    clmn <- input$table_6B_1_cell_edit$col
    output_tables$table_6B_1[row, clmn] <- input$table_6B_1_cell_edit$value
  })
  
  #6B_2 vert table
  output$table_6B_2 <- renderDT({
    #default row number
    if (is.null(output_tables$table_6B_2)) {
      val <- input_values$value[which(input_values$name == "6B_2")]
      inputs <- unlist(strsplit(val, ";"))
      output_tables$table_6B_2 <- data.frame(matrix("", nrow = length(inputs), ncol = 1))
      rownames(output_tables$table_6B_2) <- inputs
      colnames(output_tables$table_6B_2) <- "Value"
    }
    output_tables$table_6B_2
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE, autoWidth = FALSE, columnDefs = list(list(width = '400px', targets = c(1)))))
  
  observeEvent(input$table_6B_2_cell_edit, {
    row  <- input$table_6B_2_cell_edit$row
    clmn <- input$table_6B_2_cell_edit$col
    output_tables$table_6B_2[row, clmn] <- input$table_6B_2_cell_edit$value
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
