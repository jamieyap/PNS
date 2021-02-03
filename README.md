# About the Por Nuestra Salud Study

The Por Nuestra Salud (PNS) study is designed to investigate the influence of neighborhood-level and individual-level factors on the intrapersonal and contextual determinants of smoking lapses, temptations, and daily experience among 200 Spanish-speaking Mexican American (MA) smokers attempting to quit. Each participant will carry a smart phone throughout the study where software specifically designed for the study will be the primary means of delivering EMA questionnaires. Broadly, an EMA questionnaire could not only be launched at randomly selected times, but also after the participant indicated via a button press (i.e., the participant taps on their smart phone's user interface) that they experienced a smoking-related event. Other various information were also collected throughout the study; the the data collection time frame is displayed below vis-a-vis the study timeline.

![Image of study timeline](https://github.com/jamieyap/PNS/blob/master/documentation/study_visit_timeline_pns.png)

# About this Repository

This repository contains code and accompanying documentation focuses on curation of intensive longitudinal data (ILD) from EMA questionnaires from both pre- and post- quit periods; other data collected during the conduct of the study are beyond the scope of this repository and accompanying documentation. ILD from EMA questionnaires are contained in nine raw data files which are in tabular format and have similar data structure. For each raw data file, all records (i.e., rows) belong to one and only one kind of EMA questionnaire. The figure below provides a grossly simplified schematic of columns and rows contained in these raw data files for one hypothetical participant.

![Image of study timeline](https://github.com/jamieyap/PNS/blob/master/documentation/mock_data.png)

As depicted above, all nine raw data files contain columns for unique identifiers, columns for context variables, columns for time variables, and columns for EMA questionnaire items. The raw data files differ only with respect to the specific columns pertaining to EMA questionnaire items.

# Start Here

Open source code and documentation were created to: 

* Provide a detailed description of key assumptions and decisions made in data preprocessing and preparation implemented after the conduct of the study
* Offer a workflow that can be reproduced by others 
* Provide end-users with curated datasets that can be traced directly back to the raw datasets used in constructing them, similar to the concept of _farm-to-table traceability_


| <img height=0 width=350> File <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
| [documentation](https://github.com/jamieyap/PNS/blob/master/documentation/pns_documentation_20210122.pdf) | Documentation of the key assumptions and decisions made in data preprocessing and preparation implemented after the conduct of the study.|

# Code in this Repository

The script [data-curation-pipeline.R](https://github.com/jamieyap/PNS/blob/master/data-curation-pipeline.R) executes data preprocessing and preparation steps, calling on each of the scripts below in sequence. 

| <img height=0 width=350> File <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
| [calc-quit-dates.R](https://github.com/jamieyap/PNS/blob/master/scripts-studies/calc-quit-dates.R) | Calculate quit date, start study date, and end study date. |
| [create-dictionary.R](https://github.com/jamieyap/PNS/blob/master/scripts-studies/create-dictionary.R) | Identify column names in raw data where responses to EMA items are recorded, for each of the original column names, new names are given; the format used in name_new enables end-users to determine the particular kind of EMA in which the value in the merged dataset (having data from two or more kinds of EMA; there are nine kinds of EMA) was originally provided. |
| [read-ema.R](https://github.com/jamieyap/PNS/blob/master/scripts-studies/read-ema.R) | Reads in EMA raw data files, Constuct variables for time when EMA was delivered, begun, completed, construct variable indicating whether a response to any item was recorded, save to intermediate file in preparation for further data processing. |
| [clean-ema.R](https://github.com/jamieyap/PNS/blob/master/scripts-studies/clean-ema.R) | Complete preparaton by type of EMA questionnaire. |
| [create-database-smoking.R](https://github.com/jamieyap/PNS/blob/master/scripts-studies/create-database-smoking.R) | Create database of smoking information using 'cleaned' EMA questionnaire data. |
| [clean-baseline.R](https://github.com/jamieyap/PNS/blob/master/scripts-studies/clean-baseline.R) | Rudimentary preparation of baseline data collected during the 2nd in-person study visit: simply exclude rows corresponding to baseline participants who will not be included in any analysis. |
| [output-formatted-database.R](https://github.com/jamieyap/PNS/blob/master/scripts-studies/output-formatted-database.R) | Format and save RData files into stand-alone or merged databases. |

Finally, a directed acyclic graph displayed below depicts dependencies between scripts, e.g., an arrow going from A to B represents the fact that B depends on A.

![dag](https://github.com/jamieyap/PNS/blob/master/documentation/data_curation_dag.png)





