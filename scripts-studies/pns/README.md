## Por Nuestra Salud Study

## 1 Essential

| File | Description|
|:--------:|:--------------------|
| TBA | Documents data curation |

## 2 More Details

### 2.1 Data Curation

| File | Description|
|:--------:|:--------------------|
| [calc-quit-dates.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/calc-quit-dates.R) | Create a file containing time variables to anchor data analysis: Quit Time, Begin Study Time, and End of Study Time |
| [get-ema-item-names.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/get-ema-item-names.R) | In preparation of merging data corresponding to each EMA type into one file, create new variable names with similar format, e.g., postquit.random.item.XX, and a dictionary mapping the correspondence of original item names and item names in the new format |
| [get-ema-item-responses.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/get-ema-item-responses.R) | Perform data preparation tasks and save intermediate results for further data preprocessing. Data preparation tasks in this script include: 1. exclude EMAs with some indication of unsuccessful delivery, 2. constuct variables for time when EMA was delivered, begun, completed, 3. construct variable indicating whether a response to any item was recorded 4. clean up smoking counts variable 5. apply new column names created in [get-ema-item-names.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/get-ema-item-names.R) |
| [get-smoking-vars.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/get-smoking-vars.R) | Complete preparaton of smoking-related variables, particularly time variables related to the smoking outcome and save output to individual csv file |
|[get-ema-data-frames.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/get-ema-data-frames.R) | Complete preparaton for each type of EMA. Further, 1. One preparation for each individual type of EMA is complete, save output to individual csv files 2. Merge smoking outcome constructed in [get-smoking-vars.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/get-smoking-vars.R) with curated data from all types of EMA into one file (this is the `BIG.df` variable in the script) |

### 2.2 Checks on Input and Output Data

| File | Description|
|:--------:|:--------------------|
| [calc-candidate-dates.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/calc-candidate-dates.R) | Get timestamp of earliest Post-Quit EMA and merge dates from multiple sources into one file |
| [do-checks-01.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/do-checks-01.R) | Calculate summary statistics to compare number of EMAs before and after application of exclusion criteria |
| [do-checks-02.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/do-checks-02.R) | Calculate summary statistics on number of EMAs with respect to several indicators of engagement with EMA completion before application of exclusion criteria |
| [do-checks-03.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/do-checks-03.R) | Calculate summary statistics on time elapsed between Random EMA prompt/initiation of self-initiated EMAs and beginning EMA completion |
| [do-checks-04.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/do-checks-04.R) | Calls [smoking-plots.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/smoking-plots.R) to visualize reported number of cigarettes smoked with respect to days elapsed since timestamp of earliest Post-Quit EMA calculated in [calc-candidate-dates.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/calc-candidate-dates.R) and [zoomed-smoking-plots.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/zoomed-smoking-plots.R) to zoom in on specific time frames |

### 2.3 Scripts Collecting Functions

| File | Description|
|:--------:|:--------------------|
| [shared-data-manip-utils.R](https://github.com/jamieyap/MARS/blob/master/scripts-shared/shared-data-manip-utils.R) | Contains functions to perform data manipulation tasks and can be utilized across all studies |
| [data-manip-utils.R](https://github.com/jamieyap/MARS/blob/master/scripts-studies/pns/data-manip-utils.R) | Contains functions to perform data manipulation tasks specific to the PNS study |


