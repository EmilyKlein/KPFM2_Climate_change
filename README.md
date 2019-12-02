# KPFM2 and CLIMATE CHANGE ~ READ ME and ANALYSIS GUIDE:
R model code for the Krill-Predator-Fishery Model, used to assess trade-offs between predators and a fishery both focused on a forage species (currently focused on krill in the Southern Ocean). Code here specifically for results in Klein et al. 2018 “Climate-change impacts on prey increase risks for predators in the Scotia Sea”, published in PLOS ONE (https://doi.org/10.1371/journal.pone.0191011).


This file contains information on the (1) data input files, (2) code for analysis, and (3) steps in analysis used to obtain results in Klein et al., “Climate-change impacts on prey increase risks for predators in the Scotia Sea”. The analysis uses functions coded in [R] programming language, which is available for free download at https://www.r-project.org/ (<em>NB</em>: this research used RStudio, https://www.rstudio.com/, to access the R software).
 
 <br>
<h3>(1) DATA INPUT FILES</h3>
<b>mlt_new, mst_new, nlt_new, nst_new</b>: The four main .txt input for the KPFM2 ecosystem model, one for each parameterization across krill movement (no movement, n, and full movement as passive drifters, m) and predator sensitivity to krill availability (hyperstable, s, and linear, l). 

<br><b>GPRCP26CHL100, GPRCP85CHL100</b>: Input files for driving krill gross growth potential (GGP) for the two climate change scenarios, in this case a IPCC Representative Control Pathway (RCPs) projections, RCP 2.6 and RCP 8.5. 

<br>
<h3>(2) CODE FOR ANALYSIS</h3>
The analysis utilizes several functions, some of which call the ecosystem model as described in Watters et al. 2013. Detail on that model is available in that documentation.
  
<br>
<h4>Running the ecosystem model analysis across scenarios (Table 1 in the text): </h4>

<b>load.funcs()</b> Function to load and source all functions in a folder (use to easily get all necessary functions into the R workspace) 

<b>import.all.parameters()</b> Function to load the KPFM2 input files (in this case, mlt_new, mst_new, nlt_new, nst_new). The function call is: 

```import.all.parameters <- function(file.string="[location of .txt input file]", foraging.prefix="p")```

<b>ggp_func()</b> Function to run simulations of the KPFM2 ecosystem model (described in Watters et al. 2013) iteratively to compare outcomes of climate change, fishing, and both climate change and fishing scenarios to a reference (‘base case’) scenario. This calls the ssmu.ss.r() and ssmu.mc.r() code for each scenario across the number of Monte Carlo trials specified by the analyst (in our case, 1001). This function was also used to update the foraging of penguins, seals, and whales from the most recent tracking data, as well as for most recent distribution of fishing effort. The function call is: 
```
ggp_func<-function(p1=, MC= , COMPETITION= , GGP1= , NTRIALS= , NYEARS= , SD.KRILL.RDEV= , FISHING.OPTION= , GAMMA= , START.YR=  ,STOP.YR= , BT.ABUND.VAR= ){
```
 `p1= [INPUT FILE]` <em>Input file to run KPFM2 (e.g. “mlt_new”)</em><br>
 `MC= [TRUE/FALSE]` <em>Whether or not to run Monte Carlo simulations</em><br>
 `COMPETITION= [TRUE/FALSE]` <em>Whether or not to run competition within the KPFM2 model</em><br>
 `GGP1=[INPUT FILE]` <em>Gross growth potential inputs to run climate change scenario</em><br>
 `NTRIALS=` <em>Number of trials if calling MC simulations</em><br> 
 `NYEARS=` <em>Number of years to run the simulation</em><br>
 `SD.KRILL.RDEV=` <em>Parameter for deviates of krill recruitment</em><br>
 `FISHING.OPTION= [1-6]` <em>Fishing option, 1 through 6, for KPFM2 to call</em><br>
 `GAMMA= `<em>verall exploitation rate</em><br>
 `START.YR=` <em>When to start fishing (can be NULL if no fishing)</em><br>
 `STOP.YR=` <em>When to stop fishing (NULL if fishing is not stopped during the run)</em><br> 
 `BT.ABUND.VAR= [TRUE/FALSE]` Whether<em> to include random variance in krill abundance in the boundary areas.</em><br>

<br>This script calls the following additional functions: <br>
`ssmu.ss():` The KPFM2 ecosystem model. <br>
`plot.ss.calendar():` Plot of input data trajectories to ensure correct model fit. <br>
`ssmu.mc():` Monte Carlo simulations of the KPFM2 model. <br>

The KPFM2 model (`ssmu.ss`) calls the additional functions: <br>
`m2f.root.r():` Estimates root of catch equation 	<br>
`bt.abund,var():` Generates the variable bathtub krill abundances <br>
`allocate.catch():` Generalized function to allocate catch among areas <br> 

<br>**Convention for naming output**:`[parameterization]_[GGP]_[fishingoption].`
Additional _mc added to denote Monte Carlo simulations. For example: 
`mst_l_f1`: Parameterization = movement + stable, GGP = low (RCP 2.6), fishing option = 1, single trial.
`nlt_h_f1_mc`: Parameterization = no movement + linear, GGP  = high (RCP 8.5), fishing option = 1, Monte Carlo run.


<h4>Functions to organize output for figures: </h4>

`get_abund()`: Determines the final krill biomass and predator abundance at the end of a model run, averaging across the four model parameterizations for each parameterization in a Monte Carlo trial, and then across all Monte Carlo trials. Output from this code can then be assessed to determine risks reported in the manuscript using the `get_marg_perc()` and `get_stats()` functions. Output save automatically: `krill_biom` and `preds_abund`.

`get_marg_perc()`: Assesses the percentage of time biomass of krill or abundance of predators fell below 75% of the base case biomass or abundance (here termed depletion risk) as a 30-year average at end of model run. Input needed from `get_abund()`, and analyst determines whether or not results are by SSMU (`SSMU=TRUE`). Output save automatically: `all_perc` for all areas averaged and/or by ssmu, `krill/pengs/seals/whales/fish_ssmu_perc`. 

`get_stats(`): Determines statistics (mean, median, variance, standard deviation) for 30-year averaged depletion of krill biomass and predator abundance given the base case. Inputs are from `get_marg_per()` for krill and predators, analyst determines whether or not result are by SSMU (SSMU=TRUE). Output saved automatically: `krill_stats` and `pred_stats`, and if SSMU=TRUE, output for risks associated with fishing by predator group: krill/pengs/seals/fish_SSMU_Fishstats, for climate change (GGP): `krill/pengs/seals/fish_SSMU_GGPstats`, and for climate change and fishing (GGPF): `krill/pengs/seals/fish_SSMU_GGPFstats`.

`get_wbar_ave()`: Returns the average mean and standard deviation for the krill wbar parameter for the final 30 years of the simulation. 


<h4>Additional plotting function</h4>

`ggp_margins_mc_abund()`: Function to plot and map the changes in krill biomass and predator abundance over the base case scenario. Two plots are generated: (1) box-and-whisker plot of relative change over the base case (not used in this manuscript), and (2) if analyst specifies `MAP = TRUE`, the relative changes are mapped on to the Scotian Sea small-scale management units (SSMUs) (e.g. Figure 4 in the main text, and S1 an S2 in Supporting Information). <br>
Packages needed: lattice, sp, mapproj, fields, RColorBrewer <br>
Additional input files needed: “`ssmu.basic.coords`” and “`shoreline`”<br>
Additional scripts called: 'median.func()` and `plot_basic_polygons()` (Creates the small-scale management unit polygons).<br>

<br><h3>(3) STEPS FOR ANALYSIS</h3>
 1. Open R Studio, ensure all scripts are loaded and so urced. Load needed input files. 
    1. Load and source functions using load.funcs(). The call is: 
        ```
        > load.funcs(path.string = “[location of folder]”)
        ```
    1. Load input for the KPFM2 model using import.all.parameters() as follows:
       1. Update the import.all.parameters() code to reference where the .txt file is each time, source and save, and run with the desired output name: 
       ```
       > mlt <-import.all.parameters(file.string=”[location of .txt file]”)
        [1] "The setup says you have 15 SSMU(s), 3 BATHTUB(s), 38 YEAR(s) of time series data, and 2 SEASON(s) specified"
        ```
       2. Repeat for each of the four parameterization input files, mlt_new, mst_new, nlt_new, nst_new. The response will tell the analyst how many small-scale management units (SSMUs), bathtubs, years, and seasons (in this case, should be 15, 3, 38, and 2, as above).
1. Run ggp_function() for each parameterization, e.g.:<br>
    <em>mlt with RCP 2.6</em>
    ```
    mlt_l_f1_mc<-ggp_func(p1=mlt, MC=TRUE, COMPETITION=FALSE, GGP1=GGPRCP26CHL100, NTRIALS=1001, NYEARS=93, SD.KRILL.RDEV=0.7, FISHING.OPTION=1, GAMMA=0.01, START.YR=1, STOP.YR=NULL, BT.ABUND.VAR=TRUE)
    ```
    <em>mlt with RCP 8.5</em>
    ```
    mlt_h_f1_mc<-ggp_func(p1=mlt, MC=TRUE, COMPETITION=FALSE, GGP1=GGPRCP85CHL100, NTRIALS=1001, NYEARS=93, SD.KRILL.RDEV=0.7, FISHING.OPTION=1, GAMMA=0.01, START.YR=1, STOP.YR=NULL, BT.ABUND.VAR=TRUE)
    ```
    <em>mst with RCP 2.6</em>
    ```
    mst_l_f1_mc<-ggp_func(p1=mst, MC=TRUE, COMPETITION=FALSE, GGP1=GGPRCP26CHL100, NTRIALS=1001, NYEARS=93, SD.KRILL.RDEV=0.7, FISHING.OPTION=1, GAMMA=0.01, START.YR=1, STOP.YR=NULL, BT.ABUND.VAR=TRUE)
    ```
    <em>mst with RCP 8.5</em>
    ```
    mst_h_f1_mc<-ggp_func(p1=mst, MC=TRUE, COMPETITION=FALSE, GGP1=GGPRCP85CHL100, NTRIALS=1001, NYEARS=93, SD.KRILL.RDEV=0.7, FISHING.OPTION=1, GAMMA=0.01, START.YR=1, STOP.YR=NULL, BT.ABUND.VAR=TRUE)
    ```
    <em>nlt with RCP 2.6</em>
    ```
    nlt_l_f1_mc<-ggp_func(p1=nlt, MC=TRUE, COMPETITION=FALSE, GGP1=GGPRCP26CHL100, NTRIALS=1001, NYEARS=93, SD.KRILL.RDEV=0.7, FISHING.OPTION=1, GAMMA=0.01, START.YR=1, STOP.YR=NULL, BT.ABUND.VAR=TRUE)
    ```
    <em>nlt with RCP 8.5</em>
    ```
    nlt_h_f1_mc<-ggp_func(p1=nlt, MC=TRUE, COMPETITION=FALSE, GGP1=GGPRCP85CHL100, NTRIALS=1001, NYEARS=93, SD.KRILL.RDEV=0.7, FISHING.OPTION=1, GAMMA=0.01, START.YR=1, STOP.YR=NULL, BT.ABUND.VAR=TRUE)
    ```
    <em>nst with RCP 2.6</em>
    ```
    nst_l_f1_mc<-ggp_func(p1=nst, MC=TRUE, COMPETITION=FALSE, GGP1=GGPRCP26CHL100, NTRIALS=1001, NYEARS=93, SD.KRILL.RDEV=0.7, FISHING.OPTION=1, GAMMA=0.01, START.YR=1, STOP.YR=NULL, BT.ABUND.VAR=TRUE)
    ```
    <em>nst with RCP 8.5</em>
    ```
    nst_h_f1_mc<-ggp_func(p1=nst, MC=TRUE, COMPETITION=FALSE, GGP1=GGPRCP85CHL100, NTRIALS=1001, NYEARS=93, SD.KRILL.RDEV=0.7, FISHING.OPTION=1, GAMMA=0.01, START.YR=1, STOP.YR=NULL, BT.ABUND.VAR=TRUE)
    ```
   
   1. The script automatically plots the initial period of the time series against known targets as a check of model fit. These plots are not needed - analyst can check these plots if they would like and close them.
   1. Recall the function automatically runs a base case scenario without fishing or climate change as well as one with climate change alone, fishing alone, and climate change plus fishing. 
 
1. Run `get_abund()`.
   1. Output for krill (`krill_biom`) and predators (`preds_abund`) are saved automatically to global environment
    
1. Assessment of the output from (4) above is done iteratively **by RCP pathway**. It is **critical** the analyst (1) renames the output each time, and (2) updates the code itself where noted:  
   1. Run get_marg_per() – make sure to **update which RCP at line 15 and 50**: 
		```
        > get_marg_perc(krill_biom, preds_abund, SSMU=TRUE)
        ```
      1. **Rename output** for *each* species group and the total, e.g. 
		```
        > all_perc_26 <- all_perc
		> krill_ssmu_perc_26 <- krill_ssmu_perc
        ```
   1. Run `get_stats()` – make sure to **update which  RCP at line 16 and 65**:
 	   ```
       > get_stats(krill_biom, preds_abund, SSMU=TRUE)
       ```
      1. **Rename output**, e.g.
      ```
		> pred_stats_26 <- pred_stats
		> krill_stats_26 <- krill_stats
      ```
   1. Run `get_wbar_ave()` for each of the four parameterizations individually, and make sure to **update where the files should be saved** (line 260).
1. Save output to an Excel spreadsheet for plotting to create figures in the paper. 
1. For map figures (fig 4, S1 and S2 in manuscript): 
   1. Run `ggp_margins_mc_abund()` by scenario, climate change  (GGP) alone, fishing alone, or climate change and fishing, and call MAP = TRUE, e.g.: 
```
>  ggp_margins_mc_abund(MAP=TRUE, MARGIN="GGP")
```

<br>For this paper, the above was re-run with fishing is stopped in year 20, 45, and 70 of the model run. For this, re-run above but change STOP.YR= under ggp_func() to 20, 45, and 70 for each. 

