# hake-data
<span style="color:green">Updated December 6, 2022</span>

Extract Canadian hake data for annual assessment.

This requires some things be run on a DFO machine or one on the VPN.
You can transfer those files to another computer to do the extraction.

## Steps

### Extract the catch using FOS on Oracle (not GFFOS)

  The reason this method uses the FOS Internet reports and not the GF_FOS database is because at certain times,
  we need the most current catch data to show in public meetings. GF_FOS is not a realtime
  database and the most recent catch is not included in it. It is only re-populated once a week or so.
  This method gets the realtime data from FOS directly.

   - Open Microsoft Edge while on the DFO Intranet or VPN and go to:
     http://paccfprodin/fos2/index.cfm
   - user/pass = **grandinc/g.........3#**
   - Click red "Reports" button

   - **For the landings:**
   - Click "Landings by species by date (DMP)"
   - Type or select "Pacific Hake" from the species dropdown box.
   - Set Start date to April 1, 2007 (Before that catch data were in PacHarvTrawl not FOS).
   - Set End date to today's date.
   - Leave everything else blank.
   - Click "Run Report".
   - It will take a couple of minutes. There is no visible sign that it is doing anything.
     Once finished, the screen will change, giving you a link to open the new file.
     Click the link then Save As.. **LandingsSpeciesDateDMP.csv** in the `data-catch` directory.

   - **For the discards:**
   - On the FOS website, also run the 'Catch by species by Date (LOGS)' report.
   - Note you can only do 1 year at a time for some reason.
   - Leave all areas checked.
   - Make sure 'Fishing/ASOP logs' is checked, not 'Video Catch logs'
   - It will take a couple of minutes. There is no visible sign that it is doing anything.
     Click the link then Save As.. **LogCatchReportXXXX.csv** where **XXXX** is the year,
     in the `data-catch` directory.

  <span style="color:green">At this point, you can copy those files to another computer if that's
  where you plan on running the full extraction code, as long as the files are placed in the
  `data-catch` directory on that system.</span>

### Extract spatial catch data

Run the following line of code from the command line:

`run_spatial_catch_sql()`

This will create three files in the `data-catch` folder called:

`catch-locations-ft.csv`

`catch-locations-ss.csv`

`catch-locations-jv.csv`

  <span style="color:green">At this point, you can copy those CSV files to another computer if that's
  where you plan on running the full extraction code, as long as the files are placed in the
  `data-catch` directory on that system.</span>
  
### Extract samples and calculate age proportions

Open `data-sample/GFBioOracle.mdb` and run the macro called `Fetch Hake biological data`.
It will take a long time (about 45 minutes). It runs code to delete the tables
if they exist and rebuild them from the Oracle data source. There will be
several pop-ups during this time asking if it's ok to append rows, etc. Answer Yes
to all of these.

Once finished, export the two output tables `hake_domestic_obs_len_wt_age` and
`hake_jv_obs_len_wt_age`:

 - export as csv with field named included on first row and Text Qualifier set
   to {none}. The files must be called `hake_domestic_obs_len_wt_age.csv` and
   `hake_jv_obs_len_wt_age.csv`
   
 - the export takes a minute or so for each since there are hundreds of
   thousands of records.

  <span style="color:green">At this point, you can copy those CSV files to another computer if that's
  where you plan on running the full extraction code, as long as the files are placed in the
  `data-sample` directory on that system.</span>
  
  ```diff
  - When you are done and have the CSV files successfully exported, delete the tables
  - inside the Access database and then run the "Database compact and repair" tool. This ensures if you
  - accidentally commit the `data-sample/GFBioOracle.mdb` it will not be massive and ruin your
  - repository. It can be several Gigabytes in size when populated.
 ```

### Extract Weight-at-age

Run the following line of code from the command line:

`run_extra_sample_data()`

This will create a file in the `data-sample` folder called:

`samples-extra.rds`

  <span style="color:green">At this point, you can copy the RDS file to another computer if that's
  where you plan on running the full extraction code, as long as the files are placed in the
  `data-sample` directory on that system.</span>

### Create all the data files and figures used for the December JTC meeting

Assuming you're in the repository's root directory, run the following two lines of code:

`source("R/create-all-data.R")`

`create_all_data()`

Copy/paste all files in the `data-output` directory into the hake assessment repo's `data` directory,
overwriting the ones already there.

Inside the `figures-output` directory, all the figures needed to make the JTC presentation for
the December JTC meeting reside. The can be dragged into a Google slides presentation.
