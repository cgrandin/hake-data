<span style="color:blue">**How to retrieve and process real-time catch data for Pacific Hake from FOS Oracle**</span>

*Chris Grandin*
*Updated December 6, 2022*

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

 - Open R and run the following:
    ```r
    source("create-landings-data-files.R")
    ```
  This will generate the following 4 files in the `data-output` directory:
    
    `can-catch-by-year.csv`
    
    `can-ft-catch-by-month.csv`
    
    `can-jv-catch-by-month.csv`
  
    `can-ss-catch-by-month.csv`
    
 - Copy and paste those files into the `hake-assessment/data` directory, overwriting
   the ones currently there.
    
 - To make the plots for catch barplots by year, area, and month, and the cumulative catch plots,
   run the following:
   ```r
   source("create-spatial-catch-plots.R")
   ```

  This will generate the following 4 files in the `figures-output` directory:
    
    `catch-by-areas-barplot.png`    

    `catch-by-areas-month-barplot.png`

    `catch-by-years-lines.png`


