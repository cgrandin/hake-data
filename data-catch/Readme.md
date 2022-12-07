<span style="color:blue">**How to retrieve and process realtime catch data for Pacific Hake from FOS**</span>

*Chris Grandin*
*Updated December 5, 2022*

The reason this method uses the FOS Internet reports and not the GF_FOS database is because at certain times,
we need the most current catch data to show in public meetings. GF_FOS is not a realtime
database and the most recent catch is not included in it. It is only re-populated once a week or so.
This method gets the realtime data from FOS directly.

 - Open Microsoft Edge (no other browsers work for this) while on the Internet and go to:
 http://paccfprodin/fos2/index.cfm (or http://10.112.10.30/fos2/index.cfm)
   - user/pass = **grandinc/gdc0913#**
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
   Click the link then Save As.. **LandingsSpeciesDateDMP.csv** in this directory.

 - **For the discards:**
   - On the FOS website, also run the 'Catch by species by Date (LOGS)' report.
   - Note you can only do 1 year at a time for some reason.
   - Leave all areas checked.
   - Make sure 'Fishing/ASOP logs' is checked, not 'Video Catch logs'
   - It will take a couple of minutes. There is no visible sign that it is doing anything.
   Click the link then Save As.. **LogCatchReportXXXX.csv** where **XXXX** is the year, in this directory.

 - Open R and source the process-catch-data.r script. This will generate a file called **'Landings_Fleet_Year_Month.csv'**
   which has the landings and discard totals and counts for each fishery type by year and month from 2007-present.

 - To make the plots for cumulative catch and histograms of number of tows by area,
   go to ../CumulativeCatch and read the Readme.md file.
