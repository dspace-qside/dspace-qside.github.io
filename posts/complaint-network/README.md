This blog post copies the workflow from the private repo in order to share publicly. Within the /complaint_network post folder, the complaint_scraping folder contains the workflow to update the data scraping. This is still live for demonstration and transparency but must be updated to mirror the private repo's latest complaint scraping code. Run the scraping in the private repo to get the new_complaints.csv file, which should be placed in the /data folder here. 

To update the data from the blog post: 
- copy new_complaints.csv from private repo into /data
- ensure MPD_roster_2023-2025_Public.csv exists in /data
- open index.Rmd in RStudio and run, which creates network_files.Rdata
- copy network_files.Rdata into /complaint_network folder for shiny app
- run complaint_network/server.R in RStudio
- deploy/update latest version of shiny app
- update date-modified in header metadata in index.qmd
  
