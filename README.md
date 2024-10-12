To run this project you'll need R (https://www.r-project.org/) and RStudio (now Posit: https://posit.co/downloads/) or some other IDE (e.g. jupyter) to run R.

If you've never used R and RStudio, this may be a bit much. If you just want the data, look in the "DATA" and "export" folders in the github repository. The VOTES12.csv and SONGS12.csv files in the DATA folder include spotify data along with music league votes/rounds data. If you just want the raw music league data (without spotify additions), see the "export" folder tables. 

Once R and RStudio are installed follow these steps: 

1. download the IBDDE zipped repository from github (if you haven't already)
2. unzip
3. open the folder and open IBDDE.Rproj - this will open the project in RStudio.
4. RStudio will likely tell you that you don't have the necessary packages. Tell it to go ahead and install them.
5. In the Global.R, ui.R, or  server.R script, click on "Run App" 

Note: the IBDD_DATA_WRANG.R script in main directory will not work out of the box. You don't need it if you ignore it and Run App. 
  BUT: if you want to run it and pull spotify data yourself, you'll need a spotify developer account and associated access authorization token/secret.
  To do this: https://developer.spotify.com/documentation/web-api/tutorials/getting-started
  Once you have an account and secret, you'll need to reference it in the IBDD_DATA_WRANG.R. 

Another Note: this is a living project/directory, so it will be updated as the project is updated.  


