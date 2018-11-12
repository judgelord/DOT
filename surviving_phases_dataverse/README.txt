	Replication do-file
	Metzger and Jones, "Surviving Phases", PA
	
	24JUN16
	
	DESCRIPTION:
		See "__master - PA replication (read me!).do" for replication code to generate the 
		paper's 2nd application	dataset, and to	generate all figures and tables in the paper.  
		If the relevant table/figure is generated in R, the do-file notes the R file's name.					
		
	REQUIRED PACKAGES:				
		* four user-written ado files (copy all ado and sthlp files in replication folder to local ado folder)
			// To find loc of local ado folder, type: di "`c(sysdir_personal)'"			

	REQUIRED SETUP:
		* Set working directory in line 18 of do-file.  
		* Each subsequent code block in do-file is written independently, and can 
		  then be run by highlighting the entire relevant block and executing.
		  
	SOFTWARE VERSIONS:
		We used the following software and packages to generate our tables and figures.
		* Stata 14.1
		* R 3.3.1
			* foreign, v0.8-66
			* msm, v1.6.1
			* mstate, v0.2.9
			* survival, v2.39.5 (!! Important.)
			

			