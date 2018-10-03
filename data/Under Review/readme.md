
### This folder includes data files which are in need of work.

* 2017_NFIRS_Cincinnati_Fire_Department_Incident_Data.csv - National Fire Incident Reporting System dataset which reports on the full range of fire deparment activities.
	* To-Do: Dataset contains 4219 missing pairs of lat/long data. Contains street addresses from which lat/long data can be derrived.
	* Time: 2017
	* Variables of Interest: 
		* Suppression_app_count: # of fire vehicles that responded
		* Suppression_per_count: # of fire employees that responded
		* Ems_app_count: # of emergency medical vehicles that responded
		* Ems_per_count: # of emergency medical employees that responded
		* Other_app_count: # of other vehicles that responded
		* Other_per_count: # of other employees that responded
	* Dataset Size: 33,404 x 27

* hamilton_county_property_xfer_2008t2018.csv - Property transfer transaction data in Hamilton county.
	* To-Do: 7,847 rows with missing / difficult street numbers. No lat/long. 
	* Time: 2012 - 2018
	* Variables of Interest:
		* Year of Sale
		* Sale Price
	* Dataset Size: 219,943 x 25

* MasterScores.csv - Contains walk score values for various streets.
	* To-Do: Remove cities which are not Cincinnati from dataset. Decide what to do with addresses that have no walk score.
	* Variables of Interest:
		* Walk Score
		* Bike Score
		* Transit Score
	* Dataset Size: 10,681 x 12
		
* pedestrian_near_miss_incidents_2018.08.22.csv	- Near miss incidents within the city. Provided by the city.
	* To-Do: Generate lat/long from street addresses. (Done. See: pedestrian_near_miss_incidents_geocodes.csv)
	* Time: 2017-2018
	* Dataset Size: 85 x 30
	
* PedSafety.csv - Dataset given by city which contains values from the February 2018 to April 2018 online pedestrian safety survey.
	* To-Do: Generate lat/long from streets. (Done. See: pedestrian_survey_final_20180618_geocoded.txt)
	* Time: 02/2018 - 04/2018
	* Dataset Size: 3,788 x 13

* PedSafety_070718.xlsx - Appears to be the same as PedSafety.csv (above)
	* To-Do: Review for deletion.
	* Time: 02/2018 - 04/2018
	* Dataset Size: 3,788 x 13

* WalkScoreMasterFileByStreet.csv - More walk score data for streets.
	* To-Do: Format and extension of file do not match. Excel says it's a SYLK file.
	*Dataset Size: 7,993 x 13

	
* hamilton_county_property_xfer_2008t2018 - Hamilton County Auditor table of property transfers from 2008 - 2018, approx 300,000 rows, 200,000 records
* pedestrian_near_miss_incidents_geocodes - geo-coded coordinates for the list of incidents reported to the City of Cincinnati as near-miss occurrences of pedestrian - vehicle events, 84 x 4
* pedestrian_survey_final_20180618_geocoded - list of locations and descriptions identifed in Cincinnati pedestrian safety survey, early 2018, 2,000 x 7
* street_centerlines_w_pci_rating - centerline segments of the streets of Cincinnati, 89,000 x 35
* pat_addresses_geocoded - addresses of note in Cincinnati
* traffic_crash_reports_20180918 - traffic crash reports, Cincinnati safety department, 193,000 x 25
