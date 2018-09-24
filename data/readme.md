
### This folder includes data files which are ready to be used, containing lat/long values.

* ZillowNeighborhoods-OH Folder - Folder containing .shp files for use

* CinciNeighborhoodWalkScore.csv - Rankings of neighborhoods in Cincinnati.
	* Variables of Interest: 
		* Walk Score: A measurement from 0 to 100, where 100 is best. Describes the walkability of citizens to ameneties.
		* Transit Score: A measurement from 0 to 100, where 100 is best. Describes the availability and usefullness of publix transit.
		* Bike Score:  A measurement from 0 to 100, where 100 is best. Describes how good an area is for biking.
		* Population: Population count in the specified neighborhood.
	* Dataset Size: 48 x 6

* cincinnati_311_non_emergency_service_requests.csv - Located within the zip file to save space, this dataset contains records of all non-emergency service requests to the city of Cincinnati.
	* Time: 2012-2018
	* Variables of Interest:
		* SERVICE_CODE: Code used to describe reason for calling.
		* AGENCY_RESPONSIBLE: Public agency responsible for handling the service.
	* Dataset Size: 659,605 x 19
		
* cincy_zip_codes.txt - A list of all zip codes which are part of Cincinnati. If you have a zip code which is not on this list, you can safely delete it from your dataset. Note that some zip codes are partly within the city, and partly outside of the city.
	* Dataset Size: 32 x 1

* grid_centroids_250m.csv - Geo-coordinates for all cell centroids for the grid.
	* Created by: geo_gridder.R
	* Dataset Size: 11,583 x 5

* grid_points_250m.csv - Geo-coordinates for a 250m x 250m grid encompassing the area that bounds Cincinnati. 
	* Created by: geo_gridder.R
	* Dataset Size: 11,782 x 4

* grid_points_250m_w_neighborhood.csv - Geo-coordinates for a 250m x 250m grid encompassing the area that bounds Cincinnati. Additionally contains cell_id, State, County, City, (Neighborhood) Name, and RegionID
	* Created by: quel_quartier.R 
	* Dataset Size: 4,197 x 10
	
* hamilton_county_property_xfer_2008t2018_geocoded.txt - Subset of hamilton_county_property_xfer_2008t2018.csv in .txt format
	* Dataset Size: 123,150 x 7
	
* hc_xfer_unique_100k.txt - Text file subset of all unique property transfers from hamilton_county_property_xfer_2008t2018.csv.
	* Dataset Size: 101,339 x 7
	
* pat_addresses_geocoded.txt - A useful tool for Pat's stalkers.
	* Dataset Size: 10 x 6

* pedestrian_near_miss_incidents_geocodes.csv - Geo-coded data from pedestrian_near_miss_incidents_2018.08.22.csv
	* Time: 2017-2018
	* Dataset Size: 85 x 4
	
* pedestrian_survey_final_20180618_geocoded.txt - Geo-coded data from PedSaftey.csv
	* Time: 02/2018 - 04/2018
	* Dataset Size: 1,919 x 7
	
* pedestrian_survey_w_neighborhood.csv - Geo-coded data from PedSaftey.csv with neighborhood.
	* Time: 02/2018 - 04/2018
	* Dataset Size: 1,372 x 12
	
* PedSafetyCleaned.csv 	- Tidied up version of PedSafety.csv
	* Time: 02/2018 - 04/2018
	* Dataset Size: 2,699 x 13
	
* roadkill.csv - Subset of cincinnati_311_non_emergency_service_requests.csv consisting of DAPUB1 SERVICE_CODE values.
	* Time: 2012-2018
	* Dataset Size: 10,981 x 7
	* Note: May be able to enhance by creating animal type variables.

* street_centerlines_w_pci_rating.csv - City street centerline data with pavement condition index (PCI) ranking.
	* Time: Present (updated daily at https://data.cincinnati-oh.gov/Fiscal-Sustainability-Strategic-Investment/Street-Centerlines-w-PCI-rating-/574p-8utc)
	* Dataset Size: 88,879 x 35
	* Variables of Interest:
		* LANE_CNT: Number of lanes on road.
		* _: Assumed to be the pavement condition index, a numerical index between 0 and 100, where 100 is best.
		* SURFACE:
			* AC: Asphalt Concrete
			* BR: (Not Brick)??
			* GR: Gravel
			* PCC: Portland Cement Concrete
			* PVB:
			* X: 
			
* traffic_crash_reports_20180918.csv - Traffic crash reports from the Cincinnati police department. 
	* Time: 2012-2018 (updated daily at https://data.cincinnati-oh.gov/Safer-Streets/Traffic-Crash-Reports-CPD-/rvmt-pkmq)
	* Dataset Size: 193,006 x 25
	* Variables of Interest: 
		* CRASHSEVERITYID:
			* 1: Fatal Injury
			* 2: Injury
			* 3: Property Damage Only
		* INJURIES:
			* 1: No Injury
			* 2: Possible Injury
			* 3: Non-Incapacitating Injury
			* 4: Incapacitating Injury
			* 5: Fatal Injury
		* LIGHTCONDITIONSPRIMARY
		* ROADCONDITIONSPRIMARY
		* ROADCONTOUR
		* ROADSURFACE
		* TYPEOFPERSON:
			* D - DRIVER
			* O - OCCUPANT
			* P - PEDESTRIAN (!!)

* WalkScore.csv - Walk score values of various addresses. **Need to check if within city bounds.**
	* Created by: CreateScoreFile.ipynb
	* Dataset Size: 22,790 x 11


		