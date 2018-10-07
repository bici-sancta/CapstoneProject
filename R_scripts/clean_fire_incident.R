
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : clean_fire_incident.R
# ...
# ...   06-oct-2018
# ...
# ...   patrick.mcdevitt@smu.edu
# ...
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

clean_fire_incident <- function(fire_incident)
{

    names(fire_incident) <- tolower(names(fire_incident))
    names(fire_incident)[names(fire_incident) == 'latitude'] <- 'lat'
    names(fire_incident)[names(fire_incident) == 'longitude'] <- 'long'
    
# ...   fire incidents related to vehicle accidents
    
    incident_2_keep <- c(322, 323, 324, 352, 463)
    
    red1 <- fire_incident[fire_incident$incident_type_id == 322,]
    red2 <- fire_incident[fire_incident$incident_type_id == 323,]
    red3 <- fire_incident[fire_incident$incident_type_id == 324,]
    red4 <- fire_incident[fire_incident$incident_type_id == 352,]
    red5 <- fire_incident[fire_incident$incident_type_id == 463,]
    
    fire_incident_red <- rbind(red1, red2, red3, red4, red5)
    
# ...   not all have lat& long in original data file ... remove for now, consider to look up geocoords
# ...   1/2 of motor vehicle accidents DO NOT HAVE LAT/LONG COORDS
    
    fire_incident_red <- fire_incident_red[!is.na(fire_incident_red$lat),]

    return (fire_incident_red)

}

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   end_of_file
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-