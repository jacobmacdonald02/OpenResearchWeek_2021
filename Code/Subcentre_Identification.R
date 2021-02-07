########################################################################################
########################################################################################
########  Employment Subcentre Identification 
#######     Update to the method of Exponentially Decaying Thresholds (Bin et al. 2017)
######
#####     Jacob L. Macdonald
####      Last Updated: January, 2021
###
##  Function to identify the set of candidate and full employment subcentres based on the
##   methodology outlined in Bin et al. (2017). Updates include different functional forms
##   for the employment density gradient estimation and overall cleaning and efficiency 
##   updates.
##    
##  Notes: 1) The area variable should be present already and specified as hectares
##         2) Distances in the function are measured in kilometres
##  
##  Inputs:
##    GEOGRAPHIES  - sf object of the small area geographies with employment and area data
##    VARS         - character vector of the variables names in the spatial object which 
##                   correspond respectively to 1) the tract ID; 2) the employment levels; 
##                   and 3) area
##    GRADIENT     - the functional form name of employment density gradient to estimate
##    CBD.cutD     - 
##    CBD.cutE     -
##    THETA        - 
##    DISSOLVE     - whether to dissolve contiguous SC tracts and candidates into one


subcentre.identification <- function(GEOGRAPHIES, 
                                     VARS=c("WZ_CD", "employment", "area"), 
                                     GRADIENT=c("Linear", "Squared", "Cubed", "Log"), 
                                     CBD.cutD, 
                                     CBD.cutE, 
                                     THETA=1, 
                                     DISSOLVE=FALSE){
  require(numDeriv)
  require(sf)
  require(data.table)
  require(dplyr)
  require(tidygraph)
  require(stringr)
  require(igraph)
  
  ## Rename the specified tract ID, employment and area variables
  names(GEOGRAPHIES)[names(GEOGRAPHIES)==VARS[1]] <- "tractID"
  names(GEOGRAPHIES)[names(GEOGRAPHIES)==VARS[2]] <- "employment"
  names(GEOGRAPHIES)[names(GEOGRAPHIES)==VARS[3]] <- "area"
  GEOGRAPHIES <- GEOGRAPHIES[,c("tractID", "area", "employment")]
  
  GEOGRAPHIES$density <- GEOGRAPHIES$employment/GEOGRAPHIES$area
  GEOGRAPHIES$density <- ifelse(is.na(GEOGRAPHIES$density), 0, GEOGRAPHIES$density)
  
  GEOGRAPHIES$distance <- as.numeric(st_distance(st_transform(st_as_sf(st_centroid(st_geometry(GEOGRAPHIES))), crs=4326), 
    st_transform(st_as_sf(st_centroid(st_geometry(GEOGRAPHIES[which.max(GEOGRAPHIES$density),]))), crs=4326), which ="Great Circle"))*0.001
  GEOGRAPHIES$distance.sq <- GEOGRAPHIES$distance*GEOGRAPHIES$distance
  GEOGRAPHIES$distance.cb <- GEOGRAPHIES$distance.sq*GEOGRAPHIES$distance
  
  if(GRADIENT=="Linear"){
      estimates <- lm(log(ifelse(GEOGRAPHIES$density > 0, GEOGRAPHIES$density, 0.5)) ~ GEOGRAPHIES$distance)
      fn <- function(x) as.numeric(summary(estimates)$coefficients[1,1]) + as.numeric(summary(estimates)$coefficients[2,1])*x
      gradient <- data.frame(cbind(gradient=grad(fn, x=GEOGRAPHIES$distance), tractID=GEOGRAPHIES$tractID))
  } else if(GRADIENT=="Squared"){
      estimates <- lm(log(ifelse(GEOGRAPHIES$density > 0, GEOGRAPHIES$density, 0.5)) ~ GEOGRAPHIES$distance + GEOGRAPHIES$distance.sq)
      fn <- function(x) as.numeric(summary(estimates)$coefficients[1,1]) + as.numeric(summary(estimates)$coefficients[2,1])*x + as.numeric(summary(estimates)$coefficients[3,1])*(x^2)
      gradient <- data.frame(cbind(gradient=grad(fn, x=GEOGRAPHIES$distance), tractID=GEOGRAPHIES$tractID))
  } else if(GRADIENT=="Cubed"){
      estimates <- lm(log(ifelse(GEOGRAPHIES$density > 0, GEOGRAPHIES$density, 0.5)) ~ GEOGRAPHIES$distance + GEOGRAPHIES$distance.sq + GEOGRAPHIES$distance.cb)
      fn <- function(x) { as.numeric(summary(estimates)$coefficients[1,1]) + as.numeric(summary(estimates)$coefficients[2,1])*x + as.numeric(summary(estimates)$coefficients[3,1])*(x^2) + 
          as.numeric(summary(estimates)$coefficients[4,1])*(x^3) }
      gradient <- data.frame(cbind(gradient=grad(fn, x=GEOGRAPHIES$distance), tractID=GEOGRAPHIES$tractID))
  } else if(GRADIENT=="Log"){
      estimates <- lm(log(ifelse(GEOGRAPHIES$density > 0, GEOGRAPHIES$density, 0.5)) ~ log(ifelse(GEOGRAPHIES$distance > 0, GEOGRAPHIES$distance, 0.5)))
      fn <- function(x) as.numeric(summary(estimates)$coefficients[1,1]) + as.numeric(summary(estimates)$coefficients[2,1])*log(x)
      gradient <- data.frame(cbind(gradient=grad(fn, x=ifelse(GEOGRAPHIES$distance==0, 0.01, GEOGRAPHIES$distance)), tractID=GEOGRAPHIES$tractID))
  }
  
  gradient$gradient <- as.numeric(as.character(gradient$gradient))
  GEOGRAPHIES <- merge(GEOGRAPHIES, gradient, all.x=T, sort=F, by="tractID")
  rm(gradient, fn, estimates)
  
  GEOGRAPHIES$Dcutoff <- mean(GEOGRAPHIES$density[GEOGRAPHIES$distance < CBD.cutD], na.rm=T)*exp(THETA*GEOGRAPHIES$gradient*GEOGRAPHIES$distance)
  GEOGRAPHIES$candidate <- ifelse(GEOGRAPHIES$density > GEOGRAPHIES$Dcutoff, 1, 0)

  spare_mtx <- st_relate(GEOGRAPHIES, GEOGRAPHIES, pattern = "F***T****")
  GEOGRAPHIES$neighbours <- as.character(do.call(rbind, lapply(spare_mtx, function(x) paste0(as.character(GEOGRAPHIES[x,]$tractID), collapse = "; "))))
  
  GEOGRAPHIES$N.nb <- as.numeric(do.call(rbind, lapply(str_split(GEOGRAPHIES$neighbours, "; "), length)))
  GEOGRAPHIES$N.candidate.nb <- as.numeric(do.call(rbind, lapply(str_split(GEOGRAPHIES$neighbours, "; "), function(x) sum(GEOGRAPHIES[GEOGRAPHIES$tractID %in% c(x),]$candidate))))
  GEOGRAPHIES$candidate[which(GEOGRAPHIES$N.candidate.nb==GEOGRAPHIES$N.nb & GEOGRAPHIES$candidate==0)] <- 1
  GEOGRAPHIES$N.candidate.nb <- NULL
    
  candidates <- GEOGRAPHIES[GEOGRAPHIES$candidate==1,]
  spare_mtx <- st_relate(candidates, candidates, pattern = "F***T****")
  candidates$neighbours <- as.character(do.call(rbind, lapply(spare_mtx, function(x) paste0(as.character(candidates[x,]$tractID), collapse = "; "))))
  candidates$neighbours[candidates$neighbours==""] <- NA
  
  candidates.network <- do.call(rbind, lapply(as.list(1:length(candidates$tractID)), function(x) { as.data.frame(cbind(tractID=candidates[x,]$tractID, 
    Neighbour=unlist(str_split(candidates[x,]$neighbours, "; ")))) }))
  
  candidates.network1 <- candidates.network[!is.na(candidates.network$Neighbour),]
  candidates.network2 <- candidates.network[is.na(candidates.network$Neighbour),]
  
  candidates.graph <- as_tbl_graph(candidates.network1, directed=FALSE)
  candidates.graph <- split(names(V(candidates.graph)), components(candidates.graph)$membership)
  
  candidates.N <- lapply(as.list(1:length(candidates.graph)), function(x) paste0("candidateID_", names(candidates.graph[x])))
  candidates.graph <- as_tibble(do.call(rbind, lapply(as.list(1:length(candidates.graph)), function(x) cbind(candidates.N[[x]], candidates.graph[[x]])))) %>%
    filter(!is.na(V1) & !is.na(V2) & V1!="" & V2!="") %>%
    unique()
  names(candidates.graph) <- c("candidateID", "tractID")
  
  candidates.graph <- candidates.graph %>%
    group_by(candidateID) %>%
    add_count(name = "tractN") %>%
    unique()
  
  candidates.network2 <- as_tibble(cbind(tractID=as.character(candidates.network2$tractID), candidateID=paste0("candidateID_", 1:length(candidates.network2$tractID) + max(unique(as.numeric(as.character(do.call(rbind, lapply(str_split(candidates.graph$candidateID, "_"), function(x) x[2]))))))), tractN=1))
  candidates.network2$tractN <- as.numeric(candidates.network2$tractN)
  
  candidates.graph <- candidates.graph %>%
    bind_rows(candidates.network2)
  
  GEOGRAPHIES <- merge(GEOGRAPHIES, data.frame(candidates.graph), all.x=T, sort=F, by="tractID")
  
  GEOGRAPHIES <- GEOGRAPHIES %>%
    group_by(candidateID) %>%
    mutate(SC_employment=sum(employment), SC_area=sum(area), SC_distance=(employment*ifelse(distance==0, 0.01, distance))/employment, SC_density=SC_employment/SC_area) %>%
    arrange(candidateID)
  GEOGRAPHIES <- st_sf(data.frame(GEOGRAPHIES))
  GEOGRAPHIES$SC_employment[is.na(GEOGRAPHIES$candidateID)] <- NA
  GEOGRAPHIES$SC_area[is.na(GEOGRAPHIES$candidateID)] <- NA
  GEOGRAPHIES$SC_distance[is.na(GEOGRAPHIES$candidateID)] <- NA
  GEOGRAPHIES$SC_density[is.na(GEOGRAPHIES$candidateID)] <- NA
  
  GEOGRAPHIES$Ecutoff <- ifelse(GEOGRAPHIES$candidate==1, sum(GEOGRAPHIES$employment[GEOGRAPHIES$distance < CBD.cutE], na.rm=T)*exp(THETA*GEOGRAPHIES$gradient*GEOGRAPHIES$SC_distance), NA)
  GEOGRAPHIES$subcentre <- ifelse(GEOGRAPHIES$candidate==1, ifelse(GEOGRAPHIES$SC_employment > GEOGRAPHIES$Ecutoff, 1, 0), NA)
  GEOGRAPHIES$subcentreID <- ifelse(GEOGRAPHIES$subcentre==1, gsub("candidate", "subcentre", GEOGRAPHIES$candidateID), NA)
  
  GEOGRAPHIES.dissolved <- GEOGRAPHIES[!is.na(GEOGRAPHIES$subcentreID),] %>%
    st_transform(crs=27700) %>%
    st_make_valid() %>%
    st_buffer(dist = 0.1) %>%
    dplyr::group_by(subcentreID) %>%
    dplyr::summarize() %>%
    smoothr::fill_holes(threshold = units::set_units(2000, km^2)) %>%
    st_sf() %>%
    unique()
  
  GEOGRAPHIES.fill <- st_join(st_transform(st_as_sf(st_centroid(st_geometry(GEOGRAPHIES[GEOGRAPHIES$candidate==0,]))), crs=4326), 
    st_transform(GEOGRAPHIES.dissolved, crs=4326), join = st_intersects)
  GEOGRAPHIES.fill$tractID <- GEOGRAPHIES[GEOGRAPHIES$candidate==0,]$tractID
  
  GEOGRAPHIES.fill <- as_tibble(GEOGRAPHIES.fill) %>%
    select(subcentreID, tractID) %>%
    filter(!is.na(subcentreID)) %>%
    unique()
  GEOGRAPHIES[GEOGRAPHIES$tractID %in% GEOGRAPHIES.fill$tractID,]$subcentreID <- GEOGRAPHIES.fill$subcentreID
  
  GEOGRAPHIES <- GEOGRAPHIES %>%
    select(-c("tractN", "SC_employment", "SC_area", "SC_distance", "SC_density")) %>%
    group_by(subcentreID) %>%
    mutate(SC_employment=sum(employment), SC_area=sum(area), SC_distance=(employment*ifelse(distance==0, 0.01, distance))/employment, SC_density=SC_employment/SC_area) %>%
    group_by(subcentreID) %>%
    add_count(name = "tractN")
  GEOGRAPHIES <- st_sf(data.frame(GEOGRAPHIES))
  GEOGRAPHIES$SC_employment[is.na(GEOGRAPHIES$subcentreID)] <- NA
  GEOGRAPHIES$SC_area[is.na(GEOGRAPHIES$subcentreID)] <- NA
  GEOGRAPHIES$SC_distance[is.na(GEOGRAPHIES$subcentreID)] <- NA
  GEOGRAPHIES$SC_density[is.na(GEOGRAPHIES$subcentreID)] <- NA
  GEOGRAPHIES$tractN[is.na(GEOGRAPHIES$subcentreID)] <- NA

  if(DISSOLVE==TRUE){
    GEOGRAPHIES.dissolved <- GEOGRAPHIES[!is.na(GEOGRAPHIES$subcentreID),] %>%
      st_transform(crs=27700) %>%
      st_make_valid() %>%
      st_buffer(dist = 0.1) %>%
      dplyr::group_by(subcentreID) %>%
      dplyr::summarize() %>%
      smoothr::fill_holes(threshold = units::set_units(2000, km^2)) %>%
      st_sf() %>%
      unique()
    GEOGRAPHIES.dissolved.x <- as_tibble(GEOGRAPHIES[!is.na(GEOGRAPHIES$subcentreID),]) %>%
      select(subcentreID, SC_employment, SC_area, SC_distance, SC_density, tractN) %>%
      unique()
    GEOGRAPHIES.dissolved <- left_join(GEOGRAPHIES.dissolved, GEOGRAPHIES.dissolved.x)
    
    return(GEOGRAPHIES.dissolved)
    
  } else if(DISSOLVE==FALSE){
    
    return(GEOGRAPHIES) }
}