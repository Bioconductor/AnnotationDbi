## Generic code to createAnnObjs for bimaps of NCBIORG packages

## This file will define what it means to be MOUSE_DB or HUMAN_DB etc. as a
## series of functions named like createAnnObjs.MOUSE_DB() etc.


## Start with a huge named list of possible mappings.



## Then some helper functions to take a list of arguments and convert them into the ann_objs object that is returned by each function

## helper to make MOUSE_DB_AnnDbBimap_seeds from the big list and a list of mappings that are needed

## helper to make revmaps (based on that same list and the maps that are sometimes reversed)

## another helper to call both those helpers and any other stuff so that the guts of each createAnnObjs.XXX_DB function are as short as possible.



## Then define each function briefly like:


## createAnnObjs.MOUSE_DB <- function(prefix, objTarget, dbconn, datacache){
##   ## helper function goes here.
## }
