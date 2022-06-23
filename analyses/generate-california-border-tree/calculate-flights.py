import os
import urllib.request
import json

import pandas as pd

AIRLINE_DB = "https://raw.githubusercontent.com/mwgg/Airports/master/airports.json"
FLIGHTLIST_2019 = [
    "flightlist_20190101_20190131.csv.gz",
    "flightlist_20190201_20190228.csv.gz",
    "flightlist_20190301_20190331.csv.gz",
    "flightlist_20190401_20190430.csv.gz",
    "flightlist_20190501_20190531.csv.gz",
    "flightlist_20190601_20190630.csv.gz",
    "flightlist_20190701_20190731.csv.gz",
    "flightlist_20190801_20190831.csv.gz",
    "flightlist_20190901_20190930.csv.gz",
    "flightlist_20191001_20191031.csv.gz",
    "flightlist_20191101_20191130.csv.gz",
    "flightlist_20191201_20191231.csv.gz",
]
FLIGHTLIST_LOC = "/Users/natem/Documents/flighdata/"
FLIGHTLIST_COLS = ['callsign', 'number', 'typecode', 'origin', 'destination', 'day']
CNTRY_ABBREVIATIONS = "https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv"
CNTRY_COLS = ["Country", "Alpha-2 code"]

def read_airline_location_data():
    """
    Reads in country and state location information for all airlines from mwgg/Airports github repo.
    """
    with urllib.request.urlopen( AIRLINE_DB ) as url:
        icoa_dict = json.loads( url.read().decode() )

    country_dict = dict()
    state_dict = dict()
    for i in icoa_dict:
        country_dict[i] = icoa_dict[i]["country"]
        try:
            state_dict[i] = icoa_dict[i]["state"]
        except KeyError:
            state_dict[i] = "N/A"
    return country_dict, state_dict

def add_location_data( df ):
    """
    Adds state and country location information to flight list. Assumes all airports with ICAO IDs that start with "K"
    are in the US.
    """
    country_dict, state_dict = read_airline_location_data()
    df["origin_country"] = df["origin"].map( country_dict )
    df["origin_state"] = df["origin"].map( state_dict )
    df.loc[df["origin"].str[0] == "K", "origin_country"] = "US"

    return df


def read_flightlist( destination ):
    """
    Reads in flight lists for each file specified in FLIGHTLIST_2019.
    Only returns flights with specified destination and known origin
    """
    temp_dfs = list()
    for fl_file in FLIGHTLIST_2019:
        temp = pd.read_csv( os.path.join( FLIGHTLIST_LOC, fl_file ), usecols=FLIGHTLIST_COLS )

        temp = temp.loc[temp["destination"] == destination]
        temp = temp.loc[~temp["origin"].isna()]

        temp_dfs.append( temp )

    return pd.concat( temp_dfs, ignore_index=True )


def add_full_country_name( df ):
    names = pd.read_csv( CNTRY_ABBREVIATIONS, usecols=CNTRY_COLS ).replace('"','', regex=True)
    names.columns = ["country", "abbreviation"]
    names["abbreviation"] = names["abbreviation"].str.lstrip()
    names = names.sort_values( "country" ).drop_duplicates( subset=["abbreviation"], keep="first" )

    df = df.merge( names, left_on="index", right_on="abbreviation", how="left" )

    df.loc[~df["country"].isna(), "index"] = df["country"]
    df = df.drop( columns=["country","abbreviation"] )

    # Clean=up names too
    df["index"] = df["index"].replace( "-", " ", regex=True )
    df["index"] = df["index"].replace( {'Korea, Republic of': "South Korea"})
    df = df.loc[df["index"]!="N/A"]

    return df

if __name__ == "__main__":
    flights = read_flightlist( "KSAN" )
    flights = add_location_data( flights )
    
    flights = flights.loc[~flights["origin_country"].isna()]
    flights["location"] = flights["origin_country"]
    flights.loc[flights["origin_country"].isin(["US", "MX", "CA"]),"location"] = flights["origin_state"]
    
    flights_summary = flights["location"].value_counts().reset_index()
    flights_summary = add_full_country_name( flights_summary )

    flights_summary.columns = ["origin", "flights"]
    flights_summary = flights_summary.groupby( "origin" )["flights"].agg( "sum" ).reset_index()
    flights_summary.to_csv( "flights.csv", index=False )
