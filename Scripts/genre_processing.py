# -*- coding: utf-8 -*-
"""
Created on Wed Dec 29 11:44:33 2021

@author: Rasmus Säfvenberg
"""

import pandas as pd
import re
import numpy as np

def clean_genre(df):
    """
    Clean genre by reducing multiple genres to singular genre.

    Parameters
    ----------
    df : pd.DataFrame
        A data frame with a column of movie genres.

    Returns
    -------
    df : pd.DataFrame
        A modified data frame with grouped genres.

    """
    # Convert the movie Social to Drama
    df["GenreFix"] = df["Genre"].apply(lambda x: re.sub("social", "drama", x))
    
    # Take the first genre from a tuple (A,B). E.g., Comedy, Romance => Comedy
    df["GenreFix"] = df["GenreFix"].apply(lambda x: re.sub("(?<=[a-z]),\s*[a-z]+", "", x))
    # Take the first genre from A-B. E.g., Comedy-romance => Comedy
    df["GenreFix"] = df["GenreFix"].apply(lambda x: re.sub("(?<=[a-z])\s*-\s*[a-z]+", "", x))
    # Take the first genre A/B. E.g., Comedy/Romance => Comedy
    df["GenreFix"] = df["GenreFix"].apply(lambda x: re.sub("(?<=[a-z])\s*/\s*[a-z]+", "", x))
    
    # Categorize as war movies
    df["GenreFix"] = df["GenreFix"].apply(lambda x: "war" if x.find("ww") != -1 or \
                                          x.find("world war") != -1 or \
                                          x.find("war") != -1 else x)
    
    # Categorize as animations
    df["GenreFix"] = df["GenreFix"].apply(lambda x: "animation" if x.find("anim") != -1 \
                                          else x)
    # Categorize as crime movies
    df["GenreFix"] = df["GenreFix"].apply(lambda x: "crime" if x.find("crime") != -1 \
                                          else x)
        
    # Remove the word "film"
    df["GenreFix"] = df["GenreFix"].apply(lambda x: re.sub("film", " ", x))
    
    # Historical drama => drama etc.
    df["GenreFix"] = df["GenreFix"].apply(lambda x: x.split()[-1] if x != " " else x)
    
    # Remove commas and dots alongside their whitespaces
    df["GenreFix"] = df["GenreFix"].apply(lambda x: re.sub(",|\.", "", x.strip()))
    
    return df


def group_genre(df):
    """
    Group mispellings or subgenres to larger groups of genres.

    Parameters
    ----------
    df : pd.DataFrame
        A data frame in with a column "GenreFix" in which the genre shall be grouped.

    Returns
    -------
    df : pd.DataFrame
        A modified data frame with grouped genres.

    """
    # Combine genres pt.1 
    genres = np.select(
        [
            # Thriller
            (df["GenreFix"] == "suspense") | (df["GenreFix"] == "ttriller") | (df["GenreFix"] == "thriler"), 
            # Romance
            (df["GenreFix"] == "romantic") | (df["GenreFix"] == "love") | (df["GenreFix"] == "com"), 
            # Historical
            (df["GenreFix"] == "biography") | (df["GenreFix"] == "biographical") | 
            (df["GenreFix"] == "epic") | (df["GenreFix"] == "mythology") |
            (df["GenreFix"] == "history") |  (df["GenreFix"] == "mythological") |  
            (df["GenreFix"] == "biograpic") | (df["GenreFix"] == "biogtaphy") |
            (df["GenreFix"] == "bio") | (df["GenreFix"] == "biopic") | 
            (df["GenreFix"] == "folklore"), 
            # Sci-fi
            (df["GenreFix"] == "sci") | (df["GenreFix"] == "fiction") | (df["GenreFix"] == "science"),
            # War
            (df["GenreFix"] == "spy") | (df["GenreFix"] == "wuxia") | (df["GenreFix"] == "samurai"),
            # Drama
            (df["GenreFix"] == "melodrama"),
            # Horror
            (df["GenreFix"] == "slasher"),
            # Adventure
            (df["GenreFix"] == "swashbuckler"),
            # Comedy
            (df["GenreFix"] == "parody") | (df["GenreFix"] == "satire") | (df["GenreFix"] == "slapstick"),     
            # Mystery
            (df["GenreFix"] == "detective"),
            # Fantasy
            (df["GenreFix"] == "tokusatsu") | (df["GenreFix"] == "kaiju"),
            # Musical
            (df["GenreFix"] == "music") | (df["GenreFix"] == "dance"),
            # Action
            (df["GenreFix"] == "arts") | (df["GenreFix"] == "fu") | (df["GenreFix"] == "superhero"),
            # Crime
            (df["GenreFix"] == "gangster") | (df["GenreFix"] == "yakuza") | (df["GenreFix"] == "triad")
        ], 
        [
            "thriller", 
            "romance",
            "historical",
            "science fiction",
            "war",
            "drama",
            "horror",
            "adventure",
            "comedy",
            "mystery",
            "fantasy",
            "musical",
            "action",
            "crime"
        ], 
        default=df["GenreFix"]
    )
    
    # Combine genres pt. 2
    genres = np.select(
        [
            (genres == "fantasy"), 
            (genres == "children") | (genres == "children's") , 
            (genres == "neo") | (genres == "noir")
        ], 
        [
            "adventure", 
            "family",
            "film noir"
            
        ], 
        default=genres
    )

    # Convert to series
    genres = pd.Series(genres)
    # Replace genres
    df["GenreFix"] = genres

    # Keep only the most common genres
    genres_to_keep = genres.value_counts() > 100
    
    # Remove movies with too rare genre
    df = df.loc[genres.isin(genres_to_keep[genres_to_keep].index)]
    
    return df