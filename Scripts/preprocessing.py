# -*- coding: utf-8 -*-
"""
Created on Wed Dec 29 11:40:40 2021

@author: Rasmus Säfvenberg
"""
import spacy
import pandas as pd
import re

# Load Spacy
nlp = spacy.load('en_core_web_lg')

def preprocess(text):
    """
    Preprocess the text using Spacy. Remove person entities.

    Parameters
    ----------
    text : pd.Series
        A vector of all movie synopses to process.

    Returns
    -------
    A string of all the tokens generated by Spacy, separated by space.

    """
    # Convert to spacy tokens
    doc = nlp(text)
    # Extract all person entities
    ents = set([re.sub("\(|\)", "", e.text) for e in doc.ents if e.label_ == "PERSON"])
    
    # Remove all entities
    non_ents_text = re.sub("|".join(ents), "", text)
    # Re-transform to spacy
    doc = nlp(non_ents_text)

    # Retreive the lemmas of non stop-words and alphabetical characters
    tokens = [token.lemma_ for token in doc if not token.is_stop and token.lemma_.isalpha()]
    return " ".join(tokens)


def process_text(df):
    """
    Preprocess the text by removing citations, duplciated movies and then
    applying applying preprocces() on each synopsis.

    Parameters
    ----------
    df : pd.DataFrame
        A data frame containing the synposes to be processed.

    Returns
    -------
    docs : pd.DataFrame
        Preprocessed movie synopses.

    """
    try:
        # Load file if it exists
        docs = pd.read_csv("../Data/processed.csv")
    except FileNotFoundError:
        # Remove sources as given by [\d].
        df["Plot"] = df["Plot"].apply(lambda x: re.sub("(?<=\[)\d+(?=\])|\[|\]", "", x))

        # Drop duplicated movies
        df.drop_duplicates("Plot").reset_index(drop=True, inplace=True)
        
        # Preprocess the synopses
        docs = df["Plot"].apply(lambda x: preprocess(x))
        
        # Save as csv file for faster loading
        docs.to_csv("../Data/processed.csv", index=False)

    return docs


def lda_preprocess(docs):
    """
    Preprocess documents to be able to use with gensim.

    Parameters
    ----------
    docs : pd.Series
        The original documents to be processed.

    Returns
    -------
    documents : list of lists
        List of documents, where documents is a list of tokens.

    """
    documents = []
    for movie in docs:
        doc = nlp(movie)
        tokens = []
        for token in doc:
            tokens.append(token.text)
        documents.append(tokens)
    return documents


def convert_to_int(y):
    """
    Convert a vector of strings to a numeric value. 

    Parameters
    ----------
    y : pd.Series
        A vector of strings of genres.

    Returns
    -------
    y_train : pd.Series
        A series of all movies having a genre.
    y_test : pd.Series
        A series of all the movies with genre "unknown".

    """
    # Indicies of unkown genre
    indices = y[y == "unknown"].index
    
    # Convert to integeres
    y = pd.Series(pd.Categorical(y).codes).astype(int)
    
    # Convert unknown to -1
    y[indices] = -1
    
    # Access train and test genres
    y_train = y[y != -1]
    y_test  = y[y == -1]
    
    return y_train, y_test
