# -*- coding: utf-8 -*-

# Copyright: Johanna de Vos (2018)

# Standard library imports
import os
from pathlib import Path
import random

# Third-party libraries
import nltk

# Own module
import preprocessing as prep


# Set seed for reproducability of results
random.seed(2017)


### ------------------
### DIRECTORIES
### ------------------

# Set NLTK directory
if os.path.exists("U:/nltk_data"):
    nltk.data.path.append("U:/nltk_data") # On work PC only

# Set working directory to where the data are
def get_current_file_dir() -> Path:
    """Returns the directory of the script."""
    try:
        return Path(os.path.realpath(__file__)).parent
    except(NameError):
        return Path(os.getcwd())

# Define directories
src_dir = get_current_file_dir()
data_dir = src_dir / 'data'
indiv_data_dir = data_dir / 'indiv_files'
raw_data_dir = data_dir / 'raw_data'


### --------
### RUN CODE
### --------

if __name__ == "__main__":
    
    # Read and prepare student data
    #filename = 'AIP_A_EN_uncorrected.txt'
    #filename = 'AIP_A_EN_corrected.txt'
    #filename = 'AIP_A_NL_uncorrected.txt'
    #filename = 'AIP_A_NL_corrected.txt'  
    #filename = 'STAT_A_EN_uncorrected.txt'
    #filename = 'STAT_A_EN_corrected.txt'             
    #filename = 'STAT_A_NL_uncorrected.txt'
    #filename = 'STAT_A_NL_corrected.txt'
    #filename = 'STAT_C_EN_uncorrected.txt'
    #filename = 'STAT_C_EN_corrected.txt'

    # TO DO. First, assign grades to the two subquestions of 2a          
    #filename = 'STAT_C_NL_uncorrected.txt' 
    filename = 'STAT_C_NL_corrected.txt'
    
    raw_data = prep.open_file(raw_data_dir / filename) # Read data from file
    prep_data = prep.make_readable(raw_data) # Make student answers readable
    df, cols = prep.create_df(prep_data, filename) # Create and fill dataframe with student data
    
    #df = df[:10] # Make df smaller to try things out
    df = prep.preprocess(df, filename) # Tokenize, POS tag, lemmatize, and remove stop words
    df = prep.remove_unknown_subjects(df) # Remove entries where the subject code is unknown
    
    #pd.set_option('display.max_rows', None) # Print the whole pandas table, don't truncate
    #print(df)

    #dictio = dictionary(df) # Create dictionary of vocabulary --> currently, doesn't work for STAT_C_EN
    
    # Apply the Lexical Complexity Analyzer to the student answers
    #prep.create_lca_dirs(data_dir)
    #prep.prepare_for_lca(data_dir, df, filename)