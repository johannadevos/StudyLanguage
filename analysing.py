# -*- coding: utf-8 -*-

# Script written by Johanna de Vos (2018)

# Standard library imports
import os
from pathlib import Path

# Third-party libraries
import nltk

# Own module
import preprocessing as prep

### ------------------
### DIRECTORIES
### ------------------

# Set NLTK directory
nltk.data.path.append("U:/nltk_data") # On work PC only

# Set working directory to where the data are

def _get_current_file_dir() -> Path:
    """Returns the directory of the script."""
    try:
        return Path(os.path.realpath(__file__)).parent
    except(NameError):
        return Path(os.getcwd())


src_dir = _get_current_file_dir()
data_dir = src_dir / 'data'
print(data_dir)

#os.chdir(data_dir)


### --------
### RUN CODE
### --------

if __name__ == "__main__":
    
    # Read and prepare student data
    #filename = data_dir / 'AIP_A_EN_uncorrected.txt'
    #filename = data_dir / 'AIP_A_EN_corrected.txt'
    #filename = data_dir / 'AIP_A_NL_uncorrected.txt'
    #filename = data_dir / 'AIP_A_NL_corrected.txt'  
    #filename = data_dir / 'STAT_A_EN_uncorrected.txt'
    #filename = data_dir / 'STAT_A_EN_corrected.txt'             
    #filename = data_dir / 'STAT_A_NL_uncorrected.txt'
    #filename = data_dir / 'STAT_A_NL_corrected.txt'
    #filename = data_dir / 'STAT_C_EN_uncorrected.txt'
    filename = data_dir / 'STAT_C_EN_corrected.txt'

    # TO DO. First, assign grades to the two subquestions of 2a          
    #filename = data_dir / 'STAT_C_NL_uncorrected.txt' 
    #filename = data_dir / 'STAT_C_NL_corrected.txt'
    
    raw_data = prep.open_file(filename) # Read data from file
    prep_data = prep.make_readable(raw_data) # Make student answers readable
    df, cols = prep.create_df(prep_data, filename) # Create and fill dataframe with student data
    
    #df = df[:10] # Make df smaller to try things out
    df = prep.preprocess(df, filename) # Tokenize, POS tag, lemmatize, and remove stop words
    
    #pd.set_option('display.max_rows', None) # Print the whole pandas table, don't truncate
    #print(df)
        
    #dictio = dictionary(df) # Create dictionary of vocabulary --> currently, doesn't work for STAT_C_EN
