# -*- coding: utf-8 -*-
"""
Created on Thu Mar  1 09:55:43 2018

@author: johan
"""

import os
from pathlib import Path

from preprocessing import *

### ------------------
### DIRECTORIES
### ------------------

# Set NLTK directory
#nltk.data.path.append("U:/nltk_data") # On work PC only

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
    filename = data_dir / 'AIP_A_EN_corrected.txt'
    #filename = data_dir / 'AIP_A_NL_uncorrected.txt'
    #filename = data_dir / 'AIP_A_NL_corrected.txt'  
    #filename = data_dir / 'STAT_A_EN_uncorrected.txt'
    #filename = data_dir / 'STAT_A_EN_corrected.txt'             
    #filename = data_dir / 'STAT_A_NL_uncorrected.txt'
    #filename = data_dir / 'STAT_A_NL_corrected.txt'
    #filename = data_dir / 'STAT_C_EN_uncorrected.txt'
    #filename = data_dir / 'STAT_C_EN_corrected.txt'

    # TO DO. First, assign grades to the two subquestions of 2a          
    #filename = data_dir / 'STAT_C_NL_uncorrected.txt' 
    #filename = data_dir / 'STAT_C_NL_corrected.txt'
    
    raw_data = open_file(filename)
    prep_data = preprocess(raw_data) # Preprocess student answers
    df, cols = create_df(prep_data, filename) # Create dataframe of student answers
    
    #pd.set_option('display.max_rows', None) # Print the whole pandas table, don't truncate
    #print(df)
    
    df = df[:10]
    df = tok_lem(df, filename) # Tokenize and lemmatize all answers, and remove stop words
    #dictio = dictionary(df) # Create dictionary of vocabulary --> currently, doesn't work for STAT_C_EN
