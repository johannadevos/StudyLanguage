# -*- coding: utf-8 -*-

# Copyright: Johanna de Vos (2018)

# Standard library imports
import os
from pathlib import Path

import pandas as pd

### ------------------
### DIRECTORIES
### ------------------

# Set working directory to where the data are
def get_current_file_dir() -> Path:
    """Returns the directory of the script."""
    try:
        return Path(os.path.realpath(__file__)).parent
    except(NameError):
        return Path(os.getcwd())

# Define directories
root_dir = get_current_file_dir().parent
data_dir = root_dir / 'data'
results_dir = data_dir / 'lca_results'


def create_pandas_df(filename):
    
    # NB: Make sure that the text file is encoded in UTF-8    
    df = pd.read_csv(results_dir / filename, sep=",")
    
    subj_counter = 0
    for subj in range(len(df)):
        df.at[subj_counter, 'subjcode'] = df.loc[subj_counter, 'filename'][-7:-4] # Indexing to extract subject code
        subj_counter += 1
    
    df.set_index('subjcode', inplace = True) # Set subject code as index
    
    # Check whether all subject codes are unique
    if len(set(list(df.index))) != len(df):
        print("WARNING: Some subject codes are used more than once.")
        
    return df


aip = create_pandas_df('lca_AIP_A_EN_corrected.txt')
stat_a = create_pandas_df('lca_STAT_A_EN_corrected.txt')
    




### --------
### RUN CODE
### --------

#if __name__ == "__main__":
    
    