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
    """A function that reads the LCA results (per exam) from a CSV file into
    a pandas DataFrame."""
    
    # NB: Make sure that the text file is encoded in UTF-8    
    df = pd.read_csv(results_dir / filename, sep=",")
    
    subj_counter = 0
    for subj in range(len(df)):
        df.at[subj_counter, 'subjcode'] = df.loc[subj_counter, 'filename'][-7:-4] # Indexing to extract subject code
        subj_counter += 1
    
    df.set_index('subjcode', inplace = True) # Set subject code as index
    
    # Check whether all subject codes are unique
    if len(set(list(df.index))) != len(df):
        print("WARNING: Some subject codes occur more than once.")
        
    return df


def filter_df(df, measures):
    """A function that subsets only those columns (i.e., LCA measures) we are
    interested in, and in addition only includes writing samples with fifty
    words or more.""" 
            
    # Only use selected measures
    df = df.loc[: , measures]
    
    # Remove empty entries
    df = df[df.wordtokens >= 50]
    
    return df


def create_df_diff(df1, df2, measures):
    """A function that creates a new pandas DataFrame which contains the
    difference on the selected LCA measures between two exams."""
    
    # Create list of subject codes that are present in both datasets
    subs = [index for index in list(filtered_aip.index) if index in 
        list(filtered_stat_a.index)]
    
    # Create empty dataframe
    df_diff = pd.DataFrame(index=subs, columns = measures)
    
    for subj in df_diff.index:
        for col in df_diff.columns:
            diff = df2.loc[subj, col] - df1.loc[subj, col]     
            df_diff.at[subj, col] = diff
    
    return df_diff


def select_comp_length(df):
    """A function that creates a subset of entries of the dataframe of
    differences, where the entries are comparable in terms of wordtokens
    (i.e., length)."""
    
    df = df[(df['wordtokens'] >= -20) & (df['wordtokens'] <= 20)]

    return df
       

#pd.set_option('display.max_columns', None)

### --------
### RUN CODE
### --------

#if __name__ == "__main__":
measures = ['wordtokens', 'ld', 'ls2', 'vs2', 'ndwesz', 'cttr', 'svv1']

aip = create_pandas_df('lca_AIP_A_EN_corrected.txt')
filtered_aip = filter_df(aip, measures)

stat_a = create_pandas_df('lca_STAT_A_EN_corrected.txt')
filtered_stat_a = filter_df(stat_a, measures)

aip_stat_a = create_df_diff(filtered_aip, filtered_stat_a, measures)
aip_stat_a[measures].mean()

aip_stat_a_comp_len = select_comp_length(aip_stat_a)
aip_stat_a_comp_len[measures].mean() # Means are not very different as compared
# to a df where entries are not comparable in length. It seems like the 
# measures are quite length-independent.