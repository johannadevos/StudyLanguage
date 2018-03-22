# -*- coding: utf-8 -*-
# Copyright: Johanna de Vos (2018)

import argparse
import os

import pandas as pd


def create_pandas_df(lca_results_dir, filename):
    """A function that reads the LCA results (per exam) from a CSV file into
    a pandas DataFrame."""
    
    # NB: Make sure that the text file is encoded in UTF-8    
    df = pd.read_csv(os.path.join(lca_results_dir, filename), sep = ",")
    
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
    subs = [index for index in list(df1.index) if index in 
        list(df2.index)]
    
    # Create empty dataframe
    df_diff = pd.DataFrame(index=subs, columns = measures)
    
    for subj in df_diff.index:
        for col in df_diff.columns:
            diff = df2.loc[subj, col] - df1.loc[subj, col]     
            df_diff.at[subj, col] = diff
    
    return df_diff


#pd.set_option('display.max_columns', None)


### --------
### RUN CODE
### --------

#def main():
#if __name__ == "__main__":
    
# Parse arguments
parser = argparse.ArgumentParser()
parser.add_argument("exam1", help = "The first exam in the comparison.")
parser.add_argument("exam2", help = "The second exam in the comparison.")
args = parser.parse_args()
exam1 = args.exam1
exam2 = args.exam2
    
# Define directories
src_dir = os.path.dirname(os.path.abspath(__file__))
data_dir = os.path.join(src_dir, '..', 'data')
results_dir = os.path.join(src_dir, '..', 'results')
truncated_results_dir = os.path.join(results_dir, 'lca_truncated')
untruncated_results_dir = os.path.join(results_dir, 'lca_untruncated')

raw_exam1 = create_pandas_df(truncated_results_dir, exam1)
raw_exam2 = create_pandas_df(truncated_results_dir, exam2)

# Define required measures
sel_measures = ['wordtokens', 'ld', 'ls2', 'vs2', 'ndwesz', 'cttr', 'svv1']
all_measures = list(raw_exam1.columns[1:]) # [1:] to exclude filename

# Optionally, apply filters
filtered_exam1 = filter_df(raw_exam1, sel_measures)
filtered_exam2 = filter_df(raw_exam2, sel_measures)

# Inspect selected measures
diff_selected = create_df_diff(raw_exam1, raw_exam2, sel_measures)
print(diff_selected[sel_measures].mean())

# Inspect all measures
diff_all = create_df_diff(raw_exam1, raw_exam2, all_measures)
print("\n", diff_all[all_measures].mean())