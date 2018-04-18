# -*- coding: utf-8 -*-
# Copyright: Johanna de Vos (2018)

import argparse
import os

import pandas as pd
import preprocessing as prep


def create_pandas_df(lca_results_dir, filename):
    """A function that reads the LCA results (per exam) from a CSV file into
    a pandas DataFrame."""
    
    # NB: Make sure that the text file is encoded in UTF-8    
    df = pd.read_csv(os.path.join(lca_results_dir, filename), sep = ",")
    df.set_index('subjectcode', inplace = True) # Set subject code as index
    
    # Check whether all subject codes are unique
    if len(set(list(df.index))) != len(df):
        print("WARNING: Some subject codes occur more than once.")
        
    return df


def filter_df(df, measures, lca_min_sam):
    """A function that subsets only those columns (i.e., LCA measures) we are
    interested in, and in addition only includes writing samples with fifty
    words or more.""" 
    
    # Only use selected measures
    df = df.loc[: , measures]
    
    # Remove entries under a certain word length
    df = df[df.wordtokens >= lca_min_sam]
    
    return df


### --------
### RUN CODE
### --------
    
# Parse arguments
parser = argparse.ArgumentParser()
parser.add_argument("lca_min_sam", help = "Choose the minimal sample size \
                    with which the data were preprocessed.")

args = parser.parse_args()
lca_min_sam = int(args.lca_min_sam)
    
# Define directories
src_dir = os.path.dirname(os.path.abspath(__file__))
data_dir = os.path.join(src_dir, '..', 'data')
results_dir = os.path.join(src_dir, '..', 'results')
lca_results_dir = os.path.join(results_dir, 'lca_results')

# Create dataframes with LCA results for the two exams
exam1_nl = create_pandas_df(lca_results_dir, "AIP_A_NL_corrected.txt")
exam2_nl = create_pandas_df(lca_results_dir, "STAT_A_NL_corrected.txt")
exam3_nl = create_pandas_df(lca_results_dir, "STAT_C_NL_corrected_4a.txt")

exam1_en = create_pandas_df(lca_results_dir, "AIP_A_EN_corrected.txt")
exam2_en = create_pandas_df(lca_results_dir, "STAT_A_EN_corrected.txt")
exam3_en = create_pandas_df(lca_results_dir, "STAT_C_EN_corrected_4a.txt")

# Read in subject info
subject_info = prep.read_subject_info(data_dir)
dutch_subjects = subject_info[subject_info.Track == "NL"]
english_subjects = subject_info[subject_info.Track == "EN"]

# Define required measures
sel_measures = ['wordtokens', 'ld', 'ls2', 'ndwesz', 'msttr']

# Apply filters
exam1_nl_fil = filter_df(exam1_nl, sel_measures, lca_min_sam)
exam2_nl_fil = filter_df(exam2_nl, sel_measures, lca_min_sam)
exam3_nl_fil = filter_df(exam3_nl, sel_measures, lca_min_sam)

exam1_en_fil = filter_df(exam1_en, sel_measures, lca_min_sam)
exam2_en_fil = filter_df(exam2_en, sel_measures, lca_min_sam)
exam3_en_fil = filter_df(exam3_en, sel_measures, lca_min_sam)

# Add timestamp to exam headers
exam1_nl_fil.rename(columns=lambda x: x + "_oct", inplace=True)
exam2_nl_fil.rename(columns=lambda x: x + "_jan", inplace=True)
exam3_nl_fil.rename(columns=lambda x: x + "_apr", inplace=True)

exam1_en_fil.rename(columns=lambda x: x + "_oct", inplace=True)
exam2_en_fil.rename(columns=lambda x: x + "_jan", inplace=True)
exam3_en_fil.rename(columns=lambda x: x + "_apr", inplace=True)

# Merge subject_info and exam scores
nl_data = dutch_subjects.join(exam1_nl_fil)
nl_data = nl_data.join(exam2_nl_fil)
nl_data = nl_data.join(exam3_nl_fil)

en_data = english_subjects.join(exam1_en_fil)
en_data = en_data.join(exam2_en_fil)
en_data = en_data.join(exam3_en_fil)

# Merge Dutch and English data
r_data = nl_data.append(en_data)

# Remove subjects for whom one or more exams are not available
r_data = r_data.dropna(subset = ['msttr_oct', 'msttr_jan', 'msttr_apr'])

# Write to file
output = os.path.join(data_dir, 'r_data.txt')
r_data.to_csv(output)