# -*- coding: utf-8 -*-
# Copyright: Johanna de Vos (2018)

import argparse
import os
import re

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import preprocessing as prep
from scipy.stats import normaltest, pearsonr, spearmanr


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


def same_lang(df1, df2, measures, nationality):
    """A function that creates a new pandas DataFrame which contains the
    difference on the selected LCA measures between two exams."""
    
    # Create list of subject codes that are present in both datasets
    
    # Both nationalities together
    #subs = [index for index in list(df1.index) if index in list(df2.index)]
    
    # One selected nationality
    subs = [index for index in list(df1.index) if index in list(df2.index) and 
            df1.loc[index, 'Natio1'] == nationality]
    
    # Subset df1 and df2 to only contain the 'good' subjects and the selected
    # measures
    df_exam1 = df1.loc[subs, measures]
    df_exam2 = df2.loc[subs, measures]
    
    # Create dataframe containing the difference scores
    df_diff = df_exam2 - df_exam1
    
    # Print some results
    print("Sample size:", len(subs), nationality, "students")
    print(df_diff[measures].mean())
    
    return df_diff


def other_lang(df1, df2, measures, nationality):
    """A function that calculates the mean on all measures for both exams, 
    and then calculates the difference between the means."""
    
    means1 = df1[df1.loc[:, 'Natio1'] == nationality][measures].mean()
    means2 = df2[df2.loc[:, 'Natio1'] == nationality][measures].mean()
    
    nr_subj_1 = len(df1[df1.loc[:, 'Natio1'] == nationality])
    nr_subj_2 = len(df2[df2.loc[:, 'Natio1'] == nationality])

    print("Sample size:", nr_subj_2, nationality, "students in the first \
exam, and", nr_subj_1, nationality, "students in the second exam")
    
    diff_means = means2 - means1
    print(diff_means)


def corr_grade_ects(subject_info):
    
    subject_info_no_missing_grades = subject_info[subject_info['MeanPsy'].notnull()]
    
    grades = subject_info_no_missing_grades['MeanPsy']
    ects = subject_info_no_missing_grades['ECTSPsyObtained']
    
    # Test whether the variables are normally distributed

    # Histogram grades
    plt.rcParams["patch.force_edgecolor"] = True
    fig = plt.figure()
    ax = fig.add_subplot(1,1,1)
    ax.hist(grades, bins=np.arange(-0.5, 10.5, 1))
    ax.set_xlabel('Grade')
    ax.set_ylabel('Counts')
    
    # Histogram ECTS
    plt.rcParams["patch.force_edgecolor"] = True
    fig = plt.figure()
    ax = fig.add_subplot(1,1,1)
    ax.hist(ects, bins=np.arange(-0.5, 60.5, 5))
    ax.set_xlabel('ECTS obtained')
    ax.set_ylabel('Counts')

    # Relationship between grades and obtained ECTS
    subject_info_no_missing_grades.plot(kind = 'scatter', x ='ECTSPsyObtained', 
                                        y = 'MeanPsy', ylim=[-0.3,10.3], title = 
                                        'Relationship between grades and ECTS obtained')
    plt.show()
    plt.close()
    
    # Statistical tests
    test_stat_mean, p_mean = normaltest(grades)
    test_stat_ects, p_ects = normaltest(ects)
    
    # Calculate correlations
    if p_mean < 0.05 or p_ects < 0.05:
        corr, sig = spearmanr(grades, ects)
        print("Spearman's r =", round(corr, 3), "\tp = ", round(sig, 3))

    else:
        print("You should implement Pearson's correlation coefficient.")
        
    # Relationship between grades and taken ECTS
    subject_info_no_missing_grades.plot(kind ='scatter', x ='ECTSPsyTaken', y =
                                        'MeanPsy', ylim=[-0.3,10.3], title =
                                        'Relationship between grades and ECTS taken')
    plt.show()
    plt.close()
    
    # Statistical tests
    test_stat_mean, p_mean = normaltest(grades)
    test_stat_ects, p_ects = normaltest(subject_info_no_missing_grades
                                        ['ECTSPsyTaken'])
    
    # Calculate correlations
    if p_mean < 0.05 or p_ects < 0.05:
        corr, sig = spearmanr(grades, subject_info_no_missing_grades
                                        ['ECTSPsyTaken'])
        print("Spearman's r =", round(corr, 3), "\tp = ", round(sig, 3))

    else:
        print("You should implement Pearson's correlation coefficient.")
    
    # Relationship between taken and obtained ECTS in year 1
    subject_info.plot(kind ='scatter', x ='ECTSPsyTaken', y ='ECTSPsyObtained', 
                      title = 'Relationship between taken and obtained ECTS')
    plt.show()
    plt.close()
    
    # Statistical tests
    ects_taken = subject_info['ECTSPsyTaken']
    ects_obtained = subject_info['ECTSPsyObtained']
    
    test_stat_taken, p_taken = normaltest(ects_taken)
    test_stat_obtained, p_obtained = normaltest(ects_obtained)
    
    # Calculate correlations
    if p_taken < 0.05 or p_obtained < 0.05:
        corr, sig = spearmanr(ects_taken, ects_obtained)
        print("Spearman's r =", round(corr, 3), "\tp = ", round(sig, 3))

    else:
        print("You should implement Pearson's correlation coefficient.")
    
    
def exam_language(exam_name):
    if "EN" in exam_name:
        lang = "EN"
    elif "NL" in exam_name:
        lang = "NL"
        
    return lang


def overlap_between_exams(exam1, exam2, exam3, name_exam1, name_exam2, 
                          name_exam3):

    # Exam name without _(un)corrected and .txt
    short_name1 = re.sub("_(?:un)?corrected(?:_\w*)?.txt", "", name_exam1)
    short_name2 = re.sub("_(?:un)?corrected(?:_\w*)?.txt", "", name_exam2)
    short_name3 = re.sub("_(?:un)?corrected(?:_\w*)?.txt", "", name_exam3)
    exam_names = [short_name1, short_name2, short_name3]
    exams = [exam1, exam2, exam3]
    
    # Establish the language each exam was written in
    lang1 = exam_language(short_name1)
    lang2 = exam_language(short_name2)
    lang3 = exam_language(short_name3)

    # Investigate how many subjects did all exams
    assert lang1 == lang2 == lang3, "Please select exams that are all the \
                                    same language."
    
    # Create empty dataframe for the given language
    match_index = pd.DataFrame(index = subject_info
                               [subject_info['Track']==lang1].index, 
                               columns = exam_names)
    
    # Fill in dataframe with 'Yes's if a subject took part in a particular exam
    for counter in range(len(exam_names)):
        exam = exams[counter]
        exam_name = exam_names[counter]
        
        for index in exam.index:
            match_index.at[index, exam_name] = "Yes"
    
    # If an entry exists for each exam, write "Yes" to a new column 'All'
    for index, row in match_index.iterrows():
        if row[short_name1] == "Yes" and \
           row[short_name2] == "Yes" and \
           row[short_name3] == "Yes":
            match_index.at[index, 'All'] = "Yes"
        if row[short_name1] == "Yes" and \
           row[short_name2] == "Yes":
            match_index.at[index, '1And2'] = "Yes"
        if row[short_name1] == "Yes" and \
           row[short_name3] == "Yes":
            match_index.at[index, '1And3'] = "Yes"
        if row[short_name2] == "Yes" and \
           row[short_name3] == "Yes":
            match_index.at[index, '2And3'] = "Yes"
    
    # Print information
    num_subs = len(match_index)
    print(str(match_index[short_name1].value_counts()[0]), "out of", 
          str(num_subs), "subjects completed", short_name1)   
    print(str(match_index[short_name2].value_counts()[0]), "out of", 
          str(num_subs), "subjects completed", short_name2)   
    print(str(match_index[short_name3].value_counts()[0]), "out of", 
          str(num_subs), "subjects completed", short_name3)   
    print(str(match_index['1And2'].value_counts()[0]), "out of", str(num_subs), 
          "subjects completed both", short_name1, "and", short_name2)   
    print(str(match_index['1And3'].value_counts()[0]), "out of", str(num_subs), 
          "subjects completed both", short_name1, "and", short_name3)   
    print(str(match_index['2And3'].value_counts()[0]), "out of", str(num_subs), 
          "subjects completed both", short_name2, "and", short_name3)   
    print(str(match_index['All'].value_counts()[0]), "out of", str(num_subs), 
          "subjects completed all exams")
        
    return match_index


#pd.set_option('display.max_columns', None)
#pd.set_option('display.max_rows', None)


### --------
### RUN CODE
### --------

#def main():
#if __name__ == "__main__":
    
# Parse arguments
parser = argparse.ArgumentParser()
parser.add_argument("lca_min_sam", help = "Choose the minimal sample size \
                    with which the data were preprocessed.")
parser.add_argument("exam1", help = "The first exam in the comparison.")
parser.add_argument("exam2", help = "The second exam in the comparison.")
parser.add_argument("--exam3", help = "The third exam in the comparison.")
parser.add_argument("--nationality", help = "For which nationality do you \
                    want to perform the analysis?", choices = ["DU", "NL"], 
                    default = "NL")

args = parser.parse_args()
lca_min_sam = int(args.lca_min_sam)
name_exam1 = args.exam1
name_exam2 = args.exam2
name_exam3 = args.exam3
natio = args.nationality
    
# Define directories
src_dir = os.path.dirname(os.path.abspath(__file__))
data_dir = os.path.join(src_dir, '..', 'data')
results_dir = os.path.join(src_dir, '..', 'results')
lca_results_dir = os.path.join(results_dir, 'lca_results')

# Create dataframes with LCA results for the two exams
exam1 = create_pandas_df(lca_results_dir, name_exam1)
exam2 = create_pandas_df(lca_results_dir, name_exam2)
if name_exam3:
    exam3 = create_pandas_df(lca_results_dir, name_exam3)

# Establish the language each exam was written in
lang1 = exam_language(name_exam1)
lang2 = exam_language(name_exam2)

# Read in subject info
subject_info = prep.read_subject_info(data_dir)

# Calculate correlation between grades and ECTS
#corr_grade_ects(subject_info)

# Define required measures
sel_measures = ['wordtokens', 'ld', 'ls2', 'ndwesz', 'msttr']
all_measures = list(exam1.columns)
lca_measures = list(exam1.columns)[exam1.columns.get_loc('ld'):]

# Optionally, apply filters
filtered_exam1 = filter_df(exam1, sel_measures, lca_min_sam)
filtered_exam2 = filter_df(exam2, sel_measures, lca_min_sam)
if name_exam3: 
    filtered_exam3 = filter_df(exam3, sel_measures, lca_min_sam)

# Join dataframes
data1 = filtered_exam1.join(subject_info)
data2 = filtered_exam2.join(subject_info)
if name_exam3:
    data3 = filtered_exam3.join(subject_info)
    
# Investigate how many students took part in each exam
assert name_exam3, "For the below function, please enter three exams as arguments."

print("Not truncating the data at a certain length:")
match_index = overlap_between_exams(exam1, exam2, exam3, name_exam1, 
                                    name_exam2, name_exam3)

print("\nOnly using data with a minimum length of", str(lca_min_sam))
match_index = overlap_between_exams(filtered_exam1, filtered_exam2, 
                                    filtered_exam3, name_exam1, name_exam2, 
                                    name_exam3)
# Compare exams
print("\nCalculating:", name_exam2[:-4], "minus", name_exam1[:-4], "\n")

if lang1 == lang2:
    diff_scores = same_lang(data1, data2, sel_measures, natio)
elif lang1 != lang2:
    other_lang(data1, data2, sel_measures, natio)

