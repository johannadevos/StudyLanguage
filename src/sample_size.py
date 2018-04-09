#!usr/bin/en/python3
# -*- coding: utf-8 -*-

# Script written by Johanna de Vos (2018)

import argparse
import os
import statistics

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.stats import normaltest, pearsonr, spearmanr

import preprocessing


def calculate_descriptives(files, data_dir, results_dir):
    output_dir = os.path.join(results_dir, 'length_descriptives')
    
    for exam in files:
        if not "uncorrected" in exam:
            exam = exam[:-4]
                
            if not "STAT_C" in exam:
                print(exam)
                
                results_file = str(os.path.join(results_dir, 'lca_results', 
                                                exam)) + '.txt'
                
                results_df = pd.read_csv(results_file, sep = ',')
                wordtokens = results_df['wordtokens']
                        
                print("Average length of writing samples:", statistics.mean
                      (wordtokens))
                print("Standard deviation of writing sample length:", 
                      statistics.stdev(wordtokens))
                print("The longest sample is:", max(wordtokens), "words")
                #histogram(wordtokens, exam, output_dir=output_dir)
            
            elif "STAT_C" in exam:
                questions = ['4a']
                #questions = ['4a', '2aDec', '2aCaus']
                
                for question in questions:
                    print(exam, question)
                    
                    results_file = str(os.path.join(results_dir, 'lca_results', 
                                                    exam)) + '_' + question \
                                                    + '.txt'
                    
                    results_df = pd.read_csv(results_file, sep = ',')
                    wordtokens = results_df['wordtokens']
                            
                    print("Average length of writing samples:", statistics.mean
                          (wordtokens))
                    print("Standard deviation of writing sample length:", 
                          statistics.stdev(wordtokens))
                    print("The longest sample is:", max(wordtokens), "words")
                    #histogram(wordtokens, exam, question, output_dir=output_dir)
    

def histogram(variable, title, question=None, output_dir=None):
    
    # Histogram
    plt.rcParams["patch.force_edgecolor"] = True
    fig = plt.figure()
    ax = fig.add_subplot(1,1,1)
    ax.hist(variable)
    #ax.hist(variable, bins=np.arange(0, max(variable)+10, 10))
    ax.set_xlabel('{}'.format(variable.name))
    #ax.set_xlabel('Length (words)')
    ax.set_ylabel('Counts')
    if not question:
        ax.set_title('{}'.format(title))
        if output_dir:
            plt.savefig('{}\{}.png'.format(output_dir, title))
    elif question:
        ax.set_title('{}_{}'.format(title, question))
        if output_dir:
            plt.savefig('{}\{}_{}.png'.format(output_dir, title, 
                        question))        
    plt.show(fig)
    plt.close(fig)
    

def create_pandas_df(lca_results_dir, filename, lca_min_sam):
    """A function that reads the LCA results (per exam) from a CSV file into
    a pandas DataFrame."""
    
    # NB: Make sure that the text file is encoded in UTF-8    
    df = pd.read_csv(os.path.join(lca_results_dir, filename), sep = ",")
        
    return df


def all_results(results_dir, lca_min_sam):
    """A function that calculates the correlation between text length and the \
    25 outcomes of the lexical complexity analysis."""

    lca_dir = os.path.join(results_dir, "lca_results")
    all_result_files = os.listdir(lca_dir)
    
    # Create empty dataframe
    appended_data = []
    
    # Loop through all results from corrected files and append to big dataframe
    for file in all_result_files:
        df = pd.read_csv(os.path.join(lca_dir, file), sep = ",")
        appended_data.append(df)

    return pd.concat(appended_data)


def correlations(df, results_dir, lca_min_sam, lca_measures):
    
    # Only use samples that are longer than the specified cut-off point
    df = df[df.wordtokens >= lca_min_sam]
    
    # Exclude a datapoint that is an extremele outlier
    #df = df[df.wordtokens < 350]
    
    # Test whether the text length is normally distributed
    histogram(df['wordtokens'], 'Distribution of text lengths')
    test_stat, p = normaltest(df['wordtokens'])
    #print(test_stat, p)
    
    # Set up file where results should be stored
    destination = os.path.join(results_dir, "length_descriptives", 
                               "correlations_wordtokens_measures.txt")
    headers = "\t".join(["measure", "r_spearman", "p_value"])
    with open(destination, 'w') as outfile:
        outfile.write(headers)
        outfile.write("\n")
    
    # If so, use Spearman's correlation coefficient
# =============================================================================
#     if p < 0.05:
#         pass
#     else:
#         print("You should implement Pearson's correlation coefficient.")
#         
# =============================================================================
    with open(destination, "a") as outfile:
        corrs = []
        
        for measure in lca_measures:
            corr, sig = pearsonr(df['wordtokens'], df[measure])
            corrs.append(round(corr, 3))
            print(measure, "\tSpearman's r =", round(corr, 3), "\tp = ", sig)
            outfile.write("\t\t".join((measure, str(round(corr, 3)), str(
                    round(sig, 3)), "\n")))
        
# =============================================================================
#         lca_counter = 0        
# =============================================================================
        
        for measure in lca_measures:
            #print(measure)
            histogram(df[measure], 'Distribution of {}'.format(measure))
            df.plot(kind ='scatter', x ='wordtokens', y = measure, title = 'Correlation between wordtokens and {}'.format(measure))
            
# =============================================================================
#             #corrs_measure = [x for x in all_corrs if x % len(lca_measures) == lca_counter]
#             corrs_measure = all_corrs[lca_counter::len(lca_measures)]
#             print(corrs_measure)
#             
#             lca_counter +- 1
# =============================================================================
            
        return corrs
                
    
def correlations_per_exam(results_dir, lca_min_sam):
    """A function that calculates the correlation between text length and the \
    25 outcomes of the lexical complexity analysis."""

    lca_dir = os.path.join(results_dir, "lca_results")
    all_result_files = os.listdir(lca_dir)

    all_corrs = []
    
    # Define the LCA measures for which we want to get correlations
    lca_measures = ['ld', 'ls2', 'ndwerz', 'msttr']
    
    # Loop through all results from corrected files and append to big dataframe
    for file in all_result_files:
        if not "2a" in file:
            print("\nExam:", file[:-4], "\n")
            
            df = pd.read_csv(os.path.join(lca_dir, file), sep = ",")
    
            # Find columns that contain LCA measures
            index_ld = df.columns.get_loc('ld')
            #lca_measures = df.columns[index_ld:]
            
            # Calculate correlations
            all_corrs.append(correlations(df, results_dir, lca_min_sam, lca_measures))
    
    for measure in lca_measures:
        index_measure = lca_measures.index(measure)
        corrs_measure = [x[index_measure] for x in all_corrs]
        print("The average correlation between wordtokens and {} is: ".format(measure), str(round(statistics.mean(corrs_measure), 2)))
        

def distr_dep_measures(df):
    pass
    
    
def create_descr_dir(results_dir, dir_name):
    descr_dir = os.path.join(results_dir, dir_name)
    if not os.path.exists(descr_dir):
        os.mkdir(os.path.join(descr_dir))


def main():
    
    # Define directories
    src_dir = os.path.dirname(os.path.abspath(__file__))
    data_dir = os.path.join(src_dir, '..', 'data')
    raw_data_dir = os.path.join(data_dir, 'raw_data')
    results_dir = os.path.join(src_dir, '..', 'results')
    
    # Parse arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('language', help = "Choose the study language: 'EN' \
                        or 'NL'.", choices = ["EN", "NL"])
    parser.add_argument('lca_min_sam', help = "Choose the minimal sample size \
                    with which the data were preprocessed.")
    args = parser.parse_args()
    language = args.language
    lca_min_sam = int(args.lca_min_sam)
    
    # Create directory to store results
    create_descr_dir(results_dir, 'length_descriptives')
        
    # Compute descriptives
    files = preprocessing.filenames(raw_data_dir, language)
    calculate_descriptives(files, data_dir, results_dir)
    
    # Calculate correlations per exam
    correlations_per_exam(results_dir, lca_min_sam)

    # Calculate correlations between length and measures
    #big_df = all_results(results_dir, lca_min_sam)
    #correlations(big_df, results_dir, lca_min_sam)
    
# Run code
if __name__ == "__main__":
    main()