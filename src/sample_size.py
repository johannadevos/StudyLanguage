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
    for exam in files:
        exam = exam[:-4]
            
        if not "STAT_C" in exam:
            print(exam)
            
            results_file = str(os.path.join(results_dir, 'lca_untruncated', exam)) + '.txt'
            
            results_df = pd.read_csv(results_file, sep = ',')
            wordtokens = results_df['wordtokens']
                    
            print("Average length of writing samples:", statistics.mean(wordtokens))
            print("Standard deviation of writing sample length:", statistics.stdev(wordtokens))
            print("The longest sample is:", max(wordtokens), "words")
            #histogram(results_dir, wordtokens, exam)
        
        elif "STAT_C" in exam:
            questions = ['4a', '2aDec', '2aCaus']
            
            for question in questions:
                print(exam, question)
                
                results_file = str(os.path.join(results_dir, 'lca_untruncated', exam)) + '_' + question + '.txt'
                
                results_df = pd.read_csv(results_file, sep = ',')
                wordtokens = results_df['wordtokens']
                        
                print("Average length of writing samples:", statistics.mean(wordtokens))
                print("Standard deviation of writing sample length:", statistics.stdev(wordtokens))
                print("The longest sample is:", max(wordtokens), "words")
                #histogram(results_dir, wordtokens, exam, question)
    

def histogram(results_dir, wordtokens, exam, question=None):
    
    # Directory to save histograms
    descr_dir = os.path.join(results_dir, 'length_descriptives')
    if not os.path.exists(descr_dir):
        os.mkdir(os.path.join(descr_dir))
    
    # Draw histogram
    plt.rcParams["patch.force_edgecolor"] = True
    fig = plt.figure()
    ax = fig.add_subplot(1,1,1)
    ax.hist(wordtokens, bins=np.arange(0, max(wordtokens)+10, 10))
    ax.set_xlabel('Length (words)')
    ax.set_ylabel('Counts')
    if not question:
        ax.set_title('{}'.format(exam))
        plt.savefig('{}\{}.png'.format(descr_dir, exam))
    elif question:
        ax.set_title('{}_{}'.format(exam, question))
        plt.savefig('{}\{}_{}.png'.format(descr_dir, exam, question))        
    #plt.show(fig)
    

def create_pandas_df(lca_results_dir, filename):
    """A function that reads the LCA results (per exam) from a CSV file into
    a pandas DataFrame."""
    
    # NB: Make sure that the text file is encoded in UTF-8    
    df = pd.read_csv(os.path.join(lca_results_dir, filename), sep = ",")
    #df.set_index('subjectcode', inplace = True) # Set subject code as index
        
    return df


def all_results(results_dir):
    """A function that calculates the correlation between text length and the \
    25 outcomes of the lexical complexity analysis."""

    lca_dir = os.path.join(results_dir, "lca_untruncated")
    all_result_files = os.listdir(lca_dir)
    
    # Create empty dataframe
    appended_data = []
    
    # Loop through all results from corrected files and append to big dataframe
    for file in all_result_files:
        if file.endswith("corrected.txt"):
            df = pd.read_csv(os.path.join(lca_dir, file), sep = ",")
            appended_data.append(df)
    
    return pd.concat(appended_data)


def correlations(df, results_dir):
    index_ld = df.columns.get_loc('ld')
    lca_measures = df.columns[index_ld:]
    
    # Test whether the text length is normally distributed
    test_stat, p = normaltest(df['wordtokens'])
    
    # Set up file where results should be stored
    destination = os.path.join(results_dir, "length_descriptives", "correlations_wordtokens_measures.txt")
    headers = "\t".join(["measure", "r_spearman", "p_value"])
    with open(destination, 'w') as outfile:
        outfile.write(headers)
        outfile.write("\n")
    
    # If so, use Spearman's correlation coefficient
    if p < 0.05:
        
        with open(destination, "a") as outfile:
        
            for measure in lca_measures:
                corr, sig = spearmanr(df['wordtokens'], df[measure])
                print(measure, "\tSpearman's r =", corr, "\tp = ", sig)
                outfile.write("\t\t".join((measure, str(round(corr, 3)), str(round(sig, 3)), "\n")))
                
    else:
        print("You should implement Pearson's correlation coefficient.")


def main():
    
    # Define directories
    src_dir = os.path.dirname(os.path.abspath(__file__))
    data_dir = os.path.join(src_dir, '..', 'data')
    raw_data_dir = os.path.join(data_dir, 'raw_data')
    results_dir = os.path.join(src_dir, '..', 'results')
    
    # Parse arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('language', help = "Choose the study language: 'EN' or 'NL'.", choices = ["EN", "NL"])
    args = parser.parse_args()
    language = args.language
    
    # Compute descriptives
    files = preprocessing.filenames(raw_data_dir, language)
    calculate_descriptives(files, data_dir, results_dir)

    # Calculate correlations between length and measures
    big_df = all_results(results_dir)
    correlations(big_df, results_dir)
    
# Run code
if __name__ == "__main__":
    main()