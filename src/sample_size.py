#!usr/bin/en/python3
# -*- coding: utf-8 -*-

# Script written by Johanna de Vos (2018)

import argparse
import os
import statistics

import matplotlib.pyplot as plt
import pandas as pd
from scipy.stats import normaltest, pearsonr, spearmanr


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
                histogram(wordtokens, exam, output_dir=output_dir)
            
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
                    histogram(wordtokens, exam, question, 
                              output_dir=output_dir)
    

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
    

def correlations(df, results_dir, lca_min_sam, lca_measures):
    
    # Only use samples that are longer than the specified cut-off point
    df = df[df.wordtokens >= lca_min_sam]
    
    # Exclude a datapoint that is an extremele outlier
    #df = df[df.wordtokens < 350]
    
    # Test whether the text length is normally distributed
    histogram(df['wordtokens'], 'Distribution of text lengths')
    test_stat, p = normaltest(df['wordtokens'])
    #print(test_stat, p)
    
    # Calculate Pearson's correlation coefficients
    # Pearson rather than Spearman because the data look normally distributed
    # (Based on visual inspection)
    corrs = []
    
    for measure in lca_measures:
        corr, sig = pearsonr(df['wordtokens'], df[measure])
        corrs.append(round(corr, 3))
        print(measure, "\tPearson's r =", round(corr, 3), "\tp = ", sig)
           
        histogram(df[measure], 'Distribution of {}'.format(measure))
        df.plot(kind ='scatter', x ='wordtokens', y = measure, title = 
                'Correlation between wordtokens and {}'.format(measure))
                    
    return corrs
                
    
def correlations_per_exam(results_dir, lca_min_sam):
    """A function that calculates the correlation between text length and the \
    outcomes of the lexical complexity analysis."""

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
            
            # Get correlations
            corrs = correlations(df, results_dir, lca_min_sam, lca_measures)
            all_corrs.append(corrs)
            
    # Set up file where results should be stored
    destination = os.path.join(results_dir, "length_descriptives", 
                               "correlations_wordtokens_measures.txt")
    headers = "\t".join(["measure", "r_pearson"])
    with open(destination, 'w') as outfile:
        outfile.write(headers)
        outfile.write("\n")

    # Average the correlations over exams and languages
    for measure in lca_measures:
        index_measure = lca_measures.index(measure)
        corrs_measure = [x[index_measure] for x in all_corrs]
        print("The average correlation between wordtokens and {} is: "
              .format(measure), str(round(statistics.mean(corrs_measure), 2)))
        
        # Write to file
        with open(destination, "a") as outfile:
            outfile.write("\t\t".join((measure, str(round(statistics.mean
                                                          (corrs_measure), 3)), 
                                       "\n")))

    
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
    #parser.add_argument('language', help = "Choose the study language: 'EN' \
    #                    or 'NL'.", choices = ["EN", "NL"])
    parser.add_argument('lca_min_sam', help = "Choose the minimal sample size \
                    with which the data were preprocessed.")
    args = parser.parse_args()
    #language = args.language
    lca_min_sam = int(args.lca_min_sam)
    
    # Create directory to store results
    create_descr_dir(results_dir, 'length_descriptives')
        
    # Compute length descriptives
    files = os.listdir(raw_data_dir)
    calculate_descriptives(files, data_dir, results_dir)
    
    # Calculate correlations per exam
    correlations_per_exam(results_dir, lca_min_sam)


# Run code
if __name__ == "__main__":
    main()