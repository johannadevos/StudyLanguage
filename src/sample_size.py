#!usr/bin/en/python3
# -*- coding: utf-8 -*-

# Script written by Johanna de Vos (2018)

import argparse
import os
import statistics

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

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
            histogram(results_dir, wordtokens, exam)
        
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
                histogram(results_dir, wordtokens, exam, question)
    

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
        
    
# Run code
if __name__ == "__main__":
    main()