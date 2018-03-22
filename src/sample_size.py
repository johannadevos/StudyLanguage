#!usr/bin/en/python3
# -*- coding: utf-8 -*-

# Script written by Johanna de Vos (2018)

import glob
import os
import statistics

import preprocessing


def calculate_descriptives(files, data_dir):
    for exam in files:
        exam = exam[:-4]
            
        if not "STAT_C" in exam:
            print(exam)
            
            lengths = []
            indiv_files = os.path.join(data_dir, 'indiv_files_untruncated', exam)
            
            for filename in glob.glob(os.path.join(indiv_files,'*')):
                
                with open(filename, 'r') as file:
                    lemmas = file.readlines()
                    
                    if lemmas: # If the file is not empty
                        split_lemmas = lemmas[0].split()
                        length = len(split_lemmas)
                    if not lemmas:
                        length = 0
                        
                    lengths.append(length)
                    
            print("Average length of writing samples: ", statistics.mean(lengths))
            
        elif "STAT_C" in exam:
            lengths = []
            questions = ['4a', '2aDec', '2aCaus']
            
            for question in questions:
                print(exam, question)
                
                indiv_files = os.path.join(data_dir, 'indiv_files_untruncated', exam, question)
                
                for filename in glob.glob(os.path.join(indiv_files,'*')):
                    
                    with open(filename, 'r') as file:
                        lemmas = file.readlines()
                        
                        if lemmas: # If the file is not empty
                            split_lemmas = lemmas[0].split()
                            length = len(split_lemmas)
                        if not lemmas:
                            length = 0
                            
                        lengths.append(length)
                        
                print("Average length of writing samples: ", statistics.mean(lengths))
                
                
def main():
    
    # Define directories
    src_dir = os.path.dirname(os.path.abspath(__file__))
    data_dir = os.path.join(src_dir, '..', 'data')
    raw_data_dir = os.path.join(data_dir, 'raw_data')
    results_dir = os.path.join(src_dir, '..', 'results')
    
    language = "EN"
    files = preprocessing.filenames(raw_data_dir, language)
    calculate_descriptives(files, data_dir)
    
    
if __name__ == "__main__":
    main()