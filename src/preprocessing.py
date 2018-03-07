# -*- coding: utf-8 -*-

# Script written by Johanna de Vos (2018)

import os
from pathlib import Path
import random
import re

import nltk
from nltk.corpus import stopwords, wordnet
from nltk.stem import WordNetLemmatizer
from nltk.tag import pos_tag
from nltk.tokenize import RegexpTokenizer
import pandas as pd

### ------------------
### PREPARING THE DATA
### ------------------

# Open file
def open_file(filename):
    print("Opening file...")
    
    with open (filename, 'r') as f:
        raw_text = f.read()
        
        return raw_text


# Make the raw text readable
def make_readable(raw_data):
    print("Making the raw text readable...")
    
    text = raw_data
    
    # For empty anwers, insert a dash
    text = text.replace("Antwoord:\n\n", "Antwoord: -\n")
    
    # Preprocessing for STAT_C, which has a different format than the other transcriptions
    text = text.replace("\n2)", " 2)") # Put the 3-part answer on one line
    text = text.replace("\n3)", " 3)") # Idem
    text = text.replace("\n2&3)", " 2&3)")
    text = re.sub(r'-{2,}', "", text) # Remove two or more dashes
    
    # Preprocessing for STAT_C_EN
    text = re.sub(r'Answer: *\nDecision', "Answer: Decision", text) # Remove newline between Answer and Decision
    text = re.sub(r'Answer: *\nCausal', "Answer: Causal", text) # Remove newline between Answer and Causal explanation
    text = re.sub(r'Subtracted [p|P]oints 2a \(original\): -?\d*.?', "", text) # Remove the original points
    text = re.sub(r'Subtracted [p|P]oints \(Rick\): -?\d*.?', "", text) # Remove the points assigned by Rick
    text = text.replace("Exam number", "Tentamennummer")
    text = text.replace("Subject code", "Subjectcode")
    text = text.replace("Points", "Punten")
    text = text.replace("Answer", "Antwoord")
    
    # Preprocessing for STAT_C_NL
    text = text.replace("Punten 4a", "Punten")
    text = re.sub(r'Antwoord: *\nBeslissing', "Antwoord: Beslissing", text) # Remove newline between Answer and Decision
    text = re.sub(r'Antwoord: *\nCausale', "Antwoord: Causale", text) # Remove newline between Answer and Causal explanation
    text = re.sub(r'Aftrekpunten 2a: -?\d*.?', "", text) # Remove the original points
        
    # Remove white space
    text = text.replace("\\n", "")
    text = text.replace(" /n ", "")
    text = text.replace("/n", "")
    text = text.replace("\n\n", "\n")
    text = text.replace("\n\n", "\n")
    text = text.replace("\n\n", "\n")
    text = text.replace("  ", " ")
    text = text.replace("\n\nTentamennummer", "\nTentamennummer")
    text = text.replace("Punten:  ", "Punten: ")
     
    # Replace curly quotes
    text = text.replace(chr(0x2019), chr(0x0027)) # Replace right single curly quote with straight quote
    text = text.replace(chr(0x2018), chr(0x0027)) # Replace left single curly quote with straight quote
    text = text.replace(chr(0x201D), chr(0x0022)) # Replace right double curly quotes with straight quotes
    text = text.replace(chr(0x201C), chr(0x0022)) # Replace left double curly quotes with straight quotes

    # Replace abbreviated verbs
    text = text.replace("can't", "cannot")
    text = text.replace("n't", " not")
    
    # Other
    text = text.replace("???", "?") # One question mark for missing subject codes
    
    return text


# Rearrange the students' answers in a dataframe
def create_df(text, filename):
    print("Creating data frame for the student answers...")
    
    # Split text by newline
    text = text.split("\n")

    # Create empty lists
    exam_numbers = []
    subject_codes = []
    grades = []
    answers = []
        
    # Extract info    
    if not "STAT_C" in str(filename):
        mod = 4
    elif "STAT_C" in str(filename):
        mod = 8
        grades_2a_dec = []
        answers_2a_dec = []
        grades_2a_caus = []
        answers_2a_caus = []

    # Extract information from running text
    for i in range(len(text)):
        if i%mod == 0:
            exam_number = text[i][16:]
            #print(exam_number)
            exam_numbers.append(exam_number)
        elif i%mod == 1:
            subject_code = text[i][13:]
            #print(subject_code)
            subject_codes.append(subject_code)
        elif i%mod == 2:
            grade = text[i][8:]
            #print(grade)
            grades.append(int(grade))
            #grades.append(grade)
        elif i%mod == 3:
            answer = text[i][10:]
            #print(answer)
            answers.append(answer)
        
        if "STAT_C" in str(filename):
            if i%mod == 4:
                grade_2a_dec = text[i][8:]
                grades_2a_dec.append(int(grade_2a_dec))
                #grades_2a_dec.append(grade_2a_dec)
            elif i%mod == 5:
                answer_2a_dec = text[i][10:]
                answers_2a_dec.append(answer_2a_dec)                
            elif i%mod == 6:
                grade_2a_caus = text[i][8:]
                grades_2a_caus.append(int(grade_2a_caus))
                #grades_2a_caus.append(grade_2a_caus)
            elif i%mod == 7:
                answer_2a_caus = text[i][10:]
                answers_2a_caus.append(answer_2a_caus)
                                                    
    # Create dataframe
    if not "STAT_C" in str(filename):
        df = pd.DataFrame({'ExamNumber': exam_numbers, 'SubjectCode': subject_codes, 'Grade': grades, 'Answer': answers})      
    
        # Add empty columns that can later contain tokenized and lemmatized data
        df['Tokenized'] = ""
        df['POS'] = ""
        df['Lemmatized'] = ""
        df['NoStops'] = ""
        
        # Change order of columns
        cols = ['SubjectCode', 'ExamNumber', 'Grade', 'Answer', 'Tokenized', 'POS', 'Lemmatized', 'NoStops']    
        df = df[cols]
        
    elif "STAT_C" in str(filename):
        df = pd.DataFrame({'ExamNumber': exam_numbers, 'SubjectCode': subject_codes, 'Grade4a': grades, 'Answer4a': answers, 'Grade2aDec': grades_2a_dec, 'Answer2aDec': answers_2a_dec, 'Grade2aCaus': grades_2a_caus, 'Answer2aCaus': answers_2a_caus}) 
    
        # Add empty columns that can later contain tokenized and lemmatized data
        df['Tokenized4a'] = ""
        df['POS4a'] = ""
        df['Lemmatized4a'] = ""
        df['NoStops4a'] = ""  
        df['Tokenized2aDec'] = ""
        df['POS2aDec'] = ""
        df['Lemmatized2aDec'] = ""
        df['NoStops2aDec'] = ""            
        df['Tokenized2aCaus'] = ""
        df['POS2aCaus'] = ""
        df['Lemmatized2aCaus'] = ""
        df['NoStops2aCaus'] = ""           
          
        cols = ['SubjectCode', 'ExamNumber', 'Grade4a', 'Answer4a', 'Tokenized4a', 'POS4a', 'Lemmatized4a', 'NoStops4a', 'Grade2aDec', 'Answer2aDec', 'Tokenized2aDec', 'POS2aDec', 'Lemmatized2aDec', 'NoStops2aDec', 'Grade2aCaus', 'Answer2aCaus', 'Tokenized2aCaus', 'POS2aCaus', 'Lemmatized2aCaus', 'NoStops2aCaus']
        df = df[cols]
    
    return df, cols


# Convert POS tag to Wordnet POS tags (needed for later lemmatization)
def get_wordnet_pos(treebank_tag):

    if treebank_tag.startswith('J'):
        return wordnet.ADJ
    elif treebank_tag.startswith('V'):
        return wordnet.VERB
    elif treebank_tag.startswith('N'):
        return wordnet.NOUN
    elif treebank_tag.startswith('R'):
        return wordnet.ADV
    else:
        return wordnet.NOUN # Because nouns are the default POS tag in the Wordnet lemmatizer
    

# Tokenize, POS tag, lemmatize
def preprocess(df, filename):
    print("Tokenizing, POS tagging, lemmatizing, and removing stop words...")

    tokenizer = RegexpTokenizer(r'\w+')
    lemmatizer = WordNetLemmatizer() 

    if not "STAT_C" in str(filename):
        answers = ['Answer']
    elif "STAT_C" in str(filename):
        answers = ['Answer4a', 'Answer2aDec', 'Answer2aCaus']
        
    for question_type in answers:
        for word_counter in range(len(df)):
            word_in_answer = df[question_type][word_counter]
            
            # Logging
            if word_counter%20 == 0:
                print(word_counter, "/", len(df), question_type[6:])
            
            # Remove unnecessary features
            word_in_answer = word_in_answer.replace("'s", "") # Remove possessive 's
            word_in_answer = word_in_answer.lower() # Make lowercase
            word_in_answer = re.sub("\d+", "", word_in_answer) # Remove numbers
     
            # Preprocess when using spelling-corrected data
            word_in_answer = word_in_answer.replace("_abbreviation", "")
            word_in_answer = word_in_answer.replace("_nonexistent", "")
            word_in_answer = word_in_answer.replace("_dutch", "")
            word_in_answer = word_in_answer.replace("_german", "")
            
            # Tokenize and write to df
            tok_answer = tokenizer.tokenize(word_in_answer) # also removes apostrophe
            #df.at[word_counter, 'Tokenized%s' % question_type[6:]] = tok_answer                             
            df.at[word_counter, 'Tokenized{}'.format(question_type[6:])] = tok_answer                             
                        
            # POS tag and write to df
            pos_answer = pos_tag(tok_answer)
            
            pos_tags = []
            for word, tag in pos_answer:
                pos_tags.append(tag)
            
            df.at[word_counter, 'POS{}'.format(question_type[6:])] = pos_tags
            
            # Lemmatize and write to df
            lemmas = []
            
            for word, tag in pos_answer:
                lemma = lemmatizer.lemmatize(word, pos = get_wordnet_pos(tag)) 
                lemmas.append(lemma)
                df.at[word_counter, 'Lemmatized{}'.format(question_type[6:])] = lemmas
            
            # Remove stop words
            stopped_lemmas = [i for i in lemmas if not i in stopwords.words('english')]
            df.at[word_counter, 'NoStops{}'.format(question_type[6:])] = stopped_lemmas
                        
    return df

# TO DO: standardise British/American spelling?


def remove_subjects(df):
    print("Removing rows where the subject code is unknown...")
    df = df[df.SubjectCode != '?']
    
    return df


'''
# Create dictionary of vocabulary
def dictionary(df):
    print("Creating a dictionary...")
    
    dictionary = corpora.Dictionary(df['NoStops'])
    #print(dictionary.token2id)
    
    return dictionary
'''


### --------
### WRITE EACH ANSWER TO AN INDIVIDUAL FILE
### --------

# List the names of the files associated with each exam
def filenames(raw_data_dir):
    dir_contents = os.listdir(raw_data_dir)
    files = [file for file in dir_contents]
    
    return files


# Create a directory for each exam
def create_lca_dirs(data_dir):
    indiv_data_dir = data_dir / 'indiv_files'

    if not os.path.exists(indiv_data_dir):
        os.makedirs(indiv_data_dir)
    
        exam_names = filenames(data_dir / 'raw_data')
        questions_stat_c = ['4a', '2aDec', '2aCaus']
        
        for exam in exam_names:
            os.makedirs(indiv_data_dir / exam[:-4]) # [:-4] to cut off '.txt'
            
            # Make subdirectories for each question for STAT_C
            if 'STAT_C' in exam:
                
                for question in questions_stat_c:
                    os.makedirs(indiv_data_dir / exam[:-4] / question)
                    

# Write the lemmatized and POS-tagged student answers to files
def create_lca_input(data_dir, df, filename):
    print("Writing lemmatized and POS-tagged words to files...")
    
    indiv_data_dir = data_dir / 'indiv_files'

    for index, row in df.iterrows():
        subject_code = str(row['SubjectCode'] + ".txt")
                    
        if not "STAT_C" in str(filename):
            outfile = indiv_data_dir / filename[:-4] / subject_code # :-4 to cut off '.txt'
            
            with open(outfile, 'w') as f:
                for word_counter in range(len(row['POS'])):
    
                    lem_pos = row['Lemmatized'][word_counter], "_", row['POS'][word_counter]
                    f.write('{} {}'.format(''.join(lem_pos), ''))
   
        elif "STAT_C" in str(filename):
            questions = ['4a', '2aDec', '2aCaus']
            
            for question in questions:
                outfile = indiv_data_dir / filename[:-4] / question / subject_code # :-4 to cut off '.txt'
                
                with open(outfile, 'w') as f:
                    
                    for word_counter in range(len(row['POS{}'.format(question)])):
    
                        lem_pos = row['Lemmatized{}'.format(question)][word_counter], "_", row['POS{}'.format(question)][word_counter]
                        f.write('{} {}'.format(''.join(lem_pos), ''))
                        
                        
### ------------------
### DIRECTORIES
### ------------------

# Set NLTK directory
if os.path.exists("U:/nltk_data"):
    nltk.data.path.append("U:/nltk_data") # On work PC only

# Set working directory to where the data are
def get_current_file_dir() -> Path:
    """Returns the directory of the script."""
    try:
        return Path(os.path.realpath(__file__)).parent
    except(NameError):
        return Path(os.getcwd())

# Define directories
src_dir = get_current_file_dir()
data_dir = src_dir / 'data'
indiv_data_dir = data_dir / 'indiv_files'
raw_data_dir = data_dir / 'raw_data'


### --------
### RUN CODE
### --------

if __name__ == "__main__":
    
    # Read and prepare student data (select individual files)
    #filename = 'AIP_A_EN_uncorrected.txt'
    #filename = 'AIP_A_EN_corrected.txt'
    #filename = 'AIP_A_NL_uncorrected.txt'
    #filename = 'AIP_A_NL_corrected.txt'  
    #filename = 'STAT_A_EN_uncorrected.txt'
    #filename = 'STAT_A_EN_corrected.txt'             
    #filename = 'STAT_A_NL_uncorrected.txt'
    #filename = 'STAT_A_NL_corrected.txt'
    #filename = 'STAT_C_EN_uncorrected.txt'
    #filename = 'STAT_C_EN_corrected.txt'        
    #filename = 'STAT_C_NL_uncorrected.txt' 
    #filename = 'STAT_C_NL_corrected.txt'
    
    # Read and prepare student data (all at once)
    create_lca_dirs(data_dir) # Create folders to store the individual student answers
    files = filenames(raw_data_dir)
    
    for filename in files:
        if "EN" in filename: # For the time being, only work with English data
        
            print("\n{}\n".format(filename))
            raw_data = open_file(raw_data_dir / filename) # Read data from file
            prep_data = make_readable(raw_data) # Make student answers readable
            df, cols = create_df(prep_data, filename) # Create and fill dataframe with student data
            
            #df = df[:10] # Make df smaller to try things out
            df = preprocess(df, filename) # Tokenize, POS tag, lemmatize, and remove stop words
            df = remove_subjects(df) # Remove entries where the subject code is unknown or students did not give permission for their data to be used
                
            #dictio = dictionary(df) # Create dictionary of vocabulary --> currently, doesn't work for STAT_C
            
            create_lca_input(data_dir, df, filename) # Create the input files that the LCA needs