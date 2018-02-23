# -*- coding: utf-8 -*-

# Script written by Johanna de Vos, U908153

# Standard library imports
import os
import random
import re

# Third-party imports
import nltk
import pandas as pd

from gensim import corpora
from nltk.tokenize import RegexpTokenizer
from nltk.stem import WordNetLemmatizer
from nltk.corpus import stopwords

# Set seed for reproducability of results
random.seed(2017)

# Set NLTK directory
nltk.data.path.append("U:/nltk_data")

# Set working directory
os.chdir("C:/Users/U908153/Desktop/GitHub/StudyLanguage/data")

### ------------------
### PREPARING THE DATA
### ------------------

# Open file
def open_file(file):
    print("Opening file...")
    
    with open (file) as file:
        raw_text = file.read()
        
        return raw_text


# Preprocess text
def preprocess(raw_text):
    print("Preprocessing raw text...")
    
    # For empty anwers, insert a dash
    text = raw_text.replace("Antwoord:\n", "Antwoord: -\n")
        
    # Remove white space
    text = text.replace("\\n", "")
    text = text.replace(" /n ", "")
    text = text.replace("/n", "")
    text = text.replace("  ", " ")
    text = text.replace("\n\nTentamennummer", "\nTentamennummer")
     
    # Replace curly quotes
    text = text.replace(chr(0x2019), chr(0x0027)) # Replace right single curly quote with straight quote
    text = text.replace(chr(0x2018), chr(0x0027)) # Replace left single curly quote with straight quote
    text = text.replace(chr(0x201D), chr(0x0022)) # Replace right double curly quotes with straight quotes
    text = text.replace(chr(0x201C), chr(0x0022)) # Replace left double curly quotes with straight quotes

    # Replace abbreviated verbs
    text = text.replace("can't", "cannot")
    text = text.replace("n't", " not")
    
    return text


# Rearrange the students' answers in a dataframe
def create_df(text):
    print("Creating data frame for the student answers...")
    
    exam_numbers = []
    subject_codes = []
    grades = []
    answers = []
    
    # Split text by newline
    text = text.split("\n")
    
    # Extract information from running text
    for i in range(len(text)):
        if i%4 == 0:
            exam_number = text[i][16:]
            exam_numbers.append(exam_number)
        elif i%4 == 1:
            subject_code = text[i][13:]
            subject_codes.append(subject_code)
        elif i%4 == 2:
            grade = text[i][7:]
            grades.append(int(grade))
        elif i%4 == 3:
            answer = text[i][10:]
            answers.append(answer)
            
    # Create dataframe
    df = pd.DataFrame({'ExamNumber': exam_numbers, 'SubjectCode': subject_codes, 'Grade': grades, 'Answer': answers})      
    
    # Add empty columns that can later contain tokenized and lemmatized data
    df['Tokenized'] = ""
    df['Lemmatized'] = ""
    df['NoStops'] = "" 
    
    # Change order of columns
    cols = ['SubjectCode', 'ExamNumber', 'Grade', 'Answer', 'Tokenized', 'Lemmatized', 'NoStops']
    df = df[cols]    

    return df, cols


# Tokenize, lemmatize, and remove stop words
def tok_lem(df):
    print("Tokenizing, lemmatizing, and removing stop words...")
    
    # Make SubjectCode the index of the dataframe
    #df = df.set_index("SubjectCode") --> with this line, preprocessing the book no longer works
    
    # Set up tokenizer and lemmatizer
    tokenizer = RegexpTokenizer(r'\w+')
    lemmatizer = WordNetLemmatizer()    
    
    for i in range(len(df)):
        answer = df['Answer'][i]
        
        # Preprocess     
        answer = answer.replace("'s", "") # Remove possessive 's
        answer = answer.lower() # Make lowercase
        answer = re.sub("\d+", "", answer) # Remove numbers
 
        # Preprocess when using spelling-corrected data
        answer = answer.replace("_abbreviation", "")
        answer = answer.replace("_nonexistent", "")
        answer = answer.replace("_dutch", "")
        answer = answer.replace("_german", "")
        
        # Tokenize
        #tok_answer = word_tokenize(answer)
        tok_answer = tokenizer.tokenize(answer) # also removes apostrophe
        df['Tokenized'][i] = tok_answer
        
        # Lemmatize
        lem_answer = []
        
        for word in tok_answer:
            lemma = lemmatizer.lemmatize(word, pos = 'v') # default POS is 'n'
            
            # Hand-crafted rules for words not handled well by lemmatizer
            if lemma.startswith("whorf"):
                lemma = "whorf"
            
            lem_answer.append(lemma)
        
        df['Lemmatized'][i] = lem_answer

        # Remove stop words
        stopped_lemmas = [i for i in lem_answer if not i in stopwords.words('english')]
        df['NoStops'][i] = stopped_lemmas
        
    return df

# TO DO: standardise British/American spelling?


# Create dictionary of vocabulary
def dictionary(df):
    print("Creating a dictionary...")
    
    dictionary = corpora.Dictionary(df['NoStops'])
    #print(dictionary.token2id)
    
    return dictionary


# Set up output file
def setup_outfile():
    #outfile = "C:/Users/johan/Dropbox/PhD/Courses/Text and multimedia mining/Project/Results.txt"
    #outfile = "D:/Dropbox2/Dropbox/PhD/Courses/Text and multimedia mining/Project/Results2.txt"
    outfile = "D:/Dropbox2/Dropbox/PhD/Courses/Text and multimedia mining/Project/Python"
    headers = ["Model", "Counting", "TrainOrTest", "TrainingData", "Mapping", "SpellingCorrection", "Pearson", "SigPearson", "Spearman", "SigSpearman"]
    headers2 = "\t".join(headers)
    f = open(outfile, 'w') 
    f.write(headers2)
    f.close()
    
    return outfile


# Save to output file
def save_to_file(info, outfile):
    info2 = "\t".join(info)
    f = open(outfile, 'a') #open our results file in append mode so we don't overwrite anything
    f.write('\n') # write a line ending
    f.write(info2) #write the string they typed
    f.close() #close and "save" the output file
        

### --------
### RUN CODE
### --------

if __name__ == "__main__":
    
    # Read and prepare student data
    aip_en_unc = open_file('AIP_A_EN_uncorrected.txt') # Read student answers
    aip_en_prep = preprocess(aip_en_unc) # Preprocess student answers
    df, cols = create_df(aip_en_prep) # Create dataframe of student answers
    df = tok_lem(df) # Tokenize and lemmatize all answers, and remove stop words
    dictio = dictionary(df) # Create dictionary of vocabulary