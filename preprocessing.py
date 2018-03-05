# -*- coding: utf-8 -*-

# Script written by Johanna de Vos (2018)

# Standard library imports
import random
import re

# Third-party imports
import pandas as pd

from gensim import corpora

from nltk.tag import pos_tag
from nltk.tokenize import RegexpTokenizer
from nltk.stem import WordNetLemmatizer
from nltk.corpus import stopwords, wordnet

# Set seed for reproducability of results
random.seed(2017)


### ------------------
### PREPARING THE DATA
### ------------------

# Open file
def open_file(file):
    print("Opening file...")
    
    with open (file) as file:
        raw_text = file.read()
        
        return raw_text


# Make the raw text readable
def make_readable(raw_text):
    print("Making the raw text readable...")
    
    # For empty anwers, insert a dash
    text = raw_text.replace("Antwoord:\n", "Antwoord: -\n")
    
    # Preprocessing for STAT_C, which has a different format than the other transcriptions
    text = text.replace("\n2)", " 2)") # Put the 3-part answer on one line
    text = text.replace("\n3)", " 3)") # Idem
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
    text = text.replace("???", "?")
    
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

    if not "STAT_C_EN" in str(filename):
        answers = ['Answer']
    elif "STAT_C_EN" in str(filename):
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
            df.at[word_counter, 'Tokenized%s' % question_type[6:]] = tok_answer                             
            
            # POS tag and write to df
            pos_answer = pos_tag(tok_answer)
            
            pos_tags = []
            for word, tag in pos_answer:
                pos_tags.append(tag)
            
            df.at[word_counter, 'POS%s' % question_type[6:]] = pos_tags
            
            # Lemmatize and write to df
            lemmas = []
            
            for word, tag in pos_answer:
                lemma = lemmatizer.lemmatize(word, pos = get_wordnet_pos(tag)) 
                lemmas.append(lemma)
                df.at[word_counter, 'Lemmatized%s' % question_type[6:]] = lemmas
            
            # Remove stop words
            stopped_lemmas = [i for i in lemmas if not i in stopwords.words('english')]
            df.at[word_counter, 'NoStops%s' % question_type[6:]] = stopped_lemmas
                        
    return df

# TO DO: standardise British/American spelling?


'''
# Create dictionary of vocabulary
def dictionary(df):
    print("Creating a dictionary...")
    
    dictionary = corpora.Dictionary(df['NoStops'])
    #print(dictionary.token2id)
    
    return dictionary


### --------
### SAVING RESULTS
### --------

# Set up output file
def setup_outfile():
    outfile = path
    headers = ["Header1", "Header2"]
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
'''