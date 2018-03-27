#!usr/bin/en/python3
# -*- coding: utf-8 -*-

# Script written by Johanna de Vos (2018)
# Requires Python 3.6

import argparse
import glob
import os
import re
import shutil

import pandas as pd

import lca


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

        # Change order of columns
        cols = ['SubjectCode', 'ExamNumber', 'Grade', 'Answer', 'Tokenized', 'POS', 'Lemmatized']    
        df = df[cols]

    elif "STAT_C" in str(filename):
        df = pd.DataFrame({'ExamNumber': exam_numbers, 'SubjectCode': subject_codes, 'Grade4a': grades, 'Answer4a': answers, 'Grade2aDec': grades_2a_dec, 'Answer2aDec': answers_2a_dec, 'Grade2aCaus': grades_2a_caus, 'Answer2aCaus': answers_2a_caus})

        # Add empty columns that can later contain tokenized, POS-tagged and lemmatized data
        df['Tokenized4a'] = ""
        df['POS4a'] = ""
        df['Lemmatized4a'] = ""
        df['Tokenized2aDec'] = ""
        df['POS2aDec'] = ""
        df['Lemmatized2aDec'] = ""
        df['Tokenized2aCaus'] = ""
        df['POS2aCaus'] = ""
        df['Lemmatized2aCaus'] = ""

        cols = ['SubjectCode', 'ExamNumber', 'Grade4a', 'Answer4a', 'Tokenized4a', 'POS4a', 'Lemmatized4a', 'Grade2aDec', 'Answer2aDec', 'Tokenized2aDec', 'POS2aDec', 'Lemmatized2aDec', 'Grade2aCaus', 'Answer2aCaus', 'Tokenized2aCaus', 'POS2aCaus', 'Lemmatized2aCaus']
        df = df[cols]

    return df, cols


# Preprocess the Dutch data with Frog
def prep_nl(df, filename):
    from frog import Frog, FrogOptions

    print("Tokenizing, POS tagging, and lemmatizing the Dutch data...")

    # Create 'frog' instance. Turn off various options to save time.
    frog = Frog(FrogOptions(parser=False, morph=False, chunking=False, ner=False))

    # Define set of possible answers
    if not "STAT_C" in str(filename):
        answers = ['Answer']
    elif "STAT_C" in str(filename):
        answers = ['Answer4a', 'Answer2aDec', 'Answer2aCaus']

    # Loop through answers
    for question_type in answers:

        for index in df.index:
            ans = df.loc[index, question_type]

            # Logging
            if index%20 == 0:
                print(index, "/", df.index[-1], question_type[6:])

            # Remove numbers
            ans = re.sub("\d+", "", ans)

            # Remove tags in spelling-corrected data
            ans = ans.replace("_abbreviation", "")
            ans = ans.replace("_nonexistent", "")
            ans = ans.replace("_dutch", "")
            ans = ans.replace("_german", "")

            # Preprocess the data with Frog
            ans_dict = frog.process(ans)

            tok_answer = []
            lem_answer = []
            pos_tags = []

            # Append outcomes to list
            for word_index in range(len(ans_dict)):
                if ans_dict[word_index]['pos'] != "LET()": # Exclude punctuation
                    tok_answer.append(ans_dict[word_index]['text'].lower())
                    lem_answer.append(ans_dict[word_index]['lemma'])
                    pos_tags.append(ans_dict[word_index]['pos'])

            # Fill in the dataframe
            df.at[index, 'Tokenized{}'.format(question_type[6:])] = tok_answer
            df.at[index, 'Lemmatized{}'.format(question_type[6:])] = lem_answer
            df.at[index, 'POS{}'.format(question_type[6:])] = pos_tags

    return df


# Preprocess the English data with NLTK
def prep_en(df, filename):
    print("Tokenizing, POS tagging, lemmatizing, and removing stop words in the English data...")

    # Import NTLK modules and correct path to NLTK data
    import nltk
    from nltk.stem import WordNetLemmatizer
    from nltk.tag import pos_tag
    from nltk.tokenize import RegexpTokenizer

    # Set NLTK directory
    if os.path.exists("U:/nltk_data"):
        nltk.data.path.append("U:/nltk_data") # On work PC only

    # Define tokenizer and lemmatizer
    tokenizer = RegexpTokenizer(r'\w+')
    lemmatizer = WordNetLemmatizer() 

    # Define set of possible answers
    if not "STAT_C" in str(filename):
        answers = ['Answer']
    elif "STAT_C" in str(filename):
        answers = ['Answer4a', 'Answer2aDec', 'Answer2aCaus']

    # Loop through answers
    for question_type in answers:

        for index in df.index:
            ans = df.loc[index, question_type]

            # Logging
            if index%20 == 0:
                print(index, "/", df.index[-1], question_type[6:])

            # Remove numbers
            ans = re.sub("\d+", "", ans)

            # Remove tags in spelling-corrected data
            ans = ans.replace("_abbreviation", "")
            ans = ans.replace("_nonexistent", "")
            ans = ans.replace("_dutch", "")
            ans = ans.replace("_german", "")

            # Tokenize and write to df
            tok_answer = tokenizer.tokenize(ans.lower())
            df.at[index, 'Tokenized{}'.format(question_type[6:])] = tok_answer

            # POS tag and write to df
            pos_answer = pos_tag(tok_answer)

            pos_tags = []
            for word, tag in pos_answer:
                pos_tags.append(tag)

            df.at[index, 'POS{}'.format(question_type[6:])] = pos_tags

            # Lemmatize and write to df
            lemmas = []

            for word, tag in pos_answer:
                lemma = lemmatizer.lemmatize(word, pos = get_wordnet_pos(tag)) 
                lemmas.append(lemma)
                df.at[index, 'Lemmatized{}'.format(question_type[6:])] = lemmas

    return df

# TO DO: standardise British/American spelling?


# Convert POS tag to Wordnet POS tags (needed for later lemmatization of English data)
def get_wordnet_pos(treebank_tag):
    from nltk.corpus import wordnet

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


### -------------------------------------
### WRITE THE ANSWERS TO INDIVIDUAL FILES
### -------------------------------------

# List the names of the files associated with each exam
def filenames(directory, language):
    dir_contents = os.listdir(directory)
    if language == "NL":
        files = [file for file in dir_contents if 'NL' in file]
    elif language == "EN":
        files = [file for file in dir_contents if 'EN' in file]

    return files


# Create a directory for the individual files from each exam
def create_new_dirs(data_dir, language):
    
    new_dirs = ['indiv_files_untruncated', 'indiv_files_truncated']
    
    for new_dir in new_dirs:
        new_dir = os.path.join(data_dir, new_dir)
    
        # If the directory already exists, remove its contents so that a clean new dataset will be produced (otherwise, unused subjects might still have a file here, which would contaminate the results)
        if os.path.exists(new_dir):
            print("Removing", new_dir, "...")
            shutil.rmtree(new_dir)
        
        # If the directory does not exist, create it
        if not os.path.exists(new_dir):
            os.makedirs(new_dir)
    
        # Loop through exams and questions to create subdirectories
        exam_names = filenames(os.path.join(data_dir, 'raw_data'), language)
        questions_stat_c = ['4a', '2aDec', '2aCaus']
    
        for exam in exam_names:
            if not os.path.exists(os.path.join(new_dir, exam[:-4])):
                os.makedirs(os.path.join(new_dir, exam[:-4])) # [:-4] to cut off '.txt'
    
                # Make subdirectories for each question for STAT_C
                if 'STAT_C' in exam:
    
                    for question in questions_stat_c:
                        os.makedirs(os.path.join(new_dir, exam[:-4], question))


# Write the lemmatized and POS-tagged student answers to files
def create_lca_input(data_dir, df, filename):
    print("Writing lemmatized and POS-tagged words to files...")

    indiv_data_dir = os.path.join(data_dir, 'indiv_files_untruncated')

    for index, row in df.iterrows():
        subject_code = str(row['SubjectCode'] + ".txt")

        if not "STAT_C" in str(filename):
            outfile = os.path.join(indiv_data_dir, filename[:-4], subject_code) # :-4 to cut off '.txt'

            with open(outfile, 'w') as f:
                for word_counter in range(len(row['POS'])):

                    lem_pos = row['Lemmatized'][word_counter], "_", row['POS'][word_counter]
                    f.write('{} {}'.format(''.join(lem_pos), ''))

        elif "STAT_C" in str(filename):
            questions = ['4a', '2aDec', '2aCaus']

            for question in questions:
                outfile = os.path.join(indiv_data_dir, filename[:-4], question, subject_code) # :-4 to cut off '.txt'

                with open(outfile, 'w') as f:

                    for word_counter in range(len(row['POS{}'.format(question)])):

                        lem_pos = row['Lemmatized{}'.format(question)][word_counter], "_", row['POS{}'.format(question)][word_counter]
                        f.write('{} {}'.format(''.join(lem_pos), ''))


def truncate_indiv_files(filename, data_dir, language, lca_min_sam):
    print("Truncating the student answers at", str(lca_min_sam), "words and writing these truncated answers to files...")

    # Loop over all the individual files of a given exam
    exam = filename[:-4]
    
    if not "STAT_C" in exam:
        indiv_files = os.path.join(data_dir, 'indiv_files_untruncated', exam)
        
        for filename in glob.glob(os.path.join(indiv_files,'*')):
            
            with open(filename, 'r') as file:
                lemmas = file.readlines()
                
                if lemmas: # If the file is not empty
                    split_lemmas = lemmas[0].split()
                    
                    if len(split_lemmas) >= lca_min_sam:
                        
                        outfile = os.path.join(data_dir, "indiv_files_truncated", exam, filename[-7:])
                        
                        with open (outfile, 'w') as f:
                            for lemma in split_lemmas[:lca_min_sam]:
                                f.write(lemma + " ")
    
    elif "STAT_C" in exam:
        questions = ["2aCaus", "2aDec", "4a"]

        for question in questions:
            
            indiv_files = os.path.join(data_dir, 'indiv_files_untruncated', exam, question)
            
            for filename in glob.glob(os.path.join(indiv_files,'*')):
                
                with open(filename, 'r') as file:
                    lemmas = file.readlines()
                    
                    if lemmas: # If the file is not empty
                        split_lemmas = lemmas[0].split()
                        
                        if len(split_lemmas) >= lca_min_sam:
                            
                            outfile = os.path.join(data_dir, "indiv_files_truncated", exam, question, filename[-7:])
                            
                            with open (outfile, 'w') as f:
                                for lemma in split_lemmas[:lca_min_sam]:
                                    f.write(lemma + " ")


### -----------------------------------
### RUN THE LEXICAL COMPLEXITY ANALYSIS
### -----------------------------------
           
# Run LCA and write outcomes to file                         
def run_lca(filename, data_dir, results_dir, language, lca_min_sam):
    print("Running the Lexical Complexity Analysis...")
    
    if not "STAT_C" in str(filename):
        untrucated_directory = os.path.join(data_dir, "indiv_files_untruncated", filename[:-4])
        truncated_directory = os.path.join(data_dir, "indiv_files_truncated", filename[:-4])

        # Run LCA
        untrucated_results = lca.run_lca(lca_min_sam, untrucated_directory, language)
        truncated_results = lca.run_lca(lca_min_sam, truncated_directory, language)

        # Write untrucated LCA results to output file
        untrucated_outfile = os.path.join(results_dir, "lca_untruncated", str(filename[:-4] + ".txt"))

        with open (untrucated_outfile, "w") as f:
            for result in untrucated_results:
                f.write(str(result))
        
        # Write truncated LCA results to output file
        truncated_outfile = os.path.join(results_dir, "lca_truncated", str(filename[:-4] + ".txt"))

        with open (truncated_outfile, "w") as f:
            for result in truncated_results:
                f.write(str(result))

    elif "STAT_C" in str(filename):
        questions = ["2aCaus", "2aDec", "4a"]

        for question in questions:
            untrucated_directory = os.path.join(data_dir, "indiv_files_untruncated", filename[:-4], question)
            truncated_directory = os.path.join(data_dir, "indiv_files_truncated", filename[:-4], question)
            
            # Run LCA
            untrucated_results = lca.run_lca(lca_min_sam, untrucated_directory, language)
            truncated_results = lca.run_lca(lca_min_sam, truncated_directory, language)

            # Write untrucated LCA results to output file
            untrucated_outfile = os.path.join(results_dir, "lca_untruncated", str(filename[:-4] + "_" + question + ".txt"))

            with open (untrucated_outfile, "w") as f:
                for result in untrucated_results:
                    f.write(str(result))
                    
            # Write truncated LCA results to output file
            truncated_outfile = os.path.join(results_dir, "lca_truncated", str(filename[:-4] + "_" + question + ".txt"))

            with open (truncated_outfile, "w") as f:
                for result in truncated_results:
                    f.write(str(result))                                    


### ------------
### SUBJECT INFO
### ------------

# Read the text file containing subject info
def read_subject_info(data_dir):
    subject_file = os.path.join(data_dir, 'subject_info.txt')
    subject_df = pd.read_csv(subject_file, sep = '\t')
    subject_df.set_index('SubjectCode', inplace = True) # Set subject code as index
    
    return subject_df


# Remove subjects for various reasons
def remove_subjects(df, good_subjects):
    print("Removing some subjects...")
    
    # Remove subjects with unknown subject code
    df = df[df.SubjectCode != '?']

    # Convert the integers in good_subjects to strings, so that they can be compared to the subject codes in the dataframe, which are also strings
    good_subjects = [str(subject) for subject in good_subjects]
    
    # Remove subjects who are not in subject_info (for example because of their nationality, or because they didn't give permission for their data to be used)
    df = df[df.SubjectCode.isin(good_subjects)]
    
    return df


### -------------
### MAIN FUNCTION
### -------------

def main():

    # Define directories
    src_dir = os.path.dirname(os.path.abspath(__file__))
    data_dir = os.path.join(src_dir, '..', 'data')
    raw_data_dir = os.path.join(data_dir, 'raw_data')
    results_dir = os.path.join(src_dir, '..', 'results')

    # Set up argparse
    parser = argparse.ArgumentParser()

    parser.add_argument("language", help = "Choose the study language: 'EN' or 'NL'.", choices = ["EN", "NL"])
    parser.add_argument("lca_min_sam", help = "Choose the minimal sample size that the LCA uses to calculate the number of different words (NDW) and the type-token ratio (TTR).")

    args = parser.parse_args()
    language = args.language
    lca_min_sam = int(args.lca_min_sam)

    print("Study language is:", language)
    print("The minimal sample size for the LCA is:", lca_min_sam)

    # Read in subject info
    subject_df = read_subject_info(data_dir)
    good_subjects = list(subject_df.index)


    ### --------
    ### LOOPING THROUGH ALL FILES
    ### --------

    # Read and prepare student data (all at once)
    if language == "EN":
        files = filenames(raw_data_dir, "EN")
    elif language == "NL":
        files = filenames(raw_data_dir, "NL")

    # Create directories to store the data
    create_new_dirs(data_dir, language)
    
    if not os.path.exists(results_dir):
        os.makedirs(results_dir)
    if not os.path.exists(os.path.join(results_dir, "lca_untruncated")):
        os.makedirs(os.path.join(results_dir, "lca_untruncated"))
    if not os.path.exists(os.path.join(results_dir, "lca_truncated")):
        os.makedirs(os.path.join(results_dir, "lca_truncated"))

    # Loop through files to read, analyze and write
    for filename in files:

        print("\n{}\n".format(filename[:-4]))

        # Read and preprocess data
        raw_data = open_file(os.path.join(raw_data_dir, filename)) # Read data from file
        prep_data = make_readable(raw_data) # Make student answers readable
        df, cols = create_df(prep_data, filename) # Create and fill dataframe with student data
        df = remove_subjects(df, good_subjects) # Remove entries where the subject code is unknown
        #df = df[:10] # To try things out

        if language == "EN":
            df = prep_en(df, filename) # Tokenize, POS tag, and lemmatize
        elif language == "NL":
            df = prep_nl(df, filename) # Tokenize, POS tag, and lemmatize

        # Create untrucated LCA input files
        create_lca_input(data_dir, df, filename)
        
        # Create LCA input files that are cut off at a certain number of words
        truncate_indiv_files(filename, data_dir, language, lca_min_sam)

        # Run the LCA and write to file
        run_lca(filename, data_dir, results_dir, language, lca_min_sam)


### --------
### RUN CODE
### --------
        
if __name__ == "__main__":
    main()