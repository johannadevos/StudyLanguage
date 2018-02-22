# -*- coding: utf-8 -*-

# Script written by Johanna de Vos, U908153
# In the course Text and Multimedia Mining, Radboud University
# Automatic grading of open exam questions

# Import modules
import gensim
import matplotlib.pyplot as plt
import nltk
import numpy as np
import os
import pandas as pd
import random
import re
import sklearn.model_selection

from gensim import corpora, models
from matplotlib.pyplot import show, close
from nltk.tokenize import RegexpTokenizer
from nltk.stem import WordNetLemmatizer
from nltk.corpus import stopwords
from nltk.tokenize import sent_tokenize
from scipy.stats import pearsonr
from scipy.stats import spearmanr
from scipy import spatial
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import TfidfVectorizer
from statistics import mode


# Set seed for reproducability of results
random.seed(2017)

# Set NLTK directory
#nltk.data.path.append("U:/nltk_data")

# Set working directory
#os.chdir("C:/Users/johan/Documents/GitHub/AutomaticGrading")
os.chdir("C:/Users/johan/Dropbox/PhD/Courses/Text and multimedia mining/Project/Python")
#os.chdir("D:/Dropbox2/Dropbox/PhD/Courses/Text and multimedia mining/Project/Python")

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


# Add reference answer to dataframe
def add_ref(ref_answer, cols):
    print("Adding reference answer...")
    ref = pd.Series(["Ref","Ref","Ref",ref_answer_raw,"","",""], index = cols)
    df_ref = df.append(ref, ignore_index = True)
    return df_ref   


# Create dataframe for the textbook
def create_df_book(text):
    print("Creating data frame for the textbook...")
    
    # Create dataframe
    df = pd.DataFrame({'Answer': text})
    
    # Add empty columns that can later contain tokenized and lemmatized data
    df['Tokenized'] = ""
    df['Lemmatized'] = ""
    df['NoStops'] = "" 
    
    # Change order of columns
    cols = ['Answer', 'Tokenized', 'Lemmatized', 'NoStops']
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


# Create training, validation and test set
def split(df):
    print("Creating a training and test set...")
    
    ref = df[-1:]
    train, test = train_test_split(df[:-1], test_size = 0.2, random_state = 2017) # Split 80/20, pseudo-random number for reproducability
    return ref, train, test


# Explore the data
def histogram(train):
    grades = train['Grade']
    
    # Draw histogram
    plt.rcParams["patch.force_edgecolor"] = True
    fig = plt.figure()
    ax = fig.add_subplot(1,1,1)
    ax.hist(grades, bins=np.arange(-0.5, 11.5, 1))
    ax.set_xlabel('Grade')
    ax.set_ylabel('Counts')
    ax.set_xticks(range(0,11,1))
    ax.set_title('Histogram of grades')
    show(fig)
    
    
### ----------------
### BASELINE MODELS
### ----------------

# Get cosine similarity between student answers and reference answer based on raw counts
def sim_baseline(train, ref, counting):
    
    # Get raw counts
    raw_counts_train = [dictio.doc2bow(text) for text in train['NoStops']]
    raw_counts_ref = [dictio.doc2bow(text) for text in ref['NoStops']][0]
    
    # Binary scores
    if counting == "raw":
        counts_train = raw_counts_train
        counts_ref = raw_counts_ref
        
    elif counting == "binary":
        bin_counts_train = [] # Create empty list to contain the tuples with binary counts
        for student in raw_counts_train: # Loop through tuples for each student
            bin_counts_train_stud = []
            for tup in student:
                new_tup = tup[0], 1 # Replace a count of any number greater than zero by 1
                bin_counts_train_stud.append(new_tup)
            bin_counts_train.append(bin_counts_train_stud)
        
        bin_counts_ref = []
        for tup in raw_counts_ref:
            new_tup = tup[0], 1
            bin_counts_ref.append(new_tup)
        
        counts_train = bin_counts_train
        counts_ref = bin_counts_ref
    
    # Which words are in the reference answer?
    ref_words = [x[0] for x in counts_ref]
    
    # Create empty list to store the similarity scores
    sim_scores = []
    
    # Calculate the similarity score between each answer and the reference answer
    for stud_answer in counts_train:
        if stud_answer: # If the answer is not empty          
            ref_answer = counts_ref[:] # Copy reference answer
            stud_words = []
            stud_words = [x[0] for x in stud_answer] # Get word IDs
            
            # In the student answer counts, add a zero for each word in the reference answer that did not occur
            for word in ref_words:
                if word not in stud_words:
                    stud_answer.append((word, 0))
            
            # In the reference answer counts, add a zero for each word in the student answer that did not occur
            for word in stud_words:
                if word not in ref_words:
                    ref_answer.append((word, 0))
          
            # Sort by word ID
            ref_answer = sorted(ref_answer)        
            stud_answer = sorted(stud_answer)
        
            # Extract counts
            ref_counts = [x[1] for x in ref_answer]
            stud_counts = [x[1] for x in stud_answer]
    
            # Calculate cosine similarity
            sim = 1 - spatial.distance.cosine(ref_counts, stud_counts)
            sim_scores.append(sim)
        
        else: # If the answer is empty, the similarity score is zero
            sim_scores.append(0) 

    return sim_scores


# Calculate document similarities based on TF-IDF
def sim_baseline_tfidf(train, ref):
    print("\nCalculating similarity scores for the baseline TF-IDF model...")
    
    tfidf_train, tfidf_ref = tfidf(train, ref, test = None, book = None)
    
    sim_scores = []
    
    for row_index in range(tfidf_train.shape[0]):
        if tfidf_train[row_index].getnnz() > 0: # If the answer contains text
            sim = 1 - spatial.distance.cosine(tfidf_train[row_index].toarray(), tfidf_ref.toarray()) 
            sim_scores.append(sim) 
        else:
            sim_scores.append(0) 
            
    return sim_scores


# Get grades predicted by baseline and evaluate
def apply_baseline(scores_baseline, df_stud, ref, counting, mapping):
    
        if mapping == "times_ten":
            pred_grades = sim_times_ten(scores_baseline)
        elif mapping == "round_off":
            pred_grades = round_sim_to_ten(scores_baseline)
            
        real_grades = list(df_stud["Grade"])
        pearson, sig_pearson, spearman, sig_spearman, av_diff, sse = evaluate(pred_grades, real_grades, "baseline", counting)
        
        return pearson, sig_pearson, spearman, sig_spearman
        
        
# What happens when we assign the most prevalent grade (10) to all student answers?
def baseline_most_common(train):
    grades = train['Grade']
    
    real_grades = list(grades)
    pred_grades = [mode(grades)]*len(real_grades) # Find the most common grade and copy it len(real_grades) times
        
    # A correlation cannot be obtained, because x and y don't vary together
    
    # Average difference between real and predicted grades
    av_real_grades = round(np.average(real_grades), 2)
    av_pred_grades = round(np.average(pred_grades), 2)
    av_diff = round(av_real_grades - av_pred_grades, 2)
    print("The average score of the real grades is:", av_real_grades)
    print("The grade predicted for everyone is:", av_pred_grades)
    print("The difference is:", av_diff)
    
    # Sum of squared errors        
    squared_errors = [(pred_grades[x] - real_grades[x])**2 for x in range(len(pred_grades))]
    sse = sum(squared_errors)
    
    print("\nThe sum of the squared errors is:", sse) 


### -------------
### TOPIC MODELS
### -------------

# Generate LDA model
def lda(dictio, dtm_train):
    print("Training the LDA model...\n")
    ldamod = models.ldamodel.LdaModel(dtm_train, id2word = dictio, num_topics=2, passes = 20, chunksize = 1)
    print("This is the LDA model:\n")
    print(ldamod.print_topics(num_topics=5, num_words=5))
    
    return ldamod


# Generate LSA model
def lsa(dictio, dtm_train):
    print("Training the LSA model...\n")
    
    lsamod = models.LsiModel(dtm_train, id2word=dictio, num_topics=100, distributed=False, chunksize = 1) 
    print("This is the LSA model:\n")
    print(lsamod.print_topics(num_topics=5, num_words=5))
    
    return lsamod


# Calculate document similarities with a topic model
def sim_topic_mod(model, dtm_train, dtm_ref):
    print("\nCalculating similarity scores...\n")
    
    sim_scores = []
    
    for answer in dtm_train:
        if len(answer) > 0: # If the answer contains text
            sim = gensim.matutils.cossim(model[dtm_ref], model[answer])
            sim_scores.append(sim) 
        else:
            sim_scores.append(0)
        
    return sim_scores


# For development: training a topic model on the student data with k-fold cross-validation
def topic_mod_students_cross_val(train, ref, dictio, topic_mod="LSA", counting="raw", mapping="times_ten"):
    
    # Get document-term matrices
    counts_train, counts_ref = dtm(train, ref, None, None, counting, "student answers")
    
    # Stratified k-fold cross-validation (stratificiation is based on the real grades)
    k = 10
    skf = sklearn.model_selection.StratifiedKFold(n_splits=k) 
    
    # Get index of Grade column in dataframe
    index_Grade = train.columns.get_loc("Grade")
    
    # Create empty list to store the correlations of the k folds
    all_pearson = []
    all_sig_pearson = []
    all_spearman = []
    all_sig_spearman = []
    all_av_diff = []
    all_sse = []
     
    # Start k-fold cross validation
    for fold_id, (train_indices, val_indices) in enumerate(skf.split(train, train.Grade)):
        
        # Print the fold number
        print("\nFold %d" % (fold_id + 1), "\n")
         
        # Extract validation grades
        real_grades_val = [train.iloc[x, index_Grade] for x in val_indices]
        
        # Extract training and validation samples from the document-term matrix
        counts_train_fold = [counts_train[index] for index in train_indices]
        counts_val_fold = [counts_train[index] for index in val_indices]
        
        # Train topic models and calculate similarity scores between validation answers and reference answer
        if topic_mod == "LDA":
            model = lda(dictio, counts_train_fold) 
        elif topic_mod == "LSA":
            model = lsa(dictio, counts_train_fold)
    
        # Calculate similarity scores
        sim_scores = sim_topic_mod(model, counts_val_fold, counts_ref)
             
        # Transform similarity scores into grades
        if mapping == "times_ten":
            pred_grades = sim_times_ten(sim_scores)
        elif mapping == "round_off":
            pred_grades = round_sim_to_ten(sim_scores)
            
        # Evaluate
        pearson, sig_pearson, spearman, sig_spearman, av_diff, sse = evaluate(pred_grades, real_grades_val, topic_mod, counting)
        all_pearson.append(pearson)
        all_sig_pearson.append(sig_pearson)
        all_spearman.append(spearman)
        all_sig_spearman.append(sig_spearman)
        all_av_diff.append(av_diff)
        all_sse.append(sse)
          
    # Average correlation over 10 folds
    av_pearson = round(sum(all_pearson) / len(all_pearson), 2)
    av_sig_pearson = sum(all_sig_pearson) / len(all_sig_pearson)
    av_spearman = round(sum(all_spearman) / len(all_spearman), 2)
    av_sig_spearman = sum(all_sig_spearman) / len(all_sig_spearman)
    av_diff = round(sum(all_av_diff) / len(all_av_diff), 2)
    av_sse = round(sum(all_sse) / len(all_sse), 2)
    print("\nThe average Pearson correlation over", k, "folds is:", av_pearson)
    print("The average Spearman correlation over", k, "folds is:", av_spearman)
    print("The average difference between the real and predicted grades is:", av_diff)
    print("The average SSE is:", av_sse, "\n")
    
    return av_pearson, av_sig_pearson, av_spearman, av_sig_spearman
    
    
# For testing: training a topic model on all student data
def topic_mod_students(df, dictio, topic_mod="LSA", counting="TF-IDF"):
    
    ref, train, test = split(df)
    
    # Get document-term matrices
    counts_train, counts_ref, counts_test = dtm(train, ref, test, None, counting, "student answers")
    
    # Train topic models
    if topic_mod == "LDA":
        topic_mod_students = lda(dictio, counts_train) 
    elif topic_mod == "LSA":
        topic_mod_students = lsa(dictio, counts_train)

    return topic_mod_students, counts_test, counts_ref


# Training a topic model on a psychology text book, and using this model to predict grades on the training set
def topic_mod_book(df_book, train, ref, topic_mod="LSA", counting="raw", mapping = "times_ten"):
    
    dictio_book = dictionary(df_book) # Create dictionary of vocabulary
    counts_train, counts_ref, counts_book = dtm(train, ref, None, df_book, counting, training_data = "book")
    
    # Generate topic model and calculate similarity scores                  
    if topic_mod == "LDA":        
        model_book = lda(dictio_book, counts_book)
    elif topic_mod == "LSA":
        model_book = lsa(dictio_book, counts_book)
    
    pearson, sig_pearson, spearman, sig_spearman = apply_topic_mod(model_book, train, counts_train, counts_ref, counting, mapping)
    
    return model_book, pearson, sig_pearson, spearman, sig_spearman


def apply_topic_mod(topic_mod, df_stud, counts_stud, counts_ref, counting, mapping):
    
    # Get similarity scores of training answers to reference answer
    sim_scores = sim_topic_mod(topic_mod, counts_stud, counts_ref)
    #sim_scores = sim_topic_mod(model_book, counts_train, counts_ref)
    
    # Transform similarity scores into grades
    if mapping == "times_ten":
        pred_grades = sim_times_ten(sim_scores)
    elif mapping == "round_off":
        pred_grades = round_sim_to_ten(sim_scores)
    
    # Get assigned grades
    real_grades = list(df_stud["Grade"])
    
    # Get correlation between predicted grades (validation set) and lecturer-assigned grades 
    pearson, sig_pearson, spearman, sig_spearman, av_diff, sse = evaluate(pred_grades, real_grades, topic_mod, counting)
    
    return pearson, sig_pearson, spearman, sig_spearman


### ------------------------------
### FUNCTIONS NEEDED BY ALL MODELS
### ------------------------------

# Get document-term matrices
def dtm(train, ref, test, df_book, counting, training_data):
    
    # Determine which dictionary to use, based on the data that are used
    if training_data == "student answers":
    
        # Raw counts
        raw_counts_train = [dictio.doc2bow(text) for text in train['NoStops']]
        raw_counts_ref = [dictio.doc2bow(text) for text in ref['NoStops']][0]
        
        if test is not None:
            raw_counts_test = [dictio.doc2bow(text) for text in test['NoStops']]
        
    elif training_data == "book":
        
        # Get a list of all sentences in the book
        book_sents = list(df_book['NoStops'])
        
        # Raw counts
        
        # Get the document-term matrix for the book       
        dictio_book = dictionary(df_book) # Create dictionary of vocabulary
        raw_counts_book = [dictio_book.doc2bow(text) for text in book_sents] 
                              
        # Get document-term matrices for the student answers
        raw_counts_train = [dictio_book.doc2bow(text) for text in train['NoStops']]
        raw_counts_ref = [dictio_book.doc2bow(text) for text in ref['NoStops']][0]    
        
        if test is not None:
            raw_counts_test = [dictio_book.doc2bow(text) for text in test['NoStops']]
            
    # Get the counts, depending on the counting method
    
    # Raw counts
    if counting == "raw":
        counts_train = raw_counts_train
        counts_ref = raw_counts_ref
        
        if test is not None:
            counts_test = raw_counts_test
        
        if training_data == "book":
            counts_book = raw_counts_book
    
    # Binary scores
    elif counting == "binary":
        counts_train = [] # Create empty list to contain the tuples with binary counts
        
        for student in raw_counts_train: # Loop through tuples for each student
            counts_train_stud = []
            for tup in student:
                new_tup = tup[0], 1 # Replace a count of any number greater than zero by 1
                counts_train_stud.append(new_tup)
            counts_train.append(counts_train_stud)
        
        counts_ref = []
        for tup in raw_counts_ref:
            new_tup = tup[0], 1
            counts_ref.append(new_tup)
        
        # In case test data are used
        if test is not None:
           counts_test = [] # Create empty list to contain the tuples with binary counts
        
           for student in raw_counts_test: # Loop through tuples for each student
                counts_test_stud = []
                for tup in student:
                    new_tup = tup[0], 1 # Replace a count of any number greater than zero by 1
                    counts_test_stud.append(new_tup)
                counts_test.append(counts_test_stud) 
            
        # If data from the psychology book are used
        if training_data == "book":
            counts_book = [] # Create empty list to contain the tuples with binary counts
            for sentence in raw_counts_book: # Loop through tuples for each student
                counts_book_sent = []
                for tup in sentence:
                    new_tup = tup[0], 1 # Replace a count of any number greater than zero by 1
                    counts_book_sent.append(new_tup)
                counts_book.append(counts_book_sent)
                  
    # TF-IDF
    elif counting == "TF-IDF":
        
        if training_data == "student answers":
            if test is None:
                tfidf_train, tfidf_ref = tfidf(train, ref, test = None, book = None)
            elif test is not None:
                tfidf_train, tfidf_ref, tfidf_test = tfidf(train, ref, test, book = None)
                
        elif training_data == "book":
             tfidf_train, tfidf_ref, tfidf_book = tfidf(train, ref, test = None, book = df_book)
        
        counts_train = []
        for row in tfidf_train:
            t = zip(row.indices, row.data)
            counts_train.append(list(t))
            
        counts_ref = list(zip(tfidf_ref.indices, tfidf_ref.data))
        
        if test is not None:
            counts_test = []
            for row in tfidf_test:
                t = zip(row.indices, row.data)
                counts_test.append(list(t))
            
        if training_data == "book":
            counts_book = []       
            for row in tfidf_book:
                s = zip(row.indices, row.data)
                counts_book.append(list(s))

    if training_data == "student answers":
        if test is None:
            return counts_train, counts_ref
        elif test is not None:
            return counts_train, counts_ref, counts_test    
    elif training_data == "book":
        if test is None:
            return counts_train, counts_ref, counts_book
        elif test is not None:
            return counts_train, counts_ref, counts_test
 

# Calculate the document-term matrix based on TF-IDF
def tfidf(train, ref, test, book):
    
    # Temporarily merge 'train' and 'ref' into one dataframe
    df_tfidf = ref.append(train)  
    
    if test is not None:
        df_tfidf = df_tfidf.append(test)
        
    if book is not None:
        df_tfidf = df_tfidf.append(book)

    # Get document-term matrix (TF-IDF)    
    tfidf_mod = TfidfVectorizer(analyzer='word', min_df = 0) # Set up model
    strings = [" ".join(word) for word in df_tfidf['NoStops']] # Transform answers from a list of words to a string
    tfidf = tfidf_mod.fit_transform(strings) # Get TF-IDF matrix   
                                
    tfidf_ref = tfidf[0]
    tfidf_train = tfidf[1:1+len(train)]
    
    if test is None and book is None:
        return tfidf_train, tfidf_ref    
    
    if test is not None and book is None:
        tfidf_test = tfidf[1+len(train):]
        return tfidf_train, tfidf_ref, tfidf_test
    
    if test is None and book is not None:
        tfidf_book = tfidf[-len(book):]
        return tfidf_train, tfidf_ref, tfidf_book
    
    elif test is not None and book is not None:
        tfidf_test = tfidf[1+len(train):1+len(train)+len(test)]
        tfidf_book = tfidf[-len(book):]
        return tfidf_train, tfidf_ref, tfidf_test, tfidf_book
               
    '''
    # Some methods that are good to know
    print(tfidf_train)
    tfidf_train.A # Shows array, same as tfidf_train.toarray()
    tfidf_train.indices # 
    tfidf_train.data
    tfidf_train.indptr
    '''

# Try out different algorithms for transforming similarity scores into grades
def sim_times_ten(sim_scores):
    print("Transforming similarity scores into grades...\n")
    
    # Multiply the similarity scores by 10
    pred_grades = [round(sim*10) for sim in sim_scores] 

    return pred_grades


def round_sim_to_ten(sim_scores):
                    
    # Multiply the similarity scores by 10
    pred_grades = [round(sim*10) for sim in sim_scores] 
                 
    # Round off all 9's to 10, and all 1's to 0
    pred_grades2 = []
    
    for grade in pred_grades:
        if grade == 9:
            grade = 10
        elif grade == 1:
            grade = 0
        pred_grades2.append(grade)
       
    return pred_grades2


# Get evaluation measures: correlation, averages, SSE
def evaluate(pred_grades, real_grades, model, counting):
    pearson, sig_pearson = pearsonr(pred_grades, real_grades)
    pearson = round(pearson, 2)
    spearman, sig_spearman = spearmanr(pred_grades, real_grades)
    spearman = round(spearman, 2)
    print("For the", model, "model with", counting, "counting, Pearson's r between the predicted and real grades is:", pearson, "\n")
    print("For the", model, "model with", counting, "counting, Spearman's r between the predicted and real grades is:", spearman, "\n")    
      
    # Plot the predicted grades and lecturer-assigned grades
    df_grades = pd.DataFrame({'Predicted': pred_grades, 'Assigned': real_grades})
    df_counts = df_grades.groupby(['Predicted', 'Assigned']).size().reset_index(name='Count')
    df_counts.plot(kind='scatter', x='Predicted', y='Assigned', title='Scatterplot of predicted and assigned grades', s=df_counts['Count'], xlim=[-0.3,10.3])
    show()
    plt.close()
    
    # Average scores
    av_real_grades = np.average(real_grades)
    av_pred_grades = np.average(pred_grades)
    av_diff = av_real_grades - av_pred_grades
    print("The average score of the real grades is:", round(av_real_grades, 2))
    print("The average score of the predicted grades is:", round(av_pred_grades, 2))
    print("The difference is:", round(av_diff, 2))
    
    # Sum of squared errors        
    squared_errors = [(pred_grades[x] - real_grades[x])**2 for x in range(len(pred_grades))]
    sse = round(sum(squared_errors), 2)
    
    print("\nThe sum of the squared errors is:", sse) 
        
    return pearson, sig_pearson, spearman, sig_spearman, av_diff, sse


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
    ref_answer_raw = open_file('referenceAnswer.txt') # Read reference answer
    ref_answer = preprocess(ref_answer_raw) # Preprocess reference answer
    stud_answers_raw = open_file('studentAnswersCorrected.txt') # Read student answers
    spelling_correction = "Yes"
    #stud_answers_raw = open_file('studentAnswersUncorrected.txt') # Read student answers
    #spelling_correction = "No"
    stud_answers = preprocess(stud_answers_raw) # Preprocess student answers
    df, cols = create_df(stud_answers) # Create dataframe of student answers
    df = add_ref(ref_answer, cols) # Add reference answer to dataframe
    df = tok_lem(df) # Tokenize and lemmatize all answers, and remove stop words
    dictio = dictionary(df) # Create dictionary of vocabulary
    ref, train, test = split(df) # Split the data into a 80/20 (train/test)
    histogram(train) # Explore the distribution of the grades
    
    # Read and prepare Psychology book
    book_raw = open_file('psyBookChapter10.txt') # Open book
    book_raw = book_raw[:5000] # To try new things out without having to work with a big dataset
    book = book_raw.replace("\n", " ") # Remove white lines
    book = book.replace(chr(0x00AD), "") # Remove soft hyphens
    book = preprocess(book) # Preprocess book
    sent_book = sent_tokenize(book) # Split into sentences
    df_book, cols_book = create_df_book(sent_book) # Create dataframe 
    df_book = tok_lem(df_book) # Tokenize, lemmatize, remove stop words
    
    ### Running models one by one to try things out, don't write to file
    
    # Training
    print("\nRunning on the training data\n")
    
    # Baseline models
    scores_baseline = sim_baseline(train, ref, counting="raw")
    apply_baseline(scores_baseline, train, ref, counting="raw", mapping = "times_ten")
    
    scores_baseline = sim_baseline(train, ref, counting="binary")
    apply_baseline(scores_baseline, train, ref, counting="binary", mapping = "times_ten")
 
    scores_baseline = sim_baseline_tfidf(train, ref)
    apply_baseline(scores_baseline, train, ref, counting="TF-IDF", mapping = "times_ten")
    
    # Topic models: train on student answers
    topic_mod_students_cross_val(train, ref, dictio, topic_mod="LDA", counting="raw")
    topic_mod_students_cross_val(train, ref, dictio, topic_mod="LDA", counting="binary")
    topic_mod_students_cross_val(train, ref, dictio, topic_mod="LSA", counting="raw")
    topic_mod_students_cross_val(train, ref, dictio, topic_mod="LSA", counting="binary")
    topic_mod_students_cross_val(train, ref, dictio, topic_mod="LSA", counting="TF-IDF")
    
    # Topic models: train on Psychology book
    LDA_book_raw = topic_mod_book(df_book, train, ref, topic_mod="LDA", counting="raw") 
    LDA_book_binary = topic_mod_book(df_book, train, ref, topic_mod="LDA", counting="binary") 
    LSA_book_raw = topic_mod_book(df_book, train, ref, topic_mod="LSA", counting="raw") 
    LSA_book_binary = topic_mod_book(df_book, train, ref, topic_mod="LSA", counting="binary") 
    #LSA_book_tfidf = topic_mod_book(df_book, train, ref, topic_mod="LSA", counting="TF-IDF") # NOT WORKING, needs bug fixing
    
    # Testing
    print("\nRunning on the test data\n")
        
    # Baseline models
    scores_baseline = sim_baseline(test, ref, counting="raw")
    apply_baseline(scores_baseline, test, ref, counting="raw", mapping = "times_ten")
    
    scores_baseline = sim_baseline(test, ref, counting="binary")
    apply_baseline(scores_baseline, test, ref, counting="binary", mapping = "times_ten")
 
    scores_baseline = sim_baseline_tfidf(test, ref)
    apply_baseline(scores_baseline, test, ref, counting="TF-IDF", mapping = "times_ten")
        
    ### Running all models in a loop, write to file
      
    # Training
    print("\nRunning on the training data\n")
    
    # Create output file
    outfile = setup_outfile()
    
    # List models, counting methods, and mapping algorithms (spelling correction needs to be varied manually when reading the data)
    topic_mods = ["LDA", "LSA"]
    counting = ["raw", "binary", "TF-IDF"]
    mapping = ["times_ten", "round_off"]    
    
    # Baseline models    
    for count_method in counting:
        for mapp in mapping:
            
            if not count_method == "TF-IDF":
                scores_baseline = sim_baseline(train, ref, counting=count_method)
            elif count_method == "TF-IDF":
                scores_baseline = sim_baseline_tfidf(train, ref)
            
            pearson, sig_pearson, spearman, sig_spearman = apply_baseline(scores_baseline, train, ref, counting=count_method, mapping=mapp)
            save_to_file(["baseline", count_method, "train", "student_answers", mapp, spelling_correction, str(pearson), str(sig_pearson), str(spearman), str(sig_spearman)], outfile)  
        
    # Topic models: train on student answers     
    for model in topic_mods:
        for count_method in counting:
            if not (model == "LDA" and count_method == "TF-IDF"):
                for mapp in mapping:
               
                    pearson, sig_pearson, spearman, sig_spearman = topic_mod_students_cross_val(train, ref, dictio, topic_mod=model, counting=count_method, mapping=mapp)
                    save_to_file([model, count_method, "train", "student_answers", mapp, spelling_correction, str(pearson), str(sig_pearson), str(spearman), str(sig_spearman)], outfile)  
    
    # Topic models: train on psychology book
    trained_models = []

    for model in topic_mods:
        for count_method in counting:
            if not count_method == "TF-IDF": # Bug fix still needed. If done, then remove this line and use the next.
            #if not (model == "LDA" and count_method == "TF-IDF"):
                for mapp in mapping:
                    
                    trained_model, pearson, sig_pearson, spearman, sig_spearman = topic_mod_book(df_book, train, ref, topic_mod=model, counting=count_method, mapping=mapp)
                    trained_models.append(trained_model)
                    save_to_file([model, count_method, "train", "textbook", mapp, spelling_correction, str(pearson), str(sig_pearson), str(spearman), str(sig_spearman)], outfile)  
     
    # Testing
    print("\nRunning on the test data\n")
    
    # Baseline models
    for count_method in counting:
        for mapp in mapping:
            if not count_method == "TF-IDF":
                scores_baseline = sim_baseline(test, ref, counting=count_method)
            elif count_method == "TF-IDF":
                scores_baseline = sim_baseline_tfidf(test, ref)
            
            pearson, sig_pearson, spearman, sig_spearman = apply_baseline(scores_baseline, test, ref, counting=count_method, mapping=mapp)
            save_to_file(["baseline", count_method, "test", "none", mapp, spelling_correction, str(pearson), str(sig_pearson), str(spearman), str(sig_spearman)], outfile) 
           
    # Assigning the most common grade to everyone
    baseline_most_common(test)   
    
    # Topic models: train on student data (again, because now we're using all data rather than the training subset of k-fold cross-validation)
    for model in topic_mods:
        for count_method in counting:
            if not (model == "LDA" and count_method == "TF-IDF"):
                for mapp in mapping:
               
                    topic_mod, counts_test, counts_ref = topic_mod_students(df, dictio, topic_mod=model, counting=count_method)
                    pearson, sig_pearson, spearman, sig_spearman = apply_topic_mod(topic_mod, test, counts_test, counts_ref, count_method, mapping=mapp)
                    
                    # Save to file
                    save_to_file([model, count_method, "test", "student_answers", mapp, spelling_correction, str(pearson), str(sig_pearson), str(spearman), str(sig_spearman)], outfile)
    
    # Topic models: already trained on Psychology book
    counter = 0
  
    for model in topic_mods:
        for count_method in counting:
            if not count_method == "TF-IDF": # Bug fix still needed. If done, then remove this line and use the next.
            #if not (model == "LDA" and count_method == "TF-IDF"):
                for mapp in mapping:
                    
                    trained_model = trained_models[counter]
                    counts_train, counts_ref, counts_test = dtm(train, ref, test, df_book, counting=count_method, training_data="book")
                    pearson, sig_pearson, spearman, sig_spearman = apply_topic_mod(trained_model, test, counts_test, counts_ref, counting=count_method, mapping=mapp)
    
                    counter += 1

                    save_to_file([model, count_method, "test", "textbook", mapp, spelling_correction, str(pearson), str(sig_pearson), str(spearman), str(sig_spearman)], outfile)