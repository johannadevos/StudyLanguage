# Import modules
import os
import pandas as pd
from preprocessing import open_file, make_readable, create_df
from preprocessing import read_subject_info, remove_subjects

# Define directories
src_dir = os.path.dirname(os.path.abspath(__file__))
data_dir = os.path.join(src_dir, '..', 'data')
raw_data_dir = os.path.join(data_dir, 'raw_data')
results_dir = os.path.join(src_dir, '..', 'results')

# Read in subject info
subject_df = read_subject_info(data_dir)
good_subjects = list(subject_df.index)


## DUTCH DATA

# Read and process exams
exam = "AIP_A_NL_corrected.txt"
raw_data = open_file(os.path.join(raw_data_dir, exam)) # Read data from file
prep_data = make_readable(raw_data) # Make student answers readable
df, cols = create_df(prep_data, exam) # Create and fill dataframe with student data
df = remove_subjects(df, good_subjects) # Remove entries where the subject code is unknown

# Create new dataframe, using two columns from the above df
dutch_grades = df[['SubjectCode', 'Grade']]

# Continue reading and processing exams
exam = "STAT_A_NL_corrected.txt"
raw_data = open_file(os.path.join(raw_data_dir, exam))
prep_data = make_readable(raw_data)
df, cols = create_df(prep_data, exam)
df = remove_subjects(df, good_subjects)

# Merge df into dutch_grades
dutch_grades = pd.merge(dutch_grades, df[['SubjectCode', 'Grade']], on = 'SubjectCode', how = 'outer')

# Continue reading and processing exams
exam = "STAT_C_NL_corrected.txt"
raw_data = open_file(os.path.join(raw_data_dir, exam))
prep_data = make_readable(raw_data)
df, cols = create_df(prep_data, exam)
df = remove_subjects(df, good_subjects)

# Merge df into dutch_grades
dutch_grades = pd.merge(dutch_grades, df[['SubjectCode', 'Grade4a']], on = 'SubjectCode', how = 'outer')

# Rename columns
dutch_grades.columns = ['SubjectCode', 'grade_oct', 'grade_jan', 'grade_apr']


## ENGLISH DATA

# Read and process exams
exam = "AIP_A_EN_corrected.txt"
raw_data = open_file(os.path.join(raw_data_dir, exam)) # Read data from file
prep_data = make_readable(raw_data) # Make student answers readable
df, cols = create_df(prep_data, exam) # Create and fill dataframe with student data
df = remove_subjects(df, good_subjects) # Remove entries where the subject code is unknown

# Create new dataframe, using two columns from the above df
english_grades = df[['SubjectCode', 'Grade']]

# Continue reading and processing exams
exam = "STAT_A_EN_corrected.txt"
raw_data = open_file(os.path.join(raw_data_dir, exam))
prep_data = make_readable(raw_data)
df, cols = create_df(prep_data, exam)
df = remove_subjects(df, good_subjects)

# Merge df into dutch_grades
english_grades = pd.merge(english_grades, df[['SubjectCode', 'Grade']], on = 'SubjectCode', how = 'outer')

# Continue reading and processing exams
exam = "STAT_C_EN_corrected.txt"
raw_data = open_file(os.path.join(raw_data_dir, exam))
prep_data = make_readable(raw_data)
df, cols = create_df(prep_data, exam)
df = remove_subjects(df, good_subjects)

# Merge df into dutch_grades
english_grades = pd.merge(english_grades, df[['SubjectCode', 'Grade4a']], on = 'SubjectCode', how = 'outer')

# Rename columns
english_grades.columns = ['SubjectCode', 'grade_oct', 'grade_jan', 'grade_apr']


## MERGE ENGLISH AND DUTCH DATA

grades = pd.concat([dutch_grades, english_grades])
grades = grades.sort_values('SubjectCode')
grades.to_csv(os.path.join(data_dir, 'grades.txt'), index=False)