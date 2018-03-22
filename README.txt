--- preprocessing.py ---

This is a command line script which does the following:
- Reading the raw files in the data/raw_data directory
- Outputting one file per subject in the data/indiv_files directory. This file contains the subject's answer in the format lemma_POStag lemma_POStag etc. The indiv_files directory is untracked by Git.
- Reading the files from indiv_files and calculating their lexical complexity, by calling lca.py, which is a revised version of the original folder-lc.py created by Xiaofei Lu (2013). The lexical complexity is written to one results file per exam, in the directory data/lca_results.

The script should be called from the command line in the following format:
python preprocessing.py {language} {minimal sample size for LCA}

For example:
python3 preprocessing.py EN 50

As it stands, I'm using the Windows environment on my work PC to preprocess the English data, as it contains the required nltk corpora. The Dutch data can only be preprocessed from Bash on Windows (work PC), where LaMachine is installed. Before running preprocessing.py, LaMachine should be activated. On my work PC, I can do this by typing 'lm'.