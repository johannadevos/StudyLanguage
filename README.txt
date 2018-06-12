--- preprocessing.py ---

This is a command line script which does the following:

- Reading the raw files in the data/raw_data directory
- Outputting one file per subject in the data/indiv_files directory. This file contains the subject's answer in the format lemma_POStag lemma_POStag etc. The data directory is untracked by Git.
- Reading the files from indiv_files and calculating their lexical complexity, by calling lca.py, which is a revised version of the original folder-lc.py created by Xiaofei Lu (2013). The lexical complexity is written to one results file per exam, in the results directory.

The script should be called from the command line in the following format: 
python preprocessing.py {language} {minimal sample size for LCA}

For example:
python3 preprocessing.py EN 50

The code can also be run from within Spyder, as long as the command line options have been entered in Run>Configuration per file.

Preprocessing of the Dutch data requires Frog, which is part of the LaMachine NLP software distribution.
