# -*- coding: utf-8 -*-

# Copyright: Johanna de Vos (2018)

# Standard library imports
import os
from pathlib import Path


### ------------------
### DIRECTORIES
### ------------------

# Set working directory to where the data are
def get_current_file_dir() -> Path:
    """Returns the directory of the script."""
    try:
        return Path(os.path.realpath(__file__)).parent
    except(NameError):
        return Path(os.getcwd())

# Define directories
root_dir = get_current_file_dir().parent
data_dir = root_dir / 'data'
results_dir = data_dir / 'lca_results'

with open (results_dir / 'lca_AIP_A_EN_corrected.txt', 'r') as aip:
    print(aip.read())


### --------
### RUN CODE
### --------

#if __name__ == "__main__":
    
    