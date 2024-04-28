#///////////////////////////////////////////////////////////////////////////////
# File name:        setup.py
# Author:           Miguel Vázquez Vázquez
# Creation date:    20 November 2023
# Description:      This file installs packages, defines paths, loads functions
#///////////////////////////////////////////////////////////////////////////////
#----                       1 - PACKAGES AND OPTIONS                        ----
#///////////////////////////////////////////////////////////////////////////////

# Load all needed libraries
import sys
import os
import glob
import importlib
import datetime
import math
import random
import scipy
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import networkx as nx
import networkit as nk
import igraph as ig
import graphblas_algorithms as ga
from tqdm import tqdm

# Store date to save files
sysdate = datetime.datetime.now().strftime("%Y-%m-%d")

#///////////////////////////////////////////////////////////////////////////////
#----                               2 - PATHS                               ----
#///////////////////////////////////////////////////////////////////////////////

# Add default pip install location to path
if '/home/mivazq/.local/bin' not in sys.path:
    sys.path.append('/home/mivazq/.local/bin')

# Main paths for Ecuador project
ecuRaw  = '/home/mivazq/data/transactions_ecuador/1_rawdata/'
ecuAll  = '/home/mivazq/data/transactions_ecuador/2_shared/'
ecuFCT  = ecuAll + 'factorContentTrade/'
ecuMine = '/home/mivazq/data/transactions_ecuador/3_mivazq/'

# Set working directory and add to path
pathWD = ecuMine + 'Masters_Thesis/'
os.chdir(pathWD)
if pathWD not in sys.path:
    sys.path.append(pathWD)

# Project sub-folders
pathCle = pathWD + 'cleaning/'
pathEst = pathWD + 'estimation/'
pathFun = pathWD + 'functions/'

# Output files
pathFig = pathWD + 'results/figures/'
pathTab = pathWD + 'results/tables/'
pathItx = pathWD + 'results/intext/'

#///////////////////////////////////////////////////////////////////////////////
#----                           3 - FUNCTIONS                               ----
#///////////////////////////////////////////////////////////////////////////////

# First, add functions directory to path
if pathFun not in sys.path:
    sys.path.append(pathFun)

# If functions folder is not empty, source all Python functions in the folder
py_files = []
for file in glob.glob(pathFun+"*.py"):
    py_files.append(file)
py_files = [file.split(".")[0] for file in py_files] # remove the extension  
py_files = [file.split("/")[-1] for file in py_files] # remove the path

if len(py_files) > 0:
    for file in py_files:
        globals()[file] = importlib.import_module(file)
