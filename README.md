# State
State is a Mathematica package for deriving the equations of state for a dynamic system. It was originally developed by Joseph L. Garbini at the University of Washington.

## Installation
Clone or download the repository. 

To fully install, copy the `State.m` Mathematica package file to any directory in your Mathematica *path* (to see which directories are in your path, evaluate `$Path` in a Mathematica notebook).

Once `State` is fully installed, it can be loaded into a Mathematica notebook with the command

    <<State`

The package can also be loaded from the working directory, which can be set to the notebook's directory with the command

    SetDirectory[NotebookDirectory[]];

If `State.m` is then placed in the same directory as the notebook, it can be loaded with the same command (`<<State``) without requiring full installation.

## Getting started
There are two example notebooks that work out a few problems. Open the notebooks in Mathematica and run them. They work without fully installing the package.