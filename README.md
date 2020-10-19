# sequed
DNA sequence editor and alignment viewer for emacs. Provides a mode for editing sequence data in fasta format. Also, an alignment viewer with base coloring and a search feature to locate a particular base. Because of limits on the size of buffer emacs easily handles I would not recommend using this mode to view alignments larger than 10kb. There is also an 
option to export an alignment in a simple format used by the phylogenetics program BPP. Once installed, sequed-mode automatically becomes the major mode for files with .fa or .aln endings. You can manually invoke the sequed-mode major mode with 
```
M-x sequed-mode 
```
In sequed-mode use ```C-c C-e``` or ```M-x sequed-export``` to export a multiple alignment in BA3 format to a new created buffer that can then be saved as a file. Use ```C-c C-a``` or ```M-x sequed-mkaln``` to create a new read only buffer with a colorful alignment view with major mode ```sequed-aln-mode```. In sequed-aln-mode  use ```C-c C-f``` or ```M-x sequed-aln-seqfeatures``` to display the number of sequences and number of sites below the mode line. the command ```C-c C-b``` or ```M-x sequed-aln-gotobase``` will prompt you for a base number (column in the alignment) to move the cursor position to.
