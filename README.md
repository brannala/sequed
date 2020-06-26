# sequed
DNA sequence editor and alignment viewer for emacs
Provides a mode for editing sequence data in fasta format. Also, an alignment viewer with base coloring and a search feature to locate a particular base. 
Because of limits on the size of buffer emacs easily handles I would not recommend using this mode to view alignments larger than 5kb. There is also an 
option to export an alignment in a simple format used by the phylogenetics program BPP.
If you use use-package to manage emacs packages (highly recommended) add this content:
 
 (use-package sequed
  :ensure t
  :load-path "~/.emacs.d/lisp/"
  :mode (("\\.fas\\'" . sequed-mode)
	 ("\\.fa\\'" . sequed-mode)
	 ("\\.aln\\'" . sequed-mode)))

This will cause sequed mode to be invoked in any buffer created for files with the endings .fas .fa or .aln. If you have issues let me know.
