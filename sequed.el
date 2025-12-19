;;; sequed.el --- Major mode for FASTA format DNA alignments -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025 Bruce Rannala

;; Author: Bruce Rannala <brannala@ucdavis.edu>
;; URL: https://github.com/brannala/sequed
;; Package-Version: 20251219.100
;; Package-Requires: ((emacs "25.2"))
;; License: GNU General Public License Version 3

;;; Commentary:

;; Major mode for editing DNA sequence data in FASTA format and viewing
;; multiple sequence alignments.
;;
;; Usage:
;;
;; M-x sequed-mode to invoke.  Automatically invoked as major mode for .fa and .aln files.
;; Sequed-mode:
;;   C-c C-r c  sequed-reverse-complement
;;   C-c C-r t  sequed-translate
;;   C-c C-e    sequed-export
;;   C-c C-a    sequed-mkaln
;;
;; Sequed-aln-mode:
;;   C-c C-b    sequed-aln-gotobase
;;   C-c C-f    sequed-aln-seqfeatures
;;   C-c C-k    sequed-aln-kill-alignment

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(?:fa\\|aln\\)\\'" . sequed-mode))

(defface sequed-base-c '((t :background "green"))
  "Basic face for DNA base C."
  :group 'sequed)

(defface sequed-base-t '((t :background "orange"))
  "Basic face for DNA base T."
  :group 'sequed)

(defface sequed-base-a '((t :background "red"))
  "Basic face for DNA base A."
  :group 'sequed)

(defface sequed-base-g '((t :background "blue"))
  "Basic face for DNA base G."
  :group 'sequed)

(defvar sequed-aln-base-a 'sequed-base-a)
(defvar sequed-aln-base-c 'sequed-base-c)
(defvar sequed-aln-base-g 'sequed-base-g)
(defvar sequed-aln-base-t 'sequed-base-t)

(defvar sequed-aln-mode-font-lock nil
  "DNA base colors for `font-lock-defaults' in alignment view.")

(setq sequed-aln-mode-font-lock
      '(("^>[^\s]+" . font-lock-constant-face)
        ("[cC]" . sequed-aln-base-c)
        ("[tT]" . sequed-aln-base-t)
        ("[aA]" . sequed-aln-base-a)
        ("[gG]" . sequed-aln-base-g)))

(defconst sequed-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ; is a comment starter
    (modify-syntax-entry ?\; "<" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst sequed-colors
  '(("^[^>]\\([a-zA-Z- ]+\\)" . font-lock-string-face)
    ("^>.+\n" . font-lock-constant-face)))

(defvar sequed-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-a") #'sequed-mkaln)
    (define-key km (kbd "C-c C-e") #'sequed-export)
    (define-key km (kbd "C-c C-r c") #'sequed-reverse-complement)
    (define-key km (kbd "C-c C-r t") #'sequed-translate)
    (define-key km [menu-bar sequed]
      (cons "SequEd" (make-sparse-keymap "SequEd")))
    (define-key km [menu-bar sequed sequed-mkaln]
      '("View Alignment" . sequed-mkaln))
    (define-key km [menu-bar sequed sequed-export]
      '("Export" . sequed-export))
    (define-key km [menu-bar sequed sequed-reverse-complement]
      '("Reverse Complement" . sequed-reverse-complement))
    (define-key km [menu-bar sequed sequed-translate]
      '("Translation" . sequed-translate))
    km)
  "Keymap used in `sequed-mode'.")

;;;###autoload
(define-derived-mode sequed-mode fundamental-mode "SequEd"
  "Major mode for viewing and editing FASTA sequence data."
  :syntax-table sequed-mode-syntax-table
  (setq font-lock-defaults '(sequed-colors))
  (unless (sequed-check-fasta)
    (user-error "Not a FASTA file"))
  (font-lock-ensure)
  (sequed-get-sequence-length)
  (setq-local comment-start "; ")
  (setq-local comment-end ""))

(defvar sequed-aln-mode-map
  (let ((km2 (make-sparse-keymap)))
    (define-key km2 (kbd "C-c C-b") #'sequed-aln-gotobase)
    (define-key km2 (kbd "C-c C-f") #'sequed-aln-seqfeatures)
    (define-key km2 (kbd "C-c C-k") #'sequed-aln-kill-alignment)
    (define-key km2 [menu-bar sequedaln]
      (cons "SequEdAln" (make-sparse-keymap "SequEdAln")))
    (define-key km2 [menu-bar sequedaln move]
      '("Move to base" . sequed-aln-gotobase))
    (define-key km2 [menu-bar sequedaln features]
      '("Sequence features" . sequed-aln-seqfeatures))
    (define-key km2 [menu-bar sequedaln quit]
      '("Quit Alignment" . sequed-aln-kill-alignment))
    km2)
  "Keymap used in `sequed-aln-mode'.")

(define-derived-mode sequed-aln-mode fundamental-mode "SequEdAln"
  "Major mode for viewing sequence alignments."
  :syntax-table sequed-mode-syntax-table
  (setq font-lock-defaults '(sequed-aln-mode-font-lock))
  (font-lock-ensure)
  (setq mode-line-format
        (list "%e" mode-line-front-space mode-line-mule-info
              mode-line-client mode-line-modified
              mode-line-remote mode-line-frame-identification
              mode-line-buffer-identification
              "  SeqID:"
              '(:eval (aref sequed-seqID
                            (max 0 (1- (string-to-number (format-mode-line "%l"))))))
              " BasePos:"
              '(:eval (let* ((col (current-column))
                             (offset (- col (1- sequed-label-length))))
                        (when (> offset 0)
                          (number-to-string
                           (+ offset (1- sequed-startpos))))))
              "   "
              mode-line-modes mode-line-misc-info mode-line-end-spaces)))

(defun sequed-check-fasta ()
  "Check if current buffer looks like FASTA."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^>.+\n[ACGTNacgtn-]+" nil t)))

(defun sequed-remove-fasta-comments ()
  "Remove comment lines from current buffer (lines starting with `;')."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward "^;.*\n" nil t)
        (replace-match "" t t)))))

(defvar-local sequed-label-length nil)
(defvar-local sequed-seq-length nil)
(defvar-local sequed-noseqs nil)
(defvar-local sequed-seqID nil)
(defvar-local sequed-startpos nil)
(defvar-local sequed-endpos nil)

(defun sequed--parse-fasta-buffer ()
  "Parse current buffer as FASTA.

Return a list of (LABEL . SEQ) where LABEL includes the leading `>'."
  (save-excursion
    (goto-char (point-min))
    (let (result label seq-start)
      (while (re-search-forward "^>\\([^\n]*\\)\n" nil t)
        (setq label (match-string 0))
        (setq seq-start (point))
        (let ((seq-end (or (and (re-search-forward "^>" nil t)
                                (match-beginning 0))
                           (point-max))))
          (let* ((raw (buffer-substring-no-properties seq-start seq-end))
                 (seq (replace-regexp-in-string "[ \t\n\r]" "" raw)))
            (push (cons label seq) result))
          (goto-char seq-end)))
      (nreverse result))))

(defun sequed-get-sequence-length ()
  "Set `sequed-seq-length' from first FASTA sequence in buffer."
  (let* ((pairs (sequed--parse-fasta-buffer))
         (first (car pairs)))
    (setq sequed-seq-length (length (cdr first)))))

(defun sequed-labels-equal-length (labels)
  "Pad LABELS so all have equal length plus one space."
  (let* ((trimmed (mapcar #'string-trim labels))
         (maxlen (cl-loop for s in trimmed maximize (length s))))
    (cl-loop for s in trimmed
             collect (concat s
                             (make-string (1+ (- maxlen (length s))) ?\s)))))

(defun sequed-short-labels (labels)
  "Create short LABELS for display in mode line."
  (let* ((small-size (min 11 (length (nth 1 labels))))
         (len (length labels))
         (vec (make-vector len "Empty")))
    (cl-loop for i from 0 below len do
             (aset vec i (substring (nth i labels) 1 small-size)))
    vec))

(defun sequed-color-labels ()
  "Identify labels and color them."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward "^>[[:word:]\-/|_.]+" nil t)
        (add-text-properties
         (match-beginning 0) (match-end 0)
         '(face (:foreground "yellow")))))))

(defun sequed-mkaln (startpos endpos)
  "Create read-only buffer for alignment viewing from STARTPOS to ENDPOS."
  (interactive
   (let ((spos (read-number "Start Pos: " 1)))
     (sequed-get-sequence-length)
     (let ((epos (read-number "End Pos: " sequed-seq-length)))
       (list spos epos))))
  (unless (sequed-check-fasta)
    (user-error "Not a FASTA file"))
  (when (or (< startpos 1)
            (> endpos sequed-seq-length)
            (>= startpos endpos))
    (user-error "Invalid start/end positions"))
  (let* ((orig-buf (current-buffer))
         (pairs (with-current-buffer orig-buf
                  (save-excursion
                    (sequed-remove-fasta-comments)
                    (sequed--parse-fasta-buffer))))
         (labels (mapcar #'car pairs))
         (seqs   (mapcar #'cdr pairs))
         (nseqs  (length seqs))
         (elabels (sequed-labels-equal-length labels))
         (trimmed
          (cl-loop for s in seqs
                   collect (substring s (1- startpos) endpos)))
         (lines
          (cl-loop for lab in elabels
                   for s in trimmed
                   collect (concat lab s)))
         (buf (get-buffer-create "*Alignment Viewer*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sequed-aln-mode)
        (setq truncate-lines t)
        (setq sequed-label-length (length (car elabels)))
        (setq sequed-seq-length (length (car seqs)))
        (setq sequed-noseqs nseqs)
        (setq sequed-seqID (sequed-short-labels labels))
        (setq sequed-startpos startpos)
        (setq sequed-endpos endpos)
        (insert (mapconcat #'identity lines "\n"))
        (insert "\n")
        (sequed-color-labels)
        (read-only-mode 1)))
    (display-buffer buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (sequed-aln-gotobase sequed-startpos))))

(defun sequed-aln-gotobase (position)
  "Move to base at POSITION in sequence that cursor is positioned in."
  (interactive "nPosition of base: ")
  (when (or (< position sequed-startpos)
            (> position sequed-endpos))
    (user-error "Attempt to move to base outside sequence"))
  (beginning-of-line)
  (move-to-column (- (+ position (1- sequed-label-length))
                     (1- sequed-startpos))))

(defun sequed-aln-seqfeatures ()
  "List number of sequences and length of region."
  (interactive)
  (message "Sequences:%d Sites:%d" sequed-noseqs sequed-seq-length))

(defun sequed-aln-kill-alignment ()
  "Kill alignment buffer and window."
  (interactive)
  (kill-buffer-and-window))

(defun sequed-export ()
  "Export alignment in PHYLIP-like format for phylogenetic software."
  (interactive)
  (let* ((oldbuf (current-buffer))
         nseqs nsites f-lines templine f-buffer)
    (save-current-buffer
      (set-buffer (get-buffer-create "*export alignment*"))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring-no-properties oldbuf)
        (setq f-buffer (buffer-substring-no-properties (point-min) (point-max)))
        (goto-char (point-min))
        (setq f-lines
              (split-string f-buffer ">\\([[:word:]\-/|_.]+\\)\\([\s]+.*\n\\)?" t))
        (setq templine (mapconcat #'concat (split-string (nth 1 f-lines) "\n" t) ""))
        (setq-local nsites (length templine))
        (goto-char (point-min))
        (setq-local comment-start "; ")
        (setq-local comment-end "")
        (sequed-remove-fasta-comments)
        (goto-char (point-min))
        (setq nseqs (how-many ">[[:word:]\-/|_.]+"))
        (goto-char (point-min))
        (while (re-search-forward ">[[:word:]\-/|_.]+\\([^\n]*\\)\n" nil t)
          (replace-match "" t t))
        (goto-char (point-min))
        (insert (format "%d  %d\n" nseqs nsites))
        (while (re-search-forward ">" nil t)
          (delete-char 1))
        (fundamental-mode)
        (display-buffer (current-buffer))))))

(defvar sequed-genetic-code-universal (make-hash-table :test 'equal))

;; Genetic code table (unchanged)
(mapc (lambda (pair)
        (puthash (car pair) (cdr pair) sequed-genetic-code-universal))
      '(("ttt" . ?F) ("ttc" . ?F) ("tta" . ?L) ("ttg" . ?L)
        ("tct" . ?S) ("tcc" . ?S) ("tca" . ?S) ("tcg" . ?S)
        ("taa" . ?*) ("tag" . ?*) ("tat" . ?Y) ("tac" . ?Y)
        ("tgt" . ?C) ("tgc" . ?C) ("tga" . ?*) ("tgg" . ?W)
        ("ctt" . ?L) ("ctc" . ?L) ("cta" . ?L) ("ctg" . ?L)
        ("cct" . ?P) ("ccc" . ?P) ("cca" . ?P) ("ccg" . ?P)
        ("cat" . ?H) ("cac" . ?H) ("caa" . ?Q) ("cag" . ?Q)
        ("cgt" . ?R) ("cgc" . ?R) ("cga" . ?R) ("cgg" . ?R)
        ("att" . ?I) ("atc" . ?I) ("ata" . ?I) ("atg" . ?M)
        ("act" . ?T) ("acc" . ?T) ("aca" . ?T) ("acg" . ?T)
        ("aat" . ?N) ("aac" . ?N) ("aaa" . ?K) ("aag" . ?K)
        ("agt" . ?S) ("agc" . ?S) ("aga" . ?R) ("agg" . ?R)
        ("gtt" . ?V) ("gtc" . ?V) ("gta" . ?V) ("gtg" . ?V)
        ("gct" . ?A) ("gcc" . ?A) ("gca" . ?A) ("gcg" . ?A)
        ("gat" . ?D) ("gac" . ?D) ("gaa" . ?E) ("gag" . ?E)
        ("ggt" . ?G) ("ggc" . ?G) ("gga" . ?G) ("ggg" . ?G)))

(defun sequed-translate (seqbegin seqend)
  "Translation of marked DNA region SEQBEGIN SEQEND into amino acids."
  (interactive "r")
  (let* ((raw (buffer-substring-no-properties seqbegin seqend))
         (x   (replace-regexp-in-string "[ \t\n\r]" "" raw))
         (len (length x))
         (codons (/ len 3))
         (y    (make-string codons ?.))
         (i 0) (j 0))
    (while (<= (+ i 2) (1- len))
      (let ((codon (downcase (substring x i (+ i 3)))))
        (aset y j (or (gethash codon sequed-genetic-code-universal) ?.)))
      (setq i (+ i 3))
      (setq j (1+ j)))
    (let ((buf (generate-new-buffer "*translation*")))
      (with-current-buffer buf
        (insert y))
      (switch-to-buffer buf))))

(defun sequed-basepair (base)
  "Find the complement of a DNA BASE."
  (pcase base
    (?a ?t) (?t ?a) (?c ?g) (?g ?c)
    (?A ?T) (?T ?A) (?C ?G) (?G ?C)
    (_ base)))

(defun sequed-reverse-complement (seqbegin seqend)
  "Get reverse-complement of marked region SEQBEGIN SEQEND in new buffer."
  (interactive "r")
  (let* ((raw (buffer-substring-no-properties seqbegin seqend))
         (x   (replace-regexp-in-string "[ \t\n\r]" "" raw))
         (len (length x))
         (out (make-string len ?N)))
    (cl-loop for i from 0 below len
             for b = (aref x i)
             for c = (sequed-basepair b)
             do (aset out (- (1- len) i) c))
    (let ((buf (generate-new-buffer "*reverse complement*")))
      (with-current-buffer buf
        (insert out))
      (switch-to-buffer buf))))

(provide 'sequed)

;;; sequed.el ends here
