;; -*- no-byte-compile: t; -*-
;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

;; Ignore "Package cl is deprecated" warning
(setq byte-compile-warnings '(cl-functions))

(doom! :input
       ;; ...
       :completion
       ;; (company +childframe) ; TODO [opt]flags the ultimate code completion backend
       (corfu +orderless +icons) ; TODO
       (vertico +icons +orderless)    ; the search engine of the future

       :ui
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs ;
       (emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides     ; highlighted indent columns
       minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;; tabs              ; a tab bar for Emacs
       (treemacs +lsp)   ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       (vc-gutter +diff-hl +pretty) ; vcs diff in the fringe
       (window-select +numbers)     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       format            ; automated prettiness +format/buffer will use lsp if available
       ;; multiple-cursors ; editing in many places at once, seems interesting https://emacsrocks.com/e13.html
       ;; objed             ; text object editing for the innocent
       ;; parinfer          ; turn lisp into python, sort of
       snippets             ; my elves. They type so I don't have to
       word-wrap            ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)    ; making dired pretty [functional]
       (ibuffer +icons)  ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes (undo-fu)
       vc                ; version-control and Emacs, sitting in a tree (gitignore, gitconfig)

       :term
       eshell
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax                          ; tasing you for every semicolon you forget
       (spell +aspell)   ; tasing you for misspelling mispelling (+everywhere disabled)
       ;;[opt]grammar                       ; tasing grammar mistake every you make

       :tools
       ;; NOTE TODOs needs to be verified with python/C++ environments
       ansible
       ;;biblio            ; Bibtex Writes a PhD for you (citation needed)
       (debugger +lsp)     ; TODO FIXME stepping through code, to help you add bugs
       (docker +lsp)
       editorconfig        ; let someone else argue about tabs vs spaces
       ein                 ; TODO try Jupyter notebooks with emacs
       (eval +overlay)     ; TODO run code, run (also, repls)
       (lookup +dictionary +docsets)      ; navigate your code and its documentation
       (lsp +peek)         ; TODO M-x vscode
       magit               ; a git porcelain for Emacs. NOTE Add (+forge) when dealing with issues and PRs
       make                ; run make tasks from Emacs
       pdf                 ; pdf enhancements
       tmux
       ;;terraform         ; May try it: infrastructure as code, try it when using cloud services
       tree-sitter       ; TODO use the built-in in Emacs29+ instead later on
       upload            ; map local to remote projects via ssh/ftp

       :os
       ;; (:if IS-MAC macos)  ; improve compatibility with macOS
       ;; tty               ; Enable as needed, improve the terminal Emacs experience,

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       (cc +lsp +tree-sitter)         ; C > C++ == 1
       ;;clojure           ; java with a lisp
       ;;[opt]common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       graphviz
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       ;;(haskell +lsp)    ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       (json +lsp +tree-sitter)       ; At least it ain't XML
       ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
       (javascript +lsp +tree-sitter) ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       (latex +cdlatex)      ; NOTE may try +lsp
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
       ;;lua               ; one-based indices? one-based indices
       markdown            ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective came
       (org +noter +roam2 +pretty +present)
       ;;php               ; perl's insecure younger brother
       plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp +pyenv +poetry +pyright +tree-sitter) ; beautiful is better than ugly
       qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       rst                 ; ReST in peace TODO pipx install rstfmt for formatting
       (rest + jq)         ; Emacs as a REST client +jq Enable support for reading and processing REST responses with jq
       (sh +lsp +tree-sitter)
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web +lsp +tree-sitter)   ; support for various web languages, including HTML5, CSS, SASS/SCSS, as well as Django
       (yaml +lsp +tree-sitter)        ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       ;; TODO may try this later, when writing email with org is needed
       ;;(mu4e +org +gmail)
       ;;(wanderlust +gmail)

       :app
       calendar
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;(rss +org)          ; emacs as an RSS reader

       :config
       (default +smartparens) ;; +bindings

       :cc
       appearance
       bindings
       better-defaults
       notes
       agenda
       dev

       :cc-langs
       elisp
       cpp
       python
       )
