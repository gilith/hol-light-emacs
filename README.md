Emacs Macros for HOL Light Proof
================================

Instructions
------------

Clone this repo and add a line like this into your `.emacs` file:

    (load "~/hol-light-emacs/hol-light.el")

Launch Emacs and start up a HOL Light process in an Emacs shell:

    M-x shell
    ocaml
    #use "hol.ml";;

Load a proof script file and interact with the HOL Light process as follows:

 * Type `C-c C-l` to recentre the shell window so the OCaml prompt appears right at the bottom.
 * Type `C-c C-c` to evaluate the paragraph containing the cursor in the HOL Light process.
 * Type `C-c C-w` to evaluate the word containing the cursor in the HOL Light process.
 * Type `C-c C-r` to evaluate the highlighted region in the HOL Light process.
 * Highlight lines of the proof script containing a goal term and type `C-c C-g` to set it as the goal in HOL Light.
 * Highlight lines of the proof script containing one or more tactics and type `C-c C-e` to apply the tactics to the current subgoal.
 * Type `C-c C-b` to undo the last tactic application.
 * Position the cursor at some point in a tactic proof and type `C-c C-p` to jump to that proof point.

There are some configuration options at the top of the `hol-light.el` file, including the key bindings for interacting with the HOL Light process.

Notes
-----

These macros were inspired by Michael Norrish's `hol-mode` for interacting with HOL4 in Emacs, and also by Freek Wiedijk's vi mode for HOL Light.

If people find these simple macros to be useful then it would be nice to package them into an Emacs mode for HOL Light, possibly with syntax highlighting for quoted terms/tacticals/etc and standard indentation rules for tactic proofs. The macros are released under the MIT license, and pull requests are cheerfully received.
