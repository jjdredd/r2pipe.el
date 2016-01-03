;;
;; test R2PIPE in Emacs Lisp
;; Judge Dredd (jjdredd @ github) the.guard@mail.ru
;;
;; run with emacs --script test.el
;; if r2pipe.el isn't in the same directory,
;; modify its path in the script below
;;


(load-file "r2pipe.el")

(setq r2p (r2-pipe-new "/bin/ls"))
(print (r2-cmd r2p "pd 10"))
(print (r2-cmd-json r2p "pdj 10"))
(r2-pipe-close r2p)
