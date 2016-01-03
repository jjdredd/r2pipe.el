;;
;; R2PIPE in Emacs Lisp
;; Judge Dredd (jjdredd @ github) the.guard@mail.ru
;;

(require 'json)

;; Temporary storage for r2 process std output
;; FIXME: this shouldn't be a global variable
;; make a list with "process" structure instead and return this
;; buffer with it in a list or cons
(setq r2-pipe-out-string nil)

(defun r2-pipe-filter (process output)
  "This filter callback is used by emacs whenever a process has output"
  (setq r2-pipe-out-string (concat r2-pipe-out-string output)))

(defun r2-pipe-read (process)
  "Reads process output till \x00"
  (setq r2-pipe-out-string nil)
  (while (progn (accept-process-output process)
		(not (string-suffix-p "\x00" r2-pipe-out-string))))
  (concat r2-pipe-out-string))

(defun r2-pipe-new (cmdline)
  "Spawn r2 with cmdline and return process object on success or nil on failure"
  ;; "-0" also imlplies quiet, scr.color = false, scr.prompt = false
  ;; and scr.interactive = false
  (let ((process (start-process "radare2" nil "r2" "-0" cmdline)))
    (if (equal (process-status process) 'run)
	(progn (set-process-filter process 'r2-pipe-filter)
	       (r2-pipe-read process) 	;skip \x00 after r2 init
	       process)
      nil)))

(defun r2-cmd (process command)
  "Executes an r2 command and returns output in a string"
  (process-send-string process (format "%s\n" command))
  (r2-pipe-read process))

(defun r2-cmd-json (process command)
  "Executes a json r2 command and returns output in an elisp object"
  (json-read-from-string (r2-cmd process command)))

(defun r2-pipe-close (process)
  "Closes r2"
  (process-send-string process "q!!\n"))

(defun r2-kill (process)
  "Kills r2"
  (kill-process process))
