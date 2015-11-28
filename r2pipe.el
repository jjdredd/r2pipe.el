;;
;; R2PIPE in Emacs Lisp
;; Judge Dredd (jjdredd @ github) the.guard@mail.ru
;;

(require 'json)

;; Temporary storage for r2 process std output
(setq r2-pipe-out-string nil)

(defun r2-pipe-filter (process output)
  "This filter callback is used by emacs whenever a process has output"
  (setq r2-pipe-out-string (concat r2-pipe-out-string output)))

(defun r2-pipe-new (cmdline)
  "Spawn r2 with cmdline and return process object on success or nil on failure"
  (let ((process (start-process "radare2" nil "r2" "-q" cmdline)))
    (if (equal (process-status process) 'run)
	(progn (set-process-filter process 'r2-pipe-filter) process)
      nil)))

(defun r2-cmd (process command)
  "Executes an r2 command and returns output in a string"
  (setq r2-pipe-out-string nil)
  (process-send-string process (format "%s\n" command))
  (accept-process-output process .1)
  (concat r2-pipe-out-string))

(defun r2-cmd-json (process command)
  "Executes a json r2 command and returns output in an elisp object"
  (json-read-from-string (r2-cmd process command)))

(defun r2-pipe-close (process)
  "Closes r2"
  (process-send-string process "q!!\n"))

(defun r2-kill (process)
  "Kills r2"
  (kill-process process))
