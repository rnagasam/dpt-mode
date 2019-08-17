;;;; Digital Paper minor mode
;;;; requires https://github.com/janten/dpt-rp1-py

(defvar dpt-script "dptrp1"
  "Name of script used to interact with DPT RP1.
See https://github.com/janten/dpt-rp1-py.")
(defvar dpt-client-id "~/dpt/deviceid.dat"
  "Client ID file.")
(defvar dpt-private-key "~/dpt/privatekey.dat"
  "Private key file.")
(defvar dpt-addr nil
  "Hostname of IP address of DPT RP1 device.")
(defvar dpt-available-commands
  '("list-documents" "upload" "download" "command-help")
  "List of available commands.")

(defun dpt-check-script ()
  "Check if `dpt-script' is available."
  (if (not (executable-find dpt-script))
      (error "%s not found." dpt-script)))

(defun dpt-build-command (command args)
  "Build command to run in shell."
  (interactive)
  (concat dpt-script
          " --addr " dpt-addr
          " --client-id " dpt-client-id
          " --key " dpt-private-key
          " " command " " args))

(defun dpt-run-command (command args)
  "Run `dpt-script' with COMMAND and ARGS."
  (interactive
   (let ((com (completing-read
               "Command: " dpt-available-commands)))
     (list com
           (if (string-equal com "list-documents")
               ""
             (read-string "Args: ")))))
  (shell-command (dpt-build-command command args) "*dpt*" "*dpt-errors*"))
