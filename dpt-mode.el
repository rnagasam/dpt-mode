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

(defun dpt-argument-p (command)
  "Check whether COMMAND requires an argument."
  (not (string-equal command "list-documents")))

(defun dpt-build-command (command &optional args)
  "Build command to run in shell."
  (concat dpt-script
          " --addr " dpt-addr
          " --client-id " dpt-client-id
          " --key " dpt-private-key
          " " command
          (when args
            (concat " " args))))

(defun dpt-list-documents-view (process msg)
  (when (memq (process-status process) '(exit signal))
    (pop-to-buffer "*dpt-listing*")
    (goto-char (point-min))
    (view-mode)))

(defun dpt-list-documents ()
  (interactive)
  (set-process-sentinel
   (start-process-shell-command
    "dptrp1" "*dpt-listing*" (dpt-build-command "list-documents"))
   'dpt-list-documents-view))

(defun dpt-download (src dest)
  "Download file from DPT RP1."
  (interactive
   (list (read-string "From: ")
         (read-file-name "To: ")))
  (dpt-run-command "download" (concat "'" src "' " dest)))

(defun dpt-listing-download ()
  (let* ((src (thing-at-point 'line t))
         (len (length src))
         (dest (read-file-name "To: ")))
    (dpt-download (substring src 0 (- len 1)) dest)))

(defun dpt-run-command (command &optional args)
  "Run `dpt-script' with COMMAND and ARGS."
  (interactive
   (let ((com (completing-read
               "Command: " dpt-available-commands nil t)))
     (list com (when (dpt-argument-p com) (read-string "Args: ")))))
  (start-process-shell-command "dpt-run" "*dpt-run-output*"
                               (dpt-build-command command args)))

(define-minor-mode dpt-mode
  "Interact with DPT RP1"
  :lighter "Dpt"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-<RET>") 'dpt-listing-download)
            map))

(provide 'dpt-mode)
