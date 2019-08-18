;;;; Digital Paper minor mode
;;;; requires https://github.com/janten/dpt-rp1-py

(require 'subr-x)
(require 'outline)

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

(defun dpt-view (process msg)
  ;; wait until `dpt-list-documents' has finished, then
  ;; `pop-to-buffer'.
  (when (memq (process-status process) '(exit signal))
    (pop-to-buffer "*dpt-listing*")
    (goto-char (point-min))
    (sort-lines nil (point-min) (point-max))
    ;; Format buffer -- structure directories and files better.
    (save-excursion
      (let ((prev (file-name-directory (thing-at-point 'line t))))
        (insert "* " prev)
        (newline)
        (while (not (last-line-p))
          (let ((curr (file-name-directory (thing-at-point 'line t)))
                (name (file-name-base (thing-at-point 'line t))))
            (if (not (string-equal prev curr))
                ;; new folder -- create a section
                (progn (newline)
                       (insert "* " curr) ; insert current folder name
                       (newline)
                       (insert "  " name)
                       (kill-line)
                       (setq prev curr))
              ;; same folder -- place filename below
              (kill-line)
              (insert "  " name)))
          (forward-line))))
    (read-only-mode)
    (outline-minor-mode)
    (dpt-mode)))

(defun dpt-list-documents ()
  (interactive)
  (set-process-sentinel
   (start-process-shell-command
    "dptrp1" "*dpt-listing*" (dpt-build-command "list-documents"))
   'dpt-view))

(defun dpt-download (src dest)
  "Download file from DPT RP1."
  (interactive
   (list (read-string "From: ")
         (read-file-name "To: ")))
  (dpt-run-command "download" (concat "'" src "' " dest)))

(defun dpt-path-of-entry ()
  "Get full path of file at point."
  (if (outline-on-heading-p) ;; point at directory
      (error "Point must be at file and not directory")
    (let ((name (string-trim (thing-at-point 'line t))))
      (save-excursion
        (outline-previous-heading)
        (concat
         ;; directory
         (substring (string-trim (thing-at-point 'line t)) 2 nil)
         ;; base name
         name
         ;; extension
         ".pdf")))))

(defun dpt-download-entry ()
  (interactive)
  (let* ((src (dpt-path-of-entry))
         (dest (read-file-name "Save as: ")))
    (dpt-download src dest)))

(defun dpt-get-directory-at-point ()
  (file-name-directory (string-trim-right (thing-at-point 'line t))))

(defun dpt-upload (src dest)
  "Upload file to DPT RP1."
  (interactive
   (list (read-file-name "From: ")
         (read-string "To: ")))
  (dpt-run-command "upload" (concat src " '" dest "'")))

(defun dpt-upload-to-directory-at-point ()
  (interactive)
  (let* ((dir (dpt-get-directory-at-point))
         (src (read-file-name "Upload: "))
         (default (concat dir (file-name-nondirectory src)))
         (dest (read-string "To: " default nil default)))
    (dpt-upload src dest)))

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
            (define-key map (kbd "<C-return>") 'dpt-download-listing-at-point)
            (define-key map (kbd "d") 'dpt-download-entry)
            (define-key map (kbd "u") 'dpt-upload-to-directory-at-point)
            (define-key map (kbd "n") 'outline-next-visible-heading)
            (define-key map (kbd "p") 'outline-previous-visible-heading)
            (define-key map (kbd "q") 'kill-current-buffer)
            map))

(provide 'dpt-mode)
