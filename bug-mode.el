
;; Globals

(defvar bug-trackers '())


;; Constants

(defvar bug-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "TAB") 'bug-toggle)
    (define-key map (kbd "g") 'bug-update-display)
    map))


;; Functions

(defun bug ()
  (interactive)
  (let ((buffer (get-buffer-create "*bug*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'bug-mode) (bug-mode))
      (switch-to-buffer-other-window buffer))))

(defun bug-mode ()
  "Review the project list and recently active tickets."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (toggle-read-only t)
  (setq major-mode 'bug-mode
        mode-name "Bug"
        mode-line-process "")
  (use-local-map bug-mode-map)
  (set (make-local-variable 'bug-username) nil)
  (set (make-local-variable 'bug-password) nil)
  (set (make-local-variable 'bug-domain) nil)
  (set (make-local-variable 'bug-cache) (make-hash-table :test 'equal))
  (set (make-local-variable 'bug-tracker) (bug-choose-tracker))
  (run-mode-hooks 'bug-mode)
  (bug-update-display))

(defun bug-choose-tracker ()
  "Choose the tracker to use for this buffer."
  (let* ((choice (ido-completing-read
                  "Tracker: "
                  (mapcar 'bug-tracker-title bug-trackers)))
         (tracker (remove-if-not (lambda (tracker)
                                   (string= (bug-tracker-title tracker)
                                            choice))
                                 bug-trackers)))
    (when tracker
      (car tracker))))

(defun bug-toggle ()
  "Toggle the collapsed/expanded state of some part of the buffer."
  (interactive)
  (let ((buffer-read-only nil))
    (save-excursion
      (let ((end (progn (search-forward "\n\n" nil t 1)
                        (backward-char)
                        (point)))
            (start (progn (backward-char)
                          (search-backward-regexp "^[^\t]" nil t 1)
                          (search-forward "\n\t" nil t 1)
                          (backward-char 2)
                          (point))))
        (put-text-property start end 'invisible (not (get-text-property start 'invisible)))))))

(defun bug-update-display ()
  "Refresh the display."
  (interactive)
  (let ((buffer-read-only nil)) 
    (delete-region (point-min) (point-max))
    (let ((projects (bug-get-projects)))
      (bug-insert-property-list
       `(("Domain" . ,(bug-domain))
         ("User" . ,(bug-username))
         ("Project count" . ,(length projects))))
      (insert "\n")
      (bug-insert-projects-list)
      (goto-char (point-max)))))

(defun bug-insert-property-list (assocs)
  "Insert a property list."
  (let ((max-len (reduce 'max (mapcar (lambda (x) (length (car x))) assocs))))
    (mapc (lambda (assoc)
            (insert (format "%s:  %s%s\n"
                            (car assoc)
                            (make-string (- max-len (length (car assoc))) ? )
                            (cdr assoc))))
          assocs)))

(defun bug-insert-projects-list ()
  "Insert the projects."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (let ((projects (bug-get-projects)))
        (insert (propertize "Projects:" 'face 'bug-section-title)
                "\n")
        (bug-insert-projects projects)
        (insert "\n\n")
        (backward-char 3)
        (bug-toggle)))))

(defun bug-get-projects ()
  "Get the projects from on the current tracker."
  (funcall (bug-tracker-get-projects bug-tracker) bug-tracker))

(defun bug-insert-projects (projects &optional parent indent)
  "Insert projects tree into the buffer."
  (mapc (lambda (project)
          (insert (format "\t%s%s\n"
                          (make-string (or indent 0) ? )
                          (bug-project-name project)))
          (bug-insert-projects projects (bug-project-id project) (+ (or indent 0) 3)))
        (remove-if-not (lambda (project)
                         (eql (bug-project-parent project)
                              parent))
                       projects)))

(defun bug-username ()
  "Get the bug username."
  (or bug-username
      (setq bug-username (read-from-minibuffer "Username: "))))

(defun bug-password ()
  "Get the bug password."
  (or bug-password
      (setq bug-password (read-passwd "Password: "))))

(defun bug-domain ()
  "Get the bug domain."
  (or bug-domain
      (setq bug-domain (read-from-minibuffer "Domain: "))))


;; Types

(defstruct
  (bug-tracker
   (:constructor bug-tracker-make))
  get-projects
  get-recent-tickets
  internal-state
  title)

(defstruct
  (bug-project
   (:constructor bug-project-make))
  id
  name
  identifier
  description
  parent)


;; Macros

(defmacro bug-aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro bug-cache (key generate)
  (let ((value (gensym)))
    `(let ((,value (gethash ,key bug-cache :nothing)))
       (if (eq ,value :nothing)
           (let ((,value ,generate))
             (progn (puthash ,key ,value bug-cache)
                    ,value))
         ,value))))


;; Customization

(defgroup bug nil
  "Bug customization group.")

(defcustom bug-username ""
  "Your Bug username."
  :type 'string
  :group 'bug)

(defcustom bug-password ""
  "Your Bug password."
  :type 'string
  :group 'bug)

(defcustom bug-domain ""
  "Your Bug domain."
  :type 'string
  :group 'bug)

;; Faces

(defgroup bug-faces nil
  "Bug faces customization group.")

(defface bug-section-title
  '((t :inherit header-line))
  "Face for section titles."
  :group 'bug-faces)


;; Test values

(provide 'bug-mode)