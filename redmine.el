
;; Globals

(defvar *redmine-cache*
  (make-hash-table :test 'equal))


;; Constants

(defvar redmine-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "TAB") 'redmine-toggle)
    (define-key map (kbd "g") 'redmine-update-display)
    map))


;; Functions

(defun redmine-mode ()
  "Review the project list and recently active tickets."
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (toggle-read-only t)
  (setq major-mode 'redmine-mode
        mode-name "Redmine"
        mode-line-process "")
  (use-local-map redmine-mode-map)
  (run-mode-hooks 'redmine-mode)
  (redmine-update-display))

(defun redmine-toggle ()
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

(defun redmine-update-display ()
  "Refresh the display."
  (interactive)
  (let ((buffer-read-only nil)) 
    (delete-region (point-min) (point-max))
    (let ((projects (redmine-get-projects)))
      (redmine-insert-property-list
       `(("Domain" . ,(redmine-domain))
         ("User" . ,(redmine-username))
         ("Project count" . ,(length projects))))
      (insert "\n")
      (redmine-insert-projects-list)
      (goto-char (point-max)))))

(defun redmine-insert-property-list (assocs)
  (let ((max-len (reduce 'max (mapcar (lambda (x) (length (car x))) assocs))))
    (mapc (lambda (assoc)
            (insert (format "%s:  %s%s\n"
                            (car assoc)
                            (make-string (- max-len (length (car assoc))) ? )
                            (cdr assoc))))
          assocs)))

(defun redmine-insert-projects-list ()
  "Insert the projects."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (let ((projects (redmine-get-projects)))
        (insert (propertize "Projects:" 'face 'redmine-section-title)
                "\n")
        (redmine-insert-projects projects)
        (insert "\n\n")
        (backward-char 3)
        (redmine-toggle)))))

(defun redmine-insert-projects (projects &optional parent indent)
  (mapc (lambda (project)
          (insert (format "\t%s%s\n"
                          (make-string (or indent 0) ? )
                          (redmine-project-name project)))
          (redmine-insert-projects projects (redmine-project-id project) (+ (or indent 0) 3)))
        (remove-if-not (lambda (project)
                         (eql (redmine-project-parent project)
                              parent))
                       projects)))

(defun redmine-download-and-parse (path)
  "GET from the `path' of the given Redmine API."
  (let ((uri (format "curl -s -S -f 'http://%s:%s@%s/%s' 2>/dev/null"
                     (redmine-username)
                     (redmine-password)
                     (redmine-domain)
                     path)))
    (with-temp-buffer
      (shell-command uri (current-buffer))
      (let ((output (buffer-substring-no-properties (point-min) (point-max)))
            (result (xml-parse-region (point-min) (point-max))))
        (if result
            result
          result
          (throw 'redmine-curl-error
                 (format "Problem downloading from Redmine with: %s"
                         uri)))))))

(defun redmine-get-projects ()
  "Get my Redmine projects."
  (redmine-cache
   'projects
   (let ((projects (remove-if-not 'listp
                                  (cddar (redmine-download-and-parse "projects.xml")))))
     (mapcar (lambda (project)
               (flet ((get (key) (aif (assoc key project) (caddr it)))
                      (attribute (key) (aif (assoc key project) (cadr it))))
                 (redmine-project-make
                  :id (aif (get 'id) (string-to-int it))
                  :name (get 'name)
                  :identifier (get 'identifer)
                  :description (get 'description)
                  :parent (aif (attribute 'parent)
                               (aif (assoc 'id it)
                                    (string-to-int (cdr it))))
                  :created-on (get 'created_on)
                  :updated-on (get 'updated_on))))
             projects))))

(defun redmine-username ()
  "Get the redmine username."
  (if (string= "" redmine-username)
      (setq redmine-username (read-from-minibuffer "Username: "))
    redmine-username))

(defun redmine-password ()
  "Get the redmine password."
  (if (string= "" redmine-password)
      (setq redmine-password (read-passwd "Password: "))
    redmine-password))

(defun redmine-domain ()
  "Get the redmine domain."
  (if (string= "" redmine-domain)
      (setq redmine-domain (read-from-minibuffer "Domain: "))
    redmine-domain))


;; Types

(defstruct
  (redmine-project
   (:constructor redmine-project-make))
  id
  name
  identifier
  description
  parent
  created-on
  updated-on)


;; Macros

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro redmine-cache (key generate)
  (let ((value (gensym)))
    `(let ((,value (gethash ,key *redmine-cache* :nothing)))
       (if (eq ,value :nothing)
           (let ((,value ,generate))
             (progn (puthash ,key ,value *redmine-cache*)
                    ,value))
         ,value))))


;; Customization

(defgroup redmine nil
  "Redmine customization group.")

(defcustom redmine-username ""
  "Your Redmine username."
  :type 'string
  :group 'redmine)

(defcustom redmine-password ""
  "Your Redmine password."
  :type 'string
  :group 'redmine)

(defcustom redmine-domain ""
  "Your Redmine domain."
  :type 'string
  :group 'redmine)

;; Faces

(defgroup redmine-faces nil
  "Redmine faces customization group.")

(defface redmine-section-title
  '((t :inherit header-line))
  "Face for section titles."
  :group 'redmine-faces)


;; Test values
