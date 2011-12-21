(defun bug-redmine-tracker ()
  "Make the instance of the bug-tracker ‘class’."
  (bug-tracker-make
   :get-projects 'bug-redmine-get-projects
   :get-recent-tickets nil
   :internal-state nil
   :title "Redmine"))

(defun bug-redmine-download-and-parse (path)
  "GET from the `path' of the given bug tracker API."
  (let ((uri (format "curl -s -S -f 'http://%s:%s@%s/%s' 2>/dev/null"
                     (bug-username)
                     (bug-password)
                     (bug-domain)
                     path)))
    (with-temp-buffer
      (shell-command uri (current-buffer))
      (let ((output (buffer-substring-no-properties (point-min) (point-max)))
            (result (xml-parse-region (point-min) (point-max))))
        (if result
            result
          result
          (throw 'bug-curl-error
                 (format "Problem downloading from Redmine with: %s"
                         uri)))))))

(defun bug-redmine-get-projects (tracker)
  "Get my bug tracker projects."
  (bug-cache
   'projects
   (let ((projects (remove-if-not 'listp
                                  (cddar (bug-redmine-download-and-parse "projects.xml")))))
     (mapcar (lambda (project)
               (flet ((get (key) (bug-aif (assoc key project) (caddr it)))
                      (attribute (key) (bug-aif (assoc key project) (cadr it))))
                 (bug-project-make
                  :id (bug-aif (get 'id) (string-to-int it))
                  :name (get 'name)
                  :identifier (get 'identifer)
                  :description (get 'description)
                  :parent (bug-aif (attribute 'parent)
                                   (bug-aif (assoc 'id it)
                                            (string-to-int (cdr it)))))))
             projects))))

(defun bug-redmine ())

(provide 'bug-redmine)
