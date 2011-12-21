(defun bug-github-tracker ()
  "Make the instance of the bug-tracker ‘class’."
  (bug-tracker-make
   :get-projects 'bug-github-get-projects
   :get-recent-tickets nil
   :internal-state nil
   :title "Github"))

(defun bug-github-download-and-parse (path)
  "GET from the `path' of the given bug tracker API."
  (let ((uri (format "curl -s -S -f 'http://%s/%s' 2>/dev/null"
                     (bug-domain)
                     path)))
    (with-temp-buffer
      (shell-command uri (current-buffer))
      (let ((output (buffer-substring-no-properties (point-min) (point-max)))
            (result (let ((max-lisp-eval-depth 10000)) 
                      (message "Hold onto yer butts…")
                      (xml-parse-region (point-min) (point-max)))))
        (if result
            result
          result
          (throw 'bug-curl-error
                 (format "Problem downloading from Github with: %s"
                         uri)))))))

(defun bug-github-get-projects (tracker)
  "Get my bug tracker projects."
  (bug-cache
   'projects
   (let ((projects
          (remove-if-not 'listp
                         (cddar (bug-github-download-and-parse
                                 (format "api/v2/xml/repos/show/%s"
                                         (bug-username)))))))
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

(defun bug-github ())

(provide 'bug-github)
