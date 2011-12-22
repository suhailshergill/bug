(defun bug-redmine-tracker ()
  "Make the instance of the bug-tracker ‘class’."
  (bug-tracker-make
   :get-projects 'bug-redmine-get-projects
   :get-issue 'bug-redmine-get-issue
   :internal-state nil
   :title "Redmine"))

(defun bug-redmine-download-and-parse (path)
  "GET from the `path' of the given bug tracker API."
  (let* ((session bug-session)
         (uri (format "curl -s -S -f 'http://%s:%s@%s/%s' 2>/dev/null"
                      (bug-session-get-username session)
                      (bug-session-get-password session)
                      (bug-session-get-domain session)
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

(defun bug-redmine-get-projects (.tracker)
  "Get my bug tracker projects."
  (bug-cache
   'projects
   (let ((projects (remove-if-not 'listp
                                  (cddar (bug-redmine-download-and-parse "projects.xml")))))
     (mapcar (lambda (project)
               (flet ((get (key) (bug-aif (assoc key project) (caddr it)))
                      (attribute-of (el-key attr-key)
                                    (bug-aif (bug-aif (bug-aif (assoc el-key project) (cadr it))
                                                      (assoc attr-key it))
                                             (when (listp it) (cdr it)))))
                 (bug-project-make
                  :id (bug-aif (get 'id) (string-to-int it))
                  :name (get 'name)
                  :identifier (get 'identifer)
                  :description (get 'description)
                  :parent (bug-aif (attribute-of 'parent 'id) (string-to-int it)))))
             projects))))

(defun bug-redmine-get-issue (.tracker issue-no)
  "Get an issue."
  (bug-cache
   (format "issue-%d" issue-no)
   (let ((issue (remove-if-not 'listp
                               (cddar (bug-redmine-download-and-parse
                                       (format "/issues/%d.xml" issue-no)))))
         (meta-attribute-keys
          `(("Type" . tracker)
            ("Status" . status)
            ("Priority" . priority)
            ("Author" . author)
            ("Assigned To" . assigned_to)
            ("Fixed Version" . fixed_version)))
         (meta-el-keys
          `(("Subject" . subject)
            ("Description" . description)
            ("Start Date" . start_date)
            ("Due Date" . due_date)
            ("Done Ratio" . done_ratio)
            ("Estimated hours" . estimated_hours)
            ("Created" . created_on)
            ("Updated" . udpated_on))))
     (flet ((get (key) (bug-aif (assoc key issue) (caddr it)))
            (attribute-of (el-key attr-key)
                          (bug-aif (bug-aif (bug-aif (assoc el-key issue) (cadr it))
                                            (assoc attr-key it))
                                   (when (listp it) (cdr it)))))
       (bug-issue-make
        :id (bug-aif (get 'id) (string-to-int it))
        :project-id (bug-aif (attribute-of 'project 'id) (string-to-int it))
        :meta-data (remove-if (lambda (x) (equal (cdr x) nil))
                              (append (mapcar (lambda (x) (cons (car x) (get (cdr x))))
                                              meta-el-keys)
                                      (mapcar (lambda (x) (cons (car x) (attribute-of (cdr x) 'name)))
                                              meta-attribute-keys)))
        :updates (let ((updates (bug-redmine-parse-journals issue)))
                   updates))))))

(defun bug-redmine-parse-journals (issue)
  "Parse the journals from an issue."
  (let ((project-els (remove-if (lambda (el)
                                  (or (not (listp el))
                                      (null el)))
                                (assoc 'journals issue))))
    (mapcar (lambda (project-el)
              (print project-el)
              (flet ((get (key) (bug-aif (assoc key project-el) (caddr it)))
                     (attribute-of (el-key attr-key)
                                   (bug-aif (bug-aif (bug-aif (assoc el-key project-el) (cadr it))
                                                     (assoc attr-key it))
                                            (when (listp it) (cdr it)))))
                (bug-issue-update-make
                 :id (bug-aif (assoc 'id (cadr project-el))
                              (string-to-int (cdr it)))
                 :user-name (attribute-of 'user 'name)
                 :notes (get 'notes)
                 :changes (bug-aif (remove-if (lambda (el)
                                                (or (not (listp el))
                                                    (null el)))
                                              (assoc 'details project-el))
                                   (mapcar (lambda (detail) detail)
                                           it)))))
            project-els)))

(defun bug-redmine ())

(provide 'bug-redmine)
