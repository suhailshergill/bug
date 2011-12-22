
;; Constants

(defvar bug-issue-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'kill-buffer-and-window)
    (define-key map (kbd "g") 'bug-issue-refresh)
    map))


;; Functions

(defun bug-issue-mode (session issue)
  "Display an issue."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (toggle-read-only t)
  (setq major-mode 'bug-issue-mode
        mode-name "Bug-Issue"
        mode-line-process "")
  (use-local-map bug-issue-mode-map)
  (set (make-local-variable 'bug-session) session)
  (set (make-local-variable 'bug-issue) issue)
  (bug-issue-update-display)
  (run-mode-hooks 'bug-issue-mode))

(defun bug-issue-refresh ()
  "Refresh the issue."
  (interactive)
  (set (make-local-variable 'bug-issue)
       (let ((tracker (bug-session-tracker bug-session)))
         (let ((bug-cache-force-refresh t))
           (funcall (bug-tracker-get-issue tracker) tracker (bug-issue-id bug-issue)))))
  (bug-issue-update-display))

(defun bug-issue-update-display ()
  "Update the issue's display from the issue data."
  (let ((buffer-read-only nil))
    (erase-buffer)
    (let ((projects (let ((tracker (bug-session-tracker bug-session)))
                      (funcall (bug-tracker-get-projects tracker) tracker))))
      (insert (format "#%d (%s)\n"
                      (bug-issue-id bug-issue)
                      (bug-aif (let ((issue-project (bug-issue-project-id bug-issue)))
                                 (remove-if-not (lambda (project)
                                                  (= (bug-project-id project)
                                                     issue-project))
                                                projects))
                               (bug-project-name (car it))
                               "unknown project"))))
    (bug-insert-property-list (bug-issue-meta-data bug-issue))
    (bug-insert-updates (bug-issue-updates bug-issue))))

(defun bug-insert-updates (updates)
  "Insert the updates for this issue."
  (mapc (lambda (update)
          (insert "\nUpdate by "
                  (propertize (bug-issue-update-user-name update)
                              'face 'bug-key-title)
                  "\n\n")
          (let ((start (point)))
            (insert (or (bug-issue-update-notes update)
                        "—"))
            (fill-region start (point)))
          (insert "\n"))
        updates))

(defun bug-issue (session issue-no)
  "Create an issue buffer."
  (let ((issue (let ((tracker (bug-session-tracker bug-session)))
                 (funcall (bug-tracker-get-issue tracker) tracker issue-no))))
    (let ((buffer (get-buffer-create (format "*bug-issue-%d*" issue-no))))
      (with-current-buffer buffer
        (unless (eq major-mode 'bug-issue-mode) (bug-issue-mode session issue))
        (switch-to-buffer-other-window buffer)))))


;; Types

(defstruct
  (bug-issue
   (:constructor bug-issue-make))
  id
  project-id
  description
  relations
  meta-data
  updates)

(defstruct
  (bug-issue-update
   (:constructor bug-issue-update-make))
  id
  user-name
  notes
  changes
  meta-data)


;; Test values

(provide 'bug-issue-mode)
