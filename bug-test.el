(add-to-list 'load-path ".")
(load "bug.el")
(setq bug-trackers (list (bug-redmine-tracker)
                         (bug-github-tracker)))