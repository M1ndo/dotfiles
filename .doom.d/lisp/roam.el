;; -*- lexical-binding: t; -*-
(require 'org-roam)
(use-package! org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (setq org-roam-db-gc-threshold gc-cons-threshold)
  (org-roam-directory "~/org/roam")
  (org-roam-db-location (concat org-directory ".org-roam.db"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n#+category: ${tag}\n#+filetags: ${tag}")
      :unnarrowed t)
     ("p" "project" plain
      (file "~/org/Templates/Projects.org")
      :if-new (file+head "${slug}.org" "#+title: ${title}\n#+category: Projects\n#+filetags: Project")
      :unnarrowed t)
     ("g" "BBounty" plain
      (file "~/org/Templates/Bounty.org")
      :if-new (file "${slug}.org")
      :head "#+TITLE: ${title}"
      :unnarrowed t)
     ("b" "Boxes" plain
      (file "~/org/Templates/Boxes.org")
      :if-new (file+head "${slug}.org" "#+title: ${title}\n#+category: Boxes\n#+filetags: Boxes")
      :unnarrowed t)
     ("l" "Life" plain
      (file "~/org/Templates/Life.org")
      :if-new (file+head "${slug}.org" "#+title: ${title}\n#+category: ${Cat}\n#+filetags: Life")
      :unnarrowed t)
     ("m" "Biblio" plain
      (file "~/org/Templates/Bib.org")
      :if-new (file+head "${slug}.org" "#+title: ${title}\n#+category: ${Cat}\n#+filetags: Biblio")
      :unnarrowed t)
     ("e" "Letter" plain
      (file "~/org/Templates/Letter.org")
      :if-new (file+head "${slug}.org" "#+title: ${title}\n#+category: ${Cat}\n#+filetags: Letter")
      :unnarrowed t)
     ("t" "Ebook" plain
      (file "~/org/Templates/novel.tex")
      :if-new (file+head "${slug}.tex" "#+title: ${title}\n#+category: ${Cat}\n#+filetags: Ebook")
      :unnarrowed t)
     ("s" "School" plain
      (file "~/org/Templates/School.org")
      :if-new (file+head "{slug}.org" "#+title: ${title}\n#+category: ${Cat}\n#+filetags: School")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "\n* %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+category: Daily\n#+filetags: Daily")
      :unnarrowed t)
     ("m" "maybe do today" entry "\n* %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+category: Daily\n#+filetags: MaybeTo")
      :unnarrowed t)
     ("t" "Do today" entry "\n* TODO %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+category: Daily\n#+filetags: todo")
      :unnarrowed t)
     ("j" "Journal" entry "* %<%H:%M> %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+category: Daily\n#+filetags: Journal")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n p" . my/org-roam-find-project)
         ("C-c n t" . my/org-roam-capture-task)
         ("C-c n b" . my/org-roam-capture-inbox)
         ("C-c n c" . org-roam-capture)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

;; (defun roam-extra:get-filetags ()
;;   (split-string (or (org-collect-keywords "filetags") "")))

;; (defun roam-extra:add-filetag (tag)
;;   (let* ((new-tags (cons tag (roam-extra:get-filetags)))
;;          (new-tags-str (combine-and-quote-strings new-tags)))
;;     (org-roam-set-keyword "filetags" new-tags-str)))

;; (defun roam-extra:del-filetag (tag)
;;   (let* ((new-tags (seq-difference (roam-extra:get-filetags) `(,tag)))
;;          (new-tags-str (combine-and-quote-strings new-tags)))
;;     (org-roam-set-keyword "filetags" new-tags-str)))

;; (defun roam-extra:todo-p ()
;;   "Return non-nil if current buffer has any TODO entry.

;; TODO entries marked as done are ignored, meaning the this
;; function returns nil if current buffer contains only completed
;; tasks."
;;   (org-element-map
;;       (org-element-parse-buffer 'headline)
;;       'headline
;;     (lambda (h)
;;       (eq (org-element-property :todo-type h)
;;           'todo))
;;     nil 'first-match))

;; (defun roam-extra:update-todo-tag ()
;;   "Update TODO tag in the current buffer."
;;   (when (and (not (active-minibuffer-window))
;;              (org-roam-file-p))
;;     (org-with-point-at 1
;;       (let* ((tags (roam-extra:get-filetags))
;;              (is-todo (roam-extra:todo-p)))
;;         (cond ((and is-todo (not (seq-contains-p tags "todo")))
;;                (roam-extra:add-filetag "todo"))
;;               ((and (not is-todo) (seq-contains-p tags "todo"))
;;                (roam-extra:del-filetag "todo")))))))

;; (defun roam-extra:todo-files ()
;;   "Return a list of roam files containing todo tag."
;;   (org-roam-db-sync)
;;   (let ((todo-nodes (seq-filter (lambda (n)
;;                                   (seq-contains-p (org-roam-node-tags n) "todo"))
;;                                 (org-roam-node-list))))
;;     (seq-uniq (seq-map #'org-roam-node-file todo-nodes))))


;; (defvar roam-extra-original-org-agenda-files (append (my/org-roam-list-notes-by-tag "todo") (list "~/org/agenda.org" "~/org/notes.org" "~/org/todo.org"))
;;   "Original value of  `org-agenda-files'.")

;; (defun roam-extra:update-todo-files (&rest _)
;;   "Update the value of `org-agenda-files'."
;;   (unless roam-extra-original-org-agenda-files
;;     (setq roam-extra-original-org-agenda-files org-agenda-files))
;;   (setq org-agenda-files
;;         (append roam-extra-original-org-agenda-files
;;                 (roam-extra:todo-files))))

;; ;; (add-hook 'find-file-hook #'roam-extra:update-todo-tag)
;; ;; (add-hook 'before-save-hook #'roam-extra:update-todo-tag)
;; (advice-add 'org-agenda :before #'roam-extra:update-todo-files)

(setq org-roam-node-display-template
      (concat (propertize "${title:90}" 'face 'org-document-info)
              (propertize "${tags:*}" 'face 'org-tag)))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files
        (append
         (my/org-roam-list-notes-by-tag "todo")
         (list "~/org/agenda.org"
               "~/org/notes.org"
               "~/org/todo.org"))))
         ;; (my/org-roam-list-notes-by-tag "Biblio")
         ;; (my/org-roam-list-notes-by-tag "Life"))))

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))
(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (lambda (node)
     ;; Only look for nodes tagged with at least one of the following keywords
     (seq-intersection '("Project" "Life" "Biblio" "Letter" "Ebook" "Boxes" "School")
                       (org-roam-node-tags node)))))

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (lambda (node)
                              (seq-intersection '("Project" "Daily" "Todo")
                                                (org-roam-node-tags node))))
                     ;; (my/org-roam-filter-by-tag "Project" ))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Tasks"
                                                          ("Tasks"))))))

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head+olp "%<%Y-%m-%d>.org" "%<%Y-%m-%d>\n#+category: Finished\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (or (equal org-state "DONE") (equal org-state "COMPLETED") (equal org-state "NO") (equal org-state "KILL") (equal org-state "CANCELLED"))
                 (my/org-roam-copy-todo-to-today))))
