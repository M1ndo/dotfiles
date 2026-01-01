(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────")
  (setq org-agenda-custom-commands
        '(("d" "Today view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-agenda-span 'day)
                        (org-agenda-start-day nil)
                        ;; always show timelines!
                        (org-agenda-time-grid '((daily today) (800 1000 1200 1400 1600 1800 2000 2200 0000) "" "----------------"))
                        (org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────")
                        (org-agenda-prefix-format '((agenda . " %i %?-12t%-6e% s")))
                        (org-super-agenda-groups
                         '((:name "Scheduled Today"
                            :time-grid t
                            :date today
                            :order 1))
                         )
                        )
                    )
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-agenda-prefix-format '((agenda . " %i %?-12t%-6e% s")
                                                     (todo . " %i %-6e")
                                                     (tags . " %i %-12:c")
                                                     (search . " %i")))
                         (org-super-agenda-groups
                          '((:name "Important"
                             :face (:slant italic)
                             :priority "A"
                             :order 1)
                            (:name "Habits"
                             :habit t
                             :date today
                             :order 2)
                            (:name "Low Effort (<= 15 min)"
                             :and (:effort< "0:16")
                             :order 2)
                            (:name "Due Today"
                             :deadline today
                             :order 2)
                            (:name "Due Soon"
                             :deadline future
                             :order 8)
                            (:name "Next Tasks"
                             :todo "NEXT"
                             :order 3)
                            (:name "Overdue"
                             :deadline past
                             :order 5)
                            (:name "Ongoing"
                             :scheduled past
                             :order 6
                             )
                            (:name "Movies/Videos And Books"
                             :tag ("TO_WATCH" "WATCH_ME" "WATCHME" "TO_READ" "README" "READ_ME")
                             :order 6)
                            (:discard (:anything t))))))))
          ("w" "Week view"
           ((agenda "" ((org-agenda-overriding-header "Week view")
                        (org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)
                        (org-agenda-time-grid '(nil (800 1000 1200 1400 1600 1800 2000) "" "----------------"))
                        (org-agenda-prefix-format '((agenda . " %i %?-12t%-6e% s")))
                        )
                    )
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Overdue (past scheduled/deadline)"
                             :deadline past
                             :scheduled past
                             :order 1
                             )
                            (:name "Individual Tasks"
                             :file-path "task"
                             :order 2
                             )
                            (:name "Next tasks"
                             :todo "NEXT"
                             :order 3)
                            (:discard (:anything t))
                            )
                          )
                         )
                     )
            )
           )
          ("p" . "Planning")
          ("pm" "Month view"
           (
            (tags-todo "Life" ((org-agenda-overriding-header "Goals")
                               (org-super-agenda-groups
                                '((:todo "GOAL")
                                  (:discard (:anything t)))))
                       )
            (agenda "" ((org-agenda-span 'month)
                        (org-agenda-start-day "01")
                        (org-agenda-start-on-weekday 1)
                        (org-agenda-current-span t)
                        (org-super-agenda-groups
                         '((:scheduled t))))
                    )
            (todo "" ((org-agenda-overriding-header "Things to schedule")
                      (org-super-agenda-groups
                       '((:name "Next tasks"
                          :todo "NEXT"
                          )
                         (:discard (:anything t)))))
                  )
            ))
          ))
  :config
  (org-super-agenda-mode))
