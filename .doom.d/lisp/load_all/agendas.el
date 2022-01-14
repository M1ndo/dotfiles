(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-custom-commands
        '(("t" "Today view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-agenda-span 'day)
                        (org-agenda-start-day nil)
                        ;; always show timelines!
                        (org-agenda-time-grid '((daily today) (800 1000 1200 1400 1600 1800 2000) "" "----------------"))
                        (org-agenda-prefix-format '((agenda . " %i %?-12t%-6e% s")))
                        (org-super-agenda-groups
                         '((:name "Scheduled Today"
                            :time-grid t
                            :date today
                            :order 1)
                           (:name "Habits"
                            :habit t
                            :date today
                            :order 2)
                           (:name "Overdue"
                            :deadline past
                            :order 3)
                           (:name "Ongoing"
                            :scheduled past
                            :order 4
                            )
                           (:discard (:anything t)))
                         )
                        )
                    )
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-agenda-prefix-format '((agenda . " %i %?-12t%-6e% s")
                                                     (todo . " %i %-6e")
                                                     (tags . " %i %-12:c")
                                                     (search . " %i")))
                         (org-super-agenda-groups
                          '((:discard (:scheduled today))
                            (:name "Low Effort (<= 15 min)"
                             :and (:effort< "0:16")
                             :order 1)
                            (:name "Next Tasks"
                             :todo "NEXT"
                             :order 2)
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
            (tags-todo "+Goal" ((org-agenda-overriding-header "Goals")
                                )
                       )
            (agenda "" ((org-agenda-span 'month)
                        (org-agenda-start-day "01")
                        (org-super-agenda-groups
                         '((:discard (:todo "GOAL"))
                           (:discard (:todo "RECUR"))
                           (:scheduled t))
                         )
                        )
                    )
            (todo "" ((org-agenda-overriding-header "Things to schedule")
                      (org-super-agenda-groups
                       '((:name "Individual tasks"
                          :file-path "task"
                          )
                         (:name "Next tasks"
                          :todo "NEXT"
                          )
                         (:discard (:anything t)))
                       )
                      )
                  )
            ))
          ))
  :config
  (org-super-agenda-mode))
