;;; gitid.el --- Expand $GitId$ to RCS-style keyword with date and version
;;
;; Copyright (C) 2026
;; Author: ols3
;;
;; 將緩衝區中的 $GitId$ 替換為：
;;   $GitId: FILE,v VERSION YYYY/MM/DD HH:MM:SS USER Exp $
;; 其中 VERSION 為該檔案在目前分支的 commit 次數（格式 1.N），USER 取自 git user.name。
;;
;; 使用方式：
;;   M-x gitid-expand     — 立即替換目前 buffer 中所有 $GitId$
;;   M-x gitid-mode       — 切換 minor mode（可搭配 gitid-before-save）
;;   設定 (setq gitid-before-save t) 後，儲存時若在 git repo 會自動：
;;   - 未展開的 $GitId$ → 展開為完整行
;;   - 已展開的 $GitId: FILE,v 1.N ... Exp $ → 版次遞增為 1.(N+1) 並更新日期時間
;;
;; 在程式碼中預先寫好（支援 4 種註解格式）：
;;   /* $GitId$ */   — C/C++ 區塊註解
;;   // $GitId$     — C++/Java/JS 行註解
;;   # $GitId$      — Shell/Python/Ruby 行註解
;;   ;; $GitId$     — Lisp/Elisp 行註解
;; 存檔或執行 gitid-expand 後會變成例如：
;;   /* $GitId: 04LST01.c,v 1.2 2026/02/16 03:38:37 ols3 Exp $ */
;; 之後每次存檔會自動把 1.2 變成 1.3、1.3 變成 1.4 …
;;
;;; Code:

(defgroup gitid nil
  "Expand $GitId$ keyword for Git-tracked files."
  :group 'tools)

(defcustom gitid-version-format "1.%s"
  "Format string for version number. %s is the commit count for this file."
  :type 'string
  :group 'gitid)

(defcustom gitid-user (or (getenv "USER") "unknown")
  "User name to put in GitId expansion. Set from git config if possible."
  :type 'string
  :group 'gitid)

(defcustom gitid-before-save nil
  "If non-nil, expand $GitId$ in buffer when saving (when in a git repo)."
  :type 'boolean
  :group 'gitid)

(defun gitid--git-dir (&optional file)
  "Return git root directory for FILE (default: current buffer file), or nil."
  (let ((f (or file (buffer-file-name))))
    (when f
      (let ((default-directory (file-name-directory (expand-file-name f))))
        (with-temp-buffer
          (and (zerop (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
               (string-trim (buffer-string))))))))

(defun gitid--file-commit-count (file)
  "Return number of commits that touched FILE in current branch, or \"1\"."
  (when (and file (gitid--git-dir file))
    (with-temp-buffer
      (let ((default-directory (file-name-directory (expand-file-name file))))
        (when (zerop (call-process "git" nil t nil "rev-list" "--count" "HEAD" "--"
                                  (file-name-nondirectory file)))
          (string-trim (buffer-string)))))))

(defun gitid--git-user ()
  "Return git user.name if set, else gitid-user. Never return nil."
  (with-temp-buffer
    (if (zerop (call-process "git" nil t nil "config" "user.name"))
        (let ((s (string-trim (buffer-string))))
          (if (> (length s) 0) s (or gitid-user "unknown")))
      (or gitid-user "unknown"))))

(defun gitid--expand-string (filename version date-time user)
  "Build the GitId expansion string. All args are coerced to string; nil becomes fallback."
  (format "$GitId: %s,v %s %s %s Exp $"
          (or (and filename (format "%s" filename)) "unknown")
          (or (and version (format "%s" version)) "1.1")
          (or (and date-time (format "%s" date-time)) "1970/01/01 00:00:00")
          (or (and user (format "%s" user)) "unknown")))

(defconst gitid--expanded-regexp
  (concat "\\$GitId: \\([^,]+\\),v \\([0-9.]+\\) "
          "\\([0-9/]+\\) \\([0-9:]+\\) \\([^ ]*\\) Exp \\$")
  "Regexp matching already-expanded $GitId: FILE,v VERSION DATE TIME USER Exp $.")

;; 內層：GitId 內容的 regexp（capture 1-5）
(defconst gitid--expanded-inner-regexp
  (concat "\\$GitId: \\([^,]+\\),v \\([0-9.]+\\) "
          "\\([0-9/]+\\) \\([0-9:]+\\) \\([^ ]*\\) Exp \\$"))

;; 4 種註解：(PREFIX-RE SUFFIX-RE PREFIX-STR SUFFIX-STR)
;; SUFFIX-RE 為 "" 表示行註解；PREFIX-STR/SUFFIX-STR 為替換時要插入的字串
(defconst gitid--comment-patterns
  '(("/\\*[ \t]*" "\\s-*\\*/" "/* " " */")   ; /* ... */
    ("//[ \t]*" "" "// " "")                 ; // ...
    ("#[ \t]*" "" "# " "")                   ; # ...
    (";;[ \t]*" "" ";; " ""))                ; ;; ...
  "List of (prefix-re suffix-re prefix-str suffix-str) for comment styles.")

(defun gitid--version-increment (version)
  "Increment last number in VERSION string, e.g. \"1.6\" -> \"1.7\", \"1.10\" -> \"1.11\"."
  (let* ((parts (split-string version "\\."))
         (last-num (string-to-number (car (last parts))))
         (new-last (number-to-string (1+ last-num))))
    (mapconcat 'identity (append (butlast parts) (list new-last)) ".")))

(defun gitid--update-expanded ()
  "Replace one already-expanded comment line (/* // # ;;) with incremented version.
Supports: /* ... */, // ..., # ..., ;; ...
One replacement per call."
  (save-excursion
    (goto-char (point-min))
    (catch 'done
      (dolist (pat gitid--comment-patterns)
        (let ((pre-re (nth 0 pat))
              (suf-re (nth 1 pat))
              (pre-str (nth 2 pat))
              (suf-str (nth 3 pat)))
          (let ((full-re (concat pre-re gitid--expanded-inner-regexp suf-re)))
            (when (re-search-forward full-re nil t)
              (let* ((beg (match-beginning 0))
                     (end (match-end 0))
                     (new (concat pre-str
                                  (gitid--expand-string
                                   (match-string 1)
                                   (gitid--version-increment (match-string 2))
                                   (format-time-string "%Y/%m/%d %H:%M:%S")
                                   (gitid--git-user))
                                  suf-str)))
                (delete-region beg end)
                (goto-char beg)
                (insert new)
                (throw 'done t)))))))))

(defun gitid-expand ()
  "Replace all $GitId$ in current buffer with date and version.
Format: $GitId: FILE,v VERSION YYYY/MM/DD HH:MM:SS USER Exp $
Only runs when buffer has a file name and is inside a git repository."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer has no file name"))
  (unless (gitid--git-dir)
    (user-error "Not inside a git repository"))
  (let* ((file (or (file-name-nondirectory (buffer-file-name)) "unknown"))
         (count (gitid--file-commit-count (buffer-file-name)))
         (version (format (or gitid-version-format "1.%s")
                        (let ((c (string-trim (or count ""))))
                          (if (> (length c) 0) c "1"))))
         (date-time (format-time-string "%Y/%m/%d %H:%M:%S"))
         (user (gitid--git-user))
         (replacement (gitid--expand-string file version date-time user)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "$GitId$" nil t)
        (replace-match replacement t t)))))

(defun gitid-maybe-expand-on-save ()
  "Expand or update GitId before save if gitid-before-save and in git repo.
If buffer contains $GitId$, expand it. If buffer contains already-expanded
$GitId: ... Exp $, increment version and update date/time."
  (when (and gitid-before-save (buffer-file-name) (gitid--git-dir))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "$GitId$" nil t)
          (gitid-expand)
        (gitid--update-expanded)))))

;;;###autoload
(define-minor-mode gitid-mode
  "Minor mode to expand $GitId$ to RCS-style keyword.
With optional hook: when gitid-before-save is non-nil, expansion
runs before saving."
  :lighter " GitId"
  (if gitid-mode
      (if gitid-before-save
          (add-hook 'before-save-hook #'gitid-maybe-expand-on-save nil t)
        (remove-hook 'before-save-hook #'gitid-maybe-expand-on-save t))
    (remove-hook 'before-save-hook #'gitid-maybe-expand-on-save t)))

(provide 'gitid)
;;; gitid.el ends here
