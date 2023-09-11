;;; adventofcode.el --- An adventofcode client           -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2019  Wang Kai

;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions, tools
;; URL: https://github.com/kaiwk/adventofcode.el
;; Package-Requires: ((emacs "26.1") (dash "2.16.0") (graphql "0.1.1") (spinner "1.7.3") (aio "1.0") (log4e "0.3.3"))
;; Version: 0.1.27

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; adventofcode.el is an unofficial Adventofcode client.
;;
;; Now it implements several API:
;; - Check problems list
;; - Try testcase
;; - Submit code
;;
;; Since most HTTP requests works asynchronously, it won't block Emacs.
;;
;;; Code:
(eval-when-compile
  (require 'let-alist))

(require 'json)
(require 'shr)
(require 'seq)
(require 'subr-x)
(require 'mm-url)
(require 'cl-lib)

(require 'dash)
(require 'graphql)                      ; Some requests of Adventofcode use GraphQL
(require 'aio)
(require 'spinner)
(require 'log4e)

(log4e:deflogger "adventofcode" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                      (error . "error")
                                                      (warn  . "warn")
                                                      (info  . "info")
                                                      (debug . "debug")
                                                      (trace . "trace")))
(setq log4e--log-buffer-adventofcode "*adventofcode-log*")

;;;###autoload
(defun adventofcode-toggle-debug ()
  "Toggle debug."
  (interactive)
  (if (adventofcode--log-debugging-p)
      (progn
        (adventofcode--log-set-level 'info)
        (adventofcode--log-disable-debugging)
        (message "adventofcode disable debug"))
    (progn
      (adventofcode--log-set-level 'debug)
      (adventofcode--log-enable-debugging)
      (message "adventofcode enable debug"))))

(defun adventofcode--install-my-cookie ()
  "Install adventofcode dependencies."
  (let ((async-shell-command-display-buffer t))
    (async-shell-command
     "pip3 install my_cookies"
     (get-buffer-create "*adventofcode-install*"))))

(defun adventofcode--check-deps ()
  "Check if all dependencies installed."
  (if (executable-find "my_cookies")
      t
    (adventofcode--install-my-cookie)
    nil))

(defgroup adventofcode nil
  "A Adventofcode client."
  :prefix 'adventofcode-
  :group 'tools)

(defcustom adventofcode-prefer-tag-display t
  "Whether to display tags by default in the *adventofcode* buffer."
  :group 'adventofcode
  :type 'boolean)

(defcustom adventofcode-prefer-language "python3"
  "Adventofcode programming language.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift."
  :group 'adventofcode
  :type 'string)

(defcustom adventofcode-prefer-sql "mysql"
  "Adventofcode sql implementation.
mysql, mssql, oraclesql."
  :group 'adventofcode
  :type 'string)

(defcustom adventofcode-directory "~/adventofcode"
  "Directory to save solutions."
  :group 'adventofcode
  :type 'string)

(defcustom adventofcode-save-solutions nil
  "If it's t, save adventofcode solutions to `adventofcode-directory'."
  :group 'adventofcode
  :type 'boolean)

(defcustom adventofcode-focus t
  "When execute `adventofcode', always delete other windows."
  :group 'adventofcode
  :type 'boolean)

(cl-defstruct adventofcode-user
  "A Adventofcode User.
The object with following attributes:
:username String
:solved   Number
:easy     Number
:medium   Number
:hard     Number"
  username solved easy medium hard)

(cl-defstruct adventofcode-problem
  "A single Adventofcode problem.
:status     String
:id         Number
:backend-id Number
:title      String
:acceptance String
:difficulty Number {1,2,3}
:paid-only  Boolean {t|nil}
:tags       List"
  status id backend-id title acceptance
  difficulty paid-only tags)

(cl-defstruct adventofcode-problems
  "All Adventofcode problems, the problems can filtered by tag.
:num      Number
:tag      String
:problems List[adventofcode--problems]"
  num tag problems)

(defvar adventofcode--user (make-adventofcode-user)
  "A User object.")

(defvar adventofcode--problems (make-adventofcode-problems)
  "Problems object with a list of `adventofcode-problem'.")

(defvar adventofcode--all-tags nil
  "All problems tags.")

(defvar adventofcode--problem-titles nil
  "Problem titles that have been open in solving layout.")

(defvar adventofcode--display-tags adventofcode-prefer-tag-display
  "(Internal) Whether tags are displayed the *adventofcode* buffer.")

(defvar adventofcode--display-paid nil
  "(Internal) Whether paid problems are displayed the *adventofcode* buffer.")

(defvar adventofcode--lang adventofcode-prefer-language
  "Adventofcode programming language or sql for current problem internally.
Default is programming language.")

(defconst adventofcode--lang-suffixes
  '(("c" . ".c") ("cpp" . ".cpp") ("csharp" . ".cs")
    ("golang" . ".go") ("java" . ".java") ("javascript" . ".js")
    ("typescript" . ".ts") ("kotlin" . ".kt") ("php" . ".php")
    ("python" . ".py") ("python3" . ".py") ("ruby" . ".rb")
    ("rust" . ".rs") ("scala" . ".scala") ("swift" . ".swift")
    ("mysql" . ".sql") ("mssql" . ".sql") ("oraclesql" . ".sql"))
  "Adventofcode programming language suffixes.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift, mysql, mssql, oraclesql.")

(defvar adventofcode--filter-regex nil "Filter rows by regex.")
(defvar adventofcode--filter-tag nil "Filter rows by tag.")
(defvar adventofcode--filter-difficulty nil
  "Filter rows by difficulty, it can be \"easy\", \"medium\" and \"hard\".")

(defconst adventofcode--all-difficulties '("easy" "medium" "hard"))
(defconst adventofcode--paid "•" "Paid mark.")
(defconst adventofcode--checkmark "✓" "Checkmark for accepted problem.")
(defconst adventofcode--buffer-name             "*adventofcode*")

(defconst adventofcode--retry-times 20 "`adventofcode-try' or `adventofcode-submit' retry times.")

(defface adventofcode-paid-face
  '((t (:foreground "gold")))
  "Face for `adventofcode--paid'."
  :group 'adventofcode)

(defface adventofcode-checkmark-face
  '((t (:foreground "#5CB85C")))
  "Face for `adventofcode--checkmark'."
  :group 'adventofcode)

(defface adventofcode-easy-face
  '((t (:foreground "#5CB85C")))
  "Face for easy problems."
  :group 'adventofcode)

(defface adventofcode-medium-face
  '((t (:foreground "#F0AD4E")))
  "Face for medium problems."
  :group 'adventofcode)

(defface adventofcode-hard-face
  '((t (:foreground "#D9534E")))
  "Face for hard problems."
  :group 'adventofcode)

(defface adventofcode-accepted-face
  '((t (:foreground "#228b22")))
  "Face for submission accepted."
  :group 'adventofcode)

(defface adventofcode-error-face
  '((t (:foreground "#dc143c")))
  "Face for submission compile error, runtime error and TLE."
  :group 'adventofcode)

;;; Login
;; URL
(defconst adventofcode--domain    "adventofcode.com")
(defconst adventofcode--url-base  "https://adventofcode.com")
(defconst adventofcode--url-login (concat adventofcode--url-base "/accounts/login"))

;; Cookie key name
(defconst adventofcode--cookie-csrftoken "csrftoken")
(defconst adventofcode--cookie-session "ADVENTOFCODE_SESSION")

;; Header
(defconst adventofcode--User-Agent       '("User-Agent" .
                                       "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:66.0) Gecko/20100101 Firefox/66.0"))
(defconst adventofcode--X-Requested-With '("X-Requested-With" . "XMLHttpRequest"))
(defconst adventofcode--X-CSRFToken      "X-CSRFToken")
(defconst adventofcode--Content-Type     '("Content-Type" . "application/json"))

;; API URL
(defconst adventofcode--url-api                 (concat adventofcode--url-base "/api"))
(defconst adventofcode--url-graphql             (concat adventofcode--url-base "/graphql"))
(defconst adventofcode--url-all-problems        (concat adventofcode--url-api "/problems/all/"))
(defconst adventofcode--url-all-tags            (concat adventofcode--url-base "/problems/api/tags"))
(defconst adventofcode--url-daily-challenge
  (concat
   "query questionOfToday { activeDailyCodingChallengeQuestion {"
   " link question { status title titleSlug qid: questionFrontendId } } }"))
;; submit
(defconst adventofcode--url-submit              (concat adventofcode--url-base "/problems/%s/submit/"))
(defconst adventofcode--url-problems-submission (concat adventofcode--url-base "/problems/%s/submissions/"))
(defconst adventofcode--url-check-submission    (concat adventofcode--url-base "/submissions/detail/%s/check/"))
;; try testcase
(defconst adventofcode--url-try                 (concat adventofcode--url-base "/problems/%s/interpret_solution/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun adventofcode--to-list (vec)
  "Convert VEC to list."
  (append vec '()))

(defun adventofcode--referer (value)
  "It will return an alist as the HTTP Referer Header.
VALUE should be the referer."
  (cons "Referer" value))

(defun adventofcode--maybe-csrf-token ()
  "Return csrf token if it exists, otherwise return nil."
  (if-let ((cookie (seq-find
                    (lambda (item)
                      (string= (aref item 1)
                               adventofcode--cookie-csrftoken))
                    (url-cookie-retrieve adventofcode--domain "/" t))))
      (aref cookie 2)))

(aio-defun adventofcode--csrf-token ()
  "Return csrf token."
  (unless (adventofcode--maybe-csrf-token)
    (aio-await (aio-url-retrieve adventofcode--url-login)))
  (adventofcode--maybe-csrf-token))

(defun adventofcode--login-p ()
  "Whether user is login."
  (let ((username (adventofcode-user-username adventofcode--user)))
    (and username
         (not (string-empty-p username))
         (seq-find
          (lambda (item)
            (string= (aref item 1)
                     adventofcode--cookie-session))
          (url-cookie-retrieve adventofcode--domain "/" t)))))

(defun adventofcode--slugify-title (title)
  "Make TITLE a slug title.
Such as 'Two Sum' will be converted to 'two-sum'. 'Pow(x, n)' will be 'powx-n'"
  (let* ((str1 (replace-regexp-in-string "[\s-]+" "-" (downcase title)))
         (res (replace-regexp-in-string "[(),]" "" str1)))
    res))

(defun adventofcode--replace-in-buffer (regex to)
  "Replace string matched REGEX in `current-buffer' to TO."
  (with-current-buffer (current-buffer)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward regex (point-max) t)
          (replace-match to))))))

(defun adventofcode--problem-link (title)
  "Generate problem link from TITLE."
  (concat adventofcode--url-base "/problems/" (adventofcode--slugify-title title)))

(defun adventofcode--stringify-difficulty (difficulty)
  "Stringify DIFFICULTY level (number) to 'easy', 'medium' or 'hard'."
  (let ((easy-tag "easy")
        (medium-tag "medium")
        (hard-tag "hard"))
    (cond
     ((eq 1 difficulty)
      (adventofcode--add-font-lock easy-tag 'adventofcode-easy-face))
     ((eq 2 difficulty)
      (adventofcode--add-font-lock medium-tag 'adventofcode-medium-face))
     ((eq 3 difficulty)
      (adventofcode--add-font-lock hard-tag 'adventofcode-hard-face)))))

(defun adventofcode--add-font-lock (str face)
  (prog1 str
    (put-text-property
     0 (length str)
     'font-lock-face face str)))

(defun adventofcode--detail-buffer-name (problem-id)
  "Detail buffer name."
  (format "*adventofcode-detail-%s*" problem-id))

(defun adventofcode--testcase-buffer-name (problem-id)
  "Testcase buffer name."
  (format "*adventofcode-testcase-%s*" problem-id))

(defun adventofcode--result-buffer-name (problem-id)
  "Result buffer name."
  (format "*adventofcode-result-%s*" problem-id))

(defun adventofcode--maybe-focus ()
  (if adventofcode-focus (delete-other-windows)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Adventofcode API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(aio-defun adventofcode--login ()
  "Steal Adventofcode login session from local browser.
It also cleans Adventofcode cookies in `url-cookie-file'."
  (adventofcode--loading-mode t)
  (ignore-errors (url-cookie-delete-cookies adventofcode--domain))
  (aio-await (adventofcode--csrf-token))    ;knock knock, whisper me the mysterious information
  (let* ((my-cookies (executable-find "my_cookies"))
         (my-cookies-output (shell-command-to-string my-cookies))
         (cookies-list (seq-filter
                        (lambda (s) (not (string-empty-p s)))
                        (split-string my-cookies-output "\n")))
         (cookies-pairs (seq-map
                         (lambda (s) (split-string s))
                         cookies-list))
         (adventofcode-session (cadr (assoc adventofcode--cookie-session cookies-pairs)))
         (adventofcode-csrftoken (cadr (assoc "csrftoken" cookies-pairs))))
    (adventofcode--debug "login session: %s" adventofcode-session)
    (adventofcode--debug "login csrftoken: %s" adventofcode-csrftoken)
    (url-cookie-store adventofcode--cookie-session adventofcode-session nil adventofcode--domain "/" t)
    (url-cookie-store "csrftoken" adventofcode-csrftoken nil adventofcode--domain "/" t))
  (adventofcode--loading-mode -1))

(aio-defun adventofcode--api-fetch-all-tags ()
  "Fetch all problems' tags."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(,adventofcode--User-Agent
            ,adventofcode--X-Requested-With
            ,(adventofcode--referer adventofcode--url-login)))
         (result (aio-await (aio-url-retrieve adventofcode--url-all-tags))))
    (with-current-buffer (cdr result)
      (goto-char url-http-end-of-headers)
      (json-read))))

(aio-defun adventofcode--api-fetch-user-and-problems ()
  "Fetch user and problems info."
  (if adventofcode--loading-mode
      (message "Adventofcode has been refreshing...")
    (adventofcode--loading-mode t)
    (let ((url-request-method "GET")
          (url-request-extra-headers
           `(,adventofcode--User-Agent
             ,adventofcode--X-Requested-With
             ,(adventofcode--referer adventofcode--url-login)))
          (result (aio-await (aio-url-retrieve adventofcode--url-all-problems))))
      (adventofcode--loading-mode -1)
      (if-let ((error-info (plist-get (car result) :error)))
          (progn
            (switch-to-buffer (cdr result))
            (adventofcode--warn "Adventofcode fetch user and problems failed: %S" error-info))
        (with-current-buffer (cdr result)
          (goto-char url-http-end-of-headers)
          (json-read))))))

(defun adventofcode--problem-graphql-params (operation &optional vars)
  "Construct a GraphQL parameter.
OPERATION and VARS are Adventofcode GraphQL parameters."
  (list
   (cons "operationName" operation)
   (cons "query"
         (graphql-query
          questionData
          (:arguments
           (($titleSlug . String!))
           (question
            :arguments
            ((titleSlug . ($ titleSlug)))
            likes
            dislikes
            content
            sampleTestCase
            (topicTags slug)
            (codeSnippets langSlug code)))))
   (if vars (cons "variables" vars))))

(aio-defun adventofcode--api-fetch-problem (title)
  "Fetch single problem.
TITLE is a problem's title.
Return a object with following attributes:
:likes     Number
:dislikes  Number
:content   String
:topicTags String"
  (let* ((slug-title (adventofcode--slugify-title title))
         (url-request-method "POST")
         (url-request-extra-headers
          `(,adventofcode--User-Agent ,adventofcode--Content-Type))
         (url-request-data
          (json-encode (adventofcode--problem-graphql-params
                        "questionData"
                        (list (cons "titleSlug" slug-title)))))
         (result (aio-await (aio-url-retrieve adventofcode--url-graphql))))
    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (switch-to-buffer (cdr result))
          (adventofcode--warn "Adventofcode fetch problem ERROR: %S" error-info))
      (with-current-buffer (cdr result)
        (goto-char url-http-end-of-headers)
        (alist-get 'question (alist-get 'data (json-read)))))))

(aio-defun adventofcode--api-try (problem-id slug-title code testcase)
  "Test CODE for problem which has PROBLEM-ID and SLUG-TITLE with TESTCASE."
  (adventofcode--debug "adventofcode try slug-title: %s, problem-id: %s" slug-title problem-id)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(,adventofcode--User-Agent
            ,adventofcode--Content-Type
            ,(adventofcode--referer (format
                                 adventofcode--url-problems-submission
                                 slug-title))
            ,(cons adventofcode--X-CSRFToken (aio-await (adventofcode--csrf-token)))))
         (url-request-data
          (json-encode
           `((data_input  . ,testcase)
             (judge_type  . "small")
             (lang        . ,adventofcode--lang)
             (question_id . ,problem-id)
             (typed_code  . ,code)))))
    (aio-await (aio-url-retrieve (format adventofcode--url-try slug-title)))))

(aio-defun adventofcode--api-submit (problem-id slug-title code)
  "Submit CODE for problem which has PROBLEM-ID and SLUG-TITLE."
  (adventofcode--debug "adventofcode submit slug-title: %s, problem-id: %s" slug-title problem-id)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(,adventofcode--User-Agent
            ,(adventofcode--referer (format
                                 adventofcode--url-problems-submission
                                 slug-title))
            ,adventofcode--Content-Type
            ,(cons adventofcode--X-CSRFToken (aio-await (adventofcode--csrf-token)))))
         (url-request-data
          (json-encode `((lang . ,adventofcode--lang)
                         (question_id . ,problem-id)
                         (typed_code . ,code)))))
    (aio-await (aio-url-retrieve (format adventofcode--url-submit slug-title)))))

(aio-defun adventofcode--api-check-submission (submission-id slug-title)
  "Polling to check submission detail.
After each submission, either try testcase or submit, Adventofcode
returns a SUBMISSION-ID. With the SUBMISSION-ID, client will poll
for the submission detail. SLUG-TITLE is a slugified problem
title. Return response data if submission success, otherwise
nil."
  (adventofcode--loading-mode t)
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(,adventofcode--User-Agent
            ,(adventofcode--referer (format adventofcode--url-problems-submission slug-title))
            ,(cons adventofcode--X-CSRFToken (aio-await (adventofcode--csrf-token)))))
         (result (aio-await (aio-url-retrieve (format adventofcode--url-check-submission submission-id)))))
    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (adventofcode--loading-mode -1)
          (switch-to-buffer (cdr result))
          (adventofcode--warn "Adventofcode check submission failed: %S" error-info))
      (with-current-buffer (cdr result)
        (let ((submission-res
               (progn (goto-char url-http-end-of-headers)
                      (json-read))))
          (if (equal (alist-get 'state submission-res) "SUCCESS")
              submission-res))))))

(defun adventofcode--set-user-and-problems (user-and-problems)
  "Set `adventofcode--user' and `adventofcode--problems'.
If user isn't login, only `adventofcode--problems' will be set.
USER-AND-PROBLEMS is an alist comes from
`adventofcode--url-all-problems'."
  ;; user
  (let-alist user-and-problems
    (setf (adventofcode-user-username adventofcode--user) .user_name
          (adventofcode-user-solved adventofcode--user) .num_solved
          (adventofcode-user-easy adventofcode--user) .ac_easy
          (adventofcode-user-medium adventofcode--user) .ac_medium
          (adventofcode-user-hard adventofcode--user) .ac_hard)
    (adventofcode--debug "set user: %s, solved %s in %s problems" .user_name .num_solved .num_total)
    ;; problem list
    (setf (adventofcode-problems-num adventofcode--problems) .num_total
          (adventofcode-problems-tag adventofcode--problems) "all")
    (setf (adventofcode-problems-problems adventofcode--problems)
          (let* ((len .num_total)
                 (problems nil))
            (dotimes (i len problems)
              (let-alist (aref .stat_status_pairs i)
                (adventofcode--debug "frontend_question_id: %s, question_id: %s, title: %s"
                                 .stat.frontend_question_id .stat.question_id .stat.question__title)
                (push (make-adventofcode-problem
                       :status .status
                       :id .stat.frontend_question_id
                       :backend-id .stat.question_id
                       :title .stat.question__title
                       :acceptance (format
                                    "%.1f%%"
                                    (* 100
                                       (/ (float .stat.total_acs)
                                          .stat.total_submitted)))
                       :difficulty .difficulty.level
                       :paid-only (eq .paid_only t))
                      problems)))))))

(defun adventofcode--set-tags (all-tags)
  "Set `adventofcode--all-tags' and `adventofcode--problems' with ALL-TAGS."
  (let ((tags-table (make-hash-table :size 2000)))
    (let-alist all-tags
      (dolist (topic (adventofcode--to-list .topics))
        (let-alist topic
          ;; set adventofcode--all-tags
          (unless (member .slug adventofcode--all-tags)
            (push .slug adventofcode--all-tags))
          ;; tags-table cache with backend-id
          (dolist (id (adventofcode--to-list .questions))
            (let ((tags (gethash id tags-table)))
              (setf (gethash id tags-table) (cons .slug tags)))))))
    ;; set problems tags with tags-table
    (dolist (problem (adventofcode-problems-problems adventofcode--problems))
      (let ((backend-id (adventofcode-problem-backend-id problem)))
        (setf (adventofcode-problem-tags problem) (gethash backend-id tags-table))))))

(defun adventofcode--problems-rows ()
  "Generate tabulated list rows from `adventofcode--problems'.
Return a list of rows, each row is a vector:
\([<checkmark> <position> <title> <acceptance> <difficulty>] ...)"
  (let ((problems (adventofcode-problems-problems adventofcode--problems))
        (easy-tag "easy")
        (medium-tag "medium")
        (hard-tag "hard")
        rows)
    (dolist (p problems (reverse rows))
      (if (or adventofcode--display-paid
              (not (adventofcode-problem-paid-only p)))
          (setq rows
                (cons
                 (vector
                  ;; status
                  (if (equal (adventofcode-problem-status p) "ac")
                      (adventofcode--add-font-lock adventofcode--checkmark 'adventofcode-checkmark-face)
                    " ")
                  ;; id
                  (number-to-string (adventofcode-problem-id p))
                  ;; title
                  (concat
                   (adventofcode-problem-title p)
                   " "
                   (if (eq (adventofcode-problem-paid-only p) t)
                       (adventofcode--add-font-lock adventofcode--paid 'adventofcode-paid-face)
                     " "))
                  ;; acceptance
                  (adventofcode-problem-acceptance p)
                  ;; difficulty
                  (adventofcode--stringify-difficulty (adventofcode-problem-difficulty p))
                  ;; tags
                  (if adventofcode--display-tags (string-join (adventofcode-problem-tags p) ", ") ""))
                 rows))))))

(defun adventofcode--row-tags (row)
  "Get tags from ROW."
  (aref row 5))

(defun adventofcode--row-difficulty (row)
  "Get difficulty from ROW."
  (aref row 4))

(defun adventofcode--filter (rows)
  "Filter ROWS by `adventofcode--filter-regex', `adventofcode--filter-tag' and `adventofcode--filter-difficulty'."
  (seq-filter
   (lambda (row)
     (and
      (if adventofcode--filter-regex
          (let ((title (aref row 2)))
            (string-match-p adventofcode--filter-regex title))
        t)
      (if adventofcode--filter-tag
          (let ((tags (split-string (adventofcode--row-tags row) ", ")))
            (member adventofcode--filter-tag tags))
        t)
      (if adventofcode--filter-difficulty
          (let ((difficulty (adventofcode--row-difficulty row)))
            (string= difficulty adventofcode--filter-difficulty))
        t)))
   rows))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; User Command ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun adventofcode-reset-filter ()
  "Reset filter."
  (interactive)
  (setq adventofcode--filter-regex nil)
  (setq adventofcode--filter-tag nil)
  (setq adventofcode--filter-difficulty nil)
  (adventofcode-refresh))

(defun adventofcode-set-filter-regex (regex)
  "Set `adventofcode--filter-regex' as REGEX and refresh."
  (interactive "sSearch: ")
  (setq adventofcode--filter-regex regex)
  (adventofcode-refresh))

(defun adventofcode-set-filter-tag ()
  "Set `adventofcode--filter-tag' from `adventofcode--all-tags' and refresh."
  (interactive)
  (setq adventofcode--filter-tag
        (completing-read "Tags: " adventofcode--all-tags))
  (adventofcode-refresh))

(defun adventofcode-set-prefer-language ()
  "Set `adventofcode-prefer-language' from `adventofcode--lang-suffixes' and refresh."
  (interactive)
  (setq adventofcode-prefer-language
        (completing-read "Language: " adventofcode--lang-suffixes))
  (adventofcode-refresh))

(defun adventofcode-set-filter-difficulty ()
  "Set `adventofcode--filter-difficulty' from `adventofcode--all-difficulties' and refresh."
  (interactive)
  (setq adventofcode--filter-difficulty
        (completing-read "Difficulty: " adventofcode--all-difficulties))
  (adventofcode-refresh))

(defun adventofcode-toggle-tag-display ()
  "Toggle `adventofcode--display-tags` and refresh"
  (interactive)
  (setq adventofcode--display-tags (not adventofcode--display-tags))
  (adventofcode-refresh))

(defun adventofcode-toggle-paid-display ()
  "Toggle `adventofcode--display-paid` and refresh"
  (interactive)
  (setq adventofcode--display-paid (not adventofcode--display-paid))
  (adventofcode-refresh))

(defun adventofcode--make-tabulated-headers (header-names rows)
  "Calculate headers width.
Column width calculated by picking the max width of every cell
under that column and the HEADER-NAMES. HEADER-NAMES are a list
of header name, ROWS are a list of vector, each vector is one
row."
  (let ((widths
         (seq-reduce
          (lambda (acc row)
            (cl-mapcar
             (lambda (a col) (max a (length col)))
             acc
             (append row '())))
          rows
          (seq-map #'length header-names))))
    (vconcat
     (cl-mapcar
      (lambda (col size) (list col size nil))
      header-names widths))))

(defun adventofcode-refresh ()
  "Make `tabulated-list-entries'."
  (interactive)
  (let* ((header-names (append '(" " "#" "Problem" "Acceptance" "Difficulty")
                               (if adventofcode--display-tags '("Tags"))))
         (rows (adventofcode--filter (adventofcode--problems-rows)))
         (headers (adventofcode--make-tabulated-headers header-names rows)))
    (with-current-buffer (get-buffer-create adventofcode--buffer-name)
      (adventofcode--problems-mode)
      (setq tabulated-list-format headers)
      (setq tabulated-list-entries
            (cl-mapcar
             (lambda (i x) (list i x))
             (number-sequence 0 (1- (length rows)))
             rows))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (adventofcode--loading-mode -1))))

(aio-defun adventofcode-refresh-fetch ()
  "Refresh problems and update `tabulated-list-entries'."
  (interactive)
  (if-let ((users-and-problems (aio-await (adventofcode--api-fetch-user-and-problems)))
           (all-tags (aio-await (adventofcode--api-fetch-all-tags))))
      (progn
        (adventofcode--set-user-and-problems users-and-problems)
        (adventofcode--set-tags all-tags))
    (adventofcode--warn "Adventofcode parse user and problems failed"))
  (setq adventofcode--display-tags adventofcode-prefer-tag-display)
  (adventofcode-reset-filter)
  (adventofcode-refresh))

(aio-defun adventofcode--async ()
  "Show adventofcode problems buffer."
  (if (get-buffer adventofcode--buffer-name)
      (switch-to-buffer adventofcode--buffer-name)
    (unless (adventofcode--login-p)
      (aio-await (adventofcode--login)))
    (aio-await (adventofcode-refresh-fetch))
    (switch-to-buffer adventofcode--buffer-name))
  (adventofcode--maybe-focus))

;;;###autoload
(defun adventofcode ()
  "A wrapper for `adventofcode--async', because emacs-aio can not be autoloaded.
see: https://github.com/skeeto/emacs-aio/issues/3."
  (interactive)
  (if (adventofcode--check-deps)
      (adventofcode--async)
    (message "installing adventofcode dependencies...")))

;;;###autoload(autoload 'adventofcode-daily "adventofcode" nil t)
(aio-defun adventofcode-daily ()
  "Open the daily challenge."
  (interactive)
  (unless (adventofcode--login-p)
    (aio-await (adventofcode)))
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(,adventofcode--User-Agent
            ,adventofcode--Content-Type
            ,(adventofcode--referer adventofcode--url-login)
            ,(cons adventofcode--X-CSRFToken (adventofcode--maybe-csrf-token))))
         (url-request-data
          (json-encode
           `((operationName . "questionOfToday")
             (query . ,adventofcode--url-daily-challenge)))))
    (with-current-buffer (url-retrieve-synchronously adventofcode--url-graphql)
      (goto-char url-http-end-of-headers)
      (let-alist (json-read)
        (let ((qid .data.activeDailyCodingChallengeQuestion.question.qid))
          (adventofcode-show-problem (string-to-number qid)))))))

(defun adventofcode--buffer-content (buf)
  "Get content without text properties of BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun adventofcode--get-slug-title (code-buf)
  "Get slug title before try or submit with CODE-BUF.
Adventofcode require slug-title as the request parameters."
  (with-current-buffer code-buf
    (if adventofcode-save-solutions
        (file-name-base (cadr (split-string (buffer-name) "_")))
      (file-name-base (buffer-name)))))

(aio-defun adventofcode-try ()
  "Asynchronously test the code using customized testcase."
  (interactive)
  (adventofcode--loading-mode t)
  (let* ((code-buf (current-buffer))
         (slug-title (adventofcode--get-slug-title code-buf))
         (problem (adventofcode--get-problem slug-title))
         (problem-id (adventofcode-problem-id problem))
         (backend-id (adventofcode-problem-backend-id problem))
         (testcase-buf (get-buffer (adventofcode--testcase-buffer-name problem-id)))
         (result (aio-await (adventofcode--api-try backend-id slug-title
                                               (adventofcode--buffer-content code-buf)
                                               (adventofcode--buffer-content testcase-buf)))))
    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (switch-to-buffer (cdr result))
          (adventofcode--warn "Adventofcode try failed: %S" error-info))
      (let ((data (with-current-buffer (cdr result)
                    (goto-char url-http-end-of-headers)
                    (json-read)))
            (result-buf (get-buffer (adventofcode--result-buffer-name problem-id))))
        (let-alist data
          (with-current-buffer result-buf
            (erase-buffer)
            (insert (concat "Your input:\n" .test_case "\n\n")))
          ;; poll interpreted
          (let ((actual_res (aio-await (adventofcode--api-check-submission .interpret_id slug-title)))
                (retry-times 0))
            (while (and (not actual_res) (< retry-times adventofcode--retry-times))
              (aio-await (aio-sleep 0.5))
              (setq actual_res (aio-await (adventofcode--api-check-submission .interpret_id slug-title)))
              (setq retry-times (1+ retry-times)))
            (if (< retry-times adventofcode--retry-times)
                (let-alist actual_res
                  (with-current-buffer result-buf
                    (goto-char (point-max))
                    (cond
                     ((eq .status_code 10)
                      (insert "Output:\n")
                      (dotimes (i (length .code_answer))
                        (insert (aref .code_answer i))
                        (insert "\n"))
                      (insert "\n")
                      (insert "Expected:\n")
                      (dotimes (i (length .expected_code_answer))
                        (insert (aref .expected_code_answer i))
                        (insert "\n"))
                      (insert "\n"))
                     ((eq .status_code 14)
                      (insert .status_msg))
                     ((eq .status_code 15)
                      (insert (adventofcode--add-font-lock .status_msg 'adventofcode-error-face))
                      (insert "\n\n")
                      (insert .full_runtime_error))
                     ((eq .status_code 20)
                      (insert (adventofcode--add-font-lock .status_msg 'adventofcode-error-face))
                      (insert "\n\n")
                      (insert .full_compile_error)))
                    (when (> (length .code_output) 0)
                      (insert "\n\n")
                      (insert "Code output:\n")
                      (dolist (item (append .code_output nil))
                        (insert (concat item "\n"))))
                    (insert "\n\n")))
              (adventofcode--warn "Adventofcode try timeout.")))
          (adventofcode--loading-mode -1))))))

(defun adventofcode--solving-window-layout ()
  "Specify layout for solving problem.
+---------------+----------------+
|               |                |
|               |     Detail     |
|               |                |
|               +----------------+
|     Code      |   Customize    |
|               |   Testcases    |
|               +----------------+
|               |Submit/Testcases|
|               |    Result      |
+---------------+----------------+"
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (split-window-below)
  (other-window 1)
  (split-window-below)
  (other-window -1)
  (other-window -1))

(defun adventofcode--display-result (buffer &optional alist)
  "Display function for Adventofcode result.
BUFFER is used to show Adventofcode result. ALIST is a combined alist
specified in `display-buffer-alist'."
  (let ((window (window-next-sibling
                 (window-next-sibling
                  (window-top-child
                   (window-next-sibling
                    (window-left-child
                     (frame-root-window))))))))
    (set-window-buffer window buffer)
    window))

(defun adventofcode--display-testcase (buffer &optional alist)
  "Display function for Adventofcode testcase.
BUFFER is used to show Adventofcode testcase. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-next-sibling
                 (window-top-child
                  (window-next-sibling
                   (window-left-child
                    (frame-root-window)))))))
    (set-window-buffer window buffer)
    window))

(defun adventofcode--display-detail (buffer &optional alist)
  "Display function for Adventofcode detail.
BUFFER is used to show Adventofcode detail. ALIST is a combined alist
specified in `display-buffer-alist'."
  (let ((window (window-top-child
                 (window-next-sibling
                  (window-left-child
                   (frame-root-window))))))
    (set-window-buffer window buffer)
    window))

(defun adventofcode--display-code (buffer &optional alist)
  "Display function for Adventofcode code.
BUFFER is the one to show Adventofcode code. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-left-child (frame-root-window))))
    (set-window-buffer window buffer)
    window))

(defun adventofcode--show-submission-result (problem-id submission-detail)
  "Show error info in `adventofcode--result-buffer-name' based on status code.
Error info comes from SUBMISSION-DETAIL.

STATUS_CODE has following possible value:

- 10: Accepted
- 11: Wrong Anwser
- 14: Time Limit Exceeded
- 15: Runtime Error.  full_runtime_error
- 20: Compile Error.  full_compile_error"
  (let-alist submission-detail
    (with-current-buffer (get-buffer-create (adventofcode--result-buffer-name problem-id))
      (erase-buffer)
      (font-lock-mode +1)
      (cond
       ((eq .status_code 10)
        (insert (format "Status: %s\n\n"
                        (adventofcode--add-font-lock
                         (format "%s (%s/%s)" .status_msg .total_correct .total_testcases)
                         'adventofcode-accepted-face)))
        (insert (format "Runtime: %s, faster than %.2f%% of %s submissions.\n\n"
                        .status_runtime .runtime_percentile .pretty_lang))
        (insert (format "Memory Usage: %s, less than %.2f%% of %s submissions."
                        .status_memory .memory_percentile .pretty_lang)))
       ((eq .status_code 11)
        (insert (format "Status: %s\n\n"
                        (adventofcode--add-font-lock
                         (format "%s (%s/%s)" .status_msg .total_correct .total_testcases)
                         'adventofcode-error-face)))
        (insert (format "Test Case: \n%s\n\n" .input))
        (insert (format "Answer: %s\n\n" .code_output))
        (insert (format "Expected Answer: %s\n\n" .expected_output))
        (unless (string-empty-p .std_output)
          (insert (format "Stdout: \n%s\n" .std_output))))
       ((eq .status_code 14)
        (insert (format "Status: %s" (adventofcode--add-font-lock .status_msg 'adventofcode-error-face)))
        (insert "\n"))
       ((eq .status_code 15)
        (insert (format "Status: %s" (adventofcode--add-font-lock .status_msg 'adventofcode-error-face)))
        (insert "\n\n")
        (insert (format .full_runtime_error)))
       ((eq .status_code 20)
        (insert (format "Status: %s" (adventofcode--add-font-lock .status_msg 'adventofcode-error-face)))
        (insert "\n\n")
        (insert (format .full_compile_error))))
      (display-buffer (current-buffer)
                      '((display-buffer-reuse-window
                         adventofcode--display-result)
                        (reusable-frames . visible))))))

(aio-defun adventofcode-submit ()
  "Asynchronously submit the code and show result."
  (interactive)
  (adventofcode--loading-mode t)
  (let* ((code-buf (current-buffer))
         (code (adventofcode--buffer-content code-buf))
         (slug-title (adventofcode--get-slug-title code-buf))
         (problem (adventofcode--get-problem slug-title))
         (problem-id (adventofcode-problem-id problem))
         (backend-id (adventofcode-problem-backend-id problem))
         (result (aio-await (adventofcode--api-submit backend-id slug-title code))))
    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (adventofcode--loading-mode -1)
          (switch-to-buffer (cdr result))
          (adventofcode--warn "Adventofcode check submit failed: %S" error-info))
      (let* ((resp
              (with-current-buffer (cdr result)
                (progn (goto-char url-http-end-of-headers)
                       (json-read))))
             (submission-id (alist-get 'submission_id resp))
             (submission-res (aio-await (adventofcode--api-check-submission submission-id slug-title)))
             (retry-times 0))
        ;; poll submission result
        (while (and (not submission-res) (< retry-times adventofcode--retry-times))
          (aio-await (aio-sleep 0.5))
          (setq submission-res (aio-await (adventofcode--api-check-submission submission-id slug-title)))
          (setq retry-times (1+ retry-times)))
        (if (< retry-times adventofcode--retry-times)
            (adventofcode--show-submission-result problem-id submission-res)
          (adventofcode--warn "Adventofcode submit timeout."))
        (adventofcode--loading-mode -1)))))

(defun adventofcode--show-problem (problem problem-info)
  "Show the detail of PROBLEM, whose meta data is PROBLEM-INFO.
Use `shr-render-buffer' to render problem detail. This action
will show the detail in other window and jump to it."
  (let* ((problem-id (adventofcode-problem-id problem-info))
         (title (adventofcode-problem-title problem-info))
         (difficulty-level (adventofcode-problem-difficulty problem-info))
         (difficulty (adventofcode--stringify-difficulty difficulty-level))
         (buf-name (adventofcode--detail-buffer-name problem-id))
         (html-margin "&nbsp;&nbsp;&nbsp;&nbsp;"))
    (adventofcode--debug "select title: %s" title)
    (adventofcode--maybe-focus)
    (let-alist problem
      (when (get-buffer buf-name)
        (kill-buffer buf-name))
      (with-temp-buffer
        (insert (concat "<h1>" (number-to-string problem-id) ". " title "</h1>"))
        (insert (concat (capitalize difficulty) html-margin
                        "likes: " (number-to-string .likes) html-margin
                        "dislikes: " (number-to-string .dislikes)))
        (insert .content)
        (setq shr-current-font t)
        (adventofcode--replace-in-buffer "" "")
        ;; NOTE: shr.el can't render "https://xxxx.png", so we use "http"
        (adventofcode--replace-in-buffer "https" "http")
        (shr-render-buffer (current-buffer)))
      (with-current-buffer "*html*"
        (save-match-data
          (re-search-forward "dislikes: .*" nil t)
          (insert (make-string 4 ?\s))
          (insert-text-button "Solve it"
                              'action (lambda (btn)
                                        (adventofcode--start-coding problem problem-info))
                              'help-echo "Solve the problem.")
          (insert (make-string 4 ?\s))
          (insert-text-button "Link"
                              'action (lambda (btn)
                                        (browse-url (adventofcode--problem-link title)))
                              'help-echo "Open the problem in browser.")
          (insert (make-string 4 ?\s))
          (insert-text-button "Solution"
                              'action (lambda (btn)
                                        (browse-url (concat (adventofcode--problem-link title) "/solution")))
                              'help-echo "Open the problem solution page in browser."))
        (rename-buffer buf-name)
        (adventofcode--problem-detail-mode)
        (switch-to-buffer (current-buffer))))))

(aio-defun adventofcode-show-problem (problem-id)
  "Show the detail of problem with id PROBLEM-ID.
Get problem by id and use `shr-render-buffer' to render problem
detail. This action will show the detail in other window and jump
to it."
  (interactive (list (read-number "Show problem by problem id: "
                                  (adventofcode--get-current-problem-id))))
  (let* ((problem-info (adventofcode--get-problem-by-id problem-id))
         (title (adventofcode-problem-title problem-info))
         (problem (aio-await (adventofcode--api-fetch-problem title))))
    (adventofcode--show-problem problem problem-info)))

(defun adventofcode-show-problem-by-slug (slug-title)
  "Show the detail of problem with slug title.
This function will work after first run M-x adventofcode. Get problem
by id and use `shr-render-buffer' to render problem detail. This
action will show the detail in other window and jump to it.

It can be used in org-link elisp:(adventofcode-show-problem-by-slug \"3sum\")."
  (interactive (list (read-number "Show problem by problem id: "
                                  (adventofcode--get-current-problem-id))))
  (let* ((problem (seq-find (lambda (p)
                              (equal slug-title
                                     (adventofcode--slugify-title
                                      (adventofcode-problem-title p))))
                            (adventofcode-problems-problems adventofcode--problems)))
         (problem-id (adventofcode-problem-id problem))
         (problem-info (adventofcode--get-problem-by-id problem-id))
         (title (adventofcode-problem-title problem-info))
         (problem  (adventofcode--api-fetch-problem title)))
    (adventofcode-show-problem problem-id)))

(defun adventofcode-show-current-problem ()
  "Show current problem's detail.
Call `adventofcode-show-problem' on the current problem id. This
action will show the detail in other window and jump to it."
  (interactive)
  (adventofcode-show-problem (adventofcode--get-current-problem-id)))

(aio-defun adventofcode-view-problem (problem-id)
  "View problem by PROBLEM-ID while staying in `LC Problems' window.
Similar with `adventofcode-show-problem', but instead of jumping to
the detail window, this action will jump back in `LC Problems'."
  (interactive (list (read-number "View problem by problem id: "
                                  (adventofcode--get-current-problem-id))))
  (aio-await (adventofcode-show-problem problem-id))
  (adventofcode--jump-to-window-by-buffer-name adventofcode--buffer-name))

(defun adventofcode-view-current-problem ()
  "View current problem while staying in `LC Problems' window.
Similar with `adventofcode-show-current-problem', but instead of
jumping to the detail window, this action will jump back in `LC
Problems'."
  (interactive)
  (adventofcode-view-problem (adventofcode--get-current-problem-id)))

(defun adventofcode-show-problem-in-browser (problem-id)
  "Open the problem with id PROBLEM-ID in browser."
  (interactive (list (read-number "Show in browser by problem id: "
                                  (adventofcode--get-current-problem-id))))
  (let* ((problem (adventofcode--get-problem-by-id problem-id))
         (title (adventofcode-problem-title problem))
         (link (adventofcode--problem-link title)))
    (adventofcode--debug "open in browser: %s" link)
    (browse-url link)))

(defun adventofcode-show-current-problem-in-browser ()
  "Open the current problem in browser.
Call `adventofcode-show-problem-in-browser' on the current problem id."
  (interactive)
  (adventofcode-show-problem-in-browser (adventofcode--get-current-problem-id)))

(aio-defun adventofcode-solve-problem (problem-id)
  "Start coding the problem with id PROBLEM-ID."
  (interactive (list (read-number "Solve the problem with id: "
                                  (adventofcode--get-current-problem-id))))
  (let* ((problem-info (adventofcode--get-problem-by-id problem-id))
         (title (adventofcode-problem-title problem-info))
         (problem (aio-await (adventofcode--api-fetch-problem title))))
    (adventofcode--show-problem problem problem-info)
    (adventofcode--start-coding problem problem-info)))

(defun adventofcode-solve-current-problem ()
  "Start coding the current problem.
Call `adventofcode-solve-problem' on the current problem id."
  (interactive)
  (adventofcode-solve-problem (adventofcode--get-current-problem-id)))

(defun adventofcode--jump-to-window-by-buffer-name (buffer-name)
  "Jump to window by BUFFER-NAME."
  (select-window (get-buffer-window buffer-name)))

(defun adventofcode--kill-buff-and-delete-window (buf)
  "Kill BUF and delete its window."
  (delete-windows-on buf t)
  (kill-buffer buf))

(defun adventofcode-quit ()
  "Close and delete adventofcode related buffers and windows."
  (interactive)
  (adventofcode--kill-buff-and-delete-window (get-buffer adventofcode--buffer-name))
  (mapc (lambda (title)
          (adventofcode--kill-buff-and-delete-window
           (get-buffer (adventofcode--get-code-buffer-name title)))
          (let* ((slug-title (adventofcode--slugify-title title))
                 (problem (adventofcode--get-problem slug-title))
                 (problem-id (adventofcode-problem-id problem)))
            (adventofcode--kill-buff-and-delete-window (get-buffer (adventofcode--detail-buffer-name problem-id)))
            (adventofcode--kill-buff-and-delete-window (get-buffer (adventofcode--result-buffer-name problem-id)))
            (adventofcode--kill-buff-and-delete-window (get-buffer (adventofcode--testcase-buffer-name problem-id)))))
        adventofcode--problem-titles))

(defun adventofcode--set-lang (snippets)
  "Set `adventofcode--lang' based on langSlug in SNIPPETS."
  (setq adventofcode--lang
        ;; if there is a mysql snippet, we use mysql as our prefer language.
        (if (seq-find (lambda (s)
                        (equal (alist-get 'langSlug s)
                               adventofcode-prefer-sql))
                      snippets)
            adventofcode-prefer-sql
          adventofcode-prefer-language)))

(defun adventofcode--get-code-buffer-name (title)
  "Get code buffer name by TITLE and `adventofcode-prefer-language'."
  (let* ((suffix (assoc-default
                  adventofcode--lang
                  adventofcode--lang-suffixes))
         (slug-title (adventofcode--slugify-title title))
         (title-with-suffix (concat slug-title suffix)))
    (if adventofcode-save-solutions
        (format "%04d_%s" (adventofcode--get-problem-id slug-title) title-with-suffix)
      title-with-suffix)))

(defun adventofcode--get-code-buffer (buf-name)
  "Get code buffer by BUF-NAME."
  (if (not adventofcode-save-solutions)
      (get-buffer-create buf-name)
    (unless (file-directory-p adventofcode-directory)
      (make-directory adventofcode-directory))
    (find-file-noselect
     (concat (file-name-as-directory adventofcode-directory)
             buf-name))))

(defun adventofcode--get-problem (slug-title)
  "Get problem from `adventofcode--problems' by SLUG-TITLE."
  (seq-find (lambda (p)
              (equal slug-title
                     (adventofcode--slugify-title
                      (adventofcode-problem-title p))))
            (adventofcode-problems-problems adventofcode--problems)))

(defun adventofcode--get-problem-by-id (id)
  "Get problem from `adventofcode--problems' by ID."
  (seq-find (lambda (p)
              (equal id (adventofcode-problem-id p)))
            (adventofcode-problems-problems adventofcode--problems)))

(defun adventofcode--get-problem-id (slug-title)
  "Get problem id by SLUG-TITLE."
  (let ((problem (adventofcode--get-problem slug-title)))
    (adventofcode-problem-id problem)))

(defun adventofcode--get-current-problem-id ()
  "Get id of the current problem."
  (string-to-number (aref (tabulated-list-get-entry) 1)))

(defun adventofcode--start-coding (problem problem-info)
  "Create a buffer for coding PROBLEM with meta-data PROBLEM-INFO.
The buffer will be not associated with any file.  It will choose
major mode by `adventofcode-prefer-language'and `auto-mode-alist'."
  (let-alist problem
    (let* ((title (adventofcode-problem-title problem-info))
           (problem-id (adventofcode-problem-id problem-info))
           (testcase-buf-name (adventofcode--testcase-buffer-name problem-id))
           (result-buf-name (adventofcode--result-buffer-name problem-id))
           (snippets (append .codeSnippets nil))
           (testcase .sampleTestCase))
      (add-to-list 'adventofcode--problem-titles title)
      (adventofcode--solving-window-layout)
      (adventofcode--set-lang snippets)
      (let* ((slug-title (adventofcode--slugify-title title))
             (code-buf-name (adventofcode--get-code-buffer-name title))
             (code-buf (get-buffer code-buf-name))
             (suffix (assoc-default
                      adventofcode--lang
                      adventofcode--lang-suffixes)))
        (unless code-buf
          (with-current-buffer (adventofcode--get-code-buffer code-buf-name)
            (setq code-buf (current-buffer))
            (funcall (assoc-default suffix auto-mode-alist #'string-match-p))
            (adventofcode-solution-mode t)
            (let* ((snippet (seq-find (lambda (s)
                                        (equal (alist-get 'langSlug s)
                                               adventofcode--lang))
                                      snippets))
                   (template-code (alist-get 'code snippet)))
              (unless (save-mark-and-excursion
                        (goto-char (point-min))
                        (search-forward (string-trim template-code) nil t))
                (insert template-code))
              (adventofcode--replace-in-buffer "" ""))))

        (display-buffer code-buf
                        '((display-buffer-reuse-window
                           adventofcode--display-code)
                          (reusable-frames . visible))))
      (with-current-buffer (get-buffer-create testcase-buf-name)
        (erase-buffer)
        (insert testcase)
        (display-buffer (current-buffer)
                        '((display-buffer-reuse-window
                           adventofcode--display-testcase)
                          (reusable-frames . visible))))
      (with-current-buffer (get-buffer-create result-buf-name)
        (erase-buffer)
        (display-buffer (current-buffer)
                        '((display-buffer-reuse-window
                           adventofcode--display-result)
                          (reusable-frames . visible)))))))

(aio-defun adventofcode-restore-layout ()
  "This command should be run in Adventofcode code buffer.
It will restore the layout based on current buffer's name."
  (interactive)
  (let* ((slug-title (file-name-sans-extension (buffer-name)))
         (problem (adventofcode--get-problem slug-title))
         (problem-id (adventofcode-problem-id problem))
         (desc-buf (get-buffer (adventofcode--detail-buffer-name problem-id)))
         (testcase-buf (get-buffer-create (adventofcode--testcase-buffer-name problem-id)))
         (result-buf (get-buffer-create (adventofcode--result-buffer-name problem-id))))
    (adventofcode--solving-window-layout)
    (unless desc-buf
      (aio-await (adventofcode-show-problem problem-id)))
    (display-buffer desc-buf
                    '((display-buffer-reuse-window
                       adventofcode--display-detail)
                      (reusable-frames . visible)))
    (display-buffer testcase-buf
                    '((display-buffer-reuse-window
                       adventofcode--display-testcase)
                      (reusable-frames . visible)))
    (display-buffer result-buf
                    '((display-buffer-reuse-window
                       adventofcode--display-result)
                      (reusable-frames . visible)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Problems Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar adventofcode--problems-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map (kbd "RET") #'adventofcode-show-current-problem)
      (define-key map (kbd "TAB") #'adventofcode-view-current-problem)
      (define-key map "o" #'adventofcode-show-current-problem)
      (define-key map "O" #'adventofcode-show-problem)
      (define-key map "v" #'adventofcode-view-current-problem)
      (define-key map "V" #'adventofcode-view-problem)
      (define-key map "b" #'adventofcode-show-current-problem-in-browser)
      (define-key map "B" #'adventofcode-show-problem-in-browser)
      (define-key map "c" #'adventofcode-solve-current-problem)
      (define-key map "C" #'adventofcode-solve-problem)
      (define-key map "s" #'adventofcode-set-filter-regex)
      (define-key map "L" #'adventofcode-set-prefer-language)
      (define-key map "t" #'adventofcode-set-filter-tag)
      (define-key map "T" #'adventofcode-toggle-tag-display)
      (define-key map "P" #'adventofcode-toggle-paid-display)
      (define-key map "d" #'adventofcode-set-filter-difficulty)
      (define-key map "g" #'adventofcode-refresh)
      (define-key map "G" #'adventofcode-refresh-fetch)
      (define-key map "r" #'adventofcode-reset-filter)
      (define-key map "q" #'quit-window)))
  "Keymap for `adventofcode--problems-mode'.")

(define-derived-mode adventofcode--problems-mode
  tabulated-list-mode "LC Problems"
  "Major mode for browsing a list of problems."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'adventofcode-refresh nil t)
  :group 'adventofcode
  :keymap adventofcode--problems-mode-map)

(defun adventofcode--set-evil-local-map (map)
  "Set `evil-normal-state-local-map' to MAP."
  (when (featurep 'evil)
    (define-key map "h" nil)
    (setq evil-normal-state-local-map map)))

(add-hook 'adventofcode--problems-mode-hook #'hl-line-mode)
(add-hook 'adventofcode--problems-mode-hook
          (lambda () (adventofcode--set-evil-local-map adventofcode--problems-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Detail Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar adventofcode--problem-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" #'quit-window)))
  "Keymap for `adventofcode--problem-detail-mode'.")

(define-derived-mode adventofcode--problem-detail-mode
  special-mode "LC Detail"
  "Major mode for display problem detail."
  :group 'adventofcode
  :keymap adventofcode--problem-detail-mode-map)

(add-hook 'adventofcode--problem-detail-mode-hook
          (lambda () (adventofcode--set-evil-local-map adventofcode--problem-detail-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Loading Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use spinner.el to show progress indicator
(defvar adventofcode--spinner (spinner-create 'progress-bar-filled)
  "Progress indicator to show request progress.")
(defconst adventofcode--loading-lighter
  '(" [Adventofcode" (:eval (spinner-print adventofcode--spinner)) "]"))

(define-minor-mode adventofcode--loading-mode
  "Minor mode to showing adventofcode loading status."
  :require 'adventofcode
  :lighter adventofcode--loading-lighter
  :group 'adventofcode
  (if adventofcode--loading-mode
      (spinner-start adventofcode--spinner)
    (spinner-stop adventofcode--spinner)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Solution Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar adventofcode-solution-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-c C-t") #'adventofcode-try)
      (define-key map (kbd "C-c C-s") #'adventofcode-submit)
      (define-key map (kbd "C-c C-r") #'adventofcode-restore-layout)))
  "Keymap for `adventofcode-solution-mode'.")

(define-minor-mode adventofcode-solution-mode
  "Minor mode to provide shortcut and hooks."
  :require 'adventofcode
  :lighter "LC Solution"
  :group 'adventofcode
  :keymap adventofcode-solution-mode-map)

(provide 'adventofcode)
;;; adventofcode.el ends here
