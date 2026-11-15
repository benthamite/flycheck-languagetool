;;; flycheck-languagetool-test.el --- Tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for flycheck-languagetool.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'flycheck-languagetool)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun flt-test--make-match (offset length &optional message rule-id)
  "Build a single LanguageTool match alist.
OFFSET and LENGTH are character positions.  MESSAGE and RULE-ID
are optional strings."
  `((offset  . ,offset)
    (length  . ,length)
    (message . ,(or message "test error"))
    (rule    . ((id . ,(or rule-id "TEST_RULE"))))))

(defun flt-test--make-results (&rest matches)
  "Wrap MATCHES in the top-level alist that the API returns.
Uses a list (not a vector) for the matches array, matching what
`flycheck-parse-json' produces (it sets `json-array-type' to
\\='list)."
  `((matches . ,matches)))

;; ---------------------------------------------------------------------------
;; flycheck-languagetool--check-all
;; ---------------------------------------------------------------------------

(ert-deftest flt-test-check-all/valid-match ()
  "A match within buffer bounds is returned."
  (with-temp-buffer
    (insert "Hello, world!")  ; 13 chars
    (let* ((results (flt-test--make-results
                     (flt-test--make-match 0 5 "bad greeting" "HELLO")))
           (errors (flycheck-languagetool--check-all results)))
      (should (= 1 (length errors)))
      (let ((err (car errors)))
        ;; Line 1, column 0
        (should (= 1 (nth 0 err)))
        (should (= 0 (nth 1 err)))
        (should (eq 'warning (nth 2 err)))
        (should (equal "bad greeting" (nth 3 err)))))))

(ert-deftest flt-test-check-all/offset-past-point-max ()
  "A match whose offset exceeds `point-max' is silently skipped."
  (with-temp-buffer
    (insert "short")  ; 5 chars
    (let* ((results (flt-test--make-results
                     (flt-test--make-match 100 5)))
           (errors (flycheck-languagetool--check-all results)))
      (should (= 0 (length errors))))))

(ert-deftest flt-test-check-all/end-past-point-max ()
  "A match whose end (offset+length) exceeds `point-max' is skipped."
  (with-temp-buffer
    (insert "short")  ; 5 chars
    (let* ((results (flt-test--make-results
                     (flt-test--make-match 3 10)))
           (errors (flycheck-languagetool--check-all results)))
      (should (= 0 (length errors))))))

(ert-deftest flt-test-check-all/mixed-valid-and-stale ()
  "Valid matches are kept, stale ones are dropped."
  (with-temp-buffer
    (insert "Hello, world!")  ; 13 chars
    (let* ((results (flt-test--make-results
                     (flt-test--make-match 0 5 "valid")
                     (flt-test--make-match 200 10 "stale")
                     (flt-test--make-match 7 5 "also valid")))
           (errors (flycheck-languagetool--check-all results)))
      (should (= 2 (length errors)))
      ;; check-list is built with push, so order is reversed
      (should (equal "also valid" (nth 3 (nth 0 errors))))
      (should (equal "valid"      (nth 3 (nth 1 errors)))))))

(ert-deftest flt-test-check-all/empty-matches ()
  "An empty matches list returns nil."
  (with-temp-buffer
    (insert "Hello")
    (let ((errors (flycheck-languagetool--check-all
                   (flt-test--make-results))))
      (should (null errors)))))

(ert-deftest flt-test-check-all/multiline-positions ()
  "Matches on later lines get the correct line number."
  (with-temp-buffer
    (insert "line one\nline two\nline three")
    ;; "line two" starts at offset 9
    (let* ((results (flt-test--make-results
                     (flt-test--make-match 9 4 "line word")))
           (errors (flycheck-languagetool--check-all results)))
      (should (= 1 (length errors)))
      (should (= 2 (nth 0 (car errors))))))) ; line 2

;; The original implementation, which counted lines from the buffer
;; start for every match.  Kept as the reference that the linear
;; version must reproduce exactly.
(defun flt-test--check-all-reference (results)
  "Map RESULTS to errors as the original quadratic implementation did."
  (let ((matches (cdr (assoc 'matches results)))
        check-list)
    (dolist (match matches)
      (let* ((pt-beg (+ (point-min) (cdr (assoc 'offset match))))
             (len (cdr (assoc 'length match)))
             (pt-end (+ pt-beg len)))
        (when (and (<= (point-min) pt-beg) (<= pt-end (point-max)))
          (let* ((ln (save-restriction
                       (widen)
                       (line-number-at-pos pt-beg)))
                 (id (cdr (assoc 'id (assoc 'rule match))))
                 (subid (cdr (assoc 'subId (assoc 'rule match))))
                 (col-start (flycheck-languagetool--column-at-pos pt-beg))
                 (col-end (flycheck-languagetool--column-at-pos pt-end)))
            (push (list ln col-start 'warning (cdr (assoc 'message match))
                        :end-column col-end
                        :id (cons id subid))
                  check-list)))))
    check-list))

(defconst flt-test--multiline-text
  "First line.\n\n\tTabbed third line here.\nFourth line \u00e9 \u4e2d\u6587 wide.\n\nSixth line\nlast"
  "Text with empty lines, a tab, wide characters and no final newline.")

(defun flt-test--unsorted-matches ()
  "Return matches for `flt-test--multiline-text' in scrambled order.
Includes duplicates, a match on a newline, one at the very end, and
out-of-bounds matches."
  (list (flt-test--make-match 40 3 "m1")
        (flt-test--make-match 0 5 "m2")
        (flt-test--make-match 13 7 "m3")
        (flt-test--make-match 500 2 "out of bounds")
        (flt-test--make-match 11 1 "on newline")
        (flt-test--make-match 40 3 "duplicate of m1")
        (flt-test--make-match 12 1 "empty line")
        (flt-test--make-match (1- (length flt-test--multiline-text)) 1 "last")
        (flt-test--make-match (length flt-test--multiline-text) 0 "at end")
        (flt-test--make-match 60 2 "m4")
        (flt-test--make-match 5 100 "end out of bounds")
        (flt-test--make-match 30 4 "m5")))

(ert-deftest flt-test-check-all/matches-reference-unsorted ()
  "Unsorted matches on many lines map exactly as the old code did."
  (with-temp-buffer
    (insert flt-test--multiline-text)
    (let ((results (apply #'flt-test--make-results (flt-test--unsorted-matches))))
      (should (equal (flt-test--check-all-reference results)
                     (flycheck-languagetool--check-all results)))
      (should (= 10 (length (flycheck-languagetool--check-all results)))))))

(ert-deftest flt-test-check-all/matches-reference-narrowed ()
  "In a narrowed buffer, lines count from the true start as before."
  (with-temp-buffer
    (insert "before\nnarrowing\n" flt-test--multiline-text "\nafter\n")
    (narrow-to-region 18 (+ 18 (length flt-test--multiline-text)))
    (let ((results (apply #'flt-test--make-results (flt-test--unsorted-matches))))
      (should (equal (flt-test--check-all-reference results)
                     (flycheck-languagetool--check-all results)))
      (should (= 6 (nth 0 (car (last (flycheck-languagetool--check-all
                                      results)))))))))

(ert-deftest flt-test-check-all/matches-reference-random ()
  "Random matches over a large buffer map exactly as the old code did."
  (with-temp-buffer
    (dotimes (i 2000)
      (insert (make-string (% (* i 7) 23) ?x) "\n"))
    (let* ((state (cl-make-random-state 42))
           (size (buffer-size))
           (results (apply #'flt-test--make-results
                           (cl-loop repeat 3000
                                    collect (flt-test--make-match
                                             (cl-random (+ size 20) state)
                                             (cl-random 10 state))))))
      (should (equal (flt-test--check-all-reference results)
                     (flycheck-languagetool--check-all results))))))

(ert-deftest flt-test-check-all/scales-linearly ()
  "Many matches on a large buffer are mapped without per-match rescans."
  (with-temp-buffer
    (dotimes (_ 40000)
      (insert "Some words on a line of prose text.\n"))
    (let* ((size (buffer-size))
           (results (apply #'flt-test--make-results
                           (cl-loop for i below 10000
                                    collect (flt-test--make-match
                                             (% (* i 7919) (- size 5)) 4))))
           (line-calls 0)
           (start (float-time))
           errors)
      (cl-letf* ((orig (symbol-function 'line-number-at-pos))
                 ((symbol-function 'line-number-at-pos)
                  (lambda (&rest args)
                    (cl-incf line-calls)
                    (apply orig args))))
        (setq errors (flycheck-languagetool--check-all results)))
      (should (= 10000 (length errors)))
      ;; The structural check is the reliable one: no per-match scan
      ;; from the buffer start.  The time bound is deliberately loose.
      (should (= 0 line-calls))
      (should (< (- (float-time) start) 30.0)))))

;; ---------------------------------------------------------------------------
;; flycheck-languagetool--read-results — callback semantics
;; ---------------------------------------------------------------------------

(ert-deftest flt-test-read-results/dead-buffer ()
  "When the source buffer is dead, callback receives `interrupted'."
  (let (callback-args)
    (let ((dead-buf (generate-new-buffer " *flt-dead*")))
      (kill-buffer dead-buf)
      ;; Simulate url-retrieve response buffer
      (with-temp-buffer
        (let ((url-http-end-of-headers (point-min)))
          (insert "{\"matches\":[]}")
          (flycheck-languagetool--read-results
           nil dead-buf
           (lambda (status &optional errors)
             (setq callback-args (list status errors)))))))
    (should (eq 'interrupted (nth 0 callback-args)))))

(ert-deftest flt-test-read-results/callback-called-once-on-error ()
  "When parsing fails, callback is called exactly once with `errored'."
  (let ((call-count 0)
        callback-statuses)
    (let ((source-buf (generate-new-buffer " *flt-source*")))
      (unwind-protect
          (with-current-buffer source-buf
            (insert "short")
            ;; Create a separate buffer to simulate the url-retrieve
            ;; response buffer with malformed data.
            (let ((response-buf (generate-new-buffer " *flt-response*")))
              (unwind-protect
                  (progn
                    (with-current-buffer response-buf
                      (insert "{\"matches\": \"not-an-array\"}")
                      (setq-local url-http-end-of-headers (point-min))
                      (flycheck-languagetool--read-results
                       nil source-buf
                       (lambda (status &optional errors)
                         (cl-incf call-count)
                         (push status callback-statuses)))))
                ;; response-buf is killed by --read-results, but be safe
                (when (buffer-live-p response-buf)
                  (kill-buffer response-buf)))))
        (when (buffer-live-p source-buf)
          (kill-buffer source-buf))))
    (should (= 1 call-count))
    (should (eq 'errored (car callback-statuses)))))

(ert-deftest flt-test-read-results/transport-error-without-headers ()
  "A transport error without HTTP headers calls callback with `errored'."
  (let ((call-count 0)
        callback-status
        callback-message
        response-buf)
    (let ((source-buf (generate-new-buffer " *flt-source*")))
      (unwind-protect
          (progn
            (setq response-buf (generate-new-buffer " *flt-response*"))
            (with-current-buffer response-buf
              (setq-local url-http-end-of-headers nil)
              (flycheck-languagetool--read-results
               '(:error
                 (error connection-failed
                        "connection broken by remote peer\n"
                        :host "localhost" :service 8081))
               source-buf
               (lambda (status &optional message)
                 (cl-incf call-count)
                 (setq callback-status status
                       callback-message message)))))
        (when (buffer-live-p source-buf)
          (kill-buffer source-buf))
        (when (buffer-live-p response-buf)
          (kill-buffer response-buf))))
    (should (= 1 call-count))
    (should (eq 'errored callback-status))
    (should (string-match-p "connection broken by remote peer"
                            callback-message))
    (should-not (buffer-live-p response-buf))))

(ert-deftest flt-test-read-results/successful-parse ()
  "A well-formed response calls callback once with `finished'."
  (let ((call-count 0)
        callback-status
        callback-errors)
    (let ((source-buf (generate-new-buffer " *flt-source*")))
      (unwind-protect
          (with-current-buffer source-buf
            (insert "Hello, world!")
            (let ((response-buf (generate-new-buffer " *flt-response*")))
              (unwind-protect
                  (progn
                    (with-current-buffer response-buf
                      ;; json-encode with json-array-type doesn't
                      ;; matter here; flycheck-parse-json handles it.
                      (let ((json-str (json-encode
                                       '((matches . [((offset . 0)
                                                      (length . 5)
                                                      (message . "test msg")
                                                      (rule . ((id . "R1"))))])))))
                        (insert json-str))
                      (setq-local url-http-end-of-headers (point-min))
                      (flycheck-languagetool--read-results
                       nil source-buf
                       (lambda (status &optional errors)
                         (cl-incf call-count)
                         (setq callback-status status
                               callback-errors errors)))))
                (when (buffer-live-p response-buf)
                  (kill-buffer response-buf)))))
        (when (buffer-live-p source-buf)
          (kill-buffer source-buf))))
    (should (= 1 call-count))
    (should (eq 'finished callback-status))
    (should (= 1 (length callback-errors)))))

;; ---------------------------------------------------------------------------
;; flycheck-languagetool--start-server
;; ---------------------------------------------------------------------------

(defmacro flt-test--with-fake-server (script &rest body)
  "Run BODY with the server command replaced by the shell SCRIPT.
Kills the fake server and its buffer afterwards."
  (declare (indent 1))
  `(let ((flycheck-languagetool-server-command (list "sh" "-c" ,script))
         (flycheck-languagetool-server-args nil)
         (flycheck-languagetool--started-server t))
     (unwind-protect
         (progn ,@body)
       (when-let ((process (get-process "languagetool-server")))
         (delete-process process))
       (when-let ((buffer (get-buffer " *LanguageTool server*")))
         (kill-buffer buffer)))))

(defun flt-test--wait-for-exit (process)
  "Wait until PROCESS has exited and its sentinel has run."
  (while (process-live-p process)
    (accept-process-output process 0.1))
  (accept-process-output nil 0.1))

(ert-deftest flt-test-start-server/restart-after-exit ()
  "A server that exits after starting is restarted by the next check."
  (flt-test--with-fake-server "echo ' Server started'; sleep 30"
    (flycheck-languagetool--start-server)
    (let ((process (get-process "languagetool-server")))
      (should (process-get process 'flycheck-languagetool-ready))
      (kill-process process)
      (flt-test--wait-for-exit process))
    (should-not flycheck-languagetool--started-server)))

(ert-deftest flt-test-start-server/no-restart-after-failed-start ()
  "A server that exits before starting is not restarted."
  (flt-test--with-fake-server "exit 1"
    (flycheck-languagetool--start-server)
    (flt-test--wait-for-exit (get-buffer-process " *LanguageTool server*"))
    (should flycheck-languagetool--started-server)))

(ert-deftest flt-test-start-server/ignores-stale-startup-message ()
  "Output left by a previous server does not count as a start."
  (with-current-buffer (get-buffer-create " *LanguageTool server*")
    (insert "2026-01-01 INFO Server started\n"))
  (flt-test--with-fake-server "sleep 30"
    (cl-letf (((symbol-function 'accept-process-output)
               (lambda (&rest _) (throw 'waited t))))
      ;; The new server prints nothing, so the startup check must wait.
      (should (catch 'waited
                (flycheck-languagetool--start-server)
                nil)))
    (should-not (process-get (get-process "languagetool-server")
                             'flycheck-languagetool-ready))))

(provide 'flycheck-languagetool-test)
;;; flycheck-languagetool-test.el ends here
