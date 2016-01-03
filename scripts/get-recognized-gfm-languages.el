(require 'cl-lib)

(let ((urlbuf
       (url-retrieve-synchronously
        "https://raw.githubusercontent.com/github/linguist/master/lib/linguist/languages.yml"
        t)))
  (with-current-buffer urlbuf
    (goto-char (point-min))
    ;; get past http headers
    (re-search-forward "^$")
    (cl-loop while (re-search-forward "^\\([^#[:space:]]+?\\):" nil t)
             collect (match-string-no-properties 1)))
  (kill-buffer urlbuf))
