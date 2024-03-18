;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'url)

(let ((urlbuf
       (url-retrieve-synchronously
        "https://raw.githubusercontent.com/github/linguist/master/lib/linguist/languages.yml"
        t)))
  (prog1 (with-current-buffer urlbuf
           (goto-char url-http-end-of-headers)
           (cl-loop while (re-search-forward "^\\([^#[:space:]][^:\n]*\\):" nil t)
                    for lang = (match-string-no-properties 1)
                    collect (replace-regexp-in-string " " "-" lang)))
    (kill-buffer urlbuf)))
