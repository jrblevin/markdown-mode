(package "markdown-mode"
         "2.7alpha"
         "Major mode for Markdown-formatted text")

(website-url "https://jblevins.org/projects/markdown-mode/")
(keywords "markdown" "github flavored markdown" "itex")

(package-file "markdown-mode.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)

(depends-on "emacs" "27.1")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-compile-hook
           (lambda ()
             (setq byte-compile-error-on-warn t)))
