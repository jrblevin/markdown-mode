# Contributing Guidelines

## Pull Requests

For pull requests, it is recommended to create a new feature branch
(e.g., `feat/my-new-feature`) and submit the pull request from that
branch.  If instead you add the commit(s) directly to the `master`
branch in your own repository, then it can be more difficult to get
back in sync if changes are required to the patch.

To have your pull request accepted quickly, please try to follow these
guidelines:

*   Markdown mode contributions should adhere to both the
    [GNU Emacs Lisp coding conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html) and the
    [Major Mode Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Major-Mode-Conventions.html#Major-Mode-Conventions).

*   Test cases are needed for significant changes.  Markdown Mode uses
    the `ert` (Emacs Lisp Regression Testing) library.  If you are not
    familiar with writing tests, there are hundreds of examples in
    `tests/markdown-test.el` that you can use as a starting point for
    new tests.

*   You can confirm that your tests pass by either typing `make test`
    from the command line or by issuing a GitHub pull request and
    letting the Travis CI integration on GitHub run the tests for you.

*   Please be sure to update the documentation at the top of
    `markdown-mode.el` and the docstrings for related variables or
    functions.

*   If your patch involves changes to the documentation, you can
    update the `README.md` file by running the `webpage.sh` script.

*   It's also helpful if you update the `CHANGES.md` file to reflect
    the bug you fixed or the feature you added or improved.
