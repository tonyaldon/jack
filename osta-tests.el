;;; require

(require 'ert)

;;; utils

(ert-deftest osta-escape-test ()
  (should (string= (osta-escape "<") "&lt;"))
  (should (string= (osta-escape ">") "&gt;"))
  (should (string= (osta-escape "&") "&amp;"))
  (should (string= (osta-escape "\"") "&quot;"))
  (should (string= (osta-escape "'") "&apos;"))
  (should (string= (osta-escape "regular text") "regular text"))
  (should (string= (osta-escape "<...>...&...\"...'") "&lt;...&gt;...&amp;...&quot;...&apos;")))

(ert-deftest osta-parse-tag-kw-test ()
  (should-error (osta-parse-tag-kw "string-is-not-a-valid-tag-keyword"))
  (should-error (osta-parse-tag-kw 'symbol-is-not-a-valid-tag-keyword))
  (should (equal (osta-parse-tag-kw :div) '("div" nil nil)))
  (should (equal (osta-parse-tag-kw :div/id) '("div" "id" nil)))
  (should (equal (osta-parse-tag-kw :div.class) '("div" nil "class")))
  (should (equal (osta-parse-tag-kw :div/id.class) '("div" "id" "class")))
  (should (equal (osta-parse-tag-kw :div/id.class-1.class-2) '("div" "id" "class-1 class-2"))))

(ert-deftest osta-tag-test ()

  ;; id and classes in the tag-kw
  (should (equal (osta-tag :div)
                 '(:left  "<div>"
                   :right "</div>")))
  (should (equal (osta-tag :div/id)
                 '(:left  "<div id=\"id\">"
                   :right "</div>")))
  (should (equal (osta-tag :div.class)
                 '(:left  "<div class=\"class\">"
                   :right "</div>")))
  (should (equal (osta-tag :div/id.class)
                 '(:left  "<div id=\"id\" class=\"class\">"
                   :right "</div>")))
  (should (equal (osta-tag :div/id.class-1.class-2)
                 '(:left  "<div id=\"id\" class=\"class-1 class-2\">"
                   :right "</div>")))

  ;; void tags
  (should (equal (osta-tag :hr) '(:left "<hr />")))

  ;; tag-kw must be keywords
  (should-error (osta-tag 'div))
  (should-error (osta-tag "div"))
  (should-error (osta-tag 'div '(:id "id")))
  (should-error (osta-tag "div" '(:id "id")))

  ;; attributes plist
  (should (equal (osta-tag :div '(:id "id"))
                 '(:left  "<div id=\"id\">"
                   :right "</div>")))
  (should (equal (osta-tag :div '(:id "id" :class "class"))
                 '(:left  "<div id=\"id\" class=\"class\">"
                   :right "</div>")))

  ;; values in key/value pairs of attributes plist are evaluated
  (should (equal (osta-tag :div '(:id (concat "id-" "123")))
                 '(:left  "<div id=\"id-123\">"
                   :right "</div>")))

  ;; attribute values are escaped
  (should (equal (osta-tag :div '(:id "\""))
                 '(:left  "<div id=\"&quot;\">"
                   :right "</div>")))

  ;; character percent % is left as it is in attribute values.
  ;; So you can build html template feeding attributes
  ;; values with %s string, that you can use after with
  ;; the function `format'
  (should (equal (osta-tag :div '(:id "%s"))
                 '(:left  "<div id=\"%s\">"
                   :right "</div>")))

  ;; `id' in `attributes' has priority over `id' in `tag-kw'
  (should (equal (osta-tag :div/id-in-tag '(:id "id-in-plist"))
                 '(:left  "<div id=\"id-in-plist\">"
                   :right "</div>")))

  ;; classes in `tag-kw' and `attributes' plist
  (should (equal (osta-tag :div.class-in-tag '(:class "class-a class-b"))
                 '(:left  "<div class=\"class-in-tag class-a class-b\">"
                   :right "</div>")))

  ;; boolean attributes
  (should (equal (osta-tag :input '(:type "checkbox" :checked t))
                 '(:left "<input type=\"checkbox\" checked=\"checked\" />")))
  (should (equal (osta-tag :input '(:type "checkbox" :checked nil))
                 '(:left "<input type=\"checkbox\" />"))))

(ert-deftest osta-html-test ()
  ;; objects that can't be components are exported as empty string
  ;; when `osta-html-raise-error-p' is `nil' (which is the default)
  (let ((osta-html-raise-error-p nil))
    (should (string= (osta-html []) ""))
    (should (string= (osta-html t) ""))
    (should (string= (osta-html [] "foo" t "bar") "foobar")))

  ;; objects that can't be components raise an error when
  ;; when `osta-html-raise-error-p' is `t'
  (let ((osta-html-raise-error-p t))
    (should-error (string= (osta-html []) ""))
    (should-error (string= (osta-html t) ""))
    (should-error (string= (osta-html [] "foo" t "bar") "foobar"))
    ;; `nil' and '() which are `eq' always return empty string ""
    (should (string= (osta-html nil) ""))
    (should (string= (osta-html '()) "")))

  ;; `osta-html-raise-error-p' is set to nil by default
  ;; and affects only objects that can't be components
  ;; in `osta-html' function
  (let ((osta-html-raise-error-p nil))

    ;; nil component is just ignored
    (should (string= (osta-html nil) ""))
    (should (string= (osta-html '()) ""))
    (should (string= (osta-html "foo" nil "bar") "foobar"))

    ;; string component
    (should (string= (osta-html "foo") "foo"))

    ;; number component
    (should (string= (osta-html 16) "16"))

    ;; empty tags
    (should (string= (osta-html '(:div)) "<div></div>"))

    ;; voids tags
    (should (string= (osta-html '(:hr)) "<hr />"))
    (should (string= (osta-html '(:div (:hr))) "<div><hr /></div>"))
    (should (string= (osta-html '(:div "foo" (:hr) "bar")) "<div>foo<hr />bar</div>"))
    (should (string= (osta-html '(:div "foo" (:hr) "bar" (:hr))) "<div>foo<hr />bar<hr /></div>"))

    ;; tag component
    (should (string= (osta-html '(:p "foo")) "<p>foo</p>"))
    (should (string= (osta-html '(:p "foo" "bar")) "<p>foobar</p>"))

    ;; nested tags
    (should (string= (osta-html '(:section (:div (:p "foo"))))
                     "<section><div><p>foo</p></div></section>"))
    (should (string= (osta-html '(:div (:div "foo" "bar"))) "<div><div>foobar</div></div>"))
    (should (string= (osta-html '(:div "a" (:p "b" (:span "c") "d") "e"))
                     "<div>a<p>b<span>c</span>d</p>e</div>"))
    (should (string= (osta-html '(:div (:p "foo") (:p "bar")))
                     "<div><p>foo</p><p>bar</p></div>"))

    ;; component arguments in `osta-html' are concatenated
    (should (string= (osta-html "foo" "bar") "foobar"))
    (should (string= (osta-html '(:p "foo") "bar") "<p>foo</p>bar"))
    (should (string= (osta-html '(:p "foo") '(:p "bar")) "<p>foo</p><p>bar</p>"))

    ;; components in a list are concatenated at the same level
    (should (string= (osta-html '("foo" 1 "bar")) "foo1bar"))
    (should (string= (osta-html '((:li "a") (:li "b")))
                     "<li>a</li><li>b</li>"))
    (should (string= (osta-html '(:li "a") '((:li "b") (:li "c")) '(:li "d"))
                     "<li>a</li><li>b</li><li>c</li><li>d</li>"))

    ;; attributes
    (should (string= (osta-html '(:div (@ :id "id" :class "class") "foo"))
                     "<div id=\"id\" class=\"class\">foo</div>"))
    (should (string= (osta-html '(:p/id-in-tag (@ :id "id-in-plist") (:span "foo")))
                     "<p id=\"id-in-plist\"><span>foo</span></p>"))
    (should (string= (osta-html '(:p.class-in-tag (@ :class "class-in-plist") "foo"))
                     "<p class=\"class-in-tag class-in-plist\">foo</p>"))

    ;; tag content can be lists of components
    (should (string= (osta-html '(:ul ((:li "1") (:li "2"))))
                     "<ul><li>1</li><li>2</li></ul>"))
    (should (string= (osta-html '(:ul (@ :id "id") ((:li "1") (:li "2"))))
                     "<ul id=\"id\"><li>1</li><li>2</li></ul>"))

    ;; components can be generated by a form
    (should (string= (osta-html `(:p ,(concat "foo-" "bar")))
                     "<p>foo-bar</p>"))
    (should (string= (osta-html (mapcar (lambda (n) `(:p ,n)) '(1 2 3)))
                     "<p>1</p><p>2</p><p>3</p>"))

    ;; tag content can be forms
    (should (string=
             (osta-html `(:ul ,(mapcar (lambda (n) `(:li ,n)) '(1 2))))
             "<ul><li>1</li><li>2</li></ul>"))
    (should (string=
             (osta-html `(:ul (@ :id "id") ,(mapcar (lambda (n) `(:li ,n)) '(1 2))))
             "<ul id=\"id\"><li>1</li><li>2</li></ul>"))

    ;; tag content and attributes can be vars
    (should (string= (let ((x "foo") (y "bar"))
                       (osta-html `(:p (@ :id ,x) ,y)))
                     "<p id=\"foo\">bar</p>"))
    (should (string= (osta-html
                      (let ((x "foo") (y "bar"))
                        `(:p (@ :id ,x) ,y)))
                     "<p id=\"foo\">bar</p>"))

    ;; percent sign % in string tags
    (should (string= (osta-html "%") "%"))
    (should (string= (osta-html "%s") "%s"))
    (should (string= (osta-html "%s %s %s") "%s %s %s"))
    (should (string= (osta-html '(:p "%s")) "<p>%s</p>"))
    (should (string= (osta-html '(:p "% %% %%%")) "<p>% %% %%%</p>"))
    (should (string= (osta-html '(:div (:p "%s") "foo" (:p "%s")))
                     "<div><p>%s</p>foo<p>%s</p></div>"))

    ;; `osta-html' can generate html template that can be
    ;; used/consumed by the function `format'
    (should (string=
             (let ((template
                    (osta-html '(:div (@ :class "%s")
                                 (:p (@ :id "%s") "foo")
                                 (:p (@ :id "%s") "bar")))))
               (format template "container" "p-foo" "p-bar"))
             "<div class=\"container\"><p id=\"p-foo\">foo</p><p id=\"p-bar\">bar</p></div>"))))

;; (ert-deftest osta-html-lisp-nesting-test ()
;;   ;; In the first implementation (recursive way) of `osta-html'
;;   ;; (commit 554d733), `osta-html' raised the error ("Lisp nesting
;;   ;; exceeds ‘max-lisp-eval-depth’") for nested list of more than
;;   ;; 50 `:div'.
;;   ;; This test exists to ensure, we don't modify the implementation
;;   ;; in a way that it fails "badly" for "deep" nested structure.  For,
;;   ;; instance for 10000 nested `:div', `osta-html' lasts about 10s
;;   ;; to return the result but it doesn't break.
;;   (message "Might take 10 seconds or more...")
;;   (let* ((max-lisp-eval-depth 800) ; default value
;;          ;; (nested-foo-comp 3) -> (:div (:div (:div \"foo\")))
;;          (nested-foo-comp
;;           (lambda (n)
;;             (let ((comp "foo"))
;;               (dotimes (_ n) (setq comp (list :div comp)))
;;               comp)))
;;          (comp (funcall nested-foo-comp 10000)))
;;     (should (osta-html comp))))
