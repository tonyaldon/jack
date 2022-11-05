;;; require

(require 'ert)

;;; utils

(ert-deftest jack-escape-test ()
  (should (string= (jack-escape "<") "&lt;"))
  (should (string= (jack-escape ">") "&gt;"))
  (should (string= (jack-escape "&") "&amp;"))
  (should (string= (jack-escape "\"") "&quot;"))
  (should (string= (jack-escape "'") "&apos;"))
  (should (string= (jack-escape "regular text") "regular text"))
  (should (string= (jack-escape "<...>...&...\"...'") "&lt;...&gt;...&amp;...&quot;...&apos;")))

(ert-deftest jack-parse-tag-kw-test ()
  (should-error (jack-parse-tag-kw "string-is-not-a-valid-tag-keyword"))
  (should-error (jack-parse-tag-kw 'symbol-is-not-a-valid-tag-keyword))
  (should (equal (jack-parse-tag-kw :div) '("div" nil nil)))
  (should (equal (jack-parse-tag-kw :div/id) '("div" "id" nil)))
  (should (equal (jack-parse-tag-kw :div.class) '("div" nil "class")))
  (should (equal (jack-parse-tag-kw :div/id.class) '("div" "id" "class")))
  (should (equal (jack-parse-tag-kw :div/id.class-1.class-2) '("div" "id" "class-1 class-2"))))

(ert-deftest jack-tag-test ()

  ;; id and classes in the tag-kw
  (should (equal (jack-tag :div)
                 '(:left  "<div>"
                   :right "</div>")))
  (should (equal (jack-tag :div/id)
                 '(:left  "<div id=\"id\">"
                   :right "</div>")))
  (should (equal (jack-tag :div.class)
                 '(:left  "<div class=\"class\">"
                   :right "</div>")))
  (should (equal (jack-tag :div/id.class)
                 '(:left  "<div id=\"id\" class=\"class\">"
                   :right "</div>")))
  (should (equal (jack-tag :div/id.class-1.class-2)
                 '(:left  "<div id=\"id\" class=\"class-1 class-2\">"
                   :right "</div>")))

  ;; void tags
  (should (equal (jack-tag :hr) '(:left "<hr />")))

  ;; tag-kw must be keywords
  (should-error (jack-tag 'div))
  (should-error (jack-tag "div"))
  (should-error (jack-tag 'div '(:id "id")))
  (should-error (jack-tag "div" '(:id "id")))

  ;; attributes plist
  (should (equal (jack-tag :div '(:id "id"))
                 '(:left  "<div id=\"id\">"
                   :right "</div>")))
  (should (equal (jack-tag :div '(:id "id" :class "class"))
                 '(:left  "<div id=\"id\" class=\"class\">"
                   :right "</div>")))

  ;; values in key/value pairs of attributes plist are evaluated
  (should (equal (jack-tag :div '(:id (concat "id-" "123")))
                 '(:left  "<div id=\"id-123\">"
                   :right "</div>")))

  ;; attribute values are escaped
  (should (equal (jack-tag :div '(:id "\""))
                 '(:left  "<div id=\"&quot;\">"
                   :right "</div>")))

  ;; character percent % is left as it is in attribute values.
  ;; So you can build html template feeding attributes
  ;; values with %s string, that you can use after with
  ;; the function `format'
  (should (equal (jack-tag :div '(:id "%s"))
                 '(:left  "<div id=\"%s\">"
                   :right "</div>")))

  ;; `id' in `attributes' has priority over `id' in `tag-kw'
  (should (equal (jack-tag :div/id-in-tag '(:id "id-in-plist"))
                 '(:left  "<div id=\"id-in-plist\">"
                   :right "</div>")))

  ;; classes in `tag-kw' and `attributes' plist
  (should (equal (jack-tag :div.class-in-tag '(:class "class-a class-b"))
                 '(:left  "<div class=\"class-in-tag class-a class-b\">"
                   :right "</div>")))

  ;; boolean attributes
  (should (equal (jack-tag :input '(:type "checkbox" :checked t))
                 '(:left "<input type=\"checkbox\" checked=\"checked\" />")))
  (should (equal (jack-tag :input '(:type "checkbox" :checked nil))
                 '(:left "<input type=\"checkbox\" />"))))

(ert-deftest jack-html-test ()
  ;; objects that can't be components are exported as empty string
  ;; when `jack-html-raise-error-p' is `nil' (which is the default)
  (let ((jack-html-raise-error-p nil))
    (should (string= (jack-html []) ""))
    (should (string= (jack-html t) ""))
    (should (string= (jack-html [] "foo" t "bar") "foobar")))

  ;; objects that can't be components raise an error when
  ;; when `jack-html-raise-error-p' is `t'
  (let ((jack-html-raise-error-p t))
    (should-error (string= (jack-html []) ""))
    (should-error (string= (jack-html t) ""))
    (should-error (string= (jack-html [] "foo" t "bar") "foobar"))
    ;; `nil' and '() which are `eq' always return empty string ""
    (should (string= (jack-html nil) ""))
    (should (string= (jack-html '()) "")))

  ;; `jack-html-raise-error-p' is set to nil by default
  ;; and affects only objects that can't be components
  ;; in `jack-html' function
  (let ((jack-html-raise-error-p nil))

    ;; nil component is just ignored
    (should (string= (jack-html nil) ""))
    (should (string= (jack-html '()) ""))
    (should (string= (jack-html "foo" nil "bar") "foobar"))

    ;; string component
    (should (string= (jack-html "foo") "foo"))

    ;; number component
    (should (string= (jack-html 16) "16"))

    ;; empty tags
    (should (string= (jack-html '(:div)) "<div></div>"))

    ;; voids tags
    (should (string= (jack-html '(:hr)) "<hr />"))
    (should (string= (jack-html '(:div (:hr))) "<div><hr /></div>"))
    (should (string= (jack-html '(:div "foo" (:hr) "bar")) "<div>foo<hr />bar</div>"))
    (should (string= (jack-html '(:div "foo" (:hr) "bar" (:hr))) "<div>foo<hr />bar<hr /></div>"))

    ;; tag component
    (should (string= (jack-html '(:p "foo")) "<p>foo</p>"))
    (should (string= (jack-html '(:p "foo" "bar")) "<p>foobar</p>"))

    ;; nested tags
    (should (string= (jack-html '(:section (:div (:p "foo"))))
                     "<section><div><p>foo</p></div></section>"))
    (should (string= (jack-html '(:div (:div "foo" "bar"))) "<div><div>foobar</div></div>"))
    (should (string= (jack-html '(:div "a" (:p "b" (:span "c") "d") "e"))
                     "<div>a<p>b<span>c</span>d</p>e</div>"))
    (should (string= (jack-html '(:div (:p "foo") (:p "bar")))
                     "<div><p>foo</p><p>bar</p></div>"))

    ;; component arguments in `jack-html' are concatenated
    (should (string= (jack-html "foo" "bar") "foobar"))
    (should (string= (jack-html '(:p "foo") "bar") "<p>foo</p>bar"))
    (should (string= (jack-html '(:p "foo") '(:p "bar")) "<p>foo</p><p>bar</p>"))

    ;; components in a list are concatenated at the same level
    (should (string= (jack-html '("foo" 1 "bar")) "foo1bar"))
    (should (string= (jack-html '((:li "a") (:li "b")))
                     "<li>a</li><li>b</li>"))
    (should (string= (jack-html '(:li "a") '((:li "b") (:li "c")) '(:li "d"))
                     "<li>a</li><li>b</li><li>c</li><li>d</li>"))

    ;; attributes
    (should (string= (jack-html '(:div (@ :id "id" :class "class") "foo"))
                     "<div id=\"id\" class=\"class\">foo</div>"))
    (should (string= (jack-html '(:p/id-in-tag (@ :id "id-in-plist") (:span "foo")))
                     "<p id=\"id-in-plist\"><span>foo</span></p>"))
    (should (string= (jack-html '(:p.class-in-tag (@ :class "class-in-plist") "foo"))
                     "<p class=\"class-in-tag class-in-plist\">foo</p>"))

    ;; tag content can be lists of components
    (should (string= (jack-html '(:ul ((:li "1") (:li "2"))))
                     "<ul><li>1</li><li>2</li></ul>"))
    (should (string= (jack-html '(:ul (@ :id "id") ((:li "1") (:li "2"))))
                     "<ul id=\"id\"><li>1</li><li>2</li></ul>"))

    ;; components can be generated by a form
    (should (string= (jack-html `(:p ,(concat "foo-" "bar")))
                     "<p>foo-bar</p>"))
    (should (string= (jack-html (mapcar (lambda (n) `(:p ,n)) '(1 2 3)))
                     "<p>1</p><p>2</p><p>3</p>"))

    ;; tag content can be forms
    (should (string=
             (jack-html `(:ul ,(mapcar (lambda (n) `(:li ,n)) '(1 2))))
             "<ul><li>1</li><li>2</li></ul>"))
    (should (string=
             (jack-html `(:ul (@ :id "id") ,(mapcar (lambda (n) `(:li ,n)) '(1 2))))
             "<ul id=\"id\"><li>1</li><li>2</li></ul>"))

    ;; tag content and attributes can be vars
    (should (string= (let ((x "foo") (y "bar"))
                       (jack-html `(:p (@ :id ,x) ,y)))
                     "<p id=\"foo\">bar</p>"))
    (should (string= (jack-html
                      (let ((x "foo") (y "bar"))
                        `(:p (@ :id ,x) ,y)))
                     "<p id=\"foo\">bar</p>"))

    ;; percent sign % in string tags
    (should (string= (jack-html "%") "%"))
    (should (string= (jack-html "%s") "%s"))
    (should (string= (jack-html "%s %s %s") "%s %s %s"))
    (should (string= (jack-html '(:p "%s")) "<p>%s</p>"))
    (should (string= (jack-html '(:p "% %% %%%")) "<p>% %% %%%</p>"))
    (should (string= (jack-html '(:div (:p "%s") "foo" (:p "%s")))
                     "<div><p>%s</p>foo<p>%s</p></div>"))

    ;; `jack-html' can generate html template that can be
    ;; used/consumed by the function `format'
    (should (string=
             (let ((template
                    (jack-html '(:div (@ :class "%s")
                                 (:p (@ :id "%s") "foo")
                                 (:p (@ :id "%s") "bar")))))
               (format template "container" "p-foo" "p-bar"))
             "<div class=\"container\"><p id=\"p-foo\">foo</p><p id=\"p-bar\">bar</p></div>"))))

;; (ert-deftest jack-html-lisp-nesting-test ()
;;   ;; This test exists to ensure, we don't modify the implementation
;;   ;; in a way that it fails "badly" for "deep" nested structure.  For,
;;   ;; instance for 10000 nested `:div', `jack-html' lasts about 10s
;;   ;; to return the result but it doesn't break.
;;   ;;
;;   ;; In the first implementation (recursive way) of `osta-html'
;;   ;; (commit 554d733), `osta-html' raised the error ("Lisp nesting
;;   ;; exceeds ‘max-lisp-eval-depth’") for nested list of more than
;;   ;; 50 `:div'.
;;   ;;
;;   ;; Note that at commit 851c8f6 `osta-html' has been renamed `jack-html'.
;;   (message "Might take 10 seconds or more...")
;;   (let* ((max-lisp-eval-depth 800) ; default value
;;          ;; (nested-foo-comp 3) -> (:div (:div (:div \"foo\")))
;;          (nested-foo-comp
;;           (lambda (n)
;;             (let ((comp "foo"))
;;               (dotimes (_ n) (setq comp (list :div comp)))
;;               comp)))
;;          (comp (funcall nested-foo-comp 10000)))
;;     (should (jack-html comp))))
