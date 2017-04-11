;;;; Clojure and Clojurescript Indenting

;; Special forms

(define-clojure-indent
  (defrelations '(0 :defn))
  (deftypes '(0 :defn))
  (deflogic '(0 :defn))
  (add-meta '(1 :form (1)))
  (extend-instance '(1 :form :form [1])))

(put-clojure-indent 'if-cljs 0)

;; Let forms

(defvar builtin-tags
  '(if-conform
    symbol-macrolet))

(defvar om-fn-tags
  '(init-state
    will-mount
    did-mount
    should-update
    will-receive-props
    will-update
    did-update
    render
    render-state
    display-name
    will-unmount
    build
    build-all))

(dolist (tag (append builtin-tags my-let-tags om-fn-tags))
  (put-clojure-indent tag 1))

;; Function forms

(defvar builtin-fn-tags
  '(or-join
    not-join
    match
    fdef
    deftask
    transact!
    update!))

(defvar my-fn-tags
  '(respond-to
    pod-safe-vars
    go-comm
    err!))

(defvar grid-helpers
  '(inset-row
    row
    column-full
    full-row
    column-sml
    column-sm
    column-s
    column))

(dolist (tag (append builtin-fn-tags my-fn-tags grid-helpers))
  (put-clojure-indent tag :defn))

(defvar my-dom-tags
  '(a
    abbr
    address
    area
    article
    aside
    audio
    b
    base
    bdi
    bdo
    big
    blockquote
    body
    br
    button
    canvas
    caption
    cite
    code
    col
    colgroup
    data
    datalist
    dd
    del
    dfn
    div
    dl
    dt
    em
    embed
    fieldset
    figcaption
    figure
    footer
    form
    h1
    h2
    h3
    h4
    h5
    h6
    head
    header
    hr
    html
    i
    iframe
    img
    ins
    kbd
    keygen
    label
    legend
    li
    link
    main
    ;; map
    mark
    marquee
    menu
    menuitem
    meta
    meter
    nav
    noscript
    object
    ol
    optgroup
    output
    p
    param
    pre
    progress
    q
    rp
    rt
    ruby
    s
    samp
    script
    section
    select
    small
    source
    span
    strong
    style
    sub
    summary
    sup
    table
    tbody
    td
    tfoot
    th
    thead
    time
    title
    tr
    track
    u
    ul
    var
    video
    wbr
    
    ;; svg
    circle
    ellipse
    g
    line
    path
    polyline
    rect
    svg
    text
    defs
    linearGradient
    polygon
    radialGradient
    stop
    tspan))

(dolist (tag my-dom-tags)
  (let ((tag (intern (concat "dom/" (symbol-name tag)))))
    (put-clojure-indent tag :defn)))

