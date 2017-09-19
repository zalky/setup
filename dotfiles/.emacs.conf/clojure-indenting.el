;;;; Clojure and Clojurescript Indenting

;; Special forms

(define-clojure-indent
  (defrelations '(0 :defn))
  (deftypes '(0 :defn))
  (defcvs '(0 :defn))
  (add-meta '(1 :form (1)))
  (logic '(1 :form))
  (schema '(1 :form))
  (fields '(0 :form))
  (extend-instance '(1 :form :form [1]))
  (defprimitive '(2 :form :form [:defn])))

(put-clojure-indent 'if-cljs 0)
(put-clojure-indent 'p 1)
(put-clojure-indent 'profile 1)
(put-clojure-indent 'cond-class 0)

;; Let forms

(defvar my-tags
  '(if-conform
    symbol-macrolet))

(defvar om-lifecycle-tags
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
    will-unmount))

(dolist (tag (append my-tags om-lifecycle-tags))
  (put-clojure-indent tag 1))

(put-clojure-indent 'build 2)
(put-clojure-indent 'build-all 2)

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
    deflogic
    go-comm
    err!))

(dolist (tag (append builtin-fn-tags my-fn-tags))
  (put-clojure-indent tag :defn))

(defvar dom-tags
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
    input
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
    textarea
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

(defvar grid-tags
  '(inset-row
    row
    column-full
    full-row
    column-sml
    column-sm
    column-s
    column))

(defvar bootstrap-tags
  '(breadcrumb
    breadcrumb-item
    button-group
    button-toolbar
    card
    card-block
    card-header
    card-img
    card-link
    card-subtitle
    card-text
    card-title
    col
    collapse
    container
    dropdown
    dropdown-item
    dropdown-menu
    dropdown-toggle
    form
    form-group
    input-group
    input-group-addon
    menu-item
    modal
    modal-body
    modal-footer
    modal-header
    nav-item
    navbar
    page-header
    popover
    popover
    popover-content
    popover-title
    row
    toolbar
    tooltip
    tooltip))

(put-clojure-indent 'label-row 2)

(dolist (tag (append dom-tags
                     bootstrap-tags
                     grid-tags))
  (put-clojure-indent tag :defn))
