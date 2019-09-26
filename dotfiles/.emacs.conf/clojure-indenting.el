;;;; Clojure and Clojurescript Indenting

;; Special forms

(define-clojure-indent
  (defrelations '(0 :defn))
  (deftypes '(0 :defn))
  (defcvs '(0 :defn))
  (add-meta '(1 :form (1)))
  (schema '(1 :form))
  (fields '(0 :form))
  (extend-instance '(1 :form :form [1]))
  (defprimitive '(2 :form :form [:defn])))

;; Specter forms

(put-clojure-indent 'if-path 1)
(put-clojure-indent 'cond-path 0)
(put-clojure-indent 'recursive-path :defn)
(put-clojure-indent 'multi-path 0)

;; Body forms: 0

(defvar third-party-body-tags
  '(if-cljs
    cond-class))

(dolist (tag third-party-body-tags)
  (put-clojure-indent tag 0))

;; Let forms: 1

(defvar my-let-tags
  '(gen-schema
    gen-partition))

(defvar third-party-let-tags
  '(p
    bind
    profile
    if-conform
    symbol-macrolet
    if-path
    for-all
    simple-benchmark))

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

(dolist (tag (append my-let-tags
                     third-party-let-tags
                     om-lifecycle-tags))
  (put-clojure-indent tag 1))

;; Build forms: 2

(defvar my-build-tags
  '(label-row
    iff))

(defvar third-party-build-tags
  '(build
    build-all))

(dolist (tag (append my-build-tags
                     third-party-build-tags))
  (put-clojure-indent tag 2))

;; Function forms: :defn

(defvar builtin-fn-tags
  '(add-watch
    load))

(defvar my-fn-tags
  '(respond-to
    request
    pod-safe-vars
    deflogic
    go-comm
    err!
    scoped-reaction
    page
    centered-box
    user-form
    maybe-conformed
    conform-to
    assocn
    assoc-inn
    update-inn))

(defvar third-party-fn-tags
  '(or-join
    not-join
    match
    fdef
    deftask
    chsk-send!
    reaction
    make-reaction
    run!
    listen
    validate))

(defvar REST-tags
  '(GET
    POST
    PUT
    DELETE
    HEAD
    OPTIONS
    PATCH
    ANY))

(defvar re-frame-tags
  '(reg-event-db
    reg-event-fx
    reg-sub
    reg-sub-raw
    reg-fx
    reg-cofx
    reg-global-interceptor
    reg-workflow
    reg-pull
    reg-attr
    reg-link
    reg-summary))

;; From om.dom/tags
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
    details
    dfn
    dialog
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
    ;; link
    main
    mark
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
    picture
    pre
    progress
    rp
    rt
    ruby
    s
    samp
    script
    section
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
    wbr))

;; From om.dom/tags
(defvar svg-tags
  '(circle
    clipPath
    ellipse
    g
    line
    mask
    path
    pattern
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
  '(alert
    badge
    breadcrumb
    breadcrumb-item
    button
    button-group
    button-dropdown
    uncontrolled-button-dropdown
    dropdown
    dropdown-toggle
    dropdown-menu
    dropdown-item
    card
    card-img
    card-block
    card-title
    card-subtitle
    card-text
    card-link
    card-header
    card-footer
    card-img-overlay
    card-group
    card-deck
    card-columns
    collapse
    form
    form-group
    form-text
    form-feedback
    label
    input
    input-group
    input-group-addon
    input-group-button
    input-group-button
    jumbotron
    container
    row
    col
    list-group
    list-group-item
    badge
    list-group-item-heading
    list-group-item-text
    media
    modal
    modal-header
    modal-body
    modal-footer
    navbar
    navbar-toggler
    navbar-brand
    nav
    nav-item
    nav-link
    nav-dropdown
    uncontrolled-nav-dropdown
    pagination
    pagination-item
    pagination-link
    popover
    popover-title
    popover-content
    progress
    table
    tab-content
    tab-pane
    tooltip
    uncontrolled-tooltip))

(dolist (tag (append builtin-fn-tags
                     my-fn-tags
                     third-party-fn-tags
                     grid-tags
                     REST-tags
                     re-frame-tags
                     dom-tags
                     svg-tags
                     bootstrap-tags))
  (put-clojure-indent tag :defn))
