(ns clap)

(use '[clojure.string :only (join split)])

(defn attrs-to-string [attrs]
  (let [aseq (seq attrs)]
    (list " " (join " " (flatten
                         (map (fn [[k v]]
                                (join (list (name k) "=\"" v  "\""))) aseq))))))
  
(defn as-html-string [tree]
  (join (flatten tree)))

(defn make-tag [tag]
  `(defn ~(symbol (name tag))

     ([]
        (vector "<" ~(name tag) "></" ~(name tag) ">"))

     ;; 1-arity case
     ([attrs-or-child#]
        (let [attr?#          (map? attrs-or-child#)
              attrs#          (if attr?# (attrs-to-string attrs-or-child#) "")
              children#       (if attr?# "" attrs-or-child#)]

          (list "<" ~(name tag) attrs# ">" children# "</" ~(name tag) ">")))
       
     ;; 2+-artiy case
     ([attrs-or-child# & children#]
        (let [attr?#          (map? attrs-or-child#)
              attrs#          (if attr?# (attrs-to-string attrs-or-child#) "")
              children2#      (if attr?# children# (cons attrs-or-child# children#))]
          
          (list "<" ~(name tag) attrs# ">" children2# "</" ~(name tag) ">")))))

(defmacro make-tags [& tags]
  `(do ~@(map (fn [tag#] (make-tag tag#)) tags)))

(make-tags :a :b :i)
;; with thanks https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/HTML5/HTML5_element_list

(make-tags
 ;; :root
 :html
 
 ;; :document :metadata
 :head :title :base :link :hmeta :style

 ;; :scripting
 :script :noscript :template

 ;; :sections
 :body :section :nav :article :aside
 :h1 :h2 :h3 :h4 :h5 :h6
 :header :footer :address :main

 ;; :grouping
 :p :hr :pre :blockquote :ol :ul :li :dl
 :dt :dd :figure :figcaption :div

 ;; :text-level :semantics
 :a :em :strong :small :s :cite :q :dfn :abbr
 :data :htime :code :var :samp :kbd :sub :sup
 :i :b :u :mark :ruby :rt :rp :bdi :bdo :span
 :br :wbr

 ;; :edits
 :ins :del
 
 ;; :embedded :content
 :img :iframe :embed :object :param :video
 :audio :source :track :canvas :hmap :area
 :svg :math
 
 ;; :tabular
 :table :caption :colgroup :col :tbody :thead
 :tfoot :tr :td :th

 ;; :forms
 :form :fieldset :legend :label :input :button
 :select :datalist :optgroup :option :textarea
 :keygen :output :progress :meter
 
 ;; :interactive
 :details :summary :menuitem :menu)

(join (flatten
       (html
        (head
         (title "This is the title"))
        (body
         (table
          (tr (td "so much data")))))))
