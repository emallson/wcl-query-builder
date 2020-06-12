(ns wcl-query-builder.core
  (:require [clojure.core.match :refer [match]]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [cl-format]]
            [infix.macros :refer [infix]]
            [wcl-query-builder.query :as q])
  (:gen-class))

;  this is what i want things to look like
;
;  each step should produce a map with two keys: :filter-ext and :post, defining the extension to the filter itself and the post-processing function (if applicable)
;; (-> {}
;;     (within-range {:range/type :range/bracketed
;;                    ::from (infix (q/eq :type :damage) q/and (q/> :overkill 0) q/and (q/eq :target.name "Voidspawn Annihilator"))})
;;     (select {::fields [:x :y :target.name]
;;              ::where (infix (q/eq :type :damage) q/and (q/eq :ability.id 123456) q/and (q/neq :target.role :tank))})
;;     (tag-range {:range/type :range/delimited
;;                 ::delimiter (infix (q/eq :type :begincast) q/and (q/eq :ability.name "Evoke Anguish"))}
;;                :evoke-cast)
;;     (group-by :evoke-cast))
;;

(defn select
  "Select a subset of the data. Multiple selections are implicitly ANDed together."
  [{flt ::filter
    post ::post}
   {fields ::fields
    where ::where}]
  {::filter (q/and flt where)
   ::post (if fields
            (comp (fn [events]
                    (map #(select-keys % fields) events))
                  post)
            post)})

(defn post-bracketed-range
  [{from :from
    to :to
    by :group-by
    on :group-on
    :as range}
   events]
  (let [[_ [start & rst]] (split-with #(q/eval-not % from) events)]
    (when start
      (let [[group-term entity] (q/group-filter start by)
            on-term (if (or (nil? on) (nil? entity))
                      group-term
                      (q/eq (q/group-key on) entity))
            [body [end & _]] (split-with #(q/eval-not % (q/and to group-term)) rst)]
        (conj (post-bracketed-range range rst)
              {:start start
               :body (filter #(q/eval % on-term) body)
               :end end})))))

(defn within-bracketed-range
  "Selects data that lies within a given range."
  [{flt ::filter
    post ::post}
   {from ::from
    to ::to
    :as range}]
  (if (q/contains-range flt)
    {::filter (q/or flt from to)
     ::post (comp post-bracketed-range post)}
    {::filter (q/and flt (q/range range))
     ::post post}))
