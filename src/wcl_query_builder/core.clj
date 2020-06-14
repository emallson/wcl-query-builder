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

(def nil-query {::filter nil
                ::post identity})

(defn select
  "Select a subset of the data. Multiple selections are implicitly ANDed together."
  [{flt ::filter
    post ::post}
   {fields :fields
    where :where}]
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
    key :key
    :as range}
   idx
   events]
  (let [[head [start & rst]] (split-with #(q/eval-not % from) events)]
    (if start
      (let [rst (post-bracketed-range range (inc idx) rst)
            [group-term entity] (q/group-filter start by)
            on-term (if (or (nil? on) (nil? entity))
                      group-term
                      (q/eq (q/group-key on) entity))
            [body [end & tail]] (split-with #(q/eval-not % (q/and to group-term)) rst)]
        (concat head
                [(update-in start [:range key :start] conj idx)]
                (map (fn [event]
                       (if (q/eval event on-term)
                         (update-in event [:range key :match] conj idx)
                         event))
                     body)
                [(update-in end [:range key :end] conj idx)]
                tail))
      events)))

(defn filter-to-range
  "Filter out all events that do not belong to a specific range."
  [key events]
  (filter #(some? (get-in % [:range key])) events))

(defmulti within-range
  "Selects data that lies within a given range."
  (fn [_ {type :range/type}] type))

(defmethod within-range :bracketed
  [{flt ::filter
    post ::post}
   {from :from
    to :to
    key :key
    :as range}]
  (if (q/contains-range flt)
    {::filter (q/or flt from to)
     ::post (comp
             (partial filter-to-range key)
             (partial post-bracketed-range range 0)
             post)}
    {::filter (q/and flt (q/range range))
     ::post (comp
             (partial post-bracketed-range range 0) ; add range indices
             post)}))

(defn dissoc-all
  [map keys]
  (apply dissoc map keys))

(defn post-delimited-range
  "Post-processing for a delimited range."
  [{delim :delimiter
    by :group-by
    on :group-on
    key :key
    :as range}
   events]
                                        ; in theory this should be a for, but i need to track the group idx
  (loop [event (first events)
         rst (rest events)
         prev []
         idx 0
         matchers {}
         ends {}]
    (let [flt (fn [[idx term]]
                (q/eval event term))
          matching (->> matchers (filter flt) (map first))
          ending (->> ends (filter flt) (map first))
          [end-term entity] (when (q/eval event delim)
                              (q/group-filter event by))
          on-term (if (or (nil? on) (nil? entity))
                    end-term
                    (q/eq (q/group-key on) entity))
          result-event (-> event
                           (update-in [:range key :match]
                                      #(apply conj % matching))
                           (update-in [:range key :end]
                                      #(apply conj % ending))
                           (update-in [:range key :start]
                                      #(if (some? end-term)
                                         (conj % idx)
                                         %)))]
      (if (empty? rst)
        (conj prev result-event)
        (recur
         (first rst)
         (rest rst)
         (conj prev result-event)
         (if (some? end-term) (inc idx) idx)
         (cond-> matchers
           true (dissoc-all matching)
           (some? on-term) (assoc idx on-term))
         (cond-> ends
           true (dissoc-all ending)
           (some? end-term) (assoc idx (q/and delim end-term))))))))

(defmethod within-range :delimited
  [{flt ::filter
    post ::post}
   {key :key
    delim :delimiter
    :as range}]
  {::filter (q/or flt delim)
   ::post (comp
           (partial filter-to-range key)
           (partial post-delimited-range range)
           post)})

(defmulti tag-range
  "Compute ranges and annotate them with tags. Items outside any range are still included."
  (fn [_ {type :range/type}] type))

(defmethod tag-range :bracketed
  [{flt ::filter
    post ::post}
   {from :from
    to :to
    key :key
    :as range}]
  {::filter (q/or flt from to)
    ::post (comp
            (partial post-bracketed-range range 0)
            post)})

(defmethod tag-range :delimited
  [{flt ::filter
    post ::post}
   {key :key
    delim :delimiter
    :as range}]
  {::filter (q/or flt delim)
   ::post (comp
           (partial filter-to-range key)
           (partial post-delimited-range range)
           post)})
  
(defn run-query
  "Runs a textual query against WCL."
  [text api-key start-ts stop-ts]
  [])

(defn split-fights
  "Split an event sequence into multiple sequences based on fight start timestamps."
  [events fight-breaks]
  events)

(defn exec
  "Execute a query."
  [{flt ::filter
    post ::post}
   api-key
   start
   stop
   fight-breaks]
  (let [txt (q/->text flt)]
    (as-> txt t
        (run-query t api-key start stop)
        (split-fights t fight-breaks)
        (map post t))))
