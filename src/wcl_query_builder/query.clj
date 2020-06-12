(ns wcl-query-builder.query
  (:require [clojure.core :as core]
            [clojure.string :as string]))

(defn raw
  "Escape hatch to write a raw WCL filter."
  [text]
  `(::raw ~text))

(defn and
  "Simple AND of terms. If a term is nil, it is ommitted. If all terms are nil, nil is returned."
  [& args]
  (let [actual (filter some? args)]
    (when-not (empty? actual)
        `(::and ~@actual))))

(defn or
  "Simple OR of terms. If a term is nil, it is ommitted. If all terms are nil, nil is returned."
  [& args]
  (let [actual (filter some? args)]
    (when-not (empty? actual)
      `(::or ~@actual))))

(defn eq
  [field value]
  `(::eq ~field ~value))

(defn contains-range
  "Returns true if the given filter tree contains an IN RANGE, otherwise false."
  [tree]
  (some?
   (some #{::range} (tree-seq seq? identity tree))))

(defn range
  "Generates an IN RANGE query."
  [range]
  `(::range ~range))

(defn any
  "Matches any event."
  []
  `(::any))

(defmulti eval
  "Evaluates a *SIMPLE* filter term. If term contains a range, an error is thrown."
  (fn [_ term] (first term)))

(defmethod eval ::and
  [event [_ & rest]]
  (every? identity (map (partial eval event) rest)))

(defmethod eval ::or
  [event [_ & rest]]
  ; the absence of a proper any? is annoying
  (not (not-any? identity (map (partial eval event) rest))))

(defmethod eval ::any
  [_ _]
  true)

(defn- get-
  [map key]
  (if (core/or
       (seq? key)
       (vector? key))
    (get-in map key)
    (get map key)))

(defmethod eval ::eq
  [event [_ key value]]
  (core/= (get- event key)
        value))

(defmethod eval ::range
  [_ _]
  (throw (Exception. "Cannot use eval on a range.")))

(defmethod eval ::raw
  [_ _]
  (throw (Exception. "Cannot use eval on raw WCL filters.")))

(def eval-not
  "Convenience wrapper for (complement (eval ...))"
  (complement eval))

(def by-key
  {:target [:target :id]
   :source [:source :id]
   :ability [:ability :guid]})

(defn group-key
  [by]
  (get by-key by))

(defn group-filter
  "Constructs a filter term matching a :group-by key for the given event."
  [event by]
  (if-not (nil? by)
    (let [key (group-key by)
          entity (get- event key)]
      [(eq key entity) entity])
    [(any) nil]))

(defmulti ->text
  "Converts a filter to its textual representation."
  (fn [term] (first term)))

(defmethod ->text ::and
  [[_ & args]]
  (string/join " AND " (map ->text args)))

(defn- key-name
  [key-or-seq]
  (if (core/or (seq? key-or-seq) (vector? key-or-seq))
    (string/join "." (map name key-or-seq))
    (name key-or-seq)))

(defn- val-name
  [val]
  (cond
    (keyword? val) (format "\"%s\"" (name val))
    (string? val) (format "\"%s\"" val)
    true val))

(defmethod ->text ::eq
  [[_ key value]]
  (format "%s = %s" (key-name key) (val-name value)))

(defmethod ->text ::or
  [[_ & args]]
  (string/join " OR " (map ->text args)))

(defmethod ->text ::range
  [[_ {from :from
       to :to
       group-by :group-by
       group-on :group-on}]]
  (let [from-str (when from
                   (format "from %s" (->text from)))
        to-str (when to
                 (format "to %s" (->text to)))
        group-by-str (when group-by
                       (format "group by %s" (key-name group-by)))
        group-on-str (when (core/and group-by group-on)
                       (format "on %s" (key-name group-on)))]
    (format "in range %s end" (string/join " "
                                           (filter some? [from-str to-str group-by-str group-on-str])))))
                   
(defmethod ->text ::raw
  [[_ text]]
  text)
