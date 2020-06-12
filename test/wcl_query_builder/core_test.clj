(ns wcl-query-builder.core-test
  (:require [clojure.test :refer :all]
            [wcl-query-builder.query :as q]
            [wcl-query-builder.core :as wcl]))

(def dumb-events
  (map #(assoc %1 :timestamp %2)
      [{:target {:id 1}
        :source {:id 2}
        :type :applybuff
        :ability {:guid 123
                  :name "Test"}}
       {:target {:id 2}
         :source {:id 1}
         :type :damage
         :ability {:guid 1
                   :name "Melee"}}
       {:target {:id 3}
        :source {:id 2}
        :type :applybuff
        :ability {:guid 123
                  :name "Test"}}
       {:target {:id 1}
        :source {:id 2}
        :ability {:guid 123
                  :name "Test"}
        :type :removebuff}
       {:target {:id 2}
        :source {:id 3}
        :type :heal
        :ability {:guid 1234}}
       {:target {:id 47}
        :source {:id 48}
        :type :junk}
       {:target {:id 3}
        :source {:id 2}
        :type :removebuff
        :ability {:guid 123
                  :name "Test"}}]
      (range)))

(def dumb-solution
  (list {:start (first dumb-events)
                :body (list (second dumb-events))
                :end (nth dumb-events 3)}
        {:start (nth dumb-events 2)
          :body (list (nth dumb-events 4))
          :end (last dumb-events)}))

(deftest test-basic-bracketed-range
  (is (= dumb-solution
         (wcl/post-bracketed-range {:from (q/eq :type :applybuff)
                                    :to (q/eq :type :removebuff)
                                    :group-by :target
                                    :group-on :source}
                                   dumb-events))))
