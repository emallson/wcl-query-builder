(ns wcl-query-builder.query-test
  (:require [wcl-query-builder.query :as q]
            [clojure.test :refer :all]))

(deftest test-and
  (is (= (q/and nil) nil)
      "and returns nil if no non-nil are provided")
  (is (= (q/and :a :b) '(::q/and :a :b)))
  (is (= (q/->text (q/and (q/eq :a 1))) "a = 1")
      "text-and does not add ANDs unless there are multiple terms")
  (is (= (q/->text (q/and (q/eq :a 2) (q/eq :b 2))) "a = 2 AND b = 2")
      "text-and adds ANDs between terms"))

(deftest test-range
  (is (= (q/->text (q/range {:from (q/raw "a")
                             :to (q/raw "b")}))
         "in range from a to b end")
      "group-less range")
  (is (= (q/->text (q/range {:from (q/raw "a")
                             :to (q/raw "b")
                             :group-by :target
                             :group-on :source}))
         "in range from a to b group by target on source end")
      "range with group by .. on ..")
  (is (thrown-with-msg? Exception #"Cannot use eval on a range."
                        (q/eval {:a :b} (q/and (q/and (q/eq :a :b) (q/range {})))))
      "ranges cannot be eval'd"))

(deftest test-eq
  (is (= (q/->text (q/eq :a :b)) "a = \"b\"")
      "eq should wrap keyword targets in quotes")
  (is (= (q/->text (q/eq :a "test")) "a = \"test\""))
  (is (= (q/->text (q/eq [:target :id] 1)) "target.id = 1"))
  (is (true? (q/eval {:a "test"} (q/eq :a "test"))))
  (is (true? (q/eval {:target {:id 1} :id 2} (q/eq [:target :id] 1)))))

(deftest test-raw
  (is (thrown-with-msg? Exception #"Cannot use eval on raw WCL filters."
                        (q/eval {} (q/raw "test")))
      "raw filters cannot be eval'd")
  (is (= (q/->text (q/raw "test")) "test")))
