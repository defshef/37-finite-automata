(ns fsm.core-test
  (:require [clojure.test :refer :all]
            [fsm.core :as dfa]))

(def sample-rulebook
  [[1 \a 2] [1 \b 1]
   [2 \a 2] [2 \b 3]
   [3 \a 3] [3 \b 3]])

(def sample-dfa {:state 1
                 :accept-states #{3}
                 :rulebook sample-rulebook})

(deftest maybe-follow-test
  (is (#'fsm.core/maybe-follow [1 \a 2] 1 \a) "valid transition follows")
  (is (not (#'fsm.core/maybe-follow [1 \b 2] 1 \a)) "invalid transition does not follow"))

(deftest string-acceptance-test
  (are [result input-string] (= result (dfa/accept-string? sample-dfa input-string))

    :accepted "ab"
    :accepted "baba"
    :accepted "aaaab"

    :rejected "a"
    :rejected "baa"
    :rejected "bbba"))
