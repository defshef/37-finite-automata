(ns fsm.core)

(def rulebook
  [[1 \a 2] [1 \b 1]
   [2 \a 2] [2 \b 3]
   [3 \a 3] [3 \b 3]])

(defn- maybe-follow [[from on-character to] state character]
  (if (and (= from state) (= on-character character)) to))

(defn- next-state [{:keys [rulebook state]} character]
  (first (keep (fn [rule] (maybe-follow rule state character)) rulebook)))

(defn- accepting? [dfa]
  (if (not= nil ((:accept-states dfa) (:state dfa)))
    :accepted
    :rejected))

(defn accept-string? [{:keys [accept-states] :as start-dfa} string]
  (accepting? (reduce (fn [dfa character]
                        (let [next-dfa (assoc dfa :state (next-state dfa character))]
                          (if (= nil (:state next-dfa))
                            (reduced nil)
                            next-dfa))) start-dfa string)))
