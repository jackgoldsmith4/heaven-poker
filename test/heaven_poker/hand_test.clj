(ns heaven-poker.hand-test
  (:require [clojure.test :refer :all]
            [heaven-poker.hand :refer :all]))

(deftest is-pair-test
  (let [hand1 '({:rank 12 :suit 3}
                {:rank 9 :suit 4}
                {:rank 6 :suit 2}
                {:rank 5 :suit 4}
                {:rank 3 :suit 1}
                {:rank 3 :suit 3}
                {:rank 2 :suit 2})
        one-pair-1 (is-pair hand1)
        hand2 '({:rank 12 :suit 3}
                {:rank 12 :suit 4}
                {:rank 10 :suit 2}
                {:rank 8 :suit 4}
                {:rank 7 :suit 1}
                {:rank 3 :suit 3}
                {:rank 2 :suit 2})
        one-pair-2 (is-pair hand2)
        hand3 '({:rank 12 :suit 2}
                {:rank 11 :suit 3}
                {:rank 9 :suit 2}
                {:rank 8 :suit 4}
                {:rank 7 :suit 2}
                {:rank 4 :suit 3}
                {:rank 2 :suit 1})
        one-pair-3 (is-pair hand3)]
    (testing "Hand with one pair: threes"
      (is (= (:strength one-pair-1) 2))
      (is (= (:made-hand one-pair-1 '({:rank 3 :suit 1} {:rank 3 :suit 3} {:rank 12 :suit 3} {:rank 9 :suit 4} {:rank 6 :suit 2})))))
    (testing "Hand with one pair: aces"
      (is (= (:strength one-pair-2) 2))
      (is (= (:made-hand one-pair-2 '({:rank 12 :suit 3} {:rank 12 :suit 4} {:rank 10 :suit 2} {:rank 8 :suit 4} {:rank 7 :suit 1})))))
    (testing "Hand without pair"
      (is (nil? one-pair-3)))))

(deftest is-two-pair-test
  (let [hand1 '({:rank 12 :suit 3}
                {:rank 12 :suit 4}
                {:rank 6 :suit 2}
                {:rank 5 :suit 4}
                {:rank 3 :suit 1}
                {:rank 3 :suit 3}
                {:rank 2 :suit 2})
        two-pair-1 (is-two-pair hand1)
        hand2 '({:rank 13 :suit 3}
                {:rank 12 :suit 4}
                {:rank 6 :suit 2}
                {:rank 5 :suit 4}
                {:rank 3 :suit 1}
                {:rank 3 :suit 3}
                {:rank 2 :suit 2})
        two-pair-2 (is-two-pair hand2)
        hand3 '({:rank 12 :suit 3}
                {:rank 12 :suit 4}
                {:rank 10 :suit 1}
                {:rank 6 :suit 2}
                {:rank 5 :suit 4}
                {:rank 3 :suit 3}
                {:rank 2 :suit 2})
        two-pair-3 (is-two-pair hand3)
        hand4 '({:rank 14 :suit 3}
                {:rank 12 :suit 4}
                {:rank 10 :suit 1}
                {:rank 6 :suit 2}
                {:rank 5 :suit 4}
                {:rank 3 :suit 3}
                {:rank 2 :suit 2})
        two-pair-4 (is-two-pair hand4)]
    (testing "Hand with two pair: queens and threes"
      (is (= (:strength two-pair-1) 3))
      (is (= (:made-hand two-pair-1) '({:rank 12 :suit 3} {:rank 12 :suit 4} {:rank 3 :suit 1} {:rank 3 :suit 3} {:rank 6 :suit 2}))))
    (testing "Hand with one pair: threes"
      (is (nil? two-pair-2)))
    (testing "Hand with one pair: queens"
      (is (nil? two-pair-3)))
    (testing "Hand with no pair"
      (is (nil? two-pair-4)))))

(deftest is-trips-test
  (let [hand1 '({:rank 12 :suit 2}
                {:rank 11 :suit 3}
                {:rank 9 :suit 2}
                {:rank 7 :suit 4}
                {:rank 7 :suit 2}
                {:rank 7 :suit 3}
                {:rank 2 :suit 1})
        trips-1 (is-trips hand1)
        hand2 '({:rank 12 :suit 2}
                {:rank 11 :suit 3}
                {:rank 9 :suit 2}
                {:rank 8 :suit 4}
                {:rank 7 :suit 2}
                {:rank 7 :suit 3}
                {:rank 2 :suit 1})
        trips-2 (is-trips hand2)]
    (testing "Hand with trips: sevens"
      (is (= (:strength trips-1) 4))
      (is (= (:made-hand trips-1) '({:rank 7 :suit 4} {:rank 7 :suit 2} {:rank 7 :suit 3} {:rank 12 :suit 2} {:rank 11 :suit 3}))))
    (testing "Hand without trips"
      (is (nil? trips-2)))))

(deftest is-straight-test
  (let [hand1 '({:rank 12 :suit 2}
                {:rank 10 :suit 3}
                {:rank 9 :suit 2}
                {:rank 8 :suit 4}
                {:rank 7 :suit 2}
                {:rank 6 :suit 3}
                {:rank 2 :suit 1})
        straight-1 (is-straight hand1)
        hand2 '({:rank 14 :suit 2}
                {:rank 11 :suit 3}
                {:rank 9 :suit 2}
                {:rank 5 :suit 4}
                {:rank 4 :suit 2}
                {:rank 3 :suit 3}
                {:rank 2 :suit 1})
        straight-2 (is-straight hand2)
        hand3 '({:rank 12 :suit 2}
                {:rank 11 :suit 3}
                {:rank 9 :suit 2}
                {:rank 8 :suit 4}
                {:rank 7 :suit 2}
                {:rank 7 :suit 3}
                {:rank 2 :suit 1})
        straight-3 (is-straight hand3)
        hand4 '({:rank 12 :suit 2}
                {:rank 11 :suit 3}
                {:rank 11 :suit 2}
                {:rank 10 :suit 4}
                {:rank 9 :suit 2}
                {:rank 8 :suit 1}
                {:rank 2 :suit 1})
        straight-4 (is-straight hand4)
        hand5 '({:rank 14 :suit 1}
                {:rank 7 :suit 3}
                {:rank 5 :suit 2}
                {:rank 4 :suit 2}
                {:rank 3 :suit 4}
                {:rank 3 :suit 3}
                {:rank 2 :suit 1})
        straight-5 (is-straight hand5)]
    (testing "Hand with straight: ten-high"
      (is (= (:strength straight-1) 5))
      (is (= (:made-hand straight-1) '({:rank 10 :suit 3} {:rank 9 :suit 2} {:rank 8 :suit 4} {:rank 7 :suit 2} {:rank 6 :suit 3}))))
    (testing "Hand with straight: wheel"
      (is (= (:strength straight-2) 5))
      (is (= (:made-hand straight-2) '({:rank 5 :suit 4} {:rank 4 :suit 2} {:rank 3 :suit 3} {:rank 2 :suit 1} {:rank 14 :suit 2}))))
    (testing "Hand without straight"
      (is (nil? straight-3)))
    (testing "Hand with straight on a paired board (edge case)"
      (is (= (:strength straight-4) 5))
      (is (= (map #(:rank %) (:made-hand straight-4)) '(12 11 10 9 8))))
    (testing "Hand with wheel straight on a paired board (edge case)"
      (is (= (:strength straight-5) 5))
      (is (or (= (map #(:rank %) (:made-hand straight-5)) '(5 4 3 2 14)))))))

(deftest is-flush-test
  (let [hand1 '({:rank 12 :suit 1}
                {:rank 10 :suit 1}
                {:rank 9 :suit 1}
                {:rank 8 :suit 1}
                {:rank 6 :suit 1}
                {:rank 6 :suit 3}
                {:rank 2 :suit 1})
        flush-1 (is-flush hand1)
        hand2 '({:rank 12 :suit 4}
                 {:rank 10 :suit 1}
                 {:rank 9 :suit 1}
                 {:rank 8 :suit 1}
                 {:rank 6 :suit 1}
                 {:rank 6 :suit 3}
                 {:rank 2 :suit 4})
        flush-2 (is-flush hand2)
        hand3 '({:rank 12 :suit 3}
                {:rank 12 :suit 1}
                {:rank 9 :suit 3}
                {:rank 8 :suit 3}
                {:rank 6 :suit 3}
                {:rank 6 :suit 1}
                {:rank 2 :suit 3})
        flush-3 (is-flush hand3)]
    (testing "Hand with flush: spades"
      (is (= (:strength flush-1) 6))
      (is (= (:made-hand flush-1) '({:rank 12 :suit 1} {:rank 10 :suit 1} {:rank 9 :suit 1} {:rank 8 :suit 1} {:rank 6 :suit 1}))))
    (testing "Hand without flush"
      (is (nil? flush-2)))
    (testing "Hand with flush on paired board: diamonds"
      (is (= (:strength flush-3) 6))
      (is (= (:made-hand flush-3) '({:rank 12 :suit 3} {:rank 9 :suit 3} {:rank 8 :suit 3} {:rank 6 :suit 3} {:rank 2 :suit 3}))))))

(deftest is-full-house-test
  (let [hand1 '({:rank 12 :suit 1}
                {:rank 12 :suit 2}
                {:rank 9 :suit 1}
                {:rank 8 :suit 1}
                {:rank 6 :suit 1}
                {:rank 6 :suit 3}
                {:rank 6 :suit 2})
        full-house-1 (is-full-house hand1)
        hand2 '({:rank 12 :suit 4}
                {:rank 10 :suit 1}
                {:rank 9 :suit 1}
                {:rank 8 :suit 1}
                {:rank 6 :suit 1}
                {:rank 6 :suit 3}
                {:rank 6 :suit 4})
        full-house-2 (is-full-house hand2)
        hand3 '({:rank 12 :suit 1}
                {:rank 12 :suit 2}
                {:rank 12 :suit 3}
                {:rank 8 :suit 1}
                {:rank 6 :suit 1}
                {:rank 6 :suit 3}
                {:rank 4 :suit 2})
        full-house-3 (is-full-house hand3)
        hand4 '({:rank 12 :suit 1}
                {:rank 12 :suit 2}
                {:rank 12 :suit 3}
                {:rank 6 :suit 1}
                {:rank 6 :suit 3}
                {:rank 6 :suit 4}
                {:rank 4 :suit 2})
        full-house-4 (is-full-house hand4)]
    (testing "Hand with full house: sixes full of queens"
      (is (= (:strength full-house-1) 7))
      (is (= (:made-hand full-house-1) '({:rank 6 :suit 1} {:rank 6 :suit 3} {:rank 6 :suit 2} {:rank 12 :suit 1} {:rank 12 :suit 2}))))
    (testing "Hand without full house"
      (is (nil? full-house-2)))
    (testing "Hand with full house: queens full of sixes"
      (is (= (:strength full-house-3) 7))
      (is (= (:made-hand full-house-3) '({:rank 12 :suit 1} {:rank 12 :suit 2} {:rank 12 :suit 3} {:rank 6 :suit 1} {:rank 6 :suit 3}))))
    (testing "Hand with full house: two sets of trips, higher FH is returned"
      (is (= (:strength full-house-4) 7))
      (is (= (map #(:rank %) (:made-hand full-house-4)) '(12 12 12 6 6))))))

(deftest is-quads-test
  (let [hand1 '({:rank 14 :suit 1}
                {:rank 12 :suit 1}
                {:rank 12 :suit 2}
                {:rank 12 :suit 3}
                {:rank 12 :suit 4}
                {:rank 6 :suit 3}
                {:rank 6 :suit 2})
        quads-1 (is-quads hand1)
        hand2 '({:rank 9 :suit 4}
                {:rank 9 :suit 2}
                {:rank 9 :suit 3}
                {:rank 2 :suit 1}
                {:rank 2 :suit 2}
                {:rank 2 :suit 3}
                {:rank 2 :suit 4})
        quads-2 (is-quads hand2)
        hand3 '({:rank 9 :suit 4}
                {:rank 9 :suit 2}
                {:rank 9 :suit 3}
                {:rank 3 :suit 1}
                {:rank 2 :suit 2}
                {:rank 2 :suit 3}
                {:rank 2 :suit 4})
        quads-3 (is-quads hand3)]
    (testing "Hand with quads: queens"
      (is (= (:strength quads-1) 8))
      (is (= (:made-hand quads-1) '({:rank 12 :suit 1} {:rank 12 :suit 2} {:rank 12 :suit 3} {:rank 12 :suit 4} {:rank 14 :suit 1}))))
    (testing "Hand with quads: twos"
      (is (= (:strength quads-2) 8))
      (is (= (:made-hand quads-2) '({:rank 2 :suit 1} {:rank 2 :suit 2} {:rank 2 :suit 3} {:rank 2 :suit 4} {:rank 9 :suit 4}))))
    (testing "Hand without quads"
      (is (nil? quads-3)))))

(deftest is-straight-flush-test
  (let [hand1 '({:rank 12 :suit 1}
                {:rank 11 :suit 1}
                {:rank 10 :suit 1}
                {:rank 9 :suit 1}
                {:rank 8 :suit 1}
                {:rank 6 :suit 3}
                {:rank 6 :suit 1})
        straight-flush-1 (is-straight-flush hand1)
        hand2 '({:rank 14 :suit 4}
                {:rank 9 :suit 2}
                {:rank 9 :suit 3}
                {:rank 5 :suit 4}
                {:rank 4 :suit 4}
                {:rank 3 :suit 4}
                {:rank 2 :suit 4})
        straight-flush-2 (is-straight-flush hand2)
        hand3 '({:rank 9 :suit 4}
                {:rank 9 :suit 2}
                {:rank 9 :suit 3}
                {:rank 3 :suit 1}
                {:rank 2 :suit 2}
                {:rank 2 :suit 3}
                {:rank 2 :suit 4})
        straight-flush-3 (is-straight-flush hand3)
        hand4 '({:rank 14 :suit 4}
                {:rank 9 :suit 2}
                {:rank 6 :suit 3} ; higher straight starting at six, but s-f is the wheel
                {:rank 5 :suit 4}
                {:rank 4 :suit 4}
                {:rank 3 :suit 4}
                {:rank 2 :suit 4})
        straight-flush-4 (is-straight-flush hand4)]
    (testing "Hand with straight flush: queen high"
      (is (= (:strength straight-flush-1) 9))
      (is (= (:made-hand straight-flush-1) '({:rank 12 :suit 1} {:rank 11 :suit 1} {:rank 10 :suit 1} {:rank 9 :suit 1} {:rank 8 :suit 1}))))
    (testing "Hand with straight flush: wheel"
      (is (= (:strength straight-flush-2) 9))
      (is (= (:made-hand straight-flush-2) '({:rank 5 :suit 4} {:rank 4 :suit 4} {:rank 3 :suit 4} {:rank 2 :suit 4} {:rank 14 :suit 4}))))
    (testing "Hand without straight flush"
      (is (nil? straight-flush-3)))
    (testing "Nasty case: hand with straight flush but a higher straight"
      (is (= (:strength straight-flush-4) 9))
      (is (= (:made-hand straight-flush-4) '({:rank 5 :suit 4} {:rank 4 :suit 4} {:rank 3 :suit 4} {:rank 2 :suit 4} {:rank 14 :suit 4}))))))

(deftest rank-hand-test
  (let [hand1 '({:rank 14 :suit 1}
                {:rank 13 :suit 2}
                {:rank 12 :suit 2}
                {:rank 11 :suit 1}
                {:rank 10 :suit 1}
                {:rank 6 :suit 1}
                {:rank 4 :suit 1})
        ranking1 (rank-hand hand1)
        hand2 '({:rank 9 :suit 4}
                {:rank 8 :suit 2}
                {:rank 7 :suit 1}
                {:rank 6 :suit 4}
                {:rank 4 :suit 3}
                {:rank 3 :suit 4}
                {:rank 2 :suit 1})
        ranking2 (rank-hand hand2)
        hand3 '({:rank 14 :suit 1}
                {:rank 13 :suit 3}
                {:rank 13 :suit 3}
                {:rank 13 :suit 1}
                {:rank 10 :suit 2}
                {:rank 10 :suit 4}
                {:rank 10 :suit 1})
        ranking3 (rank-hand hand3)]
    (testing "Hand with a flush and a straight: returns flush"
      (is (= (:strength ranking1) 6))
      (is (= (:made-hand ranking1) '({:rank 14 :suit 1} {:rank 11 :suit 1} {:rank 10 :suit 1} {:rank 6 :suit 1} {:rank 4 :suit 1}))))
    (testing "Hand with just a high card: passes through all helper methods as false"
      (is (= (:strength ranking2) 1))
      (is (= (:made-hand ranking2) '({:rank 9 :suit 4} {:rank 8 :suit 2} {:rank 7 :suit 1} {:rank 6 :suit 4} {:rank 4 :suit 3}))))
    (testing "Hand with two sets of trips: returns full house"
      (is (= (:strength ranking3) 7))
      (is (= (map #(:rank %) (:made-hand ranking3)) '(13 13 13 10 10))))))

(deftest compare-hand-ranks-test
  (let [ranking1 {:strength 5 :made-hand '({:rank 14 :suit 3} {:rank 13 :suit 4} {:rank 12 :suit 1} {:rank 11 :suit 3} {:rank 10 :suit 4})}
        ranking2 {:strength 1 :made-hand '({:rank 14 :suit 3} {:rank 13 :suit 4} {:rank 12 :suit 1} {:rank 11 :suit 3} {:rank 6 :suit 4})}
        ranking3 {:strength 2 :made-hand '({:rank 14 :suit 1} {:rank 14 :suit 2} {:rank 13 :suit 3} {:rank 9 :suit 4} {:rank 7 :suit 4})}
        ranking4 {:strength 2 :made-hand '({:rank 14 :suit 1} {:rank 14 :suit 2} {:rank 12 :suit 3} {:rank 9 :suit 4} {:rank 7 :suit 4})}
        ranking5 {:strength 1 :made-hand '({:rank 13 :suit 4} {:rank 12 :suit 1} {:rank 11 :suit 3} {:rank 6 :suit 4} {:rank 3 :suit 3})}
        ranking6 {:strength 2 :made-hand '({:rank 14 :suit 2} {:rank 14 :suit 1} {:rank 10 :suit 1} {:rank 8 :suit 3} {:rank 7 :suit 3})}
        ranking7 {:strength 2 :made-hand '({:rank 7 :suit 4} {:rank 7 :suit 3} {:rank 14 :suit 1} {:rank 8 :suit 3} {:rank 6 :suit 3})}
        ranking8 {:strength 5 :made-hand '({:rank 14 :suit 4} {:rank 13 :suit 4} {:rank 12 :suit 1} {:rank 11 :suit 3} {:rank 10 :suit 1})}]
    (testing "Straight wins over Ace King high"
      (is (= (compare-hand-ranks (list ranking1 ranking2)) (list ranking1))))
    (testing "Split: exact same straight returns both rankings"
      (is (= (compare-hand-ranks (list ranking1 ranking1)) (list ranking1 ranking1))))
    (testing "Both hands have one pair, but one has a better kicker"
      (is (= (compare-hand-ranks (list ranking3 ranking4)) (list ranking3))))
    (testing "Ace high wins over King high"
      (is (= (compare-hand-ranks (list ranking5 ranking2)) (list ranking2))))
    (testing "Pair of aces wins over pair of 7s"
      (is (= (compare-hand-ranks (list ranking6 ranking7)) (list ranking6))))
    (testing "Same straight with different suits returns both rankings"
      (is (= (compare-hand-ranks (list ranking1 ranking8)) (list ranking1 ranking8))))))