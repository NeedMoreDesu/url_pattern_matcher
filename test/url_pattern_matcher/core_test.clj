(ns url-pattern-matcher.core-test
  (:require [clojure.test :refer :all]
            [url-pattern-matcher.core :refer :all]))

(deftest twitter
  (testing "twitter pattern"
    (let [pattern (new-pattern "host(twitter.com); path(?user/status/?id);") ]
      (is (= (recognize pattern "http://twitter.com/bradfitz/status/562360748727611392") [[:id 562360748727611392] [:user "bradfitz"]])))))

(deftest dribble
  (testing "dribble pattern"
    (let [ dribbble (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);") ]
      (is (= (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1") [[:id "1905065-Travel-Icons-pack"] [:offset "1"]]))
      (is (= (recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1") nil) "host mismatch")
      (is (= (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users") nil) "offset queryparam missing"))))
