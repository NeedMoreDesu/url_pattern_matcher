(ns url-pattern-matcher.core)

;;
;; Matcher should recognize and destruct URL by:
;; host: domain
;; path: parts, splitted with "/"
;; queryparam: name/value pairs of query
;; (see examples below)
;;
;; Each string that is started from "?" is a "bind"
;; (recognize matcher) should return nil or seq of binds
;;

(defrecord Pattern [host path params])

(defn parse-path [path-string]
  (let [ strings (clojure.string/split path-string #"/")
         elements (map
                    (fn [arg]
                      (if (= (first arg) \?)
                        (keyword (subs arg 1))
                        arg))
                    strings)]
    elements))

(defn parse-params [params-string]
  (let [ strings (map #(clojure.string/split % #"=") params-string)
         elements (reduce
                    (fn [dict [name right-value]]
                      (cond
                        (not right-value) dict ; failed to split
                        (= (first right-value) \?) (assoc dict name (keyword (subs right-value 1)))
                        :else (assoc dict name right-value)))
                    {}
                    strings)]
    elements))

(defn new-pattern [pattern-string]
  (let [ host (nth (re-find #"host\(([^\)]+)\)" pattern-string) 1)
         path-string (nth (re-find #"path\(([^\)]+)\)" pattern-string) 1)
         path (parse-path path-string)
         params-string (map
                       (fn [arg] (nth arg 1))
                       (re-seq #"queryparam\(([^\)]+)\)" pattern-string))
         params (parse-params params-string)]
    (->Pattern host path params)))

(defn split-url [url]
  (let [[_ _ host path _ params] (re-find #"^(https?:\/\/)?([^\/]+)\/([^\?]+)(\?(.+))?$" url)]
    [host path params]))

(defn match-path [path patterns]
  (defn match [elements patterns output-sequence]
    (let [ element (first elements)
           pattern (first patterns)]
      (cond
        (and (not element) (not pattern)) output-sequence
        (not (and element pattern (string? element))) nil
        (keyword? pattern) (recur (rest elements) (rest patterns) (cons [pattern element] output-sequence))
        (string? pattern) (if (= element pattern) (recur (rest elements) (rest patterns) output-sequence) false)
        :else false)))
  (if (string? path)
    (let [elements (clojure.string/split path #"/")]
      (match elements patterns '()))
    false))

(defn match-params [params patterns]
  (defn match [elements unused-patterns output-sequence]
    (let [element (first elements)]
      (cond
        (and (not element) (empty? unused-patterns)) output-sequence
        (not (and element (string? element))) false
        :else (let [[name value] (clojure.string/split element #"=")
                     pattern (get unused-patterns name)]
                (cond
                  (not pattern) (recur (rest elements) unused-patterns output-sequence)
                  (keyword? pattern) (recur (rest elements) (dissoc unused-patterns name) (cons [pattern value] output-sequence))
                  :else (if (= (str pattern) value)
                          (recur (rest elements) (dissoc unused-patterns name) output-sequence)
                          false))))))
  (if (string? params)
    (let [elements (clojure.string/split params #"&")]
      (match elements patterns '()))
    (if (empty? patterns) nil false)))

(defn recognize [pattern url]
  (let [ [host path params] (split-url url) ]
    (if (= host (:host pattern))
      (let [ path-binds (match-path path (:path pattern))
             params-binds (match-params params (:params pattern)) ]
        (if (or (= path-binds false) (= params-binds false))
          nil
          (concat path-binds params-binds))))))
