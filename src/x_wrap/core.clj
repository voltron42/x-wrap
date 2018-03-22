(ns x-wrap.core
  (:import (java.sql SQLException)
           (clojure.lang ExceptionInfo)))

(defn- json-stack-trace-elem [^StackTraceElement elem]
  {:class (.getClassName elem)
   :method (.getMethodName elem)
   :file (.getFileName elem)
   :line (.getLineNumber elem)})

(defn json-wrap-stack-trace [^Throwable t]
  (mapv json-stack-trace-elem (.getStackTrace t)))

(defn- filter-non-nil [data]
  (reduce-kv #(if (nil? %3) %1 (assoc %1 %2 %3)) {} data))

(defmulti json-wrap-exception-by-type type)

(defn json-wrap-exception [^Throwable throwable]
  (when-not (nil? throwable)
    (filter-non-nil (json-wrap-exception-by-type throwable))))

(defmethod json-wrap-exception-by-type :default [^Throwable throwable]
  {:type (str (type throwable))
   :cause (json-wrap-exception (.getCause throwable))
   :message (.getMessage throwable)
   :stack-trace (json-wrap-stack-trace throwable)})

(defmacro try-catch-json
  [& body]
  `(try
     ~@body
     (catch Throwable t#
       (throw (ExceptionInfo. (str t#) (json-wrap-exception t#))))))

(defn- xml-stack-trace-elem [^StackTraceElement elem]
  {:class (.getClassName elem)
   :method (.getMethodName elem)
   :file (.getFileName elem)
   :line (.getLineNumber elem)})

(defn xml-wrap-stack-trace [^Throwable t]
  {:tag :stack-trace
   :children (mapv json-stack-trace-elem (.getStackTrace t))})

(defn- filter-non-nil [data]
  (reduce-kv #(if (nil? %3) %1 (assoc %1 %2 %3)) {} data))

(defmulti xml-wrap-exception-by-type type)

(defn xml-wrap-exception [^Throwable throwable]
  (when-not (nil? throwable)
    (json-wrap-exception-by-type throwable)))

(defmethod xml-wrap-exception-by-type :default [^Throwable throwable]
  {:tag :thrown
   :attrs {:type        (str (type throwable))
           :message     (if-let [message (.getMessage throwable)] message "")}
   :children (concat
               (if-let [cause (.getCause throwable)]
                 [(xml-wrap-exception cause)] [])
               [(xml-wrap-stack-trace throwable)])})

(defmacro try-catch-xml
  [& body]
  `(try
     ~@body
     (catch Throwable t#
       (throw (ExceptionInfo. (str t#) (json-wrap-exception t#))))))

(defmulti log-exception-by-type
          (fn [exception _]
            (type exception)))

(defn log-exception [^Throwable throwable level]
  (when-not (nil? throwable)
    (filter-non-nil (log-exception-by-type throwable))))

(defmethod log-exception-by-type :default [^Throwable throwable level]
  (println level throwable))

(defmacro try-catch-log
  [level & body]
  `(try
     ~@body
     (catch Throwable t#
       (log-exception t# level)
       (throw t#))))
