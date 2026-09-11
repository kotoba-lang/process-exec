(ns kotoba.process.exec
  "exec -- addressed on its own.

  Split out of kotoba.lang.process on 2026-09-09 (ADR-2609091200). The unit
  here is the DEFINITION, and this repo's deps.edn names exactly the
  definitions it reaches -- nothing else.
"
  (:require [kotoba.process.max-stdout-bytes :refer [max-stdout-bytes]]
            [kotoba.process.max-timeout-ms :refer [max-timeout-ms]]
            [kotoba.process.read-bounded :refer [read-bounded]]
            [kotoba.process.validate-spawn :refer [validate-spawn]])
  #?(:cljs (:require ["child_process" :as cp]))
  #?(:clj
     (:import (java.io ByteArrayOutputStream InputStream)
              (java.nio.charset StandardCharsets)
              (java.util.concurrent TimeUnit))))

(defn exec
  "Run `argv` as an exec-array — never through a shell — capturing stdout.

  Portable replacement for shelling out in .cljc code: returns
  `{:status N :stdout string :stderr string}`. The command is executed
  directly with `argv` vector (JVM: `java.lang.ProcessBuilder` directly —
  same real-spawn primitive `kotoba.lang.process-host/sh` uses, this library
  does NOT require `clojure.java.shell`; CLJS: `child_process` array exec —
  no `shell: true`, no string shellouts), so argv values never round-trip
  through a shell. A missing/invald command fails closed: non-zero `:status`,
  no throw.

  `argv` must be a non-empty sequential of strings. Bounds match
  `validate-spawn` (max-argv 64, per-arg byte cap, path-command/backslash
  rejection). Pass an optional `allowed` set as the second arg to enforce a
  basename allowlist (production callers should)."
  ([argv] (exec argv nil))
  ([argv allowed]
   (let [err (validate-spawn argv max-stdout-bytes max-timeout-ms allowed)]
     (if err
       {:status 127 :stdout "" :stderr (str "exec rejected: " (name err))}
       #?(:clj
          (try
            (let [pb (doto (ProcessBuilder. ^java.util.List (vec (map str argv)))
                       (.redirectErrorStream false))
                  proc (.start pb)
                  ;; Read stdout/stderr concurrently (same reason sh-transport!
                  ;; does: a large child output must not deadlock against us
                  ;; still holding stdin open).
                  stdout-f (future (read-bounded (.getInputStream proc) (long max-stdout-bytes)))
                  stderr-f (future (read-bounded (.getErrorStream proc) (long max-stdout-bytes)))]
              ;; exec never pipes :in — close stdin immediately so any child
              ;; that reads stdin sees EOF rather than hanging.
              (.close (.getOutputStream proc))
              (let [finished (.waitFor proc (long max-timeout-ms) TimeUnit/MILLISECONDS)]
                (if-not finished
                  (do (.destroyForcibly proc)
                      {:status 127
                       :stdout ""
                       :stderr (str "exec timeout after " max-timeout-ms "ms")})
                  {:status (long (.exitValue proc))
                   :stdout (str @stdout-f)
                   :stderr (str @stderr-f)})))
            ;; ProcessBuilder/.start() throws IOException when the binary is
            ;; missing (it does not fail closed by itself) — this wrapper is
            ;; what turns that (and any other spawn-time failure) into a
            ;; non-zero status, never a throw.
            (catch Exception e
              {:status 127
               :stdout ""
               :stderr (or (.getMessage e) "exec failed")}))
          :cljs
          (try
            (let [r (cp/execFileSync (first argv) (subvec argv 1)
                                     #js {:encoding "utf8"
                                          :maxBuffer max-stdout-bytes
                                          :windowsHide true})]
              {:status 0 :stdout (str r) :stderr ""})
            (catch :default e
              {:status (long (or (.-status e) 1))
               :stdout (str (or (.-stdout e) ""))
               :stderr (str (or (.-stderr e) (.-message e) "exec failed"))})))))))
