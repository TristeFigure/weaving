(ns weaving.doo-test
  (:require [cljs.test :as test]
            [doo.runner :refer-macros [doo-tests]]
            [weaving.core-test]))

(enable-console-print!)
(doo-tests 'weaving.core-test)
