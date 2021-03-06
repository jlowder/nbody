(defsystem nbody
    :name "nbody"
    :version "0.1.0"
    :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
    :license "MIT"
    :description "Simple gravitational N-body simulator"
    :long-description "Interactive gravitational N-body simulator using the Euler method for integration"
    :depends-on (:cl-ppcre
                 :prompt
                 :6e)
    :components ((:module "src"
                          :components ((:file "reader")
                                       (:file "nbody" :depends-on ("reader"))))))
