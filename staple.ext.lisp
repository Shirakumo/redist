(staple:load-system-quietly "staple-markless")

(defmethod staple:subsystems ((system (eql (asdf:find-system "redist")))) ())
