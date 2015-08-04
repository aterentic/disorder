(ns disorder.core)

(use 'overtone.core)

(connect-external-server 57110)

(definst bg [] (hpf (mix (saw [50 (line 100 1600 5) 101 100.5]))
                  (lin-lin (lf-tri (line 8 20 5)) -1 1 400 4000)))

(definst fx [freq 110 width 0.2 
                         attack 0.1 sustain 0 release 0.3 
                         vol 0.4] 
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (sin-osc (+ freq (* 20 (lf-pulse:kr 0.5 0 width))))
     vol))

(definst kick [freq 120 dur 0.6 width 0.6]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.4 1 0.1 0.01 0.01)))

;(kick)

(definst c-hat [amp 0.8 t 0.03]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(definst o-hat [amp 0.8 t 0.43]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))


(defcgen detuned-saw
  "A detuned saw wave."
  [freq {:default 40 :expands? true}]
  (:ar (->> freq
            (* [0.99 1.01])
            (saw)
            (apply +))))

(defcgen wobble
  "wobble the the input"
  [in   {:doc "input source to wobble"
         :default 0}
   freq {:doc "wobble frequency"
         :default 1}]
  (:ar (let [sweep (lf-tri freq)
             sweep (lin-exp sweep -1 1 40 3000)]
         (lpf in sweep))))

(defcgen wobble-saw
  "Generate a wobbly, detuned saw wave!"
  [freq     {:doc "Main frequency"
             :default 80}
   wob-freq {:doc "Wobble frequency"
             :default 1.0}]
  (:ar (-> (detuned-saw freq)
           (wobble wob-freq)
           normalizer)))


(def metro60 (metronome 60))

(def metro (metronome 120))

(defn one [seq]
  (first seq))

(defn two [seq]
  (first (rest seq)))

(defn three [seq]
  (first (drop 2 seq)))

(defn four [seq]
  (first (drop 3 seq)))

(def hat (cycle [0 0 1 0 1 0 1 0 1 1 0]))

(defn player [beat seq]
    (at (metro beat) (kick))
    (at (metro (+ 0 beat)) (if (= 0 (one seq)) (c-hat) (o-hat)))
    (at (metro (+ 0.25 beat)) (if (= 0 (two seq)) (c-hat) (o-hat)))
    (at (metro (+ 0.5 beat)) (if (= 0 (three seq)) (c-hat) (o-hat)))
    (at (metro (+ 0.75 beat)) (if (= 0 (four seq)) (c-hat) (o-hat)))
    (apply-by (metro (inc beat)) #'player (inc beat) (drop 4 seq) []))

(defn player60 [beat]
  (at (metro60 (+ 1 beat)) (fx))
  (apply-by (metro60 (inc beat)) #'player60 (inc beat) []))


(player (metro) hat)
(player60 (metro60))

(def one-twenty-bpm (metronome 120))

(defn looper [nome sound]    
    (let [beat (nome)]
        (at (nome beat) (sound))
        (apply-by (nome (inc beat)) looper nome sound [])))

(looper one-twenty-bpm kick)
(looper (metronome 180) kick)
(looper (metronome 90) c-hat)
(looper (metronome 60) fx)
(looper (metronome 30) bg)
(kick)

(kill bg)
(stop)
