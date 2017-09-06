(ns com.wang.mine.demining)

;;0 1 2 3 4 5 7 8
;;
(def map_width 10)
(def map_height 10)
(def mine_count 10)

(def sign_hidden "H")
(def sign_open "O")
(def sign_anything "0")
(def sign_mine "*")

;(defn iter_array2d [_xmax _ymax _p _array2d _f]
;  (
;    for [_y (range ymax) _x (range xmax)] (f _x _y _p _array2d)
;                                          ))

(defn one_mine [_mines_map]
  (
    loop [_x (rand-int map_width) _y (rand-int map_height)]
    (
      let [_key [_x _y]]
      (
        if (not (nil? (get _mines_map _key)))
        (
          recur (rand-int map_width) (rand-int map_height)
                )
        (assoc _mines_map [_x _y] -1)
        )
      )
    ))

(defn create_mines [_count]
  (
    loop [_c _count _mines_map '{}]
    (
      if (pos? _c)
      (
        recur (dec _c) (conj _mines_map (one_mine _mines_map))
              )
      _mines_map
      )
    )
  )


(defn add_update_number [_mines _map _p]
  (
    do
    (let [_x (get _p 0) _y (get _p 1) _v (get _map _p)]
      (
        if (and (>= _x 0) (< _x map_width) (>= _y 0) (< _y map_height) (nil? (some #{_p} _mines)))
        (

          if (nil? _v) (
                         assoc _map _p 1
                                    )
                       (
                         assoc _map _p (+ _v 1)
                                    )

                       )
        _map
        ))
    ))

;;
(defn cal_digital_around [_mines _map _p]
  (
    let [_x (get _p 0) _y (get _p 1)]
    (

      loop [_x_offset -1 _map_x _map]
      (
        let [_x_temp (+ _x _x_offset)]
        (
          if (<= _x_offset 1)
          (

            recur (inc _x_offset)
                  (conj _map_x
                        (
                          loop [_y_offset -1 _map_y _map_x]
                          (
                            let [_y_temp (+ _y _y_offset)]
                            (

                              if (<= _y_offset 1)
                              (
                                do
                                (recur (inc _y_offset) (conj _map_y (add_update_number _mines _map_y (vector _x_temp _y_temp)))

                                       ))
                              _map_y
                              )
                            )
                          ))
                  )
          _map_x
          )
        )
      )
    ))

;; {[x y]: digital}
(defn init_mines []
  (
    let [_mines (create_mines mine_count) _mines_key (keys _mines)]
    (
      do
      ;(println (str "create mine: " _mines))
      (
        loop [_c 0 _map '{}]
        (
          do
          (if (< _c (count _mines)) (
                                      recur (inc _c) (conj _map (cal_digital_around _mines_key _map (nth _mines_key _c)))
                                            )
                                    (conj _map _mines)
                                    ))
        )
      )
    ))

;;[[0 1 3 0] ]
;;index:0 x
;;index:1 y
;;index:2 status
;;index:3 value
;;status:0 hidden
;;status:1 open
;;status:2 other
;;value:
(defn init_map [_mines_map]
  (

    loop [_y 0 _map_y '{}]
    (
      if (< _y map_height)
      (

        recur (inc _y)
              (conj _map_y
                    (
                      loop [_x 0 _map_x '{}]
                      (
                        if (< _x map_width)
                        (
                          let [_v (get _mines_map (vector _x _y))]
                          (
                            cond
                            (nil? _v) (recur (inc _x) (conj _map_x {[_x _y] [0 sign_hidden]}))
                            :else
                            (recur (inc _x) (conj _map_x {[_x _y] [_v sign_hidden]}))
                            )
                          )
                        _map_x
                        )
                      ))

              )
      _map_y
      )

    ))

;;[[0 1 3] ]
;;index:0 x
;;index:1 y
;;index:2 status
;;status:0 hidden
;;status:1 open
;;status:2 other
(defn paint_map [_mines_map]
  (
    loop [_y 0]
    (
      if (< _y map_height)
      (
        do
        (
          loop [_x 0] (
                        if (< _x map_width)
                        (
                          let [_status (get _mines_map (vector _x _y)) _v (get _status 0) _sign (get _status 1)]
                          (
                            do
                            (
                              cond (not= _sign sign_hidden)
                                   (
                                     if (= _v -1) (print (str sign_mine "\t"))
                                                  (print (str _v "\t"))

                                                  )
                                   :else
                                   (
                                     print "M\t"
                                           )
                                   )
                            (recur (inc _x))
                            )
                          )
                        )
                      )
        (print "\n")
        (recur (inc _y))

        )

      )

    ))

(defn open [_p _map]
  (
    let [_vs (get _map _p)]
    (
      if (and (not (nil? _vs)) (= (get _vs 1) sign_hidden))
      (
        let [_v (get _vs 0)]
        (
          cond (= _v -1) (println "mines, game over!")
               (= _v 0) (
                          do (
                               let [_x (get _p 0) _y (get _p 1)]
                               (
                                 loop [_x_offset -1 _map_x (assoc _map _p [_v sign_open])]
                                 (
                                   let [_x_temp (+ _x _x_offset)]
                                   (
                                     if (<= _x_offset 1)
                                     (
                                       recur (inc _x_offset) (conj _map_x (

                                                                            loop [_y_offset -1 _map_y _map_x]
                                                                            (
                                                                              let [_y_temp (+ _y _y_offset)]
                                                                              (
                                                                                if (<= _y_offset 1)
                                                                                (
                                                                                  do ;(println "_map_y" _map_y "_p" [_x_temp _y_temp])
                                                                                  (recur (inc _y_offset) (conj _map_y (open [_x_temp _y_temp] _map_y)))

                                                                                  )
                                                                                _map_y
                                                                                )
                                                                              )
                                                                            ))
                                             )
                                     _map_x
                                     )
                                   )
                                 )
                               )
                             )
               :else
               (
                 do                                         ;(println "_p" _p)
                 (assoc _map _p [_v sign_open])
                 )
               )
        )
      _map
      )
    ))


(defn show_all_mines [_mines _map]
  (
    do                                                      ;(println (keys _mines))
    (let [_mines_key (keys _mines)]
      (
        loop [_c 0 _m _map]
        (
          do                                                ;(println _c (count _mines_key))
          (
            if (< _c (count _mines_key))
            (
              let [_key (nth _mines_key _c) _is_mines (= (get (get _map _key) 0) -1)]
              (
                ;do (println _is_mines)
                ;   (recur (inc _c) (conj _m (assoc _m _key [-1 sign_open])))
                ;   )

                if (true? _is_mines) (
                                       recur (inc _c) (conj _m (assoc _m _key [-1 sign_open]))
                                             )
                                     (
                                       recur (inc _c) _m
                                             )
                                     )

              )
            (do
              ;(println _m)
              _m
              )
            )
          )
        )
      )))


(defn is_win [_map]
  (
    let [_keys (keys _map)]
    (
      loop [_c 0 _total_count 0]
      (
        if (< _c (count _keys))
        (
          let [_key (nth _keys _c) _vs (get _map _key) _v (get _vs 0) _status (get _vs 1)]
          (
            do
            (recur (inc _c) (if (= _status sign_hidden) (inc _total_count) _total_count))
            )
          )
        (
          do
          (if (= _total_count mine_count) true
                                          false
                                          ))
        )
      )
    )
  )

(defn init-game []
  (
    let [_mines (init_mines) _map (init_map _mines)]
    (
      loop [stop false _m _map]
      (
        if (and (false? stop) (not (nil? _m)))
        (
          do                                                ;(println _m)
          (paint_map _m)
          (print "input your location(x_y):")
          (flush)
          (let [_in (read-line)]
            (
              let [_p (vec (map read-string (clojure.string/split _in #"_"))) _is_mines (= (get _mines _p) -1)]
              (
                if (true? _is_mines)
                (
                  do
                  ;;show all mines
                  ;;game over
                  (paint_map (show_all_mines _mines _m))
                  (println "game over!")
                  (recur true nil)
                  )
                (
                  let [_new_map (open _p _m)]
                  (
                    if (is_win _new_map)
                    (
                      do (println "your win!")
                         (recur true nil)
                         )
                    (
                      do                                    ;(println _is_mines)
                      (recur false _new_map)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

(defn play_game []
  (

    loop [end false]
    (
      if (false? end)
      (
        do
        (init-game)
        (print "R(retry) Q(quit):")
        (flush)
        (let [_in (read-line)] (
                                 if (= _in "R")
                                 (
                                   recur false
                                         )
                                 (
                                   recur true
                                         )

                                 ))
        )

      )
    ))


(play_game)


