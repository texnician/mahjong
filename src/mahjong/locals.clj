(ns mahjong.locals)

(def ^:static ^:const *fan-name-table*
  {:zh_CN {:big-four-winds "大四喜"
           :big-three-dragons "大三元"
           :all-green "绿一色"
           :nine-gates "九莲宝灯"
           :four-quads "四杠"
           :chained-seven-pairs "连七对"
           :thirteen-orphans "十三幺"
           :all-terminals "清幺九"
           :little-four-winds "小四喜"
           :little-three-dragons "小三元"
           :all-honors "字一色"
           :all-closed-triplets "四暗刻"
           :twin-edge-sequences-plus-center-pair "一色双龙会"
           :four-same-sequences "一色四同顺"
           :four-step-triplets "一色四节高"
           :four-step-sequences "一色四步高"
           :three-quads "三杠"
           :all-terminals-or-honors "混幺九"
           :seven-pairs "七对"
           :seven-honors-and-knitted "七星不靠"
           :all-even "全双刻"
           :one-suit-only "清一色"
           :three-same-sequences "一色三同顺"
           :three-step-triplets "一色三节高"
           :large-three-only "全大"
           :medium-three-only "全中"
           :small-three-only "全小"
           :one-suit-through "清龙"
           :three-suits-edge-sequences-plus-center-pair "三色双龙会"
           :three-step-sequences "一色三步高"
           :number-5-in-each-set "全带五"
           :three-suits-triplets "三同刻"
           :three-closed-triplets "三暗刻"
           :honors-and-knitted "全不靠"
           :knitted-through "组合龙"
           :more-than-five "大于五"
           :less-than-five "小于五"
           :three-winds-triplets "三风刻"
           :three-suits-through "花龙"
           :symmetric-tiles-only "推不倒"
           :three-suits-sequences "三色三同顺"
           :three-suits-step-triplets "三色三节高"
           :two-closed-quads "双暗杠"
           :last-drawn-tile "妙手回春"
           :last-discarded-tile "海底捞月"
           :supplemental-tile-of-melding-quad "杠上开花"
           :appended-tile-to-melded-triplet "抢杠和"
           :all-triplets "碰碰和"
           :one-suit-plus-honors "混一色"
           :three-suits-step-sequences "三色三步高"
           :five-types "五门齐"
           :others-tiles-in-each-set "全求人"
           :two-dragon-triplets "双箭刻"
           :terminals-or-honors-in-each-set "全带幺"
           :self-tiles-only "不求人"
           :two-open-quads "双明杠"
           :last-tile-other-than-revealed "和绝张"
           :dragon-triplet "箭刻"
           :prevailing-wind-triplet "门风刻"
           :game-wind-triplet "圈风刻"
           :no-melding "门清"
           :simple-sequence-hand "平和"
           :four-tiles-collection "四归一"
           :two-suits-triplets "双同刻"
           :two-closed-triplets "双暗刻"
           :closed-quad "暗杠"
           :all-simples "断幺"
           :two-same-sequences "一般高"
           :two-suits-sequences "喜相逢"
           :chain-six "连六"
           :edge-sequences-pair "老少副"
           :terminal-or-non-special-wind-triplet "幺九刻"
           :open-quad "明杠"
           :lack-one-suit "缺一门"
           :no-honor "无字"
           :one-tile-wait-for-a-edge-sequence "边张"
           :one-tile-wait-for-a-holed-sequence "坎张"
           :one-tile-wait-for-a-pair "单钓将"
           :completion-by-draw "自摸"
           :avoid-points "无番和"}})