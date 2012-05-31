(+ 1 1)
;;; @ 饼
;;; # 万
;;; $ 条
;;; ~ 风
;;; % 剑
;;; ? 花
;;; abc 顺
;;; aaa 刻
;;; aaaa 杠
;;; a 单张
;;; aa 对
;;; ^ 吃/碰
;;; aa abc abc abc abc
;;; aa abc abc abc aaa
;;; aa abc abc aaa aaa
;;; aa abc aaa aaa aaa
;;; aa aaa aaa aaa aaa
;;; aa aa aa aa aa aa aa
;;; a a a a a a a a a a a a a a
;;; a a a a a a a a a a a a aa
;;; sort -> decompose 
(+ (* 4 3 9) (* 4 4) (* 4 3) 8)
(let ((result 1))
  (loop for i from 1 to 13
       do (setf result (* result i)))
  result)
;;; 234#345678@4468$
;;; 234#345678@6899$
;;; 234#345555@6899$
;;; 234#345555@6899$
;;; 555^1111-^