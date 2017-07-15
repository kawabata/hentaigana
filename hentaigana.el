;;; hentaigana.el --- Hentaigana Input Method and tools.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  川幡 太一

;; Filename: hentaigana.el
;; Description: Hentaigana Input Method
;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>
;; Created: 2017-07-10
;; Version: 1.170715
;; Package-Requires: ((dash "2.8") (cl-lib "1.0"))
;; Keywords: i18n
;; URL: https://github.com/kawabata/hentaigana

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Hentaigana Input methods and other tools.
;;
;; This tool proides following four commands, along with input method `hentaigana'.
;; M-x hentaigana
;; M-x hentaigana-region
;; M-x hentaigana-to-kana-region
;; M-x hentaigana-to-kanji-region

;; Customization Variables
;; - hentaigana-use-new-kanji :: `t' if use new kanji. `nil' if use old kanji.
;;   Otherwise both kanjis are put together with braces.
;; - hentaigana-preferred :: alist of (kana . hentaigana-list)
;;   preferences. If single character is specified, convert to that
;;   character. If multiple characters are provided, list of these
;;   characters will be put togher with braces. If not specified, all
;;   characters will be placed.

;;; Code:

(require 'quail)
(require 'dash)

;; Customized variables

(defgroup hentaigana nil
  "Options specific to Hentaigana."
  :tag "Hentaigana"
  :group 'i18n)

;;(defcustom hentaigana-preference-alist
;;  '(;; Followings are examples
;;    ;; (?あ ?𛀂) (?い ?𛀆 ?𛀇 ?𛀈)
;;    )
;;  "Alist of preferred hentaigana."
;;  :group 'hentaigana
;;  :type 'list)

(defcustom hentaigana-use-new-kanji t
  "Use new Kanji instead of Old."
  :group 'hentaigana
  :type '(choice (const t) (const nil)))

(defconst hentaigana-transliteration-alist
  '(( "a" . ?あ) ( "i" . ?い) ( "u" . ?う) ( "e" . ?え) ( "o" . ?お)
    ("ka" . ?か) ("ki" . ?き) ("ku" . ?く) ("ke" . ?け) ("ko" . ?こ)
    ("ga" . ?が) ("ki" . ?ぎ) ("ku" . ?ぐ) ("ke" . ?げ) ("ko" . ?ご)
    ("sa" . ?さ) ("si" . ?し) ("su" . ?す) ("se" . ?せ) ("so" . ?そ)
                ("shi" . ?し)
    ("ta" . ?た) ("ti" . ?ち) ("tu" . ?つ) ("te" . ?て) ("to" . ?と)
                ("chi" . ?ち) ("tsu" . ?つ)
    ("na" . ?な) ("ni" . ?に) ("nu" . ?ぬ) ("ne" . ?ね) ("no" . ?の)
    ("ha" . ?は) ("hi" . ?ひ) ("hu" . ?ふ) ("he" . ?へ) ("ho" . ?ほ)
    ("ma" . ?ま) ("mi" . ?み) ("mu" . ?む) ("me" . ?め) ("mo" . ?も)
    ("ya" . ?や)              ("yu" . ?ゆ)              ("yo" . ?よ)
    ("ra" . ?ら) ("ri" . ?り) ("ru" . ?る) ("re" . ?れ) ("ro" . ?ろ)
    ("la" . ?ら) ("li" . ?り) ("lu" . ?る) ("le" . ?れ) ("lo" . ?ろ)
    ("wa" . ?わ) ("wi" . ?ゐ) ("wu" . ?う) ("we" . ?ゑ) ("wo" . ?を)
    ("n'" . ?ん)

    ("ga" . ?が) ("gi" . ?ぎ) ("gu" . ?ぐ) ("ge" . ?げ) ("go" . ?ご)
    ("za" . ?ざ) ("zi" . ?じ) ("zu" . ?ず) ("ze" . ?ぜ) ("zo" . ?ぞ)
                 ("ji" . ?じ)
    ("da" . ?だ) ("di" . ?ぢ) ("du" . ?づ) ("de" . ?で) ("do" . ?ど)
    ("ba" . ?ば) ("bi" . ?び) ("bu" . ?ぶ) ("be" . ?べ) ("bo" . ?ぼ)
    ("pa" . ?ぱ) ("pi" . ?ぴ) ("pu" . ?ぷ) ("pe" . ?ぺ) ("po" . ?ぽ)
    ))

(defconst hentaigana-alist
  '((?あ . "𛀂𛀃𛀄𛀅")
    (?い . "𛀆𛀇𛀈𛀉")
    (?う . "𛀊𛀋𛀌𛀍𛀎")
    (?え . "𛀁𛀏𛀐𛀑𛀒𛀓")
    (?お . "𛀔𛀕𛀖")
    (?か . "𛀗𛀘𛀙𛀚𛀛𛀜𛀝𛀞𛀟𛀠𛀡𛀢")
    (?き . "𛀣𛀤𛀥𛀦𛀧𛀨𛀩𛀪𛀻")
    (?く . "𛀫𛀬𛀭𛀮𛀯𛀰𛀱")
    (?け . "𛀲𛀳𛀴𛀵𛀶𛀷𛀢")
    (?こ . "𛀸𛀹𛀺𛀻𛂘")
    (?さ . "𛀼𛀽𛀾𛀿𛁀𛁁𛁂𛁃")
    (?し . "𛁄𛁅𛁆𛁇𛁈𛁉")
    (?す . "𛁊𛁋𛁌𛁍𛁎𛁏𛁐𛁑")
    (?せ . "𛁒𛁓𛁔𛁕𛁖")
    (?そ . "𛁗𛁘𛁙𛁚𛁛𛁜𛁝")
    (?た . "𛁞𛁟𛁠𛁡")
    (?ち . "𛁢𛁣𛁤𛁥𛁦𛁧𛁨")
    (?つ . "𛁩𛁪𛁫𛁬𛁭")
    (?て . "𛁮𛁯𛁰𛁱𛁲𛁳𛁴𛁵𛁶𛂎")
    (?と . "𛁷𛁸𛁹𛁺𛁻𛁼𛁭𛁽")
    (?な . "𛁾𛁿𛂀𛂁𛂂𛂃𛂄𛂅𛂆")
    (?に . "𛂇𛂈𛂉𛂊𛂋𛂌𛂍𛂎")
    (?ぬ . "𛂏𛂐𛂑")
    (?ね . "𛂒𛂓𛂔𛂕𛂖𛂗𛂘")
    (?の . "𛂙𛂚𛂛𛂜𛂝")
    (?は . "𛂞𛂟𛂠𛂡𛂢𛂣𛂤𛂥𛂦𛂧𛂨") ;;max 11
    (?ひ . "𛂩𛂪𛂫𛂬𛂭𛂮𛂯")
    (?ふ . "𛂰𛂱𛂲")
    (?へ . "𛂳𛂴𛂵𛂶𛂷𛂸𛂹")
    (?ほ . "𛂺𛂻𛂼𛂽𛂾𛂿𛃀𛃁")
    (?ま . "𛃂𛃃𛃄𛃅𛃆𛃇𛃈𛃖")
    (?み . "𛃉𛃊𛃋𛃌𛃍𛃎𛃏")
    (?む . "𛃐𛃑𛃒𛃓𛄝𛄞")
    (?め . "𛃔𛃕𛃖")
    (?も . "𛃗𛃘𛃙𛃚𛃛𛃜𛄝𛄞")
    (?や . "𛃝𛃞𛃟𛃠𛃡𛃢")
    (?ゆ . "𛃣𛃤𛃥𛃦")
    (?よ . "𛃧𛃨𛃩𛃪𛃫𛃬𛃢")
    (?ら . "𛃭𛃮𛃯𛃰𛁽")
    (?り . "𛃱𛃲𛃳𛃴𛃵𛃶𛃷")
    (?る . "𛃸𛃹𛃺𛃻𛃼𛃽")
    (?れ . "𛃾𛃿𛄀𛄁")
    (?ろ . "𛄂𛄃𛄄𛄅𛄆𛄇")
    (?わ . "𛄈𛄉𛄊𛄋𛄌")
    (?ゐ . "𛄍𛄎𛄏𛄐𛄑")
    (?ゑ . "𛄒𛄓𛄔𛄕")
    (?を . "𛄖𛄗𛄘𛄙𛄚𛄛𛄜𛀅")
    (?ん . "𛄝𛄞")))

(defconst hentaigana-kanji-alist
  '((?万 . "𛃂")
    (?三 . "𛃉")
    (?不 . "ふ𛂰")
    (?世 . "せ𛁒𛁓𛁔")
    (?丹 . "𛂇")
    (?乃 . "の𛂙")
    (?久 . "く𛀫𛀬")
    (?之 . "し𛁄𛁅")
    (?乍 . "𛀼")
    (?乎 . "𛄖𛄗")
    (?九 . "𛀭")
    (?也 . "や𛃝𛃞")
    (?事 . "𛁆")
    (?二 . "𛂈")
    (?井 . "𛄍𛄎")
    (?亭 . "𛁮")
    (?仁 . "に𛂉")
    (?介 . "𛀲𛀳")
    (?代 . "𛃧")
    (?以 . "い𛀆")
    (?伊 . "𛀇")
    (?低 . "𛁯")
    (?佐 . "𛀽𛀾")
    (?余 . "𛃨")
    (?佳 . "𛀗")
    (?供 . "𛀮")
    (?保 . "ほ𛂺𛂻")
    (?倍 . "𛂳")
    (?倭 . "𛄈")
    (?倶 . "𛀯")
    (?傳 . "𛁰")
    (?免 . "𛃔")
    (?兒 . "𛂊")
    (?八 . "𛂞")
    (?具 . "𛀰")
    (?利 . "り𛃱𛃲")
    (?加 . "か𛀘")
    (?努 . "𛂏")
    (?勢 . "𛁕")
    (?千 . "𛁢")
    (?半 . "𛂟")
    (?南 . "𛁾")
    (?受 . "𛁊")
    (?古 . "𛀸")
    (?可 . "𛀙𛀚")
    (?名 . "𛁿")
    (?呂 . "ろ𛄂𛄃")
    (?和 . "わ𛄉𛄊")
    (?喜 . "𛀣")
    (?嘉 . "𛀛")
    (?四 . "𛁇")
    (?土 . "𛁷")
    (?地 . "𛁣")
    (?堂 . "𛁞")
    (?報 . "𛂼")
    (?壽 . "𛁋")
    (?多 . "𛁟𛁠")
    (?夜 . "𛃢")
    (?天 . "て𛁱𛁲𛁳")
    (?太 . "た")
    (?奈 . "な𛂀𛂁𛂂")
    (?奉 . "𛂽")
    (?女 . "め")
    (?奴 . "ぬ𛂐")
    (?婁 . "𛄄")
    (?婆 . "𛂠")
    (?婦 . "𛂱")
    (?子 . "𛂘")
    (?宇 . "う𛀊𛀋")
    (?安 . "あ𛀂")
    (?家 . "𛀢")
    (?寶 . "𛂾")
    (?寸 . "す")
    (?尾 . "𛄘")
    (?居 . "𛄏")
    (?屋 . "𛃟")
    (?川 . "つ𛁩𛁪")
    (?左 . "さ𛀿")
    (?差 . "𛁀")
    (?己 . "こ")
    (?布 . "𛂲")
    (?希 . "𛀴")
    (?帝 . "𛁴")
    (?年 . "𛂒𛂓𛂔")
    (?幾 . "き𛀤𛀥")
    (?度 . "𛁸")
    (?弊 . "𛂴𛂵")
    (?弖 . "𛁵")
    (?徒 . "𛁭")
    (?微 . "𛃊")
    (?志 . "𛁈")
    (?怒 . "𛂑")
    (?悲 . "𛂩")
    (?惠 . "ゑ𛄒")
    (?惡 . "𛀅")
    (?意 . "𛀈")
    (?愛 . "𛀃")
    (?憂 . "𛀌")
    (?我 . "𛀜")
    (?所 . "𛁗𛁘")
    (?支 . "𛀦")
    (?故 . "𛀹")
    (?散 . "𛁁")
    (?數 . "𛁌𛁍")
    (?斜 . "𛁂")
    (?新 . "𛁉")
    (?於 . "お𛀔𛀕")
    (?无 . "ん𛄝𛄞")
    (?日 . "𛂪")
    (?春 . "𛁎𛁏")
    (?智 . "𛁤")
    (?曾 . "そ𛁙𛁚")
    (?有 . "𛀍")
    (?期 . "𛀻")
    (?木 . "𛀧")
    (?末 . "ま𛃃𛃄")
    (?本 . "𛂿𛃀")
    (?李 . "𛃳")
    (?東 . "𛁹")
    (?根 . "𛂕")
    (?梨 . "𛃴")
    (?楚 . "𛁛")
    (?樓 . "𛄅")
    (?歟 . "𛀝")
    (?止 . "と")
    (?武 . "む𛃐")
    (?母 . "𛃗")
    (?比 . "ひ𛂫")
    (?毛 . "も𛃘𛃙𛃚")
    (?氣 . "𛀵")
    (?求 . "𛀱")
    (?江 . "𛀁")
    (?沙 . "𛁃")
    (?波 . "は𛂡")
    (?津 . "𛁫")
    (?流 . "𛃸")
    (?游 . "𛃣")
    (?滿 . "𛃅𛃆")
    (?濃 . "𛂚")
    (?無 . "𛃑")
    (?熱 . "𛂖")
    (?爲 . "ゐ𛄐")
    (?爾 . "𛂋𛂌")
    (?牟 . "𛃒")
    (?王 . "𛄋𛄌")
    (?理 . "𛃵")
    (?由 . "ゆ𛃤𛃥")
    (?留 . "る𛃹𛃺𛃻")
    (?當 . "𛁡")
    (?登 . "𛁺𛁻")
    (?盈 . "𛀏")
    (?盤 . "𛂢𛂣")
    (?知 . "ち𛁥𛁦")
    (?砥 . "𛁼")
    (?破 . "𛂤")
    (?祈 . "𛀨")
    (?禮 . "れ𛃾𛃿")
    (?禰 . "ね𛂗")
    (?移 . "𛀉")
    (?等 . "𛁽")
    (?累 . "𛃼")
    (?緒 . "𛄙")
    (?縁 . "𛀐")
    (?羅 . "𛃭")
    (?美 . "み𛃋𛃌𛃍")
    (?者 . "𛂥𛂦")
    (?而 . "𛂎")
    (?耳 . "𛂍")
    (?耶 . "𛃠𛃡")
    (?聲 . "𛁖")
    (?能 . "𛂛𛂜")
    (?致 . "𛁧")
    (?與 . "よ𛃩𛃪𛃫")
    (?舞 . "𛃓")
    (?良 . "ら𛃮𛃯𛃰")
    (?茂 . "𛃛")
    (?菜 . "𛂃")
    (?萬 . "𛃇")
    (?葉 . "𛂧")
    (?蘇 . "𛁜")
    (?處 . "𛁝")
    (?衞 . "𛄓𛄔𛄕")
    (?衣 . "え𛀑𛀒")
    (?裳 . "𛃜")
    (?要 . "𛀓")
    (?見 . "𛃎")
    (?計 . "け𛀶")
    (?許 . "𛀺")
    (?豐 . "𛃁")
    (?貴 . "𛀩")
    (?賀 . "𛀞")
    (?起 . "𛀪")
    (?越 . "𛄚")
    (?路 . "𛄆")
    (?身 . "𛃏")
    (?轉 . "𛁶")
    (?農 . "𛂝")
    (?連 . "𛄀")
    (?遊 . "𛃦")
    (?遍 . "𛂶")
    (?遠 . "を𛄛𛄜")
    (?遣 . "𛀷")
    (?遲 . "𛁨")
    (?遺 . "𛄑")
    (?避 . "𛂬")
    (?邊 . "𛂷𛂸")
    (?那 . "𛂄𛂅")
    (?部 . "へ𛂹")
    (?都 . "𛁬")
    (?里 . "𛃶")
    (?閑 . "𛀟")
    (?阿 . "𛀄")
    (?隱 . "𛀖")
    (?離 . "𛃷")
    (?難 . "𛂆")
    (?雲 . "𛀎")
    (?露 . "𛄇")
    (?非 . "𛂭")
    (?面 . "𛃕")
    (?須 . "𛁐𛁑")
    (?頗 . "𛂨")
    (?類 . "𛃽")
    (?飛 . "𛂮𛂯")
    (?餘 . "𛃬")
    (?香 . "𛀠")
    (?馬 . "𛃖")
    (?駕 . "𛀡")
    (?麗 . "𛄁")
    (?麻 . "𛃈")))

(defconst hentaigana-new-kanji-alist
  '((?与 . "よ𛃩𛃪𛃫")
    (?伝 . "𛁰")
    (?児 . "𛂊")
    (?処 . "𛁝")
    (?声 . "𛁖")
    (?宝 . "𛂾")
    (?寿 . "𛁋")
    (?尓 . "𛂋𛂌")
    (?当 . "𛁡")
    (?恵 . "ゑ𛄒")
    (?曽 . "そ𛁙𛁚")
    (?楼 . "𛄅")
    (?気 . "𛀵")
    (?満 . "𛃅𛃆")
    (?為 . "ゐ𛄐")
    (?礼 . "れ𛃾𛃿")
    (?祢 . "ね𛂗")
    (?豊 . "𛃁")
    (?辺 . "𛂷𛂸")
    (?隠 . "𛀖")))

(defvar hentaigana-to-kana-table
  (let ((table (make-hash-table)))
    (dolist (alist hentaigana-alist)
      (dolist (hkana (string-to-list (cdr alist)))
        (cl-pushnew (car alist) (gethash hkana table))))
    table))

(defvar hentaigana-to-kanji-table
  (let ((table (make-hash-table)))
    (dolist (alist hentaigana-kanji-alist)
      (dolist (hkana (string-to-list (cdr alist)))
        (puthash hkana (car alist) table)))
    table))

(defvar hentaigana-to-new-kanji-table
  (let ((table (make-hash-table)))
    (dolist (alist hentaigana-new-kanji-alist)
      (dolist (hkana (string-to-list (cdr alist)))
        (puthash hkana (car alist) table)))
    table))

;;;###autoload
(defun hentaigana-to-kana-region (from to)
  "Change to kana in a region FROM TO."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "[𛀁-𛄞]" nil t)
        (let ((hkana (gethash (string-to-char (match-string 0))
                              hentaigana-to-kana-table)))
          (if (= (length hkana) 1)
              (replace-match (char-to-string (car hkana)))
            (replace-match (concat "[" (apply 'string hkana) "]"))))))))

;;;###autoload
(defun hentaigana-to-kanji-region (from to)
  "Change to kana in a region FROM TO to kanji."
  ;; TODO 普通の平仮名の濁音処理
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "[あ-ん𛀁-𛄞]" nil t)
        (let ((kanji (gethash (string-to-char (match-string 0))
                              hentaigana-to-kanji-table))
              (new-kanji (gethash (string-to-char (match-string 0))
                            hentaigana-to-new-kanji-table)))
          (unless (null kanji)
            (replace-match
             (char-to-string
              (if (and new-kanji hentaigana-use-new-kanji) new-kanji kanji)))))))))

;;;###autoload
(defun hentaigana-region (from to)
  "Change Kana to (preferred) Hentaigana in a region FROM TO."
  (interactive "r")
  ;; TODO Preferred List 処理
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "[あ-ん]" nil t)
        (let* ((decomp (get-char-code-property
                        (string-to-char (match-string 0)) 'decomposition))
               (kana (car decomp))
               (dakuon (apply 'string (cdr decomp)))
               (hen (assoc kana hentaigana-alist)))
          (when hen
            (replace-match (concat "[" (cdr hen) "]" dakuon))))))))

;;;###autoload
(defun hentaigana ()
  "Change Hentaigana at point."
  (interactive)
  (let* ((char (char-after (point)))
         (decomp (get-char-code-property
                  (string-to-char (match-string 0)) 'decomposition))
         (kana (car decomp))
         (dakuon (apply 'string (cdr decomp)))
         (hen (assoc kana hentaigana-alist)))
    (when hen
      (insert (concat "[" (cdr hen) "]" dakuon)))))

;;;###autoload
(register-input-method "hentaigana" "hen"
                       'hentaigana-input-activate "hen")

;;;###autoload
(defun hentaigana-input-activate (name)
  "Activating Hentaigana Input method."
  (robin-use-package name))

(quail-define-package
 "hentaigana" "Japanese" "変" t
 "変体仮名入力."
 nil nil nil nil nil nil t)

(eval
 `(quail-define-rules
   ,@(cl-loop
      for pair in hentaigana-transliteration-alist
      for trans = (car pair)
      for kana = (cdr pair)
      for decomposition = (get-char-code-property kana 'decomposition)
      for dakuon = (apply 'string (cdr decomposition))
      for kana2 = (car decomposition)
      for hen = (cdr (assoc kana2 hentaigana-alist))
      for hen-list = (string-to-list hen)
      collect (list trans (apply 'vector
                                 (cons (char-to-string kana)
                                       (mapcar (lambda (x) (concat (char-to-string x)
                                                               dakuon)) hen-list))))
      nconc (--map-indexed (list (concat trans (format "%c" (+ 97 it-index)))
                                 (vector (concat (char-to-string it) dakuon)))
                           hen-list))))

(provide 'hentaigana)

;;; hentaigana.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:
