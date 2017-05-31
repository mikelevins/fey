;;; an experiment in building Fe vocabulary from the ground up

(in-package :fi)

;;; ---------------------------------------------------------------------
;;; vowels
;;; ---------------------------------------------------------------------

(defparameter $short-vowels
  '("a" "e" "i" "i" "i"))

(defparameter $long-vowels
  '("á" "é" "í" "ó" "ú"))

(defparameter $diphthongs
  '("a" "e" "i" "o" "u"
    "ae" "ai" "ao" "au"
    "ea" "ei" "eo" "eu"
    "ia" "ie" "io" "iu"
    "oa" "oe" "oi" "ou"
    "ua" "ue" "ui" "uo"))

(defparameter $vowels (append $short-vowels $long-vowels $diphthongs))

;;; ---------------------------------------------------------------------
;;; consonants
;;; ---------------------------------------------------------------------

;;; voicings
(defparameter $voicings '(:voiced :voiceless))

;;; places of articulation:
(defparameter $places
  '(:bilabial :labiodental :dental :alveolar  :alveolar-palatal :palatal :velar :labiovelar :glottal))

;;; manners of articulation:
(defparameter $manners
  '(:plosive :nasal :fricative :sibilant :trill :approximant))

;;; the consonants used in fe
(defparameter $consonants-with-classes
  '(("l" :alveolar :approximant :voiced)
    ("n" :alveolar :nasal :voiced)
    ("d" :alveolar :plosive :voiced)
    ("t" :alveolar :plosive :voiceless)
    ("z" :alveolar :sibilant :voiced)
    ("s" :alveolar :sibilant :voiceless)
    ("r" :alveolar :trill :voiced)
    ("zh" :alveolar-palatal :fricative :voiced)
    ("sh" :alveolar-palatal :fricative :voiceless)
    ("m" :bilabial :nasal :voiced)
    ("b" :bilabial :plosive :voiced)
    ("p" :bilabial :plosive :voiceless)
    ("dh" :dental :fricative :voiced)
    ("th" :dental :fricative :voiceless)
    ("h" :glottal :fricative :voiceless)
    ("v" :labiodental :fricative :voiced)
    ("f" :labiodental :fricative :voiceless)
    ("w" :labiovelar :approximant :voiced)
    ("y" :palatal :approximant :voiced)
    ("gh" :velar :fricative :voiced)
    ("ch" :velar :fricative :voiceless)
    ("ng" :velar :nasal :voiced)
    ("g" :velar :plosive :voiced)
    ("c" :velar :plosive :voiceless)))


(defun consonants (&rest kinds)
  (let ((found-kinds (if kinds
                         (remove-if-not (lambda (kind)
                                          (every (lambda (ck)(member ck kind))
                                                 kinds))
                                        $consonants-with-classes)
                         $consonants-with-classes)))
    (mapcar #'first found-kinds)))

(defun consonant-classes (consonant)
  (let ((found (assoc consonant $consonants-with-classes :test #'equalp)))
    (if found
        (cdr found)
        nil)))

;;; ---------------------------------------------------------------------
;;; particles
;;; ---------------------------------------------------------------------

(defparameter $initial-consonant-clusters
  '("br" "dr" "fw" "gl" "hr" "lh" "sw" "thw"))

(defparameter $forbidden-initial-consonants
  '("gh" "ng" "z" "zh"))

(defparameter $forbidden-final-consonants
  '("b" "p" "z"))


(defparameter $all-cv-particles
  (sort
   (delete-duplicates
    (reduce #'append
            (loop for c in (subtract-lists (append $initial-consonant-clusters (consonants))
                                           $forbidden-initial-consonants)
               collect (loop for v in $vowels
                          collect (concatenate 'string c v))))
    :test #'equalp)
   #'string<))

'("ba" "bae" "bai" "bao" "bau" "be" "bea" "bei" "beo" "beu" "bi" "bia"
  "bie" "bio" "biu" "bo" "boa" "boe" "boi" "bou" "bra" "brae" "brai"
  "brao" "brau" "bre" "brea" "brei" "breo" "breu" "bri" "bria" "brie"
  "brio" "briu" "bro" "broa" "broe" "broi" "brou" "bru" "brua" "brue"
  "brui" "bruo" "brá" "bré" "brí" "bró" "brú" "bu" "bua" "bue" "bui"
  "buo" "bá" "bé" "bí" "bó" "bú" "ca" "cae" "cai" "cao" "cau" "ce"
  "cea" "cei" "ceo" "ceu" "cha" "chae" "chai" "chao" "chau" "che"
  "chea" "chei" "cheo" "cheu" "chi" "chia" "chie" "chio" "chiu" "cho"
  "choa" "choe" "choi" "chou" "chu" "chua" "chue" "chui" "chuo" "chá"
  "ché" "chí" "chó" "chú" "ci" "cia" "cie" "cio" "ciu" "co" "coa"
  "coe" "coi" "cou" "cu" "cua" "cue" "cui" "cuo" "cá" "cé" "cí" "có"
  "cú" "da" "dae" "dai" "dao" "dau" "de" "dea" "dei" "deo" "deu" "dha"
  "dhae" "dhai" "dhao" "dhau" "dhe" "dhea" "dhei" "dheo" "dheu" "dhi"
  "dhia" "dhie" "dhio" "dhiu" "dho" "dhoa" "dhoe" "dhoi" "dhou" "dhu"
  "dhua" "dhue" "dhui" "dhuo" "dhá" "dhé" "dhí" "dhó" "dhú" "di" "dia"
  "die" "dio" "diu" "do" "doa" "doe" "doi" "dou" "dra" "drae" "drai"
  "drao" "drau" "dre" "drea" "drei" "dreo" "dreu" "dri" "dria" "drie"
  "drio" "driu" "dro" "droa" "droe" "droi" "drou" "dru" "drua" "drue"
  "drui" "druo" "drá" "dré" "drí" "dró" "drú" "du" "dua" "due" "dui"
  "duo" "dá" "dé" "dí" "dó" "dú" "fa" "fae" "fai" "fao" "fau" "fe"
  "fea" "fei" "feo" "feu" "fi" "fia" "fie" "fio" "fiu" "fo" "foa"
  "foe" "foi" "fou" "fu" "fua" "fue" "fui" "fuo" "fwa" "fwae" "fwai"
  "fwao" "fwau" "fwe" "fwea" "fwei" "fweo" "fweu" "fwi" "fwia" "fwie"
  "fwio" "fwiu" "fwo" "fwoa" "fwoe" "fwoi" "fwou" "fwu" "fwua" "fwue"
  "fwui" "fwuo" "fwá" "fwé" "fwí" "fwó" "fwú" "fá" "fé" "fí" "fó" "fú"
  "ga" "gae" "gai" "gao" "gau" "ge" "gea" "gei" "geo" "geu" "gi" "gia"
  "gie" "gio" "giu" "gla" "glae" "glai" "glao" "glau" "gle" "glea"
  "glei" "gleo" "gleu" "gli" "glia" "glie" "glio" "gliu" "glo" "gloa"
  "gloe" "gloi" "glou" "glu" "glua" "glue" "glui" "gluo" "glá" "glé"
  "glí" "gló" "glú" "go" "goa" "goe" "goi" "gou" "gu" "gua" "gue"
  "gui" "guo" "gá" "gé" "gí" "gó" "gú" "ha" "hae" "hai" "hao" "hau"
  "he" "hea" "hei" "heo" "heu" "hi" "hia" "hie" "hio" "hiu" "ho" "hoa"
  "hoe" "hoi" "hou" "hra" "hrae" "hrai" "hrao" "hrau" "hre" "hrea"
  "hrei" "hreo" "hreu" "hri" "hria" "hrie" "hrio" "hriu" "hro" "hroa"
  "hroe" "hroi" "hrou" "hru" "hrua" "hrue" "hrui" "hruo" "hrá" "hré"
  "hrí" "hró" "hrú" "hu" "hua" "hue" "hui" "huo" "há" "hé" "hí" "hó"
  "hú" "la" "lae" "lai" "lao" "lau" "le" "lea" "lei" "leo" "leu" "lha"
  "lhae" "lhai" "lhao" "lhau" "lhe" "lhea" "lhei" "lheo" "lheu" "lhi"
  "lhia" "lhie" "lhio" "lhiu" "lho" "lhoa" "lhoe" "lhoi" "lhou" "lhu"
  "lhua" "lhue" "lhui" "lhuo" "lhá" "lhé" "lhí" "lhó" "lhú" "li" "lia"
  "lie" "lio" "liu" "lo" "loa" "loe" "loi" "lou" "lu" "lua" "lue"
  "lui" "luo" "lá" "lé" "lí" "ló" "lú" "ma" "mae" "mai" "mao" "mau"
  "me" "mea" "mei" "meo" "meu" "mi" "mia" "mie" "mio" "miu" "mo" "moa"
  "moe" "moi" "mou" "mu" "mua" "mue" "mui" "muo" "má" "mé" "mí" "mó"
  "mú" "na" "nae" "nai" "nao" "nau" "ne" "nea" "nei" "neo" "neu" "ni"
  "nia" "nie" "nio" "niu" "no" "noa" "noe" "noi" "nou" "nu" "nua"
  "nue" "nui" "nuo" "ná" "né" "ní" "nó" "nú" "pa" "pae" "pai" "pao"
  "pau" "pe" "pea" "pei" "peo" "peu" "pi" "pia" "pie" "pio" "piu" "po"
  "poa" "poe" "poi" "pou" "pu" "pua" "pue" "pui" "puo" "pá" "pé" "pí"
  "pó" "pú" "ra" "rae" "rai" "rao" "rau" "re" "rea" "rei" "reo" "reu"
  "ri" "ria" "rie" "rio" "riu" "ro" "roa" "roe" "roi" "rou" "ru" "rua"
  "rue" "rui" "ruo" "rá" "ré" "rí" "ró" "rú" "sa" "sae" "sai" "sao"
  "sau" "se" "sea" "sei" "seo" "seu" "sha" "shae" "shai" "shao" "shau"
  "she" "shea" "shei" "sheo" "sheu" "shi" "shia" "shie" "shio" "shiu"
  "sho" "shoa" "shoe" "shoi" "shou" "shu" "shua" "shue" "shui" "shuo"
  "shá" "shé" "shí" "shó" "shú" "si" "sia" "sie" "sio" "siu" "so"
  "soa" "soe" "soi" "sou" "su" "sua" "sue" "sui" "suo" "swa" "swae"
  "swai" "swao" "swau" "swe" "swea" "swei" "sweo" "sweu" "swi" "swia"
  "swie" "swio" "swiu" "swo" "swoa" "swoe" "swoi" "swou" "swu" "swua"
  "swue" "swui" "swuo" "swá" "swé" "swí" "swó" "swú" "sá" "sé" "sí"
  "só" "sú" "ta" "tae" "tai" "tao" "tau" "te" "tea" "tei" "teo" "teu"
  "tha" "thae" "thai" "thao" "thau" "the" "thea" "thei" "theo" "theu"
  "thi" "thia" "thie" "thio" "thiu" "tho" "thoa" "thoe" "thoi" "thou"
  "thu" "thua" "thue" "thui" "thuo" "thwa" "thwae" "thwai" "thwao"
  "thwau" "thwe" "thwea" "thwei" "thweo" "thweu" "thwi" "thwia"
  "thwie" "thwio" "thwiu" "thwo" "thwoa" "thwoe" "thwoi" "thwou"
  "thwu" "thwua" "thwue" "thwui" "thwuo" "thwá" "thwé" "thwí" "thwó"
  "thwú" "thá" "thé" "thí" "thó" "thú" "ti" "tia" "tie" "tio" "tiu"
  "to" "toa" "toe" "toi" "tou" "tu" "tua" "tue" "tui" "tuo" "tá" "té"
  "tí" "tó" "tú" "va" "vae" "vai" "vao" "vau" "ve" "vea" "vei" "veo"
  "veu" "vi" "via" "vie" "vio" "viu" "vo" "voa" "voe" "voi" "vou" "vu"
  "vua" "vue" "vui" "vuo" "vá" "vé" "ví" "vó" "vú" "wa" "wae" "wai"
  "wao" "wau" "we" "wea" "wei" "weo" "weu" "wi" "wia" "wie" "wio"
  "wiu" "wo" "woa" "woe" "woi" "wou" "wu" "wua" "wue" "wui" "wuo" "wá"
  "wé" "wí" "wó" "wú" "ya" "yae" "yai" "yao" "yau" "ye" "yea" "yei"
  "yeo" "yeu" "yi" "yia" "yie" "yio" "yiu" "yo" "yoa" "yoe" "yoi"
  "you" "yu" "yua" "yue" "yui" "yuo" "yá" "yé" "yí" "yó" "yú")


(defparameter $all-vc-particles
  (sort
   (delete-duplicates
    (reduce #'append
            (loop for v in $vowels
               collect (loop for c in (consonants)
                          collect (concatenate 'string v c))))
    :test #'equalp)
   #'string<))

'("ab" "ac" "ach" "ad" "adh" "aeb" "aec" "aech" "aed" "aedh" "aef"
  "aeg" "aegh" "aeh" "ael" "aem" "aen" "aeng" "aep" "aer" "aes" "aesh"
  "aet" "aeth" "aev" "aew" "aey" "aez" "aezh" "af" "ag" "agh" "ah"
  "aib" "aic" "aich" "aid" "aidh" "aif" "aig" "aigh" "aih" "ail" "aim"
  "ain" "aing" "aip" "air" "ais" "aish" "ait" "aith" "aiv" "aiw" "aiy"
  "aiz" "aizh" "al" "am" "an" "ang" "aob" "aoc" "aoch" "aod" "aodh"
  "aof" "aog" "aogh" "aoh" "aol" "aom" "aon" "aong" "aop" "aor" "aos"
  "aosh" "aot" "aoth" "aov" "aow" "aoy" "aoz" "aozh" "ap" "ar" "as"
  "ash" "at" "ath" "aub" "auc" "auch" "aud" "audh" "auf" "aug" "augh"
  "auh" "aul" "aum" "aun" "aung" "aup" "aur" "aus" "aush" "aut" "auth"
  "auv" "auw" "auy" "auz" "auzh" "av" "aw" "ay" "az" "azh" "eab" "eac"
  "each" "ead" "eadh" "eaf" "eag" "eagh" "eah" "eal" "eam" "ean"
  "eang" "eap" "ear" "eas" "eash" "eat" "eath" "eav" "eaw" "eay" "eaz"
  "eazh" "eb" "ec" "ech" "ed" "edh" "ef" "eg" "egh" "eh" "eib" "eic"
  "eich" "eid" "eidh" "eif" "eig" "eigh" "eih" "eil" "eim" "ein"
  "eing" "eip" "eir" "eis" "eish" "eit" "eith" "eiv" "eiw" "eiy" "eiz"
  "eizh" "el" "em" "en" "eng" "eob" "eoc" "eoch" "eod" "eodh" "eof"
  "eog" "eogh" "eoh" "eol" "eom" "eon" "eong" "eop" "eor" "eos" "eosh"
  "eot" "eoth" "eov" "eow" "eoy" "eoz" "eozh" "ep" "er" "es" "esh"
  "et" "eth" "eub" "euc" "euch" "eud" "eudh" "euf" "eug" "eugh" "euh"
  "eul" "eum" "eun" "eung" "eup" "eur" "eus" "eush" "eut" "euth" "euv"
  "euw" "euy" "euz" "euzh" "ev" "ew" "ey" "ez" "ezh" "iab" "iac"
  "iach" "iad" "iadh" "iaf" "iag" "iagh" "iah" "ial" "iam" "ian"
  "iang" "iap" "iar" "ias" "iash" "iat" "iath" "iav" "iaw" "iay" "iaz"
  "iazh" "ib" "ic" "ich" "id" "idh" "ieb" "iec" "iech" "ied" "iedh"
  "ief" "ieg" "iegh" "ieh" "iel" "iem" "ien" "ieng" "iep" "ier" "ies"
  "iesh" "iet" "ieth" "iev" "iew" "iey" "iez" "iezh" "if" "ig" "igh"
  "ih" "il" "im" "in" "ing" "iob" "ioc" "ioch" "iod" "iodh" "iof"
  "iog" "iogh" "ioh" "iol" "iom" "ion" "iong" "iop" "ior" "ios" "iosh"
  "iot" "ioth" "iov" "iow" "ioy" "ioz" "iozh" "ip" "ir" "is" "ish"
  "it" "ith" "iub" "iuc" "iuch" "iud" "iudh" "iuf" "iug" "iugh" "iuh"
  "iul" "ium" "iun" "iung" "iup" "iur" "ius" "iush" "iut" "iuth" "iuv"
  "iuw" "iuy" "iuz" "iuzh" "iv" "iw" "iy" "iz" "izh" "oab" "oac"
  "oach" "oad" "oadh" "oaf" "oag" "oagh" "oah" "oal" "oam" "oan"
  "oang" "oap" "oar" "oas" "oash" "oat" "oath" "oav" "oaw" "oay" "oaz"
  "oazh" "ob" "oc" "och" "od" "odh" "oeb" "oec" "oech" "oed" "oedh"
  "oef" "oeg" "oegh" "oeh" "oel" "oem" "oen" "oeng" "oep" "oer" "oes"
  "oesh" "oet" "oeth" "oev" "oew" "oey" "oez" "oezh" "of" "og" "ogh"
  "oh" "oib" "oic" "oich" "oid" "oidh" "oif" "oig" "oigh" "oih" "oil"
  "oim" "oin" "oing" "oip" "oir" "ois" "oish" "oit" "oith" "oiv" "oiw"
  "oiy" "oiz" "oizh" "ol" "om" "on" "ong" "op" "or" "os" "osh" "ot"
  "oth" "oub" "ouc" "ouch" "oud" "oudh" "ouf" "oug" "ough" "ouh" "oul"
  "oum" "oun" "oung" "oup" "our" "ous" "oush" "out" "outh" "ouv" "ouw"
  "ouy" "ouz" "ouzh" "ov" "ow" "oy" "oz" "ozh" "uab" "uac" "uach"
  "uad" "uadh" "uaf" "uag" "uagh" "uah" "ual" "uam" "uan" "uang" "uap"
  "uar" "uas" "uash" "uat" "uath" "uav" "uaw" "uay" "uaz" "uazh" "ub"
  "uc" "uch" "ud" "udh" "ueb" "uec" "uech" "ued" "uedh" "uef" "ueg"
  "uegh" "ueh" "uel" "uem" "uen" "ueng" "uep" "uer" "ues" "uesh" "uet"
  "ueth" "uev" "uew" "uey" "uez" "uezh" "uf" "ug" "ugh" "uh" "uib"
  "uic" "uich" "uid" "uidh" "uif" "uig" "uigh" "uih" "uil" "uim" "uin"
  "uing" "uip" "uir" "uis" "uish" "uit" "uith" "uiv" "uiw" "uiy" "uiz"
  "uizh" "ul" "um" "un" "ung" "uob" "uoc" "uoch" "uod" "uodh" "uof"
  "uog" "uogh" "uoh" "uol" "uom" "uon" "uong" "uop" "uor" "uos" "uosh"
  "uot" "uoth" "uov" "uow" "uoy" "uoz" "uozh" "up" "ur" "us" "ush"
  "ut" "uth" "uv" "uw" "uy" "uz" "uzh" "áb" "ác" "ách" "ád" "ádh" "áf"
  "ág" "ágh" "áh" "ál" "ám" "án" "áng" "áp" "ár" "ás" "ásh" "át" "áth"
  "áv" "áw" "áy" "áz" "ázh" "éb" "éc" "éch" "éd" "édh" "éf" "ég" "égh"
  "éh" "él" "ém" "én" "éng" "ép" "ér" "és" "ésh" "ét" "éth" "év" "éw"
  "éy" "éz" "ézh" "íb" "íc" "ích" "íd" "ídh" "íf" "íg" "ígh" "íh" "íl"
  "ím" "ín" "íng" "íp" "ír" "ís" "ísh" "ít" "íth" "ív" "íw" "íy" "íz"
  "ízh" "ób" "óc" "óch" "ód" "ódh" "óf" "óg" "ógh" "óh" "ól" "óm" "ón"
  "óng" "óp" "ór" "ós" "ósh" "ót" "óth" "óv" "ów" "óy" "óz" "ózh" "úb"
  "úc" "úch" "úd" "údh" "úf" "úg" "úgh" "úh" "úl" "úm" "ún" "úng" "úp"
  "úr" "ús" "úsh" "út" "úth" "úv" "úw" "úy" "úz" "úzh")
