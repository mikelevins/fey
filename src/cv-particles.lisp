(in-package :fi)


(defun all-cv-particles ()
  (reduce #'append
          (loop for c in $cv-particle-initials
             collect (loop for v in $vowels
                        collect (concatenate 'string c v)))))

(defparameter $all-cv-particles
  '("pa" "pe" "pi" "pi" "pi" "pá" "pé" "pí" "pó" "pú" "pa" "pe" "pi"
    "po" "pu" "pae" "pai" "pao" "pau" "pea" "pei" "peo" "peu" "pia"
    "pie" "pio" "piu" "poa" "poe" "poi" "pou" "pua" "pue" "pui" "puo"

    "ba" "be" "bi" "bi" "bi" "bá" "bé" "bí" "bó" "bú" "ba" "be" "bi"
    "bo" "bu" "bae" "bai" "bao" "bau" "bea" "bei" "beo" "beu" "bia"
    "bie" "bio" "biu" "boa" "boe" "boi" "bou" "bua" "bue" "bui" "buo"

    "ma" "me" "mi" "mi" "mi" "má" "mé" "mí" "mó" "mú" "ma" "me" "mi"
    "mo" "mu" "mae" "mai" "mao" "mau" "mea" "mei" "meo" "meu" "mia"
    "mie" "mio" "miu" "moa" "moe" "moi" "mou" "mua" "mue" "mui" "muo"

    "fa" "fe" "fi" "fi" "fi" "fá" "fé" "fí" "fó" "fú" "fa" "fe" "fi"
    "fo" "fu" "fae" "fai" "fao" "fau" "fea" "fei" "feo" "feu" "fia"
    "fie" "fio" "fiu" "foa" "foe" "foi" "fou" "fua" "fue" "fui" "fuo"

    "va" "ve" "vi" "vi" "vi" "vá" "vé" "ví" "vó" "vú" "va" "ve" "vi"
    "vo" "vu" "vae" "vai" "vao" "vau" "vea" "vei" "veo" "veu" "via"
    "vie" "vio" "viu" "voa" "voe" "voi" "vou" "vua" "vue" "vui" "vuo"

    "tha" "the" "thi" "thi" "thi" "thá" "thé" "thí" "thó" "thú" "tha"
    "the" "thi" "tho" "thu" "thae" "thai" "thao" "thau" "thea" "thei"
    "theo" "theu" "thia" "thie" "thio" "thiu" "thoa" "thoe" "thoi"
    "thou" "thua" "thue" "thui" "thuo"
    
    "dha" "dhe" "dhi" "dhi" "dhi"
    "dhá" "dhé" "dhí" "dhó" "dhú" "dha" "dhe" "dhi" "dho" "dhu" "dhae"
    "dhai" "dhao" "dhau" "dhea" "dhei" "dheo" "dheu" "dhia" "dhie"
    "dhio" "dhiu" "dhoa" "dhoe" "dhoi" "dhou" "dhua" "dhue" "dhui"
    "dhuo"

    "ta" "te" "ti" "ti" "ti" "tá" "té" "tí" "tó" "tú" "ta" "te"
    "ti" "to" "tu" "tae" "tai" "tao" "tau" "tea" "tei" "teo" "teu" "tia"
    "tie" "tio" "tiu" "toa" "toe" "toi" "tou" "tua" "tue" "tui" "tuo"

    "da" "de" "di" "di" "di" "dá" "dé" "dí" "dó" "dú" "da" "de" "di"
    "do" "du" "dae" "dai" "dao" "dau" "dea" "dei" "deo" "deu" "dia"
    "die" "dio" "diu" "doa" "doe" "doi" "dou" "dua" "due" "dui" "duo"

    "na" "ne" "ni" "ni" "ni" "ná" "né" "ní" "nó" "nú" "na" "ne" "ni"
    "no" "nu" "nae" "nai" "nao" "nau" "nea" "nei" "neo" "neu" "nia"
    "nie" "nio" "niu" "noa" "noe" "noi" "nou" "nua" "nue" "nui" "nuo"

    "ra" "re" "ri" "ri" "ri" "rá" "ré" "rí" "ró" "rú" "ra" "re" "ri"
    "ro" "ru" "rae" "rai" "rao" "rau" "rea" "rei" "reo" "reu" "ria"
    "rie" "rio" "riu" "roa" "roe" "roi" "rou" "rua" "rue" "rui" "ruo"

    "la" "le" "li" "li" "li" "lá" "lé" "lí" "ló" "lú" "la" "le" "li"
    "lo" "lu" "lae" "lai" "lao" "lau" "lea" "lei" "leo" "leu" "lia"
    "lie" "lio" "liu" "loa" "loe" "loi" "lou" "lua" "lue" "lui" "luo"

    "sa" "se" "si" "si" "si" "sá" "sé" "sí" "só" "sú" "sa" "se" "si"
    "so" "su" "sae" "sai" "sao" "sau" "sea" "sei" "seo" "seu" "sia"
    "sie" "sio" "siu" "soa" "soe" "soi" "sou" "sua" "sue" "sui" "suo"

    "sha" "she" "shi" "shi" "shi" "shá" "shé" "shí" "shó" "shú" "sha"
    "she" "shi" "sho" "shu" "shae" "shai" "shao" "shau" "shea" "shei"
    "sheo" "sheu" "shia" "shie" "shio" "shiu" "shoa" "shoe" "shoi"
    "shou" "shua" "shue" "shui" "shuo"

    "ya" "ye" "yi" "yi" "yi" "yá"
    "yé" "yí" "yó" "yú" "ya" "ye" "yi" "yo" "yu" "yae" "yai" "yao" "yau"
    "yea" "yei" "yeo" "yeu" "yia" "yie" "yio" "yiu" "yoa" "yoe" "yoi"
    "you" "yua" "yue" "yui" "yuo"

    "ca" "ce" "ci" "ci" "ci" "cá" "cé"
    "cí" "có" "cú" "ca" "ce" "ci" "co" "cu" "cae" "cai" "cao" "cau"
    "cea" "cei" "ceo" "ceu" "cia" "cie" "cio" "ciu" "coa" "coe" "coi"
    "cou" "cua" "cue" "cui" "cuo"

    "ga" "ge" "gi" "gi" "gi" "gá" "gé"
    "gí" "gó" "gú" "ga" "ge" "gi" "go" "gu" "gae" "gai" "gao" "gau"
    "gea" "gei" "geo" "geu" "gia" "gie" "gio" "giu" "goa" "goe" "goi"
    "gou" "gua" "gue" "gui" "guo"

    "wa" "we" "wi" "wi" "wi" "wá" "wé"
    "wí" "wó" "wú" "wa" "we" "wi" "wo" "wu" "wae" "wai" "wao" "wau"
    "wea" "wei" "weo" "weu" "wia" "wie" "wio" "wiu" "woa" "woe" "woi"
    "wou" "wua" "wue" "wui" "wuo"

    "ha" "he" "hi" "hi" "hi" "há" "hé"
    "hí" "hó" "hú" "ha" "he" "hi" "ho" "hu" "hae" "hai" "hao" "hau"
    "hea" "hei" "heo" "heu" "hia" "hie" "hio" "hiu" "hoa" "hoe" "hoi"
    "hou" "hua" "hue" "hui" "huo"

    "bra" "bre" "bri" "bri" "bri" "brá"
    "bré" "brí" "bró" "brú" "bra" "bre" "bri" "bro" "bru" "brae" "brai"
    "brao" "brau" "brea" "brei" "breo" "breu" "bria" "brie" "brio"
    "briu" "broa" "broe" "broi" "brou" "brua" "brue" "brui" "bruo"

    "bla"
    "ble" "bli" "bli" "bli" "blá" "blé" "blí" "bló" "blú" "bla" "ble"
    "bli" "blo" "blu" "blae" "blai" "blao" "blau" "blea" "blei" "bleo"
    "bleu" "blia" "blie" "blio" "bliu" "bloa" "bloe" "bloi" "blou"
    "blua" "blue" "blui" "bluo"

    "bwa" "bwe" "bwi" "bwi" "bwi" "bwá"
    "bwé" "bwí" "bwó" "bwú" "bwa" "bwe" "bwi" "bwo" "bwu" "bwae" "bwai"
    "bwao" "bwau" "bwea" "bwei" "bweo" "bweu" "bwia" "bwie" "bwio"
    "bwiu" "bwoa" "bwoe" "bwoi" "bwou" "bwua" "bwue" "bwui" "bwuo"

    "fra"
    "fre" "fri" "fri" "fri" "frá" "fré" "frí" "fró" "frú" "fra" "fre"
    "fri" "fro" "fru" "frae" "frai" "frao" "frau" "frea" "frei" "freo"
    "freu" "fria" "frie" "frio" "friu" "froa" "froe" "froi" "frou"
    "frua" "frue" "frui" "fruo"

    "fla" "fle" "fli" "fli" "fli" "flá"
    "flé" "flí" "fló" "flú" "fla" "fle" "fli" "flo" "flu" "flae" "flai"
    "flao" "flau" "flea" "flei" "fleo" "fleu" "flia" "flie" "flio"
    "fliu" "floa" "floe" "floi" "flou" "flua" "flue" "flui" "fluo"

    "fwa"
    "fwe" "fwi" "fwi" "fwi" "fwá" "fwé" "fwí" "fwó" "fwú" "fwa" "fwe"
    "fwi" "fwo" "fwu" "fwae" "fwai" "fwao" "fwau" "fwea" "fwei" "fweo"
    "fweu" "fwia" "fwie" "fwio" "fwiu" "fwoa" "fwoe" "fwoi" "fwou"
    "fwua" "fwue" "fwui" "fwuo"

    "vra" "vre" "vri" "vri" "vri" "vrá"
    "vré" "vrí" "vró" "vrú" "vra" "vre" "vri" "vro" "vru" "vrae" "vrai"
    "vrao" "vrau" "vrea" "vrei" "vreo" "vreu" "vria" "vrie" "vrio"
    "vriu" "vroa" "vroe" "vroi" "vrou" "vrua" "vrue" "vrui" "vruo"

    "vla"
    "vle" "vli" "vli" "vli" "vlá" "vlé" "vlí" "vló" "vlú" "vla" "vle"
    "vli" "vlo" "vlu" "vlae" "vlai" "vlao" "vlau" "vlea" "vlei" "vleo"
    "vleu" "vlia" "vlie" "vlio" "vliu" "vloa" "vloe" "vloi" "vlou"
    "vlua" "vlue" "vlui" "vluo"

    "vwa" "vwe" "vwi" "vwi" "vwi" "vwá"
    "vwé" "vwí" "vwó" "vwú" "vwa" "vwe" "vwi" "vwo" "vwu" "vwae" "vwai"
    "vwao" "vwau" "vwea" "vwei" "vweo" "vweu" "vwia" "vwie" "vwio"
    "vwiu" "vwoa" "vwoe" "vwoi" "vwou" "vwua" "vwue" "vwui" "vwuo"

    "thra" "thre" "thri" "thri" "thri" "thrá" "thré" "thrí" "thró"
    "thrú" "thra" "thre" "thri" "thro" "thru" "thrae" "thrai" "thrao"
    "thrau" "threa" "threi" "threo" "threu" "thria" "thrie" "thrio"
    "thriu" "throa" "throe" "throi" "throu" "thrua" "thrue" "thrui"
    "thruo"

    "thla" "thle" "thli" "thli" "thli" "thlá" "thlé" "thlí"
    "thló" "thlú" "thla" "thle" "thli" "thlo" "thlu" "thlae" "thlai"
    "thlao" "thlau" "thlea" "thlei" "thleo" "thleu" "thlia" "thlie"
    "thlio" "thliu" "thloa" "thloe" "thloi" "thlou" "thlua" "thlue"
    "thlui" "thluo"

    "thwa" "thwe" "thwi" "thwi" "thwi" "thwá" "thwé"
    "thwí" "thwó" "thwú" "thwa" "thwe" "thwi" "thwo" "thwu" "thwae"
    "thwai" "thwao" "thwau" "thwea" "thwei" "thweo" "thweu" "thwia"
    "thwie" "thwio" "thwiu" "thwoa" "thwoe" "thwoi" "thwou" "thwua"
    "thwue" "thwui" "thwuo"

    "dhra" "dhre" "dhri" "dhri" "dhri" "dhrá"
    "dhré" "dhrí" "dhró" "dhrú" "dhra" "dhre" "dhri" "dhro" "dhru"
    "dhrae" "dhrai" "dhrao" "dhrau" "dhrea" "dhrei" "dhreo" "dhreu"
    "dhria" "dhrie" "dhrio" "dhriu" "dhroa" "dhroe" "dhroi" "dhrou"
    "dhrua" "dhrue" "dhrui" "dhruo"

    "dhla" "dhle" "dhli" "dhli" "dhli"
    "dhlá" "dhlé" "dhlí" "dhló" "dhlú" "dhla" "dhle" "dhli" "dhlo"
    "dhlu" "dhlae" "dhlai" "dhlao" "dhlau" "dhlea" "dhlei" "dhleo"
    "dhleu" "dhlia" "dhlie" "dhlio" "dhliu" "dhloa" "dhloe" "dhloi"
    "dhlou" "dhlua" "dhlue" "dhlui" "dhluo"

    "dhwa" "dhwe" "dhwi" "dhwi"
    "dhwi" "dhwá" "dhwé" "dhwí" "dhwó" "dhwú" "dhwa" "dhwe" "dhwi"
    "dhwo" "dhwu" "dhwae" "dhwai" "dhwao" "dhwau" "dhwea" "dhwei"
    "dhweo" "dhweu" "dhwia" "dhwie" "dhwio" "dhwiu" "dhwoa" "dhwoe"
    "dhwoi" "dhwou" "dhwua" "dhwue" "dhwui" "dhwuo"

    "tra" "tre" "tri"
    "tri" "tri" "trá" "tré" "trí" "tró" "trú" "tra" "tre" "tri" "tro"
    "tru" "trae" "trai" "trao" "trau" "trea" "trei" "treo" "treu" "tria"
    "trie" "trio" "triu" "troa" "troe" "troi" "trou" "trua" "true"
    "trui" "truo"

    "twa" "twe" "twi" "twi" "twi" "twá" "twé" "twí" "twó"
    "twú" "twa" "twe" "twi" "two" "twu" "twae" "twai" "twao" "twau"
    "twea" "twei" "tweo" "tweu" "twia" "twie" "twio" "twiu" "twoa"
    "twoe" "twoi" "twou" "twua" "twue" "twui" "twuo"

    "dra" "dre" "dri"
    "dri" "dri" "drá" "dré" "drí" "dró" "drú" "dra" "dre" "dri" "dro"
    "dru" "drae" "drai" "drao" "drau" "drea" "drei" "dreo" "dreu" "dria"
    "drie" "drio" "driu" "droa" "droe" "droi" "drou" "drua" "drue"
    "drui" "druo"

    "dwa" "dwe" "dwi" "dwi" "dwi" "dwá" "dwé" "dwí" "dwó"
    "dwú" "dwa" "dwe" "dwi" "dwo" "dwu" "dwae" "dwai" "dwao" "dwau"
    "dwea" "dwei" "dweo" "dweu" "dwia" "dwie" "dwio" "dwiu" "dwoa"
    "dwoe" "dwoi" "dwou" "dwua" "dwue" "dwui" "dwuo"

    "sra" "sre" "sri"
    "sri" "sri" "srá" "sré" "srí" "sró" "srú" "sra" "sre" "sri" "sro"
    "sru" "srae" "srai" "srao" "srau" "srea" "srei" "sreo" "sreu" "sria"
    "srie" "srio" "sriu" "sroa" "sroe" "sroi" "srou" "srua" "srue"
    "srui" "sruo"

    "sla" "sle" "sli" "sli" "sli" "slá" "slé" "slí" "sló"
    "slú" "sla" "sle" "sli" "slo" "slu" "slae" "slai" "slao" "slau"
    "slea" "slei" "sleo" "sleu" "slia" "slie" "slio" "sliu" "sloa"
    "sloe" "sloi" "slou" "slua" "slue" "slui" "sluo"

    "swa" "swe" "swi"
    "swi" "swi" "swá" "swé" "swí" "swó" "swú" "swa" "swe" "swi" "swo"
    "swu" "swae" "swai" "swao" "swau" "swea" "swei" "sweo" "sweu" "swia"
    "swie" "swio" "swiu" "swoa" "swoe" "swoi" "swou" "swua" "swue"
    "swui" "swuo"

    "shra" "shre" "shri" "shri" "shri" "shrá" "shré"
    "shrí" "shró" "shrú" "shra" "shre" "shri" "shro" "shru" "shrae"
    "shrai" "shrao" "shrau" "shrea" "shrei" "shreo" "shreu" "shria"
    "shrie" "shrio" "shriu" "shroa" "shroe" "shroi" "shrou" "shrua"
    "shrue" "shrui" "shruo"

    "shla" "shle" "shli" "shli" "shli" "shlá"
    "shlé" "shlí" "shló" "shlú" "shla" "shle" "shli" "shlo" "shlu"
    "shlae" "shlai" "shlao" "shlau" "shlea" "shlei" "shleo" "shleu"
    "shlia" "shlie" "shlio" "shliu" "shloa" "shloe" "shloi" "shlou"
    "shlua" "shlue" "shlui" "shluo"

    "shwa" "shwe" "shwi" "shwi" "shwi"
    "shwá" "shwé" "shwí" "shwó" "shwú" "shwa" "shwe" "shwi" "shwo"
    "shwu" "shwae" "shwai" "shwao" "shwau" "shwea" "shwei" "shweo"
    "shweu" "shwia" "shwie" "shwio" "shwiu" "shwoa" "shwoe" "shwoi"
    "shwou" "shwua" "shwue" "shwui" "shwuo"

    "cra" "cre" "cri" "cri"
    "cri" "crá" "cré" "crí" "cró" "crú" "cra" "cre" "cri" "cro" "cru"
    "crae" "crai" "crao" "crau" "crea" "crei" "creo" "creu" "cria"
    "crie" "crio" "criu" "croa" "croe" "croi" "crou" "crua" "crue"
    "crui" "cruo"

    "cla" "cle" "cli" "cli" "cli" "clá" "clé" "clí" "cló"
    "clú" "cla" "cle" "cli" "clo" "clu" "clae" "clai" "clao" "clau"
    "clea" "clei" "cleo" "cleu" "clia" "clie" "clio" "cliu" "cloa"
    "cloe" "cloi" "clou" "clua" "clue" "clui" "cluo"

    "cwa" "cwe" "cwi"
    "cwi" "cwi" "cwá" "cwé" "cwí" "cwó" "cwú" "cwa" "cwe" "cwi" "cwo"
    "cwu" "cwae" "cwai" "cwao" "cwau" "cwea" "cwei" "cweo" "cweu" "cwia"
    "cwie" "cwio" "cwiu" "cwoa" "cwoe" "cwoi" "cwou" "cwua" "cwue"
    "cwui" "cwuo"

    "gra" "gre" "gri" "gri" "gri" "grá" "gré" "grí" "gró"
    "grú" "gra" "gre" "gri" "gro" "gru" "grae" "grai" "grao" "grau"
    "grea" "grei" "greo" "greu" "gria" "grie" "grio" "griu" "groa"
    "groe" "groi" "grou" "grua" "grue" "grui" "gruo"

    "gla" "gle" "gli"
    "gli" "gli" "glá" "glé" "glí" "gló" "glú" "gla" "gle" "gli" "glo"
    "glu" "glae" "glai" "glao" "glau" "glea" "glei" "gleo" "gleu" "glia"
    "glie" "glio" "gliu" "gloa" "gloe" "gloi" "glou" "glua" "glue"
    "glui" "gluo"

    "gwa" "gwe" "gwi" "gwi" "gwi" "gwá" "gwé" "gwí" "gwó"
    "gwú" "gwa" "gwe" "gwi" "gwo" "gwu" "gwae" "gwai" "gwao" "gwau"
    "gwea" "gwei" "gweo" "gweu" "gwia" "gwie" "gwio" "gwiu" "gwoa"
    "gwoe" "gwoi" "gwou" "gwua" "gwue" "gwui" "gwuo"

    "hra" "hre" "hri"
    "hri" "hri" "hrá" "hré" "hrí" "hró" "hrú" "hra" "hre" "hri" "hro"
    "hru" "hrae" "hrai" "hrao" "hrau" "hrea" "hrei" "hreo" "hreu" "hria"
    "hrie" "hrio" "hriu" "hroa" "hroe" "hroi" "hrou" "hrua" "hrue"
    "hrui" "hruo"

    "hla" "hle" "hli" "hli" "hli" "hlá" "hlé" "hlí" "hló"
    "hlú" "hla" "hle" "hli" "hlo" "hlu" "hlae" "hlai" "hlao" "hlau"
    "hlea" "hlei" "hleo" "hleu" "hlia" "hlie" "hlio" "hliu" "hloa"
    "hloe" "hloi" "hlou" "hlua" "hlue" "hlui" "hluo"

    "hwa" "hwe" "hwi"
    "hwi" "hwi" "hwá" "hwé" "hwí" "hwó" "hwú" "hwa" "hwe" "hwi" "hwo"
    "hwu" "hwae" "hwai" "hwao" "hwau" "hwea" "hwei" "hweo" "hweu" "hwia"
    "hwie" "hwio" "hwiu" "hwoa" "hwoe" "hwoi" "hwou" "hwua" "hwue"
    "hwui" "hwuo"))
