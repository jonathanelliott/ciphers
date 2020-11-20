(defn get-index [letter]
  (- (int letter) (int \a)))

(defn from-index [i]
  (char (+ i (int \a))))

(defn shift-letter [shift letter]
  (let [i (get-index letter)
        j (mod (+ i shift) 26)]
        (from-index j)))

(defn caesar [shift text]
  (map (partial shift-letter shift) text))

(def rot13
  (partial caesar 13))

(defn get-shifts [key-word]
  (map get-index (cycle key-word)))

(defn apply-shifts [shifts text]
  (map (partial apply shift-letter)
    (map vector shifts text)))

(defn tidy [word]
  (->> word
    (filter #(Character/isLetter %))
    (apply str)
    .toLowerCase))

(defn v-encrypt [key-word plaintext]
  (let [key-word (tidy key-word)]
    (->> plaintext
      tidy
      (apply-shifts (get-shifts key-word))
      (apply str)
      .toUpperCase)))

(defn v-decrypt [key-word ciphertext]
  (let [key-word (tidy key-word)]
    (->> ciphertext
      tidy
      .toLowerCase
      (apply-shifts (map - (get-shifts key-word)))
      (apply str))))

(def alphabet
  (apply vector
    (map from-index (range 0 26))))

(defn get-freq [text char]
  {char (count (filter #(= char %) text))})

(defn get-freqs
  ([chars text]
    (let [text (tidy text)]
    (apply merge (map (partial get-freq text) chars))))
  ([text]
    (get-freqs alphabet text)))

(defn encode-with [cipher text]
  "Not implemented")

(def three-letter-keys
  (for [a alphabet b alphabet c alphabet]
    (str a b c)))

(defn get-keys [x]
  (filter
    #(clojure.string/starts-with? %1 x)
    three-letter-keys))

(defn brute-force [key-list text]
  (loop [ks key-list]
    (if ks
      (let [k (first ks)]
        (print (str k ": "))
        (println (v-decrypt k text))
        (recur (next ks))))))

(def cryptogram-p43
  "TGCSZ GEUAA EFWGQ AHQMC")

(def cryptogram-p43'
  "TGCSZ GFVAA EFWGQ AHDMC")

(def stage-1
  "BT JPX RMLX PCUV AMLX ICVJP IBTWXVR CI M LMT’R PMTN, MTN YVCJX CDXV MWMBTRJ JPX AMTNGXRJBAH UQCT JPX QGMRJXV CI JPX YMGG CI JPX HBTW’R QMGMAX; MTN JPX HBTW RMY JPX QMVJ CI JPX PMTN JPMJ YVCJX. JPXT JPX HBTW’R ACUTJXTMTAX YMR APMTWXN, MTN PBR JPCUWPJR JVCUFGXN PBL, RC JPMJ JPX SCBTJR CI PBR GCBTR YXVX GCCRXN, MTN PBR HTXXR RLCJX CTX MWMBTRJ MTCJPXV. JPX HBTW AVBXN MGCUN JC FVBTW BT JPX MRJVCGCWXVR, JPX APMGNXMTR, MTN JPX RCCJPRMEXVR. MTN JPX HBTW RQMHX, MTN RMBN JC JPX YBRX LXT CI FMFEGCT, YPCRCXDXV RPMGG VXMN JPBR YVBJBTW, MTN RPCY LX JPX BTJXVQVXJMJBCT JPXVXCI, RPMGG FX AGCJPXN YBJP RAM")

(def stage-2
  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ JHLJBZ KPJABT HYJHUBT LZA ULBAYVU")

(def ciphertext-p30
  "DIX DR TZX KXCQDIQ RDK XIHPSZXKPIB TZPQ TXGT PQ TD QZDM TZX KXCJXK ZDM XCQPVN TZPQ TNSX DR HPSZXK HCI LX LKDUXI. TZX MDKJ QTKFHTFKX DR TZX SVCPITXGT ZCQ LXXI SKXQXKWXJ TD OCUX TZX XGXKHPQX XCQPXK. PR MX ZCJ MKPTTXI TZX HKNSTDBKCO PI BKDFSQ DR RPWX VXTTXKQ TZXI PT MDFVJ ZCWX LXXI ZCKJXK. TD HDIWPIHX NDFKQXVWXQ DR TZPQ SCPKQ SCPKQ DR KXCJXKQ HCI SKDWPJX XCHZ DTZXK MPTZ HKNSTDBKCOQ MPTZ TZPQ VXTTXK BKDFSIB")

(def all-threes
  (vec (map (partial apply str)
    (partition 3 "aahaalaasabaabsabyaceactaddadoadsadzaffaftagaageagoagsahaahiahsaidailaimainairaisaitalaalbaleallalpalsaltamaamiampamuanaandaneaniantanyapeapoappaptarbarcarearfarkarmarsartashaskaspateattaukavaaveavoawaaweawlawnaxeayeaysazobaabadbagbahbalbambanbapbarbasbatbaybedbeebegbelbenbesbetbeybibbidbigbinbiobisbitbizboabobbodbogboobopbosbotbowboxboybrabrobrrbubbudbugbumbunburbusbutbuybyebyscabcadcamcancapcarcatcawcayceecelcepchicigciscobcodcogcolconcoocopcorcoscotcowcoxcoycozcrucrycubcudcuecumcupcurcutcwmdabdaddagdahdakdaldamdandapdawdaydebdeedefdeldendevdewdexdeydibdiddiedifdigdimdindipdisditdocdoedogdoldomdondordosdotdowdrydubdudduedugduhduidunduodupdyeeareateauebbecuedhedseekeeleffefsefteggegoekeeldelfelkellelmelsemeemsemuendengenseoneraereergernerrersessetaetheveeweeyefabfadfanfarfasfatfaxfayfedfeefehfemfenferfesfetfeufewfeyfezfibfidfiefigfilfinfirfitfixfizfluflyfobfoefogfohfonfopforfoufoxfoyfrofryfubfudfugfunfurgabgadgaegaggalgamgangapgargasgatgaygedgeegelgemgengetgeyghigibgidgiegiggingipgitgnugoagobgoogorgosgotgoxgoygulgumgungutguvguygymgyphadhaehaghahhajhamhaohaphashathawhayhehhemhenhepherheshethewhexheyhichidhiehimhinhiphishithmmhobhodhoehoghonhophoshothowhoyhubhuehughuhhumhunhuphuthypiceichickicyidsiffifsiggilkillimpinkinninsionireirkismitsivyjabjagjamjarjawjayjeejetjeujewjibjigjinjobjoejogjotjowjoyjugjunjusjutkabkaekafkaskatkaykeakefkegkenkepkexkeykhikidkifkinkipkirkiskitkoakobkoikopkorkoskuekyelablacladlaglamlaplarlaslatlavlawlaxlaylealedleelegleileklesletleulevlexleylezliblidlielinliplislitlobloglooloplotlowloxluglumluvluxlyemacmadmaemagmanmapmarmasmatmawmaxmaymedmegmelmemmenmetmewmhomibmicmidmigmilmimmirmismixmoamobmocmodmogmolmommonmoomopmormosmotmowmudmugmummunmusmutmycnabnaenagnahnamnannapnawnaynebneenegnetnewnibnilnimnipnitnixnobnodnognohnomnoonornosnotnownthnubnunnusnutoafoakoaroatobaobeobiocaodaoddodeodsoesoffoftohmohoohsoilokaokeoldoleomsoneonoonsoohootopeopsoptoraorborcoreorsortoseoudouroutovaoweowlownoxooxypacpadpahpalpampanpapparpaspatpawpaxpaypeapecpedpeepegpehpenpepperpespetpewphiphtpiapicpiepigpinpippispitpiupixplypodpohpoipolpompoopoppotpowpoxproprypsipstpubpudpugpulpunpuppurpusputpyapyepyxqatqisquaradragrahrairajramranraprasratrawraxrayrebrecredreerefregreiremrepresretrevrexrhoriaribridrifrigrimrinriprobrocrodroeromrotrowrubruerugrumrunrutryaryesabsacsadsaesagsalsapsatsausawsaxsayseasecseesegseiselsensersetsewshasheshhshysibsicsimsinsipsirsissitsixskaskiskyslysobsodsolsomsonsopsossotsousowsoxsoyspaspysristysubsuesuksumsunsupsuqsyntabtadtaetagtajtamtantaotaptartastattautavtawtaxteatedteetegteltentettewthethothytictietiltintiptistittodtoetogtomtontootoptortottowtoytrytsktubtugtuituntuptuttuxtwatwotyeudoughukeuluummumpunsupoupsurburdurnurpuseutauteutsvacvanvarvasvatvauvavvawveevegvetvexviavidvievigvimvisvoevowvoxvugvumwabwadwaewagwanwapwarwaswatwawwaxwaywebwedweewenwetwhawhowhywigwinwiswitwizwoewokwonwoowoswotwowwrywudwyewynxisyagyahyakyamyapyaryawyayyeayehyenyepyesyetyewyinyipyobyodyokyomyonyouyowyukyumyupzagzapzaszaxzedzeezekzepzigzinzipzitzoazoozuzzzz"))))