;;; folio-babel.el --- Folio mode languages and scripts

;; Copyright (C) 2012, 2013  Christoph W. Kluge

;; Author: Christoph W. Kluge <shift.in.emphasis@gmail.com>
;; Keywords: wp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds general support for languages and scripts to
;; Folio mode.  That includes stop words and language guessing,
;; ordinal suffixes, bidirectional formatting codes, and in particular
;; the defun `folio-language-info' for retrieving data stored in
;; key-value form for a given BCP 47 language tag.

;;; Code:

;; XXX words not ending sentences: direct, indirect articles, pronouns
;; XXX kinsoku processing--punctuation: EOL: opening parentheses,
;;   opening quotes, English 'I'

(require 'folio-time)

;;;###autoload
(defun folio-pluralize (word number)
  "Ultra trivial pluralization of English nouns."
  (if (or (zerop number) (> number 1))
      (concat word "s")
    word))

(defconst folio-language-stop-words-english
  (purecopy
   '("a" "about" "above" "after" "again" "against" "all" "am" "an" "and"
     "any" "aren't" "as" "at" "because" "before" "below" "between" "both"
     "but" "by" "can" "can't" "cannot" "could" "couldn't" "did" "didn't"
     "does" "doesn't" "doing" "don't" "down" "during" "each" "few" "for"
     "from" "further" "hadn't" "hasn't" "haven't" "he'd" "he'll" "he's"
     "here" "here's" "how" "how's" "i'd" "i'll" "i'm" "i've" "if" "in"
     "into" "isn't" "it's" "let's" "may" "might" "more" "most" "must"
     "mustn't" "no" "nor" "not" "of" "off" "on" "once" "only" "or" "other"
     "ought" "out" "over" "own" "same" "shall" "shan't" "she'd" "she'll"
     "she's" "should" "shouldn't" "so" "some" "such" "than" "that" "that's"
     "the" "then" "there" "there's" "these" "they'd" "they'll" "they're"
     "they've" "this" "those" "through" "to" "too" "under" "until" "up"
     "us" "very" "wasn't" "we'd" "we'll" "we're" "we've" "weren't" "what"
     "what's" "when" "when's" "where" "where's" "which" "while" "who" "who's"
     "whom" "why" "why's" "will" "with" "won't" "would" "wouldn't" "you'd"
     "you'll" "you're" "you've"))
  "*List of stop-words for English as the base language.  Homonym
forms like \"can\" or \"must\" are included for the purpose.")

(defconst folio-language-common-words-english
  (purecopy
   '("also" "another" "back" "even" "ever" "every" "first" "five" "four"
     "get" "go" "goes" "high" "however" "just" "least" "less" "like" "long"
     "made" "make" "many" "never" "new" "now" "old" "one" "put" "said" "say"
     "says" "second" "see" "seen" "since" "still" "take" "three" "two" "way"
     "well" "whether"))
  "Common words for English as the base language.")

(defconst folio-language-suffixes-british (purecopy
  '("our" "oured" "ouring" "iaeval" "elt" "sation" "sational" "se" "sed" "sing"))
"Word suffixes to identify British spelling.
With English as the identified base language these word suffixes
can be used to distinguish between British and American spelling.
Examples for British spelling are humour, humoured, humouring,
mediaeval, misspelt, localisation, localisational, localise,
localised, localising.  The suffix \"se\" probably should be
dropped or treated specially as not being sufficiently unique.")

(defconst folio-language-regexp-british-english
  (cons (concat "\\<\\("
                (regexp-opt
                 (append folio-language-stop-words-english
                         folio-language-common-words-english)) "\\)\\>"
                         "\\|\\B" (regexp-opt
                                   folio-language-suffixes-british) "\\>")
        (+ (length folio-language-stop-words-english)
           (length folio-language-common-words-english)
           (length folio-language-suffixes-british)))
  "Regular expression to identify English words in British spelling.
Its value is a cons of the optimized regexp and its relative
length.")

(defconst folio-language-suffixes-american
  '("or" "ored" "oring" "ieval" "led" "zation" "zational" "ze" "zed" "zing")
  "Word suffixes to identify American spelling.
With English as the identified base language these word suffixes
can be used to distinguish between British and American spelling.
Examples for American spellings are humor, humored, humoring,
medieval, misspelled, localization, localizational, localize,
localized, localizing.  The suffix \"ze\" probably should be
dropped or treated specially as not being sufficiently unique.")

(defconst folio-language-regexp-american-english
  (cons (concat "\\<\\("
                (regexp-opt
                 (append folio-language-stop-words-english
                         folio-language-common-words-english)) "\\)\\>"
                         "\\|\\B" (regexp-opt
                                   folio-language-suffixes-american) "\\>")
        (+ (length folio-language-stop-words-english)
           (length folio-language-common-words-english)
           (length folio-language-suffixes-american)))
  "Regular expression to identify English words in American spelling.
Its value is a cons of the optimized regexp and its relative
length.")

(defconst folio-language-stop-words-french
  (purecopy
   '("ai" "aie" "aient" "aies" "ait" "as" "au" "aura" "aurai" "auraient"
     "aurais" "aurait" "auras" "aurez" "auriez" "aurions" "aurons" "auront"
     "aux" "avaient" "avais" "avait" "avec" "avez" "aviez" "avions" "avons"
     "ayant" "ayez" "ayons" "ce" "ceci" "celà" "ces" "cet" "cette" "dans"
     "de" "des" "du" "elle" "en" "es" "est" "et" "eu" "eue" "eues" "eurent"
     "eus" "eusse" "eussent" "eusses" "eussiez" "eussions" "eut" "eux"
     "eûmes" "eût" "eûtes" "furent" "fus" "fusse" "fussent" "fusses"
     "fussiez" "fussions" "fut" "fûmes" "fût" "fûtes" "ici" "il" "ils" "je"
     "la" "le" "les" "leur" "leurs" "lui" "ma" "mais" "me" "mes" "moi"
     "mon" "même" "ne" "nos" "notre" "nous" "on" "ont" "ou" "par" "pas"
     "pour" "qu" "que" "quel" "quelle" "quelles" "quels" "qui" "sa" "sans"
     "se" "sera" "serai" "seraient" "serais" "serait" "seras" "serez"
     "seriez" "serions" "serons" "seront" "ses" "soi" "soient" "sois"
     "soit" "sommes" "son" "sont" "soyez" "soyons" "suis" "sur" "ta" "te"
     "tes" "toi" "ton" "tu" "un" "une" "vos" "votre" "vous" "à" "étaient"
     "étais" "était" "étant" "étiez" "étions" "été" "étée" "étées" "étés"
     "êtes"))
  "*List of French stop-words.")

(defconst folio-language-regexp-french
  (cons (concat "\\<\\("
                (regexp-opt
                 folio-language-stop-words-french) "\\)\\>")
        (length folio-language-stop-words-french))
  "Regular expression to identify French as the base language.
Its value is a cons of the optimized regexp and its relative
length.")

(defconst folio-language-stop-words-italian
  (purecopy
   '("a" "abbia" "abbiamo" "abbiano" "abbiate" "ad" "agl" "agli" "ai"
     "al" "all" "alla" "alle" "allo" "anche" "avemmo" "avendo" "avesse"
     "avessero" "avessi" "avessimo" "aveste" "avesti" "avete" "aveva"
     "avevamo" "avevano" "avevate" "avevi" "avevo" "avrai" "avranno"
     "avrebbe" "avrebbero" "avrei" "avremmo" "avremo" "avreste"
     "avresti" "avrete" "avrà" "avrò" "avuta" "avute" "avuti" "avuto"
     "c" "che" "chi" "ci" "coi" "col" "come" "con" "contro" "cui" "da"
     "dagl" "dagli" "dai" "dal" "dall" "dalla" "dalle" "dallo" "degl"
     "degli" "dei" "del" "dell" "della" "delle" "dello" "di" "dov"
     "dove" "e" "ebbe" "ebbero" "ebbi" "ed" "era" "erano" "eravamo"
     "eravate" "eri" "ero" "essendo" "faccia" "facciamo" "facciano"
     "facciate" "faccio" "facemmo" "facendo" "facesse" "facessero"
     "facessi" "facessimo" "faceste" "facesti" "faceva" "facevamo"
     "facevano" "facevate" "facevi" "facevo" "fai" "fanno" "farai"
     "faranno" "farebbe" "farebbero" "farei" "faremmo" "faremo"
     "fareste" "faresti" "farete" "farà" "farò" "fece" "fecero" "feci"
     "fosse" "fossero" "fossi" "fossimo" "foste" "fosti" "fu" "fui"
     "fummo" "furono" "gli" "ha" "hai" "hanno" "ho" "i" "il" "in" "io"
     "l" "la" "le" "lei" "li" "lo" "loro" "lui" "ma" "mi" "mia" "mie"
     "miei" "mio" "ne" "negl" "negli" "nei" "nel" "nell" "nella" "nelle"
     "nello" "noi" "non" "nostra" "nostre" "nostri" "nostro" "o" "per"
     "perché" "più" "quale" "quanta" "quante" "quanti" "quanto" "quella"
     "quelle" "quelli" "quello" "questa" "queste" "questi" "questo"
     "sarai" "saranno" "sarebbe" "sarebbero" "sarei" "saremmo" "saremo"
     "sareste" "saresti" "sarete" "sarà" "sarò" "se" "sei" "si" "sia"
     "siamo" "siano" "siate" "siete" "sono" "sta" "stai" "stando"
     "stanno" "starai" "staranno" "starebbe" "starebbero" "starei"
     "staremmo" "staremo" "stareste" "staresti" "starete" "starà"
     "starò" "stava" "stavamo" "stavano" "stavate" "stavi" "stavo"
     "stemmo" "stesse" "stessero" "stessi" "stessimo" "steste" "stesti"
     "stette" "stettero" "stetti" "stia" "stiamo" "stiano" "stiate"
     "sto" "su" "sua" "sue" "sugl" "sugli" "sui" "sul" "sull" "sulla"
     "sulle" "sullo" "suo" "suoi" "ti" "tra" "tu" "tua" "tue" "tuo"
     "tuoi" "tutti" "tutto" "un" "una" "uno" "vi" "voi" "vostra"
     "vostre" "vostri" "vostro" "è"))
  "*List of Italian stop-words.
These include single letter forms like \"a\" (at) \"i\" (the),
\"e\" (and), and \"o\" (or), forms of \"avere\" (to have),
\"essere\" (to be), \"fare\" (to do), and \"stare\" (to be).")

(defconst folio-language-regexp-italian
  (cons (concat "\\<\\("
                (regexp-opt
                 folio-language-stop-words-italian) "\\)\\>")
        (length folio-language-stop-words-italian))
  "Regular expression to identify Italian as the base language.
Its value is a cons of the optimized regexp and its relative
length.")

(defconst folio-language-stop-words-spanish
  (purecopy
   '("a" "al" "algo" "algunas" "algunos" "ante" "antes" "como" "con"
     "contra" "cual" "cuando" "de" "del" "desde" "donde" "durante" "e" "el"
     "ella" "ellas" "ellos" "en" "entre" "era" "erais" "eran" "eras" "eres"
     "es" "esa" "esas" "ese" "eso" "esos" "esta" "estaba" "estabais"
     "estaban" "estabas" "estad" "estada" "estadas" "estado" "estados"
     "estamos" "estando" "estar" "estaremos" "estará" "estarán" "estarás"
     "estaré" "estaréis" "estaría" "estaríais" "estaríamos" "estarían"
     "estarías" "estas" "este" "estemos" "esto" "estos" "estoy" "estuve"
     "estuviera" "estuvierais" "estuvieran" "estuvieras" "estuvieron"
     "estuviese" "estuvieseis" "estuviesen" "estuvieses" "estuvimos"
     "estuviste" "estuvisteis" "estuviéramos" "estuviésemos" "estuvo"
     "está" "estábamos" "estáis" "están" "estás" "esté" "estéis" "estén"
     "estés" "fue" "fuera" "fuerais" "fueran" "fueras" "fueron" "fuese"
     "fueseis" "fuesen" "fueses" "fui" "fuimos" "fuiste" "fuisteis"
     "fuéramos" "fuésemos" "ha" "habida" "habidas" "habido" "habidos"
     "habiendo" "habremos" "habrá" "habrán" "habrás" "habré" "habréis"
     "habría" "habríais" "habríamos" "habrían" "habrías" "habéis" "había"
     "habíais" "habíamos" "habían" "habías" "han" "has" "hasta" "hay"
     "haya" "hayamos" "hayan" "hayas" "hayáis" "he" "hemos" "hube"
     "hubiera" "hubierais" "hubieran" "hubieras" "hubieron" "hubiese"
     "hubieseis" "hubiesen" "hubieses" "hubimos" "hubiste" "hubisteis"
     "hubiéramos" "hubiésemos" "hubo" "la" "las" "le" "les" "lo" "los" "me"
     "mi" "mis" "mucho" "muchos" "muy" "más" "mí" "mía" "mías" "mío" "míos"
     "nada" "ni" "no" "nos" "nosotras" "nosotros" "nuestra" "nuestras"
     "nuestro" "nuestros" "o" "os" "otra" "otras" "otro" "otros" "para"
     "pero" "poco" "por" "porque" "que" "quien" "quienes" "qué" "se" "sea"
     "seamos" "sean" "seas" "seremos" "será" "serán" "serás" "seré"
     "seréis" "sería" "seríais" "seríamos" "serían" "serías" "seáis" "sido"
     "siendo" "sin" "sobre" "sois" "somos" "son" "soy" "su" "sus" "suya"
     "suyas" "suyo" "suyos" "sí" "también" "tanto" "te" "tendremos"
     "tendrá" "tendrán" "tendrás" "tendré" "tendréis" "tendría" "tendríais"
     "tendríamos" "tendrían" "tendrías" "tened" "tenemos" "tenga"
     "tengamos" "tengan" "tengas" "tengo" "tengáis" "tenida" "tenidas"
     "tenido" "tenidos" "teniendo" "tenéis" "tenía" "teníais" "teníamos"
     "tenían" "tenías" "ti" "tiene" "tienen" "tienes" "todo" "todos" "tu"
     "tus" "tuve" "tuviera" "tuvierais" "tuvieran" "tuvieras" "tuvieron"
     "tuviese" "tuvieseis" "tuviesen" "tuvieses" "tuvimos" "tuviste"
     "tuvisteis" "tuviéramos" "tuviésemos" "tuvo" "tuya" "tuyas" "tuyo"
     "tuyos" "tú" "un" "una" "uno" "unos" "vosotras" "vosotros" "vuestra"
     "vuestras" "vuestro" "vuestros" "y" "ya" "yo" "él" "éramos"))
   "*List of Spanish stop-words.
These include single letter forms like \"a\" (to), \"y\" (to),
\"o\" (or), \"e\" (and), forms of \"estar\" (to be),
\"haber\" (to have) \"ser\" (to be) \"tener\" (to have).")

(defconst folio-language-regexp-spanish
  (cons (concat "\\<\\("
                (regexp-opt
                 folio-language-stop-words-spanish) "\\)\\>")
        (length folio-language-stop-words-spanish))
  "Regular expression to identify Spanish as the base language.
Its value is a cons of the optimized regexp and its relative
length.")

(defconst folio-language-stop-words-portuguese
  (purecopy
   '("a" "ao" "aos" "aquela" "aquelas" "aquele" "aqueles" "aquilo" "as"
     "até" "com" "como" "da" "das" "de" "dela" "delas" "dele" "deles"
     "depois" "do" "dos" "e" "ela" "elas" "ele" "eles" "em" "entre" "era"
     "eram" "essa" "essas" "esse" "esses" "esta" "estamos" "estas" "estava"
     "estavam" "este" "esteja" "estejam" "estejamos" "estes" "esteve"
     "estive" "estivemos" "estiver" "estivera" "estiveram" "estiverem"
     "estivermos" "estivesse" "estivessem" "estivéramos" "estivéssemos"
     "estou" "está" "estávamos" "estão" "eu" "foi" "fomos" "for" "fora"
     "foram" "forem" "formos" "fosse" "fossem" "fui" "fôramos" "fôssemos"
     "haja" "hajam" "hajamos" "havemos" "hei" "houve" "houvemos" "houver"
     "houvera" "houveram" "houverei" "houverem" "houveremos" "houveria"
     "houveriam" "houvermos" "houverá" "houverão" "houveríamos" "houvesse"
     "houvessem" "houvéramos" "houvéssemos" "há" "hão" "isso" "isto" "já"
     "lhe" "lhes" "mais" "mas" "me" "mesmo" "meu" "meus" "minha" "minhas"
     "muito" "na" "nas" "nem" "no" "nos" "nossa" "nossas" "nosso" "nossos"
     "num" "numa" "não" "nós" "o" "os" "ou" "para" "pela" "pelas" "pelo"
     "pelos" "por" "qual" "quando" "que" "quem" "se" "seja" "sejam"
     "sejamos" "sem" "serei" "seremos" "seria" "seriam" "será" "serão"
     "seríamos" "seu" "seus" "somos" "sou" "sua" "suas" "são" "só" "também"
     "te" "tem" "temos" "tenha" "tenham" "tenhamos" "tenho" "terei"
     "teremos" "teria" "teriam" "terá" "terão" "teríamos" "teu" "teus"
     "teve" "tinha" "tinham" "tive" "tivemos" "tiver" "tivera" "tiveram"
     "tiverem" "tivermos" "tivesse" "tivessem" "tivéramos" "tivéssemos"
     "tu" "tua" "tuas" "tém" "tínhamos" "um" "uma" "você" "vocês" "vos" "à"
     "às" "éramos"))
  "*List of Portuguese stop-words.
These include single letter forms like \"a\" (the, her),
\"o\" (the, him), \"e\" (and), forms of \"estar\" (to be),
\"haver\" (to have), \"ser\" (to be), and \"ter\" (to have).")

(defconst folio-language-regexp-portuguese
  (cons (concat "\\<\\("
                (regexp-opt
                 folio-language-stop-words-portuguese) "\\)\\>")
        (length folio-language-stop-words-portuguese))
  "Regular expression to identify Portuguese as the base language.
Its value is a cons of the optimized regexp and its relative
length.")

(defconst folio-language-stop-words-german
  (purecopy
   '("aber" "alle" "allem" "allen" "aller" "alles" "als" "also" "am" "an"
     "ander" "andere" "anderem" "anderen" "anderer" "anderes" "anderem"
     "anderen" "anderer" "anders" "auch" "auf" "aus" "bei" "bin" "bis" "bist"
     "da" "damit" "dann" "das" "dasselbe" "dazu" "daß" "dass" "dein" "deine"
     "deinem" "deinen" "deiner" "deines" "dem" "demselben" "den" "denn"
     "denselben" "der" "derer" "derselbe" "derselben" "des" "desselben"
     "dessen" "dich" "die" "dies" "diese" "dieselbe" "dieselben" "diesem"
     "diesen" "dieser" "dieses" "dir" "doch" "dort" "du" "durch" "ein"
     "eine" "einem" "einen" "einer" "eines" "einig" "einige" "einigem"
     "einigen" "einiger" "einiges" "einmal" "er" "es" "etwas" "euch" "euer"
     "eure" "eurem" "euren" "eurer" "eures" "für" "gegen" "gewesen" "hab"
     "habe" "haben" "hat" "hatte" "hatten" "hier" "hin" "hinter" "ich"
     "ihm" "ihn" "ihnen" "ihr" "ihre" "ihrem" "ihren" "ihrer" "ihres" "im"
     "in" "indem" "ins" "ist" "jede" "jedem" "jeden" "jeder" "jedes" "jene"
     "jenem" "jenen" "jener" "jenes" "jetzt" "kann" "kein" "keine" "keinem"
     "keinen" "keiner" "keines" "können" "könnte" "machen" "man" "manche"
     "manchem" "manchen" "mancher" "manches" "mein" "meine" "meinem"
     "meinen" "meiner" "meines" "mich" "mir" "mit" "muss" "musste" "nach"
     "nicht" "nichts" "noch" "nun" "nur" "ob" "oder" "ohne" "sehr" "sein"
     "seine" "seinem" "seinen" "seiner" "seines" "selbst" "sich" "sie"
     "sind" "so" "solche" "solchem" "solchen" "solcher" "solches" "soll"
     "sollte" "sondern" "sonst" "um" "und" "uns" "unser" "unsere" "unserer"
     "unseres" "unter" "viel" "vom" "von" "vor" "war" "waren" "warst"
     "was" "weg" "weil" "weiter" "welche" "welchem" "welchen" "welcher"
     "welches" "wenn" "werde" "werden" "wie" "wieder" "will" "wir" "wird"
     "wirst" "wo" "wollen" "wollte" "während" "würde" "würden" "zu" "zum"
     "zur" "zwar" "zwischen" "über"))
  "*List of stop-words for German as the base language.")

(defconst folio-language-regexp-german
  (cons (concat "\\<\\("
                (regexp-opt
                 folio-language-stop-words-german) "\\)\\>")
        (length folio-language-stop-words-german))
  "Regular expression to identify German as the base language.
Its value is a cons of the optimized regexp and its relative
length.")

(defconst folio-language-ordinal-suffixes-english
  '("st" "nd" ("rd" (:alternative "d")) "th"))

(defconst folio-language-ordinal-suffix-french
  '("er" "re" ("e" (:common-misspelling "ème"))))

(defconst folio-language-ordinal-suffixes-german
  '("er" "te" "tes" "ten" "tem"))

(defconst folio-language-info-alist
  '(("ang" . ((name . "Old English")
              (alt-name . "Old English (ca. 450-1100)")))
    ("ca" . ((name . "Català")
             (alt-name . "Catalan (Valencian)")))
    ("cy" . ((name . "Cymraeg")
             (alt-name . "Welsh")))
    ("da" . ((name . "Dansk")
             (alt-name . "Danish")))
    ("de" . ((name . "Deutsch")
             (alt-name . "German")
             (stop-words . folio-language-stop-words-german)
             (regexp . folio-language-regexp-german)))
    ("de-1901" . ((name . "Deutsch (Rechtschreibung von 1901)")
                  (alt-name . "German (orthography of 1901)")
                  (stop-words . folio-language-stop-words-german)
                  (regexp . folio-language-regexp-german)))
    ("el" . ((name . "Ελληνικά (1453-)")
             (alt-name . "Modern Greek (1453-)")))
    ("en-GB" . ((base . "en")
                (name . "British English")
                (alt-name . "English (GB)")
                (stop-words . folio-language-stop-words-english)
                (common-words . folio-language-common-words-english)
                (regexp . folio-language-regexp-british-english)))
    ("en-US" . ((base . "en")
                (name . "American English")
                (alt-name . "English (US)")
                (stop-words . folio-language-stop-words-english)
                (common-words . folio-language-common-words-english)
                (regexp . folio-language-regexp-american-english)))
    ("enm" . ((name . "Middle English (1100-1500)")
              (alt-name . "Middle English (1100-1500)")))
    ("es" . ((name . "español (castellano)")
             (alt-name . "Spanish (Castilian)")
             (stop-words . folio-language-stop-words-spanish)
             (regexp . folio-language-regexp-spanish)))
    ("fi" . ((name . "suomi")
             (alt-name . "Finnish")))
    ("fr" . ((name . "français")
             (alt-name . "French")
             (stop-words . folio-language-stop-words-french)
             (regexp . folio-language-regexp-french)))
    ("frm" . ((name . "moyen français")
              (alt-name . "Middle French (ca. 1400-1600)")))
    ("fro" . ((name . "ancien français")
              (alt-name . "Old French (842-ca. 1400)")))
    ("gd" . ((name . "Gàidhlig")
             (alt-name . "Scottish Gaelic")))
    ("grc" . ((name . "Ἑλληνικά (to 1453)")
              (alt-name . "Ancient Greek (to 1453)")))
    ("he" . ((name . "עִבְרִית, עברית")
             (alt-name . "Hebrew")
             (dir . rtl)))
    ("hbo" . ((name . "עִבְרִית, עברית (ancient)")
              (alt-name . "Ancient Hebrew")
              (dir . rtl)))
    ("it" . ((name . "italiano")
             (alt-name . "Italian")
             (stop-words . folio-language-stop-words-italian)
             (regexp . folio-language-regexp-italian)))
    ("la" . ((name . "latine")
             (alt-name . "Latin")))
    ("nob" . ((name . "norvégien bokmål")
              (alt-name . "Norwegian Bokmål")))
    ("nno" . ((name . "norvégien nynorsk")
              (alt-name . "Norwegian Nynorsk")))
    ("oc" . ((name . "Occitani (Provençal)")
             (alt-name . "Occitan (Provenzale)")))
    ("pt-PT" . ((name . "português (Portugal)")
                (alt-name . "Portuguese (Portugal)")
                (stop-words . folio-language-stop-words-portuguese)
                (regexp . folio-language-regexp-portuguese)))
    ("pt-BR" . ((name . "português brasileiro")
                (alt-name . "Brazilian Portuguese")))
    ("sv" . ((name . "Svenska")
             (alt-name . "Swedish"))))
  "Alist pairing language codes with environment definitions.

An element is of the form:

        (LANGUAGE-CODE . ((KEY . INFO) ...))

where LANGUAGE-CODE is the name of the language environment, a
BCP 47 language tag, optionally with subtags, KEY is a symbol
denoting the kind of information, and INFO is the data associated
with KEY.  Meaningful values for KEY include

  base               value is a LANGUAGE-CODE specifying the base
                        language of this entry; \"en-GB\" for instance
                        should have an entry \(base . \"en\"\).
  name               value is a human readable string identifying the
                        environment as a verbatim alternative to the
                        LANGUAGE-CODE.
  alt-name           value is a human readable string identifying the
                        environment as a verbatim alternative to the
                        LANGUAGE-CODE, and an alternative to name.
  common-words       value is the symbol of a list of words common in
                        that language, excluding stop-words.
  stop-words         value is the symbol of a list of stop-words.
  regexp             value is the symbol of a predefined regexp for
                        identifying the language in a stretch of
                        text.  Normally it is set up from the
                        values of common word and stop-word
                        lists, optimized for scanning a buffer.")

;;;###autoload
(defun folio-left-to-right-mark ()
  "Insert an implicit left-to-right marker."
  (interactive)
  ;; The character count became optional with Emacs 24.
  (insert-char ?\u200E 1))

;;;###autoload
(defun folio-right-to-left-mark ()
  "Insert an implicit right-to-left marker."
  (interactive)
  (insert-char ?\u200F 1))

;;;###autoload
(defun folio-left-to-right-embedding ()
  "Insert an explicit left-to-right marker U+202A.
The marker should be paired by U+202C as with
`folio-pop-directional-formatting'."
  (interactive)
  (insert-char ?\u202A 1))

;;;###autoload
(defun folio-right-to-left-embedding ()
  "Insert an implicit right-to-left embedding marker U+202B. The
marker should be paired by U+202C as with
`folio-pop-directional-formatting'."
  (interactive)
  (insert-char ?\u202B 1))

;;;###autoload
(defun folio-left-to-right-override ()
  "Insert an explicit left-to-right override marker U+202D.
The marker should be paired by U+202C as with
`folio-pop-directional-formatting'."
  (interactive)
  (insert-char ?\u202D 1))

;;;###autoload
(defun folio-right-to-left-override ()
  "Insert an explicit right-to-left override marker U+202E. The
marker should be paired by U+202C as with
`folio-pop-directional-formatting'."
  (interactive)
  (insert-char ?\u202E 1))

;;;###autoload
(defun folio-pop-directional-formatting ()
  "Pop the directional formatting stack."
  (interactive)
  (insert-char ?\u202C 1))

;; XXX TODO
(defvar folio-directional-formatting-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x a l") 'folio-left-to-right-mark)
    (define-key map (kbd "C-x a r") 'folio-right-to-left-mark)
    map))

;;;###autoload
(defvar folio-directional-formatting-menu
  (let ((map (make-sparse-keymap "Insert BiDi control characters")))
    (define-key map [folio-left-to-right-mark]
      '(menu-item "Insert `LRM'" folio-left-to-right-mark :keys "C-x a l"))
    (define-key map [folio-right-to-left-mark]
      '(menu-item "Insert `RLM'" folio-right-to-left-mark :keys "C-x a r"))
    map))

;; XXX TODO properly position
;;;###autoload
(define-key menu-bar-edit-menu [folio-directional-formatting-menu]
  `(menu-item "Insert BiDi control characters" ,folio-directional-formatting-menu))

;;;###autoload
(defun folio-language-info (lang key &optional sub-key)
  "Return the language info entry for the tag LANG.
KEY identifies the entry, SUB-KEY an optional sub-key.  For
possible values of KEY and SUB-KEY see
`folio-language-info-alist'."
  (let ((entry (cdr (assoc lang folio-language-info-alist))))
    (when entry
      (let ((value (cdr (assoc key entry))))
        (if sub-key
            (when value
              (cdr (assoc sub-key value)))
          value)))))

(defun folio-guess-language-1 (regexp words)
  "Precursor for `folio-guess-language'."
  (when regexp
    (let ((re (car (symbol-value regexp)))
          (len (cdr (symbol-value regexp)))
          (match 0))
      (mapc (lambda (x)
              (when (string-match re x)
                (setq match (1+ match)))) words)
      ;; Normalize the match count by the number of words investigated
      ;; and the relative length of the regexp, i.e. the number of
      ;; constituents of that regexo.  The +1 is just for sanity
      ;; should one of the lists be empty; the exact value doesn't
      ;; matter either: it is a normalization after all.
      (/ (float match) (1+ len) (1+ (length words))))))

(defvar folio-guess-language-chunk-size 2000
  "The maximal chunk-size to use with `folio-guess-language'.")

;;;###autoload
(defun folio-guess-language (&optional beg end)
  "Guess language in region or buffer."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (restrict (or folio-guess-language-chunk-size
                       (buffer-size)))
         (words (let ((count 0)
                      bag)
                  (save-excursion
                    (goto-char beg)
                    (while (re-search-forward "\\<\\(\\sw+\\)\\>" end t)
                      (push (match-string-no-properties 0) bag)
                      (setq count (1+ count))
                      (when (>= count restrict)
                        (setq end (point)))))
                  bag))
         score)
    (mapc (lambda (x)
            (let* ((lang (car x))
                   (regexp (cdr (assoc 'regexp (cdr x))))
                   (lang-score (or (when regexp
                                     (folio-yield)
                                     (folio-guess-language-1
                                      regexp words)) 0)))
              (when (or (null score)
                        (and lang-score (> lang-score (cdr score))))
                (setq score (cons lang lang-score)))))
          folio-language-info-alist)
    ;; With a score at zero, or below some threshold (near 0.00158,
    ;; empirical) there possibly should be a fallback to querying a
    ;; spellchecker; this might be useful in particular when operating
    ;; on a region.
    (if (called-interactively-p 'interactive) ;; Visual feedback only.
        (message (or (and score
                          (not (zerop (cdr score)))
                          (car score)) "<unknown>"))
      (when score
        (car score)))))


(provide 'folio-babel)

;;; folio-babel.el ends here
