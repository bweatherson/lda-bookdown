require(tm)

common_words <- stopwords()
latex_words <- c("aastex", "amsbsy", "amsfonts", "amsmath", "amssymb", "amsxtra", "begin", "cal", "cyr", "declaremathsize", "declaretextfont", "document", "document class", "empty", "encodingdefault", "end", "fontenc", "landscape", "mayhem", "mathrsfs", "math strut", "newcommand", "normalfont", "pagestyle", "pifont", "portland", "renewcommand", "rmdefault", "selectfont", "sfdefault", "stmaryrd", "textcomp", "textcyr", "usepackage", "wmcyr", "wncyss", "xspace", "documentclass", "declaretextfontcommand", "wncyr", "declaremathsizes", "mathrm", "vert", "mathstrut", "hat", "mathbf", "thinspace", "ldots", "neg", "bbb", "ebc", "cdot", "boldsymbol", "vec","langle", "rangle", "leq", "infty", "mathsf","vdash", "boldmath", "boldsymbol", "cwmi", "forall", "mathrel", "mbox", "prfm", "neq", "anid")
greek_words <- c("alpha", "beta", "gamma", "delta", "omega", "theta", "lambda", "rho", "psi", "phi", "sigma")
gendered_words <- c("she", "her", "him", "his", "prof", "mrs", "professor")
foreign_words <- c("auch", "aussi", "autre", "cette", "diese", "haben", "leur", "soit", "toute", "peut", "noch", "habe", "wenn", "einem", "doch", "durch", "kann", "comme", "aber", "mais", "nur", "wird", "wie", "sont", "ich", "dieser", "oder", "avec", "une", "werden", "bien", "sie", "auf", "einer", "dans", "dass", "esta", "nicht", "entre", "uns", "ont", "que", "wir", "nach", "einen", "como", "esprit", "seine", "elles", "fait", "elle", "eine", "lui", "selbst", "aus", "deux", "vom", "pensee", "schon", "zum", "nin", "propre", "les", "pour", "espace", "las", "una", "amour", "sind", "etre", "ueber", "biran", "das", "bei", "qui", "temps", "mich", "alcan", "sich", "ein", "zur", "idee", "welt", "philosophique", "mir", "vie", "homme", "ces", "maupertuis", "leipzig", "als", "essai", "del", "sens", "hier", "monde", "und", "histoire", "soi", "por", "des", "den", "bachelard", "logique", "sans", "meyerson", "filosofia", "bourgeois", "sein", "philosophie", "ist", "meiner", "zeit", "raison", "tarde", "begriff", "los", "theorie", "dem", "der", "pas", "revue", "uber", "veblen", "mas", "weil", "ser", "philosophische", "psychologie", "milieu", "geschichte")
foreign_words <- c(foreign_words, "sur", "dire", "ses", "une", "les", "que", "est", "etc")
foreign_words <- c(foreign_words,   "cest", "vii",
                   "dun", 
                   "quon", 
                   "lidee", 
                   "ete", 
                   "moins", 
                   "dune", 
                   "meme", 
                   "quil", 
                   "aux", 
                   "celle", 
                   "quelle", 
                   "nos", 
                   "tout", 
                   "nest",
                   "chez",
                   "dont",
                   "notre",
                   "quod",
                   "son",
                   "plus",
                   "encore", "faut", "toujours", "sous", "cet", "lon", "maniere", "donc", "faire", "par", "maine")
ref_words <- c("doi", "proceedings", "review", "journal", "press", "compilation", "compilation", "editors", "supplementary", "quarterly", "aristotelian", "kegan", "dordrecht", "minnesota", "reidel", "edu", "stanford", "oxford", "cambridge", "basil", "blackwell", "thanks", "cit", "mit", "eds", "loc", "york", "university", "nous", "chicago", "clarendon", "edited")

# I got a topic with these keywords: usage, word, moores, ordinary, using, uses, austin, verbal, expressions, incorrect, everyday, use, used, phrases, words
# I'm not cutting them because I think this really was a topic
# See, for instance, Passmore 1954 on usage
ordinary_words <- c("get", "got", "sure", "try", "ask", "asked", "asking", "admit", "admitted", "commonly", "difficult", "deal", 
                    "granted", "discuss", "fairly", "hardly", "probably", "extremely", "certainly", "careful",
                    "tell", "gets", "trying", "talking", "telling", "going", "getting", "putting", "finding", "inclined",
                    "feel"     , "really"  ,  "think"    , "trouble"  ,
                    "told"     , "anyone"  ,  "wrong"    , "strange"  ,
                    "hard"     , "absurd"  ,  "want"     , "else"     ,
                     "surely"  ,  "say"    ,   "put"     ,  "perfectly",
                     "mistaken",  "answer" ,   "quite"   ,  "people"   ,
                     "plain"   ,  "tells"  ,   "heard"   ,  "supposing",
                     "room"    ,  "look"   ,   "somehow" ,  "sometimes",
                     "tried"   ,  "happens",   "coming"  ,  "perhaps"  ,
                     "easy"    ,  "miss"   ,   "prepared",  "calling"  ,
                     "seemed"  ,  "suppose",   "happen"  ,  "give"     ,
                     "seem", "seriously", "back", "rate", "exactly",
                    "obviously")

roman_words <- 1:1000 |>
  as.roman() |>
  as.character() |>
  tolower()
contractions <- c()
for (i in 1:length(common_words)) {
  if (grepl('\'', common_words[i])) {
    shorter <- substr(common_words[i], 1, nchar(common_words[i]) - 2)
    contractions <- c(contractions, shorter)
  }
}

short_words <- c(common_words, latex_words, gendered_words, foreign_words, ref_words, greek_words, contractions, ordinary_words, roman_words)

#bigram_words<-c("university", "press", "oxford", "york", "cambridge")
#short_words <- c(short_words, bigram_words)
