--# -path=.:../abstract
concrete MicroLangSpa of MicroLang = open MicroResSpa, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Gender => Number => Str} ; ---compl gender and number
    AP = {s : Gender => Number => Str};
    --Comp = {s : Str} ;
    CN = {s : Number => Str; g : Gender};
    NP = {s : Case => Str ; g : Gender ; n : Number} ;
    Pron = {s : Case => Str ; g : Gender ; n : Number} ;
    Det = {s : Gender => Str ; n : Number} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    A = {s : Gender => Number => Str};
    N = Noun ;
    Adv = {s : Str} ;
    Comp = AP ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;

    PredVPS np vp = { s = np.s ! Nom ++ vp.verb.s ! np.n ! P3 ++ vp.compl ! np.g ! np.n} ;
      
    UseV v = {
      verb = v ;
      compl = \\_,_ => [] ;
      } ;
      
   -- ComplV2 v2 np = {
   --   verb = v2 ;
   --   compl = \\_,_ => v2.c.s ++ np.s ! Acc  -- NP object in the accusative, preposition first
   --   } ;
    ComplV2 v2 np = {
      verb = v2 ;
      compl = \\_,_ => v2.c ++ np.s ! Acc  -- NP object in the accusative, preposition first
      } ;
      
    UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = \\_,_ => comp.s
      } ;
    
    CompAP ap = \\_,_ => ap ;
    --CompAP ap = ap ;
      
    AdvVP vp adv = 
      vp ** {compl = \\g,n => vp.compl ! g ! n ++ adv.s} ; --vp.compl also has g and n
       -- vp ** {compl = vp.compl ++ adv.s} ; --vp.compl also has g and n
  
    DetCN det cn = {
      s = \\c => det.s ! cn.g ++ cn.s ! det.n ;
      n = det.n ;
      } ;
    
    UsePron p = p ;
            
    a_Det = {
     s = table {Masc => "un"; Fem => "una"};
     n = Pl;
     } ; 
    aPl_Det = {
     s = table {Masc => "unos"; Fem => "unas"}; 
     n = Pl;
     } ;
    the_Det = {
     s = table {Masc => "el"; Fem => "la"}; 
     n = Sg;
     } ;
    thePl_Det = {
     s = table {Masc => "los"; Fem => "las"}; 
     n = Pl;
     } ;
    
    UseN n = n ;
    

    AdjCN ap cn = {s = \\n => cn.s ! n ++ ap.s ! cn.g ! n; g = cn.g};

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    in_Prep = {s = "en"} ;
    on_Prep = {s = "on"} ;
    with_Prep = {s = "con"} ;

    he_Pron = {
      s = table {Nom => "él" ; Acc => "lo"} ;
      g = Masc ; 
      n = Sg ;
      } ;
    she_Pron = {
      s = table {Nom => "ella" ; Acc => "la"} ;
      g = Fem ;
      n = Sg ;
      } ;
    they_Pron = {
      s = table {Nom => "ellos" ; Acc => "los"} ;
      g = Masc ;
      n = Pl ;
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "ya" ;
lin animal_N = mkN "animal" Masc ;
lin apple_N = mkN "manzana" Fem;
lin baby_N = mkN "bebé" Masc;
lin bad_A = mkA "malo" ;
lin beer_N = mkN "cerveza" Fem;
lin big_A = mkA "grande" ;
lin bike_N = mkN "bicicleta" Fem;
lin bird_N = mkN "pájaro" Masc;
lin black_A = mkA "negro" ;
lin blood_N = mkN "sangre" Fem;
lin blue_A = mkA "azul" ;
lin boat_N = mkN "barco" Masc;
lin book_N = mkN "libro" Masc;
lin boy_N = mkN "chico" Masc;
lin bread_N = mkN "pan" Masc;
lin break_V2 = mkV "romper" ;
lin buy_V2 = mkV "comprar" ;
lin car_N = mkN "coche" Masc;
lin cat_N = mkN "gato" Masc;
lin child_N = mkN "niño" Masc;
lin city_N = mkN "ciudad" Masc;
lin clean_A = mkA "limpio" ;
lin clever_A = mkA "inteligente" ;
lin cloud_N = mkN "nube" Fem;
lin cold_A = mkA "frio";
lin come_V = mkV "come";
lin computer_N = mkN "ordenador" Masc;
lin cow_N = mkN "vaca" Fem;
lin dirty_A = mkA "sucio" ;
lin dog_N = mkN "perro" Masc;
lin drink_V2 = mkV "beber" ;
lin eat_V2 = mkV "comer" ;
lin find_V2 = mkV "buscar" ;
lin fire_N = mkN "fuego" Masc;
lin fish_N = mkN "pez" Masc;
lin flower_N = mkN "flor" Fem;
lin friend_N = mkN "amigo" Masc;
lin girl_N = mkN "chica" Fem;
lin good_A = mkA "bueno" ;
lin go_V = mkV "ir" "voy" "vas" "va" "vamos" "vais" "van" ;
lin grammar_N = mkN "gramática" ;
lin green_A = mkA "verde" ;
lin heavy_A = mkA "pesado" ;
lin horse_N = mkN "caballo" Masc;
lin hot_A = mkA "caliente" ;
lin house_N = mkN "casa" Fem;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "saltar" ;
lin kill_V2 = mkV2 "matar" ;
-- lin know_VS = mkVS (mkV "know" "knew" "known") ;
lin language_N = mkN "idioma" Masc;
lin live_V = mkV "vivir" ;
lin love_V2 = mkV "querer" ;
lin man_N = mkN "hombre" Masc;
lin milk_N = mkN "leche" Fem;
lin music_N = mkN "musica" Fem;
lin new_A = mkA "nuevo" ;
lin now_Adv = mkAdv "ahora" ;
lin old_A = mkA "viejo" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "jugar" ;
lin read_V2 = mkV "leer" ;
lin ready_A = mkA "preparado" ;
lin red_A = mkA "rojo" ;
lin river_N = mkN "río" Masc;
lin run_V = mkV "correr" ;
lin sea_N = mkN "mar" Masc;
lin see_V2 = mkV "ver" ;
lin ship_N = mkN "barco" Masc;
lin sleep_V = mkV "dormir" ;
lin small_A = mkA "pequeño" ;
lin star_N = mkN "estrella" Fem;
lin swim_V = mkV "nadar" ;
lin teach_V2 = mkV "esnseñar" ;
lin train_N = mkN "tren" Masc;
lin travel_V = mkV "viajar" ;
lin tree_N = mkN "árbol" Masc;
lin understand_V2 = mkV "entender" ;
lin wait_V2 = mkV "esperar";
lin walk_V = mkV "pasear" ;
lin warm_A = mkA "tibio" ;
lin water_N = mkN "agua" Masc;
lin white_A = mkA "blanco" ;
lin wine_N = mkN "vino" Masc;
lin woman_N = mkN "mujer" Fem;
lin yellow_A = mkA "amarillo" ;
lin young_A = mkA "joven" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN : Str -> Gender -> Noun   -- predictable noun, 
      = \n,g -> lin N (smartNoun n g) ;


  mkA : Str -> A
    = \s -> lin A {s = s} ; --i have mkA and regA, so this need to be changed?


  mkV = overload {
    mkV  : Str -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (regVerb s) ;
    mkV : (comprar,compro,compras,compra,compramos,comprais,compran : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \comprar,compro,compras,compra,compramos,comprais,compran -> lin V (mkVerb comprar compro compras compra compramos comprais compran) ;
    } ;


  mkV2 = overload {
  mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (regVerb s ** {c = []}) ;
   mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (regVerb s ** {c = p}) ;
   mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
     = \v   -> lin V2 (v ** {c = []}) ;
   mkV2 : V -> Str -> V2     -- any verb with preposition
     = \v,p -> lin V2 (v ** {c = p}) ;
   } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}