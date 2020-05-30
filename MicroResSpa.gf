resource MicroResSpa = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Person = P1 | P2 | P3 ;
  Gender = Masc | Fem ;
  -- Agreement = Agr Gender Number Person ;    --remove agreement
  VForm = VInf | VPres Number Person ; 


oper
  
--agreement function:
 -- agrFeatures : Agreement -> {g : Gender; n : Number; p : Person}  = \a -> case a of {
 --   Agr g n p => {g = g; n = n; p = p}
 -- };

-- NOUN ------------------------------
  --Noun : Type = {s : Number => Str; g : Gender} ; -- TESTING
  Noun : Type = {s : Number => Str; g : Gender } ;
  --NounPhrase : Type = {s : Case => Str ; g : Gender ; n : Number } ;
  --Pronoun : Type = {s : Case => Str ; g : Gender ; n : Number } ;
  --Adjective : Type = {s : Gender => Number => Str } ;
  mkNoun : (sg, pl : Str) -> Gender -> Noun = \sg,pl,g -> {
    s = table {Sg => sg ; Pl => pl};
    g = g
    } ;
  
  vowels : pattern Str  = # ("a" | "e" | "i" | "o" | "u");
  vowels_tilde : pattern Str  = # ("á" | "é" | "í" | "ó" | "ú");
  
  regNoun : Str -> Gender -> Noun = \sg,g -> case sg of {
    amig + #vowels => mkNoun sg (sg + "s") g;
    _ => mkNoun sg (sg + "es") g
    };

  -- smart paradigm
  smartNoun : Str -> Gender-> Noun = \sg,g -> case sg of {
    canci + ("ón") => mkNoun sg (canci + "ones") g;
    lu + "z" => mkNoun sg (lu + "ces") g;
    beb + #vowels_tilde => mkNoun sg (beb + "es") g;
    _                         => regNoun sg g
    } ;

-- ADJECTIVE ------------------------------
  
  mkAdj : Str -> Str -> Str -> Str -> Gender => Number => Str  = \malo,mala,malos,malas -> table {
    Masc => table {
      Sg => malo;
      Pl => malos
      };
    Fem => table {
      Sg => mala;
      Pl => malas
      }
    };

  regAdj : Str -> Gender => Number => Str  = \malo -> case malo of {
    mal + "o" => mkAdj malo (mal + "a") (mal + "os") (mal + "as");
    verd + "e" => mkAdj malo malo (verd + "es") (verd + "es");
    azu + "l" => mkAdj malo malo (malo + "es") (malo + "es")
    };

-- VERB ------------------------------
  Verb : Type = {s : VForm => Str} ;

 mkVerb : (comprar,compro,compras,compra,compramos,comprais,compran : Str) -> Verb 
    = \comprar,compro,compras,compra,compramos,comprais,compran -> {
    s = table {
      VInf        => comprar ;
      VPres Sg P1 => compro ;
      VPres Sg P2 => compras ;
      VPres Sg P3 => compra ;
      VPres Pl P1 => compramos ;
      VPres Pl P2 => comprais ;
      VPres Pl P3 => compran
      }
    } ;
  

  -- regular verbs with predictable variations
  regVerb : Str -> Verb = \inf -> case inf of {
    habl  +  "ar" => mkVerb inf (habl + "o") (habl + "as") (habl + "a") (habl + "amos") (habl + "áis") (habl + "an");
    com  +  "er" => mkVerb inf (com + "o") (com + "es") (com + "e") (com + "emos") (com + "eis") (com + "en");
    viv  +  "ir" => mkVerb inf (viv + "o") (viv + "es") (viv + "e") (viv + "imos") (viv + "is") (viv + "en")
    } ;

-- normal irregular verbs e.g. drink,drank,drunk
--  irregVerb : (inf,past,pastpart : Str) -> Verb =
--    \inf,past,pastpart ->
--      let verb = smartVerb inf
--      in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = mkVerb "ser" "soy" "eres" "es" "somos" "sois" "son" ; ---s to be generalized

-- AGREEMENT ------------------------------
  --agr2vform : Verb -> Agr -> Str = \g,n,p,a -> case a of {
  --  Agr _ n p => case a of {
  --    Agr =>  VInf ;
  --    Agr n p => VPres n p}
  --  };

}