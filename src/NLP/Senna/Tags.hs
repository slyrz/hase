{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module NLP.Senna.Tags where

import NLP.Senna.Foreign

-- | Tags produced by Part of Speech tagging.
data POS = CC
         | CD
         | COLON
         | COMMA
         | CQM
         | DOLLAR
         | DT
         | EX
         | FW
         | HASH
         | IN
         | JJ
         | JJR
         | JJS
         | LRB
         | LS
         | MD
         | NN
         | NNP
         | NNPS
         | NNS
         | OQM
         | PDT
         | POS
         | PRP
         | PRPDOLLAR
         | RB
         | RBR
         | RBS
         | RP
         | RRB
         | STERM
         | SYM
         | TO
         | UH
         | VB
         | VBD
         | VBG
         | VBN
         | VBP
         | VBZ
         | WDT
         | WP
         | WPDOLLAR
         | WRB
         deriving (Enum,Eq,Show)

-- | Tags produced by Name Entity Recognition.
data NER = LOC
         | MISC
         | ORG
         | PER
         deriving (Enum,Eq,Show)


-- | Tags produced by Chunking.
data CHK = ADJP
         | ADVP
         | CONJP
         | INTJ
         | LST
         | NP
         | PP
         | PRT
         | SBAR
         | UCP
         | VP
         | O
         deriving (Enum,Eq,Show)

-- | Tags produced by Semantic Role Labeling.
data SRL = A0
         | A1
         | A2
         | A3
         | A4
         | A5
         | AA
         | AM
         | AM_ADV
         | AM_CAU
         | AM_DIR
         | AM_DIS
         | AM_EXT
         | AM_LOC
         | AM_MNR
         | AM_MOD
         | AM_NEG
         | AM_PNC
         | AM_PRD
         | AM_REC
         | AM_TM
         | AM_TMP
         | C_A0
         | C_A1
         | C_A2
         | C_A3
         | C_A4
         | C_A5
         | C_AM_ADV
         | C_AM_CAU
         | C_AM_DIR
         | C_AM_DIS
         | C_AM_EXT
         | C_AM_LOC
         | C_AM_MNR
         | C_AM_NEG
         | C_AM_PNC
         | C_AM_TMP
         | C_V
         | R_A0
         | R_A1
         | R_A2
         | R_A3
         | R_A4
         | R_AA
         | R_AM_ADV
         | R_AM_CAU
         | R_AM_DIR
         | R_AM_EXT
         | R_AM_LOC
         | R_AM_MNR
         | R_AM_PNC
         | R_AM_TMP
         | V
         deriving (Enum,Eq,Show)

-- | The 'Convertable' type class is used to convert foreign tags
-- defined in "NLP.Senna.Foreign.Tags" to the high-level tags defined
-- in this module.
class Convertable a b | a -> b where
  convert :: a -> b

-- All SENNA tags have some unsupported elements (PADDING,UNAVAILABLE) and/or
-- a zero element (O). To avoid creating these elements for every data type,
-- we use tags in combination with Maybe.
instance Convertable CNerTag (Maybe NER) where
  convert tag =
    case tag of
    NER_B_LOC  -> Just LOC
    NER_B_MISC -> Just MISC
    NER_B_ORG  -> Just ORG
    NER_B_PER  -> Just PER
    NER_E_LOC  -> Just LOC
    NER_E_MISC -> Just MISC
    NER_E_ORG  -> Just ORG
    NER_E_PER  -> Just PER
    NER_I_LOC  -> Just LOC
    NER_I_MISC -> Just MISC
    NER_I_ORG  -> Just ORG
    NER_I_PER  -> Just PER
    _ -> Nothing

instance Convertable CChkTag (Maybe CHK) where
  convert tag =
    case tag of
    CHK_B_ADJP  -> Just ADJP
    CHK_B_ADVP  -> Just ADVP
    CHK_B_CONJP -> Just CONJP
    CHK_B_INTJ  -> Just INTJ
    CHK_B_LST   -> Just LST
    CHK_B_NP    -> Just NP
    CHK_B_PP    -> Just PP
    CHK_B_PRT   -> Just PRT
    CHK_B_SBAR  -> Just SBAR
    CHK_B_UCP   -> Just UCP
    CHK_B_VP    -> Just VP
    CHK_E_ADJP  -> Just ADJP
    CHK_E_ADVP  -> Just ADVP
    CHK_E_CONJP -> Just CONJP
    CHK_E_INTJ  -> Just INTJ
    CHK_E_LST   -> Just LST
    CHK_E_NP    -> Just NP
    CHK_E_PP    -> Just PP
    CHK_E_PRT   -> Just PRT
    CHK_E_SBAR  -> Just SBAR
    CHK_E_UCP   -> Just UCP
    CHK_E_VP    -> Just VP
    CHK_I_ADJP  -> Just ADJP
    CHK_I_ADVP  -> Just ADVP
    CHK_I_CONJP -> Just CONJP
    CHK_I_INTJ  -> Just INTJ
    CHK_I_NP    -> Just NP
    CHK_I_PP    -> Just PP
    CHK_I_PRT   -> Just PRT
    CHK_I_UCP   -> Just UCP
    CHK_I_VP    -> Just VP
    CHK_S_ADJP  -> Just ADJP
    CHK_S_ADVP  -> Just ADVP
    CHK_S_CONJP -> Just CONJP
    CHK_S_INTJ  -> Just INTJ
    CHK_S_LST   -> Just LST
    CHK_S_NP    -> Just NP
    CHK_S_PP    -> Just PP
    CHK_S_PRT   -> Just PRT
    CHK_S_SBAR  -> Just SBAR
    CHK_S_VP    -> Just VP
    _ -> Nothing

instance Convertable CPosTag (Maybe POS) where
  convert tag =
    case tag of
    POS_CC         -> Just CC
    POS_CD         -> Just CD
    POS_COLON      -> Just COLON
    POS_COMMA      -> Just COMMA
    POS_CQM        -> Just CQM
    POS_DOLLAR     -> Just DOLLAR
    POS_DT         -> Just DT
    POS_EX         -> Just EX
    POS_FW         -> Just FW
    POS_HASH       -> Just HASH
    POS_IN         -> Just IN
    POS_JJ         -> Just JJ
    POS_JJR        -> Just JJR
    POS_JJS        -> Just JJS
    POS_LRB        -> Just LRB
    POS_LS         -> Just LS
    POS_MD         -> Just MD
    POS_NN         -> Just NN
    POS_NNP        -> Just NNP
    POS_NNPS       -> Just NNPS
    POS_NNS        -> Just NNS
    POS_OQM        -> Just OQM
    POS_PDT        -> Just PDT
    POS_POS        -> Just POS
    POS_PRP        -> Just PRP
    POS_PRP_DOLLAR -> Just PRPDOLLAR
    POS_RB         -> Just RB
    POS_RBR        -> Just RBR
    POS_RBS        -> Just RBS
    POS_RP         -> Just RP
    POS_RRB        -> Just RRB
    POS_STERM      -> Just STERM
    POS_SYM        -> Just SYM
    POS_TO         -> Just TO
    POS_UH         -> Just UH
    POS_VB         -> Just VB
    POS_VBD        -> Just VBD
    POS_VBG        -> Just VBG
    POS_VBN        -> Just VBN
    POS_VBP        -> Just VBP
    POS_VBZ        -> Just VBZ
    POS_WDT        -> Just WDT
    POS_WP         -> Just WP
    POS_WP_DOLLAR  -> Just WPDOLLAR
    POS_WRB        -> Just WRB
    _ -> Nothing

instance  Convertable CSrlTag (Maybe SRL) where
  convert tag =
    case tag of
    SRL_B_A0       -> Just A0
    SRL_B_A1       -> Just A1
    SRL_B_A2       -> Just A2
    SRL_B_A3       -> Just A3
    SRL_B_A4       -> Just A4
    SRL_B_A5       -> Just A5
    SRL_B_AA       -> Just AA
    SRL_B_AM       -> Just AM
    SRL_B_AM_ADV   -> Just AM_ADV
    SRL_B_AM_CAU   -> Just AM_CAU
    SRL_B_AM_DIR   -> Just AM_DIR
    SRL_B_AM_DIS   -> Just AM_DIS
    SRL_B_AM_EXT   -> Just AM_EXT
    SRL_B_AM_LOC   -> Just AM_LOC
    SRL_B_AM_MNR   -> Just AM_MNR
    SRL_B_AM_MOD   -> Just AM_MOD
    SRL_B_AM_NEG   -> Just AM_NEG
    SRL_B_AM_PNC   -> Just AM_PNC
    SRL_B_AM_PRD   -> Just AM_PRD
    SRL_B_AM_REC   -> Just AM_REC
    SRL_B_AM_TM    -> Just AM_TM
    SRL_B_AM_TMP   -> Just AM_TMP
    SRL_B_C_A0     -> Just C_A0
    SRL_B_C_A1     -> Just C_A1
    SRL_B_C_A2     -> Just C_A2
    SRL_B_C_A3     -> Just C_A3
    SRL_B_C_A4     -> Just C_A4
    SRL_B_C_A5     -> Just C_A5
    SRL_B_C_AM_ADV -> Just C_AM_ADV
    SRL_B_C_AM_CAU -> Just C_AM_CAU
    SRL_B_C_AM_DIR -> Just C_AM_DIR
    SRL_B_C_AM_DIS -> Just C_AM_DIS
    SRL_B_C_AM_EXT -> Just C_AM_EXT
    SRL_B_C_AM_LOC -> Just C_AM_LOC
    SRL_B_C_AM_MNR -> Just C_AM_MNR
    SRL_B_C_AM_PNC -> Just C_AM_PNC
    SRL_B_C_AM_TMP -> Just C_AM_TMP
    SRL_B_C_V      -> Just C_V
    SRL_B_R_A0     -> Just R_A0
    SRL_B_R_A1     -> Just R_A1
    SRL_B_R_A2     -> Just R_A2
    SRL_B_R_A3     -> Just R_A3
    SRL_B_R_A4     -> Just R_A4
    SRL_B_R_AM_ADV -> Just R_AM_ADV
    SRL_B_R_AM_DIR -> Just R_AM_DIR
    SRL_B_R_AM_EXT -> Just R_AM_EXT
    SRL_B_R_AM_LOC -> Just R_AM_LOC
    SRL_B_R_AM_MNR -> Just R_AM_MNR
    SRL_B_R_AM_PNC -> Just R_AM_PNC
    SRL_B_R_AM_TMP -> Just R_AM_TMP
    SRL_B_V        -> Just V
    SRL_E_A0       -> Just A0
    SRL_E_A1       -> Just A1
    SRL_E_A2       -> Just A2
    SRL_E_A3       -> Just A3
    SRL_E_A4       -> Just A4
    SRL_E_A5       -> Just A5
    SRL_E_AA       -> Just AA
    SRL_E_AM       -> Just AM
    SRL_E_AM_ADV   -> Just AM_ADV
    SRL_E_AM_CAU   -> Just AM_CAU
    SRL_E_AM_DIR   -> Just AM_DIR
    SRL_E_AM_DIS   -> Just AM_DIS
    SRL_E_AM_EXT   -> Just AM_EXT
    SRL_E_AM_LOC   -> Just AM_LOC
    SRL_E_AM_MNR   -> Just AM_MNR
    SRL_E_AM_MOD   -> Just AM_MOD
    SRL_E_AM_NEG   -> Just AM_NEG
    SRL_E_AM_PNC   -> Just AM_PNC
    SRL_E_AM_PRD   -> Just AM_PRD
    SRL_E_AM_REC   -> Just AM_REC
    SRL_E_AM_TM    -> Just AM_TM
    SRL_E_AM_TMP   -> Just AM_TMP
    SRL_E_C_A0     -> Just C_A0
    SRL_E_C_A1     -> Just C_A1
    SRL_E_C_A2     -> Just C_A2
    SRL_E_C_A3     -> Just C_A3
    SRL_E_C_A4     -> Just C_A4
    SRL_E_C_A5     -> Just C_A5
    SRL_E_C_AM_ADV -> Just C_AM_ADV
    SRL_E_C_AM_CAU -> Just C_AM_CAU
    SRL_E_C_AM_DIR -> Just C_AM_DIR
    SRL_E_C_AM_DIS -> Just C_AM_DIS
    SRL_E_C_AM_EXT -> Just C_AM_EXT
    SRL_E_C_AM_LOC -> Just C_AM_LOC
    SRL_E_C_AM_MNR -> Just C_AM_MNR
    SRL_E_C_AM_PNC -> Just C_AM_PNC
    SRL_E_C_AM_TMP -> Just C_AM_TMP
    SRL_E_C_V      -> Just C_V
    SRL_E_R_A0     -> Just R_A0
    SRL_E_R_A1     -> Just R_A1
    SRL_E_R_A2     -> Just R_A2
    SRL_E_R_A3     -> Just R_A3
    SRL_E_R_A4     -> Just R_A4
    SRL_E_R_AM_ADV -> Just R_AM_ADV
    SRL_E_R_AM_DIR -> Just R_AM_DIR
    SRL_E_R_AM_EXT -> Just R_AM_EXT
    SRL_E_R_AM_LOC -> Just R_AM_LOC
    SRL_E_R_AM_MNR -> Just R_AM_MNR
    SRL_E_R_AM_PNC -> Just R_AM_PNC
    SRL_E_R_AM_TMP -> Just R_AM_TMP
    SRL_E_V        -> Just V
    SRL_I_A0       -> Just A0
    SRL_I_A1       -> Just A1
    SRL_I_A2       -> Just A2
    SRL_I_A3       -> Just A3
    SRL_I_A4       -> Just A4
    SRL_I_A5       -> Just A5
    SRL_I_AA       -> Just AA
    SRL_I_AM       -> Just AM
    SRL_I_AM_ADV   -> Just AM_ADV
    SRL_I_AM_CAU   -> Just AM_CAU
    SRL_I_AM_DIR   -> Just AM_DIR
    SRL_I_AM_DIS   -> Just AM_DIS
    SRL_I_AM_EXT   -> Just AM_EXT
    SRL_I_AM_LOC   -> Just AM_LOC
    SRL_I_AM_MNR   -> Just AM_MNR
    SRL_I_AM_MOD   -> Just AM_MOD
    SRL_I_AM_NEG   -> Just AM_NEG
    SRL_I_AM_PNC   -> Just AM_PNC
    SRL_I_AM_PRD   -> Just AM_PRD
    SRL_I_AM_TM    -> Just AM_TM
    SRL_I_AM_TMP   -> Just AM_TMP
    SRL_I_C_A0     -> Just C_A0
    SRL_I_C_A1     -> Just C_A1
    SRL_I_C_A2     -> Just C_A2
    SRL_I_C_A3     -> Just C_A3
    SRL_I_C_A4     -> Just C_A4
    SRL_I_C_A5     -> Just C_A5
    SRL_I_C_AM_ADV -> Just C_AM_ADV
    SRL_I_C_AM_CAU -> Just C_AM_CAU
    SRL_I_C_AM_DIS -> Just C_AM_DIS
    SRL_I_C_AM_EXT -> Just C_AM_EXT
    SRL_I_C_AM_LOC -> Just C_AM_LOC
    SRL_I_C_AM_MNR -> Just C_AM_MNR
    SRL_I_C_AM_PNC -> Just C_AM_PNC
    SRL_I_C_AM_TMP -> Just C_AM_TMP
    SRL_I_C_V      -> Just C_V
    SRL_I_R_A0     -> Just R_A0
    SRL_I_R_A1     -> Just R_A1
    SRL_I_R_A2     -> Just R_A2
    SRL_I_R_AM_ADV -> Just R_AM_ADV
    SRL_I_R_AM_MNR -> Just R_AM_MNR
    SRL_I_R_AM_TMP -> Just R_AM_TMP
    SRL_I_V        -> Just V
    SRL_S_A0       -> Just A0
    SRL_S_A1       -> Just A1
    SRL_S_A2       -> Just A2
    SRL_S_A3       -> Just A3
    SRL_S_A4       -> Just A4
    SRL_S_A5       -> Just A5
    SRL_S_AA       -> Just AA
    SRL_S_AM_ADV   -> Just AM_ADV
    SRL_S_AM_CAU   -> Just AM_CAU
    SRL_S_AM_DIR   -> Just AM_DIR
    SRL_S_AM_DIS   -> Just AM_DIS
    SRL_S_AM_EXT   -> Just AM_EXT
    SRL_S_AM_LOC   -> Just AM_LOC
    SRL_S_AM_MNR   -> Just AM_MNR
    SRL_S_AM_MOD   -> Just AM_MOD
    SRL_S_AM_NEG   -> Just AM_NEG
    SRL_S_AM_PNC   -> Just AM_PNC
    SRL_S_AM_PRD   -> Just AM_PRD
    SRL_S_AM_REC   -> Just AM_REC
    SRL_S_AM_TMP   -> Just AM_TMP
    SRL_S_C_A0     -> Just C_A0
    SRL_S_C_A1     -> Just C_A1
    SRL_S_C_A2     -> Just C_A2
    SRL_S_C_AM_DIS -> Just C_AM_DIS
    SRL_S_C_AM_EXT -> Just C_AM_EXT
    SRL_S_C_AM_NEG -> Just C_AM_NEG
    SRL_S_C_V      -> Just C_V
    SRL_S_R_A0     -> Just R_A0
    SRL_S_R_A1     -> Just R_A1
    SRL_S_R_A2     -> Just R_A2
    SRL_S_R_A3     -> Just R_A3
    SRL_S_R_A4     -> Just R_A4
    SRL_S_R_AA     -> Just R_AA
    SRL_S_R_AM_CAU -> Just R_AM_CAU
    SRL_S_R_AM_EXT -> Just R_AM_EXT
    SRL_S_R_AM_LOC -> Just R_AM_LOC
    SRL_S_R_AM_MNR -> Just R_AM_MNR
    SRL_S_R_AM_PNC -> Just R_AM_PNC
    SRL_S_R_AM_TMP -> Just R_AM_TMP
    SRL_S_V        -> Just V
    _ -> Nothing
