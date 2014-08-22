{-# LANGUAGE ForeignFunctionInterface #-}
module NLP.Senna.Foreign.Tags where

#include "senna-chk.h"
#include "senna-ner.h"
#include "senna-pos.h"
#include "senna-srl.h"

{#enum senna_chk_tag_t as CChkTag {} deriving (Eq, Show)#}
{#enum senna_ner_tag_t as CNerTag {} deriving (Eq, Show)#}
{#enum senna_pos_tag_t as CPosTag {} deriving (Eq, Show)#}
{#enum senna_srl_tag_t as CSrlTag {} deriving (Eq, Show)#}

-- Senna uses IOBES tagging scheme. IOBES stands for
--   I inside
--   O other
--   B beginning
--   E end
--   S single
-- We use the Spanning type class to get the corresponding End tag for Beginning
-- and Inside tags. For all other tags (including End tags), this function
-- returns Nothing.
class Spanning a where
  end :: a -> Maybe a

instance Spanning CNerTag where
  end tag =
    case tag of
    NER_B_LOC  -> Just NER_E_LOC
    NER_B_MISC -> Just NER_E_MISC
    NER_B_ORG  -> Just NER_E_ORG
    NER_B_PER  -> Just NER_E_PER
    NER_I_LOC  -> Just NER_E_LOC
    NER_I_MISC -> Just NER_E_MISC
    NER_I_ORG  -> Just NER_E_ORG
    NER_I_PER  -> Just NER_E_PER
    _ -> Nothing

instance Spanning CChkTag where
  end tag =
    case tag of
    CHK_B_ADJP  -> Just CHK_E_ADJP
    CHK_B_ADVP  -> Just CHK_E_ADVP
    CHK_B_CONJP -> Just CHK_E_CONJP
    CHK_B_INTJ  -> Just CHK_E_INTJ
    CHK_B_LST   -> Just CHK_E_LST
    CHK_B_NP    -> Just CHK_E_NP
    CHK_B_PP    -> Just CHK_E_PP
    CHK_B_PRT   -> Just CHK_E_PRT
    CHK_B_SBAR  -> Just CHK_E_SBAR
    CHK_B_UCP   -> Just CHK_E_UCP
    CHK_B_VP    -> Just CHK_E_VP
    CHK_I_ADJP  -> Just CHK_E_ADJP
    CHK_I_ADVP  -> Just CHK_E_ADVP
    CHK_I_CONJP -> Just CHK_E_CONJP
    CHK_I_INTJ  -> Just CHK_E_INTJ
    CHK_I_NP    -> Just CHK_E_NP
    CHK_I_PP    -> Just CHK_E_PP
    CHK_I_PRT   -> Just CHK_E_PRT
    CHK_I_UCP   -> Just CHK_E_UCP
    CHK_I_VP    -> Just CHK_E_VP
    _ -> Nothing

instance Spanning CSrlTag where
  end tag =
    case tag of
    SRL_B_A0       -> Just SRL_E_A0
    SRL_B_A1       -> Just SRL_E_A1
    SRL_B_A2       -> Just SRL_E_A2
    SRL_B_A3       -> Just SRL_E_A3
    SRL_B_A4       -> Just SRL_E_A4
    SRL_B_A5       -> Just SRL_E_A5
    SRL_B_AA       -> Just SRL_E_AA
    SRL_B_AM       -> Just SRL_E_AM
    SRL_B_AM_ADV   -> Just SRL_E_AM_ADV
    SRL_B_AM_CAU   -> Just SRL_E_AM_CAU
    SRL_B_AM_DIR   -> Just SRL_E_AM_DIR
    SRL_B_AM_DIS   -> Just SRL_E_AM_DIS
    SRL_B_AM_EXT   -> Just SRL_E_AM_EXT
    SRL_B_AM_LOC   -> Just SRL_E_AM_LOC
    SRL_B_AM_MNR   -> Just SRL_E_AM_MNR
    SRL_B_AM_MOD   -> Just SRL_E_AM_MOD
    SRL_B_AM_NEG   -> Just SRL_E_AM_NEG
    SRL_B_AM_PNC   -> Just SRL_E_AM_PNC
    SRL_B_AM_PRD   -> Just SRL_E_AM_PRD
    SRL_B_AM_REC   -> Just SRL_E_AM_REC
    SRL_B_AM_TM    -> Just SRL_E_AM_TM
    SRL_B_AM_TMP   -> Just SRL_E_AM_TMP
    SRL_B_C_A0     -> Just SRL_E_C_A0
    SRL_B_C_A1     -> Just SRL_E_C_A1
    SRL_B_C_A2     -> Just SRL_E_C_A2
    SRL_B_C_A3     -> Just SRL_E_C_A3
    SRL_B_C_A4     -> Just SRL_E_C_A4
    SRL_B_C_A5     -> Just SRL_E_C_A5
    SRL_B_C_AM_ADV -> Just SRL_E_C_AM_ADV
    SRL_B_C_AM_CAU -> Just SRL_E_C_AM_CAU
    SRL_B_C_AM_DIR -> Just SRL_E_C_AM_DIR
    SRL_B_C_AM_DIS -> Just SRL_E_C_AM_DIS
    SRL_B_C_AM_EXT -> Just SRL_E_C_AM_EXT
    SRL_B_C_AM_LOC -> Just SRL_E_C_AM_LOC
    SRL_B_C_AM_MNR -> Just SRL_E_C_AM_MNR
    SRL_B_C_AM_PNC -> Just SRL_E_C_AM_PNC
    SRL_B_C_AM_TMP -> Just SRL_E_C_AM_TMP
    SRL_B_C_V      -> Just SRL_E_C_V
    SRL_B_R_A0     -> Just SRL_E_R_A0
    SRL_B_R_A1     -> Just SRL_E_R_A1
    SRL_B_R_A2     -> Just SRL_E_R_A2
    SRL_B_R_A3     -> Just SRL_E_R_A3
    SRL_B_R_A4     -> Just SRL_E_R_A4
    SRL_B_R_AM_ADV -> Just SRL_E_R_AM_ADV
    SRL_B_R_AM_DIR -> Just SRL_E_R_AM_DIR
    SRL_B_R_AM_EXT -> Just SRL_E_R_AM_EXT
    SRL_B_R_AM_LOC -> Just SRL_E_R_AM_LOC
    SRL_B_R_AM_MNR -> Just SRL_E_R_AM_MNR
    SRL_B_R_AM_PNC -> Just SRL_E_R_AM_PNC
    SRL_B_R_AM_TMP -> Just SRL_E_R_AM_TMP
    SRL_B_V        -> Just SRL_E_V
    SRL_I_A0       -> Just SRL_E_A0
    SRL_I_A1       -> Just SRL_E_A1
    SRL_I_A2       -> Just SRL_E_A2
    SRL_I_A3       -> Just SRL_E_A3
    SRL_I_A4       -> Just SRL_E_A4
    SRL_I_A5       -> Just SRL_E_A5
    SRL_I_AA       -> Just SRL_E_AA
    SRL_I_AM       -> Just SRL_E_AM
    SRL_I_AM_ADV   -> Just SRL_E_AM_ADV
    SRL_I_AM_CAU   -> Just SRL_E_AM_CAU
    SRL_I_AM_DIR   -> Just SRL_E_AM_DIR
    SRL_I_AM_DIS   -> Just SRL_E_AM_DIS
    SRL_I_AM_EXT   -> Just SRL_E_AM_EXT
    SRL_I_AM_LOC   -> Just SRL_E_AM_LOC
    SRL_I_AM_MNR   -> Just SRL_E_AM_MNR
    SRL_I_AM_MOD   -> Just SRL_E_AM_MOD
    SRL_I_AM_NEG   -> Just SRL_E_AM_NEG
    SRL_I_AM_PNC   -> Just SRL_E_AM_PNC
    SRL_I_AM_PRD   -> Just SRL_E_AM_PRD
    SRL_I_AM_TM    -> Just SRL_E_AM_TM
    SRL_I_AM_TMP   -> Just SRL_E_AM_TMP
    SRL_I_C_A0     -> Just SRL_E_C_A0
    SRL_I_C_A1     -> Just SRL_E_C_A1
    SRL_I_C_A2     -> Just SRL_E_C_A2
    SRL_I_C_A3     -> Just SRL_E_C_A3
    SRL_I_C_A4     -> Just SRL_E_C_A4
    SRL_I_C_A5     -> Just SRL_E_C_A5
    SRL_I_C_AM_ADV -> Just SRL_E_C_AM_ADV
    SRL_I_C_AM_CAU -> Just SRL_E_C_AM_CAU
    SRL_I_C_AM_DIS -> Just SRL_E_C_AM_DIS
    SRL_I_C_AM_EXT -> Just SRL_E_C_AM_EXT
    SRL_I_C_AM_LOC -> Just SRL_E_C_AM_LOC
    SRL_I_C_AM_MNR -> Just SRL_E_C_AM_MNR
    SRL_I_C_AM_PNC -> Just SRL_E_C_AM_PNC
    SRL_I_C_AM_TMP -> Just SRL_E_C_AM_TMP
    SRL_I_C_V      -> Just SRL_E_C_V
    SRL_I_R_A0     -> Just SRL_E_R_A0
    SRL_I_R_A1     -> Just SRL_E_R_A1
    SRL_I_R_A2     -> Just SRL_E_R_A2
    SRL_I_R_AM_ADV -> Just SRL_E_R_AM_ADV
    SRL_I_R_AM_MNR -> Just SRL_E_R_AM_MNR
    SRL_I_R_AM_TMP -> Just SRL_E_R_AM_TMP
    SRL_I_V        -> Just SRL_E_V
    _ -> Nothing
