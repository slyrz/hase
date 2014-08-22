#ifndef SENNA_POS_H
#define SENNA_POS_H

typedef enum {
  POS_NNP,
  POS_COMMA,
  POS_CD,
  POS_NNS,
  POS_JJ,
  POS_MD,
  POS_VB,
  POS_DT,
  POS_NN,
  POS_IN,
  POS_STERM,                    /* sentence terminator: (.) */
  POS_VBZ,
  POS_VBG,
  POS_CC,
  POS_VBD,
  POS_VBN,
  POS_RB,
  POS_TO,
  POS_PRP,
  POS_RBR,
  POS_WDT,
  POS_VBP,
  POS_RP,
  POS_PRP_DOLLAR,
  POS_JJS,
  POS_POS,
  POS_OQM,                      /* opening quotation mark (``) */
  POS_WP,
  POS_CQM,                      /* closing quotation mark ('') */
  POS_COLON,                    /* colon (:) */
  POS_JJR,
  POS_WRB,
  POS_EX,
  POS_DOLLAR,                   /* dollar ($) */
  POS_NNPS,
  POS_WP_DOLLAR,
  POS_LRB,                      /* left round bracket (-LRB-) */
  POS_RRB,                      /* right round bracket (-RRB-) */
  POS_PDT,
  POS_RBS,
  POS_FW,
  POS_UH,
  POS_SYM,
  POS_LS,
  POS_HASH,                     /* hash (#) */
  POS_PADDING,
  POS_UNAVAILABLE,
} senna_pos_tag_t;

#endif
