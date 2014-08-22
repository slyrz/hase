#ifndef SENNA_NER_H
#define SENNA_NER_H

typedef enum {
  NER_O,
  NER_S_LOC,
  NER_E_PER,
  NER_B_PER,
  NER_S_ORG,
  NER_E_ORG,
  NER_B_ORG,
  NER_S_PER,
  NER_S_MISC,
  NER_I_ORG,
  NER_E_LOC,
  NER_B_LOC,
  NER_E_MISC,
  NER_B_MISC,
  NER_I_PER,
  NER_I_MISC,
  NER_I_LOC,
  NER_PADDING,
  NER_UNAVAILABLE,
} senna_ner_tag_t;

#endif
