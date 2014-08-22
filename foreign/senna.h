#ifndef SENNA_H
#define SENNA_H

#include "senna/SENNA_utils.h"
#include "senna/SENNA_Hash.h"
#include "senna/SENNA_Tokenizer.h"
#include "senna/SENNA_POS.h"
#include "senna/SENNA_CHK.h"
#include "senna/SENNA_NER.h"
#include "senna/SENNA_VBS.h"
#include "senna/SENNA_PT0.h"
#include "senna/SENNA_SRL.h"
#include "senna/SENNA_PSG.h"

#include "senna-pos.h"
#include "senna-chk.h"
#include "senna-ner.h"

typedef struct {
  SENNA_Hash *caps_hash;
  SENNA_Hash *suff_hash;
  SENNA_Hash *word_hash;
  SENNA_Hash *gazl_hash;
  SENNA_Hash *gazm_hash;
  SENNA_Hash *gazo_hash;
  SENNA_Hash *gazp_hash;
  SENNA_Hash *gazt_hash;

  SENNA_CHK *chk;
  SENNA_NER *ner;
  SENNA_POS *pos;
  SENNA_PSG *psg;
  SENNA_PT0 *pt0;
  SENNA_SRL *srl;
  SENNA_VBS *vbs;

  SENNA_Tokenizer *tokenizer;
  SENNA_Tokens *tokens;

  int *pos_labels;
  int *chk_labels;
  int *pt0_labels;
  int *ner_labels;
  int *vbs_labels;
  int **srl_labels;

  int verbs;
  int error;
} senna_t;

/**
 * Initalize context and load all data from path.
 */
senna_t* senna_new (const char *path);

/**
 * Free context.
 */
void senna_free (senna_t *ctx);

/**
 * Tokenize a sentence. This method must be called before calling the
 * functions:
 *   - senna_get_length
 *   - senna_get_verbs
 *   - senna_get_words
 *   - senna_get_pos
 *   - senna_get_chk
 *   - senna_get_ner
 *   - senna_get_srl
 */
void senna_tokenize (senna_t *ctx, const char *sentence);

/**
 * Returns a value != 0 if an error was encountered.
 */
int senna_get_error (senna_t *ctx);

/**
 * Returns the number of tokens.
 */
int senna_get_length (senna_t *ctx);

/**
 * Returns the number of verbs in the tokenized sentence.
 */
int senna_get_verbs (senna_t *ctx);

/**
 * Returns the tokens.
 */
char** senna_get_words (senna_t *ctx);

/**
 * Perform a task:
 * 	pos - Part of Speech tagging
 * 	chk - Chunking
 * 	ner - Name Entity Recognition
 * 	srl - Semantic Role Labeling
 */
int*  senna_get_pos (senna_t *ctx);
int*  senna_get_chk (senna_t *ctx);
int*  senna_get_ner (senna_t *ctx);
int** senna_get_srl (senna_t *ctx);

#endif
