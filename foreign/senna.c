#include "senna.h"

#include <stdlib.h>

/**
 * Macro used to indicate error and abort function if senna_tokenize wasn't
 * called yet. The variadic argument is used as return value in non-void
 * functions.
 */
#define senna_require_tokens(ctx,...) \
  do { \
    if ((ctx)->tokens == NULL) { \
      (ctx)->error = 1; \
      return __VA_ARGS__; \
    } \
  } while (0)

/**
 * Macro used to make sure labels were generated for a given type.
 */
#define senna_require_labels(ctx,tag) \
  do { \
    if ((ctx)->tag ## _labels == NULL) { \
      senna_generate_ ## tag (ctx); \
    } \
  } while (0)

static void senna_generate_pos (senna_t * ctx);
static void senna_generate_chk (senna_t * ctx);
static void senna_generate_ner (senna_t * ctx);
static void senna_generate_pt0 (senna_t * ctx);
static void senna_generate_vbs (senna_t * ctx);
static void senna_generate_srl (senna_t * ctx);

senna_t *
senna_new (const char *path)
{
  senna_t *result;

  result = calloc (1, sizeof (senna_t));
  if (result == NULL)
    return NULL;

  result->chk = SENNA_CHK_new (path, "data/chk.dat");
  result->ner = SENNA_NER_new (path, "data/ner.dat");
  result->pos = SENNA_POS_new (path, "data/pos.dat");
  result->psg = SENNA_PSG_new (path, "data/psg.dat");
  result->pt0 = SENNA_PT0_new (path, "data/pt0.dat");
  result->srl = SENNA_SRL_new (path, "data/srl.dat");
  result->vbs = SENNA_VBS_new (path, "data/vbs.dat");

  result->caps_hash = SENNA_Hash_new (path, "hash/caps.lst");
  result->gazt_hash = SENNA_Hash_new (path, "hash/gazetteer.lst");
  result->suff_hash = SENNA_Hash_new (path, "hash/suffix.lst");
  result->word_hash = SENNA_Hash_new (path, "hash/words.lst");

  result->gazl_hash =
    SENNA_Hash_new_with_admissible_keys (path,
      "hash/ner.loc.lst",
      "data/ner.loc.dat");
  result->gazm_hash =
    SENNA_Hash_new_with_admissible_keys (path,
      "hash/ner.msc.lst",
      "data/ner.msc.dat");
  result->gazo_hash =
    SENNA_Hash_new_with_admissible_keys (path,
      "hash/ner.org.lst",
      "data/ner.org.dat");
  result->gazp_hash =
    SENNA_Hash_new_with_admissible_keys (path,
      "hash/ner.per.lst",
      "data/ner.per.dat");

  result->tokenizer =
    SENNA_Tokenizer_new (result->word_hash, result->caps_hash,
      result->suff_hash, result->gazt_hash, result->gazl_hash,
      result->gazm_hash, result->gazo_hash, result->gazp_hash, 0);

  return result;
}

void
senna_free (senna_t * ctx)
{
  if (ctx == NULL)
    return;

  SENNA_Tokenizer_free (ctx->tokenizer);

  SENNA_Hash_free (ctx->caps_hash);
  SENNA_Hash_free (ctx->gazt_hash);
  SENNA_Hash_free (ctx->suff_hash);
  SENNA_Hash_free (ctx->word_hash);
  SENNA_Hash_free (ctx->gazl_hash);
  SENNA_Hash_free (ctx->gazm_hash);
  SENNA_Hash_free (ctx->gazo_hash);
  SENNA_Hash_free (ctx->gazp_hash);

  SENNA_POS_free (ctx->pos);
  SENNA_CHK_free (ctx->chk);
  SENNA_PT0_free (ctx->pt0);
  SENNA_NER_free (ctx->ner);
  SENNA_VBS_free (ctx->vbs);
  SENNA_SRL_free (ctx->srl);
  SENNA_PSG_free (ctx->psg);

  free (ctx);
}

void
senna_tokenize (senna_t * ctx, const char *sentence)
{
  /**
   * All cached results become invalid. Setting pointers to NULL doesn't leak
   * memory because SENNA manages these pointers.
   */
  ctx->error = 0;
  ctx->verbs = 0;
  ctx->pos_labels = NULL;
  ctx->chk_labels = NULL;
  ctx->pt0_labels = NULL;
  ctx->ner_labels = NULL;
  ctx->vbs_labels = NULL;
  ctx->srl_labels = NULL;

  ctx->tokens = SENNA_Tokenizer_tokenize (ctx->tokenizer, sentence);
}

int
senna_get_error (senna_t * ctx)
{
  return ctx->error;
}

int
senna_get_length (senna_t * ctx)
{
  senna_require_tokens (ctx, 0);
  return ctx->tokens->n;
}

int
senna_get_verbs (senna_t * ctx)
{
  senna_require_tokens (ctx, 0);
  senna_require_labels (ctx, vbs);
  return ctx->verbs;
}

char **
senna_get_words (senna_t * ctx)
{
  senna_require_tokens (ctx, NULL);
  return ctx->tokens->words;
}

static void
senna_generate_pos (senna_t * ctx)
{
  ctx->pos_labels =
    SENNA_POS_forward (ctx->pos, ctx->tokens->word_idx, ctx->tokens->caps_idx,
      ctx->tokens->suff_idx, ctx->tokens->n);
}

static void
senna_generate_chk (senna_t * ctx)
{
  senna_require_labels (ctx, pos);
  ctx->chk_labels =
    SENNA_CHK_forward (ctx->chk, ctx->tokens->word_idx, ctx->tokens->caps_idx,
      ctx->pos_labels, ctx->tokens->n);
}

static void
senna_generate_ner (senna_t * ctx)
{
  ctx->ner_labels =
    SENNA_NER_forward (ctx->ner, ctx->tokens->word_idx, ctx->tokens->caps_idx,
      ctx->tokens->gazl_idx, ctx->tokens->gazm_idx, ctx->tokens->gazo_idx,
      ctx->tokens->gazp_idx, ctx->tokens->n);
}

static void
senna_generate_pt0 (senna_t * ctx)
{
  senna_require_labels (ctx, pos);
  ctx->pt0_labels =
    SENNA_PT0_forward (ctx->pt0, ctx->tokens->word_idx, ctx->tokens->caps_idx,
      ctx->pos_labels, ctx->tokens->n);
}

static void
senna_generate_vbs (senna_t * ctx)
{
  senna_require_labels (ctx, pos);
  ctx->vbs_labels =
    SENNA_VBS_forward (ctx->vbs, ctx->tokens->word_idx, ctx->tokens->caps_idx,
      ctx->pos_labels, ctx->tokens->n);

  int i;
  for (i = 0, ctx->verbs = 0; i < ctx->tokens->n; i++) {
    ctx->vbs_labels[i] = (ctx->vbs_labels[i] != 22);
    ctx->verbs += ctx->vbs_labels[i];
  }
}

static void
senna_generate_srl (senna_t * ctx)
{
  senna_require_labels (ctx, pt0);
  senna_require_labels (ctx, vbs);
  ctx->srl_labels =
    SENNA_SRL_forward (ctx->srl, ctx->tokens->word_idx, ctx->tokens->caps_idx,
      ctx->pt0_labels, ctx->vbs_labels, ctx->tokens->n);
}

int *
senna_get_pos (senna_t * ctx)
{
  senna_require_tokens (ctx, NULL);
  senna_require_labels (ctx, pos);
  return ctx->pos_labels;
}

int *
senna_get_chk (senna_t * ctx)
{
  senna_require_tokens (ctx, NULL);
  senna_require_labels (ctx, chk);
  return ctx->chk_labels;
}

int *
senna_get_ner (senna_t * ctx)
{
  senna_require_tokens (ctx, NULL);
  senna_require_labels (ctx, ner);
  return ctx->ner_labels;
}

int **
senna_get_srl (senna_t * ctx)
{
  senna_require_tokens (ctx, NULL);
  senna_require_labels (ctx, srl);
  return ctx->srl_labels;
}
